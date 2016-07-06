;
;  NAME: 
;    cvcPlotVectorStation
;
;  PURPOSE:
;    High-level class. Provides plotting of results. Stations on a map.
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorStation::Init, in_oGlobal

    res = self->cvcPlotVector::Init(in_oGlobal)

    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotVectorStation::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
    set_plot, 'x'
END
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorStation::__PlotStationShp, in_sData, in_sDestDesc

  entityTypePoint = 1
  entityTypePolyLine = 3
  entityTypePolygon = 5
  typeLong = 3
  typeDouble = 5
  typeString = 7

; create shapefile for stations       
  goodIdx = where(in_sData.aData ne in_sData.missingVal, nEnt)
;  nEnt = n_elements(in_sData.aData)
;  oNewShape = obj_new('IDLffShape', in_sDestDesc.file.name, entity_type=entityTypePoint, /update)  
  oNewShape = obj_new('IDLffShape', in_sDestDesc.file.name, entityTypePoint, 1)  

  oNewShape->AddAttribute, 'ID', typeString, 11  
  oNewShape->AddAttribute, 'VALUE', typeDouble, 20
  oNewShape->AddAttribute, 'F_CODE', typeString, 10  
  oNewShape->AddAttribute, 'F_CODE_DES', typeString, 25
  oNewShape->AddAttribute, 'NAME', typeString, 25

  for entIdx = 0, nEnt-1 do begin
    gIdx = goodIdx[entIdx]
;    self->printLog, 'endIdx=', entIdx
    sNewEnt = {IDL_SHAPE_ENTITY}
;    self->printLog, in_sData.aLons[gIdx], in_sData.aLats[gIdx]
    sNewEnt.SHAPE_TYPE = long(entityTypePoint)
    sNewEnt.ISHAPE = long(entIdx)
    sNewEnt.BOUNDS[0] = in_sData.aLons[gIdx]
    sNewEnt.BOUNDS[1] = in_sData.aLats[gIdx]
    sNewEnt.BOUNDS[2] = 0.00000000
    sNewEnt.BOUNDS[3] = 0.00000000 
    sNewEnt.BOUNDS[4] = in_sData.aLons[gIdx] 
    sNewEnt.BOUNDS[5] = in_sData.aLats[gIdx] 
    sNewEnt.BOUNDS[6] = 0.00000000 
    sNewEnt.BOUNDS[7] = 0.00000000 
    sNewEnt.N_VERTICES = 1
  
    newAttr = oNewShape ->GetAttributes(/ATTRIBUTE_STRUCTURE)
    if (ptr_valid(in_sData.extra)) then newAttr.ATTRIBUTE_0 = (*in_sData.extra).aStCodes[gIdx] else newAttr.ATTRIBUTE_0 = string(entIdx)
    newAttr.ATTRIBUTE_1 = in_sData.aData[gIdx]
    newAttr.ATTRIBUTE_2 = 'CA010'
    newAttr.ATTRIBUTE_3 = 'Stations'
    if (ptr_valid(in_sData.extra)) then newAttr.ATTRIBUTE_4 = (*in_sData.extra).aStNames[gIdx] else newAttr.ATTRIBUTE_4 = ''
  
    oNewShape->PutEntity, sNewEnt
    oNewShape->SetAttributes, entIdx, newAttr
  endfor
  
  if (obj_valid(oNewShape)) then obj_destroy, oNewShape

self->printLog, strtrim(string(nEnt), 2), ' station points have been written'

 ; prepare legend as a separate file
  if (in_sDestDesc.graphics.legend.kind eq 'file') then begin
self->printLog, 'Preparing legend'

; load color table
self->printLog, 'Loading color table'
    res = self->__GetCTIndex(in_sDestDesc.graphics.colortable, colorTableId)
    if (self->Assert(res)) then return, -1
       
    loadct, colorTableId
    tvlct, aColorTable, /get
    aColorTable[0, *] = 0b
    aColorTable[254, *] = 192b
    aColorTable[255, *] = 255b

    if (in_sDestDesc.graphics.blackbg eq 'yes') then begin
      colorFG = 255
      colorBG = 0
    endif else begin
      colorFG = 0
      colorBG = 255
    endelse

    nMaxLegColor = 253
    if (in_sDestDesc.graphics.legend.type eq 'continuous') then begin
      nLegColor = 253
    endif else begin
      nLegColor = in_sDestDesc.graphics.legend.ncolors
    endelse

    nLegLabels = in_sDestDesc.graphics.legend.nlabels

    goodIdxs = where(in_sData.aData ne in_sData.missingVal)
    if (goodIdxs[0] ne -1) then begin
      minVal = min(in_sData.aData[goodIdxs], max=maxVal)
; little hack to remove outliers
      minMinVal = min(in_sData.aData[goodIdxs[where(in_sData.aData[goodIdxs] ne minVal)]])
      maxMaxVal = max(in_sData.aData[goodIdxs[where(in_sData.aData[goodIdxs] ne maxVal)]])
      minVal = minMinVal
      maxVal = maxMaxVal
    endif else begin
      minVal = in_sData.missingVal
      maxVal = in_sData.missingVal
    endelse

    labelFmt = (abs(maxVal) lt 1000000.) and (abs(maxVal) gt 1e-6) ? '(f8.'+strtrim(string(fix(5-alog10(abs(maxVal)))<5), 2)+')' : '(e9.2)'
    sLegOpts = { title : in_sDestDesc.graphics.legend.title + ', ' + in_sDestDesc.graphics.legend.units, $ ; legend title
                 minVal : minVal, $ ; minimum value
                 maxVal : maxVal, $ ; maximum value
                 steps : 10, $ ; number of major steps
                 nColor : nLegColor, $ ; number of colors
                 nLabels : nLegLabels, $ ; number of labels
                 colorScale : in_sDestDesc.graphics.colorScale, $ ; color scale
                 aLim : [0.0, 0.00, 1.5, 1.00], $ ; positions of legend's borders
                 charThick : 1.5, $ ; thickness of characters 
                 charSize : 1.8, $ ; size of characters
                 titleSize : 1.7, $ ; size of title
                 titleAlign : 0, $ ; title alignment
                 labelFmt : labelFmt, $ ; label format
                 colorBG : colorBG, $ ; background color
                 colorFG : colorFG, $ ; foreground color
                 kind : 'stations' $ ; legend for raster graphics
               }
    legFileName = in_sDestDesc.graphics.legend.file.name
    legFileType = in_sDestDesc.graphics.legend.file.type
    
    if (legFileType eq 'xml') then begin ; here we only prepare an XML-file descripting a legend
      res = self->__WriteXMLLegend(legFileName, aColorTable, OPTIONS = sLegOpts)
    endif else begin ; otherwise we try to plot a graphical file with a legend

self->printLog, 'Implementation was not tested!'

      device, set_resolution=[200, 500]           

      plot, [0, 1], [0, 1], xstyle=4, ystyle=4, pos=[0, 0, 1, 1], /nodata ; a hack to fix /norm-problem
      res = self->__PlotALegend(OPTIONS = sLegOpts)
  
      image = tvrd()
;    stop
;      write_image, legFileName, legFileType, image, aColorTable[*, 0], aColorTable[*, 1], aColorTable[*, 2]
;      write_image2, legFileName, legFileType, image, red=aColorTable[*, 0], green=aColorTable[*, 1], blue=aColorTable[*, 2]
self->printLog, 'Writing legend file'
      write_tiff, legFileName, reverse(image, 2), red=aColorTable[*, 0], green=aColorTable[*, 1], blue=aColorTable[*, 2]
    endelse ; if legend is written into file
 
  endif
    
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorStation::Run, in_pInputs

;    self.pInputs = in_pInputs

    ; where to plot
    idx = where((*in_pInputs).type eq 'destination')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: No destination!'
      return, -1
    endif
    sDestDesc = *((*in_pInputs)[idx[0]].data)
    
    ; what to plot (only one and the first dataset is plotted!!!)
    idx = where((*in_pInputs).type eq 'data')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: Nothing to plot!'
      return, -1
    endif
    sData = *((*in_pInputs)[idx[0]].data)
        
    if (sDestDesc.file.name ne '') then begin
      outFileType = sDestDesc.file.type 
    endif else begin
      outFileType = 'x' ; no file name - no file. output to screen then.
    endelse
    
    ret = self->__Preprocess2D(sData, sDestDesc, sPlotData)
    if (self->Assert(ret)) then return, ret
    
    case outFileType of
      'x': begin
             ret = self->__OpenX(sDestDesc)
             ret = self->__PlotMap(sPlotData, sDestDesc)
           end
      'eps': begin
               ret = self->__OpenEPS(sDestDesc)
               ret = self->__PlotMap(sPlotData, sDestDesc)
               device, /close
               set_plot, 'x'
             end
      'shape': ret = self->__PlotStationShp(sPlotData, sDestDesc)
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotVectorStation__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotVectorStation, $
                  INHERITS cvcPlotVector $
             }
END
