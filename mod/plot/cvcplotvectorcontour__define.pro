;
;  NAME: 
;    cvcPlotVectorContour
;
;  PURPOSE:
;    High-level class. Provides plotting of results. Contour field.
;
;  INHERITS:
;    cvcPlotVector
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorContour::Init, in_oGlobal

    res = self->cvcPlotVector::Init(in_oGlobal)

    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotVectorContour::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
    set_plot, 'x'
END
;--------------------------------------------------------------------
PRO cvcPlotVectorContour::__AddEnts, in_oNewShape, in_sPath, in_aXY, in_dLon, in_dLat, in_descName

  entityTypePoint = 1
  entityTypePolyLine = 3
  entityTypePolygon = 5

  nEnt = n_elements(in_sPath)

  nPoints = n_elements(in_aXY[0, *])

  for entIdx = 0, nEnt-1 do begin
      sNewEnt = {IDL_SHAPE_ENTITY}
      sNewEnt.SHAPE_TYPE = long(entityTypePolyLine)
      sNewEnt.ISHAPE = long(entIdx)
      offset = in_sPath[entIdx].offset

      n = in_sPath[entIdx].n
      if ( (abs(in_aXY[0, offset] - in_aXY[0, offset+n-1]) lt 2*in_dLon) and (abs(in_aXY[1, offset] - in_aXY[1, offset+n-1]) lt 2*in_dLat) and $
         (offset+n lt nPoints) ) then begin 
        aSubXY = [[in_aXY[*, offset:(offset+n-1)]], [in_aXY[*, offset]]]
        n = n + 1
      endif else begin
        aSubXY = in_aXY[*, offset:(offset+n-1)]
      endelse

      sNewEnt.BOUNDS[0] = min(aSubXY[0, *])
      sNewEnt.BOUNDS[1] = min(aSubXY[1, *])
      sNewEnt.BOUNDS[2] = 0.00000000
      sNewEnt.BOUNDS[3] = 0.00000000 
      sNewEnt.BOUNDS[4] = max(aSubXY[0, *]) 
      sNewEnt.BOUNDS[5] = max(aSubXY[1, *]) 
      sNewEnt.BOUNDS[6] = 0.00000000 
      sNewEnt.BOUNDS[7] = 0.00000000 
      sNewEnt.N_VERTICES = n
      sNewEnt.VERTICES = ptr_new(aSubXY)
  
      newAttr = in_oNewShape ->GetAttributes(/ATTRIBUTE_STRUCTURE)
      newAttr.ATTRIBUTE_0 = entIdx
      newAttr.ATTRIBUTE_1 = in_sPath[entIdx].value
      newAttr.ATTRIBUTE_2 = 'CA010'
      newAttr.ATTRIBUTE_3 = 'Contour Line (Land)'
      newAttr.ATTRIBUTE_4 = in_descName
  
      in_oNewShape->PutEntity, sNewEnt
      in_oNewShape->SetAttributes, entIdx, newAttr
  endfor    

END
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorContour::__PlotContourShp, in_sData, in_sDestDesc

  entityTypePoint = 1
  entityTypePolyLine = 3
  entityTypePolygon = 5
  typeLong = 3
  typeDouble = 5
  typeString = 7

  set_plot, 'z'

  aLons = in_sData.aLons
  aLats = in_sData.aLats
;  aData = in_sData.aData

; let's check if longitude grid is continous when crossing 0 meridian
  cross360step = aLons[0]-aLons[-1]+360 ; grid step across 0 meridian
  lonStep = round((aLons[1]-aLons[0])*100)/100. ; assume we don't have a step less than 0.01 deg.
  epsStep = lonStep / 20. ; epsilon vicinity of a 360 deg. point
  if (cross360step eq lonStep or ((cross360step lt 360+epsStep) and (cross360step gt 360-epsStep))) then begin ; we can merge both parts
    self->printLog, 'No gap between parts'
    isLonGap = 0
  endif else begin ; we can't merge two parts if there is a gap greater than a lonStep between them
    self->printLog, 'There is a gap between parts'
    isLonGap = 1
  endelse

  set_plot, 'z'
  device, set_resolution=[in_sDestDesc.graphics.width, in_sDestDesc.graphics.height]

; set map conversion
  self->printLog, 'Setting map projection'
  map_set, 0, 0, name=in_sDestDesc.projection.name, xmargin = [0, 0], ymargin = [0, 0], $
   limit=[in_sDestDesc.projection.limits.bottom, in_sDestDesc.projection.limits.left, $
   in_sDestDesc.projection.limits.top, in_sDestDesc.projection.limits.right], /noborder

  minLon = min(in_sData.aLons, max = maxLon)
  minLat = min(in_sData.aLats, max = maxLat)

; map image to the chosen projection
  self->printLog, 'Mapping image'
  latmin =  in_sData.aLats[0]
  latmax = in_sData.aLats[n_elements(in_sData.aLats)-1]
  lonmin = in_sData.aLons[0]
  lonmax = in_sData.aLons[n_elements(in_sData.aLons)-1] eq in_sDestDesc.projection.limits.right ? $
   in_sDestDesc.projection.limits.right - 0.001 : in_sData.aLons[n_elements(in_sData.aLons)-1]
  if (in_sDestDesc.graphics.smoothing eq 'yes') then begin
    aMappedData = map_image(in_sData.aData, startXPos, startYPos, $
     imageXSize, imageYSize, lonmin = lonmin, lonmax = lonmax, $
     latmin = latmin, latmax = latmax, compress = 1, missing = in_sData.missingVal, $
     min_value = in_sData.missingVal, mask = goodMask, /bilinear)
  endif else begin
    aMappedData = map_image(in_sData.aData, startXPos, startYPos, $
     imageXSize, imageYSize, lonmin = lonmin, lonmax = lonmax, $
     latmin = latmin, latmax = latmax, compress = 1, missing = in_sData.missingVal, $
     min_value = in_sData.missingVal, mask = goodMask)
  endelse

;  if (max(aLons) gt 180) then begin
;    aLons = (aLons + 180.) mod 360. -180.
;    pos = min(where(aLons lt 0))
;    aLons = [aLons[pos:*], aLons[0:pos-1]]
;    aData = [aData[pos:*, *], aData[0:pos-1, *]]
;  endif

; create shapefile for contours

;  oNewShape = obj_new('IDLffShape', in_sDestDesc.file.name, entity_type = entityTypePolyLine, /update)  
  oNewShape = obj_new('IDLffShape', in_sDestDesc.file.name, entityTypePolyLine, 1)  

  oNewShape->AddAttribute, 'ID', typeLong, 10
  oNewShape->AddAttribute, 'ID2', typeDouble, 10
  oNewShape->AddAttribute, 'F_CODE', typeString, 10
  oNewShape->AddAttribute, 'F_CODE_DES', typeString, 25
  oNewShape->AddAttribute, 'NAM', typeString, 25
  
  minVal = min(in_sData.aData[where(in_sData.aData ne in_sData.missingVal)], max = maxVal)


  dLon = (lonMax-lonMin)/(imageXsize-1)
  dLat = (latMax-latMin)/(imageYsize-1)

  aLons = findgen(imageXsize)*dLon+lonMin
  aLats = latMax-findgen(imageYsize)*dLat
  aData = temporary(aMappedData)

;  dLon = abs(aLons[1] - aLons[0])
;  dLat = abs(aLats[1] - aLats[0])

  if (isLonGap eq 1) then begin ; if there is a gap between two parts of a 0-360 field we need to process them separately

    idxsR = where(aLons ge 0, complement=idxsL) ; indices for right and left parts

; contours for the right part
    if (idxsR[0] ne -1) then begin
      contour1, aData[idxsR, *], aLons[idxsR], aLats, xstyle=1, ystyle=1, nlevels = in_sDestDesc.graphics.steps, $
       min_value = minVal, max_value = maxVal, path_info=sPath, path_xy=aXY, /path_data_coords, /PATH_DOUBLE, closed = 0
    endif
  
    self->__AddEnts, oNewShape, sPath, aXY, dLon, dLat, in_sData.description.name

; contours for the left part
    if (idxsL[0] ne -1) then begin
      contour1, aData[idxsL, *], aLons[idxsL], aLats, xstyle=1, ystyle=1, nlevels = in_sDestDesc.graphics.steps, $
       min_value = minVal, max_value = maxVal, path_info=sPath, path_xy=aXY, /path_data_coords, /PATH_DOUBLE, closed = 0
    endif

    self->__AddEnts, oNewShape, sPath, aXY, dLon, dLat, in_sData.description.name
    
  endif else begin ; we have a normal field and we can contour it at once

    contour1, aData, aLons, aLats, xstyle=1, ystyle=1, nlevels = in_sDestDesc.graphics.steps, $
     min_value = minVal, max_value = maxVal, path_info=sPath, path_xy=aXY, /path_data_coords, /PATH_DOUBLE, closed = 0
    
    self->__AddEnts, oNewShape, sPath, aXY, dLon, dLat, in_sData.description.name
  endelse
  
  if (obj_valid(oNewShape)) then  obj_destroy, oNewShape
  
  set_plot, 'x'

  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorContour::Run, in_pInputs

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
      'shape': ret = self->__PlotContourShp(sPlotData, sDestDesc)
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotVectorContour__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotVectorContour, $
                  INHERITS cvcPlotVector $
             }
END