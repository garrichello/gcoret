;
;  NAME: 
;    cvcPlot
;
;  PURPOSE:
;    High-level class. Provides plotting of results. Shaded field.
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotShaded::Init, in_oGlobal

    res = self->cvcPlot::Init(in_oGlobal)

    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotShaded::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
    set_plot, 'x'
END
;--------------------------------------------------------------------
FUNCTION cvcPlotShaded::__PlotGeoTIFF, in_sData, in_sDestDesc

    if (in_sDestDesc.graphics.kind ne 'shaded') then begin
      self->printLog, 'Error: GeoTIFF is only for shaded graphics kind!'
      return, -1
    endif
    
    set_plot, 'z'
    device, set_resolution=[in_sDestDesc.graphics.width, in_sDestDesc.graphics.height]

; set map conversion
self->printLog, 'Setting map projection'
    map_set, 0, 0, name=in_sDestDesc.projection.name, xmargin = [0, 0], ymargin = [0, 0], $
     limit=[in_sDestDesc.projection.limits.bottom, in_sDestDesc.projection.limits.left, $
            in_sDestDesc.projection.limits.top, in_sDestDesc.projection.limits.right], /noborder   

    minLon = min(in_sData.aLons, max = maxLon)
    minLat = min(in_sData.aLats, max = maxLat)

;    stop
; map image to the chosen projection
self->printLog, 'Mapping image'
    if ((size(in_sData.aData))[0] eq 3) then begin
      nSlices = (size(in_sData.aData))[3]
      self->printLog, 'WARNING! Data to plot has 3 dimensions... Several files will be written.'
    endif else nSlices = 1

    latmin =  in_sData.aLats[0]
    latmax = in_sData.aLats[n_elements(in_sData.aLats)-1]
    lonmin = in_sData.aLons[0]
    lonmax = in_sData.aLons[n_elements(in_sData.aLons)-1] eq in_sDestDesc.projection.limits.right ? $
	      in_sDestDesc.projection.limits.right - 0.001 : in_sData.aLons[n_elements(in_sData.aLons)-1]
 
    
;    if ((maxLon gt 180) and (minLon lt 180)) then begin
;      minLon = -180.
;      maxLon = 180.
;    endif
;    xScale = (maxLon - minLon + 1) / imageXsize
;    yScale = (maxLat - minLat + 1) / imageYsize

    xScale = (in_sDestDesc.projection.limits.right - in_sDestDesc.projection.limits.left) / (in_sDestDesc.graphics.width - 1) 
    yScale = (in_sDestDesc.projection.limits.top - in_sDestDesc.projection.limits.bottom) / (in_sDestDesc.graphics.height - 1) 

; prepare geokeys    
self->printLog, 'Preparing geokeys'
    ModelTypeProjected = 1 	;/* Projection Coordinate System Model   */
    ModelTypeGeographic = 2   	;/* Geographic latitude-longitude System */
    ModelTypeGeocentric = 3 	;/* Geocentric (X,Y,Z) Coordinate System */
    RasterPixelIsArea  = 1
    GCS_WGS_84 =  4326 
    CT_PolarStereographic =	15

    if (in_sDestDesc.projection.name eq 'cylindrical') then begin

      geokeys = { MODELPIXELSCALETAG : [ xScale, yScale, 0.0D ], $
                MODELTIEPOINTTAG : [ 0.0D, 0.0D, 0.0D, $
                                     double(in_sDestDesc.projection.limits.left), double(in_sDestDesc.projection.limits.bottom), 0.0D ], $
                GTMODELTYPEGEOKEY : ModelTypeGeographic, $
                GTRASTERTYPEGEOKEY : RasterPixelIsArea, $
                GEOGRAPHICTYPEGEOKEY : GCS_WGS_84 $
              }

    endif else if (in_sDestDesc.projection.name eq 'stereographic') then begin
      sMAP = MAP_PROJ_INIT('Stereographic', CENTER_LONGITUDE = in_sDestDesc.projection.p0lon, CENTER_LATITUDE = in_sDestDesc.projection.p0lat)
      xyCoord = map_proj_forward(in_sData.aLons[0], in_sData.aLats[0], MAP_STRUCTURE=sMAP)      
      geokeys = { MODELPIXELSCALETAG : [ 30000, 30000, 0.0D ], $
                  MODELTIEPOINTTAG : [ 0.0D, 0.0D, 0.0D, $
                                     double(xyCoord[0]), double(xyCoord[1]), 0.0D ], $                
                  GTMODELTYPEGEOKEY : ModelTypeProjected, $
                  GTRASTERTYPEGEOKEY : RasterPixelIsArea, $
                  GEOGRAPHICTYPEGEOKEY : GCS_WGS_84, $
                  ProjectedCSTypeGeoKey: 32767, $ ; (user-defined)
                  ProjectionGeoKey: 32767, $ ; (user-defined)
                  ProjLinearUnitsGeoKey: 9001, $ ; (Linear_Meter)
                  ProjCoordTransGeoKey: CT_PolarStereographic, $
;                  ProjStdParallel1GeoKey: 90.000, $
;                  ProjStdParallel2GeoKey: 90.000, $
;                  ProjCenterLongGeoKey: 0.0, $
                  ProjNatOriginLatGeoKey: 90.0, $
                  ProjStraightVertPoleLongGeoKey: 0 $
;                  ProjFalseEastingGeoKey: 0.0, $
;                  ProjFalseNorthingGeoKey: 0.0 $
              }
      endif


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

; loop by slices
    for iSlice = 0, nSlices-1 do begin
;stop
; Let's map the data...
if (in_sDestDesc.projection.name eq 'cylindrical') then begin
    if (in_sDestDesc.graphics.smoothing eq 'yes') then begin
      aMappedData = map_image(in_sData.aData[*, *, iSlice], startXPos, startYPos, $
       imageXSize, imageYSize, lonmin = lonmin, lonmax = lonmax, $
       latmin = latmin, latmax = latmax, compress = 1, missing = in_sData.missingVal, $
       min_value = in_sData.missingVal, mask = goodMask, /bilinear)
    endif else begin
      aMappedData = map_image(in_sData.aData[*, *, iSlice], startXPos, startYPos, $
       imageXSize, imageYSize, lonmin = lonmin, lonmax = lonmax, $
       latmin = latmin, latmax = latmax, compress = 1, missing = in_sData.missingVal, $
       min_value = in_sData.missingVal, mask = goodMask)
    endelse
endif else begin
    aMappedData = in_sData.aData[*, *, iSlice]
    startXPos = 0
    startYpos = 0
    imageXSize = in_sDestDesc.graphics.width
    imageYSize = in_sDestDesc.graphics.height
endelse

   if (in_sDestDesc.graphics.legend.limited eq 'yes') then begin
      minVal = in_sDestDesc.graphics.legend.minimum
      maxVal = in_sDestDesc.graphics.legend.maximum
    endif else minVal = min(aMappedData[where(aMappedData ne in_sData.missingVal)], max = maxVal)


; scale data
    if (in_sDestDesc.graphics.legend.file.type ne 'xml') then begin ; we scale and write RGB geotiff only when legend type is NOT XML!
self->printLog, 'Scaling data'
      idxMissing = where(aMappedData eq in_sData.missingVal, complement=idxPresent)

;ToDo: Bad code! Should be rewritten!
      idxMin = where(aMappedData le minVal)
      idxMax = where(aMappedData ge maxVal)
      idxMid = where((aMappedData gt minVal) and (aMappedData lt maxVal))

      aMappedAndScaledData = aMappedData
      aMappedAndScaledData[idxMin] = 1

      minValMid = min(aMappedData[idxMid], max=maxValMid)
      if (in_sDestDesc.graphics.colorscale eq 'linear') then begin
        aMappedAndScaledData[idxMid] = fix((nLegColor-3+0.9999) * (aMappedData[idxMid] - minValMid)/(maxValMid - minValMid)) + 2
      endif else begin
        aTmp = alog10((nLegColor-1)*(aMappedData[idxMid] - minValMid)/(maxValMid - minValMid)+1)
        minValMid = min(aTmp, max=maxValMid)
        aMappedAndScaledData[idxMid] = fix((nLegColor-3+0.9999) * (aTmp - minValMid)/(maxValMid - minValMid)) + 2
      endelse

      if (idxMax[0] ne -1) then aMappedAndScaledData[idxMax] = nLegColor
      aMappedAndScaledData = aMappedAndScaledData*goodMask
      if (idxMissing[0] ne -1) then aMappedAndScaledData[idxMissing] = colorBG

; write scaled RGB geotiff

;    erase, colorBG
;    tv, aMappedAndScaledData, startXPos, startYPos, xsize = imageXSize, ysize = imageYSize
;    buf = tvrd()

print, 'Writing RGB GeoTIFF file'                                                                                                                             
                                                                                                                                                              
;      dotPos = strpos(in_sDestDesc.file.name, '.', /reverse_search)                                                                                          
;      rgbFileName = strmid(in_sDestDesc.file.name, 0, dotPos)+'_rgb'+strmid(in_sDestDesc.file.name, dotPos)                                                  
      rgbFileName = in_sDestDesc.file.name                                                                                                                    
      buf = fltarr(in_sDestDesc.graphics.width, in_sDestDesc.graphics.height, /nozero)                                                                        
      buf[*] = colorBG                                                                                                                                        
      buf[startXpos:startXpos+imageXsize-1, startYpos:startYpos+imageYsize-1] = aMappedAndScaledData                                                          
                                                                                                                                                              
      write_tiff, rgbFileName, buf, $                                                                                                                         
        red = aColorTable[*, 0], green = aColorTable[*, 1], blue = aColorTable[*, 2], geotiff = geokeys                                                       
                                                                                                                                                              
self->printLog, 'Writing float GeoTIFF file'                                                                                                                           
      dotPos = strpos(in_sDestDesc.file.name, '.', /reverse_search)                                                                                           
      dataFileName = strmid(in_sDestDesc.file.name, 0, dotPos)+'_data'+strmid(in_sDestDesc.file.name, dotPos)                                                 
      buf = fltarr(in_sDestDesc.graphics.width, in_sDestDesc.graphics.height, /nozero)                                                                        
      buf[*] = in_sData.missingVal                                                                                                                            
      buf[startXpos:startXpos+imageXsize-1, startYpos:startYpos+imageYsize-1] = aMappedData                                                                   
      write_tiff, dataFileName, buf, geotiff = geokeys, /float                                                                                                
                                                                                                                                                              
    endif else begin ; ne 'XML'                                                                                                                                         
                                                                                                                                                              
; write float geoTIFF                                                                                                                                         
self->printLog, 'Writing GeoTIFF file'                                                                                                                                 
      dotPos = strpos(in_sDestDesc.file.name, '.', /reverse_search)                                                                                           
      dataFileName = in_sDestDesc.file.name
; the next string is for multifile output. uncomment when needed
;      dataFileName = strmid(in_sDestDesc.file.name, 0, dotPos)+'_'+strtrim(string(islice), 2)+strmid(in_sDestDesc.file.name, dotPos)                         

      buf = fltarr(in_sDestDesc.graphics.width, in_sDestDesc.graphics.height, /nozero)                                                                        
      buf[*] = in_sData.missingVal                                                                                                                            
      buf[startXpos:startXpos+imageXsize-1, startYpos:startYpos+imageYsize-1] = aMappedData                                                                   
      write_tiff, dataFileName, buf, geotiff = geokeys, /float                                                                                      
    endelse ; eq 'XML'


; prepare legend as a separate file
    if (in_sDestDesc.graphics.legend.kind eq 'file') then begin
self->printLog, 'Preparing legend'

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
                 kind : 'raster' $ ; legend for raster graphics
               }

      dotPos = strpos(in_sDestDesc.graphics.legend.file.name, '.', /reverse_search)

; the next string is for multifile output. uncomment when needed
;      legFileName = strmid(in_sDestDesc.graphics.legend.file.name, 0, dotPos)+'_'+strtrim(string(islice), 2)+strmid(in_sDestDesc.graphics.legend.file.name, dotPos)
      legFileName = in_sDestDesc.graphics.legend.file.name
      legFileType = in_sDestDesc.graphics.legend.file.type
    
      if (legFileType eq 'xml') then begin ; here we only prepare an XML-file descripting a legend
        res = self->__WriteXMLLegend(legFileName, aColorTable, OPTIONS = sLegOpts)
      endif else begin ; otherwise we try to plot a graphical file with a legend

;      set_plot, 'x'
;      device, decomposed=0
;      window, xsize=200, ysize=500
;      loadct, colorTableId
;      tvlct, aColorTable, /get
;      aColorTable[0, *] = 0b
;      aColorTable[254, *] = 192b
;      aColorTable[255, *] = 255b
;      tvlct, aColorTable
    
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
self->printLog, "(cvcPlotShaded) Don't know where to put a legend. Skipping..."
    endif

    endfor ; loop for slices
  
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotShaded::Run, in_pInputs

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
      'geotiff': ret = self->__PlotGeoTIFF(sPlotData, sDestDesc)
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotShaded__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotShaded, $
                  INHERITS cvcPlot $
             }
END
