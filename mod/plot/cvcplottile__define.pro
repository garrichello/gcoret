;
;  NAME: 
;    cvcPlotTile
;
;  PURPOSE:
;    High-level class. Provides plotting of results. Landsat tiles.
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotTile::Init, in_hLogFile

    res = self->cvcPlot::Init(in_hLogFile)

    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotTile::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
    set_plot, 'x'
END
;--------------------------------------------------------------------
FUNCTION cvcPlotTile::__PlotGeoTIFF, in_sData, in_sDestDesc

; load color table
    res = self->__GetCTIndex(in_sDestDesc.graphics.colortable, colorTableId)
    if (self->Assert(res)) then return, -1

    loadct, colorTableId
    tvlct, aColorTable, /get
    aColorTable[0, *] = 255b
;    aColorTable[254, *] = 192b
    aColorTable[255, *] = 0b
    
    minval = min((*(*in_sData.data).pData)[where(*(*in_sData.data).pData ne -999.0)], max = maxval)

; write geotiff    
    write_tiff, in_sDestDesc.file.name, bytscl(*(*in_sData.data).pData, min=minval, max=maxval, top=253)+1, $
      red=aColorTable[*, 0], green=aColorTable[*, 1], blue=aColorTable[*, 2], geotiff=*(*in_sData.data).extra
;  endif
 
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotTile::__PlotGeoTIFF24, in_sData, in_sDestDesc

; write geotiff    
  write_tiff, in_sDestDesc.file.name, *(*in_sData.data).pData, geotiff=*(*in_sData.data).extra
  
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotTile::Run, in_pInputs

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
    
;    ret = self->__Preprocess2D(sData, sPlotData)
;    if (self->Assert(ret)) then return, ret
    
    case outFileType of
      'x': begin
             ret = self->__OpenX(sDestDesc)
             ret = self->__PlotMap(sData, sDestDesc)
           end
      'geotiff': ret = self->__PlotGeoTIFF(sData, sDestDesc)
      'geotiff24': ret = self->__PlotGeoTIFF24(sData, sDestDesc)
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotTile__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotTile, $
                  INHERITS cvcPlot $
             }
END