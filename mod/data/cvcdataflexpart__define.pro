;
;  NAME: 
;    cvcDataFLEXPART
;
;  PURPOSE:
;    Low-level class. Provides access to FLEXTRA model output
;
;  INHERITS:
;    cvcData
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataFLEXPART::Init, in_pDataDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_pDataDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataFLEXPART::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcDataFLEXPART::Read, in_aArea, in_aTimeRng, in_level, out_sData

    missingVal = -999.0

    var_name = self.sRequest.varName
    
    self->printLog, '(cvcDataFLEXPART) Reading data...'
    res = python('myflexpart', 'read_grid', in_aTimeRng)
    
    aLons = python('myflexpart', 'get_lons')
    aLats = python('myflexpart', 'get_lats')
    times = python('myflexpart', 'get_times')
    heights = python('myflexpart', 'get_heights')

    in_nLev = n_elements(in_level)
    for i = 0, in_nLev-1 do begin
	cur_idx = where(heights eq in_level[i])
	if (n_elements(lev_idxs) eq 0) then lev_idxs = cur_idx else lev_idxs = [lev_idxs, cur_idx]
    endfor

    data_grid = python('myflexpart', 'get_grid')
    dims = size(data_grid, /dim)
    data_grid = transpose(reform(data_grid, reverse(dims)))
    aAllData = data_grid[*, *, lev_idxs, *]
    dataDims = size(aAllData, /dim)
    
    self->printLog, 'Reformatting data...', format='(a, $)'
    newDims = dataDims[where(dataDims ne 1)]
    if (n_elements(newDims) ne n_elements(dataDims)) then begin
        aAllData = reform(aAllData, newDims, /overwrite)
    endif
    self->printLog, 'OK', /notimestamp
    
; convert time to JD
    self->DayMonthYear, long(times), reqYear, reqMonth, reqDay, reqHour
    aTimes = julday(reqMonth, reqDay, reqYear, reqHour)
            
; set missingVal
    missingVal = -999.

; search for min/max values
    self->printLog, '(cvcDataFLEXPART) Searching for min/max values...'
    goodidx = where(aAllData ne missingVal)
    if (goodidx[0] ne -1) then begin
      minVal = min(aAllData[goodidx], max=maxVal) 
    endif else begin
      minVal = missingVal
      maxVal = missingVal
    endelse
    
    out_sData = { aData : aAllData, $
                  aLons : aLons, $
                  aLats : aLats, $
                  aTimes : aTimes, $
                  minVal : minVal, $
                  maxVal : maxVal, $
                  missingVal : missingVal, $
                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                  lev : in_level, $
                  aLev : [in_level], $
                  nLev : in_nLev, $
                  gridType : 'regular', $
		  units : '', $ ; this field is filled in the cvcDataAccess::Get() method
                  resultCode : self.ERROR_OK $
                }

    self->printLog, '(cvcDataFLEXPART) Finished'

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataFLEXPART::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

    self.sRequest.varName = in_varName
    self.sRequest.sTimeSeg = in_sTimeSeg ; current time segment, in_sVar.asTimeSeg is ignored
    self.sRequest.aLevel = in_aLevel
    self.sRequest.nLev = n_elements(in_aLevel)

    ; read header
    self->printLog, '(cvcDataFLEXPART) Reading header...
    res = python('myflexpart', 'read_header', self.dataset.path, in_varName)
    if (res eq 0) then begin
	self->printLog, 'OK'
	self.prepared = 1
    endif else begin
	self->printLog, 'Error! Exiting...'
	self.prepared = 0
    endelse
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataFLEXPART__define

    struct = { cvcDataFLEXPART, $
               INHERITS cvcData $
             }
END
