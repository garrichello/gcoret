;
;  NAME: 
;    cvcDataUSER
;
;  PURPOSE:
;    Low-level class. Provides access to meteo- and climatic data in user-provided file. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataUSER::Init, in_sDatasetDesc, modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataUSER::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
;+
; :Description:
;    Reads data array from user file. Accepts time range.
;
; :Author: garry
;-
FUNCTION cvcDataUSER::Read, in_aArea, in_aTimeRng, in_level, out_sData

    res = self->__ReadNCDF(in_aArea, in_aTimeRng, in_level, out_sData)

    if (~self->Assert(res)) then self.prepared = 1    
    return, res
END
;--------------------------------------------------------------------
FUNCTION cvcDataUSER::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

    self.sRequest.varName = in_varName
    self.sRequest.sTimeSeg = in_sTimeSeg
    self.sRequest.aLevel = in_aLevel
    self.sRequest.nLev = n_elements(in_aLevel)
    self.sRequest.sRegion = in_sRegion
      
    self.numberOfDataFiles = 1

    self.pDataFile = ptr_new(make_array(self.sRequest.nLev, self.numberOfDataFiles+1, value={structDataFile}))

    filename = self.dataset.name
    if (file_test(filename)) then begin
      info = self->__QueryNCDF(filename)
      pInfo = ptr_new(info, /no_copy)
      type = self.dataFileType
    endif else begin
      pInfo = ptr_new()
      type = 'MISSING'
    endelse

    for levIdx = 0, self.sRequest.nLev-1 do begin
      (*self.pDataFile)[levIdx, 0].pInfo = pInfo
      (*self.pDataFile)[levIdx, 0].type = type
      (*self.pDataFile)[levIdx, 0].name = filename
      (*self.pDataFile)[levIdx, 0].level = self.sRequest.aLevel[levIdx]
    endfor
    
    self.prepared = 1    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataUSER__define

    struct = { cvcDataUSER, $
               INHERITS cvcData $
             }
END
