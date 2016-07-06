;
;  NAME: 
;    cvcDataAPHRO
;
;  PURPOSE:
;    Low-level class. Provides access to meteo- and climatic data. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataAPHRO::Init, in_sDatasetDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataAPHRO::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
;+
; :Description:
;    Reads data array from ERA40-file. Accepts time range.
;
; :Author: garry
;-
FUNCTION cvcDataAPHRO::Read, in_aArea, in_aTimeRng, in_level, out_sData

    res = self->__ReadNCDF(in_aArea, in_aTimeRng, in_level, out_sData)
    
    return, res
END
;--------------------------------------------------------------------
FUNCTION cvcDataAPHRO::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

    res = self->__PrepareNCDF(in_varName, in_sTimeSeg, in_aLevel, in_sRegion)

    if (~self->Assert(res)) then self.prepared = 1    
    return, res
END
;--------------------------------------------------------------------
PRO cvcDataAPHRO__define

    struct = { cvcDataAPHRO, $
               INHERITS cvcData $
             }
END