;
;  NAME: 
;    cvcDataJRA55nc
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
FUNCTION cvcDataJRA55::Init, in_sDatasetDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataJRA55::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
;+
; :Description:
;    Reads data array from JRA55-file. Accepts time range.
;
; :Params:
;   in_pFile - pointer to an array of pointers to structures of file informations
;   in_sRequest - structure:
;     sTimeRng - structure of type structTimeSeg - time range to read
;     lev - level
;     varName - variable name
;   out_sData - structure:
;     aData - data array of size n_elements(aLons) x n_elements(aLats) x number of time steps
;     aLons - array of longitudes
;     aLats - array of latitudes
;     minVal - minimum value in aData
;     maxVal - maximum value in aData
;     missingVal - value in aData which means missing data
;     sTimeRng - time range
;     aLev - array of levels (only one element)
;     nLev - number of levels (always 1)
;     resultCode - error code (ERROR_OK if no errors)
;
; :Author: garry
;-
FUNCTION cvcDataJRA55::Read, in_aArea, in_aTimeRng, in_level, out_sData

    res = self->__ReadNCDF(in_aArea, in_aTimeRng, in_level, out_sData)

    if (~self->Assert(res)) then self.prepared = 1    
    return, res

END
;--------------------------------------------------------------------
FUNCTION cvcDataJRA55::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

    res = self->__PrepareNCDF(in_varName, in_sTimeSeg, in_aLevel, in_sRegion)
    
    if (~self->Assert(res)) then self.prepared = 1    
    return, res

END
;--------------------------------------------------------------------
PRO cvcDataJRA55__define

    struct = { cvcDataJRA55, $
               INHERITS cvcData $
             }
END