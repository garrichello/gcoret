;  NAME: 
;    structDataReq
;
;  PURPOSE:
;    Structure. Contains request for data extraction by cvcData* classes.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
PRO structDataReq__define

    MAX_NLEV = 128
    MAX_N_TIME_SEGMENTS = 128

    structDataReq = { structDataReq, $
                      varName : '', $     ; variable name                    
                      sTimeSeg : {structTimeSeg}, $ ; time range, obsolete!
            	      numTimeSeg : 0, $  ; number of time segments
                      asTimeSeg : make_array(MAX_N_TIME_SEGMENTS, value={structTimeSeg}), $   ; array of time segments
                      aLevel : make_array(MAX_NLEV, /string), $ ; levels
                      nLev : 0, $ ; number of levels
                      sRegion : {structROI} $ ; region of interest
                    }
END
