;
;  NAME: 
;    structPoint
;
;  PURPOSE:
;    Geographical point structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------

PRO structPoint__define
    
    MAX_N_VARIABLES = 10

    structPoint = { structPoint, $
                           name : '', $
                           lon : 0.0, $
                           lat : 0.0 $
                     }
END