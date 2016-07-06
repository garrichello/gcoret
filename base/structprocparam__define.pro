;
;  NAME: 
;    structProcParam
;
;  PURPOSE:
;    Processing parameters structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------

PRO structProcParam__define
    
    structProcParam = { structProcParam, $
                           uid : '', $
                           data : '' $
                      }
END