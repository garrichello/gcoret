;
;  NAME: 
;    structProcOutput
;
;  PURPOSE:
;    Processing output structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------

PRO structProcOutput__define
        
    structProcOutput = { structProcOutput, $
                           uid : '', $
                           data : '' $
                     }
END