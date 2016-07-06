;
;  NAME: 
;    structTile
;
;  PURPOSE:
;    Landsat tile structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------

PRO structTile__define
    
    MAX_N_VARIABLES = 10

    structTile = { structTile, $
                           name : '', $
                           path : 0, $
                           row : 0 $
                     }
END