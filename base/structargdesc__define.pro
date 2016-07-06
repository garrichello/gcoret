;
;  NAME: 
;    structArgDesc
;
;  PURPOSE:
;    Input description structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;;
;--------------------------------------------------------------------

PRO structArgDesc__define

     structArgDesc = { structArgDesc, $
               uid : '', $ ; argument's uid
               type : '', $ ; argument's type: data/destination
               data : ptr_new() $ ; pointer to data
             }
END