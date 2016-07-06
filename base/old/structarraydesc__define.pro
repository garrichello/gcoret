;
;  NAME: 
;    structArrayDesc
;
;  PURPOSE:
;    Array description
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
PRO structArrayDesc__define

    structArrayDesc = { structArrayDesc, $
                         uid : '', $ ; array's uid
                         type : '', $ ; array's type: array
                         kind : '', $ ; data's kind: 'map', 'stations', 'lines'
                         description : { description, $ ; description
                           title : '', $ ; long description 
                           name : '', $ ; human-readable variable name
                           units : '' $ ; units of variable
                         }, $
                         pXgrid : ptr_new(), $ ; pointer to longitude grid
                         pYgrid : ptr_new(), $ ; pointer to latitude grid
                         pZgrid : ptr_new(), $ ; pointer to time grid
                         pTimeGrid : ptr_new(),  $ ; pointer to level grid
                         missingVal : 0.0, $ ; missing value
                         gridType : '', $ ; grid type: 'regular', 'irregular', 'station'
                         pData : ptr_new(), $ ; pointer to some kind of data
                         extra : ptr_new() $ ; extra data
                       }
END