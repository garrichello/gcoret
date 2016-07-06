;
;  NAME: 
;    structROI
;
;  PURPOSE:
;    Structure. Contains coordinates of borders of region of interest
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
PRO structROI__define

    MAX_N_LEVELS = 100

    structROI = { structROI, $
                    units : '', $ ; 'degrees' by default, 'pathrow' for landsat
                    lon0 : 0, $ ; central meredian: 0 or 180. default: 0. other value = default value
                    pPoints : ptr_new(), $ ; ponter to an array of lon-lat coordinates
                    pTiles : ptr_new() $ ; pointer to an array of structTile structures
                }

END