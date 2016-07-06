;
;  NAME: 
;    IDL_SHAPE_ENTITY
;
;  PURPOSE:
;    Replacement of IDL class for using in GDL
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION IDL_SHAPE_ENTITY::Init

    return, 1
END
;--------------------------------------------------------------------
PRO IDL_SHAPE_ENTITY::Cleanup

END
;--------------------------------------------------------------------
PRO IDL_SHAPE_ENTITY__define
    
    struct = { IDL_SHAPE_ENTITY, $
               shape_type: 0L, $
               ishape: 0L, $
               bounds: dblarr(8), $
               n_vertices: 0L, $
               vertices: ptr_new(), $
               measure: ptr_new(), $
               n_parts: 0L, $
               part_types: ptr_new(), $
               attributes: ptr_new() $
             }
END
