;
;  NAME: 
;    IDLffXMLDOMCharacterData
;
;  PURPOSE:
;    Replacement of IDL class for using in GDL
;
;  INHERITS:
;    IDLffXMLDOMNode
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMCharacterData::Init
    return, 1
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMCharacterData::Cleanup
    if (obj_valid(self.oFirstChild)) then obj_destroy, self.oFirstChild
    if (obj_valid(self.oChildNodes)) then obj_destroy, self.oChildNodes
;    print, 'Node is clean'
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMCharacterData__define
    
    struct = { IDLffXMLDOMCharacterData, $
               foo : 0, $
               INHERITS IDLffXMLDOMNode $
             }
END
