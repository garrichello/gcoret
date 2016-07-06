;
;  NAME: 
;    IDLffXMLDOMText
;
;  PURPOSE:
;    Replacement of IDL class for using in GDL
;
;  INHERITS:
;    IDLffXMLDOMNode
;    IDLffXMLDOMCharacterData
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMText::Init, name
    self.name = name
    self.type = 3 ; TEXT_NODE
    return, 1
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMText::Cleanup
    if (obj_valid(self.oFirstChild)) then obj_destroy, self.oFirstChild
    if (obj_valid(self.oChildNodes)) then obj_destroy, self.oChildNodes
;    print, 'Node is clean'
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMText__define
    
    struct = { IDLffXMLDOMText, $
               bar : 0, $
               INHERITS IDLffXMLDOMCharacterData $
             }
END
