;
;  NAME: 
;    IDLffXMLDOMElement
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
FUNCTION IDLffXMLDOMElement::Init, name
    self.name = name
    self.type = 1 ; ELEMENT_NODE
    return, 1
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMElement::Cleanup
    if (obj_valid(self.oFirstChild)) then obj_destroy, self.oFirstChild
    if (obj_valid(self.oChildNodes)) then obj_destroy, self.oChildNodes
;    print, 'Node is clean'
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMElement::GetAttribute, attrName
    attr = python('myxmldom', 'getAttribute', self.name, attrName)
;    print, 'attribute: ', attrName, ' has value: ', attr
    return, attr
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMElement::GetElementsByTagName, tagName
;    print, 'tag: ', tagName
    nNodes = python('myxmldom', 'getElementsByTagName', self.name, tagName)
;    print, 'list:', self.name, ', nodes: ', nNodes
    self.oChildNodes = obj_new('IDLffXMLDOMNodeList', nNodes, self.name)
;    print, 'Node list returns here'
    return, self.oChildNodes
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMElement__define
    
    struct = { IDLffXMLDOMElement, $
               foo : 0, $
               INHERITS IDLffXMLDOMNode $
             }
END
