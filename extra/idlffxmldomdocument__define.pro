;
;  NAME: 
;    IDLffXMLDOMDocument
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
FUNCTION IDLffXMLDOMDocument::Init
    self.type = 9 ; DOCUMENT_NODE
    return, 1
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMDocument::Cleanup
    if (obj_valid(self.oChildNodes)) then obj_destroy, self.oChildNodes
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMDocument::Load, FILENAME=in_fileName, MSG_ERROR=in_msgErrorFunc, MSG_FATAL=in_msgFatalFunc
    nNodes = python('myxmldom', 'load', in_fileName)
    self.name = file_basename(in_fileName)
;    nodeType = python('myxmldom', 'getItem', self.name, 0)
;    self.oChildNodes = obj_new('IDLffXMLDOMNodeList', nNodes, self.name)
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMDocument::GetElementsByTagName, tagName
;    print, 'tag: ', tagName
    nNodes = python('myxmldom', 'getElementsByTagName', self.name, tagName)
;    print, 'list:', self.name, ', nodes: ', nNodes
    self.oChildNodes = obj_new('IDLffXMLDOMNodeList', nNodes, self.name)
;    print, 'Node list returns here'
    return, self.oChildNodes
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMDocument__define
    
    struct = { IDLffXMLDOMDocument, $
               foo : 0, $
               INHERITS IDLffXMLDOMNode $
             }
END
