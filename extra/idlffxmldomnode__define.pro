;
;  NAME: 
;    IDLffXMLDOMNode
;
;  PURPOSE:
;    Replacement of IDL class for using in GDL
;
;  INHERITS:
;    
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMNode::Init
    return, 1
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMNode::Cleanup
    if (obj_valid(self.oFirstChild)) then obj_destroy, self.oFirstChild
    if (obj_valid(self.oChildNodes)) then obj_destroy, self.oChildNodes
;    print, 'Node is clean'
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMNode::GetChildNodes
    nNodes = python('myxmldom', 'getChildNodes', self.name)
    self.oChildNodes = obj_new('IDLffXMLDOMNodeList', nNodes, self.name)
;    print, 'Node list returns here'
    return, self.oChildNodes
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMNode::GetFirstChild
    nodeType = python('myxmldom', 'getFirstChild', self.name)
;    print, 'Node type:', nodeType
    case nodeType of
      1: self.oFirstChild = obj_new('IDLffXMLDOMElement', self.name+'_fc')
      3: self.oFirstChild = obj_new('IDLffXMLDOMText', self.name+'_fc')
      else: begin
        print, 'Unknown node type'
        stop
      end
    endcase
    return, self.oFirstChild
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMNode::GetNodeValue
    nodeValue = python('myxmldom', 'getNodeValue', self.name)
;    print, 'Node value: ', nodeValue
    return, nodeValue
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMNode__define
    
    struct = { IDLffXMLDOMNode, $
               name : '', $
               type : 0, $
               oFirstChild : obj_new(), $
               oChildNodes : obj_new() $
             }
END
