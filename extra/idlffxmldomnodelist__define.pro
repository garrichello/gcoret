;
;  NAME: 
;    IDLffXMLDOMNodeList
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
FUNCTION IDLffXMLDOMNodeList::Init, in_nNodes, in_listName
    if (in_nNodes lt 1024) then begin
      if (in_nNodes gt 0) then begin
        self.nNodes = in_nNodes
        self.name = in_listName
        self.oNodes = objarr(in_nNodes)
        for i = 0, in_nNodes-1 do begin
          nodeName = in_listName+'-'+strtrim(string(i), 2)
          nodeType = python('myxmldom', 'getItem', in_listName, i)
          case nodeType of
            1: self.oNodes[i] = obj_new('IDLffXMLDOMElement', nodeName)
            3: self.oNodes[i] = obj_new('IDLffXMLDOMText', nodeName)
            else: begin
              print, 'Unknown node type: ', nodeType
              stop
            end
          endcase
        endfor
      endif 
    endif else begin
      print, "Error! Number of nodes should be less than 1024!"
      return, 0
    endelse

    return, 1
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMNodeList::Cleanup
    if (self.nNodes gt 0) then begin
      for i = 0, self.nNodes-1 do obj_destroy, self.oNodes[i]
    endif
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMNodeList::GetLength
    return, self.nNodes
END
;--------------------------------------------------------------------
FUNCTION IDLffXMLDOMNodeList::Item, index
    if (index lt self.nNodes) then return, self.oNodes[index] else return, obj_new()
END
;--------------------------------------------------------------------
PRO IDLffXMLDOMNodeList__define
    
    MAX_N_NODES = 1024
    
    struct = { IDLffXMLDOMNodeList, $
               name : '', $
               nNodes : 0, $
               oNodes : objarr(MAX_N_NODES) $
             }
END
