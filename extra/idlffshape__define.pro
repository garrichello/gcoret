;
;  NAME: 
;    IDLffShape
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
FUNCTION IDLffShape::Init, in_fileName, in_entityType, kw_update

  if (keyword_set(kw_update)) then self.update = 1 else self.update = 0
  if (n_elements(in_entityType) ne 0) then self.entityType = in_entityType else self.entityType = -1
  if (n_elements(in_fileName) ne 0) then begin
    if (strlen(in_fileName) ne 0) then begin
      res = self->Open(in_fileName)
    endif
  endif
 
  return, 1
END
;--------------------------------------------------------------------
PRO IDLffShape::Cleanup

    self->Close
    
END
;--------------------------------------------------------------------
FUNCTION IDLffShape::Open, in_fileName, dbf_only=in_kwDBFOnly

  if (keyword_set(in_kwDBFOnly)) then self.dbfOnly = 1 else self.dbfOnly = 0
  len = strlen(in_fileName)
  if (len ne 0) then begin
    if (strmid(in_fileName, 0, 1) eq '"') then fileName = strmid(in_fileName, 1, len-2) else fileName = in_fileName ; cut bracing quotes
  endif else begin
    print, "(IDLffShape::Open) No file name specified! Aborting!"
    return, -1
  endelse
  self.fileName = fileName
  res = python('myshape', 'openshape', fileName, self.entityType, self.update)

; if res is success set isOpen attribute
  if (res eq 0) then begin
    if (self.update) then self.isOpen = 3 else self.isOpen = 1
  endif else self.isOpen = 0
  
  return, res
END
;--------------------------------------------------------------------
PRO IDLffShape::Close
  

  if (self.update) then res = python('myshape', 'writeshape')

  if (ptr_valid(self.pAttrStruct)) then ptr_free, self.pAttrStruct
  self.update = 0
  self.nAttributes = 0
  self.fileName = ''
  
  return
END
;--------------------------------------------------------------------
PRO IDLffShape::AddAttribute, in_name, in_type, in_width, precision=in_precision

  curAttIdx = 'ATTRIBUTE_'+strtrim(string(self.nAttributes), 2)
  case in_type of
     3: begin
          attVal = 0L
          pytype = 'N'
          prec = 0
        end
     5: begin
          attVal = 0D
          pytype = 'N'
          prec = 8
        end
     7: begin
          attVal = ""
          pytype = 'C'
          prec = 0
        end
     else: begin
        print, 'IDLffShape::AddAttribute(): Unknown data type '+string(in_type)
        return
     end
  endcase

  if (n_elements(in_precision) ne 0) then prec = in_precision
  
  if (ptr_valid(self.pAttrStruct)) then begin
;    if (strlen(in_name)) then sAttrStruct = create_struct(curAttIdx+'_NAME', in_name, curAttIdx, attVal, *self.pAttrStruct) $
;    else 
    sAttrStruct = create_struct(curAttIdx, attVal, *self.pAttrStruct)
  endif else begin
;    if (strlen(in_name)) then sAttrStruct = create_struct(curAttIdx+'_NAME', in_name, curAttIdx, attVal) $
;    else 
    sAttrStruct = create_struct(curAttIdx, attVal)
  endelse
  
  if (self.update) then res = python('myshape', 'addfield', in_name, pytype, in_width, prec)

  self.pAttrStruct = ptr_new(sAttrStruct)
  self.attributeInfo[self.nAttributes].name = in_name
  self.attributeInfo[self.nAttributes].type = in_type
  self.attributeInfo[self.nAttributes].width = in_width
  self.attributeInfo[self.nAttributes].precision = prec
  self.attributeNames[self.nAttributes] = in_name
  
  self.nAttributes = self.nAttributes + 1

END
;--------------------------------------------------------------------
FUNCTION IDLffShape::GetAttributes, attribute_structure=kw_attStruct, all=kw_all

  if (~self.update) then begin
    nAtts = python('myshape', 'readrec', 0)
;    print, nAtts
    
    for iAtt = 0L, nAtts-1 do begin
      attVal = python('myshape', 'getvalue', iAtt)
      self->AddAttribute, '', size(attVal, /type), strlen(strtrim(attVal, 2))
    endfor
  endif
    
  if (keyword_set(kw_attStruct)) then begin
    if (ptr_valid(self.pAttrStruct)) then return, *self.pAttrStruct else return, ptr_new()
  endif
  
  if (keyword_set(kw_all)) then begin
    nRecs = python('myshape', 'readallrecs')
    asAtts = make_array(nRecs, value=*self.pAttrStruct)
    for iRec = 0L, nRecs-1 do begin
      for jAtt = 0L, nAtts-1 do begin
        attVal = python('myshape', 'getvalue1', iRec, jAtt)
        asAtts[iRec].(nAtts-1-jAtt) = attVal
      endfor
    endfor
    return, asAtts
  endif

END
;--------------------------------------------------------------------
PRO IDLffShape::SetAttributes, in_index, in_value

  nAtts = n_elements(tag_names(in_value))

  res = python('myshape', 'resetrec')

;  for i = 0, nAtts/2-1 do begin
;     val = in_value.(nAtts-i*2-1)
;     res = python('myshape', 'addvalue', val)
;  endfor


  for i = 0, nAtts-1 do begin
     val = in_value.(nAtts-i-1)
     res = python('myshape', 'addvalue', val)
  endfor

  res = python('myshape', 'writerec')

  return
END
;--------------------------------------------------------------------
PRO IDLffShape::PutEntity, in_data
  
  shape_type = in_data.shape_type
  if (shape_type ne 1) then begin
    verts = *in_data.vertices 
    dims = size(verts, /dim)
    verts = reform(verts, dims[1], dims[0])
  endif else verts = in_data.bounds[0:1]

  res = python('myshape', 'writeent', shape_type, float(verts))
  
  return
END
;--------------------------------------------------------------------
FUNCTION IDLffShape::GetEntity, in_index, all = in_kwAll, attributes = in_kwAtts

; get number of entities
  if (keyword_set(in_kwAll)) then begin
    nEnts = python('myshape2', 'getnents')
  endif else nEnts = 1

  print, 'Number of entities is ', nEnts

  asEnts = make_array(nEnts, value={IDL_SHAPE_ENTITY})

; read entities
  if (keyword_set(in_kwAll)) then begin
    for idx = 0, nEnts-1 do begin
      if (~(idx mod 100000)) then print, 'Reading entity # ', idx
      shape_type = python('myshape2', 'readent', idx)
      verts = python('myshape2', 'getverts')
      ; reformatting vertices array to conform IDL array rules
      dims = size(verts, /dim)
      verts = reform(verts, dims[1], dims[0])
      bounds = python('myshape2', 'getbounds')
      asEnts[idx].shape_type = shape_type
      asEnts[idx].ishape = idx
      asEnts[idx].bounds = bounds
      asEnts[idx].n_vertices = dims[0]
      asEnts[idx].vertices = ptr_new(verts)
    endfor
  endif else begin
    entType = python('myshape2', 'readent', in_index)
    verts = python('myshape2', 'getverts')
    ; reformatting vertices array to conform IDL array rules
    dims = size(verts, /dim)
    verts = reform(verts, dims[1], dims[0])
    bounds = python('myshape2', 'getbounds')
    asEnts[0].shape_type = shape_type
    asEnts[0].ishape = idx
    asEnts[0].bounds = bounds
    asEnts[0].n_vertices = dims[0]
    asEnts[0].vertices = ptr_new(verts)
  endelse
stop
  return, asEnts
END
;--------------------------------------------------------------------
PRO IDLffShape::GetProperty, ATTRIBUTE_INFO=out_attInfo, ATTRIBUTE_NAMES=out_attNames, DBF_ONLY=out_dbfOnly, $
 ENTITY_TYPE=out_entType, FILENAME=out_fileName, IS_OPEN=out_isOpen, N_ATTRIBUTES=out_nAttrs, N_ENTITIES=out_nEnts, $
 N_RECORDS=out_nRecs, UPDATE=out_update
 
 out_attInfo = self.attributeInfo
 out_attNames = self.attributeNames
 out_dbfOnly = self.dbfOnly
 out_entType = self.entityType
 out_fileName = self.filename
 out_isOpen = self.isOpen
 out_nAtts = self.nAttributes
 out_nEnts = self.nEntities
 out_nRecs = self.nRecords
 out_update = self.update

END
;--------------------------------------------------------------------
PRO IDLffShape__define
    
    MAX_N_ATTRIBUTES = 32768
    ATTRINFO = { ATTRINFO, NAME : '', TYPE : 0, WIDTH : 0, PRECISION : 0 }
    
    struct = { IDLffShape, $
               pAttrStruct : ptr_new(), $
               attributeInfo : make_array( MAX_N_ATTRIBUTES, value = { ATTRINFO } ), $
               attributeNames : make_array( MAX_N_ATTRIBUTES, value = '' ), $
               dbfOnly : 0, $
               entityType : 0, $
               filename : '', $
               isOpen : 0, $
               nAttributes : 0L, $
               nEntities : 0L, $
               nRecords : 0L, $
               update : 0, $
               INHERITS IDL_SHAPE_ENTITY $
             }
END
