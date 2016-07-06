;
;  NAME: 
;    cvcPlotVectorField
;
;  PURPOSE:
;    High-level class. Provides plotting of results. Vector field.
;
;  INHERITS:
;    cvcPlotVector
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorField::Init, in_oGlobal

    res = self->cvcPlotVector::Init(in_oGlobal)

    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotVectorField::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
    set_plot, 'x'
END
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorField::__PlotVectorShp, in_sData, in_sDestDesc

  entityTypePoint = 1
  entityTypePolyLine = 3
  entityTypePolygon = 5
  typeLong = 3
  typeDouble = 5
  typeString = 7

  set_plot, 'z'

; create shapefile for arrows
  aLons = in_sData[0].aLons
  if (max(aLons) gt 180.) then aLons = (aLons + 180.) mod 360. - 180.
  velovect2, in_sData[0].aData, in_sData[1].aData, aLons, in_sData[0].aLats, /overplot, color = colorFG, $
                   missing = -in_sData[0].missingVal, length = 2.0, arrows=arrows, magnitude=mag

  arrowsSize = size(arrows)
  
  nEnt = arrowsSize[3]
  
;  oNewShape = obj_new('IDLffShape', in_sDestDesc.file.name, /update, entity_type=entityTypePolyLine)  
  oNewShape = obj_new('IDLffShape', in_sDestDesc.file.name, entityTypePolyLine, 1)  

  oNewShape->AddAttribute, 'ID', typeLong, 10  
  oNewShape->AddAttribute, 'ID2', typeDouble, 20
  oNewShape->AddAttribute, 'F_CODE', typeString, 10  
  oNewShape->AddAttribute, 'F_CODE_DES', typeString, 25
  oNewShape->AddAttribute, 'NAM', typeString, 25
  
  for entIdx = 0L, nEnt-1 do begin
    sNewEnt = {IDL_SHAPE_ENTITY}
    sNewEnt.SHAPE_TYPE = long(entityTypePolyLine)
    sNewEnt.ISHAPE = long(entIdx)
    sNewEnt.BOUNDS[0] = min(arrows[0, *, entIdx])
    sNewEnt.BOUNDS[1] = min(arrows[1, *, entIdx])
    sNewEnt.BOUNDS[2] = 0.00000000
    sNewEnt.BOUNDS[3] = 0.00000000 
    sNewEnt.BOUNDS[4] = max(arrows[0, *, entIdx]) 
    sNewEnt.BOUNDS[5] = max(arrows[1, *, entIdx]) 
    sNewEnt.BOUNDS[6] = 0.00000000 
    sNewEnt.BOUNDS[7] = 0.00000000 
    sNewEnt.N_VERTICES = 5
    sNewEnt.VERTICES = ptr_new(arrows[*, *, entIdx])
  
    newAttr = oNewShape ->GetAttributes(/ATTRIBUTE_STRUCTURE)
    newAttr.ATTRIBUTE_0 = entIdx
    newAttr.ATTRIBUTE_1 = mag[entIdx]
    newAttr.ATTRIBUTE_2 = 'CA010'
    newAttr.ATTRIBUTE_3 = 'Vector field'
    newAttr.ATTRIBUTE_4 = 'Wind [m/s]'
  
    oNewShape->PutEntity, sNewEnt
    oNewShape->SetAttributes, entIdx, newAttr
    
  endfor    
  if (obj_valid(oNewShape)) then obj_destroy, oNewShape

  set_plot, 'x'
    
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotVectorField::Run, in_pInputs

;    self.pInputs = in_pInputs

    ; where to plot
    idx = where((*in_pInputs).type eq 'destination')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: No destination!'
      return, -1
    endif
    sDestDesc = *((*in_pInputs)[idx[0]].data)
    
    ; what to plot (only one and the first dataset is plotted!!!)
    idx = where((*in_pInputs).type eq 'data')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: Nothing to plot!'
      return, -1
    endif
    if (n_elements(idx) ne 2) then begin
      self->printLog, '(cvcPlotVector::Run) Error: Vector field output module expects 2 input datasets: U and V, however ', strtrim(string(n_elements(sData)), 2), ' dataset(s) are given!'
      return, -1
    endif 

    apData = (*in_pInputs)[idx].data
    
;    stop
;    if (n_elements(sData) ne 2) then begin
;      self->printLog, '(cvcPlotVector::Run) Error: Vector field output module expects 2 input datasets: U and V, however ', strtrim(string(n_elements(sData)), 2), ' dataset(s) are given!'
;      return, -1
;    endif 
        
    if (sDestDesc.file.name ne '') then begin
      outFileType = sDestDesc.file.type 
    endif else begin
      outFileType = 'x' ; no file name - no file. output to screen then.
    endelse

    ; U-component
    ret = self->__Preprocess2D(*apData[0], sDestDesc, sPlotDataU)
    if (self->Assert(ret)) then return, ret
    
    ; V-component
    ret = self->__Preprocess2D(*apData[1], sDestDesc, sPlotDataV)
    if (self->Assert(ret)) then return, ret

    case outFileType of
      'x': begin
             ret = self->__OpenX(sDestDesc)
             ret = self->__PlotMap([sPlotDataU, sPlotDataV], sDestDesc)
           end
      'eps': begin
               ret = self->__OpenEPS(sDestDesc)
               ret = self->__PlotMap([sPlotDataU, sPlotDataV], sDestDesc)
               device, /close
               set_plot, 'x'
             end
      'shape': ret = self->__PlotVectorShp([sPlotDataU, sPlotDataV], sDestDesc)
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotVectorField__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotVectorField, $
                  INHERITS cvcPlotVector $
             }
END