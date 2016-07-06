;
;  NAME: 
;    cvcPlotVector
;
;  PURPOSE:
;    High-level class. Governs plotting and writing of results. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotVector::Init, in_oGlobal

    res = self->cvcPlot::Init(in_oGlobal)
    
    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotVector::Cleanup
 
END
;--------------------------------------------------------------------
FUNCTION cvcPlotVector::Run, in_pInputs

    idx = where((*in_pInputs).type eq 'data', nidxs)
    if (nidxs eq 0) then begin
      self->printLog, 'Error: No data to plot!'
      return, -1
    endif
    
    ; get the data grid type
    gridType = (*((*in_pInputs)[idx[0]].data)).gridtype

    ; generate output class name
    if (gridType eq 'station') then outputClass = 'cvcPlotVectorStation'
    if ((gridType eq 'regular') or (gridType eq 'irregular')) then begin
      if (n_elements(idx) eq 1) then outputClass = 'cvcPlotVectorContour' else $
        if (n_elements(idx) eq 2) then outputClass = 'cvcPlotVectorField' else begin
          self->printLog, "(cvcPlotVector) Too many inputs. Don't know what to do! Aboorting..."
          return, -1
        endelse
    endif

; try to create object of a plotting module
    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_BADMODULENAME, outputClass)
    endif
;      

    oOutput = obj_new( outputClass, self.oGlobal )
;      
    catch, /cancel
      
; try to execute Run method of the processing module      
    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      if (obj_valid(oOutput)) then obj_destroy, oOutput
      return, self->SayError(self.ERROR_ERRORINMODULE, outputClass)
    endif    
;     
    retCode = oOutput->Run( in_pInputs )
    catch, /cancel

    if (obj_valid(oOutput)) then obj_destroy, oOutput

; check result code
    if (self->Assert(retCode)) then return, self.ERROR_ERRORINMODULE
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotVector__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotVector, $
                  INHERITS cvcPlot $
             }
END