;
;  NAME: 
;    cvcOutput
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
FUNCTION cvcOutput::Init, in_asInputs, in_asOutputs, in_oGlobal

    self.pInputs = ptr_new(in_asInputs)

    self.oGlobal = in_oGlobal
    hLogFile = self.oGlobal->Get('hLogFile')
    res = self->cvcError::Init(hLogFile)
    
    self.hLogFile = hLogFile
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcOutput::Cleanup
 
END
;--------------------------------------------------------------------
FUNCTION cvcOutput::Run

    idx = where((*self.pInputs).type eq 'destination')
    if (idx eq -1) then begin
      self->printLog, 'Error: No destination!'
      return, -1
    endif
    
    ; check the destination type
    destType = (*((*self.pInputs)[idx].data)).type

    ; generate output class name
    case destType of
      'image': outputClass = 'cvcPlot'+(*((*self.pInputs)[idx].data)).graphics.kind
      'raw': outputClass = 'cvcWrite'
      else: begin
        self->printLog, 'Unknown destination type:', destType
        return, -1
      end
    endcase
    
; try to create object of a plotting module
    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_BADMODULENAME, plotClass)
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
    retCode = oOutput->Run( self.pInputs )
    catch, /cancel

    if (obj_valid(oOutput)) then obj_destroy, oOutput

; check result code
    if (self->Assert(retCode)) then return, self.ERROR_ERRORINMODULE
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcOutput::GetOutput, out_data, out_num 
    out_data = self.apResults
    out_num = self.nResults
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcOutput__define

    MAX_N_RESULTS = 128

    struct = { cvcOutput, $
		  oGlobal : obj_new(), $ ; global data
                  pInputs : ptr_new(), $
                  apResults : make_array(MAX_N_RESULTS, value=ptr_new()), $ ; array of pointers to resulting arrays
                  nResults : 0, $ ; number of results
                  INHERITS cvcError $
             }
END