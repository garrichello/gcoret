;
;  NAME: 
;    cvcCalc
;
;  PURPOSE:
;    Base class. Should be inherited to define processing classes.
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcCalc::Init, in_asInputs, in_asOutputs, in_oGlobal

    self.oGlobal = in_oGlobal
    hLogFile = self.oGlobal->Get('hLogFile')
    if (self->cvcError::Init(hLogFile) eq 0) then return, 0
    
    acceptedInputs = ['dataset', 'datafile', 'array']
    self.nInputs = 0
    self.nParams = 0
    self.hLogFile = hLogFile
    for iInput = 0, n_elements(in_asInputs)-1 do begin
      if (ptr_valid(in_asInputs[iInput].data)) then begin
;        if (((*in_asInputs[iInput].data).type eq 'dataset') or ((*in_asInputs[iInput].data).type eq 'datafile') or ((*in_asInputs[iInput].data).type eq 'array')) then begin
        if ( (where(acceptedInputs eq (*in_asInputs[iInput].data).type))[0] ne -1 ) then begin
          self.aoInputs[self.nInputs] = obj_new('cvcDataAccess', in_oGlobal)
          resultCode = self.aoInputs[self.nInputs]->Open(in_asInputs[iInput])
          if (self->Assert(resultCode)) then return, resultCode
          self.nInputs = self.nInputs + 1
        endif else if ((*in_asInputs[iInput].data).type eq 'parameter') then begin
          self.aoParams[self.nParams] = obj_new('cvcDataAccess', in_oGlobal)
          resultCode = self.aoParams[self.nParams]->Open(in_asInputs[iInput])
          if (self->Assert(resultCode)) then return, resultCode
          self.nParams = self.nParams + 1
        endif else begin
          self->printLog, 'Error! Input type '+(*in_asInputs[iInput].data).type+' is unknown. Aborting!'
          return, -1
        endelse
      endif else begin
        self->printLog, 'Input is undefined! Aborting!'
        return, -1
      endelse
    endfor

    self.nOutputs = n_elements(in_asOutputs)
    self.aoOutputs = objarr(self.nOutputs)
    for iOutput = 0, self.nOutputs-1 do begin
      self.aoOutputs[iOutput] = obj_new('cvcDataAccess', in_oGlobal)
      resultCode = self.aoOutputs[iOutput]->Open(in_asOutputs[iOutput])
      if (self->Assert(resultCode)) then return, resultCode
    endfor

    return, 1
END
;--------------------------------------------------------------------
PRO cvcCalc::Cleanup

  for i = 0, self.nInputs-1 do begin
    if (obj_valid(self.aoInputs[i])) then obj_destroy, self.aoInputs[i]
  endfor

  for i = 0, self.nOutputs-1 do begin
    if (obj_valid(self.aoOutputs[i])) then obj_destroy, self.aoOutputs[i]
  endfor

END
;--------------------------------------------------------------------
FUNCTION cvcCalc::Run, value

    self->printLog, 'Function Run should be implemented!'
    return, 1
END
;--------------------------------------------------------------------
FUNCTION cvcCalc::GetInputs, out_data, out_n
    if (self.nInputs gt 0) then out_data = self.aoInputs[0:self.nInputs-1] else out_data = obj_new()
    out_n = self.nInputs
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalc::GetOutputs, out_data, out_n
    if (self.nOutputs gt 0) then out_data = self.aoOutputs[0:self.nOutputs-1] else out_data = obj_new()
    out_n = self.nOutputs
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalc::GetParams, out_data, out_n
    if (self.nParams gt 0) then out_data = self.aoParams[0:self.nParams-1] else out_data = obj_new()
    out_n = self.nParams
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalc::BandNormalize, inout_band, LIMIT=in_limit

    if (n_elements(in_limit) eq 0) then in_limit = 0.1
    
    hist = histogram(inout_band, min = 1)
    maxhist = float(max(hist))
    idx = where((hist / maxhist) gt in_limit) 
    if (idx[0] ne -1) then begin 
      minval = idx[0] 
      maxval = idx[n_elements(idx)-1]
    endif else return, -1
    
    minidx = where(inout_band lt minval)
    maxidx = where(inout_band gt maxval)
    maxval = maxval - minval
    inout_band = (inout_band - minval) * 253.0 / maxval + 1
    if (minidx[0] ne -1) then inout_band[minidx] = 0
    if (maxidx[0] ne -1) then inout_band[maxidx] = 255
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalc__define

    MAX_N_INPUTS = 128
    MAX_N_OUTPUTS = 128
    MAX_N_PARAMS = 128
    
    struct = { cvcCalc, $
	       oGlobal : obj_new(), $
               nInputs : 0, $
               aoInputs : objarr(MAX_N_INPUTS), $
               nOutputs : 0, $
               aoOutputs : objarr(MAX_N_OUTPUTS), $
               nParams : 0, $
               aoParams : objarr(MAX_N_PARAMS), $
               INHERITS cvcError, $
               INHERITS cvcDate $ 
             }
END
