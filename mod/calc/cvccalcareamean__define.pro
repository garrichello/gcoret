;
;  NAME: 
;    cvcCalcAreaMean
;
;  PURPOSE:
;    Sample class. Implements calculation of an area mean for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTimeMean = obj_new('cvcCalcAreaMean', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTimeMean->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcAreaMean::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcAreaMean::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcAreaMean::Run

 
    self->printLog, "(cvcCalcAreaMean::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

;    ; get parameters
    res = self->GetParams(aoParams, nParams)

    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='areaMean', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
    endif else calcMode = 'segment'
    
    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcAreaMean::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    for segIdx = 0, numTimeSeg-1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode

      if (sResponse.gridType ne 'station') then begin ; for non-station data
        nTimes = n_elements(sResponse.aTimes)
        if (segIdx eq 0) then begin
          totMean = fltarr(nTimes, numTimeSeg)
        endif
        aMean = fltarr(nTimes)
        for iTime = 0, nTimes - 1 do begin
          pos = sResponse.aData[*, *, iTime] ne sResponse.missingVal
          if (total(pos) eq 0) then begin
            aMean[iTime] = sResponse.missingVal
	  endif else begin
            else aMean[iTime] = total(sResponse.aData[*, *, iTime]*pos) / total(pos)
          endelse
        endfor
        totMean[*, segIdx] = aMean
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        nTimes = n_elements(sResponse.aTimes)
        if (segIdx eq 0) then begin
          totMean = fltarr(nTimes, numTimeSeg)
        endif
        aMean = fltarr(nTimes)
        for iTime = 0, nTimes - 1 do begin
          pos = sResponse.aData[*, iTime] ne sResponse.missingVal
          if (total(pos) eq 0) then begin
            aMean[iTime] = sResponse.missingVal
          endif else begin
            aMean[iTime] = total(sResponse.aData[*, iTime]*pos) / total(pos)
          endelse
        endfor
        totMean[*, segIdx] = aMean       
      endelse
    endfor

    if (calcMode eq 'data') then begin
      pos = totMean[*, 0] ne sResponse.missingVal
      aDataArray = total(totMean[*, 0] * pos) / total(pos)
      cnt = total(pos) / total(pos) ; WTF?
      for segIdx = 1, numTimeSeg - 1 do begin
        pos = totMean[*, segIdx] ne sResponse.missingVal
        aDataArray = aDataArray + total(totMean[*, segIdx] * pos) / total(pos)
        cnt = cnt + total(pos) / total(pos)     
      endfor
      aDataArray = aDataArray / cnt
      idxs = where(cnt eq 0)
      if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
    endif else begin    
      if (calcMode eq 'segment') then begin

        aDataArray = totMean

      endif else begin
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0
;    print, aDataArray

;    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    if (calcMode eq 'data') then begin
      res = (aoOutputs[outIdx])->SetInfo(XGRID=numTimeSeg, YGRID=aDataArray, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endif else begin
      res = (aoOutputs[outIdx])->SetInfo(XGRID=sResponse.aTimes, YGRID=aDataArray, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endelse
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK

END
;--------------------------------------------------------------------
PRO cvcCalcAreaMean__define

    struct = { cvcCalcAreaMean, $
               INHERITS cvcCalc $ 
             }
END
