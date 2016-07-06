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

    ; variable Id we want to process
    dataIdx = 0
    ; level Id to read
    levelIdx = 0
    
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcAreaMean::Run) Get data..."
    cnt = 0 ; number of summed fields
 
    for segIdx = 0, numTimeSeg-1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode      
      
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        nTimes = n_elements(sResponse.aTimes)
        aMean = fltarr(nTimes)
        for i = 0, nTimes-1 do begin
          pos = sResponse.aData[*, *, i] ne sResponse.missingVal
          aMean[i] = total(sResponse.aData[*, *, i]*pos) / total(pos)
        endfor
        
;        res = self->__PushResult(XGRID=sResponse.aTimes, YGRID=aMean, MISSING=sResponse.missingVal)
;        if (self->Assert(res)) then return, res
      endif else begin ; for station data
        res = self->__PushResult(XGRID=sResponse.aTimes, YGRID=sResponse.aData, MISSING=sResponse.missingVal)      
;        res = self->__PushResult(XGRID=sResponse.aTimes[0:5, *], YGRID=sResponse.aData[0:5, *], MISSING=sResponse.missingVal)      
;        res = self->__PushResult(XGRID=(sResponse.aTimes[0, *])[*], YGRID=(sResponse.aData[0, *])[*], MISSING=sResponse.missingVal)      
;        res = self->__PushResult(XGRID=(sResponse.aTimes[1, *])[*], YGRID=(sResponse.aData[1, *])[*], MISSING=sResponse.missingVal)      
;        res = self->__PushResult(XGRID=(sResponse.aTimes[2, *])[*], YGRID=(sResponse.aData[2, *])[*], MISSING=sResponse.missingVal)      
;        res = self->__PushResult(XGRID=(sResponse.aTimes[3, *])[*], YGRID=(sResponse.aData[3, *])[*], MISSING=sResponse.missingVal)      
;        res = self->__PushResult(XGRID=(sResponse.aTimes[4, *])[*], YGRID=(sResponse.aData[4, *])[*], MISSING=sResponse.missingVal)      
        if (self->Assert(res)) then return, res
      endelse
    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0

;    res = (aoOutputs[outIdx])->Put(aMean)
    res = (aoOutputs[outIdx])->SetInfo(XGRID=sResponse.aTimes, YGRID=aMean, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcAreaMean__define

    struct = { cvcCalcAreaMean, $
               INHERITS cvcCalc $ 
             }
END
