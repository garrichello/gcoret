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
    
    ; variable Id we want to process
    dataId = 0
    ; level Id to read
    levelId = 0
    retCode = self->GetInfo(dataId, OUT_NTIMESEG=numTimeSeg, IN_LEVELIDX=levelId, OUT_LEVEL=level, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcAreaMean::Run) Get data..."
    cnt = 0 ; number of summed fields
 
    for segIdx = 0, numTimeSeg-1 do begin
      retCode = self->GetInfo(dataId, IN_TIMESEGIDX=segIdx, OUT_TIMESEG=sTimeSeg) ; take one time segment at a time
      if (self->Assert(retCode)) then return, retCode

      resultCode = self.oData->Get(dataId, Time=sTimeSeg, Level=level, Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode

      if (sResponse.gridType ne 'station') then begin ; for non-station data
        nTimes = n_elements(sResponse.aTimes)
        aMean = fltarr(nTimes)
        for i = 0, nTimes-1 do begin
          pos = sResponse.aData[*, *, i] ne sResponse.missingVal
          aMean[i] = total(sResponse.aData[*, *, i]*pos) / total(pos)
        endfor
        res = self->__PushResult(XGRID=sResponse.aTimes, YGRID=aMean, MISSING=sResponse.missingVal)
        if (self->Assert(res)) then return, res
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

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcAreaMean__define

    struct = { cvcCalcAreaMean, $
               INHERITS cvcCalc $ 
             }
END
