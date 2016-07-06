;
;  NAME: 
;    cvcCalcVariance
;
;  DESCRIPTION:
;    Time series variance
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of variance for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcVariance = obj_new('cvcCalcVariance', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcVariance->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcVariance::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcVariance::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcVariance::Run

    self->printLog, "(cvcCalcVariance::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcVariance::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          averArray = fltarr(sz[1], sz[2], numTimeSeg)
          totSum = fltarr(sz[1], sz[2])
          totCnt = fltarr(sz[1], sz[2])
        endif
        sum = sResponse.aData[*, *, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          tmpArr = sResponse.aData[*, *, i]
          pos = (tmpArr ne sResponse.missingVal)
          cnt = cnt + pos
          sum = sum + tmpArr * pos
        endfor
        totSum = totSum + sum 
        totCnt = totCnt + cnt
        sum = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
        averArray[*, *, segIdx]= sum
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes } 
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          averArray = fltarr(sz[1], numTimeSeg)
          totSum = fltarr(sz[1])
          totCnt = fltarr(sz[1])    
        endif
        sum = sResponse.aData[*, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          tmpArr = sResponse.aData[*, i]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos
        endfor
        totSum = totSum + sum 
        totCnt = totCnt + cnt
        sum = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
        averArray[*, segIdx]= sum
      endelse
    endfor
    totSum = totSum / totCnt
    idxs = where(totCnt eq 0)
    if (idxs[0] ne -1) then totSum[idxs] = sResponse.missingVal 

    ; Calculation of standard deviation value
    if (sResponse.gridType ne 'station') then begin ; for non-station data
      tmpArr = averArray[*, *, 0]
      cnt = tmpArr ne sResponse.missingVal
      aStDev = (tmpArr * cnt - totSum * cnt)^2 
      for segIdx = 1, numTimeSeg - 1 do begin
        tmpArr = averArray[*, *, segIdx]
        pos = tmpArr ne sResponse.missingVal
        cnt = cnt + pos
        aStDev = aStDev + (tmpArr * pos - totSum * pos)^2
      endfor
    endif else begin ; for station data
      tmpArr = averArray[*, 0]
      cnt = tmpArr ne sResponse.missingVal
      aStDev = (tmpArr * cnt - totSum * cnt)^2      
      for segIdx = 1,numTimeSeg - 1 do begin
        tmpArr = averArray[*, segIdx]
        pos = tmpArr ne sResponse.missingVal
        cnt = cnt + pos
        aStDev = aStDev + (tmpArr * pos - totSum * pos)^2
      endfor
    endelse
    aDataArray = aStDev / cnt
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal
    
;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
    midSeg = fltarr(numTimeSeg)
    for segIdx = 0, numTimeSeg - 1 do begin
      self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
      self->DayMonthYear, asTimeSeg[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
      midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcVariance::GetResult
    return, self.sResult
END
;--------------------------------------------------------------------
PRO cvcCalcVariance__define

    struct = { cvcCalcVariance, $
               INHERITS cvcCalc $ 
             }
END
