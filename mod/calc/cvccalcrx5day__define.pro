;
;  NAME: 
;    cvcCalcRx5Day
;
;  DESCRIPTION:
;    Monthly maximum 5?-day precipitation
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of account of days for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcRx5Day = obj_new('cvcCalcRx5Day', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcRx5Day->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcRx5Day::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcRx5Day::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcRx5Day::Run
    self->printLog, "(cvcCalcRx5Day::Run) Started..."
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

;    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
      
      res = aoParams[0]->Get(UID='threshold', sParam)
      if (self->Assert(res)) then return, res
      calcMode1 = *(sParam.data)
    endif else begin
      calcMode = 'single'
      calcMode1 = 1
    endelse

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcrx5day::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totSum = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        begTimeSpan = julday(mn, dy, yr, 00)
        endTimeSpan = julday(mn, dy + calcMode1 - 1, yr, 23)

; ToDo: Fix the following two lines
;        begIdx = n_elements(where(sResponse.aTimes lt begTimeSpan)) ; strange and wrong!
;        endIdx = n_elements(where(sResponse.aTimes le endTimeSpan)) ; strange and wrong!
; OK, here is a better solution
	foo = min(abs(sResponse.aTimes - begTimeSpan), begIdx)
	foo = min(abs(sResponse.aTimes - endTimeSpan), endIdx)

        numTimeSteps = endIdx - begIdx + 1
        curDataArray = fltarr(sz[1], sz[2], numTimeSteps)
        curCntArray = fltarr(sz[1], sz[2], numTimeSteps)
        iStep = 0
        maxSum = sResponse.aData[*, *, 0]
        begPos = maxSum ne sResponse.missingVal
        curCntArray[*, *, iStep] = sResponse.aData[*, *, 0] ne sResponse.missingVal
        curDataArray[*, *, iStep] = sResponse.aData[*, *, 0] * curCntArray[*, *, iStep]
        for i = 1, n_elements(sResponse.aTimes)-1 do begin 
          if endTimeSpan le asTimeSeg[segIdx].ending then begin
            if (long(sResponse.aTimes[i] - 0.5) lt long(endTimeSpan)) then begin
              iStep = iStep + 1
              curCntArray[*, *, iStep] = sResponse.aData[*, *, i] ne sResponse.missingVal
              curDataArray[*, *, iStep] = sResponse.aData[*, *, i] * curCntArray[*, *, iStep]
            endif else begin
              if (calcMode1 ge 2) then begin
                sum = total(curDataArray, 3)
                pos = total(curCntArray, 3) ne 0
              endif else begin
                sum = curDataArray
                pos = curCntArray ne 0
              endelse
              valPos = ((pos eq 1) and (begPos eq 1))
              idxs = where(sum * valPos gt maxSum * valPos)
              if (idxs[0] ne -1) then maxSum[idxs] = sum[idxs]
              idxs = where((pos - valPos) eq 1)
              if (idxs[0] ne -1) then maxSum[idxs] = sum[idxs]
              begPos = maxSum ne sResponse.missingVal
              begTimeSpan = begTimeSpan + 1
              endTimeSpan = endTimeSpan + 1
              iStep = 0 ;i mod numTimeSteps 
              curCntArray[*, *, iStep] = sResponse.aData[*, *, i] ne sResponse.missingVal 
              curDataArray[*, *, iStep] =  sResponse.aData[*, *, i] * curCntArray[*, *, iStep]  
            endelse
          endif
        endfor
        totSum[*, *, segIdx] = maxSum
      endif else begin ; for stations
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes } 
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totSum = fltarr(sz[1], numTimeSeg)
        endif
        begTimeSpan = julday(mn, dy, yr, 00)
        endTimeSpan = julday(mn, dy + calcMode1 -1, yr, 23)

; ToDo: Fix the following two lines
        begIdx = n_elements(where(sResponse.aTimes[0, *] lt begTimeSpan)) ; strange and wrong!
        endIdx = n_elements(where(sResponse.aTimes[0, *] le endTimeSpan)) ; strange and wrong!
        
        numTimeSteps = endIdx - begIdx
        curDataArray = fltarr(sz[1], numTimeSteps)
        curCntArray = fltarr(sz[1], numTimeSteps)
        iStep = 0
        maxSum = sResponse.aData[*, 0]
        begPos = maxSum ne sResponse.missingVal
        curCntArray[*, iStep] = sResponse.aData[*, 0] ne sResponse.missingVal
        curDataArray[*, iStep] = sResponse.aData[*, 0] * curCntArray[*, iStep]
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin 
          if endTimeSpan le asTimeSeg[segIdx].ending then begin
            if (long(sResponse.aTimes[0, i]) lt long(endTimeSpan)) then begin
              iStep = iStep + 1
              curCntArray[*, iStep] = sResponse.aData[*, i] ne sResponse.missingVal
              curDataArray[*, iStep] = sResponse.aData[*, i] * curCntArray[*, iStep]
            endif else begin
              if (calcMode1 ge 2) then begin
                sum = total(curDataArray, 2)
                pos = total(curCntArray, 2) ne 0
              endif else begin
                sum = curDataArray
                pos = curCntArray ne 0
              endelse
              valPos = ((pos eq 1) and (begPos eq 1))
              idxs = where(sum * valPos gt maxSum * valPos)
              if (idxs[0] ne -1) then maxSum[idxs] = sum[idxs]
              idxs = where((pos - valPos) eq 1)
              if (idxs[0] ne -1) then maxSum[idxs] = sum[idxs]
              begPos = maxSum ne sResponse.missingVal
              begTimeSpan = begTimeSpan + 1
              endTimeSpan = endTimeSpan + 1
              iStep = i mod numTimeSteps 
              curCntArray[*, iStep] = sResponse.aData[*, i] ne sResponse.missingVal 
              curDataArray[*, iStep] =  sResponse.aData[*, i] * curCntArray[*, iStep]
            endelse
          endif
        endfor
        totSum[*, segIdx] = maxSum
      endelse 
    endfor

    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totSum[*, *, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totSum[*, *, segIdx]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totSum[*, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totSum[*, segIdx]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endelse
    endif else begin
      if (calcMode eq 'multi') then begin
        if (sResponse.gridType ne 'station') then begin ; for non-station data
          aDataArray = totSum
        endif else begin ; for station data
          sz = size(totSum[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totSum[*, segIdx] / totCnt[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        self->printLog, '(cvcCalcRx5Day) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse

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
    if (calcMode eq 'single') then begin
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      endif else begin
        self->printLog, '(cvcCalcRx5Day) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
   
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcRx5Day::GetResult
    return, self.sResult
END
;--------------------------------------------------------------------
PRO cvcCalcRx5Day__define

    struct = { cvcCalcRx5Day, $
               INHERITS cvcCalc $ 
             }
END
