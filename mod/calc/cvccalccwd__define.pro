;
;  NAME: 
;    cvcCalcCWD
;
;  DESCRIPTION:
;    Maximum number of consecutive wet days (RR >= 1mm)
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of number of consecutive wet days for time series of data
;    (Maximum lenght of wet spell, maximum number of consecutive days with RR >= 1mm)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcCWD = obj_new('cvcCalcCWD', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcCWD->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcCWD::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcCWD::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcCWD::Run
    self->printLog, "(cvcCalcCWD::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; get parameters
    res = self->GetParams(aoParams, nParams)

    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
    endif else calcMode = 'single'
    
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
          totCWD = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        curCWD = fltarr(size(sResponse.aData[*, *, 0], /dim))
        maxCWD = fltarr(size(sResponse.aData[*, *, 0], /dim))
        segCnt = fltarr(size(sResponse.aData[*, *, 0], /dim))
        sum = sResponse.aData[*, *, 0]
        cnt = long(sum ne sResponse.missingVal)
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            sum = sum + tmpArr * pos
          endif else begin
            valPos = cnt ne 0
            segCnt = segCnt + valPos
            idxs = where(sum ge 1)
            if (idxs[0] ne -1) then curCWD[idxs] = curCWD[idxs] + 1
            if (idxs[0] ne - 1) then valPos[idxs] = 0
            idxs = where(curCWD * valPos gt maxCWD * valPos)
            if (idxs[0] ne -1) then maxCWD[idxs] = curCWD[idxs]
            tmpPos = replicate(1, size(sResponse.aData[*,*,0], /dim))
            curCWD = curCWD * (tmpPos - valPos)
            curDay = curDay + 1
            sum = sResponse.aData[*, *, i]
            cnt = long(sum ne sResponse.missingVal)
            sum = sum * cnt
          endelse
        endfor
        valPos = cnt ne 0
        segCnt = segCnt + valPos
        idxs = where(sum lt 1)
        if (idxs[0] ne -1) then curCWD[idxs] = curCWD[idxs] + 1
        idxs = where(curCWD gt maxCWD)
        if (idxs[0] ne -1) then maxCWD[idxs] = curCWD[idxs]
        idxs = where(segCnt eq 0)
        if (idxs[0] ne -1) then maxCWD[idxs] = sResponse.missingVal
        totCWD[*, *, segIdx] = maxCWD
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totCWD = fltarr(sz[1], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        curCWD = fltarr(size(sResponse.aData[*, 0], /dim))
        maxCWD = fltarr(size(sResponse.aData[*, 0], /dim))
        segCnt = fltarr(size(sResponse.aData[*, 0], /dim))
        sum = sResponse.aData[*, 0]
        cnt = long(sum ne sResponse.missingVal)
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i]) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            sum = sum + tmpArr * pos
          endif else begin
            valPos = cnt ne 0
            segCnt = segCnt + valPos
            idxs = where(sum ge 1)
            if (idxs[0] ne -1) then curCWD[idxs] = curCWD[idxs] + 1
            if (idxs[0] ne - 1) then valPos[idxs] = 0
            idxs = where(curCWD * valPos gt maxCWD * valPos)
            if (idxs[0] ne -1) then maxCWD[idxs] = curCWD[idxs]
            tmpPos = replicate(1, size(sResponse.aData[*, 0], /dim))
            curCWD = curCWD * (tmpPos - valPos)
            curDay = curDay + 1
            sum = sResponse.aData[*, i]
            cnt = long(sum ne sResponse.missingVal)
            sum = sum * cnt
          endelse
        endfor
        valPos = cnt ne 0
        segCnt = segCnt + valPos
        idxs = where(sum ge 1)
        if (idxs[0] ne -1) then curCWD[idxs] = curCWD[idxs] + 1
        idxs = where(curCWD gt maxCWD)
        if (idxs[0] ne -1) then maxCWD[idxs] = curCWD[idxs]
        idxs = where(segCnt eq 0)
        if (idxs[0] ne -1) then maxCWD[idxs] = sResponse.missingVal
        totCWD[*, segIdx] = maxCWD
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totCWD[*, *, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totCWD[*, *, segIdx]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totCWD[*, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totCWD[*, segIdx]
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
        self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
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
        self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
   
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcCWD::GetResult
    return, self.sResult
END
;--------------------------------------------------------------------
PRO cvcCalcCWD__define

    struct = { cvcCalcCWD, $
               INHERITS cvcCalc $ 
             }
END