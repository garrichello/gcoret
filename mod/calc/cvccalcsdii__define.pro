;
;  NAME: 
;    cvcCalcSDII
;
;  DESCRIPTION:
;    Simple precipitation intensity index
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of precipitation intensity for time series of data
;    (Simple precipitation intensity index)
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcSDII = obj_new('cvcCalcSDII', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcSDII->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcSDII::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcSDII::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcSDII::Run
    self->printLog, "(cvcCalcSDII::Run) Started..."
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
    self->printLog, "(cvcCalcSDII::Run) Get data..."
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
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, 00)
        sumArr = fltarr(size(sResponse.aData[*, *, 0], /dim))
        cntArr = fltarr(size(sResponse.aData[*, *, 0], /dim))
        sum = sResponse.aData[*, *, 0]
        pos = (sum ne sResponse.missingVal)
        sum = sum * pos
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            pos = tmpArr ne sResponse.missingVal
            sum = sum + tmpArr * pos
          endif else begin
            idxs = where(sum ge 1)
            if (idxs[0] ne -1) then begin
              sumArr[idxs] = sumArr[idxs] + sum[idxs]
              cntArr[idxs] = cntArr[idxs] + 1
            endif
            curday = curday + 1
            sum = sResponse.aData[*, *, i]
            pos = sum ne sResponse.missingVal
            sum = sum * pos
          endelse
        endfor
        totSum[*, *, segIdx] = sumArr
        totCnt[*, *, segIdx] = cntArr
      endif else begin ; for stations
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totSum = fltarr(sz[1], numTimeSeg)
          totCnt = fltarr(sz[1], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, 00)
        sumArr = fltarr(size(sResponse.aData[*, 0], /dim))
        cntArr = fltarr(size(sResponse.aData[*, 0], /dim))
        sum = sResponse.aData[*, 0]
        pos = (sum ne sResponse.missingVal)
        sum = sum * pos
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            pos = tmpArr ne sResponse.missingVal
            sum = sum + tmpArr * pos
          endif else begin
            idxs = where(sum ge 1)
            if (idxs[0] ne -1) then begin
              sumArr[idxs] = sumArr[idxs] + sum[idxs]
              cntArr[idxs] = cntArr[idxs] + 1
            endif
            curday = curday + 1
            sum = sResponse.aData[*, i]
            pos = sum ne sResponse.missingVal
            sum = sum * pos
          endelse
        endfor
        totSum[*, segIdx] = sumArr
        totCnt[*, segIdx] = cntArr
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totSum[*, *, 0]
        cnt = totCnt[*, *, 0]
        pos = cnt ne 0
        sum = sum * pos
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totSum[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + totCnt[*, *, segIdx]
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totSum[*, 0]
        cnt = totCnt[*, 0]
        pos = cnt ne 0
        sum = sum * pos
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totSum[*, segIdx]
          pos = totCnt[*, segIdx] ne 0
          cnt = cnt + totCnt[*, segIdx]
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endelse
    endif else begin
      if (calcMode eq 'multi') then begin
        if (sResponse.gridType ne 'station') then begin ; for non-station data
          sz = size(totSum[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totSum[*, *, segIdx] / totCnt[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
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
        self->printLog, '(cvcCalcSDII) Error! Unknown calculation mode: ', mode
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
        self->printLog, '(cvcCalcSDII) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcSDII::GetResult
    return, self.sResult
END
;--------------------------------------------------------------------
PRO cvcCalcSDII__define

    struct = { cvcCalcSDII, $
               INHERITS cvcCalc $ 
             }
END