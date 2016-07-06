;
;  NAME: 
;    cvcCalcFrostDays
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of frost days for time series of data
;    (annual count of days when daily minimum temperature less than 0 C)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcFrostDays = obj_new('cvcCalcFrostDays', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcFrostDays->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcFrostDays::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcFrostDays::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcFrostDays::Run

    self->printLog, "(cvcCalcGSL::Run) Started..."
    
    constK = 273.15
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; get parameters
    res = self->GetParams(aoParams, nParams)
    calcMode = 'single'
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
      if (res ne -1) then calcMode = *(sParam.data)
    endif

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcGSL::Run) Get data..."
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
          totFD = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        minArr = sResponse.aData[*, *, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
        curDay = julday(mn, dy, yr, hr)
        fdArr = fltarr(size(sResponse.aData[*,*,0], /dim))
        begPos = cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) lt minArr * (curPos - difPos))
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            idxs = where(tmpArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = minArr[idxs]
            idxs = where(minArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            begPos = (minArr ne sResponse.missingVal)
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0)
            idxs = where(minArr * pos lt 0+constK)
            if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1     
            curDay = curDay + 1
            minArr = sResponse.aData[*, *, i]
            cnt = minArr ne sResponse.missingVal
            begPos = cnt
            totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]            
          endelse
        endfor
        pos = (cnt ne 0)
        idxs = where(minArr * pos lt 0+constK)
        if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1
        totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]
        missingIdxs = where(cnt eq 0)
        if (missingIdxs[0] ne -1) then fdArr[missingIdxs] = sResponse.missingVal 
        totFD[*, *, segIdx]= fdArr
      endif else begin ; for station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totFD = fltarr(sz[1], numTimeSeg)
          totCnt = fltarr(sz[1], numTimeSeg)
        endif
        minArr = sResponse.aData[*, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
        curDay = julday(mn, dy, yr, hr)
        fdArr = fltarr(size(sResponse.aData[*,0], /dim))
        begPos = cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) lt minArr * (curPos - difPos))
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            idxs = where(tmpArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = minArr[idxs]
            idxs = where(minArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            begPos = (minArr ne sResponse.missingVal)
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0)
            idxs = where(minArr * pos lt 0+constK)
            if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1     
            curDay = curDay + 1
            minArr = sResponse.aData[*, i]
            cnt = minArr ne sResponse.missingVal
            begPos = cnt
            totCnt[*, segIdx] = totCnt[*, segIdx] + pos            
          endelse
        endfor
        pos = (cnt ne 0)
        idxs = where(minArr * pos lt 0+constK)
        if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1
        totCnt[*, segIdx] = totCnt[*, segIdx] + pos
        missingIdxs = where(cnt eq 0)
        if (missingIdxs[0] ne -1) then fdArr[missingIdxs] = sResponse.missingVal 
        totFD[*, segIdx]= fdArr
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totFD[*, *, 0]
        cnt = long(totCnt[*, *, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totFD[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totFD[*, 0]
        cnt = long(totCnt[*, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totFD[*, segIdx]
          pos = totCnt[*, segIdx] ne 0
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
          sz = size(totFD[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totFD[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totFD[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totFD[*, segIdx]
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
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
      endif else begin
        self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res

     return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcFrostDays::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcFrostDays__define
   struct = { cvcCalcFrostDays, $
               INHERITS cvcCalc $
             }
end
