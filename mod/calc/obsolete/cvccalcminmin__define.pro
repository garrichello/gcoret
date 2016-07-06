;
;  NAME: 
;    cvcCalcMinMin
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of minimum values for time series of data
;    (Monthly minimum of daily minimum temperature)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMinMin = obj_new('cvcCalcMinMin', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcMinMin->Run()
;
;     sResponse structure contains:
;       aData - data array, dimensions: [nLon, nLat, nTime] - for non stations, [nStations, nTime] - for stations
;       aLons - longitudes array, dimensions: [nLons] - for regular, [nLons, nLats] - for non-regular, [nStations] - for stations 
;       aLats - latitudes array, dimensions: [nLats] - for regular, [nLons, nLats] - for non-regular, [nStations] - for stations
;       aTimes - times (JD) array, dimensions: [nTimes]
;       minVal - minimum data value
;       maxVal - maximum data value
;       missingVal - missing data value
;       sTimeRng - date-time range of data returned
;       aLev - array of levels, dimensions: [nLev]
;       nLev - number of levels
;       resultCode - result code
;--------------------------------------------------------------------
FUNCTION cvcCalcMinMin::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcMinMin::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
PRO cvcCalcMinMin::regDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totMin, totCnt
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    begDay = julday(mn, dy, yr, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr, mn, dy, hr
    endDay = julday(mn, dy, yr, hr) 
    numDays = fix(endDay - begDay + 1)
       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMin = fltarr(sz[1], sz[2], numTimeSeg, numDays)
      totCnt = fltarr(sz[1], sz[2], numTimeSeg, numDays)
    endif
    dayIdx = 0
    minArr = sResponse.aData[*, *, 0]
    cnt = long(minArr ne sResponse.missingVal)
    minCnt = cnt
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      if (long(sResponse.aTimes[i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, *, i]
        pos = long(tmpArr ne sResponse.missingVal)
        minPos = long(tmpArr * pos * cnt lt minArr * pos * cnt)
        addPos = long(pos - cnt eq 1)
        idxs = where(minPos + addPos eq 1)
        if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
        cnt = pos
        minCnt = minCnt + pos
      endif else begin
        idxs = where(minCnt eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totMin[*, *, segIdx, dayIdx] = minArr 
        totCnt[*, *, segIdx, dayIdx] = long(minArr ne sResponse.missingVal)
        minArr = sResponse.aData[*, *, i]
        cnt = long(minArr ne sResponse.missingVal)
        minCnt = cnt
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
    totMin[*, *, segIdx, dayIdx] = minArr 
    totCnt[*, *, segIdx, dayIdx] = long(minArr ne sResponse.missingVal)
      
END
;--------------------------------------------------------------------
PRO cvcCalcMinMin::stDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totMin, totCnt
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    begDay = julday(mn, dy, yr, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr, mn, dy, hr
    endDay = julday(mn, dy, yr, hr) 
    numDays = fix(endDay - begDay + 1)
       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMin = fltarr(sz[1], numTimeSeg, numDays)
      totCnt = fltarr(sz[1], numTimeSeg, numDays)
    endif
    dayIdx = 0
    minArr = sResponse.aData[*, 0]
    cnt = long(minArr ne sResponse.missingVal)
    minCnt = cnt
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      if (long(sResponse.aTimes[0, i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, i]
        pos = long(tmpArr ne sResponse.missingVal)
        minPos = long(tmpArr * pos * cnt lt minArr * pos * cnt)
        addPos = long(pos - cnt eq 1)
        idxs = where(minPos + addPos eq 1)
        if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
        cnt = pos
        minCnt = minCnt + pos
      endif else begin
        idxs = where(minCnt eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totMin[*, segIdx, dayIdx] = minArr 
        totCnt[*, segIdx, dayIdx] = long(minArr ne sResponse.missingVal)
        minArr = sResponse.aData[*, i]
        cnt = long(minArr ne sResponse.missingVal)
        minCnt = cnt
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
    totMin[*, segIdx, dayIdx] = minArr 
    totCnt[*, segIdx, dayIdx] = long(minArr ne sResponse.missingVal)
END
;--------------------------------------------------------------------
PRO cvcCalcMinMin::regTimeMin, calcMode, totMin, numTimeSeg, numDays, missingValue, aDataArray
    if (calcMode eq 'dayMin') or (calcMode eq 'segMin') or (calcMode eq 'dataMin') or (calcMode eq 'tiMean') then begin
      aDataArray = totMin
      if (calcMode eq 'segMin') or (calcMode eq 'dataMin') or (calcMode eq 'tiMean')  then begin
        curDataArray = aDataArray
        aDataArray = fltarr(size(curDataArray[*, *, *, 0], /dim))
        for segIdx = 0, numTimeSeg - 1 do begin
          aSegMin = curDataArray[*, *,segIdx, 0]
          cnt = long(aSegMin ne missingValue)
          aSegMin = aSegMin * cnt
          for i = 1, numDays - 1 do begin
            tmpArr = curDataArray[*, *, segIdx, i]
            pos = long(tmpArr ne missingValue)
            idxs = where(tmpArr * pos lt aSegMin * pos)
            if (idxs[0] ne -1) then aSegMin[idxs] = tmpArr[idxs]
            cnt = cnt + pos
          endfor
          idxs = where(cnt eq 0)
          if (idxs[0] ne -1) then aSegMin[idxs] = missingValue
          aDataArray[*, *, segIdx] = aSegMin
        endfor 
        if (calcMode eq 'dataMin') then begin
          curDataArray = aDataArray
          aDataMin = curDataArray[*, *, 0]
          cnt = long(aDataMin ne missingValue)
          aDataMin = aDataMin * cnt
          for segIdx = 1, numTimeSeg - 1 do begin
            tmpArr = curDataArray[*, *, segIdx]
            pos = long(tmpArr ne missingValue)
            idxs = where(tmpArr * pos lt aDataMin * pos)
            if (idxs[0] ne -1) then aDataMin[idxs] = tmpArr[idxs]
            cnt = cnt + pos
          endfor
          idxs = where(cnt eq 0)
          if (idxs[0] ne -1) then aDataMin[idxs] = missingValue
          aDataArray = aDataMin
        endif
        if (calcMode eq 'tiMean') then begin
          sum = aDataArray[*, *, 0]
          cnt = long(sum ne missingValue)
          sum = sum * cnt
          for segIdx = 1, numTimeSeg - 1 do begin
            tmpArr = aDataArray[*, *, segIdx]
            pos = tmpArr ne missingValue
            cnt = cnt + pos
            sum = sum + tmpArr * pos  
          endfor
          aDataArray = sum / cnt
          idxs = where(cnt eq 0)
          if (idxs[0] ne -1) then aDataArray[idxs] = missingValue 
        endif      
      endif
    endif else begin
      print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
      return
    endelse

END
;--------------------------------------------------------------------
PRO cvcCalcMinMin::stTimeMin, calcMode, totMin, numTimeSeg, numDays, missingValue, aDataArray
    if (calcMode eq 'dayMin') or (calcMode eq 'segMin') or (calcMode eq 'dataMin') or (calcMode eq 'tiMean') then begin
      aDataArray = totMin
      if (calcMode eq 'segMin') or (calcMode eq 'dataMin') or (calcMode eq 'tiMean')  then begin
        curDataArray = aDataArray
        aDataArray = fltarr(size(curDataArray[*, *, 0], /dim))
        for segIdx = 0, numTimeSeg - 1 do begin
          aSegMin = curDataArray[*, segIdx, 0]
          cnt = long(aSegMin ne missingValue)
          aSegMin = aSegMin * cnt
          for i = 1, numDays - 2 do begin
            tmpArr = curDataArray[*, segIdx, i]
            pos = long(tmpArr ne missingValue)
            idxs = where(tmpArr * pos lt aSegMin * pos)
            if (idxs[0] ne -1) then aSegMin[idxs] = tmpArr[idxs]
            cnt = cnt + pos
          endfor
          idxs = where(cnt eq 0)
          if (idxs[0] ne -1) then aSegMin[idxs] = missingValue
          aDataArray[*, segIdx] = aSegMin
        endfor 
        if (calcMode eq 'dataMin') then begin
          curDataArray = aDataArray
          aDataMin = curDataArray[*, 0]
          cnt = long(aDataMin ne missingValue)
          aDataMin = aDataMin * cnt
          for segIdx = 1, numTimeSeg - 1 do begin
            tmpArr = curDataArray[*, segIdx]
            pos = long(tmpArr ne missingValue)
            idxs = where(tmpArr * pos lt aDataMin * pos)
            if (idxs[0] ne -1) then aDataMin[idxs] = tmpArr[idxs]
            cnt = cnt + pos
          endfor
          idxs = where(cnt eq 0)
          if (idxs[0] ne -1) then aDataMin[idxs] = missingValue
          aDataArray = aDataMin
        endif
        if (calcMode eq 'tiMean') then begin
          sum = aDataArray[*, 0]
          cnt = long(sum ne missingValue)
          sum = sum * cnt
          for segIdx = 1, numTimeSeg - 1 do begin
            tmpArr = aDataArray[*, segIdx]
            pos = tmpArr ne missingValue
            cnt = cnt + pos
            sum = sum + tmpArr * pos  
          endfor
          aDataArray = sum / cnt
          idxs = where(cnt eq 0)
          if (idxs[0] ne -1) then aDataArray[idxs] = missingValue 
        endif      
      endif
    endif else begin
      print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
      return
    endelse

END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinMin::Run
     print, "(cvcCalcMinMin::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; input we want to process, let's get the first one
    dataIdx = 0  
    
    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='timeMin', sParam)
      calcMode = *(sParam.data)
    endif else calcMode = 'tiMean'  
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcMinMin::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
      
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
          
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        self->regDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMin, totCnt
      endif else begin
        self->stDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMin, totCnt
      endelse
    endfor
    
    segIdx = 0
    missingValue = sResponse.missingVal
    if (sResponse.gridType ne 'station') then begin ; for non-station data
      self->regTimeMin, calcMode, totMin, numTimeSeg, numDays, missingValue, aDataArray
    endif else begin
      self->stTimeMin, calcMode, totMin, numTimeSeg, numDays, missingValue, aDataArray
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
    i = 0
    midDaySeg = fltarr(numTimeSeg*numDays)
    for segIdx = 0, numTimeSeg - 1 do begin
       for dayIdx = 0, numDays - 1 do begin
          self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
          begTimeJD = julday(reqMonth, reqDay, reqYear, 06)
          midDaySeg[i] = begTimeJD + dayIdx
          i = i + 1
       endfor
    endfor
    
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
    
    if (calcMode eq 'dayMin') then begin
      aDataArray = reform(aDataArray, n_elements(aLons), n_elements(aLats), numTimeSeg*numDays)
    endif
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    if (calcMode eq 'dayMin') then begin
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midDaySeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
    endif else begin
      if (calcMode eq 'segMin') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
      endif else begin
        if (calcMode eq 'dataMin') then begin
          res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
        endif else begin
          if (calcMode eq 'tiMean') then begin
            res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
          endif else begin
            print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
            return, -1
          endelse
        endelse
      endelse
    endelse
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinMin::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
pro cvcCalcMinMin__define
   struct = { cvcCalcMinMin, $
               INHERITS cvcCalc $
             }
end