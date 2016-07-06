;
;  NAME: 
;    cvcCalcMaxMin
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of maximum values for time series of data
;    (Monthly maximum of daily minimum temperature)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMaxMin = obj_new('cvcCalcMaxMin', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcMaxMin->Run()
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
FUNCTION cvcCalcMaxMin::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcMaxMin::Cleanup

    self->cvcCalc::Cleanup

END
;---------------------------------------------------------------
FUNCTION cvcCalcMaxMin::Run

    print, "(cvcCalcTimeMean::Run) Started..."
;    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")
    
    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
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
    print, "(cvcCalcMaxMax::Run) Get data..."
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
          totMax = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        minArr = sResponse.aData[*, *, 0]
        maxArr = sResponse.aData[*, *, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
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
            idxs = where(minArr * pos gt maxArr * pos)
            if (idxs[0] ne -1) then maxArr[idxs] = minArr[idxs]          
            curDay = curDay + 1
            minArr = sResponse.aData[*, *, i]
            cnt = minArr ne sResponse.missingVal
            minArr = minArr * cnt
            begPos = cnt
            totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]            
          endelse
        endfor
        pos = (cnt ne 0) 
        idxs = where(minArr * pos gt maxArr * pos)
        if (idxs[0] ne -1) then maxArr[idxs] = minArr[idxs] 
        totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]
        idxs = where(totCnt[*, *, segIdx] eq 0)
        if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
        totMax[*, *, segIdx] = maxArr
      endif else begin ; for station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totMax = fltarr(sz[1], numTimeSeg)
          totCnt = fltarr(sz[1], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        minArr = sResponse.aData[*, 0]
        maxArr = sResponse.aData[*, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
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
            idxs = where(minArr * pos gt maxArr * pos)
            if (idxs[0] ne -1) then maxArr[idxs] = minArr[idxs]          
            curDay = curDay + 1
            minArr = sResponse.aData[*, i]
            cnt = minArr ne sResponse.missingVal
            minArr = minArr * cnt
            begPos = cnt
            totCnt[*, segIdx] = totCnt[*, segIdx] + pos            
          endelse
        endfor
        pos = (cnt ne 0) 
        idxs = where(minArr * pos gt maxArr * pos)
        if (idxs[0] ne -1) then maxArr[idxs] = minArr[idxs] 
        totCnt[*, segIdx] = totCnt[*, segIdx] + pos
        idxs = where(totCnt[*,segIdx] eq 0)
        if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
        totMax[*, segIdx] = maxArr
        idxs = where(cnt eq 0)
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totMax[*, *, 0]
        cnt = long(totCnt[*, *, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totMax[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totMax[*, 0]
        cnt = long(totCnt[*, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totMax[*, segIdx]
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
          sz = size(totMax[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totMax[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totMax[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totMax[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
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
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
    
END
;--------------------------------------------------------------------
 
pro cvcCalcMaxMin__define
   struct = { cvcCalcMaxMin, $
               INHERITS cvcCalc $
             }
end
