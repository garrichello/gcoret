;
;  NAME: 
;    cvcCalcMaximum
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of maximum values for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMaximum = obj_new('cvcCalcMaximum', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcMaximum->Run()
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
FUNCTION cvcCalcMaximum::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcMaximum::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
PRO cvcCalcMaximum::regDailyMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totNumDays, totMax
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr_b, mn, dy, hr
    begDay = julday(mn, dy, yr_b, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr_e, mn, dy, hr
    endDay = julday(mn, dy, yr_e, hr) 
    numDays = long(endDay - begDay + 1) 
    if (yr_b ne yr_e) then yr_b = yr_b + 1
    numYrDays =long(julday(12, 31, yr_b, 18) - julday(01, 01, yr_b, 00)) + 1
    if (numYrDays gt 365) then begin 
      missDay = julday(2, 29, yr_b, hr)
    endif else begin
      missDay = sResponse.missingVal
    endelse 
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMax = fltarr(sz[1], sz[2], numDays, numTimeSeg)
      totNumDays = fltArr(numTimeSeg)
    endif
    totNumDays[segIdx] = numDays
    dayIdx = 0
    dataIdx = 0
    maxArr = sResponse.aData[*, *, 0]
    cnt = long(maxArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      if (long(sResponse.aTimes[i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, *, i]
        pos = long(tmpArr ne sResponse.missingVal)
        maxPos = long(tmpArr * pos * cnt gt maxArr * pos * cnt)
        addPos = long(pos - cnt eq 1)
        idxs = where(maxPos + addPos eq 1)
        if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
        cnt = long(cnt + addPos eq 1)
      endif else begin
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
        totMax[*, *, dataIdx, segIdx] = maxArr 
        maxArr = sResponse.aData[*, *, i]
        cnt = long(maxArr ne sResponse.missingVal)
        if (long(sResponse.aTimes[i] - 0.5) ne long(missDay)) then dataIdx = dataIdx + 1
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
    totMax[*, *, dataIdx, segIdx] = maxArr 
END
;--------------------------------------------------------------------
PRO cvcCalcMaximum::regSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMax       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMax = fltarr(sz[1], sz[2], numTimeSeg)
    endif
    maxArr = sResponse.aData[*, *, 0]
    cnt = long(maxArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      tmpArr = sResponse.aData[*, *, i]
      pos = long(tmpArr ne sResponse.missingVal)
      maxPos = long(tmpArr * pos * cnt gt maxArr * pos * cnt)
      addPos = long(pos - cnt eq 1)
      idxs = where(maxPos + addPos eq 1)
      if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
      cnt = long(cnt + addPos eq 1)
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
    totMax[*, *, segIdx] = maxArr     
END
;--------------------------------------------------------------------
PRO cvcCalcMaximum::stDailyMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totNumDays, totMax
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr_b, mn, dy, hr
    begDay = julday(mn, dy, yr_b, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr_e, mn, dy, hr
    endDay = julday(mn, dy, yr_e, hr) 
    numDays = fix(endDay - begDay + 1)  
    if (yr_b ne yr_e) then yr_b = yr_b + 1
    numYrDays =long(julday(12, 31, yr_b, 18) - julday(01, 01, yr_b, 00)) + 1
    if (numYrDays gt 365) then begin 
      missDay = julday(2, 29, yr_b, hr)
    endif else begin
      missDay = sResponse.missingVal
    endelse 
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMax = fltarr(sz[1], numDays, numTimeSeg)
      totNumDays = fltArr(numTimeSeg)
    endif
    totNumDays[segIdx] = numDays
    dayIdx = 0
    dataIdx = 0
    maxArr = sResponse.aData[*, 0]
    cnt = long(maxArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      if (long(sResponse.aTimes[0, i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, i]
        pos = long(tmpArr ne sResponse.missingVal)
        maxPos = long(tmpArr * pos * cnt gt maxArr * pos * cnt)
        addPos = long(pos - cnt eq 1)
        idxs = where(maxPos + addPos eq 1)
        if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
        cnt = long(cnt + addPos eq 1)
      endif else begin
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
        totMax[*, dataIdx, segIdx] = maxArr 
        maxArr = sResponse.aData[*, i]
        cnt = long(maxArr ne sResponse.missingVal)
        if (long(sResponse.aTimes[0, i] - 0.5) ne long(missDay)) then dataIdx = dataIdx + 1
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
    totMax[*, dataIdx, segIdx] = maxArr 
END
;--------------------------------------------------------------------
PRO cvcCalcMaximum::stSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMax, totCnt       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMax = fltarr(sz[1], numTimeSeg)
    endif
    maxArr = sResponse.aData[*, 0]
    cnt = long(maxArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      tmpArr = sResponse.aData[*, i]
      pos = long(tmpArr ne sResponse.missingVal)
      maxPos = long(tmpArr * pos * cnt gt maxArr * pos * cnt)
      addPos = long(pos - cnt eq 1)
      idxs = where(maxPos + addPos eq 1)
      if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
      cnt = long(cnt + addPos eq 1)
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
    totMax[*, segIdx] = maxArr    
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMaximum::Run
     self->printLog, "(cvcCalcMaximum::Run) Started..."
    
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
      res = aoParams[0]->Get(UID='timeMax', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
    endif else calcMode = 'data'  
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcMaximum::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
      
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        case calcMode of
        'day': self->regDailyMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totNumDays, totMax
        'segment': self->regSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMax
        'data': begin
                     self->regSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMax
                     if (segIdx eq 0) then begin
                       maxArr = totMax[*, *, segIdx]
                       maxCnt = long(totMax[*, *, segIdx] ne sResponse.missingVal)
                     endif else begin
                     tmpArr = totMax[*, *, segIdx]
                     pos = long(totMax[*, *, segIdx] ne sResponse.missingVal) ;totCnt[*, *, segIdx]
                     maxPos = long(tmpArr * pos * maxCnt gt maxArr * pos * maxCnt)
                     addPos = long(pos - maxCnt eq 1)
                     idxs = where(maxPos + addPos eq 1)
                     if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
                       maxCnt = long(maxCnt + addPos eq 1)
                     endelse
                     if (segIdx eq numTimeSeg - 1) then begin
                       idxs = where(maxCnt eq 0)
                       if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
                       totMax = maxArr 
                     endif
                 end
       'mean': begin
                  self->regSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMax
                  if (segIdx eq 0) then begin
                    sum = totMax[*, *, segIdx]
                    cnt = long(totMax[*, *, segIdx] ne sResponse.missingVal)
                    sum = sum * cnt
                  endif else begin
                    tmpArr = totMax[*, *, segIdx]
                    pos = long(totMax[*, *, segIdx] ne sResponse.missingVal)
                    sum = sum + tmpArr * pos
                    cnt = cnt + pos     
                  endelse
                  if (segIdx eq numTimeSeg - 1) then begin
                    idxs = where(cnt ne 0, complement = zdxs)
                    if (idxs[0] ne -1) then totMax[idxs] = sum[idxs] / cnt[idxs]
                    if (zdxs[0] ne -1) then totMax[zdxs] = sResponse.missingVal
                  endif
                end 
              else: self->printLog, '(cvcCalcMaximum) Error! Unknown calculation mode: ', mode
            endcase
      endif else begin ; station data process
	sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        case calcMode of
          'day': self->stDailyMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMax
          'segment': self->stSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMax
          'data': begin
                      self->stSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMax
                      if (segIdx eq 0) then begin
                        maxArr = totMax[*, segIdx]
                        maxCnt = long(totMax[*, segIdx] ne sResponse.missingVal)
                      endif else begin
                        tmpArr = totMax[*, segIdx]
                        pos = long(totMax[*, segIdx] ne sResponse.missingVal)
                        maxPos = long(tmpArr * pos * maxCnt gt maxArr * pos * maxCnt)
                        addPos = long(pos - maxCnt eq 1)
                        idxs = where(maxPos + addPos eq 1)
                        if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
                        maxCnt = long(maxCnt + addPos eq 1)
                      endelse
                      if (segIdx eq numTimeSeg - 1) then begin
                        idxs = where(maxCnt eq 0)
                        if (idxs[0] ne -1) then maxArr[idxs] = sResponse.missingVal 
                        totMax = maxArr 
                      endif
                    end
          'mean': begin
                      self->stSegMax, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMax
                      if (segIdx eq 0) then begin
                       sum = totMax[*, segIdx]
                       cnt = long(totMax[*, segIdx] ne sResponse.missingVal)
                       sum = sum * cnt
                     endif else begin
                       tmpArr = totMax[*, segIdx]
                       pos = long(totMax[*, segIdx] ne sResponse.missingVal)
                       sum = sum + tmpArr * pos
                       cnt = cnt + pos     
                     endelse
                     if (segIdx eq numTimeSeg - 1) then begin
                       idxs = where(cnt ne 0, complement = zdxs)
                       if (idxs[0] ne -1) then totMax[idxs] = sum[idxs] / cnt[idxs]
                       if (zdxs[0] ne -1) then totMax[zdxs] = sResponse.missingVal
                     endif 
                   end
            else: self->printLog, '(cvcCalcMaximum::Run) Error! Unknown calculation mode: ', mode
        endcase
      endelse
    endfor
    aDataArray = totMax
 
;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
    
;   time Grid
    
    case calcMode of
      'day': begin
                  i = 0
                  for segIdx = 0, numTimeSeg - 1 do begin
                    if (segIdx eq 0) then midDaySeg = fltarr(numTimeSeg*totNumDays[segIdx])
                    numDays = totNumDays[segIdx]
                    self->DayMonthYear, asTimeSeg[segIdx].beginning, begYear, begMonth, begDay, begHour
                    begTimeJD = julday(begMonth, begDay, begYear, begHour)
                    self->DayMonthYear, asTimeSeg[segIdx].ending, endYear, endMonth, endDay, endHour
                    endTimeJD = julday(endMonth, endDay, endYear, endHour)
                    if (begYear ne endYear) then begYear = begYear + 1
                    caldat, julday(2, 29, begYear, begHour), misMonth, misDay, misYear, misHour
                    if ((float(misMonth) eq 2.0) and (float(misDay) eq 29.0)) then begin
                       missDay = julday(2, 29, begYear, begHour)
                    endif else missDay = sResponse.missingVal
                    for dayIdx = 0, numDays - 1 do begin
                      if (long(begTimeJD + dayIdx) ne long(missDay)) then begin
                        midDaySeg[i] = begTimeJD + dayIdx
                        i = i + 1
                      endif else begin
                        self->printLog, '(cvcCalcMaximum::Run) 29 Febrary', begYear
                      endelse
                    endfor
                    if (numDays gt totNumDays[0]) then numDays = numDays -1 
                  endfor
                  if (sResponse.gridType ne 'station') then begin ; for non-station data
                    aDataArray = reform(aDataArray, n_elements(aLons), n_elements(aLats), numTimeSeg*numDays)
                  endif else begin
                    aDataArray = reform(aDataArray, n_elements(aLons), numTimeSeg*numDays)
                  endelse
                end
      'segment': begin
                  midSeg = fltarr(numTimeSeg)
                  for segIdx = 0, numTimeSeg - 1 do begin
                    self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
                    reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
                    self->DayMonthYear, asTimeSeg[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
                    reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
                    midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
                  endfor 
                end
      else: self->printLog, '(cvcCalcMaximum::Run) calcModes of data and mean do not have time grid '
    endcase
    
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    

    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    case calcMode of
      'day': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midDaySeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'segment': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'data': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'mean': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    else: self->printLog, '(cvcCalcMaximum::Run) Error! Unknown calculation mode: ', mode
    endcase
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMaximum::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
pro cvcCalcMaximum__define
   struct = { cvcCalcMaximum, $
               INHERITS cvcCalc $
             }
end