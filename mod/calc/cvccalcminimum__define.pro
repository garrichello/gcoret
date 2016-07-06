;
;  NAME: 
;    cvcCalcMinimum
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of minimum values for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMinimum = obj_new('cvcCalcMinimum', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcMinimum->Run()
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
FUNCTION cvcCalcMinimum::Init, in_sInputs, in_sOutputs, in_hLogFile
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcMinimum::Cleanup
    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
PRO cvcCalcMinimum::regDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totNumDays, totMin
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr_b, mn, dy, hr
    begDay = julday(mn, dy, yr_b, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr_e, mn, dy, hr
    endDay = julday(mn, dy, yr_e, hr) 
    numDays = long(endDay - begDay) + 1
    if (yr_b ne yr_e) then yr_b = yr_b + 1
    numYrDays =long(julday(12, 31, yr_b, 18) - julday(01, 01, yr_b, 00)) + 1
    if (numYrDays gt 365) then begin 
      missDay = julday(2, 29, yr_b, hr)
    endif else begin
      missDay = sResponse.missingVal
    endelse 
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMin = fltarr(sz[1], sz[2], numDays, numTimeSeg)
      totNumDays = fltArr(numTimeSeg)
    endif
    totNumDays[segIdx] = numDays
    dayIdx = 0
    dataIdx = 0
    minArr = sResponse.aData[*, *, 0]
    cnt = long(minArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      if (long(sResponse.aTimes[i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, *, i]
        pos = long(tmpArr ne sResponse.missingVal)
        minPos = long(tmpArr * cnt * pos lt minArr * cnt * pos)
        addPos = long(pos - cnt eq 1)
        idxs = where(minPos + addPos eq 1)
        if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
        cnt = long(cnt + addPos eq 1)
      endif else begin
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal
        totMin[*, *, dataIdx, segIdx] = minArr 
        minArr = sResponse.aData[*, *, i]
        cnt = long(minArr ne sResponse.missingVal)
        if (long(sResponse.aTimes[i] - 0.5) ne long(missDay)) then dataIdx = dataIdx + 1
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
    totMin[*, *, dataIdx, segIdx] = minArr 
END
;--------------------------------------------------------------------
PRO cvcCalcMinimum::regSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMin      
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMin = fltarr(sz[1], sz[2], numTimeSeg)
    endif
    minArr = sResponse.aData[*, *, 0]
    cnt = long(minArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      tmpArr = sResponse.aData[*, *, i]
      pos = long(tmpArr ne sResponse.missingVal)
      minPos = long(tmpArr * pos * cnt lt minArr * pos * cnt)
      addPos = long(pos - cnt eq 1)
      idxs = where(minPos + addPos eq 1)
      if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
      cnt = long(cnt + addPos eq 1)
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
    totMin[*, *, segIdx] = minArr
END
;--------------------------------------------------------------------
PRO cvcCalcMinimum::stDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totNumDays, totMin
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
      totMin = fltarr(sz[1], numDays, numTimeSeg)
      totNumDays = fltArr(numTimeSeg)
    endif
    totNumDays[segIdx] = numDays
    dayIdx = 0
    dataIdx = 0
    minArr = sResponse.aData[*, 0]
    cnt = long(minArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      if (long(sResponse.aTimes[0, i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, i]
        pos = long(tmpArr ne sResponse.missingVal)
        minPos = long(tmpArr * pos * cnt lt minArr * pos * cnt)
        addPos = long(pos - cnt eq 1)
        idxs = where(minPos + addPos eq 1)
        if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
        cnt = long(cnt + addPos eq 1)
      endif else begin
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totMin[*, dataIdx, segIdx] = minArr 
        minArr = sResponse.aData[*, i]
        cnt = long(minArr ne sResponse.missingVal)
        if (long(sResponse.aTimes[0, i] - 0.5) ne long(missDay)) then dataIdx = dataIdx + 1
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
    totMin[*, dataIdx, segIdx] = minArr 
END
;--------------------------------------------------------------------
PRO cvcCalcMinimum::stSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMin       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMin = fltarr(sz[1], numTimeSeg)
    endif
    minArr = sResponse.aData[*, 0]
    cnt = long(minArr ne sResponse.missingVal)
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      tmpArr = sResponse.aData[*, i]
      pos = long(tmpArr ne sResponse.missingVal)
      minPos = long(tmpArr * pos * cnt lt minArr * pos * cnt)
      addPos = long(pos - cnt eq 1)
      idxs = where(minPos + addPos eq 1)
      if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
      cnt = long(cnt + addPos eq 1)
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
    totMin[*, segIdx] = minArr    
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinimum::Run
     self->printLog, "(cvcCalcMinimum::Run) Started..."
    
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
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
    endif else calcMode = 'data'  
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcMinimum::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
      
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        case calcMode of
        'day': self->regDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totNumDays, totMin
        'segment': self->regSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMin
        'data': begin
                     self->regSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMin
                     if (segIdx eq 0) then begin
                     minArr = totMin[*, *, segIdx]
                     minCnt = long(totMin[*, *, segIdx] ne sResponse.missingVal) 
                   endif else begin
                     tmpArr = totMin[*, *, segIdx]
                     pos = long(totMin[*, *, segIdx] ne sResponse.missingVal) 
                     minPos = long(tmpArr * pos * minCnt lt minArr * pos * minCnt)
                     addPos = long(pos - minCnt eq 1)
                     idxs = where(minPos + addPos eq 1)
                     if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
                     minCnt = long(minCnt + addPos eq 1)
                   endelse
                   if (segIdx eq numTimeSeg - 1) then begin
                     idxs = where(minCnt eq 0)
                     if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
                     totMin = minArr 
                   endif
                 end
       'mean': begin
                  self->regSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMin
                  if (segIdx eq 0) then begin
                    sum = totMin[*, *, segIdx]
                    cnt = long(totMin[*, *, segIdx] ne sResponse.missingVal) 
                    sum = sum * cnt
                  endif else begin
                    tmpArr = totMin[*, *, segIdx]
                    pos = long(totMin[*, *, segIdx] ne sResponse.missingVal) 
                    sum = sum + tmpArr * pos
                    cnt = cnt + pos     
                  endelse
                  if (segIdx eq numTimeSeg - 1) then begin
                    idxs = where(cnt ne 0, complement = zdxs)
                    if (idxs[0] ne -1) then totMin[idxs] = sum[idxs] / cnt[idxs]
                    if (zdxs[0] ne -1) then totMin[zdxs] = sResponse.missingVal
                  endif
                end 
              else: begin
                self->printLog, '(cvcCalcMinimum) Error! Unknown calculation mode: ', calcMode
                return, -1
              end
            endcase
      endif else begin ; station data process
	sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        case calcMode of
          'day': self->stDailyMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totNumDays, totMin
          'segment': self->stSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMin
          'data': begin
                      self->stSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMin
                      if (segIdx eq 0) then begin
                        minArr = totMin[*, segIdx]
                        minCnt = long(totMin[*, segIdx] ne sResponse.missingVal) 
                      endif else begin
                        tmpArr = totMin[*, segIdx]
                        pos = long(totMin[*, segIdx] ne sResponse.missingVal)
                        minPos = long(tmpArr * pos * minCnt lt minArr * pos * minCnt)
                        addPos = long(pos - minCnt eq 1)
                        idxs = where(minPos + addPos eq 1)
                        if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
                        minCnt = long(minCnt + addPos eq 1)
                      endelse
                      if (segIdx eq numTimeSeg - 1) then begin
                        idxs = where(minCnt eq 0)
                        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
                        totMin = minArr 
                      endif
                    end
          'mean': begin
                      self->stSegMin, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMin
                      if (segIdx eq 0) then begin
                       sum = totMin[*, segIdx]
                       cnt = long(totMin[*, segIdx] ne sResponse.missingVal)
                       sum = sum * cnt
                     endif else begin
                       tmpArr = totMin[*, segIdx]
                       pos = long(totMin[*, segIdx] ne sResponse.missingVal)
                       sum = sum + tmpArr * pos
                       cnt = cnt + pos     
                     endelse
                     if (segIdx eq numTimeSeg - 1) then begin
                       idxs = where(cnt ne 0, complement = zdxs)
                       if (idxs[0] ne -1) then totMin[idxs] = sum[idxs] / cnt[idxs]
                       if (zdxs[0] ne -1) then totMin[zdxs] = sResponse.missingVal
                     endif 
                   end
            else: begin
              self->printLog, '(cvcCalcMinimum) Error! Unknown calculation mode: ', calcMode
              return, -1
            end
        endcase
      endelse
    endfor
    aDataArray = totMin
 
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
                        self->printLog, '(cvcCalcMinimum::Run) 29 Febrary', begYear
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
      else: begin
        self->printLog, '(cvcCalcMinimum::Run) calcModes of data and mean do not have time grid '
        return, -1
      end
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
    else: begin
      self->printLog, '(cvcCalcMinimum::Run) Error! Unknown calculation mode: ', calcMode
      return, -1
    end
    endcase
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinimum::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
pro cvcCalcMinimum__define
   struct = { cvcCalcMinimum, $
               INHERITS cvcCalc $
             }
end