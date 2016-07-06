;
;  NAME: 
;    cvcCalcTotAmount
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of minimum values for time series of data
;    
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTotAmount = obj_new('cvcCalcTotAmount', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcA->Run()
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
FUNCTION cvcCalcTotAmount::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcTotAmount::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
PRO cvcCalcTotAmount::regDailyAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totMnt
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    begDay = julday(mn, dy, yr, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr, mn, dy, hr
    endDay = julday(mn, dy, yr, hr) 
    numDays = fix(endDay - begDay + 1)
       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMnt = fltarr(sz[1], sz[2], numDays, numTimeSeg)
  ;    totCnt = fltarr(sz[1], sz[2], numTimeSeg, numDays)
    endif
    dayIdx = 0
    sumArr = sResponse.aData[*, *, 0]
    cnt = long(sumArr ne sResponse.missingVal)
    sumArr = sumArr * cnt
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      if (long(sResponse.aTimes[i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, *, i]
        pos = long(tmpArr ne sResponse.missingVal)
        sumArr = sumArr + tmpArr * pos
        cnt = cnt + pos
      endif else begin
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal 
        totMnt[*, *, dayIdx, segIdx] = sumArr 
  ;      totCnt[*, *, segIdx, dayIdx] = long(sumArr ne sResponse.missingVal)
        sumArr = sResponse.aData[*, *, i]
        cnt = long(sumArr ne sResponse.missingVal)
        sumArr = sumArr * cnt
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal 
    totMnt[*, *, dayIdx, segIdx] = sumArr 
  ;  totCnt[*, *, segIdx, dayIdx] = long(sumArr ne sResponse.missingVal)
END
;--------------------------------------------------------------------
PRO cvcCalcTotAmount::regSegAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMnt       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMnt = fltarr(sz[1], sz[2], numTimeSeg)
  ;    totCnt = fltarr(sz[1], sz[2], numTimeSeg)
    endif
    sumArr = sResponse.aData[*, *, 0]
    cnt = long(sumArr ne sResponse.missingVal)
    sumArr = sumArr * cnt
    for i = 1, n_elements(sResponse.aTimes)-1 do begin
      tmpArr = sResponse.aData[*, *, i]
      pos = long(tmpArr ne sResponse.missingVal)
      sumArr = sumArr + tmpArr * pos
      cnt = cnt + pos
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal  
    totMnt[*, *, segIdx] = sumArr 
 ;   totCnt[*, *, segIdx] = long(sumArr ne sResponse.missingVal)    
END
;--------------------------------------------------------------------
PRO cvcCalcTotAmount::stDailyAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totMnt
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    begDay = julday(mn, dy, yr, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr, mn, dy, hr
    endDay = julday(mn, dy, yr, hr) 
    numDays = fix(endDay - begDay + 1)
       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMnt = fltarr(sz[1], numDays, numTimeSeg)
  ;    totCnt = fltarr(sz[1], numTimeSeg, numDays)
    endif
    dayIdx = 0
    sumArr = sResponse.aData[*, 0]
    cnt = long(sumArr ne sResponse.missingVal)
    sumArr = sumArr * cnt
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      if (long(sResponse.aTimes[0, i] - 0.5) eq long(begDay + dayIdx)) then begin
        tmpArr = sResponse.aData[*, i]
        pos = long(tmpArr ne sResponse.missingVal)
        sumArr = sumArr + tmpArr * pos
        cnt = cnt + pos
      endif else begin
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal 
        totMnt[*, dayIdx, segIdx] = sumArr 
   ;     totCnt[*, segIdx, dayIdx] = long(sumArr ne sResponse.missingVal)
        sumArr = sResponse.aData[*, i]
        cnt = long(sumArr ne sResponse.missingVal)
        sumArr = sumArr * cnt
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal 
    totMnt[*, dayIdx, segIdx] = sumArr 
 ;  totCnt[*, segIdx, dayIdx] = long(sumArr ne sResponse.missingVal)  
END
;--------------------------------------------------------------------
PRO cvcCalcTotAmount::stSegAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMnt       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMnt = fltarr(sz[1], numTimeSeg)
 ;     totCnt = fltarr(sz[1], numTimeSeg)
    endif
    sumArr = sResponse.aData[*, 0]
    cnt = long(sumArr ne sResponse.missingVal)
    sumArr = sumArr * cnt
    for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
      tmpArr = sResponse.aData[*, i]
      pos = long(tmpArr ne sResponse.missingVal)
      sumArr = sumArr + tmpArr * pos
      cnt = cnt + pos
    endfor
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal 
    totMnt[*, segIdx] = sumArr 
 ;   totCnt[*, segIdx] = long(sumArr ne sResponse.missingVal)    
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTotAmount::Run
     print, "(cvcCalcTotAmount::Run) Started..."
    
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
      res = aoParams[0]->Get(UID='timeAmount', sParam)
      calcMode = *(sParam.data)
    endif else calcMode = 'tiMean'  
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcTotAmount::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
      
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        case calcMode of
        'dayAmount': self->regDailyAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMnt
        'segAmount': self->regSegAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMnt
        'tiMean': begin
                     self->regSegAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMnt
                     if (segIdx eq 0) then begin
                     meanArr = totMnt[*, *, segIdx]
                     meanCnt = totCnt[*, *, segIdx]
                     meanArr = meanArr * meanCnt
                   endif else begin
                     tmpArr = totMnt[*, *, segIdx]
                     pos = totCnt[*, *, segIdx]
                     meanArr = meanArr + tmpArr * pos
                     meanCnt = meanCnt + pos
                   endelse
                   if (segIdx eq numTimeSeg - 1) then begin
                     idxs = where(meanCnt ne 0, complement = zdxs)
                     if (idxs[0] ne -1) then meanArr[idxs] = meanArr[idxs] / meanCnt[idxs]
                     if (zdxs[0] ne -1) then meanArr[zdxs] = sResponse.missingVal 
                     totMnt = meanArr 
                   endif
                 end 
              else: print, '(cvcCalcTotAmount) Error! Unknown calculation mode: ', mode
            endcase
      endif else begin ; station data process
        case calcMode of
          'dayAmount': self->stDailyAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMnt
          'segAmount': self->stSegAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMnt
          'tiMean': begin
                      self->stSegAmount, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMnt
                      if (segIdx eq 0) then begin
                        meanArr = totMnt[*, segIdx]
                        meanCnt = totCnt[*, segIdx]
                        meanArr = meanArr * meanCnt
                      endif else begin
                        tmpArr = totMnt[*, segIdx]
                        pos = totCnt[*, segIdx]
                        meanArr = meanArr + tmpArr * pos
                        meanCnt = meanCnt + pos
                      endelse
                      if (segIdx eq numTimeSeg - 1) then begin
                        idxs = where(meanCnt ne 0, complement = zdxs)
                        if (idxs[0] ne -1) then meanArr[idxs] = meanArr[idxs] / meanCnt[idxs]
                        if (zdxs[0] ne -1) then meanArr[zdxs] = sResponse.missingVal 
                        totMnt = meanArr 
                      endif
                    end
            else: print, '(cvcCalcTotAmount) Error! Unknown calculation mode: ', mode
        endcase
      endelse
    endfor
    aDataArray = totMnt
 
;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
    
;   time Grid
    case calcMode of
      'dayAmount': begin
                  i = 0
                  midDaySeg = fltarr(numTimeSeg*numDays)
                  for segIdx = 0, numTimeSeg - 1 do begin
                    for dayIdx = 0, numDays - 1 do begin
                      self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
                      begTimeJD = julday(reqMonth, reqDay, reqYear, reqHour) 
                      midDaySeg[i] = begTimeJD + dayIdx + 0.5
                      i = i + 1
                    endfor
                  endfor
                  if (sResponse.gridType ne 'station') then begin ; for non-station data
                    aDataArray = reform(aDataArray, n_elements(aLons), n_elements(aLats), numTimeSeg*numDays, /overwrite)
                  endif else begin
                    aDataArray = reform(aDataArray, n_elements(aLons), numTimeSeg*numDays)
                  endelse
                end
      'segAmount': begin
                  midSeg = fltarr(numTimeSeg)
                  for segIdx = 0, numTimeSeg - 1 do begin
                    self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
                    reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
                    self->DayMonthYear, asTimeSeg[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
                    reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
                    midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
                  endfor 
                end
      else: print, 'calcModes of dataMin and tiMean do not have time grid '
    endcase
    
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    

    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    case calcMode of
      'dayAmount': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midDaySeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
      'segAmount': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
      'tiMean': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
    else: print, '(cvcCalcTotAmount) Error! Unknown calculation mode: ', mode
    endcase
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTotAmount::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
pro cvcCalcTotAmount__define
   struct = { cvcCalcTotAmount, $
               INHERITS cvcCalc $
             }
end