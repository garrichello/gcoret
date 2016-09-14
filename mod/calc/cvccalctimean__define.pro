;
;  NAME: 
;    cvcCalcTiMean
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of mean values for time series of data
;    
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTiMean = obj_new('cvcCalcTiMean', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTiMean->Run()
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
FUNCTION cvcCalcTiMean::Init, in_sInputs, in_sOutputs, in_hLogFile
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcTiMean::Cleanup
    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
PRO cvcCalcTiMean::regDailyMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totMean
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    begDay = julday(mn, dy, yr, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr, mn, dy, hr
    endDay = julday(mn, dy, yr, hr) 
    numDays = fix(endDay - begDay + 1)
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMean = fltarr(sz[1], sz[2], numDays, numTimeSeg)
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
        idxs = where(cnt ne 0, complement = zdxs)
        if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
        if (zdxs[0] ne -1) then sumArr[zdxs] = sResponse.missingVal 
        totMean[*, *, dayIdx, segIdx] = sumArr 
        sumArr = sResponse.aData[*, *, i]
        cnt = long(sumArr ne sResponse.missingVal)
        sumArr = sumArr * cnt
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt ne 0, complement = zdxs)
    if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
    if (zdxs[0] ne -1) then sumArr[zdxs] = sResponse.missingVal 
    totMean[*, *, dayIdx, segIdx] = sumArr 
END
;--------------------------------------------------------------------
PRO cvcCalcTiMean::regSegMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMean       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMean = fltarr(sz[1], sz[2], numTimeSeg)
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
    idxs = where(cnt ne 0, complement = zdxs)
    if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
    if (zdxs[0] ne -1) then sumArr[zdxs] = sResponse.missingVal  
    totMean[*, *, segIdx] = sumArr  
END
;--------------------------------------------------------------------
PRO cvcCalcTiMean::stDailyMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, numDays, totMean
    self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    begDay = julday(mn, dy, yr, hr) 
    self->DayMonthYear, asTimeSeg[segIdx].ending, yr, mn, dy, hr
    endDay = julday(mn, dy, yr, hr) 
    numDays = fix(endDay - begDay + 1)    
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMean = fltarr(sz[1], numDays, numTimeSeg)
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
        idxs = where(cnt ne 0, complement = zdxs)
        if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
        if (zdxs[0] ne -1) then sumArr[zdxs] = sResponse.missingVal 
        totMean[*, dayIdx, segIdx] = sumArr 
        sumArr = sResponse.aData[*, i]
        cnt = long(sumArr ne sResponse.missingVal)
        sumArr = sumArr * cnt
        dayIdx = dayIdx + 1
      endelse
    endfor
    idxs = where(cnt ne 0, complement = zdxs)
    if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
    if (zdxs[0] ne -1) then sumArr[zdxs] = sResponse.missingVal 
    totMean[*, dayIdx, segIdx] = sumArr 
END
;--------------------------------------------------------------------
PRO cvcCalcTiMean::stSegMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevels, aLevel, sROI, totMean       
    if (segIdx eq 0) then begin
      sz = size(sResponse.aData)
      totMean = fltarr(sz[1], numTimeSeg)
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
    idxs = where(cnt ne 0, complement = zdxs)
    if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
    if (zdxs[0] ne -1) then sumArr[zdxs] = sResponse.missingVal 
    totMean[*, segIdx] = sumArr 
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTiMean::Run
     self->printLog, "(cvcCalcTiMean::Run) Started..."
   
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
      res = aoParams[0]->Get(UID='timeMean', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
      res = aoParams[0]->Get(UID='Slope', sParam)
      if (self->Assert(res)) then calcSlope = 1. else calcSlope = *(sParam.data)
      res = aoParams[0]->Get(UID='Intersept', sParam)
      if (self->Assert(res)) then calcIntercept = 0. else calcIntersept = *(sParam.data)
    endif else begin
      calcMode = 'data'
      calcIntercept = 0.
      calcSlope = 1.
    endelse
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcTiMean::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    ; number of months for month_accum and month_clim
    nMonths = 12

    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse) 
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        case calcMode of
          'day': self->regDailyMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMean
          'segment': self->regSegMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMean
          'data': begin
                    self->regSegMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMean
                    if (segIdx eq 0) then begin
                      meanArr = totMean[*, *, segIdx]
                      meanCnt = long(totMean[*, *, segIdx] ne sResponse.missingVal)
                      meanArr = meanArr * meanCnt
                    endif else begin
                      tmpArr = totMean[*, *, segIdx]
                      pos = long(totMean[*, *, segIdx] ne sResponse.missingVal)
                      meanArr = meanArr + tmpArr * pos
                      meanCnt = meanCnt + pos
                    endelse
                    if (segIdx eq numTimeSeg - 1) then begin
                      idxs = where(meanCnt ne 0, complement = zdxs)
                      if (idxs[0] ne -1) then meanArr[idxs] = meanArr[idxs] / meanCnt[idxs]
                      if (zdxs[0] ne -1) then meanArr[zdxs] = sResponse.missingVal 
                      totMean = meanArr 
                    endif
                  end
            'month_accum' : begin
			     sz = size(sResponse.aData, /dim) ; dimensions of the original data array
			     totMean = fltarr(sz[0], sz[1], nMonths, numTimeSeg) ; sums [lon, lat, year, month]
			     cnts = lonarr(sz[0], sz[1], nMonths, numTimeSeg) ; number of summed values [lon, lat, year, month]
                             
			     self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr ; we need the starting year - yr
			     mejd = julday(indgen(nMonths)+2, 1, yr, 23)-1 ; julian days of last day of each month
			     
                             startPos = 0 ; starting position in the time grid
			     for monthIdx = 0, nMonths-1 do begin
			       foo = min(abs(sResponse.aTimes-mejd[monthIdx]), endPos) ; endPos will point at the first day of the next month
			       for iTime = startPos, endPos-1 do begin
			         subData = sResponse.aData[*, *, iTime]
			         dataValid = (subData ne sResponse.missingVal)
			         totMean[*, *, monthIdx, segIdx] += subData*dataValid ; we sum only valid values, missing values are zeroed
			         cnts[*, *, monthIdx, segIdx] += dataValid
			       endfor
			       startPos = endPos ; move pointer to the start of the next month
			     endfor
                           end 
            'month_clim' : begin
			     sz = size(sResponse.aData, /dim) ; dimensions of the original data array
			     totMean = fltarr(sz[0], sz[1], nMonths, numTimeSeg) ; sums [lon, lat, year, month]
			     cnts = lonarr(sz[0], sz[1], nMonths, numTimeSeg) ; number of summed values [lon, lat, year, month]
                             
			     self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr ; we need the starting year - yr
			     mejd = julday(indgen(nMonths)+2, 1, yr, 23)-1 ; julian days of last day of each month
			     
                             startPos = 0 ; starting position in the time grid
			     for monthIdx = 0, nMonths-1 do begin
			       foo = min(abs(sResponse.aTimes-mejd[monthIdx]), endPos) ; endPos will point at the first day of the next month
			       for iTime = startPos, endPos-1 do begin
			         subData = sResponse.aData[*, *, iTime]
			         dataValid = (subData ne sResponse.missingVal)
			         totMean[*, *, monthIdx, segIdx] += subData*dataValid ; we sum only valid values, missing values are zeroed
			         cnts[*, *, monthIdx, segIdx] += dataValid
			       endfor
			       startPos = endPos ; move pointer to the start of the next month
			     endfor
                           end 
          else: self->printLog, '(cvcCalcTiMean::Run) Error! Unknown calculation mode: ', mode
        endcase
      endif else begin ; station data process
	sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        case calcMode of
          'day': self->stDailyMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, numDays, totMean
          'segment': self->stSegMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMean
          'data': begin
                      self->stSegMean, asTimeSeg, segIdx, sResponse, numTimeSeg, numLevel, aLevel, sROI, totMean
                      if (segIdx eq 0) then begin
                        meanArr = totMean[*, segIdx]
                        meanCnt = long(totMean[*, segIdx] ne sResponse.missingVal)
                        meanArr = meanArr * meanCnt
                      endif else begin
                        tmpArr = totMean[*, segIdx]
                        pos = long(totMean[*, segIdx] ne sResponse.missingVal)
                        meanArr = meanArr + tmpArr * pos
                        meanCnt = meanCnt + pos
                      endelse
                      if (segIdx eq numTimeSeg - 1) then begin
                        idxs = where(meanCnt ne 0, complement = zdxs)
                        if (idxs[0] ne -1) then meanArr[idxs] = meanArr[idxs] / meanCnt[idxs]
                        if (zdxs[0] ne -1) then meanArr[zdxs] = sResponse.missingVal 
                        totMean = meanArr 
                      endif
                  end
          else: self->printLog, '(cvcCalcTiMean::Run) Error! Unknown calculation mode: ', mode
        endcase
      endelse
    endfor
    aDataArray = totMean

;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
    
;   time Grid
    case calcMode of
      'day': begin
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
      'data': begin
                   self->DayMonthYear, asTimeSeg[0].beginning, reqYear, reqMonth, reqDay, reqHour
                   reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
                   self->DayMonthYear, asTimeSeg[0].ending, reqYear, reqMonth, reqDay, reqHour
                   reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
		   midBegSeg = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
	       end
      'month_accum': begin
		       self->DayMonthYear, asTimeSeg[0].beginning, yr0, mn, dy, hr ; we need the first year - yr0
		       self->DayMonthYear, asTimeSeg[numTimeSeg-1].ending, yr1, mn, dy, hr ; and we need the last year - yr1
		       nYears = yr1 - yr0 + 1 ; number of years under consideration
		       if (size(aDataArray, /n_dim) eq 4) then aDataArray = total(aDataArray, 4) / nYears 
		     end
      'month_clim': begin
		       if (size(aDataArray, /n_dim) eq 4) then aDataArray = total(aDataArray, 4) / total(cnts, 4)
		     end
      else: self->printLog, '(cvcCalcTiMean::Run) calcModes of dataMin and tiMean do not have time grid '
    endcase
    
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    

    aDataArray = calcIntercept + calcSLope*aDataArray ; apply scaling coefficients
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    case calcMode of
      'day': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID=midDaySeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'segment': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID=midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'data': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'month_accum': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID=mejd, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      'month_clim': res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID=mejd, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    else: self->printLog, '(cvcCalcTiMean::Run) Error! Unknown calculation mode: ', mode
    endcase
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTiMean::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
pro cvcCalcTiMean__define
   struct = { cvcCalcTiMean, $
               INHERITS cvcCalc $
             }
end
