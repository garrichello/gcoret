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
;       minVal - minimum data value
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
PRO cvcCalcMinimum::__findMin, inout_aMinVals, in_aCurVals, in_missVal

  curValIdxs = where(in_aCurVals ne in_missVal, curValCnt) ; get indices of valid cells in current field
  minValBigs = (inout_aMinVals eq in_missVal)*1e20 ; set very big values in cells with missing values for minimum field
; tricky part: 
; minValBigs is 0 for valid values and 1e20 for missing values in minimum field, thus summing does not affect good values
; and makes missing values very big so they are greater than new minimum value FOR SURE... 
  if (curValCnt gt 0) then begin ; ... and (if there are valid values in current field) we get indices of new minimum values
    idxs = where(in_aCurVals[curValIdxs] lt inout_aMinVals[curValIdxs]+minValBigs[curValIdxs], cnt) 
  endif else cnt = 0         
  if (cnt gt 0) then inout_aMinVals[idxs] = in_aCurVals[idxs] ; which are stored in the resulting array
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinimum::regDailyMin, in_aData, in_aUniqDaysPos, in_missVal
  sz = size(in_aData, /dim)
  if (n_elements(sz) eq 2) then begin ; no time dimension
    aMinData = in_aData ; nothing to search
; ToDo: this will return 2-D array, outside 3-D is expected, may be some improvements will be required
  endif else begin
    nTimes = sz[2] ; time dimension
    nDays = n_elements(in_aUniqDaysPos) ; number of unique days
    aMinData = fltarr(sz[0], sz[1], nDays, /nozero) ; output array with fields of minimum values for each day

; array in_aUniqDays contains indices of the LAST element in the sequence of times corresponding to a day
    stDayPos = 0 ; we start from the first time point on a time grid (starting from the beginning of the first day)
    for iDay = 0, nDays - 1 do begin ; for each unique day...
      aMinVals = in_aData[*, *, stDayPos] ; for the start we set minimum as a field at the beginning of the day
      for iTime = stDayPos+1, in_aUniqDaysPos[iDay] do begin ; starting from the next time step until the end of the day
        self->__findMin, aMinVals, in_aData[*, *, iTime], in_missVal ; find minimum values of both arrays
      endfor ; loop inside one day
      stDayPos = in_aUniqDaysPos[iDay] + 1 ; set start position to the beginning of the next day
      aMinData[*, *, iDay] = aMinVals
    endfor ; loop for days
  endelse

  return, aMinData
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinimum::regSegMin, in_aData, in_missVal
  sz = size(in_aData, /dim)
  if (n_elements(sz) eq 2) then begin ; no time dimension
    aMinData = in_aData ; nothing to search
  endif else begin
    nTimes = sz[2] ; time dimension

    aMinVals = in_aData[*, *, 0] ; let's search along the time dimension
    for iTime = 1, nTimes - 1 do begin
      self->__findMin, aMinVals, in_aData[*, *, iTime], in_missVal ; find minimum values of both arrays
    endfor
  endelse
  return, aMinVals
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinimum::stDailyMin, in_aData, in_aUniqDaysPos, in_missVal

  sz = size(in_aData, /dim)
  nStations = sz[0] ; number of stations
  nDays = n_elements(in_aUniqDaysPos) ; number of unique days
  
  aMinData = fltarr(nStations, nDays, /nozero) ; output array with minimum values for each station and day

; array in_aUniqDays contains indices of the LAST element in the sequence corresponding to the whole day
  for iSt = 0, nStations - 1 do begin
    stDayPos = 0 ; we start from the first time point on a time grid (starting from the beginning of the first day)
    for iDay = 0, nDays - 1 do begin ; for each unique day...
      aCurData = in_aData[iSt, stDayPos:in_aUniqDaysPos[iDay]]
      idxs = where(aCurData ne in_missVal, cnt)
      if (cnt gt 0) then aMinData[iSt, iDay] = min(aCurData[idxs]) else aMinData[iSt, iDay] = in_missVal ; ... find minimum during this day
      stDayPos = in_aUniqDaysPos[iDay] + 1 ; set starting point to the element corresponding to the beginning of the next day
    endfor
  endfor

  return, aMinData
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinimum::stSegMin, in_aData, in_missVal

    sz = size(in_aData, /dim)
    nStations = sz[0]; number of stations

    aMinData = fltarr(nStations, /nozero); allocate array for min values

    for i = 0, nStations-1 do begin
      aCurData = in_aData[i, *]
      idxs = where(aCurData ne in_missVal, cnt)
      if (cnt gt 0) then aMinData[i] = min(aCurData[idxs]) else aMinData[i] = in_missVal
    endfor

    return, aMinData
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
    print, "(cvcCalcMinimum::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
      
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        case calcMode of
          'day': begin ; find fields of min values for each day in each segment separately, result is [lon, lat, days, segments]
		   aUniqDaysPos = uniq(long(sResponse.aTimes-0.5)) ; locate indices of unique days (see uniq() )
    		   if (segIdx eq 0) then begin
                     sz = size(sResponse.aData, /dim) ; dimension of the original data array
                     totMin = fltarr(sz[0], sz[1], n_elements(aUniqDaysPos), numTimeSeg, /nozero) ; define [lon, lat, days, segment]
		     totNumDays = fltarr(numTimeSeg, /nozero) ; number of days in a segment
                   endif
		   totMin[*, *, *, segIdx] = self->regDailyMin(sResponse.aData, aUniqDaysPos, sResponse.missingVal)
		   totNumDays[segIdx] = n_elements(aUniqDaysPos)
		 end
          'segment': begin ; find fields of min values for each segment separately, result is [lon, lat, segments]
	    	       if (segIdx eq 0) then begin  
                         sz = size(sResponse.aData, /dim) ; dimensions of the original data array
    		         totMin = fltarr(sz[0], sz[1], numTimeSeg, /nozero)
                       endif
		       totMin[*, *, segIdx] = self->regSegMin(sResponse.aData, sResponse.missingVal) 
		     end
          'data': begin ; find field of min values over all segments, result is [lon, lat]
                    aCurVals = self->regSegMin(sResponse.aData, sResponse.missingVal) ; get current data field
    	 	    if (segIdx eq 0) then begin
		      totMin = aCurVals
		    endif else begin
        	      self->__findMin, totMin, aCurVals, sResponse.missingVal ; find minimum values of both arrays
                    endelse
                  end
          'mean': begin ; find field of average min values over all segments, result is [lon, lat]
                    tmpMin = self->regSegMin(sResponse.aData, sResponse.missingVal) 
                    if (segIdx eq 0) then begin
		      sz = size(sResponse.aData, /dim) ; dimensions of the original data array
		      totMin = fltarr(sz[0], sz[1], /nozero) ;  define [lon, lat] array
		      totMin[*] = sResponse.missingVal ; fill it with a missing value for the beginning
                      sum = tmpMin ; first input into sum
                      cnt = long(tmpMin ne sResponse.missingVal) ; counter of summed elements
                    endif else begin
                      sumValid = long(sum ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      tmpValid = long(tmpMin ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      idxs = where(tmpValid) ; indices of elements to be summed (we exclude missing values in the array to be added)
                      sum[idxs] = sum[idxs]*sumValid[idxs] + tmpMin[idxs] ; here we zero missing values before summing in both arrays, but only those where at least one of summed values is not missing 
                      cnt[idxs] = cnt[idxs] + 1 ; here we simply increase counters for valid stations
                    endelse
                    if (segIdx eq numTimeSeg - 1) then begin ; at the last segment we calculate mean values for all stations
                      idxs = where(cnt ne 0)
                      if (idxs[0] ne -1) then totMin[idxs] = sum[idxs] / cnt[idxs]
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
          'day': begin ; find min for each station for each day in each segment separately, result is [stations, days, segments]
		   aUniqDaysPos = uniq(long(sResponse.aTimes-0.5)) ; locate indices of unique days (see uniq() )
    		   if (segIdx eq 0) then begin
                     totMin = fltarr((size(sResponse.aData, /dim))[0], n_elements(aUniqDaysPos), numTimeSeg, /nozero)
                     totNumDays = fltarr(numTimeSeg, /nozero)
                   endif
		   totMin[*, *, segIdx] = self->stDailyMin(sResponse.aData, aUniqDaysPos, sResponse.missingVal)
                   totNumDays[segIdx] = n_elements(aUniqDaysPos)
		 end
          'segment': begin ; find min for each station in each segment separately, result is [stations, segments]
    		       if (segIdx eq 0) then totMin = fltarr((size(sResponse.aData, /dim))[0], numTimeSeg, /nozero)
		       totMin[*, segIdx] = self->stSegMin(sResponse.aData, sResponse.missingVal) 
		     end
          'data': begin ; find min for each station over all segments, result is [stations]
                    aCurVals = self->stSegMin(sResponse.aData, sResponse.missingVal) 
    		    if (segIdx eq 0) then begin
		      totMin = aCurVals
		    endif else begin
        	      self->__findMin, totMin, aCurVals, sResponse.missingVal ; find minimum values of both arrays
		    endelse
;                    curValIdxs = where(aCurVals ne sResponse.missingVal, valCnt)
;		    totMinBadFlag = totMin eq sResponse.missingVal
;		    if (valCnt gt 0) then begin
;		      idx = where(aCurVals[curValIdxs] lt totMin[curValIdxs]+totMinBadFlag[curValIdxs]*1e20, cnt)
;		      if (cnt gt 0) then totMin[curValIdxs[idx]] = aCurVals[curValIdxs[idx]]
;		    endif
                  end
          'mean': begin ; find average min for each station over all segments, result is [stations]
                    tmpMin = self->stSegMin(sResponse.aData, sResponse.missingVal) 
                    if (segIdx eq 0) then begin
		      totMin = fltarr((size(sResponse.aData, /dim))[0], /nozero)
		      totMin[*] = sResponse.missingVal
                      sum = tmpMin ; first input into sum
                      cnt = long(tmpMin ne sResponse.missingVal) ; counter of summed elements
                    endif else begin
                      sumValid = long(sum ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      tmpValid = long(tmpMin ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      idxs = where(tmpValid) ; indices of elements to be summed (we exclude missing values in the array to be added)
                      sum[idxs] = sum[idxs]*sumValid[idxs] + tmpMin[idxs] ; here we zero missing values before summing in both arrays, but only those where at least one of summed values is not missing 
                      cnt[idxs] = cnt[idxs] + 1 ; here we simply increase counters for valid stations
                    endelse
                    if (segIdx eq numTimeSeg - 1) then begin ; at the last segment we calculate mean values for all stations
                       idxs = where(cnt ne 0)
                       if (idxs[0] ne -1) then totMin[idxs] = sum[idxs] / cnt[idxs]
                    endif 
                  end
            else: begin
		self->printLog, '(cvcCalcMinimum::Run) Error! Unknown calculation mode: ', calcMode
		return, -1
	    end
        endcase
      endelse
    endfor
    aDataArray = totMin
    idxs = where(aDataArray gt 1e20, cnt)
    if (cnt gt 0) then aDataArray[idxs] = sResponse.missingValue
 
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
                    midSeg[segIdx] = (long(reqTimeJD_0) + long(reqTimeJD_1))/2
                  endfor 
                end
      else: self->printLog, '(cvcCalcMinimum::Run) calcModes of data and mean do not have time grid '
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
