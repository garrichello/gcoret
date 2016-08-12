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
FUNCTION cvcCalcMaximum::regDailyMax, in_aData, in_aUniqDaysPos
  sz = size(in_aData, /dim)
  if (n_elements(sz) eq 2) then begin ; no time dimension
    aMaxData = in_aData ; nothing to search
; ToDo: this will return 2-D array, outside 3-D is expected, may be some improvements will be required
  endif else begin
    nTimes = sz[2] ; time dimension
    nDays = n_elements(in_aUniqDaysPos) ; number of unique days
    aMaxData = fltarr(sz[0], sz[1], nDays, /nozero) ; output array with fields of maximum values for each day

; array in_aUniqDays contains indices of the LAST element in the sequence of times corresponding to a day
    stDayPos = 0 ; we start from the first time point on a time grid (starting from the beginning of the first day)
    for iDay = 0, nDays - 1 do begin ; for each unique day...
      aMaxData[*, *, iDay] = in_aData[*, *, stDayPos] ; for the start we set maximum as a field at the beginning of the day
      for iTime = stDayPos+1, in_aUniqDaysPos[iDay] do begin ; starting from the next time step until the end of the day
        tmpData = in_aData[*, *, iTime]; we choose data fields
        idxs = where(tmpData gt aMaxData[*, *, iDay], cnt) ; and search for the maximum elements
        if (cnt gt 0) then aMaxData[idxs] = tmpData[idxs] ; which are stored in the resulting array
      endfor
      stDayPos = in_aUniqDaysPos[iDay] + 1 ; set start position to the beginning of the next day
    endfor
  endelse

  return, aMaxData
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMaximum::regSegMax, in_aData
  sz = size(in_aData, /dim)
  if (n_elements(sz) eq 2) then begin ; no time dimension
    aMaxData = in_aData ; nothing to search
  endif else begin
    nTimes = sz[2] ; time dimension

    aMaxData = in_aData[*, *, 0] ; let's search along the time dimension
    for iTime = 1, nTimes - 1 do begin
      tmpData = in_aData[*, *, iTime]
      idxs = where(tmpData gt aMaxData, cnt)
      if (cnt gt 0) then aMaxData[idxs] = tmpData[idxs]
    endfor
  endelse
  return, aMaxData
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMaximum::stDailyMax, in_aData, in_aUniqDaysPos

  sz = size(in_aData, /dim)
  nStations = sz[0] ; number of stations
  nDays = n_elements(in_aUniqDaysPos) ; number of unique days
  
  aMaxData = fltarr(nStations, nDays, /nozero) ; output array with maximum values for each station and day

; array in_aUniqDays contains indices of the LAST element in the sequence corresponding to the whole day
  for iSt = 0, nStations - 1 do begin
    stDayPos = 0 ; we start from the first time point on a time grid (starting from the beginning of the first day)
    for iDay = 0, nDays - 1 do begin ; for each unique day...
      aMaxData[iSt, iDay] = max(in_aData[iSt, stDayPos:in_aUniqDaysPos[iDay]]) ; ... find maximum during this day
      stDayPos = in_aUniqDaysPos[iDay] + 1 ; set starting point to the element corresponding to the beginning of the next day
    endfor
  endfor

  return, aMaxData
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMaximum::stSegMax, in_aData

    sz = size(in_aData, /dim)
    nStations = sz[0]; number of stations

    aMaxData = fltarr(nStations, /nozero); allocate array for max values

    for i = 0, nStations-1 do begin
      aMaxData[i] = max(in_aData[i, *])
    endfor

    return, aMaxData
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
          'day': begin ; find fields of max values for each day in each segment separately, result is [lon, lat, days, segments]
		   totNumDays = fltarr(numTimeSeg, /nozero)   
		   aUniqDaysPos = uniq(long(sResponse.aTimes-0.5)) ; locate indices of unique days (see uniq() )
    		   if (segIdx eq 0) then begin
                     sz = size(sResponse.aData, /dim) ; dimension of the original data array
                     totMax = fltarr(sz[0], sz[1], n_elements(aUniqDaysPos), numTimeSeg, /nozero) ; define [lon, lat, days, segment]
                   endif
		   totMax[*, *, *, segIdx] = self->regDailyMax(sResponse.aData, aUniqDaysPos)
		   totNumDays[segIdx] = n_elements(aUniqDaysPos)
		 end
          'segment': begin ; find fields of max values for each segment separately, result is [lon, lat, segments]
	    	       if (segIdx eq 0) then begin  
                         sz = size(sResponse.aData, /dim) ; dimensions of the original data array
    		         totMax = fltarr(sz[0], sz[1], numTimeSeg, /nozero)
                       endif
		       totMax[*, *, segIdx] = self->regSegMax(sResponse.aData) 
		     end
          'data': begin ; find field of max values over all segments, result is [lon, lat]
    	 	    if (segIdx eq 0) then begin
		      sz = size(sResponse.aData, /dim) ; dimensions of the original data array
		      totMax = fltarr(sz[0], sz[1], /nozero) ; define [lon, lat] array
		      totMax[*] = -1e20 ; very negative value for the beginning
		    endif
                    tmpMax = self->regSegMax(sResponse.aData) 
                    idx = where(tmpMax gt totMax, cnt)
		    if (cnt gt 0) then totMax[idx] = tmpMax[idx]
                  end
          'mean': begin ; find field of average max values over all segments, result is [lon, lat]
                    tmpMax = self->regSegMax(sResponse.aData) 
                    if (segIdx eq 0) then begin
		      sz = size(sResponse.aData, /dim) ; dimensions of the original data array
		      totMax = fltarr(sz[0], sz[1], /nozero) ;  define [lon, lat] array
		      totMax[*] = sResponse.missingVal ; fill it with a missing value for the beginning
                      sum = tmpMax ; first input into sum
                      cnt = long(tmpMax ne sResponse.missingVal) ; counter of summed elements
                    endif else begin
                      sumValid = long(sum ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      tmpValid = long(tmpMax ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      idxs = where(tmpValid) ; indices of elements to be summed (we exclude missing values in the array to be added)
                      sum[idxs] = sum[idxs]*sumValid[idxs] + tmpMax[idxs] ; here we zero missing values before summing in both arrays, but only those where at least one of summed values is not missing 
                      cnt[idxs] = cnt[idxs] + 1 ; here we simply increase counters for valid stations
                    endelse
                    if (segIdx eq numTimeSeg - 1) then begin ; at the last segment we calculate mean values for all stations
                      idxs = where(cnt ne 0)
                      if (idxs[0] ne -1) then totMax[idxs] = sum[idxs] / cnt[idxs]
                    endif 
                  end
          else: begin
		  self->printLog, '(cvcCalcMaximum) Error! Unknown calculation mode: ', calcMode
		  return, -1
		end
        endcase
      endif else begin ; station data process
	sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        case calcMode of
          'day': begin ; find max for each station for each day in each segment separately, result is [stations, days, segments]
		   totNumDays = fltarr(numTimeSeg, /nozero)
		   aUniqDaysPos = uniq(long(sResponse.aTimes-0.5)) ; locate indices of unique days (see uniq() )
    		   if (segIdx eq 0) then totMax = fltarr((size(sResponse.aData, /dim))[0], n_elements(aUniqDaysPos), numTimeSeg, /nozero)
		   totMax[*, *, segIdx] = self->stDailyMax(sResponse.aData, aUniqDaysPos)
		   totNumDays[segIdx] = n_elements(aUniqDaysPos)
		 end
          'segment': begin ; find max for each station in each segment separately, result is [stations, segments]
    		       if (segIdx eq 0) then totMax = fltarr((size(sResponse.aData, /dim))[0], numTimeSeg, /nozero)
		       totMax[*, segIdx] = self->stSegMax(sResponse.aData) 
		     end
          'data': begin ; find max for each station over all segments, result is [stations]
    		    if (segIdx eq 0) then begin
		      totMax = fltarr((size(sResponse.aData, /dim))[0], /nozero)
		      totMax[*] = -1e20 ; very negative value for the beginning
		    endif
                    tmpMax = self->stSegMax(sResponse.aData) 
                    idx = where(tmpMax gt totMax, cnt)
		    if (cnt gt 0) then totMax[idx] = tmpMax[idx]
                  end
          'mean': begin ; find average max for each station over all segments, result is [stations]
                    tmpMax = self->stSegMax(sResponse.aData) 
                    if (segIdx eq 0) then begin
		      totMax = fltarr((size(sResponse.aData, /dim))[0], /nozero)
		      totMax[*] = sResponse.missingVal
                      sum = tmpMax ; first input into sum
                      cnt = long(tmpMax ne sResponse.missingVal) ; counter of summed elements
                    endif else begin
                      sumValid = long(sum ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      tmpValid = long(tmpMax ne sResponse.missingVal) ; 1 - valid value, 0 - missing value
		      idxs = where(tmpValid) ; indices of elements to be summed (we exclude missing values in the array to be added)
                      sum[idxs] = sum[idxs]*sumValid[idxs] + tmpMax[idxs] ; here we zero missing values before summing in both arrays, but only those where at least one of summed values is not missing 
                      cnt[idxs] = cnt[idxs] + 1 ; here we simply increase counters for valid stations
                    endelse
                    if (segIdx eq numTimeSeg - 1) then begin ; at the last segment we calculate mean values for all stations
                       idxs = where(cnt ne 0)
                       if (idxs[0] ne -1) then totMax[idxs] = sum[idxs] / cnt[idxs]
                    endif 
                  end
            else: begin
		self->printLog, '(cvcCalcMaximum::Run) Error! Unknown calculation mode: ', calcMode
		return, -1
	    end
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
    else: begin
	    self->printLog, '(cvcCalcMaximum::Run) Error! Unknown calculation mode: ', calcMode
	    return, -1
	  end
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
