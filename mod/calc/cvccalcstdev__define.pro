;
;  NAME: 
;    cvcCalcStDev
;
;  DESCRIPTION:
;    Standard deviation
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of standard deviation for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcVariance = obj_new('cvcCalcStDev', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcStDev->Run()
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
FUNCTION cvcCalcStDev::Init, in_sInputs, in_sOutputs, in_hLogFile 

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcStDev::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcStDev::Run

    self->printLog, "(cvcCalcStDev::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcStDev::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          averArray = fltarr(sz[1], sz[2], numTimeSeg)
          totSum = fltarr(sz[1], sz[2])
          totCnt = fltarr(sz[1], sz[2])
        endif
        sum = sResponse.aData[*, *, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          tmpArr = sResponse.aData[*, *, i]
          pos = (tmpArr ne sResponse.missingVal)
          cnt = cnt + pos
          sum = sum + tmpArr * pos
        endfor
        totSum = totSum + sum 
        totCnt = totCnt + cnt
        sum = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
        averArray[*, *, segIdx]= sum
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          averArray = fltarr(sz[1], numTimeSeg)
          totSum = fltarr(sz[1])
          totCnt = fltarr(sz[1])    
        endif
        sum = sResponse.aData[*, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          tmpArr = sResponse.aData[*, i]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos
        endfor
        totSum = totSum + sum 
        totCnt = totCnt + cnt
        sum = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
        averArray[*, segIdx]= sum
      endelse
    endfor
    totSum = totSum / totCnt
    idxs = where(totCnt eq 0)
    if (idxs[0] ne -1) then totSum[idxs] = sResponse.missingVal 

    ; Calculation of standard deviation value
    if (sResponse.gridType ne 'station') then begin ; for non-station data
      tmpArr = averArray[*, *, 0]
      cnt = tmpArr ne sResponse.missingVal
      aStDev = (tmpArr * cnt - totSum * cnt)^2 
      for segIdx = 1, numTimeSeg - 1 do begin
        tmpArr = averArray[*, *, segIdx]
        pos = tmpArr ne sResponse.missingVal
        cnt = cnt + pos
        aStDev = aStDev + (tmpArr * pos - totSum * pos)^2
      endfor
    endif else begin ; for station data
      tmpArr = averArray[*, 0]
      cnt = tmpArr ne sResponse.missingVal
      aStDev = (tmpArr * cnt - totSum * cnt)^2      
      for segIdx = 1,numTimeSeg - 1 do begin
        tmpArr = averArray[*, segIdx]
        pos = tmpArr ne sResponse.missingVal
        cnt = cnt + pos
        aStDev = aStDev + (tmpArr * pos - totSum * pos)^2
      endfor
    endelse
    aDataArray = sqrt(aStDev / (cnt - 1))
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal

;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
;    midSeg = fltarr(numTimeSeg)
;    for segIdx = 0, numTimeSeg - 1 do begin
      self->DayMonthYear, asTimeSeg[0].beginning, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
      self->DayMonthYear, asTimeSeg[0].ending, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
      midSeg = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
;    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcStDev__define

    struct = { cvcCalcStDev, $
               INHERITS cvcCalc $ 
             }
END
