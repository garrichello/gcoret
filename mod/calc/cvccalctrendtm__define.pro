;
;  NAME: 
;    cvcCalcTrendTM
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of trend for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTrendTM = obj_new('cvcCalcTrendTM', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTrendTM->Run()
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
FUNCTION cvcCalcTrendTM::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcTrendTM::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcTrendTM::Run
    
    self->printLog, "(cvcCalcTrendTM::Run) Started..."
    
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
    self->printLog, "(cvcCalcTimeMean::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0

    ; Time mean value for all segments
    for segIdx = 0, numTimeSeg - 1 do begin 
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Region=sROI_IP, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      self->DayMonthYear, asTimeSeg[segIdx].beginning, Yr, Mn, Dy, Hr
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if segIdx eq 0 then begin 
          sum = sResponse.aData
          sz = size(sum)
          tmpSeg = fltarr(sz[1], sz[2], numTimeSeg)
          tmpSeg[*, *, segIdx] = sum
          cnt = long(sum ne sResponse.missingVal)
          sum = sum * cnt
          yrSum = Yr * cnt
        endif else begin 
          tmpArr = sResponse.aData
          pos = tmpArr ne sResponse.missingVal
          tmpSeg[*, *, segIdx] = tmpArr
          sum = sum + tmpArr * pos    
          cnt = cnt + pos
          yrSum = yrSum + Yr * pos
        endelse
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        if segIdx eq 0 then begin 
          sum = sResponse.aData
          sz = size(sum)
          tmpSeg = fltarr(sz[1], numTimeSeg)
          tmpSeg[*, segIdx] = sum
          cnt = long(sum ne sResponse.missingVal)
          sum = sum * cnt
          yrSum = Yr * cnt
        endif else begin 
          tmpArr = sResponse.aData
          pos = tmpArr ne sResponse.missingVal
          tmpSeg[*, segIdx] = tmpArr
          sum = sum + tmpArr * pos    
          cnt = cnt + pos
          yrSum = yrSum + Yr * pos
        endelse 
      endelse
    endfor
    sum = sum / cnt
    yrSum = yrSum / cnt
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then begin
      sum[idxs] = sResponse.missingVal
      yrSum[idxs] = sResponse.missingVal
    endif
    
    ; Calculation of trend value
    if (sResponse.gridType ne 'station') then begin ; for non-station data
      for segIdx = 0, numTimeSeg - 1 do begin
        self->DayMonthYear, asTimeSeg[segIdx].beginning, Yr, Mn, Dy, Hr
        pos = tmpSeg[*, *, segIdx] ne sResponse.missingVal
        if segIdx eq 0 then begin
          numArr = (tmpSeg[*, *, segIdx] - sum) * (Yr - yrSum) * pos
          denArr = ((Yr - yrSum)^2) * pos
        endif else begin
          numArr = numArr + (tmpSeg[*, *, segIdx] - sum) * (Yr - yrSum) * pos
          denArr = denArr + ((Yr - yrSum)^2) * pos
        endelse 
      endfor
    endif else begin
      sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
      for segIdx = 0, numTimeSeg - 1 do begin
        self->DayMonthYear, asTimeSeg[segIdx].beginning, Yr, Mn, Dy, Hr
        pos = tmpSeg[*, segIdx] ne sResponse.missingVal
        if segIdx eq 0 then begin
          numArr = (tmpSeg[*, segIdx] - sum) * (Yr - yrSum) * pos
          denArr = ((Yr - yrSum)^2) * pos
        endif else begin
          numArr = numArr + (tmpSeg[*, segIdx] - sum) * (Yr - yrSum) * pos
          denArr = denArr + ((Yr - yrSum)^2) * pos
        endelse 
      endfor
    endelse
    ; Trend value for 10 yr period
    aDataArray = (numArr/denArr) * 10
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal
    
    aLons = sResponse.aLons
    aLats = sResponse.aLats
 
     ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0
    
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)   
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcTrendTM__define

    struct = { cvcCalcTrendTM, $
               INHERITS cvcCalc $ 
             }
END
