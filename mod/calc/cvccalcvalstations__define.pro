;
;  NAME: 
;    cvcCalcValStations
;
;  DESCRIPTION:
;    Analysis of time series homogeneity
;
;  PURPOSE:
;    
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMean = obj_new('cvcCalcValStations', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcValStations->Run()
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
FUNCTION cvcCalcValStations::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcSelect::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcValStations::Run

    self->printLog, "(cvcCalcValStations::Run) Started..."
    
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
    self->printLog, "(cvcCalcValStations::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    for segIdx = 0, numTimeSeg - 1 do begin
      self->printLog, "Segment = ", segIdx
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      self->DayMonthYear, asTimeSeg[segIdx].beginning, begYr, begMnt, begDy, begHr
      begJDay = julday(begMnt, begDy, begYr, begHr)
      if (segIdx eq 0) then begin
        sz = size(sResponse.aData)
        totValSTs = fltarr(sz[1])
        totValData = fltarr(sz[1], sz[2], numTimeSeg)
      endif
      dayIdx = 1
      misDay = fltarr(size(sResponse.aData[*, 0], /dim))
      misMonth = fltarr(size(sResponse.aData[*, 0], /dim))
      misSPoint = fltarr(size(sResponse.aData[*, 0], /dim))
      misMsure = where(sResponse.aData[*, 0] eq sResponse.missingVal)
      if (misMsure[0] ne -1) then misDay[misMsure] = 1.0
      for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
        caldat, sResponse.aTimes[0, i], curMnt, curDy, curYr, curHr
        if (curMnt eq begMnt) then begin
          if (sResponse.aTimes[0, i] eq (begJDay + dayIdx - 1)) then begin
            curMsure = where(sResponse.aData[*, i] eq sResponse.missingVal)
            if (curMsure[0] ne -1) then misDay[curMsure] = misDay[curMsure] + 1.0
          endif else begin
            idx = where(misDay ne 0.0)
            if (idx[0] ne -1) then misMonth[idx] = misMonth[idx] + 1.0
            dayIdx = dayIdx + 1
            misDay = fltarr(size(sResponse.aData[*, 0], /dim))
            curMsure = where(sResponse.aData[*, i] eq sResponse.missingVal)
            if (curMsure[0] ne -1) then misDay[curMsure] = 1.0
          endelse  
        endif else begin
          idx = where(misDay ne 0.0)
          if (idx[0] ne -1) then misMonth[idx] = misMonth[idx] + 1.0
          num = long(0.1*dayIdx)
          misMnt = where(misMonth gt num)
          if (misMnt[0] ne -1) then misSPoint[misMnt] = 1.0
          misMonth = fltarr(size(sResponse.aData[*, 0], /dim))
          begMnt = begMnt + 1
        endelse
      endfor
      totValSTs = totValSTs + misSPoint 
    endfor
    idxs = where(totValSTs gt float(numTimeSeg*0.05), complement = valIdx)
    if (idxs[0] ne -1) then totValSTs[idxs] = 0.0
    if (valIdx[0] ne -1) then totValSTs[valIdx] = 1.0
    self->printLog, "Number of stations across selected region = ", (size(totValSTs))[1]
    self->printLog, "Valid stations = ", (size(ValIdx))[1]
    self->printLog, "Unvalid stations, = ", (size(idxs))[1]
    aDataArray = totValSTs
  
;    case sResponse.gridType of
        aLons = sResponse.aLons
        aLats = sResponse.aLats

        self->DayMonthYear, asTimeSeg[0].beginning, reqYear, reqMonth, reqDay, reqHour
        reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
        self->DayMonthYear, asTimeSeg[0].ending, reqYear, reqMonth, reqDay, reqHour
        reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
        midSeg = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2

    ; stations names
    sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
     
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0
    res = (aoOutputs[outIdx])->Put(aDataArray)
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)

    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcValStations__define

    struct = { cvcCalcValStations, $
               INHERITS cvcCalc $ 
             }
END