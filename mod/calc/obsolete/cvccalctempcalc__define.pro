;
;  NAME: 
;    cvcCalcTimeMean
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of mean values for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTimeMean = obj_new('cvcCalcTimeMean', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTimeMean->Run()
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
FUNCTION cvcCalcTempCalc::Init, in_sInputs, in_sOutputs

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END
;--------------------------------------------------------------------
PRO cvcCalcTempCalc::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTempCalc::Run

    print, "(cvcCalcTempCalc::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

;    ; get parameters
    res = self->GetParams(aoParams, nParams)
;
;;    res = aoParams[0]->GetInfo(OUT_NTIMESEG=nnn)
;    res = aoParams[0]->Get(asParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='timeSegs', sParam)
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
    print, "(cvcCalcTimeMean::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totDays = fltarr(sz[1], sz[2], numTimeSeg)
        endif
;        for i = 0, n_elements(sResponse.aTimes)-1 do begin
          curData = sResponse.aData
          pos = long(curData ne sResponse.missingVal)
          curData = (curData * 365 * pos) / 100
;        endfor
        totDays[*, *, segIdx]= curData
    endfor
    aDataArray = totDays
           
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
PRO cvcCalcTempCalc__define

    struct = { cvcCalcTempCalc, $
               INHERITS cvcCalc $ 
             }
END