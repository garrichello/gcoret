;
;  NAME: 
;    cvcCalcMinMin
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of minimum values for time series of data
;    (Monthly minimum of daily minimum temperature)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMinMin = obj_new('cvcCalcMinMin', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcMinMin->Run()
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
FUNCTION cvcCalcMinMin::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcMinMin::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcMinMin::Run
     self->printLog, "(cvcCalcMinMin::Run) Started..."
    
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

    calcMode = 'single' ; default value
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
      if (res[0] ne -1) then calcMode = *(sParam.data)
    endif
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcMinMin::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
    
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totMin = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        minArr = sResponse.aData[*, *, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          tmpArr = sResponse.aData[*, *, i]
          pos = long(tmpArr ne sResponse.missingVal)
          idxs = where(tmpArr * pos lt minArr * pos)
          if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
        endfor
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totCnt[*, *, segIdx] = cnt
        totMin[*, *, segIdx]= minArr
      endif else begin ; for station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totMin = fltarr(sz[1], numTimeSeg)
          totCnt = intarr(sz[1], numTimeSeg)   
        endif
        minArr = sResponse.aData[*, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          tmpArr = sResponse.aData[*, i]
          pos = tmpArr ne sResponse.missingVal
          idxs = where(tmpArr * pos lt minArr * pos)
          if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
        endfor
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totCnt[*, segIdx] = cnt
        totMin[*,segIdx]= minArr
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totMin[*, *, 0]
        cnt = long(totCnt[*, *, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totMin[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totMin[*, 0]
        cnt = long(totCnt[*, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totMin[*, segIdx]
          pos = totCnt[*, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endelse
    endif else begin
      if (calcMode eq 'multi') then begin
        if (sResponse.gridType ne 'station') then begin ; for non-station data
          sz = size(totMin[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totMin[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totMin[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totMin[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    
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
        self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res


    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinMin::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcMinMin__define
   struct = { cvcCalcMinMin, $
               INHERITS cvcCalc $
             }
end
