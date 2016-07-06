;
;  NAME: 
;    cvcCalcTmpRange
;
;  DESCRIPTION:
;    Daily temperature range (difference between max and min values)
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of mean values for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTmpRange = obj_new('cvcCalcTmpRange', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTmpRange->Run()
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
;    print, "(cvcCalcETR::Run) Started..."
; variable we want to process
;    sVar = self.sIniData.asVar[0] ; only one and the first variable is used
    ; level to read
;    lev = sVar.sROI.aLev[0]
;--------------------------------------------------------------------
FUNCTION cvcCalcTmpRange::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcTmpRange::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcTmpRange::Run
    self->printLog, "(cvcCalcTmpRange::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; get parameters 
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
      if (self->Assert(res)) then return, res
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
    self->printLog, "(cvcCalcTmpRange::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0

    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
           
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totDtr = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = intarr(sz[1], sz[2], numTimeSeg)   
        endif
        self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
        curDay = julday(reqMonth, reqDay, reqYear, reqHour)
        curArr = sResponse.aData[*, *, 0]
        cnt = long(curArr ne sResponse.missingVal)
        minArr = curArr * cnt
        maxArr = curArr * cnt
        dtrArr = fltarr(size(sResponse.aData[*,*,0], /dim))
        dtrCnt = intarr(size(sResponse.aData[*,*,0], /dim))
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            idxMin = where(tmpArr * pos lt minArr * pos)
            if (idxMin[0] ne -1) then minArr[idxMin] = tmpArr[idxMin]
            idxMax = where(tmpArr * pos gt maxArr * pos)
            if (idxMax[0] ne -1) then maxArr[idxMax] = tmpArr[idxMax]
          endif else begin
            dtrArr = dtrArr + (maxArr - minArr)
            dtrPos = cnt ne 0
            dtrCnt = dtrCnt + dtrPos
            curDay = curDay + 1
            tmpArr = sResponse.aData[*, *, i]
            cnt = tmpArr ne sResponse.missingVal
            minArr = curArr * cnt
            maxArr = curArr * cnt
          endelse
        endfor
        dtrArr = dtrArr + (maxArr - minArr)
        dtrPos = cnt ne 0
        dtrCnt = dtrCnt + dtrPos
        idxs = where(dtrCnt eq 0)
        if (idxs[0] ne -1) then dtrArr[idxs] = sResponse.missingVal 
        totCnt[*, *, segIdx] = dtrCnt
        totDtr[*, *, segIdx]= dtrArr
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes } 
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totDtr = fltarr(sz[1], numTimeSeg)
          totCnt = intarr(sz[1], numTimeSeg)    
        endif
        curDay = min(sResponse[segIdx].aTimes)
        curArr = sResponse.aData[*, 0]
        cnt = long(curArr ne sResponse.missingVal)
        minArr = curArr * cnt
        maxArr = curArr * cnt
        dtrArr = fltarr(size(sResponse.aData[*, 0], /dim))
        dtrCnt = intarr(size(sResponse.aData[*,0], /dim))
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0,i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            idxMin = where(tmpArr * pos lt minArr * pos)
            if (idxMin[0] ne -1) then minArr[idxMin] = tmpArr[idxMin]
            idxMax = where(tmpArr * pos gt maxArr * pos)
            if (idxMax[0] ne -1) then maxArr[idxMax] = tmpArr[idxMax]
          endif else begin
            dtrArr = dtrArr + (maxArr - minArr)
            dtrPos = cnt ne 0
            dtrCnt = dtrCnt + dtrPos
            curDay = curDay + 1
            tmpArr = sResponse.aData[*, i]
            cnt = tmpArr ne sResponse.missingVal
            minArr = curArr * cnt
            maxArr = curArr * cnt
          endelse
        endfor
        dtrArr = dtrArr + (maxArr - minArr)
        dtrPos = cnt ne 0
        dtrCnt = dtrCnt + dtrPos
        idxs = where(dtrCnt eq 0)
        if (idxs[0] ne -1) then dtrArr[idxs] = sResponse.missingVal 
        totCnt[*, segIdx] = dtrCnt
        totDtr[*, segIdx]= dtrArr
      endelse
    endfor

    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totDtr[*, *, 0]
        cnt = totCnt[*, *, 0]
        pos = cnt ne 0
        sum = sum * pos
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totDtr[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + totCnt[*, *, segIdx]
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totDtr[*, 0]
        cnt = totCnt[*, 0]
        pos = cnt ne 0
        sum = sum * pos
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totDtr[*, segIdx]
          pos = totCnt[*, segIdx] ne 0
          cnt = cnt + totCnt[*, segIdx]
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endelse
    endif else begin
      if (calcMode eq 'multi') then begin
        if (sResponse.gridType ne 'station') then begin ; for non-station data
          sz = size(totDtr[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totDtr[*, *, segIdx] / totCnt[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totDtr[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totDtr[*, segIdx] / totCnt[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        self->printLog, '(cvcCalcTmpRange) Error! Unknown calculation mode: ', mode
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
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg[0], MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      endif else begin
        self->printLog, '(cvcCalcTmpRange) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTmpRange::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcTmpRange__define
   struct = { cvcCalcTmpRange, $
               INHERITS cvcCalc $
             }
end
