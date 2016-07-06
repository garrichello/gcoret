;
;  NAME: 
;    cvcCalcGSL
;
;  DESCRIPTION:
;    Growing season length. Northern hemisphere ONLY!!!
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of growing season lenth for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcGSL = obj_new('cvcCalcGSL', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcGSL->Run()
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
FUNCTION cvcCalcGSL::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcGSL::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcGSL::Run
    
    self->printLog, "(cvcCalcGSL::Run) Started..."
    
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
    self->printLog, "(cvcCalcGSL::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0

    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
      halfYear = julday(07, 01, yr, 00)
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totGSL = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        sumArr = sResponse.aData[*, *, 0]
        cnt = long(sumArr ne sResponse.missingVal)
        sumArr = sumArr * cnt
        curArr = fltarr(size(sResponse.aData[*,*,0], /dim))
        gslArr = fltarr(size(sResponse.aData[*,*,0], /dim))
        curCnt = cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            curCnt = curCnt + pos
            sumArr = sumArr + tmpArr * pos
          endif else begin
            idxs = where(cnt ne 0)
            if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
            idxs = where(cnt eq 0)
            if (idxs[0] ne -1) then sumArr[idxs] = sResponse.missingVal
            if (long(curDay) lt long(halfYear)) then begin   ; first period: from 1 January to 1 July
              tmpPos = replicate(1, size(sResponse.aData[*,*,0], /dim))
              missPos = sumArr eq sResponse.missingVal
              gslPos = sumArr gt 5
              gslArr = gslArr + gslPos
              tmpPos = tmpPos - gslPos - missPos
              addPos = (gslArr * tmpPos) gt 6
              gslArr = (gslArr + addPos) * (gslPos + addPos)
            endif else begin   ; second period: from 1 July to 31 December
              tmpPos = replicate(1, size(sResponse.aData[*,*,0], /dim))
              missPos = sumArr eq sResponse.missingVal
              gslPos = (sumArr le 5) * (tmpPos - missPos)
              curArr = curArr + gslPos
              curPos = (sumArr gt 5)
              cur1Pos = (curArr * curPos) lt 6
              addArr = cur1Pos * curPos * curArr
              gslArr = gslArr + addArr + curPos * cur1Pos
              curArr = curArr * (tmpPos - cur1Pos * curPos)
            endelse 
              curDay = curDay + 1
              sumArr = sResponse.aData[*, *, i]
              cnt = sumArr ne sResponse.missingVal
              sumArr = sumArr * cnt
              curCnt = curCnt + cnt
            endelse    
          endfor
          idxs = where(curCnt eq 0)
          if (idxs[0] ne -1) then gslArr[idxs] = sResponse.missingVal 
          totCnt[*, *, segIdx] = curCnt
          totGSL[*, *, segIdx] = gslArr
        endif else begin ; for station data
          sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes } 
          if (segIdx eq 0) then begin
            sz = size(sResponse.aData)
            totGSL = fltarr(sz[1], numTimeSeg)
            totCnt = intarr(sz[1], numTimeSeg)  
          endif
          curDay = julday(mn, dy, yr, hr)
          sumArr = sResponse.aData[*, 0]
          cnt = long(sumArr ne sResponse.missingVal)
          sumArr = sumArr * cnt
          curArr = fltarr(size(sResponse.aData[*, 0], /dim))
          gslArr = fltarr(size(sResponse.aData[*, 0], /dim))
          curCnt = cnt
          for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
            if (long(sResponse.aTimes[0, i] - 0.5) eq long(curDay)) then begin
              tmpArr = sResponse.aData[*, i]
              pos = tmpArr ne sResponse.missingVal
              cnt = cnt + pos
              sumArr = sumArr + tmpArr * pos
              curCnt = curCnt + pos
            endif else begin
              idxs = where(cnt ne 0)
              if (idxs[0] ne -1) then sumArr[idxs] = sumArr[idxs] / cnt[idxs]
              if (idxs[0] eq -1) then sumArr[idxs] = sResponse.missingVal
              if (long(curDay) lt long(halfYear)) then begin   ; first period: from 1 January to 1 July
                tmpPos = replicate(1, size(sResponse.aData[*,0], /dim))
                gslPos = sumArr gt 5
                gslArr = gslArr + gslPos
                tmpPos = tmpPos - gslPos
                addPos = (gslArr * tmpPos) gt 6
                gslArr = (gslArr + addPos) * (gslPos + addPos)
              endif else begin   ; second period: from 1 July to 31 December
                tmpPos = replicate(1, size(sResponse.aData[*,0], /dim))
                missPos = sumArr eq sResponse.missingVal
                gslPos = (sumArr le 5) * (tmpPos - missPos)
                curArr = curArr + gslPos
                curPos = (sumArr gt 5)
                cur1Pos = (curArr * curPos) lt 6
                addArr = cur1Pos * curPos * curArr
                gslArr = gslArr + addArr + curPos * cur1Pos
                curArr = curArr * (tmpPos - cur1Pos * curPos)
              endelse 
              curDay = curDay + 1
              sumArr = sResponse.aData[*, i]
              cnt = sumArr ne sResponse.missingVal
              sumArr = sumArr * cnt 
              curCnt = curCnt + cnt
            endelse    
          endfor
          idxs = where(curCnt eq 0)
          if (idxs[0] ne -1) then gslArr[idxs] = sResponse.missingVal 
          totCnt[*, segIdx] = curCnt
          totGSL[*, segIdx] = gslArr
        endelse
      endfor

      if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totGSL[*, *, 0]
        cnt = long(totCnt[*, *, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totGSL[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totGSL[*, 0]
        cnt = long(totCnt[*, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totGSL[*, segIdx]
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
          sz = size(totGSL[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totGSL[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totGSL[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totGSL[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        self->printLog, '(cvcCalcGSL) Error! Unknown calculation mode: ', mode
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
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      endif else begin
        self->printLog, '(cvcCalcGSL) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcGSL__define

    struct = { cvcCalcGSL, $
               INHERITS cvcCalc $ 
             }
END
