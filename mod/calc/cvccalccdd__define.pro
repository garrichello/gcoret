;
;  NAME: 
;    cvcCalcCDD
;
;  DESCRIPTION:
;    Maximum number of consecutive dry days (RR < 1 mm)
;
;  PURPOSE:
;    Implements calculation of a spatial field of number of consecutive dry days for time series of data
;    (Maximum lenght of gry spell, maximum number of consecutive days with RR < 1mm)
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcCDD = obj_new('cvcCalcCDD', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcCDD->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcCDD::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcCDD::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcCDD::Run

    self->printLog, "(cvcCalcCDD::Run) Started..."

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
    self->printLog, "(cvcCalcrx5day::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode
      self->DayMonthYear, asTimeSeg[segIdx].beginning, yr, mn, dy, hr
    
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totCDD = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        curCDD = fltarr(size(sResponse.aData[*, *, 0], /dim))
        maxCDD = fltarr(size(sResponse.aData[*, *, 0], /dim))
        segCnt = fltarr(size(sResponse.aData[*, *, 0], /dim))
        curPos = curCDD ne sResponse.MissingVal
        sum = sResponse.aData[*, *, 0]
        cnt = long(sum ne sResponse.missingVal)
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            sum = sum + tmpArr * pos
          endif else begin
            valPos = cnt ne 0
            segCnt = segCnt + valPos
            idxs = where(sum lt 1)
            if (idxs[0] ne -1) then curCDD[idxs] = curCDD[idxs] + 1
            cddPos = ((curPos eq 1) or (valPos eq 1))
            curCDD = curCDD * cddPos
            curPos = cddPos
            if (idxs[0] ne - 1) then valPos[idxs] = 0
            idxs = where(curCDD * valPos gt maxCDD * valPos)
            if (idxs[0] ne -1) then maxCDD[idxs] = curCDD[idxs]
            tmpPos = replicate(1, size(sResponse.aData[*,*,0], /dim))
            curCDD = curCDD * (tmpPos - valPos)
            curDay = curDay + 1
            sum = sResponse.aData[*, *, i]
            cnt = long(sum ne sResponse.missingVal)
            sum = sum * cnt
          endelse
        endfor
        valPos = cnt ne 0
        segCnt = segCnt + valPos
        idxs = where(sum lt 1)
        if (idxs[0] ne -1) then curCDD[idxs] = curCDD[idxs] + 1
        cddPos = ((curPos eq 1) or (valPos eq 1))
        curCDD = curCDD * cddPos
        idxs = where(curCDD gt maxCDD)
        if (idxs[0] ne -1) then maxCDD[idxs] = curCDD[idxs]
        idxs = where(segCnt eq 0)
        if (idxs[0] ne -1) then maxCDD[idxs] = sResponse.missingVal
        totCDD[*, *, segIdx] = maxCDD
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }	
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totCDD = fltarr(sz[1], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        curCDD = fltarr(size(sResponse.aData[*, 0], /dim))
        maxCDD = fltarr(size(sResponse.aData[*, 0], /dim))
        segCnt = fltarr(size(sResponse.aData[*, 0], /dim))
        curPos = curCDD ne sResponse.MissingVal
        sum = sResponse.aData[*, 0]
        cnt = long(sum ne sResponse.missingVal)
        sum = sum * cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i]) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            pos = tmpArr ne sResponse.missingVal
            cnt = cnt + pos
            sum = sum + tmpArr * pos
          endif else begin
            valPos = cnt ne 0
            segCnt = segCnt + valPos
            idxs = where(sum lt 1)
            if (idxs[0] ne -1) then curCDD[idxs] = curCDD[idxs] + 1
            cddPos = ((curPos eq 1) or (valPos eq 1))
            curCDD = curCDD * cddPos
            curPos = cddPos
            if (idxs[0] ne - 1) then valPos[idxs] = 0
            idxs = where(curCDD * valPos gt maxCDD * valPos)
            if (idxs[0] ne -1) then maxCDD[idxs] = curCDD[idxs]
            tmpPos = replicate(1, size(sResponse.aData[*, 0], /dim))
            curCDD = curCDD * (tmpPos - valPos)
            curDay = curDay + 1
            sum = sResponse.aData[*, i]
            cnt = long(sum ne sResponse.missingVal)
            sum = sum * cnt
          endelse
        endfor
        valPos = cnt ne 0
        segCnt = segCnt + valPos
        idxs = where(sum lt 1)
        if (idxs[0] ne -1) then curCDD[idxs] = curCDD[idxs] + 1
        cddPos = ((curPos eq 1) or (valPos eq 1))
        curCDD = curCDD * cddPos
        idxs = where(curCDD gt maxCDD)
        if (idxs[0] ne -1) then maxCDD[idxs] = curCDD[idxs]
        idxs = where(segCnt eq 0)
        if (idxs[0] ne -1) then maxCDD[idxs] = sResponse.missingVal
        totCDD[*, segIdx] = maxCDD
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totCDD[*, *, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totCDD[*, *, segIdx]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totCDD[*, 0]
        cnt = sum ne sResponse.missingVal
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totCDD[*, segIdx]
          pos = tmpArr ne sResponse.missingVal
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
          aDataArray = totSum
        endif else begin ; for station data
          sz = size(totSum[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totSum[*, segIdx] / totCnt[*, segIdx]
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
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      endif else begin
        self->printLog, '(cvcCalcCDD) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcCDD__define

    struct = { cvcCalcCDD, $
               INHERITS cvcCalc $ 
             }
END