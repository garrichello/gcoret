;
;  NAME: 
;    cvcCalcSummerDays
;
;  DESCRIPTION:
;    Annual count of days when daily maximum temperature greater than 25 C
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of summer days for time series of data
;    (Annual count of days when daily maximum temperature greater than 25 C)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcSummerDays = obj_new('cvcCalcSummerDays', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcSummerDays->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcSummerDays::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcSummerDays::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcSummerDays::Run
    self->printLog, "(cvcCalcSummerDays::Run) Started..."
    
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
    self->printLog, "(cvcCalcSummerDays::Run) Get data..."
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
          totID = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        maxArr = sResponse.aData[*, *, 0]
        cnt = long(maxArr ne sResponse.missingVal)
        curDay = julday(mn, dy, yr, hr)
        idArr = fltarr(size(sResponse.aData[*,*,0], /dim))
        begPos = cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) gt maxArr * (curPos - difPos))
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
            idxs = where(tmpArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then maxArr[idxs] = maxArr[idxs]
            idxs = where(maxArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
            begPos = (maxArr ne sResponse.missingVal)
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0)
            idxs = where(maxArr * pos gt 25)
            if (idxs[0] ne -1) then idArr[idxs] = idArr[idxs] + 1     
            curDay = curDay + 1
            maxArr = sResponse.aData[*, *, i]
            cnt = maxArr ne sResponse.missingVal
            begPos = cnt
            totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos            
          endelse
        endfor
        pos = (cnt ne 0)
        idxs = where(maxArr * pos gt 25)
        if (idxs[0] ne -1) then idArr[idxs] = idArr[idxs] + 1
        totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos
        idxs = where(totCnt[*, *, segIdx] eq 0)
        if (idxs[0] ne -1) then idArr[Idxs] = sResponse.missingVal 
        totID[*, *, segIdx]= idArr
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totID = fltarr(sz[1], numTimeSeg)
          totCnt = fltarr(sz[1], numTimeSeg)
        endif
        maxArr = sResponse.aData[*, 0]
        cnt = long(maxArr ne sResponse.missingVal)
        curDay = julday(mn, dy, yr, hr)
        begPos = cnt
        idArr = fltarr(size(sResponse.aData[*,0], /dim))
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) gt maxArr * (curPos - difPos))
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
            idxs = where(tmpArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then maxArr[idxs] = maxArr[idxs]
            idxs = where(maxArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
            begPos = (maxArr ne sResponse.missingVal)
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0)
            idxs = where(maxArr * pos gt 25)
            if (idxs[0] ne -1) then idArr[idxs] = idArr[idxs] + 1     
            curDay = curDay + 1
            maxArr = sResponse.aData[*, i]
            cnt = maxArr ne sResponse.missingVal
            begPos = cnt
            totCnt[*, segIdx] = totCnt[*, segIdx] + pos           
          endelse
        endfor
        pos = (cnt ne 0)
        idxs = where(maxArr * pos gt 25)
        if (idxs[0] ne -1) then idArr[idxs] = idArr[idxs] + 1
        totCnt[*, segIdx] = totCnt[*, segIdx] + pos
        idxs = where(totCnt[*, segIdx] eq 0)
        if (idxs[0] ne -1) then idArr[Idxs] = sResponse.missingVal 
        totID[*, segIdx]= idArr
      endelse
    endfor

if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totID[*, *, 0]
        cnt = long(totCnt[*, *, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totID[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totID[*, 0]
        cnt = long(totCnt[*, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totID[*, segIdx]
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
          sz = size(totID[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totID[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totID[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totID[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        self->printLog, '(cvcCalcSummerDays) Error! Unknown calculation mode: ', mode
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
        self->printLog, '(cvcCalcSummerDays) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res

     return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcSummerDays::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcSummerDays__define
   struct = { cvcCalcSummerDays, $
               INHERITS cvcCalc $
             }
end
