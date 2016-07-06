;
;  NAME: 
;    cvcCalcMinMax
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of minimum values for time series of data
;    (Monthly minimum of daily maximum temperature)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcMinMax = obj_new('cvcCalcMinMax', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcMinMax->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcMinMax::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcMinMax::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcMinMax::Run
    print, "(cvcCalcTimeMean::Run) Started..."
;    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")
    
    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
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
    print, "(cvcCalcMaxMax::Run) Get data..."
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
          totMin = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        maxArr = sResponse.aData[*, *, 0]
        minArr = sResponse.aData[*, *, 0]
        cnt = long(maxArr ne sResponse.missingVal)
        maxArr = maxArr * cnt
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
            idxs = where(maxArr * pos lt minArr * pos)
            if (idxs[0] ne -1) then minArr[idxs] = maxArr[idxs]          
            curDay = curDay + 1
            maxArr = sResponse.aData[*, *, i]
            cnt = maxArr ne sResponse.missingVal
            maxArr = maxArr * cnt
            begPos = cnt
            totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]            
          endelse
        endfor
        pos = (cnt ne 0) 
        idxs = where(maxArr * pos lt minArr * pos)
        if (idxs[0] ne -1) then minArr[idxs] = maxArr[idxs] 
        totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]
        idxs = where(totCnt[*, *, segIdx] eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totMin[*, *, segIdx] = minArr
      endif else begin ; for station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totMin = fltarr(sz[1], numTimeSeg)
          totCnt = fltarr(sz[1], numTimeSeg)
        endif
        curDay = julday(mn, dy, yr, hr)
        maxArr = sResponse.aData[*, 0]
        minArr = sResponse.aData[*, 0]
        cnt = long(minArr ne sResponse.missingVal)
        maxArr = maxArr * cnt
        begPos = cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) gt maxArr * (curPos - difPos))
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0) 
            idxs = where(maxArr * pos lt minArr * pos)
            if (idxs[0] ne -1) then minArr[idxs] = maxArr[idxs]          
            curDay = curDay + 1
            maxArr = sResponse.aData[*, i]
            cnt = maxArr ne sResponse.missingVal
            maxArr = maxArr * cnt
            begPos = cnt
            totCnt[*, segIdx] = totCnt[*, segIdx] + pos            
          endelse
        endfor
        pos = (cnt ne 0) 
        idxs = where(maxArr * pos lt minArr * pos)
        if (idxs[0] ne -1) then minArr[idxs] = maxArr[idxs] 
        totCnt[*, segIdx] = totCnt[*, segIdx] + pos
        idxs = where(totCnt[*,segIdx] eq 0)
        if (idxs[0] ne -1) then minArr[idxs] = sResponse.missingVal 
        totMin[*, segIdx] = minArr
        idxs = where(cnt eq 0)
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
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
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
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    

;    print, "(cvcCalcMinMax::Run) Started..."
;; variable we want to process
;    sVar = self.sIniData.asVar[0] ; only one and the first variable is used
;    ; level to read
;    lev = sVar.sROI.aLev[0]
;        
;    ; main loop for all time segments
;    print, "(cvcCalcMinMax::Run) Get data..."
;    for segIdx = 0, sVar.numTimeSeg-1 do begin
;      sTimeSeg = sVar.asTimeSeg[segIdx] ; take one time segment at a time
;      resultCode = self.oData->Prepare(sVar, sTimeSeg)  ; set region of interest and prepare datasets
;      if (resultCode ne self.ERROR_OK) then return, resultCode
;
;      sResponse = self.oData->GetNextArray(lev)
;      if (sResponse.resultCode ne self.ERROR_OK) then return, sResponse.resultCode
;      aMaxArray = sResponse.aData
;            
;      self->DayMonthYear, sTimeSeg.beginning, yrc, mnc, dyc, hrc
;      dayBegCur = self->LongDate(yrc, mnc, dyc, 0) 
;      dayEndCur = self->LongDate(yrc, mnc, dyc, 18)
;      self->DayMonthYear, sTimeSeg.ending, yre, mne, dye
;      while (dayBegCur le sTimeSeg.ending) do begin
;         sDaySeg = {beginning: dayBegCur, ending: dayEndCur} ; take one day in the time segment
;         sDayResponse = self.oData->GetArray(lev, sDaySeg)
;         aDataArray = max(sDayResponse.aData, Dimension = 3)
;         minIdxs = where(aDataArray lt aMaxArray, cnt)
;         if (cnt gt 0) then begin
;           aMaxArray[minIdxs] = aDataArray[minIdxs] ; minimum of daily maximum parameter
;         endif
;         self->SetNextDay, yrc, mnc, dyc
;         dayBegCur = self->Longdate(yrc, mnc, dyc, 0)
;         dayEndCur = self->LongDate(yrc, mnc, dyc, 18)
;      endwhile
;      
;    endfor
;
;;    lonIdxs = where((sResponse.aLons ge self.sIniData.asVar[0].sROI.lon1) and (sResponse.aLons le self.sIniData.asVar[0].sROI.lon2))
;;    latIdxs = where((sResponse.aLats ge self.sIniData.asVar[0].sROI.lat1) and (sResponse.aLats le self.sIniData.asVar[0].sROI.lat2))
;;    aSubData = aMaxArray[min(lonIdxs):max(lonIdxs), min(latIdxs):max(latIdxs)]
;
; ; set result
;
;;    self.sResult.pData = ptr_new(aMaxArray, /no_copy)
;;    self.sResult.varName = self.sIniData.asVar[0].name
;;    self.sResult.pLons = ptr_new(sResponse.aLons, /no_copy)
;;    self.sResult.pLats = ptr_new(sResponse.aLats, /no_copy)
;;    self.sResult.minVal = min(aSubData)*0.99
;;    self.sResult.maxVal = max(aSubData)*1.01
;;    self.sResult.missingVal = sResponse.missingVal
;;    self.sResult.sTimeRng = sResponse.sTimeRng
;;    self.sResult.aLev = sResponse.aLev
;;    self.sResult.nLev = sResponse.nLev
;
;    self->__SetResult, aData = aMaxArray,$
;                       aLons = sResponse.aLons,$
;                       aLats = sResponse.aLats, $
;                       minVal = min(aMaxArray), $
;                       maxVal = max(aMaxArray), $
;                       missingVal = sResponse.missingVal, $
;                       sTimeRng = sResponse.sTimeRng, $
;                       aLev = sResponse.aLev, $
;                       nLev = sResponse.nLev   
;                        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcMinMax::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcMinMax__define
   struct = { cvcCalcMinMax, $
               INHERITS cvcCalc $
             }
end
