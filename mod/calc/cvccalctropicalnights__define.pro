;
;  NAME: 
;    cvcCalcTropicalNights
;
;  DESCRIPTION:
;    Annual count of days when daily minimum temperature greater than 20 C
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of tropical nights counts for time series of data
;    (Annual count of days when daily minimum temperature greater than 20 C)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTropicalNights = obj_new('cvcCalcTropicalNights', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTropicalNights->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcTropicalNights::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcTropicalNights::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcTropicalNights::Run
    self->printLog, "(cvcCalcTropicalNights::Run) Started..."
    
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
    self->printLog, "(cvcCalcTropicalNights::Run) Get data..."
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
          totFD = fltarr(sz[1], sz[2], numTimeSeg)
          totCnt = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        minArr = sResponse.aData[*, *, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
        curDay = julday(mn, dy, yr, hr)
        fdArr = fltarr(size(sResponse.aData[*,*,0], /dim))
        begPos = cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, *, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) lt minArr * (curPos - difPos))
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            idxs = where(tmpArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = minArr[idxs]
            idxs = where(minArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            begPos = (minArr ne sResponse.missingVal)
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0)
            idxs = where(minArr * pos gt 20)
            if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1     
            curDay = curDay + 1
            minArr = sResponse.aData[*, *, i]
            cnt = minArr ne sResponse.missingVal
            begPos = cnt
            totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]            
          endelse
        endfor
        pos = (cnt ne 0)
        idxs = where(minArr * pos gt 20)
        if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1
        totCnt[*, *, segIdx] = totCnt[*, *, segIdx] + pos[*, *]
        missingIdxs = where(cnt eq 0)
        if (missingIdxs[0] ne -1) then fdArr[missingIdxs] = sResponse.missingVal 
        totFD[*, *, segIdx]= fdArr
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes } 
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totFD = fltarr(sz[1], numTimeSeg)
          totCnt = fltarr(sz[1], numTimeSeg)
        endif
        minArr = sResponse.aData[*, 0]
        cnt = long(minArr ne sResponse.missingVal)
        minArr = minArr * cnt
        curDay = julday(mn, dy, yr, hr)
        fdArr = fltarr(size(sResponse.aData[*,0], /dim))
        begPos = cnt
        for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
          if (long(sResponse.aTimes[0, i] - 0.5) eq long(curDay)) then begin
            tmpArr = sResponse.aData[*, i]
            curPos = (tmpArr ne sResponse.missingVal)
            difPos = (curPos ne begPos)
            idxs = where(tmpArr * (curPos - difPos) lt minArr * (curPos - difPos))
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            idxs = where(tmpArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = minArr[idxs]
            idxs = where(minArr * difPos eq sResponse.missingVal)
            if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
            begPos = (minArr ne sResponse.missingVal)
            cnt = cnt + curPos
          endif else begin
            pos = (cnt ne 0)
            idxs = where(minArr * pos gt 20)
            if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1     
            curDay = curDay + 1
            minArr = sResponse.aData[*, i]
            cnt = minArr ne sResponse.missingVal
            begPos = cnt
            totCnt[*, segIdx] = totCnt[*, segIdx] + pos            
          endelse
        endfor
        pos = (cnt ne 0)
        idxs = where(minArr * pos gt 20)
        if (idxs[0] ne -1) then fdArr[idxs] = fdArr[idxs] + 1
        totCnt[*, segIdx] = totCnt[*, segIdx] + pos
;        missingIdxs = where(cnt eq 0)
;        if (missingIdxs[0] ne -1) then fdArr[missingIdxs] = sResponse.missingVal 
        totFD[*, segIdx]= fdArr
      endelse
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totFD[*, *, 0]
        cnt = long(totCnt[*, *, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totFD[*, *, segIdx]
          pos = totCnt[*, *, segIdx] ne 0
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sum = totFD[*, 0]
        cnt = long(totCnt[*, 0] ne 0)
        sum = sum * cnt
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totFD[*, segIdx]
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
          sz = size(totFD[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totFD[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sz = size(totFD[*, 0])
          aDataArray = fltarr(sz[1], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totFD[*, segIdx]
            idxs = where(totCnt[*, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, segIdx] = sum
          endfor
        endelse
      endif else begin
        self->printLog, '(cvcCalcTropicalNights) Error! Unknown calculation mode: ', mode
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
        self->printLog, '(cvcCalcTropicalNights) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
;    print, "(cvcCalcTropicalNights::Run) Started..."
;; variable we want to process
;    sVar = self.sIniData.asVar[0] ; only one and the first variable is used
;    ; level to read
;    lev = sVar.sROI.aLev[0]
;        
;    ; main loop for all time segments
;    print, "(cvcCalcTropicalNights::Run) Get data..."
;    for segIdx = 0, sVar.numTimeSeg-1 do begin
;      sTimeSeg = sVar.asTimeSeg[segIdx] ; take one time segment at a time
;      resultCode = self.oData->Prepare(sVar, sTimeSeg)  ; set region of interest and prepare datasets
;      if (resultCode ne self.ERROR_OK) then return, resultCode
;
;      sResponse = self.oData->GetNextArray(lev)
;      if (sResponse.resultCode ne self.ERROR_OK) then return, sResponse.resultCode
;      sz = size(sResponse.aData)
;      aMaxArray = intarr(sz[1], sz[2]) ;sResponse.aData
; 
;      self->DayMonthYear, sTimeSeg.beginning, yrc, mnc, dyc, hrc
;      dayBegCur = self->LongDate(yrc, mnc, dyc, 0) 
;      dayEndCur = self->LongDate(yrc, mnc, dyc, 18)
;      self->DayMonthYear, sTimeSeg.ending, yre, mne, dye
;      while (dayBegCur le sTimeSeg.ending) do begin
;         sDaySeg = {beginning: dayBegCur, ending: dayEndCur} ; take one day in the time segment
;         sDayResponse = self.oData->GetArray(lev, sDaySeg)
;         validIdx = where(sDayResponse.aData ne sDayResponse.missingVal)
;         aDataArray = min(sDayResponse.aData, dimension = 3)
;         maxIdxs = where(aDataArray[validIdx] gt 293.15, cnt)
;         if (cnt gt 0) then begin
;           aMaxArray[maxIdxs] = aMaxArray[maxIdxs] + 1 
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
;    self->__SetResult, aData = aMaxArray,$
;                       aLons = sResponse.aLons,$
;                       aLats = sResponse.aLats, $
;                       minVal = min(aMaxArray) , $
;                       maxVal = max(aMaxArray) , $
;                       missingVal = sResponse.missingVal, $
;                       sTimeRng = sResponse.sTimeRng, $
;                       aLev = sResponse.aLev, $
;                       nLev = sResponse.nLev

     return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcTropicalNights::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcTropicalNights__define
   struct = { cvcCalcTropicalNights, $
               INHERITS cvcCalc $
             }
end
