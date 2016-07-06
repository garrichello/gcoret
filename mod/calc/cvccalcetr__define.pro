;
;  NAME: 
;    cvcCalcETR
;
;  DESCRIPTION:
;    Extreme temperature range: annual/monthly difference between highest and lowest temperature (can be replaced by combination of codes Maximum and Minimum)
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of absole extreme temperature for time series of data
;    (Extreme temperature range: Annual/monthly difference between highest and lowest temperature)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcETR = obj_new('cvcCalcETR', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcETR->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcETR::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcETR::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcETR::Run
    self->printLog, "(cvcCalcETR::Run) Started..."
    
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
    self->printLog, "(cvcCalcETR::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    ; time segment to read
    segIdx = 0

    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode  

      if (sResponse.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          sz = size(sResponse.aData)
          totETR = fltarr(sz[1], sz[2], numTimeSeg)
        endif
        maxArr = sResponse.aData[*, *, 0]
        minArr = sResponse.aData[*, *, 0]
        maxPos = long(maxArr ne sResponse.missingVal)
        minPos = long(minArr ne sResponse.missingVal)
        maxArr = maxArr * cnt
        minArr = minArr * cnt
        for i = 1, n_elements(sResponse.aTimes)-1 do begin
          tmpArr = sResponse.aData[*, *, i]
          pos = (tmpArr ne sResponse.missingVal)
          
          valPos = (pos eq 1) and (maxPos eq 1)
          idxs = where(tmpArr * valPos gt maxArr * valPos)
          if (idxs[0] ne -1) then begin
            maxArr[idxs] = tmpArr[idxs]
            pos[idxs] = 0
          endif
          idxs = where(pos ne 0)
          if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
          maxPos = (pos eq 1) or (maxPos eq 1)
          
          valPos = (pos eq 1) and (minPos eq 1)
          idxs = where(tmpArr * valPos lt minArr * valPos)
          if (idxs[0] ne -1) then begin
            minArr[idxs] = tmpArr[idxs]
            pos[idxs] = 0
          endif
          idxs = where(pos ne 0)
          if (idxs[0] ne -1) then minArr[idxs] = tmpArr[idxs]
          minPos = (pos eq 1) or (minPos eq 1)
          
  ;        maxCnt = maxCnt + maxPos
  ;        minCnt = minCnt + minPos
        endfor
        ETR = MaxArr - minArr
        idxs = where(maxPos eq 0)
        if (idxs[0] ne -1) then ETR[idxs] = sResponse.missingVal
        totETR[*, *, segIdx]= ETR
      endif
    endfor

    if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = totETR[*, *, 0]
        pos = sum ne sResponse.missingVal
        sum = sum * pos
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totETR[*, *, segIdx]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          sum = sum + tmpArr * pos      
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        sum = totETR[*, 0]
  ;      cnt = totCnt[*, 0]
        pos = sum ne sResponse.missingVal
        sum = sum * pos
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totETR[*, segIdx]
          pos = tmpArra ne sResponse.missingVal
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
          sz = size(totSum[*, *, 0])
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg)
          for segIdx = 0, numTimeSeg - 1 do begin
            sum = totSum[*, *, segIdx] / totCnt[*, *, segIdx]
            idxs = where(totCnt[*, *, segIdx] eq 0)
            if (idxs[0] ne -1) then sum[idxs] = sResponse.missingVal
            aDataArray[*, *, segIdx] = sum
          endfor
        endif else begin ; for station data
          sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
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
        self->printLog, '(cvcCalcETR) Error! Unknown calculation mode: ', mode
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
        self->printLog, '(cvcCalcETR) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res
    
                       
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcETR::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcETR__define
   struct = { cvcCalcETR, $
               INHERITS cvcCalc $
             }
end
