;
;  NAME: 
;    cvcCalcPercentileTN10
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of cold nights values for time series of data
;    
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcPercentileTX10 = obj_new('cvcCalcPercentileTX10', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcPercentileTX10->Run()
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
FUNCTION cvcCalcPercentileTX10degrees::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcPercentileTX10degrees::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
PRO cvcCalcPercentileTX10degrees::BP10pVals, aBPData, aBPDate, curIPDay, valBPmissing, BP10pArray
    pVal = ((size(aBPData))[4] * 5)/10 + 1
    tmpBPArray = replicate(1e20, (size(aBPData))[1], (size(aBPData))[2], pVal)
    caldat, curIPDay, mn_IP, dy_IP, yr_IP, hr_IP
    for segIdx = 0, (size(aBPDate))[2] - 1 do begin 
      winWidth = 2
      maxBPArr = aBPData[*, *, 0, segIdx]
      maxBPcnt = long(maxBPArr ne valBPmissing)
      for i = 1, (size(aBPDate))[1]-1 do begin
        caldat, aBPDate[i, segIdx] + winWidth, mn_BP, dy_BP, yr_BP, hr_BP
        if (mn_BP eq mn_IP) and (dy_IP eq dy_BP) then begin
          tmpBPArr = aBPData[*, *, i, segIdx]
          curBPpos = long(tmpBPArr ne valBPmissing)
          BPidxs = where(tmpBPArr * curBPpos * maxBPcnt gt maxBPArr * curBPpos * maxBPcnt)
          if (BPidxs[0] ne -1) then maxBPArr[BPidxs] = tmpBPArr[BPidxs]
          BPidxs = where(curBPpos * (1 - maxBPcnt) eq 1)
          if (BPidxs[0] ne -1) then maxBPArr[BPidxs] = tmpBPArr[BPidxs]
        endif else begin 
          tmpBPArray[*,*,pVal-1] = maxBPArr
          for val = 1, pVal-1 do begin
            aArray = tmpBPArray[*,*,pVal-val]
            aPos = long(aArray ne valBPmissing)
            bArray = tmpBPArray[*,*,pVal-1-val]
            curArray = fltarr(size(bArray, /dim))
            maxIdx = where(aArray * aPos lt bArray * aPos)
            if (maxIdx[0] ne -1) then begin
                curArray[maxIdx] = bArray[maxIdx]
                bArray[maxIdx] = aArray[maxIdx]
                aArray[maxIdx] = curArray[maxIdx]
                tmpBPArray[*, *, pVal - val] = aArray
                tmpBPArray[*, *, pVal - 1 - val] = bArray
            endif
          endfor
          maxBPArr = aBPData[*, *, i, segIdx] 
          maxBPcnt = long(maxBPArr ne valBPmissing)
          winWidth = winWidth - 1
        endelse
        if (winWidth eq -3) then break
      endfor
    endfor
    pos2 = long(tmpBPArray[*,*,pVal-2] ne 1e20)
    pos1 = long(tmpBPArray[*,*,pVal-1] ne 1e20)
    BP10pArray = (0.9 * (tmpBPArray[*,*,pVal-2]) + 0.1 * (tmpBPArray[*,*,pVal-1])) * pos1 * pos2
    idxBP = where(pos1 * pos2 eq 0)
    if (idxBP[0] ne -1) then BP10pArray[idxBP] = valBPmissing
;    BP10pArray = tmpBPArray[*,*,pVal-2]

END
;--------------------------------------------------------------------
PRO cvcCalcPercentileTX10degrees::BPtoIPgrid, BP10pArray, minIPArr, aBPlats, aBPlons, aIPlats, aIPlons, valBPmissing, aBPreGrid, aIPreGrid, lonArr, latArr 
   if ((abs(aIPlats[1] - aIPlats[0]) ge abs(aBPlats[1] - aBPlats[0])) and (abs(aIPlons[1] - aIPlons[0]) ge abs(aBPlons[1] - aBPlons[0]))) then begin
     RN1 = minIPArr
     RN2 = BP10pArray
     xRN1 = aIPlons
     yRN1 = aIPlats
     xRN2 = aBPlons
     yRN2 = aBPlats
     missRN1 = valBPmissing
     missRN2 = valBPmissing
   endif else begin
     RN1 = BP10pArray
     RN2 = minIPArr
     xRN1 = aBPlons
     yRN1 = aBPLats
     xRN2 = aIPlons
     yRN2 = aIPLats
     missRN1 = valBPmissing
     missRN2 = valBPmissing
   endelse
   pos = RN2 ne missRN2
   xUnitVector = replicate(1.0, (size(RN2))[1])
   yUnitVector = replicate(1.0, (size(RN2))[2])
   xRq = xRN2 # yUnitVector
   yRq = xUnitVector # yRN2
   validx = where(RN2 ne missRN2)
   triangulate, xRq[validx], yRq[validx], tr
   intRNArray = griddata(xRq[validx], yRq[validx], RN2[validx], /linear, triangles = tr, xout = [xRN1], yout = [yRN1], /grid)
   triangulate, xRq, yRq, trPos
   intPosArray = griddata(xRq, yRq, pos, /linear, triangles = trPos, xout = [xRN1], yout = [yRN1], /grid)
   idxs = where(intPosArray lt 1)
   if (idxs[0] ne -1) then intPosArray[idxs] = 0
   tmpPos = replicate(1, size(RN1[*,*], /dim))
   RN2 = intRNArray * intPosArray + (tmpPos - intPosArray) * missRN1
   idxs = where(RN1 eq BP10pArray)  
   if (idxs[0] ne -1) then begin
     aIPreGrid = RN2
     aBPreGrid = RN1
   endif else begin
     aIPreGrid = RN1
     aBPreGrid = RN2
   endelse 
   lonArr = xRN1
   latArr = yRN1   
END
;--------------------------------------------------------------------
FUNCTION cvcCalcPercentileTX10degrees::Run
     print, "(cvcCalcPercentileTX10::Run) Started..."
    
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
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='Mode', sParam)
      calcMode = *(sParam.data)
    endif else calcMode = 'single'
    
        
    ; get info about number of levels and time segments
    retCode_IP = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_IP, OUT_TIMESEGS=asTimeSeg_IP, OUT_NLEVELS=numLevels_IP, OUT_LEVELS=aLevel_IP, OUT_ROI=sROI_IP)
    if (self->Assert(retCode_IP)) then return, retCode_IP
    retCode_BP = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_BP, OUT_TIMESEGS=asTimeSeg_BP, OUT_NLEVELS=numLevels_BP, OUT_LEVELS=aLevel_BP, OUT_ROI=sROI_BP)
    if (self->Assert(retCode_BP)) then return, retCode_BP

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcPercentileTX10::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
   
 
    for segIdx = 0, numTimeSeg_BP - 1 do begin 
      resultCode_BP = (aoInputs[1])->Get(Time=asTimeSeg_BP[segIdx], Level=aLevel_BP[0], Region=sROI_BP, sResponse_BP)
      print, segIdx, size(sResponse_BP.aData) 
      if (self->Assert(resultCode_BP)) then return, resultCode_BP
      if (segIdx eq 0) then begin
        dims = size(sResponse_BP.aData, /dim)
        aBPData = fltarr([dims, numTimeSeg_BP])
        aBPDate = fltarr([dims[2], numTimeSeg_BP])
      endif
      aBPData[*, *, *, segIdx] = sResponse_BP.aData
      aBPDate[*, segIdx] = sResponse_BP.aTimes
    endfor
    valBPmissing = sResponse_BP.missingVal
    aBPlats = sResponse_BP.aLats
    aBPlons = sResponse_BP.aLons
    
    ; Calculation of 10th percentile temperature value for the Basic Period
    for segIdx = 0, numTimeSeg_IP - 1 do begin
      resultCode_IP = (aoInputs[0])->Get(Time=asTimeSeg_IP[segIdx], Level=aLevel_IP[levelIdx], Region=sROI_IP, sResponse_IP)
      if (self->Assert(resultCode_IP)) then return, resultCode_IP
      aIPlats = sResponse_IP.aLats
      aIPlons = sResponse_IP.aLons
    
      if (sResponse_IP.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin
          self->BPtoIPgrid, aBPData[*,*,0,0], sResponse_IP.aData[*,*,0], aBPlats, aBPlons, aIPlats, aIPlons, valBPmissing, aBPData1, aIPData1, lonArr, latArr
          totTX10p = fltarr(n_elements(lonArr), n_elements(latArr), numTimeSeg_IP)
          totCnt = fltarr(n_elements(lonArr), n_elements(latArr), numTimeSeg_IP)
        endif
        curTX10p = fltarr(n_elements(lonArr), n_elements(latArr))
        curTX10inc = fltarr(n_elements(lonArr), n_elements(latArr))
        self->DayMonthYear, asTimeSeg_IP[segIdx].beginning, yr, mn, dy, hr
        curIPDay = julday(mn, dy, yr, hr)
        maxArr = sResponse_IP.aData[*, *, 0]
        maxCnt = long(maxArr ne sResponse_IP.missingVal)
        for i = 1, n_elements(sResponse_IP.aTimes)-1 do begin
          if (long(sResponse_IP.aTimes[i] - 0.5) eq long(curIPDay)) then begin
            tmpArr = sResponse_IP.aData[*, *, i]
            curPos = long(tmpArr ne sResponse_IP.missingVal)
            idxs = where(tmpArr * curPos * maxCnt gt maxArr * curPos * maxCnt)
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
            idxs = where(curPos * (1 - maxCnt) eq 1)
            if (idxs[0] ne -1) then maxArr[idxs] = tmpArr[idxs]
          endif else begin
            maxIPArr = maxArr
            self->BP10pVals, aBPData, aBPDate, curIPDay, valBPmissing, BP10pArray
            self->BPtoIPgrid, BP10pArray, maxIPArr, aBPlats, aBPlons, aIPlats, aIPlons, valBPmissing, aBPreGrid, aIPreGrid, lonArr, latArr 
            maxIPCnt = long(aIPreGrid ne sResponse_IP.missingVal)
            valIdxs = where(aIPreGrid * maxIPCnt lt aBPreGrid * maxIPCnt)
            if (valIdxs[0] ne -1) then curTX10p[valIdxs] = curTX10p[valIdxs] + 1
            if (valIdxs[0] ne -1) then curTX10inc[valIdxs] = curTX10inc[valIDXs] + abs(aIPreGrid[valIdxs] - aBPreGrid[valIdxs])
            maxArr = sResponse_IP.aData[*, *, i]
            maxCnt = long(maxArr ne sResponse_IP.missingVal)
            curIPDay = curIPDay + 1
          endelse
        endfor
        maxIPArr = maxArr
        self->BP10pVals, aBPData, aBPDate, curIPDay, valBPmissing, BP10pArray
        self->BPtoIPgrid, BP10pArray, maxIPArr, aBPlats, aBPlons, aIPlats, aIPlons, valBPmissing, aBPreGrid, aIPreGrid, lonArr, latArr 
        maxIPCnt = long(aIPreGrid ne sResponse_IP.missingVal)
        valIdxs = where(aIPreGrid * maxIPCnt lt aBPreGrid * maxIPCnt)
        if (valIdxs[0] ne -1) then curTX10p[valIdxs] = curTX10p[valIdxs] + 1
        if (valIdxs[0] ne -1) then curTX10inc[valIdxs] = curTX10inc[valIDXs] + abs(aIPreGrid[valIdxs] - aBPreGrid[valIdxs])
      endif else begin ; for station data
        
      endelse
      curTX10inc = curTX10inc/curTX10p
      idxs = where(curTX10p eq 0)
      if (idxs[0] ne -1) then curTX10inc[idxs] = 0
      totTX10p[*, *, segIdx] = curTX10inc
;      totTX10p[*, *, segIdx] = curTX10p
    endfor
    
    if (calcMode eq 'single') then begin
      if (sResponse_IP.gridType ne 'station') then begin ; for non-station data
        sum = totTX10p[*, *, 0]
        cnt = long(sum ne 0)
        for segIdx = 1, numTimeSeg_IP - 1 do begin
          tmpArr = totTX10p[*, *, segIdx]
          pos = long(totTX10p[*, *, segIdx] ne 0)
          sum = sum + tmpArr  
          cnt = cnt + pos    
        endfor
        aDataArray = sum / cnt
        idxs = where(cnt eq 0)
        if (idxs[0] ne -1) then aDataArray[idxs] = 0 
      endif else begin ; for station data
         
      endelse
    endif else begin
      if (calcMode eq 'multi') then begin
        if (sResponse_IP.gridType ne 'station') then begin ; for non-station data
          aDataArray = totTX10p
        endif else begin ; for station data

        endelse
      endif else begin
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    
;    case sResponse.gridType of
    aLons = lonArr
    aLats = latArr
    midSeg = fltarr(numTimeSeg_IP)
    for segIdx = 0, numTimeSeg_IP - 1 do begin
      self->DayMonthYear, asTimeSeg_IP[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
      self->DayMonthYear, asTimeSeg_IP[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
      midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    if (calcMode eq 'single') then begin
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse_IP.missingVal, GRIDTYPE=sResponse_IP.gridType)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse_IP.missingVal, GRIDTYPE=sResponse_IP.gridType)
      endif else begin
        print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res


    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcPercentileTX10degrees::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcPercentileTX10degrees__define
   struct = { cvcCalcPercentileTX10degrees, $
               INHERITS cvcCalc $
             }
end