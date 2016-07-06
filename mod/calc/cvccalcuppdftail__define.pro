;
;  NAME: 
;    cvcCalcUpPDFtail
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of cold nights/days values for time series of data
;    
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcupPDFtail = obj_new('cvcCalcupPDFtail', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcupPDFtail->Run()
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
FUNCTION cvcCalcUpPDFtail::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcupPDFtail::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcUpPDFtail::Run
    
    print, "(cvcCalcUpPDFtail::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")   
    
    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='percentileThreshold', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
    endif else calcMode = 5
    
    ; get info about number of levels and time segments
    retCode_BP = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_BP, OUT_TIMESEGS=asTimeSeg_BP, OUT_NLEVELS=numLevels_BP, OUT_LEVELS=aLevel_BP, OUT_ROI=sROI_BP)
    if (self->Assert(retCode_BP)) then return, retCode_BP

    ; level to read
    levelIdx = 0
    
    ; main loop for all time segments
    self->printLog, "(cvcCalcUpPDFTail::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    ; time segment to read
    segIdx = 0
    
    self->DayMonthYear, asTimeSeg_BP[segIdx].beginning, yr, mn, dy, hr
    begBPday = julday(mn, dy, yr, hr)
    self->DayMonthYear, asTimeSeg_BP[segIdx].ending, yr, mn, dy, hr
    endBPday = julday(mn, dy, yr, hr)
    numBPdays = long(endBPday) - long(begBPday) - 4
    pVals = (numTimeSeg_BP * 5 * (100 - calcMode))/100 + 2  ; position of percentile in the sample sorted by ascending
    
    for segIdx = 0, numTimeSeg_BP - 1 do begin
      resultCode_BP = (aoInputs[0])->Get(Time=asTimeSeg_BP[segIdx], Level=aLevel_BP[0], Region=sROI_BP, sResponse_BP)
      if (self->Assert(resultCode_BP)) then return, resultCode_BP
      if (sResponse_BP.gridType ne 'station') then begin ; for non-station data
        if (segIdx eq 0) then begin        
          dims = size(sResponse_BP.aData)
          aBPpercentile = fltarr(dims[1], dims[2], numBPdays)
          tmpArray = replicate(-1e20, dims[1], dims[2], numBPdays, pVals)
        endif
        curIPDay = 0
        self->DayMonthYear, asTimeSeg_BP[segIdx].beginning, yr, mn, dy, hr
        winEnd = julday(mn, dy, yr, 18) + 4
        for curIPday = 0, numBPdays - 1 do begin
          for i = curIPday, curIPday + 4 do begin
            if (long(sResponse_BP.aTimes[i] - 0.5) lt long(winEnd)) then begin
              tmpArray[*, *, curIPday, 0] = sResponse_BP.aData[*, *, i]
              for prcIdx = 1, pVals - 1 do begin
                aArray = tmpArray[*, *, curIPday, prcIdx - 1]
                aPos = long(aArray ne sResponse_BP.missingVal)
                bArray = tmpArray[*, *, curIPday, prcIdx]
                bPos = long(bArray ne sResponse_BP.missingVal)
                curArray = fltarr(size(aArray, /dim))
                valPos = long(aArray * aPos * bPos gt bArray * aPos * bPos)
                addPos = long(aPos - bPos eq 1)
                idxs = where(valPos + addPos eq 1)
                if (idxs[0] ne -1) then begin
                  curArray[idxs] = bArray[idxs]
                  bArray[idxs] = aArray[idxs]
                  aArray[idxs] = curArray[idxs]
                  aMissPos = long(aPos eq 0)
                  amIdxs = where(aMissPos + addPos eq 1)
                  if (amIdxs[0] ne -1) then aArray[amIdxs] = sResponse_BP.missingVal
                  bMissPos = long(bPos eq 0)
                  bmIdxs = where(bMissPos - addPos eq 1)
                  if (bmIdxs[0] ne -1) then bArray[bmIdxs] = sResponse_BP.missingVal
                  tmpArray[*, *, curIPday, prcIdx - 1] = aArray
                  tmpArray[*, *, curIPday, prcIdx] = bArray
                endif else begin
                  prcIdx = pVals + 1
                endelse
              endfor         
            endif else begin
              i = curIPday + 5
            endelse
          endfor
          winEnd = winEnd + 1
        endfor
      endif else begin ; station data process
	sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        if (segIdx eq 0) then begin        
          dims = size(sResponse_BP.aData)
          aBPpercentile = fltarr(dims[1], numBPdays)
          tmpArray = replicate(-1e20, dims[1], numBPdays, pVals)
        endif
        curIPDay = 0
        self->DayMonthYear, asTimeSeg_BP[segIdx].beginning, yr, mn, dy, hr
        winEnd = julday(mn, dy, yr, 18) + 4
        for curIPday = 0, numBPdays - 1 do begin
          for i = curIPday, curIPday + 4 do begin
            if (long(sResponse_BP.aTimes[i] - 0.5) lt long(winEnd)) then begin
              tmpArray[*, curIPday, 0] = sResponse_BP.aData[*, i]
              for prcIdx = 1, pVals - 1 do begin
                aArray = tmpArray[*, curIPday, prcIdx - 1]
                aPos = long(aArray ne sResponse_BP.missingVal)
                bArray = tmpArray[*, curIPday, prcIdx]
                bPos = long(bArray ne sResponse_BP.missingVal)
                curArray = fltarr(size(aArray, /dim))
                valPos = long(aArray * aPos * bPos gt bArray * aPos * bPos)
                addPos = long(aPos - bPos eq 1)
                idxs = where(valPos + addPos eq 1)
                if (idxs[0] ne -1) then begin
                  curArray[idxs] = bArray[idxs]
                  bArray[idxs] = aArray[idxs]
                  aArray[idxs] = curArray[idxs]
                  aMissPos = long(aPos eq 0)
                  amIdxs = where(aMissPos + addPos eq 1)
                  if (amIdxs[0] ne -1) then aArray[amIdxs] = sResponse_BP.missingVal
                  bMissPos = long(bPos eq 0)
                  bmIdxs = where(bMissPos - addPos eq 1)
                  if (bmIdxs[0] ne -1) then bArray[bmIdxs] = sResponse_BP.missingVal
                  tmpArray[*, curIPday, prcIdx - 1] = aArray
                  tmpArray[*, curIPday, prcIdx] = bArray
                endif else begin
                  prcIdx = pVals + 1
                endelse
              endfor         
            endif else begin
              i = curIPday + 5
            endelse
          endfor
          winEnd = winEnd + 1
        endfor
      endelse
    endfor
    
    curArray[*] = 0
    if (sResponse_BP.gridType ne 'station') then begin ; for non-station data
      for dayIdx = 0, numBPDays - 1 do begin
        pos2 = long(tmpArray[*, *, dayIdx, 1] ne sResponse_BP.missingVal)
        pos1 = long(tmpArray[*, *, dayIdx, 0] ne sResponse_BP.missingVal)
        pos4 = long(tmpArray[*, *, dayIdx, 1] ne -1e20)
        pos3 = long(tmpArray[*, *, dayIdx, 0] ne -1e20)
        curArray = ((100 - calcMode) * tmpArray[*, *, dayIdx, 2] + calcMode * tmpArray[*, *, dayIdx, 1]) * pos1 * pos2 * pos3 * pos4 / 100
        idxs = where(pos1 * pos2 * pos3 * pos4 eq 0)
        if (idxs[0] ne -1) then curArray[idxs] = sResponse_BP.missingVal
        aBPpercentile[*, *, dayIdx] = curArray
      endfor
    endif else begin
      for dayIdx = 0, numBPDays - 1 do begin
        pos2 = long(tmpArray[*, dayIdx, 1] ne sResponse_BP.missingVal)
        pos1 = long(tmpArray[*, dayIdx, 0] ne sResponse_BP.missingVal)
        pos4 = long(tmpArray[*, dayIdx, 1] ne -1e20)
        pos3 = long(tmpArray[*, dayIdx, 0] ne -1e20)
        curArray = ((100 - calcMode) * tmpArray[*, dayIdx, 2] + calcMode * tmpArray[*, dayIdx, 1]) * pos1 * pos2 * pos3 * pos4 / 100
        idxs = where(pos1 * pos2 * pos3 * pos4 eq 0)
        if (idxs[0] ne -1) then curArray[idxs] = sResponse_BP.missingVal
        aBPpercentile[*, dayIdx] = curArray
      endfor
    endelse
    aDataArray = aBPpercentile

;    case sResponse.gridType of
    aLons = sResponse_BP.aLons
    aLats = sResponse_BP.aLats
    midDaySeg = fltarr(numBPDays)
    for dayIdx = 0, numBPDays - 1 do begin
      self->DayMonthYear, asTimeSeg_BP[0].beginning, reqYear, reqMonth, reqDay, reqHour
      begTimeJD = julday(reqMonth, reqDay, reqYear, reqHour)
      midDaySeg[dayIdx] = begTimeJD + dayIdx + 2.5
    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midDaySeg, MISSING=sResponse_BP.missingVal, GRIDTYPE=sResponse_BP.gridType, EXTRA=sExtra)

    return, self.ERROR_OK
END

;--------------------------------------------------------------------
FUNCTION cvcCalcUpPDFTail::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------

PRO cvcCalcUpPDFTail__define

    struct = { cvcCalcUpPDFTail, $
               INHERITS cvcCalc $ 
             }
END