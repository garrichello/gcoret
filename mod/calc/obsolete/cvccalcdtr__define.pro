;
;  NAME: 
;    cvcCalcDTR
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
FUNCTION cvcCalcDTR::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcDTR::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcDTR::Run
    print, "(cvcCalcTimeMean::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcTimeMean::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0

    resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
    if (self->Assert(resultCode)) then return, resultCode
 
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

    if (sResponse.gridType ne 'station') then begin ; for non-station data
      numDays = long(max(sResponse[segIdx].aTimes))-long(min(sResponse[segIdx].aTimes))
      curDay = min(sResponse[segIdx].aTimes)
      curArr = sResponse.aData[*, *, 0]
      cnt = curArr ne sResponse.missingVal
      minArr = curArr * cnt
      maxArr = curArr * cnt
      dtrArr = fltarr(size(sResponse.aData[*,*,0], /dim))
      for i = 1, n_elements(sResponse.aTimes)-1 do begin
        print, i
        if (long(sResponse.aTimes[i] - 0.5) eq long(curDay)) then begin
          tmpArr = sResponse.aData[*, *, i]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          tmpArr = tmpArr * pos
          idxMin = where(tmpArr le minArr)
          if (idxMin[0] ne -1) then minArr[idxMin] = tmpArr[idxMin]
          idxMax = where(tmpArr ge maxArr)
          if (idxMax[0] ne -1) then maxArr[idxMax] = tmpArr[idxMax]
        endif else begin
          dtrArr = dtrArr + (maxArr - minArr)
          curDay = curDay + 1
          tmpArr = sResponse.aData[*, *, i]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          tmpArr = tmpArr * pos
          idxMin = where(tmpArr le minArr)
          if (idxMin[0] ne -1) then minArr[idxMin] = tmpArr[idxMin]
          idxMax = where(tmpArr ge maxArr)
          if (idxMax[0] ne -1) then maxArr[idxMax] = tmpArr[idxMax]
        endelse
      endfor
      dtrArr = dtrArr + (maxArr - minArr)
    endif else begin ; for station data
      numDays = max(sResponse[segIdx].aTimes)-min(sResponse[segIdx].aTimes)+1
      curDay = min(sResponse[segIdx].aTimes)
      curArr = sResponse.aData[*, 0]
      cnt = curArr ne sResponse.missingVal
      minArr = curArr * cnt
      maxArr = curArr * cnt
      dtrArr = fltarr(size(sResponse.aData[*, 0], /dim))
      for i = 1, n_elements(sResponse.aTimes[0, *])-1 do begin
        if (long(sResponse.aTimes[0,i] - 0.5) eq long(curDay)) then begin
          tmpArr = sResponse.aData[*, i]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          tmpArr = tmpArr * pos
          idxMin = where(tmpArr le minArr)
          if (idxMin[0] ne -1) then minArr[idxMin] = tmpArr[idxMin]
          idxMax = where(tmpArr ge maxArr)
          if (idxMax[0] ne -1) then maxArr[idxMax] = tmpArr[idxMax]
        endif else begin
          dtrArr = dtrArr + (maxArr - minArr)
          curDay = curDay + 1
          tmpArr = sResponse.aData[*, i]
          pos = tmpArr ne sResponse.missingVal
          cnt = cnt + pos
          tmpArr = tmpArr * pos
          idxMin = where(tmpArr le minArr)
          if (idxMin[0] ne -1) then minArr[idxMin] = tmpArr[idxMin]
          idxMax = where(tmpArr ge maxArr)
          if (idxMax[0] ne -1) then maxArr[idxMax] = tmpArr[idxMax]
        endelse
      endfor
      dtrArr = dtrArr + (maxArr - minArr)
    endelse
    aDataArray = dtrArr / numDays
    idxs = where(cnt eq 0)
    if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
    
    case sResponse.gridType of
    'irregular': begin
        aLons = sResponse.aLons[*, *, 0]
        aLats = sResponse.aLats[*, *, 0]
      end
    else: begin
        aLons = sResponse.aLons
        aLats = sResponse.aLats
      end      
    endcase

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
    if (self->Assert(res)) then return, res
                       
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
;FUNCTION cvcCalcDTR::GetResult
;   return, self.sResult
;END
;--------------------------------------------------------------------
    


pro cvcCalcDTR__define
   struct = { cvcCalcDTR, $
               INHERITS cvcCalc $
             }
end