;
;  NAME: 
;    cvcCalcCorelTM
;
;  DESRIPTION:
;    Linear correlation of two time series
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of trend for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcCorelTM = obj_new('cvcCalcCorelTM', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcCorelTM->Run()
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
FUNCTION cvcCalcCorelTM::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcCorelTM::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcCorelTM::Run
    
    self->printLog, "(cvcCalcCorelTM::Run) Started..."
    
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
    self->printLog, "(cvcCalcCorelTM::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    
    ; Time mean value for all segments of each dataset
    for dataIdx = 0, nData - 1 do begin
      for segIdx = 0, numTimeSeg - 1 do begin 
        resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Region=sROI_IP, sResponse)
        if (self->Assert(resultCode)) then return, resultCode
        self->DayMonthYear, asTimeSeg[segIdx].beginning, Yr, Mn, Dy, Hr
        if (sResponse.gridType ne 'station') then begin ; for non-station data
          if ((dataIdx eq 0) and (segIdx eq 0)) then begin
            sz = size(sResponse.aData)
            totData = fltarr(sz[1], sz[2], numTimeSeg, nData)
            totMean = fltarr(sz[1], sz[2], nData)
            totCnt = fltarr(sz[1], sz[2], nData)
            aDataArray = fltArr(sz[1], sz[2])
          endif
          tmpArr = sResponse.aData
          pos= long(tmpArr ne sResponse.missingVal)
          totData[*, *, segIdx, dataIdx] = tmpArr * pos
          totMean[*, *, dataIdx] = totMean[*, *, dataIdx] + tmpArr * pos
          totCnt[*, *, dataIdx] = totCnt[*, *, dataIdx] + pos
        endif else begin ; for station data
          sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
          if ((dataIdx eq 0) and (segIdx eq 0)) then begin
            sz = size(sResponse.aData)
            totData = fltarr(sz[1], numTimeSeg, nData)
            totMean = fltarr(sz[1], nData)
            totCnt = fltarr(sz[1], nData)
            aDataArray = fltArr(sz[1])
          endif
          tmpArr = sResponse.aData
          pos= long(tmpArr ne sResponse.missingVal)
          totData[*, segIdx, dataIdx] = tmpArr * pos
          totMean[*, dataIdx] = totMean[*, dataIdx] + tmpArr * pos
          totCnt[*, dataIdx] = totCnt[*, dataIdx] + pos
        endelse
      endfor
      curCnt = totCnt[*,*, dataIdx]
      curMean = totMean[*,*, dataIdx]
      idxs = where(curCnt ne 0, complement = misIdxs)
      if (idxs[0] ne -1) then curMean[idxs] = curMean[idxs] / curCnt[idxs]
      if (misIdxs[0] ne -1) then curMean[misIdxs] = sResponse.missingVal
      totMean[*, *, dataIdx] = curMean
    endfor
    
    ; Calculation of correlation coefficient components
    if (sResponse.gridType ne 'station') then begin ; for non-station data
      for segIdx = 0, numTimeSeg - 1 do begin
        if segIdx eq 0 then begin
          numArr = (totData[*,*, segIdx, 0] - totMean[*,*, 0]) * (totData[*,*, segIdx, 1] - totMean[*,*, 1])
          denArr_0 = ((totData[*,*, segIdx, 0] - totMean[*,*, 0])^2)
          denArr_1 = ((totData[*,*, segIdx, 1] - totMean[*,*, 1])^2)
        endif else begin
          numArr = numArr + (totData[*,*, segIdx, 0] - totMean[*,*, 0]) * (totData[*,*, segIdx, 1] - totMean[*,*, 1])
          denArr_0 = denArr_0 + ((totData[*,*, segIdx, 0] - totMean[*,*, 0])^2)
          denArr_1 = denArr_1 + ((totData[*,*, segIdx, 1] - totMean[*,*, 1])^2)
        endelse 
      endfor
      valPos0 = totCnt[*,*, 0] ne 0
      valPos1 = totCnt[*,*, 1] ne 0
    endif else begin
      for segIdx = 0, numTimeSeg - 1 do begin
        if segIdx eq 0 then begin
          numArr = (totData[*, segIdx, 0] - totMean[*, 0]) * (totData[*, segIdx, 1] - totMean[*, 1])
          denArr_0 = ((totData[*, segIdx, 0] - totMean[*, 0])^2)
          denArr_1 = ((totData[*, segIdx, 1] - totMean[*, 1])^2)
        endif else begin
          numArr = numArr + (totData[*, segIdx, 0] - totMean[*, 0]) * (totData[*, segIdx, 1] - totMean[*, 1])
          denArr_0 = denArr_0 + ((totData[*, segIdx, 0] - totMean[*, 0])^2)
          denArr_1 = denArr_1 + ((totData[*, segIdx, 1] - totMean[*, 1])^2)
        endelse 
      endfor
      valPos0 = totCnt[*, 0] ne 0
      valPos1 = totCnt[*, 1] ne 0
    endelse
    ; Correlation coefficient
    idxs = where(valPos0 * valPos1 ne 0, complement = misIdx)
    if (idxs[0] ne -1) then aDataArray[idxs] = (numArr[idxs]/sqrt((denArr_0[idxs])*(denArr_1[idxs])))
    if (misIdx[0] ne -1) then aDataArray[misIdx] = sResponse.missingVal
    
    aLons = sResponse.aLons
    aLats = sResponse.aLats
 
     ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0

    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)   
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcCorelTM__define

    struct = { cvcCalcCorelTM, $
               INHERITS cvcCalc $ 
             }
END