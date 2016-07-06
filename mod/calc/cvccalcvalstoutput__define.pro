;
;  NAME: 
;    cvcCalcValSToutput
;
;  DESCRIPTION:
;    Mask of homogeneous time series
;
;  PURPOSE:
;    Sample class. 
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcValSTOutput = obj_new('cvcCalcValSTOutput', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcValSTOutput->Run()
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
FUNCTION cvcCalcValSTOutput::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcValSTOutput::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcValSTOutput::Run
    
    self->printLog, "(cvcCalcValSTOutput::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_DT, OUT_TIMESEGS=asTimeSeg_DT, OUT_NLEVELS=numLevels_DT, OUT_LEVELS=aLevel_DT, OUT_ROI=sROI_DT)
    if (self->Assert(retCode)) then return, retCode
    retCode = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_ST, OUT_TIMESEGS=asTimeSeg_ST, OUT_NLEVELS=numLevels_ST, OUT_LEVELS=aLevel_ST, OUT_ROI=sROI_ST)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
    
    ; main loop for all time segments
    self->printLog, "(cvcCalcValSTOutput::Run) Get data..."

    resultCode = (aoInputs[1])->Get(Time=asTimeSeg_ST[0], Region=sROI_ST, sResponse_ST)
    if (self->Assert(resultCode)) then return, resultCode
    
    for segIdx = 0, numTimeSeg_DT - 1 do begin
       resultCode = (aoInputs[0])->Get(Time=asTimeSeg_DT[segIdx], Region=sROI_DT, sResponse_DT)
       if (self->Assert(resultCode)) then return, resultCode 
       if (segIdx eq 0) then begin
         sz = size(sResponse_DT.adata)
         if (sz[0] eq 1) then begin
           aDataArray = fltarr(sz[1], numTimeSeg_DT)
         endif else begin
           if (sz[0] eq 2) then begin
             aDataArray = fltarr(sz[1], sz[2], numTimeSeg_DT)
           endif
         endelse
       endif      
       valSTOuts = sResponse_ST.aData   
       idxs = where(valSTOuts eq 0.0)
       if (sz[0] eq 1) then begin
           tmpArray = sResponse_DT.aData 
           if (idxs[0] ne -1) then tmpArray[idxs] = sResponse_DT.missingVal
           aDataArray[*, segIdx] = tmpArray
         endif else begin
           if (sz[0] eq 2) then begin
             for i = 0, sz[2] - 1 do begin
               tmpArray = sResponse_DT.aData[*, i]
               if (idxs[0] ne -1) then tmpArray[idxs] = sResponse_DT.missingVal
               aDataArray[*, i, segIdx] = tmpArray
             endfor
           endif
         endelse
    endfor
    
    aLons = sResponse_DT.aLons
    aLats = sResponse_DT.aLats
    midSeg = fltarr(numTimeSeg_DT)
    for segIdx = 0, numTimeSeg_DT - 1 do begin
       self->DayMonthYear, asTimeSeg_DT[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
       reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
       self->DayMonthYear, asTimeSeg_DT[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
       reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
       midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
     endfor
 
    midDaySeg = fltarr(n_elements(sResponse_DT.aTimes))
    for dayIdx = 0, n_elements(sResponse_DT.aTimes) - 1 do begin
      self->DayMonthYear, asTimeSeg_DT[0].beginning, reqYear, reqMonth, reqDay, reqHour
      begTimeJD = julday(reqMonth, reqDay, reqYear, reqHour)
      midDaySeg[dayIdx] = begTimeJD + dayIdx + 2.5
    endfor

    ; stations names
    sExtra = { aStNames : sResponse_DT.aNames, aStCodes : sResponse_DT.aCodes } 

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0

    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse_DT.missingVal, GRIDTYPE=sResponse_DT.gridType, EXTRA=sExtra)
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcValSTOutput__define

    struct = { cvcCalcValSTOutput, $
               INHERITS cvcCalc $ 
             }
END