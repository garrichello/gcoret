;
;  NAME: 
;    cvcCalcDifference
;
;  DESCRIPTION:
;    Absolute and normalized difference between two datasets with same and different spatial resolution 
;
;  PURPOSE:
;   
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcDifference = obj_new('cvcCalcDifference', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcDifference->Run()
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
FUNCTION cvcCalcDifference::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcDifference::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcDifference::Run
    
    self->printLog, "(cvcCalcDifference::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

;    ; get parameters
    res = self->GetParams(aoParams, nParams)
    
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='ariProc', sParam)
      if (self->Assert(res)) then return, res
      calcMode1 = *(sParam.data)
    endif
    
    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode1 = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_0, OUT_TIMESEGS=asTimeSeg_0, OUT_NLEVELS=numLevels_0, OUT_LEVELS=aLevel_0, OUT_ROI=sROI_0)
    if (self->Assert(retCode1)) then return, retCode1
    retCode2 = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_1, OUT_TIMESEGS=asTimeSeg_1, OUT_NLEVELS=numLevels_1, OUT_LEVELS=aLevel_1, OUT_ROI=sROI_1)
    if (self->Assert(retCode2)) then return, retCode2

    ; level to read
    levelIdx = 0
    
    ; main loop for all time segments
    self->printLog, "(cvcCalcDifference::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg_0 - 1 do begin
      resultCode = (aoInputs[0])->Get(Time=asTimeSeg_0[segIdx], sResponse1)
      if (self->Assert(resultCode)) then return, resultCode
      resultCode = (aoInputs[1])->Get(Time=asTimeSeg_1[0], sResponse2) ; Time=asTimeSeg_1[segIdx]
      if (self->Assert(resultCode)) then return, resultCode
      if ((sResponse1.gridType ne 'station') and (sResponse2.gridType ne 'station')) then begin ; for non-station data
        if ((abs(sResponse1.alats[1] - sResponse1.alats[0]) ge abs(sResponse2.alats[1] - sResponse2.alats[0])) $
           and  (abs(sResponse1.alons[1] - sResponse1.alons[0]) ge abs(sResponse2.alons[1] - sResponse2.alons[0]))) then begin
          RN1 = sResponse1.aData
          RN2 = sResponse2.aData
          xRN1 = sResponse1.alons
          yRN1 = sResponse1.aLats
          xRN2 = sResponse2.alons
          yRN2 = sResponse2.aLats
          missRN1 = sResponse1.missingVal
          missRN2 = sResponse2.missingVal
        endif else begin
          RN1 = sResponse2.aData
          RN2 = sResponse1.aData
          xRN1 = sResponse2.alons
          yRN1 = sResponse2.aLats
          xRN2 = sResponse1.alons
          yRN2 = sResponse1.aLats
          missRN1 = sResponse2.missingVal
          missRN2 = sResponse1.missingVal
        endelse
        if (segIdx eq 0) then begin
          sz = size(RN1)
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg_0)
        endif
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
        tmpPos = replicate(1, size(RN1[*,*,0], /dim))
        RN2 = intRNArray * intPosArray + (tmpPos - intPosArray) * missRN1 
        posRN1 = RN1 ne missRN1
        posRN2 = RN2 ne missRN1
        comPos = ((posRN1 eq 1) and (posRN2 eq 1)) 
        idxs = where(RN1 ne sResponse1.aData)  
        if (idxs[0] eq -1) then begin
          Data1 = RN1
          Data2 = RN2
        endif else begin
          Data1 = RN2
          Data2 = RN1
        endelse    
        case calcMode1 of
          'dif': tmpArray = Data1 - Data2
          'normd1': tmpArray = (Data1 - Data2) / Data1
          'normd2': tmpArray = (Data1 - Data2) / Data2
          'averadd': tmpArray = (Data1 + Data2) / 2
        else: self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode1
        endcase
        aDataArray[*, *, segIdx] = tmpArray * comPos + (tmpPos - comPos) * missRN1 
        idxs = where(comPos eq 0)
      endif else begin ; for station data
        sExtra = { aStNames : sResponse1.aNames, aStCodes : sResponse1.aCodes } 
        if (segIdx eq 0) then begin
          latIdx = where(sResponse1.alats eq sResponse2.alats)
          lonIdx = where(sResponse1.alons eq sResponse2.alons)
          timeIdx = where(sResponse1.aTimes eq sResponse2.aTimes)
          if ((latIdx[0] ne -1) and (lonIdx[0] ne -1)) then begin
            num_lats = n_elements(sResponse1.alats)
            num_lons = n_elements(sResponse1.alons)
            if ((timeIdx[0] ne -1) and (n_elements(sResponse1.aTimes) gt 1)) then begin
               aDataArray = fltarr(num_lons, n_elements(sResponse1.aTimes), numTimeSeg_0)
            endif else begin
               aDataArray = fltarr(num_lons, numTimeSeg_0)
            endelse
          endif else begin
            self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
            return, -1
          endelse
        endif
        if (n_elements(sResponse1.aTimes) gt 1) then begin 
          for i = 0,  n_elements(sResponse1.aTimes) - 1 do begin
            pos1 = sResponse1.aData[*, i] ne sResponse1.missingVal
            pos2 = sResponse2.aData[*, i] ne sResponse2.missingVal
            comPos = ((pos1 eq 1) and (pos2 eq 1))
            case calcMode1 of
              'dif': tmpArray = sResponse1.aData[*, i] * comPos - sResponse2.aData[*, i] * comPos ; tmpArray = sResponse1.aData * comPos - sResponse2.aData * comPos
              'normd1': tmpArray = (sResponse1.aData * comPos - sResponse2.aData * comPos) / (sResponse1.aData * comPos)
              'normd2': tmpArray = (sResponse1.aData * comPos - sResponse2.aData * comPos) / (sResponse2.aData * comPos)
              'averadd': tmpArray = (sResponse1.aData * comPos + sResponse2.aData * comPos) / 2
            else: self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode1
            endcase
            idxs = where(comPos eq 0)
            if (idxs[0] ne -1) then tmpArray[idxs] = sResponse1.missingVal
            aDataArray[*, i, segIdx] = tmpArray 
          endfor
        endif else begin
          pos1 = sResponse1.aData ne sResponse1.missingVal
          pos2 = sResponse2.aData ne sResponse2.missingVal
          comPos = ((pos1 eq 1) and (pos2 eq 1))
          case calcMode1 of
            'dif':  tmpArray = abs(sResponse1.aData * comPos) - abs(sResponse2.aData * comPos)
            'normd1': tmpArray = (sResponse1.aData * comPos - sResponse2.aData * comPos) / (sResponse1.aData * comPos)
            'normd2': tmpArray = (sResponse1.aData * comPos - sResponse2.aData * comPos) / (sResponse2.aData * comPos)
            'averadd': tmpArray = (sResponse1.aData * comPos + sResponse2.aData * comPos) / 2
          else: self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode1
          endcase
          idxs = where(comPos eq 0)
          if (idxs[0] ne -1) then tmpArray[idxs] = sResponse1.missingVal
          aDataArray[*, segIdx] = tmpArray
        endelse      
      endelse
    endfor
    
;; temparal case of study
;     pos1 = (aDataArray eq sResponse1.missingVal)
;     pos2 = (aDataArray gt 0.0)
;     aDataArray = aDataArray * pos2
;     idxs = where(pos2 eq 1.0)
;     if (idxs[0] ne -1) then aDataArray[idxs] = 1.0
;     idxs = where(pos1 eq 1.0)
;     if (idxs[0] ne -1) then aDataArray[idxs] = sResponse1.missingVal
      
;    case sResponse.gridType of
   if ((sResponse1.gridType ne 'station') and (sResponse2.gridType ne 'station')) then begin ; for non-station data
      aLons = xRN1
      aLats = yRN1
   endif else begin
      aLons = sResponse1.alons
      aLats = sResponse1.alats
   endelse
   if (n_elements(sResponse1.aTimes) gt 1) then begin
      i = 0
      midDaySeg = fltarr(numTimeSeg_0*(size(aDataArray))[2])
      for segIdx = 0, numTimeSeg_0 - 1 do begin
        for dayIdx = 0, (size(aDataArray))[2] - 1 do begin
            self->DayMonthYear, asTimeSeg_0[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
            begTimeJD = julday(reqMonth, reqDay, reqYear, reqHour) 
            midDaySeg[i] = begTimeJD + dayIdx + 0.5
            i = i + 1
        endfor
      endfor
      if (sResponse1.gridType ne 'station') then begin ; for non-station data
        aDataArray = reform(aDataArray, n_elements(aLons), n_elements(aLats), numTimeSeg_0*numDays)
      endif else begin
        aDataArray = reform(aDataArray, n_elements(aLons), numTimeSeg_0*(size(aDataArray))[2])
      endelse
    endif else begin
      midSeg = fltarr(numTimeSeg_0)
      for segIdx = 0, numTimeSeg_0 - 1 do begin
        self->DayMonthYear, asTimeSeg_0[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
        reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
        self->DayMonthYear, asTimeSeg_0[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
        reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
        midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
      endfor
    endelse
    
    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0

    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse1.missingVal, GRIDTYPE=sResponse1.gridType, EXTRA=sExtra)
    if (self->Assert(res)) then return, res


    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcDifference__define

    struct = { cvcCalcDifference, $
               INHERITS cvcCalc $ 
             }
END