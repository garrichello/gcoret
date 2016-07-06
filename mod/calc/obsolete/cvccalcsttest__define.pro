;
;  NAME: 
;    cvcCalcStTest
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of standard deviation for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcVariance = obj_new('cvcCalcStTest', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcConfRange->Run()
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
FUNCTION cvcCalcStTest::Init, in_sInputs, in_sOutputs 

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END
;--------------------------------------------------------------------
PRO cvcCalcStTest::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcStTest::Run

    print, "(cvcCalcStTest::Run) Started..."
    
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
      res = aoParams[0]->Get(UID='sampLen1', sParam)
      calcMode1 = *(sParam.data)
    endif
    
    if (nParams ne 0) then begin
      res = aoParams[1]->Get(UID='sampLen2', sParam)
      calcMode2 = *(sParam.data)
    endif
    
   ; get info about number of levels and time segments
    retCode_TiMean1 = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_TM1, OUT_TIMESEGS=asTimeSeg_TM1, OUT_NLEVELS=numLevels_TM1, OUT_LEVELS=aLevel_TM1, OUT_ROI=sROI_TM1)
    if (self->Assert(retCode_TiMean1)) then return, retCode_TiMean1
    retCode_TiMean2 = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_TM2, OUT_TIMESEGS=asTimeSeg_TM2, OUT_NLEVELS=numLevels_TM2, OUT_LEVELS=aLevel_TM2, OUT_ROI=sROI_TM2)
    if (self->Assert(retCode_TiMean2)) then return, retCode_TiMean2
    retCode_StDev1 = (aoInputs[2])->GetInfo(OUT_NTIMESEG=numTimeSeg_SD1, OUT_TIMESEGS=asTimeSeg_SD1, OUT_NLEVELS=numLevels_SD1, OUT_LEVELS=aLevel_SD1, OUT_ROI=sROI_SD1)
    if (self->Assert(retCode_StDev1)) then return, retCode_StDev1
    retCode_StDev2 = (aoInputs[3])->GetInfo(OUT_NTIMESEG=numTimeSeg_SD2, OUT_TIMESEGS=asTimeSeg_SD2, OUT_NLEVELS=numLevels_SD2, OUT_LEVELS=aLevel_SD2, OUT_ROI=sROI_SD2)
    if (self->Assert(retCode_StDev2)) then return, retCode_StDev2

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcStTest::Run) Get data..."
    
;    resultCode_TM1 = (aoInputs[0])->Get(Time=asTimeSeg_TM1[0], sResponse_TM1)
;    if (self->Assert(resultCode_TM1)) then return, resultCode_TM1
;    resultCode_TM2 = (aoInputs[1])->Get(Time=asTimeSeg_TM2[0], sResponse_TM2)
;    if (self->Assert(resultCode_TM2)) then return, resultCode_TM2
;    resultCode_SD1 = (aoInputs[2])->Get(Time=asTimeSeg_SD1[0], sResponse_SD1)
;    if (self->Assert(resultCode_SD1)) then return, resultCode_SD1
;    resultCode_SD2 = (aoInputs[3])->Get(Time=asTimeSeg_SD2[0], sResponse_SD2)
;    if (self->Assert(resultCode_SD2)) then return, resultCode_SD2
   
    for segIdx = 0, numTimeSeg_TM1 - 1 do begin
      resultCode_TM1 = (aoInputs[0])->Get(Time=asTimeSeg_TM1[segIdx], sResponse_TM1)
      if (self->Assert(resultCode_TM1)) then return, resultCode_TM1
      resultCode_TM2 = (aoInputs[1])->Get(Time=asTimeSeg_TM2[segIdx], sResponse_TM2)
      if (self->Assert(resultCode_TM2)) then return, resultCode_TM2
        if ((abs(sResponse_TM1.alats[1] - sResponse_TM1.alats[0]) ge abs(sResponse_TM2.alats[1] - sResponse_TM2.alats[0])) $
           and  (abs(sResponse_TM1.alons[1] - sResponse_TM1.alons[0]) ge abs(sResponse_TM2.alons[1] - sResponse_TM2.alons[0]))) then begin
          RN1 = sResponse_TM1.aData
          RN2 = sResponse_TM2.aData
          xRN1 = sResponse_TM1.alons
          yRN1 = sResponse_TM1.aLats
          xRN2 = sResponse_TM2.alons
          yRN2 = sResponse_TM2.aLats
          missRN1 = sResponse_TM1.missingVal
          missRN2 = sResponse_TM2.missingVal
        endif else begin
          RN1 = sResponse_TM2.aData
          RN2 = sResponse_TM1.aData
          xRN1 = sResponse_TM2.alons
          yRN1 = sResponse_TM2.aLats
          xRN2 = sResponse_TM1.alons
          yRN2 = sResponse_TM1.aLats
          missRN1 = sResponse_TM2.missingVal
          missRN2 = sResponse_TM1.missingVal
        endelse
        if (segIdx eq 0) then begin
          sz = size(RN1)
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg_TM1)
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
        idxs = where(RN1 ne sResponse_TM1.aData)  
        if (idxs[0] eq -1) then begin
          Data1 = RN1
          Data2 = RN2
        endif else begin
          Data1 = RN2
          Data2 = RN1
        endelse    
    endfor
   
    for segIdx = 0, numTimeSeg_SD1 - 1 do begin
      resultCode_SD1 = (aoInputs[2])->Get(Time=asTimeSeg_SD1[segIdx], sResponse_SD1)
      if (self->Assert(resultCode_SD1)) then return, resultCode_SD1
      resultCode_SD2 = (aoInputs[3])->Get(Time=asTimeSeg_SD2[segIdx], sResponse_SD2)
      if (self->Assert(resultCode_SD2)) then return, resultCode_SD2
        if ((abs(sResponse_SD1.alats[1] - sResponse_SD1.alats[0]) ge abs(sResponse_SD2.alats[1] - sResponse_SD2.alats[0])) $
           and  (abs(sResponse_SD1.alons[1] - sResponse_SD1.alons[0]) ge abs(sResponse_SD2.alons[1] - sResponse_SD2.alons[0]))) then begin
          RN1 = sResponse_SD1.aData
          RN2 = sResponse_SD2.aData
          xRN1 = sResponse_SD1.alons
          yRN1 = sResponse_SD1.aLats
          xRN2 = sResponse_SD2.alons
          yRN2 = sResponse_SD2.aLats
          missRN1 = sResponse_SD1.missingVal
          missRN2 = sResponse_SD2.missingVal
        endif else begin
          RN1 = sResponse_SD2.aData
          RN2 = sResponse_SD1.aData
          xRN1 = sResponse_SD2.alons
          yRN1 = sResponse_SD2.aLats
          xRN2 = sResponse_SD1.alons
          yRN2 = sResponse_SD1.aLats
          missRN1 = sResponse_SD2.missingVal
          missRN2 = sResponse_SD1.missingVal
        endelse
        if (segIdx eq 0) then begin
          sz = size(RN1)
          aDataArray = fltarr(sz[1], sz[2], numTimeSeg_SD1)
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
        idxs = where(RN1 ne sResponse_SD1.aData)  
        if (idxs[0] eq -1) then begin
          Data3 = RN1
          Data4 = RN2
        endif else begin
          Data3 = RN2
          Data4 = RN1
        endelse    
    endfor

;   idx = where(sResponse_TM2.aData eq sResponse_TM2.missingVal)
;   numArray = abs(sResponse_TM1.aData - sResponse_TM2.aData) / sqrt((float(calcMode1)-1)*(sResponse_SD1.aData)^2 + (float(calcMode2) - 1)*(sResponse_SD2.aData)^2)
;   denArray = sqrt(float(calcMode1)*float(calcMode2)*(float(calcMode1)+float(calcMode2)-2)/(float(calcMode1) + float(calcMode2)))
;   aDataArray = numArray * denArray
;   if (idx[0] ne -1) then aDataArray[idx] = sResponse_TM2.missingVal

   numArray = abs(Data1 - Data2) / sqrt((float(calcMode1)-1)*Data3^2 + (float(calcMode2) - 1)*Data4^2)
   denArray = sqrt(float(calcMode1)*float(calcMode2)*(float(calcMode1)+float(calcMode2)-2)/(float(calcMode1) + float(calcMode2)))
   aDataArray = numArray * denArray

;    case sResponse.gridType of
    aLons = sResponse_TM2.aLons
    aLats = sResponse_TM2.aLats
    self->DayMonthYear, asTimeSeg_TM1[0].beginning, reqYear, reqMonth, reqDay, reqHour
    reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
    self->DayMonthYear, asTimeSeg_TM1[0].ending, reqYear, reqMonth, reqDay, reqHour
    reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
    midSeg = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse_TM2.missingVal, GRIDTYPE=sResponse_TM2.gridType)
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcStTest__define

    struct = { cvcCalcStTest, $
               INHERITS cvcCalc $ 
             }
END