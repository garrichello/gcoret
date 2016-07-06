;
;  NAME: 
;    cvcCalcStat
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of standard deviation for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcVariance = obj_new('cvcCalcStat', sIniData)
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
FUNCTION cvcCalcStat::Init, in_sInputs, in_sOutputs 

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END
;--------------------------------------------------------------------
PRO cvcCalcStat::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcStat::Run

    print, "(cvcCalcStat::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
   ; get info about number of levels and time segments
    retCode = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    print, "(cvcCalcStat::Run) Get data..."
    
    resultCode = (aoInputs[0])->Get(Time=asTimeSeg[0], sResponse)
    if (self->Assert(resultCode)) then return, resultCode
    sz = size(sResponse.aData)
    aDataArray= fltarr(sz[1], sz[2])
    idxSt = where(sResponse.aData gt 2.4286)
    if (idxSt[0] ne -1) then aDataArray[idxSt] = 1.0
    idxNo = where(sResponse.aData le 2.4286)
    if (idxNo[0] ne -1) then aDataArray[idxNo] = 0.0
    idx = where(sResponse.aData eq sResponse.missingVal)
    if (idx[0] ne -1) then aDataArray[idx] = sResponse.missingVal


;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
;    self->DayMonthYear, asTimeSeg_TM1[0].beginning, reqYear, reqMonth, reqDay, reqHour
;    reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
;    self->DayMonthYear, asTimeSeg_TM1[0].ending, reqYear, reqMonth, reqDay, reqHour
;    reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
;    midSeg = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2

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
PRO cvcCalcStat__define

    struct = { cvcCalcStat, $
               INHERITS cvcCalc $ 
             }
END