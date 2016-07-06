;
;  NAME: 
;    cvcCalcConfRange
;
;  DESCRIPTION:
;    Unknown. It can be used, but it requires using t-test critical values (you have to set it up by hand…).
;
;  PURPOSE:
;    Implements calculation of a spatial field of standard deviation for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcVariance = obj_new('cvcCalcConfRange', sIniData)
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
FUNCTION cvcCalcConfRange::Init, in_sInputs, in_sOutputs, in_hLogFile 

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcConfRange::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcConfRange::Run

    self->printLog, "(cvcCalcConfRange::Run) Started..."
    
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
      res = aoParams[0]->Get(UID='tValue', sParam)
      if (self->Assert(res)) then return, res
      calcMode1 = *(sParam.data)
    
      res = aoParams[1]->Get(UID='sampLen', sParam)
      if (self->Assert(res)) then return, res
      calcMode2 = *(sParam.data)
    endif else begin
      self->printLog, "(cvcCalcConfRange::Run) Parameters tValue and sampLen must be set in the task-file! Aborting..."
      return, -1
    endelse
    
   ; get info about number of levels and time segments
    retCode_TiMean = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_TM, OUT_TIMESEGS=asTimeSeg_TM, OUT_NLEVELS=numLevels_TM, OUT_LEVELS=aLevel_TM, OUT_ROI=sROI_TM)
    if (self->Assert(retCode_TiMean)) then return, retCode_TiMean
    retCode_StDev = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_SD, OUT_TIMESEGS=asTimeSeg_SD, OUT_NLEVELS=numLevels_SD, OUT_LEVELS=aLevel_SD, OUT_ROI=sROI_SD)
    if (self->Assert(retCode_StDev)) then return, retCode_StDev
    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcConfRange::Run) Get data..."
   
   resultCode_TM = (aoInputs[0])->Get(Time=asTimeSeg_TM[0], Region=sROI_TM, sResponse_TM)
   
   resultCode_SD = (aoInputs[1])->Get(Time=asTimeSeg_SD[0], Region=sROI_SD, sResponse_SD)
   if (self->Assert(resultCode_SD)) then return, resultCode_SD
   
   if (self->Assert(resultCode_TM)) then return, resultCode_TM
   if ((sResponse_TM.gridType eq 'station') or (sResponse_SD.gridType eq 'station') then begin
     self->printLog, "(cvcCalcConfRange::Run) This module does not work with station data! Aborting..."
     return, -1
   endif

   aDataArray = float(calcMode1) * sResponse_SD.aData / sqrt(float(calcMode2))

;    case sResponse.gridType of
    aLons = sResponse_TM.aLons
    aLats = sResponse_TM.aLats
    self->DayMonthYear, asTimeSeg_TM[0].beginning, reqYear, reqMonth, reqDay, reqHour
    reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
    self->DayMonthYear, asTimeSeg_TM[0].ending, reqYear, reqMonth, reqDay, reqHour
    reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
    midSeg = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2

    ; put result
    ; output we want to write to, let's get the first one

    outIdx = 0    
      
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse_TM.missingVal, GRIDTYPE=sResponse_TM.gridType, EXTRA=sExtra2)
    if (self->Assert(res)) then return, res
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcConfRange__define

    struct = { cvcCalcConfRange, $
               INHERITS cvcCalc $ 
             }
END
