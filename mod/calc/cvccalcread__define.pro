;
;  NAME: 
;    cvcCalcRead
;
;  PURPOSE:
;    Sample class. Implements plain reading of a spatial field
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTimeMean = obj_new('cvcCalcTimeMean', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTimeMean->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcRead::Init, in_sInputs, in_sOutputs, in_hLogFile

    res =  self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
    return, res
END
;--------------------------------------------------------------------
PRO cvcCalcRead::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcRead::Run
 
    self->printLog, "(cvcCalcRead::Run) Started..."
    
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
    self->printLog, "(cvcCalcRead::Run) Get data..."

    ; time segment to read
    segIdx = 0

    resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel, Region=sROI, sResponse)
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

    if (sResponse.gridType eq 'stations') then sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    

;    help, sResponse.aData
;    help, sResponse.aLons
;    help, sResponse.aLats
;    help, sResponse.aTimes
;    help, sResponse.aLev

    res = (aoOutputs[outIdx])->Put(sResponse.aData)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=sResponse.aLons, YGRID=sResponse.aLats, $
      TIMEGRID=sResponse.aTimes, $ ;ZGRID=sResponse.aLev, $
      MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    if (self->Assert(res)) then return, res

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcRead__define

    struct = { cvcCalcRead, $
               INHERITS cvcCalc $ 
             }
END
