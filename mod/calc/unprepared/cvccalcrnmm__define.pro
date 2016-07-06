UNFINISHED!!!!
;
;  NAME: 
;    cvcCalcRNmm
;
;  DESCRIPTION:
;    Annual count of days with daily precipitation amount greater than N mm
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of annual count of days for time series of data
;    (Annual count of days with daily precipitation amount greater then N mm)
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTimeMean = obj_new('cvcCalcRNmm', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcR10->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcRNmm::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcRNmm::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcRNmm::Run
    self->printLog, "(cvcCalcRNmm::Run) Started..."

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
    self->printLog, "(cvcCalcRNmm::Run) Get data..."
    
    for segIdx = 0, numTimeSeg-1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode      
       
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sz = size(sResponse.aData)
        aDayArray = intarr(sz[1], sz[2]) ;sResponse.aData
      
        self->DayMonthYear, asTimeSeg[segIdx].beginning, yrc, mnc, dyc, hrc
        curBegDay = self->LongDate(yrc, mnc, dyc, 0) 
        curEndDay = self->LongDate(yrc, mnc, dyc, 18)
        self->DayMonthYear, asTimeSeg[segIdx].ending, yre, mne, dye
        while (curBegDay le asTimeSeg[segIdx].ending) do begin
          sDaySeg = {beginning: curBegDay, ending: curEndDay} ; take one day in the time segment
          sDayResponse = self.oData->GetArray(lev, sDaySeg)
          validIdx = where(sDayResponse.aData ne sDayResponse.missingVal)
          aDataArray = temporary(total(sDayResponse.aData, 3))
          dayIdxs = where(aDataArray[validIdx] gt 0.01, cnt)
          if (cnt gt 0) then begin
             aDayArray[dayIdxs] = aDayArray[dayIdxs] + 1 
          endif
          self->SetNextDay, yrc, mnc, dyc
          curBegDay = self->Longdate(yrc, mnc, dyc, 0)
          CurEndDay = self->LongDate(yrc, mnc, dyc, 18)
      endwhile
    endfor

; set result
    self->__SetResult, aData = aDayArray, $
                       varName = self.sIniData.asVar[0].name, $
                       aLons = sResponse.aLons, $
                       aLats = sResponse.aLats, $
                       minVal = min(aDayArray), $
                       maxVal = max(aDayArray), $
                       missingVal = sResponse.missingVal, $
                       sTimeRng = sResponse.sTimeRng, $
                       aLev = sResponse.aLev, $
                       nLev = sResponse.nLev
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcRNmm::GetResult
    return, self.sResult
END
;--------------------------------------------------------------------
PRO cvcCalcRNmm__define

    struct = { cvcCalcRNmm, $
               INHERITS cvcCalc $ 
             }
END