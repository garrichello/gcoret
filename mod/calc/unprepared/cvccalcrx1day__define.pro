UNFINISHED!!!!
;
;  NAME: 
;    cvcCalcRx1Day
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of max values for time series of data
;    (Monthly maximum 1-day precipitation)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcRx1Day = obj_new('cvcCalcRx1Day', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcRx1Day->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcRx1Day::Init, in_sInputs, in_sOutputs
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END

;--------------------------------------------------------------------
PRO cvcCalcRx1Day::Cleanup

    self->cvcCalc::Cleanup

END
FUNCTION cvcCalcRx1Day::Run
    print, "(cvcCalcRx1Day::Run) Started..."
  ; variable we want to process
    sVar = self.sIniData.asVar[0] ; only one and the first variable is used
  ; level to read
    lev = sVar.sROI.aLev[0]
        
  ; main loop for all time segments
    print, "(cvcCalcRx1Day::Run) Get data..."
    for segIdx = 0, sVar.numTimeSeg-1 do begin
       sTimeSeg = sVar.asTimeSeg[segIdx] ; take one time segment at a time
       resultCode = self.oData->Prepare(sVar, sTimeSeg)  ; set region of interest and prepare datasets
       if (resultCode ne self.ERROR_OK) then return, resultCode

       sResponse = self.oData->GetNextArray(lev)
       if (sResponse.resultCode ne self.ERROR_OK) then return, sResponse.resultCode
       aMaxArray = sResponse.aData
            
       self->DayMonthYear, sTimeSeg.beginning, yrc, mnc, dyc, hrc
       curBegDay = self->LongDate(yrc, mnc, dyc, 0) 
       curEndDay = self->LongDate(yrc, mnc, dyc, 18)
       self->DayMonthYear, sTimeSeg.ending, yre, mne, dye
       while (CurBegDay le sTimeSeg.ending) do begin
          sDaySeg = {beginning: curBegDay, ending: curEndDay} ; take one day in the time segment
          sDayResponse = self.oData->GetArray(lev, sDaySeg)
          aDataArray = temporary(total(sDayResponse.aData, 3))
          maxIdxs = where(aDataArray gt aMaxArray, cnt)
          if (cnt gt 0) then begin
             aMaxArray[maxIdxs] = aDataArray[maxIdxs] ; maximum daily value
          endif
          self->SetNextDay, yrc, mnc, dyc
          curBegDay = self->Longdate(yrc, mnc, dyc, 0)
          curEndDay = self->LongDate(yrc, mnc, dyc, 18)
       endwhile 
    endfor

 ; set result
    self->__SetResult, aData = aMaxArray,$
                       aLons = sResponse.aLons,$
                       aLats = sResponse.aLats, $
                       minVal = min(aMaxArray), $
                       maxVal = max(aMaxArray), $
                       missingVal = sResponse.missingVal, $
                       sTimeRng = sResponse.sTimeRng, $
                       aLev = sResponse.aLev, $
                       nLev = sResponse.nLev
                       
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcRx1Day::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcRx1Day__define
   struct = { cvcCalcRx1Day, $
               INHERITS cvcCalc $
             }
end