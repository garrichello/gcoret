;
;  NAME: 
;    cvcCalcTN10p
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of mean values for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTN10p = obj_new('cvcCalcTN10p', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTN10p->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcTN10p::Init, in_sInputs, in_sOutputs

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs)
END
;--------------------------------------------------------------------
PRO cvcCalcTN10p::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcTN10p::Run
    print, "(cvcCalcTN10p::Run) Started..."
    sVar = self.sIniData.asVar[0] ; only one and the first variable is used
    lev = sVar.sROI.aLev[0]
    oBaseData = obj_new('cvcData', self.sIniData.datasetsInfoFile) ; new object for based period data
    sTimeBased = {structTimeSeg}
      
    print, "(cvcCalcTN10p::Run) Get data..."
    for segIdx = 0, sVar.numTimeSeg-1 do begin
      sTimeSeg = sVar.asTimeSeg[segIdx] ; take one time segment at a time
      resultCode = self.oData->Prepare(sVar, sTimeSeg)  ; set region of interest and prepare datasets
      if (resultCode ne self.ERROR_OK) then return, resultCode

      sResponse = self.oData->GetArray(lev, sTimeSeg)
      if (sResponse.resultCode ne self.ERROR_OK) then return, sResponse.resultCode
      sz = size(sResponse.aData)
      
      timeStep = fix(strmid(sTimeSeg.step, 0, 1)) 
      period = 24/timeStep
      nPrc = sz[3]/period
      
      self->DayMonthYear, sTimeSeg.beginning, in_yr, in_mn, in_dy, in_hr
      begShiftDate = self->DateShift(in_mn, in_dy, 1961, in_hr, 0)
      self->DayMonthYear, sTimeSeg.ending, in_yr, in_mn, in_dy, in_hr
      endShiftDate = self->DateShift(in_mn, in_dy, 1965, in_hr, 1)
      sTimeBased.beginning = begShiftDate
      sTimeBased.ending = endShiftDate
      sTimeBased.step = sTimeSeg.step
      resultCode = oBaseData->Prepare(sVar, sTimeBased)
      if (resultCode ne self.ERROR_OK) then return, resultCode
      nYrs = 5
      print, begShiftDate, endShiftDate
      aPrcCur = fltarr(sz[1], sz[2])
      aDataCur = fltarr(sz[1], sz[2], nYrs*5)
      aCountCur = intarr(sz[1], sz[2])
      
      self->DayMonthYear, sTimeSeg.beginning, in_yr, in_mn, in_dy, in_hr
      for prcIdx = 0, nPrc - 1 do begin 
         in_dayBegCur = self->LongDate(in_yr, in_mn, in_dy, 00) 
         in_dayEndCur = self->LongDate(in_yr, in_mn, in_dy, 18)
         in_sDaySeg = {beginning: in_dayBegCur, ending: in_dayEndCur}
         sDayResponse = self.oData->GetArray(lev, in_sDaySeg)
         aDataReq = min(sDayResponse.aData, Dimension = 3)
         print, in_yr, in_mn, in_dy
         self->SetNextDay, in_yr, in_mn, in_dy
 ;        tst = self->PrcCalc(nYrs, prcIdx, sTimeBased, lev, oBaseData, aDataCur)
         for yrIdx = 0, nYrs - 1 do begin  
            self->DayMonthYear, sTimeBased.beginning, yrc
            if (prcIdx eq 0) then begin
               self->DayMonthYear, sTimeBased.beginning, yrc, mnc, dyc, hrc
               for dy5Idx = 0, 4 do begin
                  dayBegCur = self->LongDate(yrc + yrIdx, mnc, dyc, 00) 
                  dayEndCur = self->LongDate(yrc + yrIdx, mnc, dyc, 18)
                  print, dayBegCur, dayEndCur
                  sDaySeg = {beginning: dayBegCur, ending: dayEndCur} ; take one day in the time segment
                  sDayResponse = oBaseData->GetArray(lev, sDaySeg)
                  aDataArray = min(sDayResponse.aData, Dimension = 3)
                  aDataCur[*,*,dy5Idx + yrIdx*5] = aDataArray
                  self->SetNextDay, yrc, mnc, dyc     
               endfor
            endif else begin
               dayBegCur = self->LongDate(yrc, mnc, dyc, 00) 
               dayEndCur = self->LongDate(yrc, mnc, dyc, 18)
               print, dayBegCur, " - ", dayEndCur
               sDaySeg = {beginning: dayBegCur, ending: dayEndCur}
               sDayResponse = oBaseData->GetArray(lev, sDaySeg)
               aDataArray = min(sDayResponse.aData, Dimension = 3)
               aDataCur[*,*,((prcIdx - 1) mod 5) + yrIdx * 5] = aDataArray
               self->SetNextYear, yrc
            endelse
         endfor
         self->SetNextDay, yrc, mnc, dyc
         sz = size(aDataCur)
         for i = 0, sz[1] - 1 do begin
            for j = 0, sz[2] - 1 do begin
               aPrc = aDataCur[i,j,sort(aDataCur[i,j,*])]
               P10Idx = (10*(n_elements(aPrc))/100) + 1
               aPrcCur[i,j] = aPrc[P10Idx - 1]
               if (aDataReq[i,j] lt aPrcCur[i,j]) then begin
                  aCountCur[i,j] = aCountCur[i,j] + 1
               endif
            endfor
         endfor
         print, aCountCur
      endfor
            
    endfor

    aSubData = aMinArray[min(lonIdxs):max(lonIdxs), min(latIdxs):max(latIdxs)]

 ; set result

;self->__SetResult, aData = aMinArray,$
;                       aLons = sResponse.aLons,$
;                       aLats = sResponse.aLats, $
;                       minVal = min(aMinArray), $
;                       maxVal = max(aMinArray), $
;                       missingVal = sResponse.missingVal, $
;                       sTimeRng = sResponse.sTimeRng, $
;                       aLev = sResponse.aLev, $
;                       nLev = sResponse.nLev
                       
     obj_destroy, oBaseData
;    return, self.ERROR_OK
END

;FUNCTION cvcCalcTN10p::PrcCalc, nYrs, prcIdx, sTimeBased, lev, oBaseData, aDataCur
;    self->DayMonthYear, sTimeBased.beginning, yrc, mnc, dyc, hrc
;    for yrIdx = 0, nYrs-1 do begin  
;        yrs = yrIdx + 1
;        if (prcIdx eq 0) then begin
;           self->DayMonthYear, sTimeBased.beginning, yrc, mnc, dyc, hrc
;           for dy5Idx = 0, 4 do begin
;              dayBegCur = self->LongDate(yrc + yrIdx, mnc, dyc, 00) 
;              dayEndCur = self->LongDate(yrc + yrIdx, mnc, dyc, 18)
       ;       print, dayBegCur, " - ", dayEndCur
;              sDaySeg = {beginning: dayBegCur, ending: dayEndCur} ; take one day in the time segment
;              sDayResponse = oBaseData->GetArray(lev, sDaySeg)
;              aDataArray = min(sDayResponse.aData, Dimension = 3)
;              aDataCur[*,*,dy5Idx + yrIdx*5] = aDataArray
;              self->SetNextDay, yrc, mnc, dyc  
;           endfor
;           print, yrc, mnc, dyc
;        endif else begin    
;         print, prcIdx, yrc, mnc, dyc
       ;    dayBegCur = self->LongDate(yrc, mnc, dyc, 00) 
       ;    dayEndCur = self->LongDate(yrc, mnc, dyc, 18)
   ;        print, dayBegCur, " - ", dayEndCur
;               sDaySeg = {beginning: dayBegCur, ending: dayEndCur}
;               sDayResponse = oBaseData->GetArray(lev, sDaySeg)
;               aDataArray = min(sDayResponse.aData, Dimension = 3)
;               aDataCur[*,*,((prcIdx - 1) mod 5) + yrIdx * 5] = aDataArray
   ;        self->SetNextYear, yrc
 ;       endelse
;         endfor
 ;        sz = size(aDataCur)
 ;        for i = 0, sz[1] - 1 do begin
 ;           for j = 0, sz[2] - 1 do begin
 ;              aPrc = aDataCur[i,j,sort(aDataCur[i,j,*])]
 ;              P10Idx = (10*(n_elements(aPrc))/100) + 1
 ;              aPrcCur[i,j] = aPrc[P10Idx - 1]
         ;   endfor
  ;  endfor
  ; return, yrs
;END

;;-------------------------------------------------------------------
FUNCTION cvcCalcTN10p::DateShift, mn, dy, yr, hr, sft
    if (sft eq 0) then begin
      DateJD = julday(mn, dy, yr, hr) - 2
      caldat, DateJD, out_mn, out_dy, out_yr, out_hr
      ShiftDate = self->LongDate(out_yr, out_mn, out_dy, out_hr)
    endif else if (sft eq 1) then begin
      DateJD = julday(mn, dy, yr, hr) + 2
      caldat, DateJD, out_mn, out_dy, out_yr, out_hr
      ShiftDate = self->LongDate(out_yr, out_mn, out_dy, out_hr)
    endif
    return, ShiftDate
END

;;--------------------------------------------------------------------
FUNCTION cvcCalcTN10p::GetResult
    return, self.sResult
END
;--------------------------------------------------------------------
PRO cvcCalcTN10p__define

    struct = { cvcCalcTN10p, $
               INHERITS cvcCalc $ 
             }
END
