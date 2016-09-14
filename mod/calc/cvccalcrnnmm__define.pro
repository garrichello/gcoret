;
;  NAME: 
;    cvcCalcRnnmm
;
;  DESCRIPTION:
;    Annual count of days when daily maximum temperature greater than 25 C
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of precipitation for time series of data
;    (Annual count of days when daily precipitation greater than nn mm)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcRnnmm = obj_new('cvcCalcRnnmm', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcSummerDays->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcRnnmm::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END

;--------------------------------------------------------------------
PRO cvcCalcRnnmm::Cleanup

    self->cvcCalc::Cleanup

END

FUNCTION cvcCalcRnnmm::Run
    self->printLog, "(cvcCalcRnnmm::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
    
      res = aoParams[0]->Get(UID='Mode', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
      
      res = aoParams[0]->Get(UID='threshold', sParam)
      if (self->Assert(res)) then return, res
      threshold = *(sParam.data)
            
    endif else begin
	calcMode = 'single'
	threshold = 0.01
    endelse

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode = (aoInputs[dataIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
    if (self->Assert(retCode)) then return, retCode

    ; level to read
    levelIdx = 0
        
    ; main loop for all time segments
    self->printLog, "(cvcCalcRnnmm::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0

    for segIdx = 0, numTimeSeg - 1 do begin
      resultCode = (aoInputs[dataIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponse)
      if (self->Assert(resultCode)) then return, resultCode

      if (sResponse.gridType ne 'station') then begin ; for non-station data
	aUniqDaysPos = uniq(long(sResponse.aTimes-0.5))
	nDays = n_elements(aUniqDaysPos)
	if (segIdx eq 0) then begin
	    sz = size(sResponse.aData, /dim)
    	    Rnnmm = fltarr(sz[0],sz[1],numTimeSeg, /nozero)
    	endif
    	totDay = fltarr(sz[0],sz[1], nDays, /nozero)
    	

    	stDayPos = 0
    	for iDays = 0, nDays-2 do begin
    	    totDay[*,*,iDays] = total(sresponse.adata[*,*,stDayPos:aUniqDaysPos[iDays]],3)
    	    stDayPos = aUniqDaysPos[iDays] + 1
    	endfor
        
        for j=0, sz[1]-1 do begin
    	    for i=0, sz[0]-1 do begin
    		Rnnmm[i,j,segIdx] = n_elements(where(totDay[i,j,*] ge threshold))
    	    endfor
        endfor

      endif else begin ; for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        aUniqDaysPos = uniq(long(sResponse.aTimes[0,*] - 0.5))
        nDays = n_elements(aUniqDaysPos)
        if (segIdx eq 0) then begin
    	    sz = size(sResponse.aData, /dim)
            Rnnmm = fltarr(sz[0],numTimeSeg, /nozero)
	endif
	totDay = fltarr(sz[0], nDays, /nozero)
	
	for iSt = 0, sz[0]-1 do begin
	    stDayPos = 0
	    for iDays = 0, nDays-1 do begin
		totDay[iSt, iDays] = total(sresponse.adata[iSt, stDayPos:aUniqDaysPos[iDays]])
	        stDayPos = aUniqDaysPos[iDays] + 1
	    endfor
	endfor
	
	for iSt=0, sz[0]-1 do begin
	    Rnnmm[iSt,segIdx] = n_elements(where(totDay[iSt,*] ge threshold))
	endfor
        
      endelse
    endfor

if (calcMode eq 'single') then begin
      if (sResponse.gridType ne 'station') then begin ; for non-station data
        sum = Rnnmm[*, *, 0]
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = Rnnmm[*, *, segIdx]
          sum = sum + tmpArr      
        endfor
        aDataArray = sum / numTimeSeg 
      endif else begin ; for station data
        sum = Rnnmm[*, 0]
        for segIdx = 1, numTimeSeg - 1 do begin
          tmpArr = totID[*, segIdx]
          sum = sum + tmpArr     
        endfor
        aDataArray = sum /NumTimeSeg
      endelse
    endif else begin
      if (calcMode eq 'multi') then begin
        if (sResponse.gridType ne 'station') then begin ; for non-station data
    	    aDataArray = Rnnmm
        endif else begin ; for station data
            aDataArray = Rnnmm
        endelse
      endif else begin
        self->printLog, '(cvcCalcRnnmm) Error! Unknown calculation mode: ',Calcmode
        return, -1
      endelse
    endelse
    
    ;    case sResponse.gridType of
    aLons = sResponse.aLons
    aLats = sResponse.aLats
    midSeg = fltarr(numTimeSeg)
    for segIdx = 0, numTimeSeg - 1 do begin
      self->DayMonthYear, asTimeSeg[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
      self->DayMonthYear, asTimeSeg[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
      midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0

    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    if (calcMode eq 'single') then begin
      res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
    endif else begin
      if (calcMode eq 'multi') then begin
        res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType, EXTRA=sExtra)
      endif else begin
        self->printLog, '(cvcCalcRnnmm) Error! Unknown calculation mode: ', Calcmode
        return, -1
      endelse
    endelse
    if (self->Assert(res)) then return, res

     return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcCalcRnnmm::GetResult
   return, self.sResult
END
;--------------------------------------------------------------------
    


pro cvcCalcRnnmm__define
   struct = { cvcCalcRnnmm, $
               INHERITS cvcCalc $
             }
end
