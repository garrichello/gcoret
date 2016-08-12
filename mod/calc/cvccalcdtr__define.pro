;NAME: 
;    cvcCalcDTR
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of daily temperature range for time series of data
;    (Daily temperature range: Relative sum of annual/monthly difference between highest and lowest temperature)
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcdtr = obj_new('cvcCalcdtr', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcdtr->Run()
;
;--------------------------------------------------------------------    

FUNCTION cvcCalcDTR::Init, in_sInputs, in_sOutputs, in_hLogFile
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END    
      
;--------------------------------------------------------------------
PRO cvcCalcDTR::Cleanup
       self->cvcCalc::Cleanup
END
  
  
  
FUNCTION cvcCalcDTR::Run
    self->printLog, "(cvcCalcDTR::Run) Started..."

; get inputs
res = self->GetInputs(aoInputs, nData)
if (nData eq 0) then return, self->SetErrMsg("No module inputs!")
; get outputs
res = self->GetOutputs(aoOutputs, nOutputs)
if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")
                     
res = self->GetParams(aoParams, nParams)

if (nParams ne 0) then begin
    res = aoParams[0]->Get(UID='mode1', sParam)
    if (self->Assert(res)) then return, res
    calcMode = *(sParam.data)
endif else calcMode = 'single'
                     
; input we want to process, let's get the first one
maxIdx = 0
minIdx = 1    
                         
; get info about number of levels and time segments
retCode = (aoInputs[maxIdx])->GetInfo(OUT_NTIMESEG=numTimeSeg, OUT_TIMESEGS=asTimeSeg, OUT_NLEVELS=numLevels, OUT_LEVELS=aLevel, OUT_ROI=sROI)
if (self->Assert(retCode)) then return, retCode
                         
; level to read
levelIdx = 0
                             
; main loop for all time segments
self->printLog, "(cvcCalcDTR::Run) Get data..."
cnt = 0 ; number of summed fields
                                 
; time segment to read
segIdx = 0
                                 
                               

for segIdx = 0, numTimeSeg - 1 do begin
resultCode1 = (aoInputs[maxIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponseMax)
if (self->Assert(resultCode1)) then return, resultCode1
                                 
resultCode2 = (aoInputs[minIdx])->Get(Time=asTimeSeg[segIdx], Level=aLevel[levelIdx], Region=sROI, sResponseMin)
if (self->Assert(resultCode2)) then return, resultCode2
                            
    if (sResponseMax.gridType ne 'station') then begin ; for non-station data
                                     
	if (segIdx eq 0) then begin
	    sz = size(sResponseMax.aData)
	    numTime = n_elements(sResponseMax.aTimes)
	    SumDef = fltarr(sz[1], sz[2], numTimeSeg)
	endif  
	maxArr = sResponseMax.adata
	minArr = sResponseMin.adata                           
	for iTime=0, numTime-1 do begin
	    SumDef[*,*,segIdx] = SumDef[*,*,segIdx] + maxArr[*,*,iTime]-minArr[*,*,iTime]
	endfor      
                                                   
    endif else begin ; for station data
	sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }	
                                                       
	if (segIdx eq 0) then begin
	    sz = size(sResponseMax.aData)
	    numTime = n_elements(sResponseMax.aTimes)
    	    SumDef = fltarr(sz[1], numTimeSeg)
	endif  
	maxArr = sResponseMax.adata
	minArr = sResponseMin.adata
                                                           
	for iTime=0, numTime-1 do begin
	    SumDef[*,segIdx] = SumDef[*,segIdx] + maxArr[*,iTime]-minArr[*,iTime]
	endfor    
                                                           
    endelse
                                                           
endfor
DTR = SumDef/numTime
                                                               
if (calcMode eq 'single') then begin
    if (sResponseMax.gridType ne 'station') then begin ; for non-station data
	sum = DTR[*, *, 0]
	for segIdx = 1, numTimeSeg - 1 do begin
	    tmpArr = DTR[*, *, segIdx]
	    sum = sum + tmpArr     
	endfor
	aDataArray = sum / numTimeSeg
    endif else begin ; for station data
	sum = DTR[*, 0]
	for segIdx = 1, numTimeSeg - 1 do begin
	    tmpArr = DTR[*, segIdx]
	    sum = sum + tmpArr     
	endfor
	aDataArray = sum / numTimeSeg
    endelse
endif else begin
    if (calcMode eq 'multi') then begin
	if (sResponseMax.gridType ne 'station') then begin ; for non-station data
	    aDataArray = DTR
	endif else begin ; for station data
	    aDataArray = DTR
	endelse
    endif else begin
	self->printLog, "(cvcCalcDTR Error! Unknown calculation mode: ", calcMode
	return, -1
    endelse
endelse
                                                               
                                                                       
    aLons = sResponseMax.aLons
    aLats = sResponseMax.aLats
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
	res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, MISSING=sResponseMax.missingVal, GRIDTYPE=sResponseMax.gridType)
    endif else begin
	if (calcMode eq 'multi') then begin
	    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponseMax.missingVal, GRIDTYPE=sResponseMax.gridType)
	endif else begin
	    self->printLog, '(cvcCalcDTR) Error! Unknown calculation mode: ', calcmode
    	    return, -1
	endelse
    endelse
    if (self->Assert(res)) then return, res
                                                                                                    
    return, self.ERROR_OK

END

;--------------------------------------------------------------------
FUNCTION cvcCalcDTR::GetResult
   return, self.sResult
   END
;--------------------------------------------------------------------
   
                                                                                                    
                                                                                                    
pro cvcCalcDTR__define
    struct = { cvcCalcDTR, $
	        INHERITS cvcCalc $
	     }
end
                                                                                                    
