;
;  NAME: 
;    cvcCalcUser
;
;  PURPOSE:
;    Intermediate class for using user modules
;
;  INHERITS:
;    cvcCalc
;
;--------------------------------------------------------------------
FUNCTION cvcCalcUser::Init, sIniData, in_hLogFile

    return, self->cvcCalc::Init(sIniData, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcUser::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcUser::Run

    self->printLog, "(cvcCalcUser::Run) Not implemented yet..."
    
;    ; get number of datasets
;    if (self->Assert(self->GetInfo(OUT_NDATA=nData))) then return, retCode
;    ; variable Id we want to process
;    dataId = 0
;    ; level Id to read
;    levelId = 0
;    retCode = self->GetInfo(dataId, OUT_NTIMESEG=numTimeSeg, IN_LEVELIDX=levelId, OUT_LEVEL=level, OUT_ROI=sROI)
;    if (self->Assert(retCode)) then return, retCode
;        
;    ; main loop for all time segments
;    self->printLog, "(cvcCalcUser::Run) Get data..."
; 
;    for segIdx = 0, numTimeSeg-1 do begin
;      retCode = self->GetInfo(dataId, IN_TIMESEGIDX=segIdx, OUT_TIMESEG=sTimeSeg) ; take one time segment at a time
;      if (self->Assert(retCode)) then return, retCode
;
;      resultCode = self.oData->Get(dataId, Time=sTimeSeg, Level=level, Region=sROI, sResponse)
;      if (self->Assert(resultCode)) then return, resultCode
;
;
;
;;     sResponse structure contains:
;;       aData - data array
;;       aLons - longitudes array
;;       aLats - latitudes array
;;       aTimes - times (JD) array
;;       minVal - minimum data value
;;       maxVal - maximum data value
;;       missingVal - missing data value
;;       sTimeRng - date-time range of data returned
;;       aLev - array of levels
;;       nLev - number of levels
;;       resultCode - result code
;    
;      if (sResponse.gridType ne 'station') then begin ; for non-station data
;        sum = sResponse.aData[*, *, 0]
;        cnt = sum ne sResponse.missingVal
;        sum = sum * cnt
;        for i = 1, (size(sResponse.aData))[3]-1 do begin
;          pos = (sResponse.aData[*, *, i] ne sResponse.missingVal)
;          cnt = cnt + pos
;          sum = sum + sResponse.aData[*, *, i] * pos
;        endfor
;        aDataArray = sum / cnt
;        idxs = where(cnt eq 0)
;        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
;        case sResponse.gridType of
;        'regular': begin
;            aLons = sResponse.aLons
;            aLats = sResponse.aLats
;          end
;        'irregular': begin
;            aLons = sResponse.aLons[*, *, 0]
;            aLats = sResponse.aLats[*, *, 0]
;          end
;        endcase
;      endif else begin ; for station data
;        sum = sResponse.aData[*, 0]
;        cnt = sum ne sResponse.missingVal
;        sum = sum * cnt
;        for i = 1, (size(sResponse.aData))[2]-1 do begin
;          pos = (sResponse.aData[*, i] ne sResponse.missingVal)
;          cnt = cnt + pos
;          sum = sum + sResponse.aData[*, i] * pos
;        endfor
;        aDataArray = sum / cnt
;        idxs = where(cnt eq 0)
;        if (idxs[0] ne -1) then aDataArray[idxs] = sResponse.missingVal 
;        aLons = sResponse.aLons
;        aLats = sResponse.aLats
;      endelse
;  
;  ; push result
;      res = self->__PushResult(DATA=aDataArray, XGRID=aLons, YGRID=aLats, MISSING=sResponse.missingVal, GRIDTYPE=sResponse.gridType)
;      if (self->Assert(res)) then return, res
;
;    endfor

    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcUser__define

    struct = { cvcCalcUser, $
               INHERITS cvcCalc $ 
             }
END
