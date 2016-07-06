;
;  NAME: 
;    cvcCalcLowExceed
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of cold nights/days values for time series of data
;    
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalclowTSexceedance = obj_new('cvcCalcLowExceed', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalclowPDFtail->Run()
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
FUNCTION cvcCalcLowExceed::Init, in_sInputs, in_sOutputs, in_hLogFile
  
    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcLowExceed::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
PRO cvcCalcLowExceed::BPtoIPinterp, i, sResponse_IP, sResponse_BP, aIPreGrid, aBPreGrid, reLons, reLats
   if ((abs(sResponse_IP.aLats[1] - sResponse_IP.aLats[0]) ge abs(sResponse_BP.aLats[1] - sResponse_BP.aLats[0])) and $
   (abs(sResponse_IP.aLons[1] - sResponse_IP.aLons[0]) ge abs(sResponse_BP.aLons[1] - sResponse_BP.alons[0]))) then begin
     IData = sResponse_BP.aData[*, *, i]
     reLons = sResponse_IP.aLons
     reLats = sResponse_IP.aLats
     xUnitVector = replicate(1.0, (size(IData))[1])
     yUnitVector = replicate(1.0, (size(IData))[2])
     xRq = sResponse_BP.aLons # yUnitVector
     yRq = xUnitVector # sResponse_BP.aLats
     validx = where(IData ne sResponse_IP.missingVal)
     triangulate, xRq[validx], yRq[validx], tr
     aIPreGrid = griddata(xRq[validx], yRq[validx], IData[validx], /linear, triangles = tr, xout = [reLons], yout = [reLats], /grid)
     aBPreGrid = sResponse_IP.aData
   endif else begin
     IData = sResponse_IP.aData[*, *, i]
     reLons = sResponse_BP.aLons
     reLats = sResponse_BP.aLats
     xUnitVector = replicate(1.0, (size(IData))[1])
     yUnitVector = replicate(1.0, (size(IData))[2])
     xRq = sResponse_IP.aLons # yUnitVector
     yRq = xUnitVector # sResponse_IP.aLats
     validx = where(IData ne sResponse_IP.missingVal)
     triangulate, xRq[validx], yRq[validx], tr
     aIPreGrid = griddata(xRq[validx], yRq[validx], IData[validx], /linear, triangles = tr, xout = [reLons], yout = [reLats], /grid)
     aBPreGrid = sResponse_BP.aData[*, *, i]
   endelse
   
END
;--------------------------------------------------------------------
PRO cvcCalcLowExceed::Exseedance, aIPreGrid, aBPreGrid, missingVal, calcMode, valPos, tmpCnt, curData, curCnt  
   posIP = long(aIPreGrid ne missingVal)
   posBP = long(aBPreGrid ne missingVal)
   idxs = where(aIPreGrid * posIP * posBP lt aBPreGrid * posIP * posBP, complement = zdxs)
   case calcMode of
     'frequency': begin                   
                    if (idxs[0] ne -1) then curCnt[idxs] = curCnt[idxs] + 1
                      valPos = valPos + posIP * posBP
                    end
     'intensity': begin
                    if (idxs[0] ne -1) then begin
                      curData[idxs] = curData[idxs] + abs(aIPreGrid[idxs] - aBPreGrid[idxs])
                      curCnt[idxs] = curCnt[idxs] + 1
                    endif
                      valPos = valPos + posIP * posBP
                  end
     'duration': begin
                   pos = long(posIP * posBP eq 1)
                   valPos = valPos + pos
                   tmpPos = fltarr(size(pos, /dim))
                   if (idxs[0] ne -1) then begin
                     tmpCnt[idxs] = tmpCnt[idxs] + 1
                     pos[idxs] = 0.0
                     tmpPos[idxs] = 1.0
                     idx = where(tmpCnt * tmpPos gt curCnt * tmpPos)
                     if (idx[0] ne -1) then curCnt[idx] = tmpCnt[idx]
                   endif
                   idx = where(tmpCnt * pos gt curCnt * pos)
                   if (idx[0] ne -1) then curCnt[idx] = tmpCnt[idx]
                   if (zdxs[0] ne -1) then tmpCnt[zdxs] = 0.0
                 end
            else: print, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode           
          endcase
END
;--------------------------------------------------------------------
FUNCTION cvcCalcLowExceed::Run
    
    self->printLog, "(cvcCalcLowExceed::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")   
    
    ; get info about number of levels and time segments
    retCode_IP = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_IP, OUT_TIMESEGS=asTimeSeg_IP, OUT_NLEVELS=numLevels_IP, OUT_LEVELS=aLevel_IP, OUT_ROI=sROI_IP)
    if (self->Assert(retCode_IP)) then return, retCode_IP
    retCode_BP = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_BP, OUT_TIMESEGS=asTimeSeg_BP, OUT_NLEVELS=numLevels_BP, OUT_LEVELS=aLevel_BP, OUT_ROI=sROI_BP)
    if (self->Assert(retCode_BP)) then return, retCode_BP

    ; level to read
    levelIdx = 0
    
    ; main loop for all time segments
    self->printLog, "(cvcCalcLowExceed::Run) Get data..."
    cnt = 0 ; number of summed fields
    
    ; time segment to read
    segIdx = 0

    ; get parameters
    res = self->GetParams(aoParams, nParams)
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='exceedanceType', sParam)
      if (self->Assert(res)) then return, res
      calcMode = *(sParam.data)
    endif else calcMode = 'frequency'
    
    ; dataset of percentiles calculated for based period

    resultCode_BP = (aoInputs[1])->Get(Time=asTimeSeg_BP[0], Level=aLevel_BP[0], Region=sROI_BP, sResponse_BP)
    if (self->Assert(resultCode_BP)) then return, resultCode_BP
   
    for segIdx = 0, numTimeSeg_IP - 1 do begin
      resultCode_IP = (aoInputs[0])->Get(Time=asTimeSeg_IP[segIdx], Level=aLevel_IP[0], Region=sROI_IP, sResponse_IP)
      if (self->Assert(resultCode_IP)) then return, resultCode_IP   
      if (sResponse_BP.gridType ne 'station') then begin ; for non-station data
        print, n_elements(sResponse_IP.aTimes), segIdx
        for i = 0, n_elements(sResponse_IP.aTimes) - 1 do begin
          if ((abs(sResponse_IP.aLats[1] - sResponse_IP.aLats[0]) ne abs(sResponse_BP.aLats[1] - sResponse_BP.aLats[0])) and $
             (abs(sResponse_IP.aLons[1] - sResponse_IP.aLons[0]) ne abs(sResponse_BP.aLons[1] - sResponse_BP.alons[0]))) then begin
             self->BPtoIPinterp, i, sResponse_IP, sResponse_BP, aIPreGrid, aBPreGrid, reLons, reLats
          endif else begin
             reLons = sResponse_IP.aLons
             reLats = sResponse_IP.aLats
             aIPreGrid = sResponse_IP.aData[*,*,i]
             aBPreGrid = sResponse_BP.aData[*,*,i]
          endelse
          missingVal = sResponse_IP.missingVal 
          if (i eq 0) then begin
            valPos = fltarr(n_elements(reLons), n_elements(reLats))
            curData = fltarr(n_elements(reLons), n_elements(reLats))
            curCnt = fltarr(n_elements(reLons), n_elements(reLats))
            tmpCnt = fltarr(n_elements(reLons), n_elements(reLats))
            if (segIdx eq 0) then begin
              excData = fltarr(n_elements(reLons), n_elements(reLats), numTimeSeg_IP)
              excCnt = fltarr(n_elements(reLons), n_elements(reLats), numTimeSeg_IP)
              aDataArray = fltarr(n_elements(reLons), n_elements(reLats), numTimeSeg_IP)
            endif
          endif
          self->Exseedance, aIPreGrid, aBPreGrid, missingVal, calcMode, valPos, tmpCnt, curData, curCnt
        endfor
        case calcMode of
          'frequency': begin 
                         idxs = where(valPos eq 0)
                         if (idxs[0] ne -1) then curCnt[idxs] = sResponse_IP.missingVal
                         aDataArray[*, *, segIdx] = curCnt
                       end
          'intensity': begin
                         idxs = where(curCnt ne 0)
                         if (idxs[0] ne -1) then curData[idxs] = curData[idxs] / curCnt[idxs]
                         idxs = where(valPos eq 0)
                         if (idxs[0] ne -1) then curData[idxs] = sResponse_IP.missingVal
                         aDataArray[*, *, segIdx] = curData
                       end
          'duration': begin 
                        idxs = where(valPos eq 0)
                        if (idxs[0] ne -1) then curCnt[idxs] = sResponse_IP.missingVal
                        aDataArray[*, *, segIdx] = curCnt
                      end
        else: self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        endcase
      endif else begin ; data processing for station data
        sExtra = { aStNames : sResponse.aNames, aStCodes : sResponse.aCodes }
        for i = 0, n_elements(sResponse_IP.aTimes) - 1 do begin
          aIPreGrid = sResponse_IP.aData[*, i]
          aBPreGrid = sResponse_BP.aData[*, i]
          missingVal = sResponse_IP.missingVal 
          if (i eq 0) then begin
            valPos = fltarr((size(aIPreGrid, /dim))[0])
            curData = fltarr((size(aIPreGrid, /dim))[0])
            curCnt = fltarr((size(aIPreGrid, /dim))[0])
            tmpCnt = fltarr((size(aIPreGrid, /dim))[0])
            if (segIdx eq 0) then begin
              excData = fltarr((size(aIPreGrid, /dim))[0], numTimeSeg_IP)
              excCnt = fltarr((size(aIPreGrid, /dim))[0], numTimeSeg_IP)
              aDataArray = fltarr((size(aIPreGrid, /dim))[0], numTimeSeg_IP)
            endif
          endif
          self->Exseedance, aIPreGrid, aBPreGrid, missingVal, calcMode, valPos, tmpCnt, curData, curCnt
        endfor   
        case calcMode of
          'frequency': begin
                         idxs = where(valPos eq 0)
                         if (idxs[0] ne -1) then curCnt[idxs] = sResponse_IP.missingVal
                         aDataArray[*, segIdx] = curCnt
                       end
          'intensity': begin
                         idxs = where(curCnt ne 0)
                         if (idxs[0] ne -1) then curData[idxs] = curData[idxs] / curCnt[idxs]
                         idxs = where(valPos eq 0)
                         if (idxs[0] ne -1) then curData[idxs] = sResponse_IP.missingVal
                         aDataArray[*, segIdx] = curData
                       end
          'duration': begin
                        idxs = where(valPos eq 0)
                        if (idxs[0] ne -1) then curCnt[idxs] = sResponse_IP.missingVal
                        aDataArray[*, segIdx] = curCnt
                      end
        else: self->printLog, '(cvcCalcTimeMean) Error! Unknown calculation mode: ', mode
        endcase
      endelse
    endfor
    
    ;    case sResponse.gridType of
    aLons = sResponse_BP.aLons
    aLats = sResponse_BP.aLats
    midSeg = fltarr(numTimeSeg_IP)
    for segIdx = 0, numTimeSeg_IP - 1 do begin
      self->DayMonthYear, asTimeSeg_IP[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
      self->DayMonthYear, asTimeSeg_IP[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
      reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
      midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
    endfor

    ; put result
    ; output we want to write to, let's get the first one
    outIdx = 0    
    
    res = (aoOutputs[outIdx])->Put(aDataArray)
    if (self->Assert(res)) then return, res
    res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse_BP.missingVal, GRIDTYPE=sResponse_BP.gridType, EXTRA=sExtra)

    return, self.ERROR_OK

END
;--------------------------------------------------------------------
PRO cvcCalcLowExceed__define

    struct = { cvcCalcLowExceed, $
               INHERITS cvcCalc $ 
             }
END
