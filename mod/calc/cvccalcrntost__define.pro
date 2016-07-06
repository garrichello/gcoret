;
;  NAME: 
;    cvcCalcRNtoST
;
;  DESCRIPTION:
;    Interpolating regular reanalysis grid to irregular stations grid using various interpolation techniques
;
;  PURPOSE:
;    Sample class. Implements calculation of a spatial field of trend for time series of data
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcRNtoST = obj_new('cvcCalcRNtoST', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcRNtoST->Run()
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
FUNCTION cvcCalcRNtoST::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcRNtoST::Cleanup

    self->cvcCalc::Cleanup

END
;--------------------------------------------------------------------
FUNCTION cvcCalcRNtoST::Run
    
    self->printLog, "(cvcCalcRNtoST::Run) Started..."
    
    ; get inputs
    res = self->GetInputs(aoInputs, nData)
    if (nData eq 0) then return, self->SetErrMsg("No module inputs!")

    ; get outputs
    res = self->GetOutputs(aoOutputs, nOutputs)
    if (nOutputs eq 0) then return, self->SetErrMsg("No module outputs!")

    ; get parameters
    res = self->GetParams(aoParams, nParams)
       
    if (nParams ne 0) then begin
      res = aoParams[0]->Get(UID='interpol', sParam)
      calcMode2 = *(sParam.data)
      if (self->Assert(res)) then return, res
    endif else calcMode2 = 'bilinear'

    ; input we want to process, let's get the first one
    dataIdx = 0    
    
    ; get info about number of levels and time segments
    retCode_RN = (aoInputs[0])->GetInfo(OUT_NTIMESEG=numTimeSeg_RN, OUT_TIMESEGS=asTimeSeg_RN, OUT_NLEVELS=numLevels_RN, OUT_LEVELS=aLevel_RN, OUT_ROI=sROI_RN)
    if (self->Assert(retCode_RN)) then return, retCode_RN
    retCode_ST = (aoInputs[1])->GetInfo(OUT_NTIMESEG=numTimeSeg_ST, OUT_TIMESEGS=asTimeSeg_ST, OUT_NLEVELS=numLevels_ST, OUT_LEVELS=aLevel_ST, OUT_ROI=sROI_ST)
    if (self->Assert(retCode_ST)) then return, retCode_ST
    
    ; station coordinates to read
    resultCode = (aoInputs[1])->Get(Time=asTimeSeg_ST[0], Level=aLevel_ST[0], Region=sROI_ST, sResponse_ST) 
    if (self->Assert(resultCode)) then return, resultCode

    ; level to read
    levelIdx = 0
    
    ; main loop for all time segments
    self->printLog, "(cvcCalcRNtoST::Run) Get data..."
    cnt = 0 ; number of summed fields

    ; time segment to read
    segIdx = 0
    
    for segIdx = 0, numTimeSeg_RN - 1 do begin
      self->printLog, 'Segment = ', segIdx
      resultCode = (aoInputs[0])->Get(Time=asTimeSeg_RN[segIdx], Level=aLevel_RN[levelIdx], Region=sROI_RN, sResponse_RN)  
      if (self->Assert(resultCode)) then return, resultCode
      if (segIdx eq 0) then begin
        sz_RN = size(sResponse_RN.aData)
        intRNArray = fltarr(size(sResponse_ST.aData[*, 0], /dim), numTimeSeg_RN)
      endif
      xST = sResponse_ST.alons
      yST = sResponse_ST.aLats
      xRq = sResponse_RN.alons
      yRq = sResponse_RN.aLats
      xUnitVector = replicate(1.0, (size(sResponse_RN.aData))[1])
      yUnitVector = replicate(1.0, (size(sResponse_RN.aData))[2])
      xRN = xRq # yUnitVector
      yRN = xUnitVector # yRq
      validx = where(sResponse_RN.adata ne sResponse_RN.MissingVal)
      for iPnt = 0, n_elements(xST) - 1 do begin
        case calcMode2 of
          'bilinear': begin
                        triangulate, xRN[validx], yRN[validx], tr
                        intRNArray[iPnt, segIdx] = griddata(xRN[validx], yRN[validx], sResponse_RN.aData[validx], /linear, triangles = tr, xout = [xST[iPnt]], yout = [yST[iPnt]])
                       end
          'invdistance': intRNArray[iPnt, segIdx] = griddata(xRN[validx], yRN[validx], sResponse_RN.aData[validx], /inverse_distance, xout = [xST[iPnt]], yout = [yST[iPnt]], power = 2)
          'nearneighbor': begin
                            triangulate, xRN[validx], yRN[validx], tr
                            intRNArray[iPnt, segIdx] = griddata(xRN[validx], yRN[validx], sResponse_RN.aData[validx], /nearest_neighbor, triangles = tr, xout = [xST[iPnt]], yout = [yST[iPnt]])
                          end
          'shepard': begin
                       triangulate, xRN[validx], yRN[validx], tr
                       intRNArray[iPnt, segIdx] = griddata(xRN[validx], yRN[validx], sResponse_RN.aData[validx], /shepards, triangle = tr, xout = [xST[iPnt]], yout = [yST[iPnt]])
                     end
          'kriging': intRNArray[iPnt, segIdx] = griddata(xRN[validx], yRN[validx], sResponse_RN.aData[validx], /kriging, xout = [xST[iPnt]], yout = [yST[iPnt]])
        else: self->printLog, '(cvcCalcRNtoST) Error! Unknown calculation mode: ', calcMode2
        endcase  
      endfor
    endfor
    aDataArray = intRNArray
  
;    case sResponse.gridType of
     aLons = sResponse_ST.aLons
     aLats = sResponse_ST.aLats
     midSeg = fltarr(numTimeSeg_RN)
     for segIdx = 0, numTimeSeg_RN - 1 do begin
       self->DayMonthYear, asTimeSeg_RN[segIdx].beginning, reqYear, reqMonth, reqDay, reqHour
       reqTimeJD_0 = julday(reqMonth, reqDay, reqYear, reqHour)
       self->DayMonthYear, asTimeSeg_RN[segIdx].ending, reqYear, reqMonth, reqDay, reqHour
       reqTimeJD_1 = julday(reqMonth, reqDay, reqYear, reqHour)
       midSeg[segIdx] = reqTimeJD_0 + (long(reqTimeJD_1) - long(reqTimeJD_0))/2
     endfor
    
    ; station names
    sExtra = { aStNames : sResponse_ST.aNames, aStCodes : sResponse_ST.aCodes } 
    
    ; put result
    ; output we want to write to, let's get the first one
     outIdx = 0
 
     res = (aoOutputs[outIdx])->Put(aDataArray)
     if (self->Assert(res)) then return, res
     res = (aoOutputs[outIdx])->SetInfo(XGRID=aLons, YGRID=aLats, timeGRID = midSeg, MISSING=sResponse_ST.missingVal, GRIDTYPE=sResponse_ST.gridType, EXTRA=sExtra)
     if (self->Assert(res)) then return, res

     return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcRNtoST__define

    struct = { cvcCalcRNtoST, $
               INHERITS cvcCalc $ 
             }
END