;
;  NAME: 
;    cvcDataWRF
;
;  PURPOSE:
;    Low-level class. Provides access to meteo- and climatic data. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataWRF::Init, in_sDatasetDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataWRF::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
;+
; :Description:
;    Reads data array from WRF-file. Accepts time range.
;
; :Author: garry
;-
FUNCTION cvcDataWRF::Read, in_aArea, in_aTimeRng, in_level, out_sData

    res = self->__ReadNCDF(in_aArea, in_aTimeRng, in_level, out_sData)
    out_sData.gridType = 'irregular'

    return, res
END
;FUNCTION cvcDataWRF::Read, in_aArea, in_aTimeRng, in_level, out_sData
;
;    levPos = where(self.sRequest.aLevel eq in_level)
;    if (levPos[0] eq -1) then return, self->SayError(self.ERROR_WRONGLEV)
;    
;    reqTimeJD = dblarr(2)
;    self->DayMonthYear, in_aTimeRng[0], reqYear, reqMonth, reqDay, reqHour
;    reqTimeJD[0] = julday(reqMonth, reqDay, reqYear, reqHour)
;    self->DayMonthYear, in_aTimeRng[1], reqYear, reqMonth, reqDay, reqHour
;    reqTimeJD[1] = julday(reqMonth, reqDay, reqYear, reqHour)
;
;    for fileIdx = 0, self.numberOfDataFiles-1 do begin
;      if ((*self.pDataFile)[levPos, fileIdx].type eq 'MISSING') then continue ; skip missing files
;      asCurFileInfo = *(*self.pDataFile)[levPos, fileIdx].pInfo
;      timeVarId = where(asCurFileInfo.asVar.name eq 'Times')
;      actualDateRangeId = where(asCurFileInfo.asVar[timeVarId].atts.name eq 'actual_range')
;      actualDateRangeJD = strsplit(asCurFileInfo.asVar[timeVarId].atts[actualDateRangeId].value, /extract)
;      timeUnitsId = where(asCurFileInfo.asVar[timeVarId].atts.name eq 'units') ; !!! timeUnitsId = 'units'
;      if ((reqTimeJD[1] ge actualDateRangeJD[0]) and (reqTimeJD[0] le actualDateRangeJD[1])) then begin
;
;        if (self.debugMode) then begin
;          self->printLog, "(cvcDataWRF::Read) Reading file: ", (*self.pDataFile)[levPos, fileIdx].name  
;          self->printLog, "(cvcDataWRF::Read) Requesting variable: ", self.sRequest.varName, ", level: ", in_level, ", time: ", in_aTimeRng
;        endif
;        
;        fileId = asCurFileInfo.fileId
;        varId = (where(asCurFileInfo.asVar.name eq self.sRequest.varName))[0]
;        
;        lonVarId = (where(asCurFileInfo.asVar.name eq 'XLONG'))[0]
;        ncdf_varget, fileId, lonVarId, aLons
;        latVarId = (where(asCurFileInfo.asVar.name eq 'XLAT'))[0]    
;        ncdf_varget, fileId, latVarId, aLats
;        timeVarId = (where(asCurFileInfo.asVar.name eq 'Times'))[0]
;        if (timeVarId ne -1) then begin
;          ncdf_varget, fileId, timeVarId, aTime
;; TODO: make conversion of hours to JD more versatile
;          res = self->__ByteStrToDate(aTime, aYr, aMo, aDy, aHr)
;          aTimeJD = julday(aMo, aDy, aYr, aHr)
;          timeIdxs = where((aTimeJD ge reqTimeJD[0]) and (aTimeJD le reqTimeJD[1]))
;          aTimes = aTimeJD[timeIdxs]
;        endif else aTimes = [-1.0]
;        levVarId = (where(asCurFileInfo.asVar.name eq 'bottom_top'))[0] ; level variable = 'bottom_top'
;        if (levVarId ne -1) then begin
;          ncdf_varget, fileId, levVarId, aLevel
;          levIdx = (where(aLevel eq in_level))[0]
;        endif
;    
;        lonDimId = (where(asCurFileInfo.asDim.name eq 'west_east'))[0] ; longitude = 'west_east' 
;        latDimId = (where(asCurFileInfo.asDim.name eq 'south_north'))[0] ; latitude = 'south_north'
;        timeDimId = (where(asCurFileInfo.asDim.name eq 'Time'))[0] ; time = 'Time'
;        levDimId = (where(asCurFileInfo.asDim.name eq 'bottom_top'))[0] ; level dimension = 'bottom_top'
;    
;        lonSize = asCurFileInfo.asDim[lonDimId].size
;        latSize = asCurFileInfo.asDim[latDimId].size
;        
;        lonPos = (where(asCurFileInfo.asVar[varId].dim eq lonDimId))[0]
;        latPos = (where(asCurFileInfo.asVar[varId].dim eq latDimId))[0]
;        levPos = (where(asCurFileInfo.asVar[varId].dim eq levDimId))[0]
;        timePos = (where(asCurFileInfo.asVar[varId].dim eq timeDimId))[0]
;        
;        aCount = replicate(1, asCurFileInfo.asVar[varId].ndims)
;        if (lonPos ne -1) then aCount[lonPos] = lonSize
;        if (latPos ne -1) then aCount[latPos] = latSize
;        if (timePos ne -1) then aCount[timePos] = n_elements(timeIdxs)
;        aOffset = replicate(0, asCurFileInfo.asVar[varId].ndims)
;        if ((levPos ne -1) and (levVarId ne -1)) then aOffset[levPos] = levIdx
;        if (timePos ne -1) then aOffset[timePos] = timeIdxs[0]
;        
;        ncdf_varget, fileId, varId, aData, count=aCount, offset=aOffset
;;        missingVal = asCurFileInfo.asVar[varId].atts[where(asCurFileInfo.asVar[varId].atts.name eq '_FillValue')].value
;;        missingIdxs = where(aData eq missingVal, missingCnt)
;;        if (missingCnt gt 0) then aData[missingIdxs] = missingVal
;        
;        aLons = aLons[*, *, timeIdxs]
;        aLats = aLats[*, *, timeIdxs]
;        dataDims = size(aData, /dimensions)
;        newDims = dataDims[where(dataDims ne 1)]
;        if (n_elements(newDims) ne n_elements(dataDims)) then begin
;          aData = reform(aData, newDims, /overwrite)
;        endif
;        if (n_elements(aAllData)) then begin
;          aAllData = [[[temporary(aAllData)]], [[temporary(aData)]]]
;        endif else aAllData = temporary(aData)
;      endif
;    endfor
;
;;    ; borders of the requested area
;;    minLon = min(in_aArea[0, *], max = maxLon)
;;    minLat = min(in_aArea[1, *], max = maxLat)
;;
;;    ; reverse longitudes from -180-0-180 to 0-360 grid
;;    in_aAreaRev = in_aArea
;;    in_aAreaRev[0, *] = (in_aAreaRev[0, *] + 360.) mod 360.
;;    minLonRev = (minLon + 360.) mod 360.
;;    maxLonRev = (maxLon + 360.) mod 360.
;;    
;;    nLons = n_elements(aLons)
;;    nLats = n_elements(aLats)
;;    
;;    ; position of the requested area in the global grid
;;    lonStart = max([(where(aLons gt minLonRev))[0]-1, 0])
;;    lonEnd = min([(where(aLons lt maxLonRev, cnt))[cnt-1]+1, nLons-1])
;;    latStart = max([(where(aLats lt maxLat))[0]-1, 0])
;;    latEnd = min([(where(aLats gt minLat, cnt))[cnt-1]+1, nLats-1])
;;    
;;    if (((minLon lt 0) and (maxLon gt 0)) or ((minLon gt 0) and (maxLon lt 0))) then begin  ; we have 0-meridian inside requested area
;;      aSubData = [aAllData[lonStart:nLons-1, latStart:latEnd, *], aAllData[0:lonEnd, latStart:latEnd, *]]
;;      aSubLons = [aLons[lonStart:nLons-1], aLons[0:lonEnd]] 
;;    endif else begin
;;       aSubData = aAllData[lonStart:lonEnd, latStart:latEnd, *]
;;       aSubLons = aLons[lonStart:lonEnd]
;;    endelse
;;    
;;    aSubLons = ((aSubLons + 180.) mod 360) - 180.
;;    nLons = n_elements(aSubLons)
;;    if (aSubLons[nLons-1] eq -180.) then aSubLons[nLons-1] = 180. 
;;    aSubLats = aLats[latStart:latEnd]
;;
;    minVal = min(aAllData, max = maxVal)
;
;; ToDo: Here it is necessary to insert a function call for missing-valueing points-outlayers of the given area  
;    
;    out_sData = { aData : aAllData, $
;                  aLons : aLons, $
;                  aLats : aLats, $
;                  aTimes : aTimes, $
;                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
;                  lev : in_level, $
;                  minVal : MinVal, $
;                  maxVal : MaxVal, $
;                  missingVal : -999.0, $
;                  gridType : 'irregular', $
;                  aLev : [in_level], $
;                  nLev : 1, $
;                  resultCode : self.ERROR_OK $
;                }
;    
;    return, self.ERROR_OK
;END
;--------------------------------------------------------------------
FUNCTION cvcDataWRF::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

    res = self->__PrepareNCDF(in_varName, in_sTimeSeg, in_aLevel, in_sRegion)
    
    if (~self->Assert(res)) then self.prepared = 1    
    return, res

END
;--------------------------------------------------------------------
PRO cvcDataWRF__define

    struct = { cvcDataWRF, $
               INHERITS cvcData $
             }
END