;
;  NAME: 
;    cvcDataStations600
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
FUNCTION cvcDataStations600::Init, in_sDatasetDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataStations600::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
;+
; :Description:
;    Reads data array from 600 stations dataset. Accepts time range.
;
; :Params:
;   in_pFile - pointer to an array of pointers to structures of file informations
;   in_sRequest - structure:
;     sTimeRng - structure of type structTimeSeg - time range to read
;     lev - level
;     varName - variable name
;   out_sData - structure:
;     aData - data array of size n_elements(aLons) x n_elements(aLats) x number of time steps
;     aLons - array of longitudes
;     aLats - array of latitudes
;     minVal - minimum value in aData
;     maxVal - maximum value in aData
;     missingVal - value in aData which means missing data
;     sTimeRng - time range
;     aLev - array of levels (only one element)
;     nLev - number of levels (always 1)
;     resultCode - error code (ERROR_OK if no errors)
;
; :Author: garry
;-
FUNCTION cvcDataStations600::Read, in_aArea, in_aTimeRng, in_level, out_sData

    missingVal = -999.0

    asCurFileInfo = *(*self.pDataFile).pInfo

    aLats = asCurFileInfo.attribute_3
    aLons = asCurFileInfo.attribute_4

    ; borders of the requested area
    minLon = min(in_aArea[0, *], max = maxLon)
    minLat = min(in_aArea[1, *], max = maxLat)

    lonStart = minLon
    lonEnd = maxLon
    latStart = minLat
    latEnd = maxLat

    idxsPos = where((aLats ge latStart) and (aLats le latEnd) and $
                 (aLons ge lonStart) and (aLons le lonEnd))

    dataDir = file_dirname((*self.pDataFile).name)
    aDataFile = dataDir[0] + "/" + asCurFileInfo[idxsPos].attribute_1 + ".dbf"

    ; define data array dimensions
    nx = n_elements(idxsPos)
    reqTimeJD = lonarr(2)
    self->DayMonthYear, in_aTimeRng[0], reqYear, reqMonth, reqDay, reqHour
    reqTimeJD[0] = julday(reqMonth, reqDay, reqYear, reqHour)
    self->DayMonthYear, in_aTimeRng[1], reqYear, reqMonth, reqDay, reqHour
    reqTimeJD[1] = julday(reqMonth, reqDay, reqYear, reqHour)
    ny = (reqTimeJD[1] - reqTimeJD[0])

    aAllData = dblarr(nx, ny)
    aAllData[*] = missingVal

    oDataDbf = obj_new('IDLffShape')
    aTimes = dblarr(nx, ny)
    
    self->printLog, 'Reading...'
    
    nPrSteps = 10
    for i = 0L, nx-1 do begin
;      self->printLog, i
      if ((i mod ((nx-1)/min([nx-1, nPrSteps]))) eq 0) then self->printLog, i*100L/(nx-1), format='("Progress: ", i3, "%")'
      res = oDataDbf->Open(aDataFile[i], /dbf_only)
      saData = oDataDbf->GetAttributes(/all)
      idxsTime = where((saData.attribute_1 ge in_aTimeRng[0]/100) and (saData.attribute_1 le in_aTimeRng[1]/100))
      if (idxsTime[0] eq -1) then begin
        oDataDbf->Close
        continue
      endif
      self->DayMonthYear, saData[idxsTime].attribute_1*100, yr, mo, dy, hr
      aDataJD = long(julday(mo, dy, yr, hr))
      pos = aDataJD - reqTimeJD[0]
      aAllData[i, pos] = saData[idxsTime].attribute_3
      aTimes[i, *] = reqTimeJD[0] + findgen(ny)
      oDataDbf->Close
    endfor
    obj_destroy, oDataDbf
    
    minVal = min(aAllData, max=maxVal)
    
    out_sData = { aData : aAllData, $
                  aLons : aLons[idxsPos], $
                  aLats : aLats[idxsPos], $
                  aTimes : aTimes, $
                  minVal : minVal, $
                  maxVal : maxVal, $
                  missingVal : missingVal, $
                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                  lev : in_level, $
                  aLev : [in_level], $
                  nLev : 1, $
                  gridType : 'station', $
                  resultCode : self.ERROR_OK $
                }
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataStations600::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

    self.sRequest.varName = in_varName
    self.sRequest.sTimeSeg = in_sTimeSeg ; current time segment, in_sVar.asTimeSeg is ignored
    self.sRequest.aLevel = in_aLevel
    self.sRequest.nLev = n_elements(in_aLevel)
    
    ; set data file name template
    res = self->__SetDataFileTemplate(in_varName, in_sTimeSeg, in_aLevel, aDataFileTemplate)
    if (res ne self.ERROR_OK) then return, res

; determine number of files to open
; WARNING! only the very first element of the templates array is used for determination...
; ... since the only difference between templates is the name of the file for each level...
; ... which SHOULD NOT contain templates!
    self.numberOfDataFiles = 1
    self->DayMonthYear, self.sRequest.sTimeSeg.beginning, yr1, mn1, dy1, hr1
    self->DayMonthYear, self.sRequest.sTimeSeg.ending, yr2, mn2, dy2, hr2
       
    self.pDataFile = ptr_new(make_array(self.sRequest.nLev, self.numberOfDataFiles, value={structDataFile}))
;    yrc = yr1 & mnc = mn1 & dyc = dy1 & hrc = hr1
    jdc = julday(mn1, dy1, yr1, hr1)
    for dataFileIdx = 0, self.numberOfDataFiles - 1 do begin
      for levIdx = 0, self.sRequest.nLev-1 do begin 
        filename = self->__ParseFileTemplate(aDataFileTemplate[levIdx], jdc)
        print, filename
        if ( file_test(filename) ) then begin
          info = self->__QueryDBF(filename)
          (*self.pDataFile)[levIdx, dataFileIdx].pInfo = ptr_new(info, /no_copy)
          (*self.pDataFile)[levIdx, dataFileIdx].name = filename
          (*self.pDataFile)[levIdx, dataFileIdx].type = self.dataFileType
          (*self.pDataFile)[levIdx, dataFileIdx].level = self.sRequest.aLevel[levIdx]
        endif else begin
          (*self.pDataFile)[levIdx, dataFileIdx].pInfo = ptr_new()
          (*self.pDataFile)[levIdx, dataFileIdx].name = filename
          (*self.pDataFile)[levIdx, dataFileIdx].type = 'MISSING'        
          (*self.pDataFile)[levIdx, dataFileIdx].level = self.sRequest.aLevel[levIdx]
        endelse
      endfor
    endfor

    self.prepared = 1
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataStations600__define

    struct = { cvcDataStations600, $
               INHERITS cvcData $
             }
END
