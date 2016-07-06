;
;  NAME: 
;    cvcDataNCEP1
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
FUNCTION cvcDataNCEP1::Init, in_sDatasetDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataNCEP1::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcDataNCEP1::Name2Level, in_levelName
  level = -1000
  
  case in_levelName of
    '2m': level = 2
    '10m': level = 10
    'sfc': level = 0
    'msl': level = 0
    '1000mb': level = 1000
    '925mb': level = 925
    '850mb': level = 850
    '700mb': level = 700
    '600mb': level = 600
    '500mb': level = 500
    '400mb': level = 400
    '300mb': level = 300
    '250mb': level = 250
    '150mb': level = 150
    '100mb': level = 100
    '70mb': level = 70
    '50mb': level = 50
    '30mb': level = 30
    '20mb': level = 20
    '10mb': level = 10
  endcase
  
  return, level
END
;--------------------------------------------------------------------
;+
; :Description:
;    Reads data array from NCEP1-file. Accepts time range.
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
FUNCTION cvcDataNCEP1::Read, in_aArea, in_aTimeRng, in_level, out_sData
;in_pFile, in_sRequest, out_sData

    globalMinVal = 1e30
    globalMaxVal = -1e30
    levIdx = where(self.sRequest.aLevel eq in_level)
    if (levIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGLEV)

    for fileIdx = 0, self.numberOfDataFiles-1 do begin
      asCurFileInfo = *(*self.pDataFile)[levIdx, fileIdx].pInfo
      if ((in_aTimeRng[1] ge asCurFileInfo[0].info.date) and $
          (in_aTimeRng[0] le asCurFileInfo[n_elements(asCurFileInfo)-1].info.date)) then begin

        if (self.debugMode) then begin
          self->printLog, "(cvcDataNCEP1::Read) Reading file: ", (*self.pDataFile)[levIdx, fileIdx].name  
          self->printLog, "(cvcDataNCEP1::Read) Requesting variable: ", self.sRequest.varName, ", level: ", in_level, ", time: ", in_aTimeRng
        endif
      
        recId = where((asCurFileInfo.info.quantity eq self.sRequest.varName) and $
                    (asCurFileInfo.info.date ge in_aTimeRng[0]) and $
                    (asCurFileInfo.info.date le in_aTimeRng[1]) and $
                    (asCurFileInfo.info.kdps7 eq self->Name2Level(in_level)))    
      
        if (recId[0] ne -1) then begin
          aData = read_grib_record((*self.pDataFile)[levIdx, fileIdx].name, recId+1) ; +1 because record numbering starts from 1
          globalMinVal = min([globalMinVal, asCurFileInfo[recId[0]].info.minmaxdata[0]])
          globalMaxVal = max([globalMaxVal, asCurFileInfo[recId[0]].info.minmaxdata[1]])
          aTimesRaw = asCurFileInfo[recId].info.date
        endif else begin
          return, self->SayError(self.ERROR_BADREQ, 'cvcData::ReadNCEP1')
        endelse
        if (n_elements(aAllData) ne 0) then begin
          aAllData = [[[temporary(aAllData)]], [[temporary(aData)]]]
          aAllTimesRaw = [temporary(aAllTimesRaw), temporary(aTimesRaw)]
        endif else begin
          aAllData = temporary(aData)
          aAllTimesRaw = temporary(aTimesRaw)
        endelse
      endif
    endfor
      
; create preliminary latitude grid depending on the grid in the grib file
    latGridType = asCurFileInfo[0].info.projectiontype                  
    
    nLons = asCurFileInfo[0].info.nx
    nLats = asCurFileInfo[0].info.ny
    
    aLons = asCurFileInfo[0].projection.lon1-findgen(nLons)*asCurFileInfo[0].projection.lon2 
    if (latGridType eq 'GAUSSIAN') then begin
      aLats = reverse(self.oConst->Get('T62'))
    endif else begin
      aLats = asCurFileInfo[0].projection.lat1+findgen(nLats)*(asCurFileInfo[0].projection.lat2 - asCurFileInfo[0].projection.lat1)/(nLats-1)
      if (latGridType ne 'LATLON') then self->printLog, "(cvcData::ReadNCEP1) Warning: Unknown latitude grid type! Using default!"
    endelse

    ; borders of the requested area
;    minLon = min(in_aArea[0, *], max = maxLon)
;    minLat = min(in_aArea[1, *], max = maxLat)

;    ; reverse longitudes from -180-0-180 to 0-360 grid
;    in_aAreaRev = in_aArea
;    in_aAreaRev[0, *] = (in_aAreaRev[0, *] + 360.) mod 360.
;    minLonRev = (minLon + 360.) mod 360.
;    maxLonRev = (maxLon + 360.) mod 360.
;    
;    ; position of the requested area in the global grid
;    lonStart = max([(where(aLons gt minLonRev))[0]-1, 0])
;    lonEnd = min([(where(aLons lt maxLonRev, cnt))[cnt-1]+1, nLons-1])
;    latStart = max([(where(aLats lt maxLat))[0]-1, 0])
;    latEnd = min([(where(aLats gt minLat, cnt))[cnt-1]+1, nLats-1])
;    
;    if (((minLon lt 0) and (maxLon gt 0)) or ((minLon gt 0) and (maxLon lt 0))) then begin  ; we have 0-meridian inside requested area
;      aSubData = [aAllData[lonStart:nLons-1, latStart:latEnd, *], aAllData[0:lonEnd, latStart:latEnd, *]]
;      aSubLons = [aLons[lonStart:nLons-1], aLons[0:lonEnd]] 
;    endif else begin
;       aSubData = aAllData[lonStart:lonEnd, latStart:latEnd, *]
;       aSubLons = aLons[lonStart:lonEnd]
;    endelse
;    
;    aSubLons = ((aSubLons + 180.) mod 360) - 180.
;    nLons = n_elements(aSubLons)
;    if (aSubLons[nLons-1] eq -180.) then aSubLons[nLons-1] = 180. 
;    aSubLats = aLats[latStart:latEnd]

    ; fix missing values
    missingVal = -999.0
    idxs = where(aAllData eq -9.99E33)
    if (idxs[0] ne -1) then aAllData[idxs] = missingVal

    res = self->CutROI(self.sRequest.sRegion.lon0, in_aArea, aLons, aLats, missingVal, aAllData)

    goodIdxs = where(aAllData ne missingVal)
    if (goodIdxs[0] ne -1) then minVal = min(aAllData[goodIdxs], max = maxVal)
    
    ; convert raw time into JD
    self->DayMonthYear, aAllTimesRaw, yr, mo, dy, hr
    aTimes = julday(mo, dy, yr, hr)

    out_sData = { aData : aAllData, $
                  aLons : aLons, $
                  aLats : aLats, $
                  aTimes : aTimes, $
                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                  lev : in_level, $
                  minVal : minVal, $
                  maxVal : maxVal, $
                  missingVal : missingVal, $
                  gridType : 'regular', $
                  aLev : [in_level], $
                  nLev : 1, $
                  resultCode : self.ERROR_OK $
                }
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataNCEP1::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_sRegion

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
    self->DayMonthYear, in_sTimeSeg.beginning, yr1, mn1, dy1, hr1
    self->DayMonthYear, in_sTimeSeg.ending, yr2, mn2, dy2, hr2
    self.numberOfDataFiles = yr2 - yr1 + 1
    
    self->printLog, 'GRIB_API is barely implemented in GDL and not supported by now. May be it would in the future. Aborting...'
    return, -1 
       
    self.pDataFile = ptr_new(make_array(self.sRequest.nLev, self.numberOfDataFiles, value={structDataFile}))
;    yrc = yr1 & mnc = mn1 & dyc = dy1 & hrc = hr1
    jdc = julday(mn1, dy1, yr1, hr1)
    for dataFileIdx = 0, self.numberOfDataFiles - 1 do begin
      for levIdx = 0, self.sRequest.nLev-1 do begin 
        filename = self->__ParseFileTemplate(aDataFileTemplate[levIdx], jdc)
        if ( file_test(filename) ) then begin
;          if ( file_test(filename+'.info')) then begin
;            restore, filename=filename+'.info' ; fast loading of info-structure
;          endif else begin
            self->printLog, "(cvcData::Prepare) Warning! Reading info from GRIB-file for the first time. Now caching. Next time will be much faster."
            res = query_grib_file(filename, info, /full)
; TODO: check 'res' variable for error codes and break execution if there are any
;            save, filename=filename+'.info', info ; storing of info-struture for the fast loading next time
;          endelse
          (*self.pDataFile)[levIdx, dataFileIdx].pInfo = ptr_new(info, /no_copy)
          (*self.pDataFile)[levIdx, dataFileIdx].name = filename
          (*self.pDataFile)[levIdx, dataFileIdx].type = self.dataFileType
          (*self.pDataFile)[levIdx, dataFileIdx].level = self.sRequest.aLevel[levIdx]
        endif else begin
          self->printLog, 'Warning: Could not find file '+filename
          (*self.pDataFile)[levIdx, dataFileIdx].pInfo = ptr_new()
          (*self.pDataFile)[levIdx, dataFileIdx].name = filename
          (*self.pDataFile)[levIdx, dataFileIdx].type = 'MISSING'        
          (*self.pDataFile)[levIdx, dataFileIdx].level = self.sRequest.aLevel[levIdx]
        endelse
      endfor
      self->IncYear, jdc
    endfor

    self.prepared = 1
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataNCEP1__define

    struct = { cvcDataNCEP1, $
               INHERITS cvcData $
             }
END