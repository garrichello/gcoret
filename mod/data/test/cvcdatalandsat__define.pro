;
;  NAME: 
;    cvcDataLandsat
;
;  PURPOSE:
;    Low-level class. Provides access to Landsat imagery. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataLandsat::Init, in_sDatasetDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_sDatasetDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataLandsat::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
; Warning!!! Only one file at a time is read and returned
;
; Arguments:
;   in_level is ignored
;   in_aTimeRng is ignored
;--------------------------------------------------------------------
FUNCTION cvcDataLandsat::Read, in_aArea, in_aTimeRng, in_level, out_sData

    if ((*self.pDataFile).type eq 'MISSING') then return, -1 ; skip missing files

    if (self.debugMode) then begin
      self->printLog, "(cvcDataWRF::Read) Reading file: ", (*self.pDataFile).name  
    endif
    
    
    aData = read_tiff((*self.pDataFile).name, geotiff = metadata)
    
    self->DayMonthYear, in_aTimeRng, yr, mn, dy, hr
    aTimes = [julday(mn, dy, yr, hr)]
    minVal = min(aData, max = maxVal)

; ToDo: Here it is necessary to insert a function call for missing-valueing points-outlayers of the given area  
    
    out_sData = { aData : aData, $
                  aLons : '', $
                  aLats : '', $
                  aTimes : aTimes, $
                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                  lev : in_level, $
                  minVal : MinVal, $
                  maxVal : MaxVal, $
                  missingVal : -999.0, $
                  gridType : 'regular', $
                  aLev : [in_level], $
                  nLev : 1, $
                  meta : metadata, $
                  resultCode : self.ERROR_OK $
                }
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataLandsat::__ParseFileTemplate, in_fileNameTemplate, in_jd, in_tile

;  aPatternName = ['day2dig', 'month2dig', 'month3small', 'month3cap', 'month3big', 'year2dig', 'year4dig', 'hour2dig']
;  aDateFormat = ['(C(CDI2.2))', '(C(CMOI2.2))', '(C(CmoA))', '(C(CMoA))', '(C(CMOA))', '(C(CYI2.2))', '(C(CYI4.4))', '(C(CHI2.2))']
;  nPattern = n_elements(aPatternName)
;
  fileName = in_fileNameTemplate
;  
;  for patternIdx=0, nPattern-1 do begin
;    pattern = self.oConst->Get(aPatternName[patternIdx])
;    pos = strpos(fileName, pattern)
;    while (pos ne -1) do begin
;      fileName = strmid(fileName, 0, pos) + string(in_jd, format=aDateFormat[patternIdx]) + strmid(fileName, pos+strlen(pattern))
;      pos = strpos(fileName, pattern)
;    endwhile
;  endfor

  ; call parent method to process basic templates
  fileName = self->cvcData::__ParseFileTemplate(fileName, in_jd)

  ; incept path
  pattern = self.oConst->Get('path2dig')
  pos = strpos(fileName, pattern)
  while (pos ne -1) do begin
    fileName = strmid(fileName, 0, pos) + string(in_tile.path, format='(I3.3)') + strmid(fileName, pos+strlen(pattern))
    pos = strpos(fileName, pattern)
  endwhile
  ; incept row
  pattern = self.oConst->Get('row2dig')
  pos = strpos(fileName, pattern)
  while (pos ne -1) do begin
    fileName = strmid(fileName, 0, pos) + string(in_tile.row, format='(I3.3)') + strmid(fileName, pos+strlen(pattern))
    pos = strpos(fileName, pattern)
  endwhile
    
  return, fileName
END
;--------------------------------------------------------------------
FUNCTION cvcDataLandsat::Prepare, in_varName, in_sTimeSeg, in_aLevel, sRegion

    self.sRequest.varName = in_varName
    self.sRequest.sTimeSeg = in_sTimeSeg ; current time segment, in_sVar.asTimeSeg is ignored
    self.sRequest.sRegion = sRegion
    nTiles = n_elements(sRegion.pTiles)
    
    ; set data file name template
    res = self->__SetDataFileTemplate(in_varName, in_sTimeSeg, in_aLevel, aDataFileTemplate)
    if (res ne self.ERROR_OK) then return, res

; determine number of files to open
    self->DayMonthYear, in_sTimeSeg.beginning, yr1, mn1, dy1, hr1
    self->DayMonthYear, in_sTimeSeg.ending, yr2, mn2, dy2, hr2
    nTimes = julday(mn2, dy2, yr2) - julday(mn1, dy1, yr1) + 1
;    self.numberOfDataFiles = nTimes * nTiles
;       
;    self.pDataFile = ptr_new(make_array(nTiles, nTimes, value={structDataFile}))
;;    yrc = yr1 & mnc = mn1 & dyc = dy1 & hrc = hr1
;    jdc = julday(mn1, dy1, yr1, hr1)
;    for timeIdx = 0, nTimes-1 do begin
;      for tileIdx = 0, nTiles-1 do begin 
;        filename = self->__ParseFileTemplate(aDataFileTemplate, jdc, (*sRegion.pTiles)[tileIdx])
;        if ( file_test(filename) ) then begin
;          (*self.pDataFile)[tileIdx, timeIdx].pInfo = ptr_new()
;          (*self.pDataFile)[tileIdx, timeIdx].name = filename
;          (*self.pDataFile)[tileIdx, timeIdx].type = self.dataFileType
;          (*self.pDataFile)[tileIdx, timeIdx].level = in_aLevel[0]
;        endif else begin
;          (*self.pDataFile)[tileIdx, timeIdx].pInfo = ptr_new()
;          (*self.pDataFile)[tileIdx, timeIdx].name = filename
;          (*self.pDataFile)[tileIdx, timeIdx].type = 'MISSING'        
;          (*self.pDataFile)[tileIdx, timeIdx].level = in_aLevel[0]
;        endelse
;      endfor
;      self->IncDay, jdc
;    endfor

    self.numberOfDataFiles = 1
    self.pDataFile = ptr_new({structDataFile})
    jdc = julday(mn1, dy1, yr1, hr1)
    filename = self->__ParseFileTemplate(aDataFileTemplate, jdc, (*sRegion.pTiles)[0])
    res = query_tiff(filename, sInfo, geotiff=sGeoInfo)
    (*self.pDataFile).pInfo = ptr_new(sGeoInfo)
    (*self.pDataFile).name = filename
    (*self.pDataFile).level = in_aLevel[0]
    if ( file_test(filename) ) then begin
      (*self.pDataFile).type = self.dataFileType
    endif else begin
      (*self.pDataFile).type = 'MISSING'        
    endelse

    self.prepared = 1
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataLandsat__define

    struct = { cvcDataLandsat, $
               INHERITS cvcData $
             }
END
