;
;  NAME: 
;    cvcDataArray
;
;  PURPOSE:
;    Low-level class. Provides access to data array in memory. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataArray::Init, in_pDataDesc, in_modulePath, in_oGlobal

    if ( self->cvcData::Init(in_pDataDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataArray::Cleanup
    
    self->cvcData::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcDataArray::Read, in_aArea, in_aTimeRng, in_level, out_sData

  self->printLog, '(cvcDataArray) Reading from an array...'

  sData = *self.pDataFile

  if(sData.gridType ne 'station') then begin ; gridded data
   gridType = 'regular' 
   
self->PrintLog, '(cvcDataArray) Data not from stations...'  

    gridFlag = [0, 0, 0, 0] ; [x, y, z, t] grid presence
  
; check level grid
    if (ptr_valid(sData.pZgrid)) then begin ; if data array has a level grid
      gridFlag[2] = 1
      if (in_level ne '') then ZIdx = where(*sData.pZgrid eq in_level) else ZIdx = indgen(n_elements(*sData.pZgrid))
      if (ZIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGLEV)
    endif

; check time grid
    if (ptr_valid(sData.pTimeGrid)) then begin ; if data array has a time grid
self->printLog, '(cvcDataArray) Data array has a time grid'
      gridFlag[3] = 1
;      actualDateRangeJD[0] = min(*sData.pTimeGrid, max = actualDateRangeJD[1])

      reqTimeJD = dblarr(2)
      if ((in_aTimeRng[0] ne 0) and (in_aTimeRng[1] ne 0)) then begin ; if we have time limits
        self->DayMonthYear, in_aTimeRng[0], reqYear, reqMonth, reqDay, reqHour
        reqTimeJD[0] = julday(reqMonth, reqDay, reqYear, reqHour)
        self->DayMonthYear, in_aTimeRng[1], reqYear, reqMonth, reqDay, reqHour
        reqTimeJD[1] = julday(reqMonth, reqDay, reqYear, reqHour)

        timeIdx = where((*sData.pTimeGrid ge reqTimeJD[0]) and (*sData.pTimeGrid le reqTimeJD[1]))
      endif else timeIdx = indgen(n_elements(*sData.pTimeGrid))

      if (timeIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGTIME)      
    endif

; check X grid
    if (ptr_valid(sData.pXgrid)) then begin ; if data array has a X grid

self->printLog, '(cvcDataArray) Data array has a X grid'

      gridFlag[0] = 1
      
      nAreaDims = size(in_aArea)
      if (nAreaDims[0] eq 1) then minX = min(in_aArea, max = maxX)
      if (nAreaDims[0] eq 2) then minX = min(in_aArea[0, *], max = maxX)
      
      if (nAreaDims[0] ne 0) then begin
        if (max(*sData.pXgrid) gt 180.) then begin
          tmpXarr = (*sData.pXgrid + 180.) mod 360 - 180.
          XIdx = where((tmpXarr ge minX) and (tmpXarr le maxX))
          tmpXarr = 0
        endif else XIdx = where((*sData.pXgrid ge minX) and (*sData.pXgrid le maxX))
      endif else XIdx = indgen(n_elements(*sData.pXgrid))

      if (XIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGX)      
    endif

; check Y grid
    if (ptr_valid(sData.pYgrid)) then begin ; if data array has a Y grid

self->printLog, '(cvcDataArray) Data array has a Y grid'

      gridFlag[1] = 1
      
      nAreaDims = size(in_aArea)
      if (nAreaDims[0] eq 1) then minY = min(in_aArea, max = maxY)
      if (nAreaDims[0] eq 2) then minY = min(in_aArea[1, *], max = maxY)
      
      if (nAreaDims[0] ne 0) then begin
        YIdx = where((*sData.pYgrid ge minY) and (*sData.pYgrid le maxY))
      endif else YIdx = indgen(n_elements(*sData.pYgrid))

      if (YIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGY)      
    endif

    aSubLons = -1
    aSubLats = -1
    aTimes = -1
    aLev = -1

self->printLog, '(cvcDataArray) Extracting subgrids...', format='(a, $)'

    if (gridFlag[0]) then begin
      aIdx = XIdx & aSubLons = (*sData.pXgrid)[aIdx]
      if (gridFlag[1]) then begin
        bIdx = YIdx & aSubLats = (*sData.pYgrid)[bIdx]
        if (gridFlag[2]) then begin
          cIdx = ZIdx & aLev = (*sData.pZgrid)[cIdx]
          if (gridFlag[3]) then begin
            dIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[dIdx]
          endif
        endif else begin
          if (gridFlag[3]) then begin
            cIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[cIdx]
          endif
        endelse
      endif else begin
        if (gridFlag[2]) then begin
          bIdx = ZIdx & aLev = (*sData.pZgrid)[bIdx]
          if (gridFlag[3]) then begin
            cIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[cIdx]
          endif
        endif else begin
          if (gridFlag[3]) then begin
            bIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[bIdx]
          endif
        endelse
      endelse
    endif else begin
      if (gridFlag[1]) then begin
        aIdx = YIdx & aSubLats = (*sData.pYgrid)[aIdx]
        if (gridFlag[2]) then begin
          bIdx = ZIdx & aLev = (*sData.pZgrid)[bIdx]
          if (gridFlag[3]) then begin
            cIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[cIdx]
          endif
        endif else begin
          if (gridFlag[3]) then begin
            bIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[bIdx]
          endif
        endelse
      endif else begin
        if (gridFlag[2]) then begin
          aIdx = ZIdx & aLev = (*sData.pZgrid)[aIdx]
          if (gridFlag[3]) then begin
            bIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[bIdx]
          endif
        endif else begin
          if (gridFlag[3]) then begin
            aIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[aIdx]
          endif
        endelse
      endelse
    endelse

self->printLog, 'OK!', /NoTimeStamp

self->printLog, '(cvcDataArray) gridFlag: ', gridFlag
    
    case total(gridFlag) of
      1: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1]]    
      2: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1], bIdx[0]:bIdx[n_elements(bIdx)-1]]    
      3: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1], bIdx[0]:bIdx[n_elements(bIdx)-1], cIdx[0]:cIdx[n_elements(cIdx)-1]]    
      4: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1], bIdx[0]:bIdx[n_elements(bIdx)-1], cIdx[0]:cIdx[n_elements(cIdx)-1], dIdx[0]:dIdx[n_elements(dIdx)-1]]
      else: begin
        self->PrintLog, '(cvcDataArray) Bad gridFlag. Aborting...'
        out_sData = 0
        return, -1
      end
    endcase
  endif else begin ; if station data

self->printLog, '(cvcDataArray) Data from stations'

    gridType = 'station'
  
    gridFlag = [0, 0, 0] ; [s, z, t] grid presence (station, height, time)
  
; check level grid
    if (ptr_valid(sData.pZgrid)) then begin ; if data array has a level grid
      gridFlag[1] = 1
      if (in_level ne '') then ZIdx = where(*sData.pZgrid eq in_level) else ZIdx = indgen(n_elements(*sData.pZgrid))
      if (ZIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGLEV)
    endif

; check time grid
    if (ptr_valid(sData.pTimeGrid)) then begin ; if data array has a time grid

self->printLog, '(cvcDataArray) Data array has a time grid'

      gridFlag[2] = 1
;
      reqTimeJD = dblarr(2)
      if ((in_aTimeRng[0] ne 0) and (in_aTimeRng[1] ne 0)) then begin ; if we have time limits
        self->DayMonthYear, in_aTimeRng[0], reqYear, reqMonth, reqDay, reqHour
        reqTimeJD[0] = julday(reqMonth, reqDay, reqYear, reqHour)
        self->DayMonthYear, in_aTimeRng[1], reqYear, reqMonth, reqDay, reqHour
        reqTimeJD[1] = julday(reqMonth, reqDay, reqYear, reqHour)

        timeIdx = where((*sData.pTimeGrid ge reqTimeJD[0]) and (*sData.pTimeGrid le reqTimeJD[1]))
      endif else timeIdx = indgen(n_elements(*sData.pTimeGrid))
      
      if (timeIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGTIME)      
    endif

; check stations
    if (ptr_valid(sData.pXgrid) and ptr_valid(sData.pYgrid)) then begin ; if data array has a X AND Y grid

self->printLog, '(cvcDataArray) Data array has a X AND Y grid'

      gridFlag[0] = 1
      
      nAreaDims = size(in_aArea)
      if (nAreaDims[0] eq 1) then begin
        minX = min(in_aArea, max = maxX)
        minY = min(in_aArea, max = maxY)
      endif
      if (nAreaDims[0] eq 2) then begin
        minX = min(in_aArea[0, *], max = maxX)
        minY = min(in_aArea[1, *], max = maxY)
      endif
      
      if (nAreaDims[0] ne 0) then begin
        if (max(*sData.pXgrid) gt 180.) then begin
          tmpXarr = (*sData.pXgrid + 180.) mod 360 - 180.
          StIdx = where((tmpXarr ge minX) and (tmpXarr le maxX) and (*sData.pYgrid ge minY) and (*sData.pYgrid le maxY))
          tmpXarr = 0
        endif else StIdx = where((*sData.pXgrid ge minX) and (*sData.pXgrid le maxX) and (*sData.pYgrid ge minY) and (*sData.pYgrid le maxY))
      endif else StIdx = indgen(n_elements(*sData.pXgrid)) ; here we suppose each station has x and y coordinates :) 

      if (StIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGX)      
    endif

    aSubLons = -1
    aSubLats = -1
    aTimes = -1
    aLev = -1

self->printLog, '(cvcDataArray) Extracting subgrids...', format='(a, $)'

    if (gridFlag[0]) then begin
      aIdx = StIdx & aSubLons = (*sData.pXgrid)[aIdx] & aSubLats = (*sData.pYgrid)[aIdx]
      if (gridFlag[1]) then begin
        bIdx = ZIdx & aLev = (*sData.pZgrid)[bIdx]
        if (gridFlag[2]) then begin
          cIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[cIdx]
        endif
      endif else begin
        if (gridFlag[2]) then begin
          bIdx = TimeIdx & aTimes = (*sData.pTimeGrid)[bIdx]
        endif
      endelse
    endif else begin
      self->printLog, 'Error! No Lon-Lat data for stations! Aborting...'
      out_sData = 0
      return, -1
    endelse
    
self->printLog, 'OK!', /notimestamp

self->printLog, '(cvcDataArray) gridFlag: ', gridFlag

    case total(gridFlag) of
      1: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1]]    
      2: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1], bIdx[0]:bIdx[n_elements(bIdx)-1]]    
      3: aSubData = (*sData.data)[aIdx[0]:aIdx[n_elements(aIdx)-1], bIdx[0]:bIdx[n_elements(bIdx)-1], cIdx[0]:cIdx[n_elements(cIdx)-1]]    
      else: begin
        self->PrintLog, '(cvcDataArray) Bad gridFlag. Aborting...'
        out_sData = 0
        return, -1
      end
    endcase
  endelse        
    
  missingVal = sData.missingVal
  minVal = min(aSubData[where(aSubData ne missingVal)], max = maxVal)

; ToDo: Here it is necessary to insert a function call for missing-valueing points-outlayers of the given area  
   
  out_sData = { aData : temporary(aSubData), $
                aLons : aSubLons, $
                aLats : aSubLats, $
                aTimes : aTimes , $
                sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                lev : in_level, $
                minVal : MinVal, $
                maxVal : MaxVal, $
                missingVal : missingVal, $
                gridType : gridType, $
                aLev : aLev, $
                nLev : 1, $
                resultCode : self.ERROR_OK $
              }
  
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataArray::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_pData

    self.sRequest.varName = in_varName
    self.sRequest.sTimeSeg = in_sTimeSeg ; current time segment, in_sVar.asTimeSeg is ignored
    self.sRequest.aLevel = in_aLevel
    self.sRequest.nLev = n_elements(in_aLevel)

    self.pDataFile = in_pData 

    self.prepared = 1
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataArray__define

    struct = { cvcDataArray, $
               INHERITS cvcData $
             }
END
