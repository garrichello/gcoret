;
;  NAME: 
;    cvcDataNetCDF
;
;  PURPOSE:
;    Base low-level class. Provides access to meteo- and climatic data. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataNetCDF::Init, in_pDataDesc, in_modulePath, in_oGlobal

    if ( self->cvcData::Init(in_pDataDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0

    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataNetCDF::Cleanup

    self->cvcData::Cleanup 
END
;--------------------------------------------------------------------
FUNCTION cvcDataNetCDF::__ByteStrToDate, in_aByteStr, o_aYear, o_aMonth, o_aDay, o_aHour

  o_aMonth = fix(string(in_aByteStr[5:6, *]))
  o_aDay = fix(string(in_aByteStr[8:9, *]))
  o_aYear = fix(string(in_aByteStr[0:3, *]))
  o_aHour = fix(string(in_aByteStr[11:12, *]))  

  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataNetCDF::__ConvertTU2JD, in_timeTU, in_TUV, out_timeJD
    timeUnits = strmid(in_TUV, 0, strpos(in_TUV, 'since')-1) ; we assume timeUnitsValue has form '<timeUnits>
    if (timeUnits ne '') then begin
      timeStart = strmid(in_TUV, strpos(in_TUV, 'since')+6) ; we assume timeStart has form 'year-mo-dy hr:mm:s
      sinceDate = strsplit(timeStart, '- :', /extract)
      nFieldsActual = n_elements(sinceDate)
      if (nFieldsActual lt 4) then begin
        nFieldsToAdd = 4 - nFieldsActual
        addFields = strarr(nFieldsToAdd)
        sinceDate = [sinceDate, addFields]
      endif
      case timeUnits of
        'days':  out_timeJD = julday(sinceDate[1], sinceDate[2], sinceDate[0], sinceDate[3]) + in_timeTU
        'hours': out_timeJD = julday(sinceDate[1], sinceDate[2], sinceDate[0], sinceDate[3] + in_timeTU)
        else: begin
                print, 'cvcDataNetCDF::__ConvertTU2JD: Unknown time units! Execution stopped!'
                stop
              end
      endcase
    endif else begin
      timeUnits = strmid(in_TUV, 0, strpos(in_TUV, 'as')-1) ; we assume timeUnitsValue has form '<timeUnits> a
      if (timeUnits ne '') then begin
        yr = fix(in_TimeTU / 10000)
        mo = fix((in_TimeTU - yr*10000.) / 100)
        dy = fix(in_TimeTU - yr*10000. - mo*100.)
        out_timeJD = julday(mo, dy, yr)
      endif else begin
        print, 'cvcDataNetCDF::__ConvertTU2JD: Unknown time units separator! Execution stopped!'
        stop
      end
    endelse

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataNetCDF::__CutROI, in_lon0, in_aArea, in_aLons, in_aLats, in_missingVal, inout_aData
; cut out selected ROI
    
    if (n_elements(inout_aData) eq 0) then return, -1
    
    dataDims = size(inout_aData, /dimensions)    
    nLons = dataDims[0]
    nLats = dataDims[1]

    if (in_lon0 eq '180') then begin
      in_aArea[0, *] = (in_aArea[0, *] + 360.) mod 360. ; reverse longitudes
      aLonsNew = (in_aLons[*, *, 0] + 360.) mod 360
    endif else begin
      if ((in_aLons[0, 0, 0] lt 0) and (in_aLons[nLons-1, 0, 0] gt 0)) then $
        aLonsNew = in_aLons[*, *, 0] else aLonsNew = (in_aLons[*, *, 0] + 180) mod 360 - 180
    endelse
     
    ; create ROI
    oROI = obj_new('IDLanROI', in_aArea)

    ; create masks    
    if (size(aLonsNew, /n_dim) eq 1) then begin ; for regular grid
      Ilons = replicate(1.0, nLons)
      Ilats = replicate(1.0, nLats)    
      aLonsM = aLonsNew # transpose(Ilats)
      aLatsM = Ilons # transpose(in_aLats)
    endif else begin ; for irregular grid
      aLonsM = aLonsNew
      aLatsM = in_aLats[*, *, 0]
    endelse
self->printLog, 'Creating ROI mask...'
;stop
    mask = reform(oROI->ContainsPoints(aLonsM, aLatsM), nLons, nLats)
    mask = mask ne 0
    maskMiss = (~mask)*in_missingVal
self->printLog, 'OK'
self->printLog, 'Masking outlying data...'
    if (n_elements(dataDims) eq 3) then begin ; for 3-D dataset
      for t = 0, dataDims[2]-1 do begin
        inout_aData[*, *, t] = inout_aData[*, *, t] * mask + maskMiss
      endfor
    endif else begin
      inout_aData = inout_aData * mask + maskMiss
    endelse
self->printLog, 'OK'
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataNetCDF::__QueryNCDF, in_fileName

    MAX_DIMS = 50
    MAX_ATTS = 50
    
    sNCDim = { sNCDim, name : '', size : 0L }
    sNCAtt = { sNCAtt, name : '', datatype : '', length : 0L, value : '' }
    sNCVar = { sNCVar, name : '', datatype : '', ndims : 0L, natts : 0L, $
      dim : lonarr(MAX_DIMS), atts : replicate({sNCAtt}, MAX_ATTS)}
      
    NCFileId = ncdf_open(in_fileName)
    sNCFileInfo = ncdf_inquire(NCFileId)
    timeUnitsValue = ''
; get dimensions    
    asNCDim = replicate({sNCDim}, sNCFileInfo.NDIMS)
    for dimId = 0, sNCFileInfo.NDIMS-1 do begin
      ncdf_diminq, NCFileId, dimID, name, size
      asNCDim[dimId].name = name
      asNCDim[dimId].size = size
    endfor

; get global attributes
    if (sNCFileInfo.NGATTS gt 0) then begin
      asNCGAtt = replicate(sNCAtt, sNCFileInfo.NGATTS)
      for gattId = 0, sNCFileInfo.NGATTS-1 do begin
        asNCGAtt[gattId].name = ncdf_attname(NCFileId, gattId, /global)
        sRes = ncdf_attinq(NCFileId, asNCGAtt[gattId].name, /global)
        asNCGAtt[gattId].datatype = sRes.datatype
        asNCGAtt[gattId].length = sRes.length
        ncdf_attget, NCFileId, asNCGAtt[gattId].name, value, /global
        asNCGAtt[gattId].value = strjoin(string(value))
      endfor
    endif else asNCGAtt = {sNCAtt}

; get variables
    asNCVar = replicate({sNCVar}, sNCFileInfo.NVARS)
    for varId = 0, sNCFileInfo.NVARS-1 do begin 
      sNCVar = ncdf_varinq(NCFileId, varID)
      asNCVar[varId].name = sNCVar.name
      asNCVar[varId].datatype = sNCVar.datatype
      asNCVar[varId].ndims = sNCVar.ndims
      asNCVar[varId].natts = sNCVar.natts
      asNCVar[varId].dim = sNCVar.dim

; get variable attributes      
      for attId = 0, asNCVar[varId].natts-1 do begin
        asNCVar[varId].atts[attId].name = ncdf_attname(NCFileId, varId, attId)
        sRes = ncdf_attinq(NCFileId, varId, asNCVar[varId].atts[attId].name)
        asNCVar[varId].atts[attId].datatype = sRes.datatype
        asNCVar[varId].atts[attId].length= sRes.length
        ncdf_attget, NCFileId, varId, asNCVar[varId].atts[attId].name, value
        asNCVar[varId].atts[attId].value = strjoin(strtrim(string(value), 2), ",")       
      endfor
    endfor

    varId = (where(asNCVar.name eq self.sRequest.varName))[0] ; variable id
    if (varId eq -1) then begin ; we don't have requested variable in the file
      print, 'Error! There is no variable '+self.sRequest.varName+' in the file '+in_fileName+'. Aborting!'
      return, {}
    endif
      
    ; get positions of attribute 'units' for all variables
    unitsPos = where(asNCVar.atts.name eq 'units')
    noUnits = where(unitsPos eq -1)
    if (noUnits[0] ne -1) then unitsPos[noUnits] = 0 ; set to 0 to get no error if some variable does not have attribute 'units'

    varDimIdxs = asNCVar[varId].dim[0:asNCVar[varId].ndims-1]
    varDimNames = asNCDim[varDimIdxs].name

self->printLog, ' lonVar...'        
;    lonVarId = (where((asNCVar.atts.value)[unitsPos] eq 'degrees_east'))[0]
    lonVarId = (where(							$
		  ((asNCVar.atts.value)[unitsPos] eq 'degrees_east') or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degree_east')  or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degrees_E')    or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degree_E')     or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degreesE')     or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degreeE')		$
	       ))[0]
        
    if (lonVarId ne -1) then begin
      ncdf_varget, NCFileId, lonVarId, aLons
      minLon = min(aLons, max=maxLon)
    endif else begin
      self->printLog, 'Warning! No longitude variable in the file!'
    endelse

self->printLog, ' latVar...'        
;    latVarId = (where((asNCVar.atts.value)[unitsPos] eq 'degrees_north'))[0]
    latVarId = (where(							 $
		  ((asNCVar.atts.value)[unitsPos] eq 'degrees_north') or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degree_north')  or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degrees_N')     or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degree_N')      or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degreesN')      or $
		  ((asNCVar.atts.value)[unitsPos] eq 'degreeN')		 $
	       ))[0]

    if (latVarId ne -1) then begin
      ncdf_varget, NCFileId, latVarId, aLats
      minLat = min(aLats, max=maxLat)
    endif else begin
      self->printLog, 'Warning! No latitude variable in the file!'
    endelse

self->printLog, ' timeVar...'
; IMPORTANT! We detect time variable by a text attribute containing word 'since'. It MUST be present in CF/COARDS netCDF files.
    timeVarId = (where(strpos((asNCVar.atts.value)[unitsPos], 'since') ne -1))[0]
    timeUnitsId = (where(asNCVar[timeVarId].atts.name eq 'units'))[0]

self->printLog, ' levVar...'

    ; try to find a level name in the names of variable dimensions
    levVarName = self.levVarName
    levVarId = (where(asNCVar.name eq levVarName))[0] 
    
    if (levVarId ne -1) then begin
      ncdf_varget, NCFileId, levVarId, aLevs
      if (size(aLevel, /n_dim) eq 2) then begin ; suppose we have [lev, time] 2-D grid
        aLevs = aLevs[*, 0] ; then reduce it to 1-D
      endif
    endif else aLevs = ''
    
self->printLog, ' lonDim...'        
    if (lonVarId ne -1) then begin
      lonDimId = asNCVar[lonVarId].dim[0 mod asNCVar[lonVarId].ndims]
      lonPos = (where(asNCVar[varId].dim eq lonDimId))[0] ; should be 0
    endif else lonPos = -1

self->printLog, ' latDim...'
    if (latVarId ne -1) then begin
      latDimId = asNCVar[latVarId].dim[1 mod asNCVar[latVarId].ndims]
      latPos = (where(asNCVar[varId].dim eq latDimId))[0] ; should be 1
    endif else latPos = -1

self->printLog, ' timeDim...'
    if (timeVarId ne -1) then begin
      timeDimId = asNCVar[timeVarId].dim[0]
      timePos = (where(asNCVar[varId].dim eq timeDimId))[0] ; should be 3 (or 2 in case of level grid absence)
    endif else timePos = -1

self->printLog, ' levDim...'
    if (levVarId ne -1) then begin
      levDimId = asNCVar[levVarId].dim[0]
      levPos = (where(asNCVar[varId].dim eq levDimId))[0] ; should be 2
    endif else levPos = -1

    lonSize = asNCDim[lonDimId].size
    latSize = asNCDim[latDimId].size

    ncdf_close, NCFileId
    
    aCount = replicate(1L, asNCVar[varId].ndims)
    aOffset = replicate(0L, asNCVar[varId].ndims)
    
    if (size(aLons, /n_dim) eq 1) then begin ; if aLons is 1-D then we suppose all grids are 1-D
	iii = where((aLats gt self.sROIBounds.lat0) and (aLats lt self.sROIBounds.lat1), latCnt)
	latOffs = max([0, iii[0] - 1])
	latCnt = min([latSize-latOffs, latCnt + 1]) ; add one at the top
	latCnt = min([latSize-latOffs, latCnt + 1]) ; add one at the bottom
	latIdxs = indgen(latCnt)+latOffs
    endif else begin
	; we suppose 2D-grids are used for irregular grids thus prepare count and offset arrays for reading of the whole area represented in a file
	latOffs = 0
	latCnt = latSize
	latIdxs = -1
	isCutROI = 1	
    endelse

    if (latPos ne -1) then begin
      aCount[latPos] = latCnt
      aOffset[latPos] = latOffs
    endif

; ok, that's a long part for longitudes
	lonOffs2 = 0
	lonCnt2 = -1
	lonIdxs2 = -1
	isCutROI = 0 ; flag, should we cut ROI from data we have read
	if (size(aLons, /n_dim) eq 1) then begin ; if aLons is 1-D then we suppose all grids are 1-D
	    if (max(aLons) gt 180) then begin ; if aLons values are greater 180 then we have 0-360 longitude grid
	      if ((self.sROIBounds.lon0 lt 0) and (self.sROIBounds.lon1 gt 0)) then begin ; required area overlaps 0-meridian and thus we have 2 separated parts in 0-360 longitude grid
	        ; let's select RIGHT part of the area (0 - lon1)
	        iii = where((aLons gt 0) and (aLons lt (self.sROIBounds.lon1 + 360) mod 360), lonCnt)
	        lonOffs = max([0, iii[0] - 1])
	        lonCnt = lonCnt gt 0 ? min([lonSize-lonOffs, lonCnt + 1]) : 0 ; add one at the right
	        lonIdxs = lonCnt gt 0 ? indgen(lonCnt)+lonOffs : 0
	        ; let's select LEFT part of the area (lon0 - 360)
	        iii = where((aLons gt (self.sROIBounds.lon0 + 360) mod 360) and (aLons lt 360), lonCnt2)
	        lonOffs2 = max([0, iii[0] - 1])
	        lonCnt2 = lonCnt2 gt 0 ? min([lonSize-lonOffs, lonCnt2 + 1]) : 0 ; add one at the left
	        lonIdxs2 = lonCnt2 gt 0 ? indgen(lonCnt2)+lonOffs2 : 0
	        ; if right part is empty let's replace it with the left one thus we will read only one part
	        if (lonCnt eq 0) then begin
	          lonOffs = lonOffs2
	          lonCnt = lonCnt2
	          lonIdxs = lonIdxs2
	        endif
	        if ((lonCnt ne 0) and (lonCnt2 ne 0)) then begin ; if we have both parts
	          ; let's check if longitude grid is continous when crossing 0 meridian
	          cross360step = aLons[0]-aLons[-1]+360 ; grid step across 0 meridian
	          lonStep = aLons[1]-aLons[0] ; assume it a longitude grid
	          epsStep = lonStep / 20. ; epsilon vicinity of a 360 deg. point (1/20. of a step)
	          if (cross360step eq lonStep or ((cross360step lt 360+epsStep) and (cross360step gt 360-epsStep))) then begin ; we can merge both parts
	            self->printLog, ' No gap between parts'
	          endif else begin ; we can't merge two parts if there is a gap greater than a lonStep between them
		    self->printLog, ' There is a gap between parts. Reading whole longitude range'
		    lonOffs = 0
		    lonCnt = lonSize
		    lonIdxs = lonSize gt 0 ? indgen(lonSize) : 0
		    lonOffs2 = 0
		    lonCnt2 = -1
		    isCutROI = 1
	          endelse
	        endif
	      endif else begin ; required area fall into the LEFT or RIGHT half of the 0-360 field, so it easy here...
	        iii = where((aLons gt (self.sROIBounds.lon0 + 360) mod 360) and (aLons lt (self.sROIBounds.lon1 + 360) mod 360), lonCnt)
	        lonOffs = max([0, iii[0] - 1])
	        lonCnt = lonCnt gt 0 ? min([lonSize-lonOffs, lonCnt + 1]) : 0 ; add one at the left
	        lonCnt = lonCnt gt 0 ? min([lonSize-lonOffs, lonCnt + 1]) : 0 ; and another at the right
	        lonIdxs = lonCnt gt 0 ? indgen(lonCnt)+lonOffs : 0
	      endelse
	    endif else begin ; otherwise we have usual -180-+180 latitude grid
	      iii = where((aLons gt self.sROIBounds.lon0) and (aLons lt self.sROIBounds.lon1), lonCnt)
	      lonOffs = max([0, iii[0] - 1])
	      lonCnt = lonCnt gt 0 ? min([lonSize-lonOffs, lonCnt + 1]) : 0 ; add one at the left
	      lonCnt = lonCnt gt 0 ? min([lonSize-lonOffs, lonCnt + 1]) : 0 ; and another at the right
	      lonIdxs = lonCnt gt 0 ? indgen(lonCnt)+lonOffs : 0
	    endelse
	endif else begin
	  ; we suppose 2D-grids are used for irregular grids thus prepare count and offset arrays for reading of the whole area represented in a file
	  lonOffs = 0
	  lonCnt = lonSize
	  lonIdxs = -1
	  isCutROI = 1
	endelse

    struct = { fileName : in_fileName, $
               asDim : asNCDim, $
               asVar : asNCVar, $
               asGAtt : asNCGAtt, $
               varId : varId, $
               lonVarId : lonVarId, $
               latVarId : latVarId, $
               levVarId : levVarId, $
               timeVarId : timeVarId, $
               timeUnitsId : timeUnitsId, $
               aLons : aLons, $
               aLats : aLats, $
               aLevs : aLevs, $
               lonPos : lonPos, $
               latPos : latPos, $
               levPos : levPos, $
               timePos : timePos, $
               lonSize : lonSize, $
               latSize : latSize, $
               aCount : aCount, $
               aOffset : aOffset, $
               latIdxs : latIdxs, $
               lonCnt : lonCnt, $
               lonOffs : lonOffs, $
               lonIdxs : lonIdxs, $
               lonCnt2 : lonCnt2, $
               lonOffs2 : lonOffs2, $
               lonIdxs2 : lonIdxs2, $
               isCutROI : isCutROI $
             }
    
    return, struct
END
;--------------------------------------------------------------------
;+
; :Description:
;   Prepares NetCDF-files for reading.
;  
; :Author: 
;   garry
;-
FUNCTION cvcDataNetCDF::Prepare, in_varName, in_asTimeSeg, in_aLevel, in_sRegion

self->printLog, 'Preparing NCDF data'

    self.sRequest.varName = in_varName
    self.sRequest.asTimeSeg = in_asTimeSeg
    self.sRequest.numTimeSeg = n_elements(in_asTimeSeg)
    self.sRequest.aLevel = in_aLevel
    self.sRequest.nLev = n_elements(in_aLevel)
    self.sRequest.sRegion = in_sRegion

;    ; set data file name template
;    res = self->__SetDataFileTemplate(in_varName, in_aLevel, aDataFileTemplate)
;    if (res ne self.ERROR_OK) then return, res

;    self.pDataFile = ptr_new({datafile, nameTmpl:aDataFileTemplate})

self->printLog, 'NCDF data is prepared!'
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
;+
; :Description:
;   Reads data array from NetCDF-file.
;  
; :Author: 
;   garry
;-
FUNCTION cvcDataNetCDF::Read, in_aArea, in_aTimeRng, in_level, out_sData

    self->printLog, 'Reading netCDF file...'
    levIdx = where(self.sRequest.aLevel eq in_level)
    if (levIdx[0] eq -1) then return, self->SayError(self.ERROR_WRONGLEV)

; time range requested (in Julian days)    
    reqTimeJD = dblarr(2)
    self->DayMonthYear, in_aTimeRng[0], reqYearBeg, reqMonthBeg, reqDayBeg, reqHourBeg
    reqTimeJD[0] = julday(reqMonthBeg, reqDayBeg, reqYearBeg, reqHourBeg)
    self->DayMonthYear, in_aTimeRng[1], reqYearEnd, reqMonthEnd, reqDayEnd, reqHourEnd
    reqTimeJD[1] = julday(reqMonthEnd, reqDayEnd, reqYearEnd, reqHourEnd)

; ROI bounds:
    lon0 = min(in_aArea[0, *], max=lon1)
    lat0 = min(in_aArea[1, *], max=lat1)
    self.sROIBounds = {lon0:lon0, lat0:lat0, lon1:lon1, lat1:lat1}

; let's calculate number of files to read depending on the time span of one file

    fileSpan = self.dataset.filespan
    if (fileSpan ne 'none') then begin
      fileTimeSpan = fix(strmid(fileSpan, 0, strlen(fileSpan)-1))
      fileTimeUnits = strmid(fileSpan, strlen(fileSpan)-1, 1)
    
      case fileTimeUnits of
          'y': begin
    	    numberOfDataFiles = (reqYearEnd - reqYearBeg) / fileTimeSpan + 1
    	    curTimeJD = julday(1, 1, reqYearBeg)
	  end
          'm': begin
            numberOfDataFiles = (reqYearEnd*12 + reqMonthEnd - reqYearBeg*12 - reqMonthBeg) / fileTimeSpan + 1
            curTimeJD = julday(reqMonthBeg, 1, reqYearBeg)
    	  end
          'd': begin
            numberOfDataFiles = (julday(reqMonthEnd, reqDayEnd, reqYearEnd) - julday(reqMonthBeg, reqDayBeg, reqYearBeg)) / fileTimeSpan + 1
            curTimeJD = julday(reqMonthBeg, reqDayBeg, reqYearBeg)
    	  end
          'h': begin
    	    numberOfDataFiles = (reqTimeJD[1] - reqTimeJD[0]) * 24 / fileTimeSpan + 1
    	    curTimeJD = reqTimeJD[0]
    	  end
          else: begin 
    	    self->printLog, "Error! Unknown time span unit!"
            return, -1
          end
      endcase
      numberOfDataFiles = fix(numberOfDatafiles)
    endif else begin ; if fileSpan is 'none' we have a single data file
      fileTimeSpan = fileSpan
      fileTimeUnits = ''
      numberOfDataFiles = 1
    endelse
    
self->printLog, numberOfDataFiles, ' files in list'

;    curTimeJD = reqTimeJD[0]
    isFileKnown = "no"
    nameTmpl = self.dataset.fileTemplate

;    
; Loop by data files
;
    for fileIdx = 0, numberOfDataFiles-1 do begin

      self->printLog, 'Preparing file ', fileIdx+1, 'of ', numberOfDataFiles

      fileName = self->__ParseFileTemplate(nameTmpl[levIdx], curTimeJD, fileTimeSpan, fileTimeUnits)

      if ( file_test(fileName) ) then begin
          if (isFileKnown eq "no") then begin ; if this is a new file we read info otherwise we use the old one
            self->printLog, 'Reading datafile metadata...'
            asCurFileInfo = self->__QueryNCDF(filename)
            if (n_elements(asCurFileInfo) eq 0) then begin
              print, '(cvcDataNetCDF::readNCDF) Something wrong happend when querying netCDF-file. Unable to continue...'
              return, -1
            endif
    	    varId = asCurFileInfo.varId
	    if (varId eq -1) then begin
    	      self->printLog, 'Error! Variable '+self.sRequest.varName+' not found!'
    	      return, -1
	    endif
            isFileKnown = "no"
            self->printLog, 'OK'
          endif
      endif else begin
          self->printLog, 'Warning! File is missing:', filename, ' Skipping it.'

	  case fileTimeUnits of
    	    'y': self->IncYear, curTimeJD, fileTimeSpan
    	    'm': self->IncMonth, curTimeJD, fileTimeSpan
    	    'd': self->IncDay, curTimeJD, fileTimeSpan
    	    'h': self->IncHour, curTimeJD, fileTimeSpan
    	    else:
    	  endcase      

          continue
      endelse

;self->printLog, 'File:', fileIdx, ' is ', self.dataFileType
;======================================================================================================

      if (self.debugMode) then begin
        self->printLog, "(cvcDataNetCDF::ReadNCDF) Reading file: ", fileName  
        self->printLog, "(cvcDataNetCDF::ReadNCDF) Requesting variable: ", self.sRequest.varName, ", level: ", in_level, ", time: ", in_aTimeRng, $
         format='(a, a, a, a, a, 2i)'
      endif

self->printLog, 'Open netCDF...', format='(a, $)'        
      fileId = ncdf_open(fileName)
self->printLog, 'OK', /NoTimeStamp

self->printlog, 'Set variables...'
      aLons = asCurFileInfo.aLons
      aLats = asCurFileInfo.aLats
      aLevs = asCurFileInfo.aLevs
      levVarId = asCurFileInfo.levVarId
      timeVarId = asCurFileInfo.timeVarId
      timeUnitsId = asCurFileInfo.timeUnitsId
      lonPos = asCurFileInfo.lonPos
      levPos = asCurFileInfo.levPos
      timePos = asCurFileInfo.timePos
      aCount = asCurFileInfo.aCount
      aOffset = asCurFileInfo.aOffset
      latIdxs = asCurFileInfo.latIdxs
      lonCnt = asCurFileInfo.lonCnt
      lonOffs = asCurFileInfo.lonOffs
      lonIdxs = asCurFileInfo.lonIdxs
      lonCnt2 = asCurFileInfo.lonCnt2
      lonOffs2 = asCurFileInfo.lonOffs2
      lonIdxs2 = asCurFileInfo.lonIdxs2
      isCutROI = asCurFileInfo.isCutROI

self->printlog, 'Set levels variable...'
      if (levVarId ne -1) then begin
        nReqLevs = n_elements(in_level)
        levIdxs = intarr(nReqLevs)
        for iReqLev = 0, nReqLevs-1 do begin
          reqLev = in_level[iReqLev]
          if (strmid(reqLev, 0, 1) eq '#') then begin
            levIdxs[iReqLev] = fix(strmid(reqLev, 1))-1
          endif else begin
            levIdxs[iReqLev] = (where(aLevs eq reqLev))[0] ; only one level is taken!
          endelse
          aLevel = aLevs[levIdxs]
        endfor
      endif else aLevel = in_level


      if (timeVarId ne -1) then begin ; we have time grid in a file
self->printlog, 'Reading time variable...'
        ncdf_varget, fileId, timeVarId, aTimeInFile
        if (timeUnitsId ne -1) then begin
self->printlog, 'Converting time to JD...'
          res = self->__ConvertTU2JD(aTimeInFile, asCurFileInfo.asVar[timeVarId].atts[timeUnitsId].value, aTimeInFileJD)
        endif else begin ; it seems we have WRF data... but may be not!
          res = self->__ByteStrToDate(aTimeInFile, aYr, aMo, aDy, aHr)
          aTimeInFileJD = julday(aMo, aDy, aYr, aHr)
        endelse
self->printlog, 'Set time variable...'
; number of time points and their positions in the current file time grid
        timeIdxs = where((aTimeInFileJD ge reqTimeJD[0]) and (aTimeInFileJD le reqTimeJD[1]), nCurTimes)

	case self.dataset.tstep of
	  '1m': begin
	    timeStep = 1
	    nTimes = (reqYearEnd - reqYearBeg)*12 + reqMonthEnd - reqMonthBeg + 1
	  end
	  '1y': begin
            timeStep = 1
	    nTimes = (reqYearEnd - reqYearBeg) + 1
	  end
	  '6h': begin
	    timeStep = 0.25 
            nTimes = floor((reqTimeJD[1]-reqTimeJD[0]) / timeStep + 1)
	  end
	  '1mc': begin ; monthly ciimatological values
	    caldat, aTimeInFileJD, m_InFile, d_InFile, y_InFile ; months, days and years set in file time grid
	    ; then we create a new 'required time range' by taking the same year as in the file (it must be the same for all time grid points!)
	    reqTimeJDClim = [julday(reqMonthBeg, 1, y_InFile[0]), julday(reqMonthEnd, 31, y_InFile[0])] 
	    ; and find time indices (suppose, we want to process monthly climatological values for a few months)
	    timeIdxs = where((aTimeInFileJD ge reqTimeJDClim[0]) and (aTimeInFileJD le reqTimeJDClim[1]), nCurTimes)
	    timeStep = 1
	    nTimes = n_elements(timeIdxs)
	  end
	  '1yc': begin ; yearly climatology values
	    timeIdxs = [0] ; we suppose there is only one field in a file for the yearly climatology
	    timeStep = 1
	    nTimes = 1
	  end
	  else: begin
	    timeStep = (aTimeInFileJD[1]-aTimeInFileJD[0])
            nTimes = floor((reqTimeJD[1]-reqTimeJD[0]) / timeStep + 1)
	  end
	endcase

;        if (self.dataset.tstep eq '1m') then begin
;	  timeStep = 1
;	  nTimes = (reqYearEnd - reqYearBeg)*12 + reqMonthEnd - reqMonthBeg + 1
;        endif else if (self.dataset.tstep eq '1y') then begin
;            timeStep = 1
;	    nTimes = (reqYearEnd - reqYearBeg) + 1
;          endif else if (n_elements(aTimeInFileJD) gt 1) then begin
;              timeStep = (aTimeInFileJD[1]-aTimeInFileJD[0])
;              nTimes = ceil((float(reqTimeJD[1])-float(reqTimeJD[0])) / timeStep + 1)
;            endif else begin
;    	      timeStep = 0
;    	      nTimes = 1
;    	    endelse    	    
; allocate global time grid
        if (n_elements(aTimes) eq 0) then begin
    	  aTimes = dblarr(nTimes, /nozero)
	  maxJD = 1827933925 ; maximum julian day
	  aTimes[*] = maxJD
    	  startTimeArr = 0
    	endif
; fill global time grid by current file time data
        aTimes[startTimeArr:startTimeArr+nCurTimes-1] = aTimeInFileJD[timeIdxs]
        startTimeArr += nCurTimes
        if (size(aLons, /n_dim) eq 3) then begin ; we have irregular lon-lat grid
          aLons = aLons[*, *, 0]
          aLats = aLats[*, *, 0]
        endif
      endif else begin ; we don't have time grid in a file
        timeAttId = (where(asCurFileInfo.asVar[varId].atts.name eq 'initial_time'))[0]
        if (timeAttId ne -1) then begin ; we assume there is an attribute with one time point for each file
          spl_dt = strsplit(asCurFileInfo.asVar[varId].atts[timeAttId].value, ' ', /extract)
          spl_date = strsplit(spl_dt[0], '/', /extract)
          spl_time = strsplit(strmid(spl_dt[1], 1, strlen(spl_dt[1])-2), ':', /extract)
          if (n_elements(aTimes) eq 0) then begin
            aTimes = fltarr(numberOfDataFiles)
            nTimes = numberOfDataFiles
	  endif
          aTimes[fileIdx] = julday(spl_date[0], spl_date[1], spl_date[2], spl_time[0], spl_time[1])
          timeIdxs = [0]
        endif else begin ; there is no any time info
    	  aTimes = [0]
    	  timeIdxs = [0]
    	  nTimes = 0
    	endelse
      endelse

; read data
self->printlog, 'Setting count and offset...'
; we set here times and levels but deal with longitudes later
      if (timePos ne -1) then begin
        aCount[timePos] = n_elements(timeIdxs)
	aOffset[timePos] = timeIdxs[0]
      endif
      if (levPos ne -1) then begin
        aCount[levPos] = n_elements(levIdxs)
        aOffset[levPos] = levIdxs
      endif
	
; let's set longitudes for the FIRST area
        if (lonPos ne -1) then begin
          aCount[lonPos] = lonCnt
          aOffset[lonPos] = lonOffs
        endif

	iii = where(aCount eq 0)
	if (iii[0] eq -1) then begin ; if the area to read is not empty let's read it
self->printLog, '======= Reading from netCDF actually...', format='(a, $)'
	  ncdf_varget, fileId, varId, aData, count=aCount, offset=aOffset
self->printLog, 'OK', /NoTimeStamp
	endif else begin
	  self->printLog, 'Warning! Area is empty! No data to read! Skipping file!'
	  continue
	endelse

	
; let's set longitudes for the SECOND area (if it is present!)
	if (lonCnt2[0] ne -1) then begin
          if (lonPos ne -1) then begin
            aCount[lonPos] = lonCnt2
            aOffset[lonPos] = lonOffs2
          endif
	
	  iii = where(aCount eq 0)
	  if (iii[0] eq -1) then begin ; if the area to read is not empty let's read it
self->printLog, '======= Reading SECOND area from netCDF actually...', format='(a, $)'
	    ncdf_varget, fileId, varId, aData2, count=aCount, offset=aOffset
self->printLog, 'OK', /notimestamp
	  endif else self->printLog, 'SECOND area is empty! No data to read! Skipping part!'
	endif

; let's get dimensions of aData and aData2
        dataDims = size(aData, /dimensions)
        dataDims2 = size(aData2, /dimensions)
        
; ok, here we suppose two things: 
; 1) if aData is NULL then aData2 is NULL also and we have no data; 
; 2) if aData2 is NOT NULL then aData is NOT NULL also, and, consequently, we can merge it with aData along longitude grid (grids also should be merged)

	if (dataDims2[0] ne 0) then begin
	  dataDimsNew = dataDims
	  dataDimsNew[0] += dataDims2[0]
	  dataDims = dataDimsNew
	  aDataNew = fltarr(dataDimsNew, /nozero)
	  ; data parts are merged in such way: [aData2:aData]
;	  aDataNew[0:lonCnt2-1, *, *, *] = temporary(aData2)
;	  aDataNew[lonCnt2:lonCnt2+lonCnt-1, *, *, *] = temporary(aData)
	  aDataNew = [temporary(aData2), temporary(aData)]
	  aData = temporary(aDataNew)
	  aLons = [(aLons[lonIdxs2] + 180) mod 360 - 180, (aLons[lonIdxs] + 180) mod 360 - 180]
	  aLats = aLats[latIdxs]
	endif else begin
	  if (size(aLons, /n_dim) eq 1) then aLons = aLons[lonIdxs]
	  if (size(aLats, /n_dim) eq 1) then aLats = aLats[latIdxs]
	endelse

; self->printLog, min(aData), max(aData)

; apply scale/offset        
self->printLog, 'Apply scale/offset...', format='(a, $)'
        addOffsPos = (where(asCurFileInfo.asVar[varId].atts.name eq 'add_offset'))[0] ; let's take only the first element
        if (addOffsPos ne -1) then addOffset = float(asCurFileInfo.asVar[varId].atts[addOffsPos].value) else addOffset = 0
        scaleFactPos = (where(asCurFileInfo.asVar[varId].atts.name eq 'scale_factor'))[0]
        if (scaleFactPos ne -1) then scaleFactor = float(asCurFileInfo.asVar[varId].atts[scaleFactPos].value) else scaleFactor = 1
	if ((addOffset ne 0.) or (scaleFactor ne 1.)) then aData = float(aData) * scaleFactor + addOffset
self->printLog, 'OK', /notimestamp

; missing value = fix('missing_value')
self->printLog, 'Fixing missing value...', format='(a, $)'
        misValPos = (where(asCurFileInfo.asVar[varId].atts.name eq 'missing_value'))[0]
        if (misValPos eq -1) then misValPos = (where(asCurFileInfo.asVar[varId].atts.name eq '_FillValue'))[0]
        if (misValPos ne -1) then missingVal = asCurFileInfo.asVar[varId].atts[misValPos].value else missingVal = -999.0

        newMissingVal = -999.0
        if (missingVal ne newMissingVal) then begin
          missingIdxs = where(aData eq missingVal, missingCnt)
          if (missingCnt gt 0) then aData[missingIdxs] = newMissingVal
        endif
        missingVal = newMissingVal
self->printLog, 'OK', /notimestamp

self->printLog, 'Eliminating single-size dimensions...'
	iii = where(dataDims ne 1, cnt)
	if (cnt eq 0) then begin
	  self->printLog, "(cvcDataNetCDF::ReadNCDF) Error! Scalar has been read! Don't know what to do. Aborting..."
	  return, -1
	endif
        newDims = dataDims[iii]
self->printLog, 'Info: One data array contains '+strtrim(string(n_elements(timeIdxs)), 2)+' time points'
        if (n_elements(newDims) ne n_elements(dataDims)) then begin
self->printLog, 'Reforming aData...', format='(a, $)'
          aData = reform(aData, newDims, /overwrite)
self->printLog, 'OK', /notimestamp
        endif

	if (n_elements(aAllData) eq 0) then begin
self->printLog, 'Allocating global data array...'
	  allDims = newDims
          if (n_elements(aTimeInFileJD) eq 0) then allDims = [allDims, 1]
	  if (nTimes gt 1) then allDims[n_elements(allDims)-1] = nTimes
self->printLog, 'Global array dimensions:', allDims
	  aAllData = fltarr(allDims, /nozero)
	  aAllData[*] = -999.
self->printLog, 'OK'
; here we create start position where to place per-file data... it's a temporary stub and should be fixed in t
	  startArr = 0
	endif

self->printLog, 'Inserting current data array into global data array...', format='(a, $)'
  if (nTimes eq 1) then aAllData = aData else begin
    if (size(allDims, /dim) eq 3) then aAllData[*, *, startArr:startArr+n_elements(timeIdxs)-1] = aData
    if (size(allDims, /dim) eq 2) then aAllData[*, startArr:startArr+n_elements(timeIdxs)-1] = aData
    if (size(allDims, /dim) eq 1) then aAllData[startArr:startArr+n_elements(timeIdxs)-1] = aData
  endelse
; shift start position to a new place.
  startArr += n_elements(timeIdxs)
self->printLog, 'OK', /notimestamp

; close opened nc-file         
self->printLog, 'Close netCDF file...', format='(a, $)'
        ncdf_close, fileId
self->printLog, 'OK', /notimestamp
; OK, here we comment out closing endif for 'date-in-file-range' check
;      endif ;else self->printLog, 'Date requested is outside of dataset date range! Skipping. No data will be read!'


      case fileTimeUnits of
        'y': self->IncYear, curTimeJD, fileTimeSpan
        'm': self->IncMonth, curTimeJD, fileTimeSpan
        'd': self->IncDay, curTimeJD, fileTimeSpan
        'h': self->IncHour, curTimeJD, fileTimeSpan
        else:
      endcase      

self->printLog, '<End of turn>'
    endfor

    if (n_elements(aAllData) eq 0) then begin
       self->printLog, 'No data has been read. Returning dummy values.'
       out_sData = { aData : 0, aLons : 0, aLats : 0, aTimes : 0, sTimeRng : {dummy:0}, lev : 0, $
                     minVal : 0, maxVal : 0, missingVal : 0, gridType : 'empty', aLev : 0, nLev : 0, $
                     resultCode : self.ERROR_NODATAREAD }
       return, self.ERROR_NODATAREAD
    endif

; Cutting ROI is necessary only if area we have just read is greater than required (and specified by in_aArea)
; Also we need to cut if in_aArea represents non-rectangular area
    isRect1 = ((in_aArea[1, 0] eq in_aArea[1, 1]) and (in_aArea[1, 2] eq in_aArea[1, 3]) and (in_aArea[0, 1] eq in_aArea[0, 2]) and (in_aArea[0, 0] eq in_aArea[0, 3]))
    isRect2 = ((in_aArea[1, 0] eq in_aArea[1, 3]) and (in_aArea[1, 2] eq in_aArea[1, 1]) and (in_aArea[0, 1] eq in_aArea[0, 0]) and (in_aArea[0, 2] eq in_aArea[0, 3]))
    if (~(isRect1 or isRect2) or isCutROI) then begin

self->printLog, 'Cutting ROI...'
      res = self->__CutROI(self.sRequest.sRegion.lon0, in_aArea, aLons, aLats, missingVal, aAllData)
      if (res ne self.ERROR_OK) then return, -1
self->printLog, 'OK'
    endif else self->printLog, 'Rectangular area. Skip ROI cutting.'

; self->printLog, 'Good values...'
self->printLog, 'NOT Searching for min and max values...', format='(a, $)'
    minVal = 1e9
    maxVal = -1e9

; We suppose if aLats AND aLons are NOT vectors (1-D) we have irregular grid
    if ((size(aLons, /n_dim) gt 1) and (size(aLats, /n_dim) gt 1)) then gridType = 'irregular' else gridType = 'regular'


self->printLog, 'OK', /notimestamp

    out_sData = { aData : temporary(aAllData), $
                  aLons : temporary(aLons), $
                  aLats : temporary(aLats), $
                  aTimes : temporary(aTimes), $
                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                  lev : aLevel, $
                  minVal : MinVal, $
                  maxVal : MaxVal, $
                  missingVal : missingVal, $
                  gridType : gridType, $
                  aLev : [aLevel], $
                  nLev : 1, $
		  units : '', $ ; this field is filled in the cvcDataAccess::Get() method
                  resultCode : self.ERROR_OK $
                }

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataNetCDF::__ParseFileTemplate, in_fileNameTemplate, in_jd, in_span, in_unit

  aPatternName = ['day2dig', 'month2dig', 'month3small', 'month3cap', 'month3big', 'year2dig', 'year4dig', 'year4dig1', 'year4dig2', 'hour2dig']
  aDateFormat = ['(C(CDI2.2))', '(C(CMOI2.2))', '(C(CmoA))', '(C(CMoA))', '(C(CMOA))', '(C(CYI2.2))', '(C(CYI4.4))', '(C(CYI4.4))', '(C(CYI4.4))', '(C(CHI2.2))']
  aDateElementPos = [2, 1, 1, 1, 1, 0, 0, 0, 0, 3]
  aDateElementAlt = [0, 0, -1, 1, 2, 0, 0, 3, 4, 0]
  aDateElementFmt = ['(I2.2)', '(I2.2)', '(A)', '(A)', '(A)', '(I2.2)', '(I4.4)', '(I4.4)', '(I4.4)', '(I2.2)']
  aMonths = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec']
  nPattern = n_elements(aPatternName)

  fileName = in_fileNameTemplate

  for patternIdx=0, nPattern-1 do begin
    pattern = self.oConst->Get(aPatternName[patternIdx])
    pos = strpos(fileName, pattern)
    while (pos ne -1) do begin
      caldat, in_jd, mm, dd, yy, hh
      tt = [yy, mm, dd, hh]
      dateElem = tt[aDateElementPos[patternIdx]]
      case aDateElementAlt[patternIdx] of
       -1: dateElem = aMonths[dateElem] 
        1: dateElem = strupcase(strmid(aMonths[dateElem], 0, 1))+strmid(aMonths[dateElem], 1)
        2: dateElem = strupcase(aMonths[dateElem])
        3: if (in_unit eq 'y') then dateElem = (dateElem / in_span) * in_span 
        4: if (in_unit eq 'y') then dateElem =  (dateElem / in_span + 1) * in_span - 1
        else:
      endcase
      dateElem = strtrim(string(dateElem, format=aDateElementFmt[patternIdx]), 2)
;      fileName = strmid(fileName, 0, pos) + string(in_jd, format=aDateFormat[patternIdx]) + strmid(fileName, pos+strlen(pattern))
      fileName = strmid(fileName, 0, pos) + dateElem + strmid(fileName, pos+strlen(pattern))
      pos = strpos(fileName, pattern)
    endwhile
  endfor

  return, fileName
END
;--------------------------------------------------------------------
PRO cvcDataNetCDF::GetProperty, sRequest = sRequest
  sRequest = self.sRequest
END
;--------------------------------------------------------------------
PRO cvcDataNetCDF__define

    struct = { cvcDataNetCDF, $
                  INHERITS cvcData $
             }
END
