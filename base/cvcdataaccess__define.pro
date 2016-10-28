;
;  NAME: 
;    cvcDataAccess
;
;  PURPOSE:
;    Mid-level class-helper. Provides access to low-level data-access classes. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataAccess::Init, in_oGlobal
    
    self.oGlobal = in_oGlobal
    hLogFile = self.oGlobal->Get('hLogFile')
    if ( self->cvcError::Init(hLogFile) eq 0 ) then return, 0
    
    self.hLogFile = hLogFile
;    self.nDataIds = 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataAccess::Cleanup
 
  if (obj_valid(self.oDataObj)) then obj_destroy, self.oDataObj
END
;--------------------------------------------------------------------
; Creates data file name template using dataset description file
FUNCTION cvcDataAccess::__SetDataFileTemplate, out_fileType

    sData = *(self.pData)

    sMetaDBDesc = self.oGlobal->Get('metadb') ; get metadatabase description

    ; open db
    res = python('mymydb', 'connect_db', sMetaDBDesc.host, sMetaDBDesc.name, sMetaDBDesc.user, sMetaDBDesc.password)

    filePathTemplate = '"' ; file path template

    ; get dataset id
    res = python('mymydb', 'query_db', 'select id from collection where name="'+sData.dataset.name+'";')
    if (res eq 0) then begin
      self->printLog, 'ERROR! No such data collection: '+sData.dataset.name+'. Aborting!'
      return, -1
    endif
    collection_id = fix(python('mymydb', 'get_data', 0, 'numeric'))

    ; get scenario name
    res = python('mymydb', 'query_db', 'select id, subpath0 from scenario where name="'+sData.dataset.scenario+'";')
    if (res eq 0) then begin
      self->printLog, 'ERROR! No such scenario: '+sData.dataset.scenario+' for dataset '+sData.dataset.name+'. Aborting!'
      return, -1
    endif
    scenario_id = fix(python('mymydb', 'get_data', 0, 'numeric'))
    subpath0 = python('mymydb', 'get_data', 1, 'string')

    ; get horizontal resolution
    res = python('mymydb', 'query_db', 'select id, subpath1 from res where name="'+sData.dataset.res+'";')
    if (res eq 0) then begin
      self->printLog, 'ERROR! No such resolution: '+sData.dataset.res+' for dataset '+sData.dataset.name+'. Aborting!'
      return, -1
    endif
    res_id = fix(python('mymydb', 'get_data', 0, 'numeric'))
    subpath1 = python('mymydb', 'get_data', 1, 'string')

    ; get time step
    res = python('mymydb', 'query_db', 'select id, subpath2 from tstep where name="'+sData.dataset.tstep+'";')
    if (res eq 0) then begin
      self->printLog, 'ERROR! No such time step: '+sData.dataset.tstep+' for dataset '+sData.dataset.name+'. Aborting!'
      return, -1
    endif
    tstep_id = fix(python('mymydb', 'get_data', 0, 'numeric'))
    subpath2 = python('mymydb', 'get_data', 1, 'string')

    nLev = sData.numLevels ; number of levels
    aFileNameTemplate = strarr(nLev) ; array of file name templates (one template for each level)
    for levIdx = 0, nLev-1 do begin
    ; get file name
      qry = 'select file_id, ds_id, scale, offset from data where ds_id in '+$
    	    '(select id from ds where collection_id='+string(collection_id)+' and scenario_id='+string(scenario_id)+' and res_id='+string(res_id)+' and tstep_id='+string(tstep_id)+')'+$
            ' and var_id in (select id from var where name="'+sData.VariableName+'") and '+$
            'lvs_id in (select id from lvs where name like "%:'+sData.aLevel[levIdx]+':%");'
      res = python('mymydb', 'query_db', qry)
      if (res eq 0) then begin
        self->printLog, 'ERROR! No matching subdataset found. Aborting!'
        return, -1
      endif
      file_id = fix(python('mymydb', 'get_data', 0, 'numeric'))
      ds_id = fix(python('mymydb', 'get_data', 1, 'numeric'))
      data_scale = float(python('mymydb', 'get_data', 2, 'numeric'))
      data_offset = float(python('mymydb', 'get_data', 3, 'numeric'))
      (*self.pData).modify.scale = data_scale
      (*self.pData).modify.offset = data_offset

      ; get subdataset root path
      res = python('mymydb', 'query_db', 'select rootpath from dsroot where id in'+$
                   '(select dsroot_id from ds where id='+string(ds_id)+');')
      rootpath = python('mymydb', 'get_data', 0, 'string')
      filePathTemplate += string(rootpath) + string(subpath0) + string(subpath1) + string(subpath2)

      ; get file type
      res = python('mymydb', 'query_db', 'select name from filetype where id in'+$
                   '(select filetype_id from ds where id='+string(ds_id)+');')
      out_fileType = string(python('mymydb', 'get_data', 0, 'string'))

      ; get file name template
      res = python('mymydb', 'query_db', 'select name from file where id='+string(file_id)+';')
      filepath = python('mymydb', 'get_data', 0, 'string')
      aFileNameTemplate[levIdx] = filePathTemplate + string(filepath) + '"'
      self->printLog, 'Selected filename: ' + aFileNameTemplate[levIdx]

      ; get file time span
; NB! Here we extract file time span for each level BUT only the last one will be used. This could lead to errors in some cases. Should be expanded in the future!
      res = python('mymydb', 'query_db', 'select name from timespan where id in'+$
                   '(select timespan_id from ds where id='+string(ds_id)+');')
      filespan = python('mymydb', 'get_data', 0, 'string')
;      res = self.oGlobal->Add(UID='fileSpan', TYPE='string', VALUE=string(filespan))
      (*self.pData).dataset.filespan = string(filespan)
    endfor

; OK, here is a trick: we suppose the task contains request for a data on (at least) one level (that's reasonable).
; BUT, also we suppose the data variable requested is selected on vertical levels stored in only one level variable.
; In other words, for now, we don't know what to do if the task contains a request for a data on a 1000mb level... 
; ... from a netCDF-file with vertical grid stored in a variable 'level'...
; ... and at 2m height from another netCDF-file without a vertical grid at all. We can work only with one vertical grid.
; That should be fixed in the future.
    
    if (nLev gt 0) then begin ; let's hope nLev is ALWAYS greater than 0 :)
    ; get level variable name for a first level. We suppose all levels requested are stored in one level variable.    
      qry = 'select name from lvs_var where id in (select lvs_var_id from data where ds_id='+string(ds_id)+$
            ' and var_id in (select id from var where name="'+sData.variableName+'") and '+$
            'lvs_id in (select id from lvs where name like "%:'+sData.aLevel[0]+':%"));'
      res = python('mymydb', 'query_db', qry)
      levVarName = string(python('mymydb', 'get_data', 0, 'string'))
      if (levVarName eq '') then begin
        self->printLog, "(cvcDataAccess::__SetDataFileTemplate): No such level for this dataset: ", sData.aLevel[0]
        return, -1
      endif
;      res = self.oGlobal->Add(UID='levVarName', TYPE='string', VALUE=levVarName)
      (*self.pData).levVarName = levVarName
    endif else begin
      self->printLog, 'Kernel panic! In function cvcDataAccess::__SetDataFileTemplate variable nLev is less or equal to 0!'
      return, -1
    endelse

    ; close db
    res = python('mymydb', 'close_db')

;    res = self.oGlobal->Add(UID='fileTemplate', TYPE='string array', VALUE=aFileNameTemplate)                                    
    (*self.pData).dataset.fileTemplate = aFileNameTemplate

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataAccess::Open, in_sData

  self->printLog, '(cvcDataAccess::Open) Prepare data...'  
  res = self.ERROR_OK ; everything looks good from there
  oNewDataObj = obj_new() ; nullify object pointer
  
  self.pData = in_sData.data
  sData = *(in_sData.data)

  ; we do not open parameters
  if (sData.type eq 'parameter') then return, self.ERROR_OK
  
  DSName = sData.dataset.name
  DSRes = sData.dataset.res
  DSTStep = sData.dataset.tstep
  varName = sData.VariableName
  numLevels = sData.numLevels
  aLevel = sData.aLevel[0:sData.numLevels-1]
  numTimeSeg = sData.numTimeSeg
  asTimeSeg = sData.asTimeSeg[0:sData.numTimeSeg-1]
  minTime = min(sData.asTimeSeg[0:sData.numTimeSeg-1].beginning)
  maxTime = max(sData.asTimeSeg[0:sData.numTimeSeg-1].ending)
  sMaxTimeSeg = {structTimeSeg, name:'MaxTimeSeg', beginning:minTime, ending:maxTime, step:sData.asTimeSeg[0].step}
  modulePath = sData.module_path
  sRegion = sData.sRegion

  if (sData.type eq 'dataset') then begin ; dataset processing


; set data file name template and add it to the global env
    res = self->__SetDataFileTemplate(fileType)
    if (res ne self.ERROR_OK) then return, res
                                    
;    if (DSName eq '') then return, self->SayError(self.ERROR_BADDSNAME)
    self.className = 'cvcData'+fileType

    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_BADMODULENAME, self.className)
    endif

    self->printLog, "(cvcDataAccess::Open) Create data object..."
    oNewDataObj = obj_new( self.className, in_sData.data, sData.module_path, self.oGlobal )
    catch, /cancel
    self->printLog, "(cvcDataAccess::Open) OK"

    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_ERRORINMODULE, self.className)
    endif

;    if ((strupcase(strmid(DSName, 0, 4)) eq 'USER')) then begin ; user module is used
;      userModuleName = strmid(DSName, 5)
;      res = oNewDataObj->Prepare(varName, sMaxTimeSeg, aLevel, userModuleName)
;    endif else begin  ; internal module is used

      res = oNewDataObj->Prepare(varName, asTimeSeg, aLevel, sRegion)

;    endelse

    catch, /cancel
    self->printLog, "(cvcDataAccess::Open) Data object is created"
  endif

  if (sData.type eq 'datafile') then begin ; dataset processing
    
    fileType = 'netcdf'
    (*in_sData.data).dataset.filespan = 'none'
    (*in_sData.data).levVarName = 'none'
    (*in_sData.data).dataset.fileTemplate = DSName
 
    if (DSName eq '') then return, self->SayError(self.ERROR_BADDSNAME)
    self.className = 'cvcData'+fileType
    self.dataFileName = DSName
    
    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_BADMODULENAME, self.className)
    endif
    oNewDataObj = obj_new( self.className, in_sData.data, sData.module_path, self.oGlobal ) ; here we pass a file name instead of a module path
    catch, /cancel
  
    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_ERRORINMODULE, self.className)
    endif
    res = oNewDataObj->Prepare(varName, sMaxTimeSeg, aLevel, sRegion)
    catch, /cancel
  endif
  
  if (sData.type eq 'array') then begin ; dataset processing
    self.className = 'cvcDataArray'
    
    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_BADMODULENAME, self.className)
    endif
    oNewDataObj = obj_new( self.className, in_sData.data, modulePath, self.oGlobal )
    catch, /cancel

    catch, errorStatus
    if (errorStatus ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_ERRORINMODULE, self.className)
    endif
    res = oNewDataObj->Prepare(varName, sMaxTimeSeg, aLevel, in_sData.data)
    catch, /cancel
  endif

  if (res eq self.ERROR_OK) then begin
    ; let's find the first free element in data IDs array
;    for idNew = 0, n_elements(self.oaDataObj)-1 do if (~obj_valid(self.oaDataObj[idNew])) then break
;    if (IdNew eq n_elements(self.oaDataObj)) then return, self->SayError(self.ERROR_NOMOREIDS)
    
;    self.oaDataObj[IdNew] = oNewDataObj
;    self.nDataIds = self.nDataIds + 1
;    out_dataId = IdNew

    self.oDataObj = oNewDataObj
    retCode = self.ERROR_OK
  endif else retCode = res

  self->printLog, '(cvcDataAccess::Open) Data preparation is finished'  

  return, retCode
END
;--------------------------------------------------------------------
;FUNCTION cvcDataAccess::Close ;, in_dataId

;  if (n_elements(in_dataId) eq 0) then return, self->SayError(self.ERROR_BADDATAID)
  
;  if (in_dataId ge 0) then begin
;    if (obj_valid(self.oaDataObj[in_dataId])) then begin
;      obj_destroy, self.oaDataObj[in_dataId]
;      self.nDataIds = self.nDataIds - 1
;      retCode = self.ERROR_OK
;    endif else retCode = self->SayError(self.ERROR_BADDATAID)
;  endif else retCode = self->SayError(self.ERROR_BADDATAID)

;  if (obj_valid(self.oDataObj)) then obj_destroy, self.oDataObj
;  return, retCode
;END
;--------------------------------------------------------------------
;FUNCTION cvcDataAccess::Get, in_dataId, Time = in_sTimeSeg, Level = in_level, Region = in_sROI, out_sResult
;  
;  if (n_elements(in_dataId) eq 0) then return, self->SayError(self.ERROR_BADDATAID)
;  (self.oaDataObj[in_dataId])->GetProperty, sRequest = sCurrentRequest
;  if (n_elements(in_sROI) eq 0) then begin
;    in_sROI = {pPoints:ptr_new([[-180., 90.], [180., 90.], [180., -90.], [-180., -90.]]), pTiles:ptr_new()}
;  endif
;  if (n_elements(in_sTimeSeg) eq 0) then begin
;    in_sTimeSeg = sCurrentRequest.sTimeSeg
;  endif
;  if (n_elements(in_level) eq 0) then in_level = sCurrentRequest.aLevel[0]
;  
;  if ((in_dataId lt 0) or (in_dataId ge n_elements(self.oaDataObj))) then begin
;    retCode = self->SayError(self.ERROR_BADDATAID)
;  endif else begin  
;    catch, errorStatus
;    if (errorStatus ne 0) then begin
;      catch, /cancel
;      retCode = self->SayError(self.ERROR_ERRORINMODULE, className)
;    endif
;    aTimeRng = [in_sTimeSeg.beginning, in_sTimeSeg.ending]
;    if (ptr_valid(in_sROI.pPoints)) then aArea = *in_sROI.pPoints
;    if (ptr_valid(in_sROI.pTiles)) then aArea = *in_sROI.pTiles
;    retCode = (self.oaDataObj[in_dataId])->Read(aArea, aTimeRng, in_level, sResult)
;    catch, /cancel
;  endelse
;  
;  if (retCode eq self.ERROR_OK) then out_sResult = temporary(sResult) $
;  else out_sResult = ptr_new()
;  
;  return, retCode
;END
;--------------------------------------------------------------------
FUNCTION cvcDataAccess::Get, Time = in_sTimeSeg, Level = in_level, Region = in_sROI, UID = in_uid, out_sResult
  
  localData = *self.pData
  if (localData.type eq 'parameter') then begin
    if (n_elements(in_uid) ne 0) then begin
      pos = where(localData.asParam.uid eq in_uid)
      if (pos[0] ne -1) then out_sResult = localData.asParam[pos] else begin
        self->printLog, 'Warning! No parameter with such UID: '+in_uid
        return, -1
      endelse
    endif else out_sResult = localData.asParam
    return, self.ERROR_OK
  endif
  
  (self.oDataObj)->GetProperty, sRequest = sCurrentRequest
  if (n_elements(in_sROI) eq 0) then begin
    in_sROI = {pPoints:ptr_new([[-180., 90.], [180., 90.], [180., -90.], [-180., -90.]]), pTiles:ptr_new()}
  endif
  if (n_elements(in_sTimeSeg) eq 0) then begin
    in_sTimeSeg = sCurrentRequest.sTimeSeg
  endif
  if (n_elements(in_level) eq 0) then in_level = sCurrentRequest.aLevel[0]
  
  catch, errorStatus
  if (errorStatus ne 0) then begin
    catch, /cancel
    retCode = self->SayError(self.ERROR_ERRORINMODULE, self.className)
  endif
  aTimeRng = [in_sTimeSeg.beginning, in_sTimeSeg.ending]
  if (ptr_valid(in_sROI.pPoints)) then aArea = *in_sROI.pPoints
  if (ptr_valid(in_sROI.pTiles)) then aArea = *in_sROI.pTiles
  retCode = (self.oDataObj)->Read(aArea, aTimeRng, in_level, sResult)
  catch, /cancel

  if (retCode ne self.ERROR_OK) then begin
    out_sResult = obj_new()
    return, -1
  endif
  
  if ((*self.pData).tempk2c eq 'yes') then begin
    idxs = where(sResult.aData ne sResult.missingVal)
    sResult.aData[idxs] = sResult.aData[idxs] - 273.15
    sResult.minVal = sResult.minVal - 273.15
    sResult.maxVal = sResult.maxVal - 273.15
  endif

; Add variable units to the result
  sResult.units = localData.description.units
 
  out_sResult = temporary(sResult)
  
  return, retCode
END
;--------------------------------------------------------------------
FUNCTION cvcDataAccess::GetInfo, OUT_NTIMESEG=out_numTimeSeg, OUT_NLEVELS=out_numLevels, $
  OUT_TIMESEGS=out_asTimeSeg, OUT_LEVELS=out_aLevel, OUT_ROI=out_sROI
  
    localData = *self.pData

    if (localData.type eq 'parameter') then begin
      self->printLog, 'Warning! GetInfo function does not work with parameters.'
      return, self.ERROR_OK
    endif

    ; return number of time segements
    ;if (keyword_set(kw_getNTimeSeg)) then out_numTimeSeg = (*self.pInputs)[idx].numTimeSeg
    out_numTimeSeg = localData.numTimeSeg
    out_numLevels = localData.numLevels
    
    ; return specified time segment
;    if (n_elements(in_timeSegIdx) ne 0) then begin
;      if (in_timeSegIdx gt localData.numTimeSeg) then return, -1 ; time segment index is out of range
;      out_sTimeSeg = (localData.asTimeSeg)[in_timeSegIdx]
;    endif
    out_asTimeSeg = localData.asTimeSeg
    
    ; return specified level
;    if (n_elements(in_levelIdx) ne 0) then begin
;      if (in_levelIdx gt localData.numLevels) then return, -1 ; level index is out of range
;      out_level = (localData.aLevel)[in_LevelIdx]  
;    endif
     out_aLevel = localData.aLevel[0:localData.numLevels-1]  
    
    ; return ROI
    ;if (keyword_set(kw_getROI) ne 0) then out_sROI = (*self.pInputs)[idx].sRegion
    out_sROI = localData.sRegion 

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataAccess::Put, in_aData ;Time = in_sTimeSeg, Level = in_level, Region = in_sROI, out_sResult
  
    (*self.pData).data = ptr_new(in_aData, /no_copy)
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataAccess::SetInfo, DATA=in_aData, XGRID=in_aXgrid, YGRID=in_aYgrid, ZGRID=in_aZgrid, TIMEGRID=in_aTimeGrid, $
  MISSING=in_missingVal, GRIDTYPE=in_gridType, EXTRA=in_extra

    localData = *self.pData
    if (localData.type eq 'parameter') then begin
      self->printLog, 'Warning! SetInfo function does not work with parameters.'
      return, self.ERROR_OK
    endif

    if (n_elements(in_aXgrid) ne 0) then (*self.pData).pXgrid = ptr_new(in_aXgrid)
    if (n_elements(in_aYgrid) ne 0) then (*self.pData).pYgrid = ptr_new(in_aYgrid)
    if (n_elements(in_aZgrid) ne 0) then (*self.pData).pZgrid = ptr_new(in_aZgrid)
    if (n_elements(in_aTimeGrid) ne 0) then (*self.pData).pTimeGrid = ptr_new(in_aTimeGrid)
    if (n_elements(in_missingVal) ne 0) then (*self.pData).missingVal = in_missingVal
    if (n_elements(in_gridType) ne 0) then (*self.pData).gridType = in_gridType
    if (n_elements(in_aData) ne 0) then (*self.pData).pData = ptr_new(in_aData)
    if (n_elements(in_extra) ne 0) then (*self.pData).extra = ptr_new(in_extra)

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
;PRO cvcDataAccess::GetProperty, nDataIds = out_nDataIds
;  out_nDataIds = self.nDataIds
;END
;--------------------------------------------------------------------
PRO cvcDataAccess__define

    struct = { cvcDataAccess, $
                  INHERITS cvcError, $
                  oGlobal : obj_new(), $ ; global data
                  className : '', $ ; name of data access class
                  dataFileName : '', $ ; name of data file
                  pData : ptr_new(), $ ; data
                  oDataObj : obj_new() $ ; data access object
             }
END
