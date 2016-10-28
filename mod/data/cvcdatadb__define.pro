;
;  NAME: 
;    cvcDataDB
;
;  PURPOSE:
;    Low-level class. Provides access to meteo- and climatic data. Works with PostGIS database. 
;
;  INHERITS:
;    cvcData
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDataDB::Init, in_pDataDesc, in_modulePath, in_oGlobal
    
    if ( self->cvcData::Init(in_pDataDesc, in_modulePath, in_oGlobal) eq 0 ) then return, 0
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDataDB::Cleanup
    res = python('mypgdb', 'close_db', self.db_name, self.user_name)
    
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
FUNCTION cvcDataDB::Read, in_aArea, in_aTimeRng, in_level, out_sData

    missingVal = -999.0
    code_type = 'string'
    name_type = 'string'
    loc_type = 'location'
    data_type = 'float'
    date_type = 'date'

    var_name = self.sRequest.varName
    roi_poly = string('(', format='(a)')
    for i = 0L, n_elements(in_aArea[0, *])-1 do begin
      roi_poly += string(in_aArea[0, i], in_aArea[1, i], ',', format='(2f9.3, a)')
    endfor
    roi_poly += string(in_aArea[0, 0], in_aArea[1, 0], ')', format='(2f9.3, a)')
;    str = strtrim(in_aTimeRng[0], 2) & date_start = strmid(str, 0, 4)+'-'+strmid(str, 4, 2)+'-'+strmid(str, 6, 2)
;    str = strtrim(in_aTimeRng[1], 2) & date_end = strmid(str, 0, 4)+'-'+strmid(str, 4, 2)+'-'+strmid(str, 6, 2)
    date_start = strtrim(in_aTimeRng[0]/100, 2)
    date_end = strtrim(in_aTimeRng[1]/100, 2)

; query stations data
    self->printLog, '(cvcDataDB) Quering data...'

;    name_pos = 0
;    loc_pos = 1
    data_pos = 0
;    time_pos = 3
;    qry = "select "+self.st_table_name+".st_name, ST_asText("+self.st_table_name+".location), "+ $
;	   self.data_table_name+"."+var_name+", "+self.data_table_name+".date from "+self.data_table_name+" join "+ $
;	   self.st_table_name+" on "+self.data_table_name+".station="+self.st_table_name+".station where ST_Covers('POLYGON("+ $
;	   roi_poly+")', ST_asText("+self.st_table_name+".location)) and "+self.data_table_name+".date >= '"+date_start+"' and "+ $
;	   self.data_table_name+".date <= '"+date_end+"';"
    qry = "select "+self.data_table_name+"."+var_name+" from "+self.data_table_name+" join "+ $
	   self.st_table_name+" on "+self.data_table_name+".station="+self.st_table_name+".station where ST_Covers('POLYGON("+ $
	   roi_poly+")', ST_asText("+self.st_table_name+".location)) and "+self.data_table_name+".date >= '"+date_start+"' and "+ $
	   self.data_table_name+".date <= '"+date_end+"';"
    nrecs = python('mypgdb', 'query_db', qry)

; get stations data
    self->printLog, '(cvcDataDB) Processing stations data...'
    st_data = python('mypgdb', 'get_data', data_pos, data_type)
    self->printLog, '(cvcDataDB) ', strtrim(string(n_elements(st_data)), 2), ' records are extracted from DB'

; query dates/times
    self->printLog, '(cvcDataDB) Quering times...'
    time_pos = 0
    qry = "select distinct date from "+self.data_table_name+" where date >= '"+date_start+"' and date <= '"+date_end+"' order by date;"
    nrecs = python('mypgdb', 'query_db', qry)
    
; get dates/times
    self->printLog, '(cvcDataDB) Processing dates...'
    st_dates = python('mypgdb', 'get_data', time_pos, date_type)
    st_dates_yr = long(st_dates / 10000)
    st_dates_mm = long(st_dates/100-st_dates_yr*100)
    st_dates_dd = long(st_dates-st_dates_yr*10000-st_dates_mm*100)
    aTimes = julday(st_dates_mm, st_dates_dd, st_dates_yr)
    n_times = n_elements(aTimes)

;    dims = size(st_times, /dim)
;    st_times = fix(reform(st_times, dims[1], dims[0]))
;    n_times = dims[0]
;    aTimes = transpose(julday(st_times[1, *], st_times[2, *], st_times[0, *], st_times[3, *], st_times[4, *]))
;    st_times = 0 ; free mem

; query stations names, coordinates and codes
    self->printLog, '(cvcDataDB) Quering stations names and locations...'
    name_pos = 0
    loc_pos = 1
    code_pos = 2
    qry = "select st_name, ST_asText(location), station from "+self.st_table_name+" where ST_Covers('POLYGON("+roi_poly+")', ST_asText(location));"
    nrecs = python('mypgdb', 'query_db', qry)

; get locations
    self->printLog, '(cvcDataDB) Processing stations coordinates...'
    st_locs = python('mypgdb', 'get_data', loc_pos, loc_type)
    dims = size(st_locs, /dim)
    st_locs = reform(st_locs, dims[1], dims[0])

; get stations names
    self->printLog, '(cvcDataDB) Processing stations names...'
    st_bulk_names = python('mypgdb', 'get_data', name_pos, name_type)
    st_name_len = python('mypgdb', 'get_elem_len')
    st_num = n_elements(st_bulk_names) / st_name_len
    aNames = strarr(st_num)
    for i = 0L, st_num-1 do aNames[i] = string(st_bulk_names[i*st_name_len:(i+1)*st_name_len-1])

; get stations codes
    self->printLog, '(cvcDataDB) Processing stations codes...'
    st_bulk_codes = python('mypgdb', 'get_data', code_pos, code_type)
    st_code_len = python('mypgdb', 'get_elem_len')
    st_num = n_elements(st_bulk_codes) / st_code_len
    aCodes = strarr(st_num)
    for i = 0L, st_num-1 do aCodes[i] = string(st_bulk_codes[i*st_code_len:(i+1)*st_code_len-1])

    self->printLog, '(cvcDataDB) Prepare output arrays...'

; set lon/lat grids
    aLats = transpose(st_locs[1, *])
    aLons = transpose(st_locs[0, *])

; reform data array
    aAllData = transpose(reform(st_data, n_times, st_num))

; search for min/max values
    self->printLog, '(cvcDataDB) Searching for min/max values...'
    goodidx = where(aAllData ne missingVal, goodcnt)
    if (goodidx[0] ne -1) then begin
      minVal = min(aAllData[goodidx], max=maxVal) 
    endif else begin
      minVal = missingVal
      maxVal = missingVal
    endelse

; apply scale/offset factors
    if (goodcnt gt 0) then if ((self.modify.scale ne 1.0) or (self.modify.offset ne 0.0)) then aAllData[goodidxs] = self.modify.scale*aAllData[goodidxs] + self.modify.offset

    out_sData = { aData : temporary(aAllData), $
                  aLons : aLons, $
                  aLats : aLats, $
                  aTimes : aTimes, $
                  aNames : aNames, $
                  aCodes : aCodes, $
                  minVal : minVal, $
                  maxVal : maxVal, $
                  missingVal : missingVal, $
                  sTimeRng : {structTimeRng, beginning:in_aTimeRng[0], ending:in_aTimeRng[1], step:self.sRequest.sTimeSeg.step}, $
                  lev : in_level, $
                  aLev : [in_level], $
                  nLev : 1, $
                  gridType : 'station', $
		  units : '', $ ; this field is filled in the cvcDataAccess::Get() method
                  resultCode : self.ERROR_OK $
                }

    self->printLog, '(cvcDataDB) Finished'

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcDataDB::Prepare, in_varName, in_asTimeSeg, in_aLevel, in_sRegion

    self.sRequest.varName = in_varName
    self.sRequest.asTimeSeg = in_asTimeSeg
    self.sRequest.numTimeSeg = n_elements(in_asTimeSeg)
    self.sRequest.aLevel = in_aLevel
    self.sRequest.nLev = n_elements(in_aLevel)

; fileTemplate is prepared in cvcDataAccess::Open()
    fileTemplate = self.dataset.fileTemplate; ;;;self.oGlobal->Get('fileTemplate')
    fileTemplate = strmid(fileTemplate, 1, strlen(fileTemplate)-2) ; remove leading and trailing quotes
    
    ; split fileTemplate into rootpath (database connection info) and filepath (tables' names)
    spl_path = strsplit(fileTemplate, path_sep(), /extract)
    rootpath = spl_path[0]
    filepath = spl_path[3]

    ; split rootpath to extract info
    spl_uphd = strsplit(rootpath, '@', /extract) ; splitted into (user:passwd) @ (host:db_name)
    spl_up = strsplit(spl_uphd[0], ':', /extract) ; splitted into (user) : (passwd)
    spl_hd = strsplit(spl_uphd[1], ':', /extract) ; splitted into (host) : (db_name)
    
    self.user_name = spl_up[0] ; user name
    if (n_elements(spl_up) eq 2) then self.passwd = spl_up[1] ; password
    if (n_elements(spl_hd) eq 3) then begin
	self.host = spl_hd[0]+':'+spl_hd[1] ; host name
	self.db_name = spl_hd[2] ; database name
    endif else begin
	self.host = spl_hd[0] ; host name
	self.db_name = spl_hd[1] ; database name
    endelse
    
    spl_id = strsplit(filepath, ':', /extract) ; splitted into (stations info table) : (stations data table) 
    self.st_table_name = spl_id[0] ; table name with station names and coordinates
    self.data_table_name = spl_id[1] ; table name with station data

    res = python('mypgdb', 'connect_db', self.db_name, self.user_name)

    if (res eq 0) then begin
      self->printLog, 'Connected to DB'
    endif else begin
      self->printLog, 'Error connecting to DB!'
      self.prepared = 0
      return, -1
    endelse

    self.prepared = 1
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcDataDB__define

    struct = { cvcDataDB, $
	       host : "", $
	       db_name : "", $
	       st_table_name : "", $
	       data_table_name : "", $
	       user_name : "", $
	       passwd : "", $
               INHERITS cvcData $
             }
END
