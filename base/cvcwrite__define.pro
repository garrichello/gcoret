;
;  NAME: 
;    cvcWrite
;
;  PURPOSE:
;    High-level base class. Provides writing of results.
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcWrite::Init, in_oGlobal

    self.oGlobal = in_oGlobal
    hLogFile = self.oGlobal->Get('hLogFile')
    res = self->cvcError::Init(hLogFile)
    self.oConsts = obj_new('cvcConstants', in_oGlobal)
    self.hLogFile = hLogFile

    return, 1
END
;--------------------------------------------------------------------
PRO cvcWrite::Cleanup
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::__WriteNetCDF, in_sData, in_sDestDesc
    
    self->printLog, '(cvcWrite) Writing netCDF...'

    sData = *(in_sData[0]).data    
    
    nData = n_elements(in_sData)
    gridType = sData.gridType
    
; check if all datasets are of the same kind
    for i = 1, nData-1 do begin
      if ((*in_sData[i].data).gridType ne gridType) then begin
        self->printLog, 'Error: All data MUST be have the same grid to be written into one file!'
        return, -1
      endif
    endfor
  
; open file
    self->printLog, '(cvcWrite) Open file'
    id = ncdf_create(in_sDestDesc.file.name, /clobber)
    
; for data fields
    if ((gridType eq 'regular') OR (gridType eq 'irregular')) then begin
      self->printLog, '(cvcWrite) Grid is ' + gridType +'. Checking datasets...', format='(a, $)'
      dataSize = size(*sData.data)

      for i = 1, nData-1 do begin
        sDatai = (*(in_sData[i]).data)
        ; check if all datasets are of the same size
        if ((n_elements(*sDatai.data) ne n_elements(*sData.data)) or $
            (n_elements(*sData.pXgrid) ne n_elements(*sDatai.pXgrid)) or $
            (n_elements(*sData.pYgrid) ne n_elements(*sDatai.pYgrid))) then begin
          self->printLog, 'Error: All data MUST be of the same size and have the same lon-lat grid to be written into one file!'
          return, -1
        endif
      endfor
      self->printLog, 'OK', /notimestamp
      
      self->printLog, '(cvcWrite) Define lon/lat dimensions and dimension variables'
; longitude variable
      if (ptr_valid(sData.pXgrid) and ptr_valid(sData.pYgrid)) then begin
        lonGridSize = size(*sData.pXgrid)
        latGridSize = size(*sData.pYgrid)
        if (lonGridSize[0] ne latGridSize[0]) then begin
          self->printLog, '(cvcWrite::__WriteNetCDF) Error: XGrid and YGrid dimensions are not equal!'
          return, -1
        endif
        gridSize = lonGridSize[0]

        ; 1-D grid variables
        if (gridSize eq 1) then begin
          self->printLog, '(cvcWrite) 1-D grids are used'
          ; lon-lat dimensions
          if (n_elements(*sData.pXgrid) eq dataSize[1]) then begin
            xid = ncdf_dimdef(id, 'lon', dataSize[1])
          endif else begin
            self->printLog, '(cvcWrite::__WriteNetCDF) Error: X grid does not correspond data!'
            return, -1
	  endelse
	  
          if (n_elements(*sData.pYgrid) eq dataSize[2]) then begin
            yid = ncdf_dimdef(id, 'lat', dataSize[2])
          endif else begin
            self->printLog, '(cvcWrite::__WriteNetCDF) Error: Y grid does not correspond data!'
            return, -1
	  endelse

          lonid = ncdf_vardef(id, 'lon', [xid], /float)
          latid = ncdf_vardef(id, 'lat', [yid], /float)
        endif
        
        ; 2-D grid variables
        if (gridSize eq 2) then begin
          self->printLog, '(cvcWrite) 2-D grids are used'
          xid = ncdf_dimdef(id, 'xc', dataSize[1])
          yid = ncdf_dimdef(id, 'yc', dataSize[2])
          xcvarid = ncdf_vardef(id, 'xc', [xid], /float)
          ycvarid = ncdf_vardef(id, 'yc', [yid], /float)
          
          lonid = ncdf_vardef(id, 'lon', [xid, yid], /float)
          latid = ncdf_vardef(id, 'lat', [xid, yid], /float)
        endif

        ; bad grid variables
        if (gridSize gt 2) then begin
          self->printLog, '(cvcWrite::__WriteNetCDF) Error: XGrid and YGrid have more than 2 dimensions'
          return, -1
        endif
        
        ncdf_attput, id, lonid, 'units', 'degrees_east'
        ncdf_attput, id, lonid, 'long_name', 'Longitude'
        ncdf_attput, id, latid, 'units', 'degrees_north'
        ncdf_attput, id, latid, 'long_name', 'Latitude'
        aDimIDs = [xid, yid]
      endif

; level dimension
      if (ptr_valid(sData.pZgrid)) then begin
        self->printLog, '(cvcWrite) Define level dimension and variable'
        zid = ncdf_dimdef(id, 'lev', n_elements(*sData.pZgrid))
        levid = ncdf_vardef(id, 'lev', [zid], /float)
        ncdf_attput, id, levid, 'units', '-'
        ncdf_attput, id, levid, 'long_name', 'Level'
        if (n_elements(*sData.pZgrid) gt 1) then aDimIDs = [aDimIDs, zid] 
      endif
; time dimension
      if (ptr_valid(sData.pTimeGrid)) then begin
        self->printLog, '(cvcWrite) Define time dimension and variable'
        tid = ncdf_dimdef(id, 'time', n_elements(*sData.pTimeGrid))
        timeid = ncdf_vardef(id, 'time', [tid], /double)
        ncdf_attput, id, timeid, 'units', 'hours since 1979-1-1 00:00:0.0'
        ncdf_attput, id, timeid, 'long_name', 'Time'
        if (n_elements(*sData.pTimeGrid) gt 1) then aDimIDs = [aDimIDs, tid] 
      endif
; global attributes
      self->printLog, '(cvcWrite) Write global attributes'
      ncdf_attput, id, /global, 'Title', sData.description.title
      ncdf_attput, id, /global, 'Conventions', 'CF'
      ncdf_attput, id, /global, 'Source', 'Generated by CLEARS system'
      
; define data variables and attributes
      self->printLog, '(cvcWrite) Define data variables and attributes'
      dataId = intarr(nData)
      for iData = 0, nData-1 do begin
        sDatai = (*in_sData[iData].data)
        dataId[iData] = ncdf_vardef(id, 'data'+strtrim(string(iData), 2), aDimIDs, /float)
        ncdf_attput, id, dataId[iData], 'units', sDatai.description.units
        ncdf_attput, id, dataId[iData], 'long_name', sDatai.description.name
        ncdf_attput, id, dataId[iData], 'missing_value', sDatai.missingVal
        ncdf_attput, id, dataId[iData], 'coordinates', 'lon lat'
      endfor
      
; put file in data mode
      self->printLog, '(cvcWrite) Put file in data mode'
      ncdf_control, id, /endef

; put variables
      self->printLog, '(cvcWrite) Write grids'
      ncdf_varput, id, lonid, *sData.pXgrid
      ncdf_varput, id, latid, *sData.pYgrid

      if (ptr_valid(sData.pZgrid)) then begin
        for i = 0, n_elements(*sData.pZgrid)-1 do begin
          if (size((*sData.pZgrid)[i], /type) eq 7) then begin ; string type
             case (*sData.pZgrid)[i] of
                'sfc': curlev = 0
                '2m': curlev = 1
                '10m' : curlev = 2
                else: begin
                   self->printLog, 'Unrecognized level string value: '+(*sData.pZgrid)[i]+'. Trying to convert it to integer. Otherwise defalut = 0 will be used'
                   curlev = fix((*sData.pZgrid)[i])
                end
             endcase
          endif else curlev = (*sData.pZgrid)[i]
          if (i eq 0) then levs = curlev else levs = [levs, curlev]
       endfor
        ncdf_varput, id, levid, levs
      endif

      if (ptr_valid(sData.pTimeGrid)) then begin
        ncdf_varput, id, timeid, (*sData.pTimeGrid - julday(1, 1, 1979, 0, 0))*24.0
      endif
; put data
      self->printLog, '(cvcWrite) Write data...', format='(a, $)'
      for iData = 0, nData-1 do begin
        ncdf_varput, id, dataId[iData], *(*(in_sData[iData]).data).data
      endfor
      self->printLog, 'OK', /notimestamp
    endif

; for stations
    if (gridType eq 'station') then begin ; we have a set of stations
      self->printLog, '(cvcWrite) Grid type is '+ gridType
      self->printLog, '(cvcWrite) Define dimensions and attributes'
      ; define dimensions
      stid = ncdf_dimdef(id, 'station', /unlimited)
      ; define variables
      lonid = ncdf_vardef(id, 'longitude', [stid], /float)
      latid = ncdf_vardef(id, 'latitude', [stid], /float)
      dataid = ncdf_vardef(id, 'data', [stid], /float)
      ; define attributes
      ncdf_attput, id, lonid, 'units', 'degrees_east'
      ncdf_attput, id, lonid, 'long_name', 'station longitude'
      ncdf_attput, id, latid, 'units', 'degrees_north'
      ncdf_attput, id, latid, 'long_name', 'station latitude'
      ncdf_attput, id, dataid, 'units', sData.description.units
      ncdf_attput, id, dataid, 'long_name', sData.description.name
      ncdf_attput, id, dataid, 'coordinates', 'lat lon'    
      ncdf_attput, id, /global, 'Title', sData.description.title
      ; put file in data mode
      self->printLog, '(cvcWrite) Put file in data mode'
      ncdf_control, id, /endef
      ; concatenate data
      xGrid = *sData.pXgrid
      yGrid = *sData.pYgrid
      data = *sData.data
      for iData = 1, nData-1 do begin
        sDatai =  *(in_sData[iData]).data
        xGrid = [xGrid, *sDatai.pXgrid]
        yGrid = [yGrid, *sDatai.pYgrid]
        data = [data, *sDatai.data]
      endfor
      ; put data
      self->printLog, '(cvcWrite) Write data...', format='(a, $)'
      ncdf_varput, id, lonid, xGrid
      ncdf_varput, id, latid, yGrid
      ncdf_varput, id, dataid, data
      self->printLog, 'OK', /notimestamp
    endif
  
    if (gridType eq 'points') then begin ; we have a set of lines
      self->printLog, '(cvcWrite) Grid type is ' + gridType
      ; calculate number of lines and number of points
      nLines = 0
      nPoints = 0
      for iLine = 0, nData-1 do begin
        sDatai = (*(in_sData[iLine]).data)
        dataSize = size(*sDatai.pXgrid)
        if (dataSize[0] eq 1) then begin
          nLines = nLines + 1
          nPoints = max([nPoints, dataSize[1]])
        endif else $
        if (dataSize[0] eq 2) then begin
          nLines = nLines + dataSize[1]
          nPoints = max([nPoints, dataSize[2]])
        endif else begin
          self->printLog, '(cvcWrite::__WriteNetCDF) Error: Too much dimensions specfied to write simple lines'
          return, -1
        endelse
      endfor
      ; define dimensions
      lineid = ncdf_dimdef(id, 'line', nLines)
      pointid = ncdf_dimdef(id, 'point', nPoints)
      ; define variables
      xid = ncdf_vardef(id, 'x', [pointid, lineid], /float)
      yid = ncdf_vardef(id, 'y', [pointid, lineid], /float)
        ; define attributes
      ; put file in data mode
      ncdf_control, id, /endef
      ; put data
      self->printLog, '(cvcWrite) Write data...', format='(a, $)'
      iPos = 0
      for iLine = 0, nData-1 do begin
        dataSize = size(*(*(in_sData[iLine]).data).pXgrid)
        if (dataSize[0] eq 1) then begin
          sDatai = (*(in_sData[iLine]).data)
          ncdf_varput, id, xid, *sDatai.pXgrid, offset=[0, iPos]
          ncdf_varput, id, yid, *sDatai.pYgrid, offset=[0, iPos]
          iPos = iPos + 1        
        endif else $
        if (dataSize[0] eq 2) then begin
          for iSet = 0, dataSize[1]-1 do begin
            sDatai = (*(in_sData[iLine]).data)
            ncdf_varput, id, xid, ((*sDatai.pXgrid)[iSet, *])[*], offset=[0, iPos]
            ncdf_varput, id, yid, ((*sDatai.pYgrid)[iSet, *])[*], offset=[0, iPos]
            iPos = iPos + 1
          endfor
        endif
      endfor
      self->printLog, 'OK', /notimestamp
    endif
    
; close file

    self->printLog, '(cvcWrite) Closing file...', format='(a, $)'
    ncdf_close, id
    self->printLog, 'OK', /notimestamp

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::__WriteGeoTIFF, in_sData, in_sDestDesc

    sData = *in_sData[0].data
    gridType = sData.gridType
    if ((gridType ne 'regular') and (gridType ne 'irregular') and (~ptr_valid(sData.extra))) then begin
      self->printLog, 'Error: GeoTIFF is only for field data!'
      return, -1
    endif
    
    if (~ptr_valid(sData.extra)) then begin
      minLon = min(*sData.pXgrid, max = maxLon)
      minLat = min(*sData.pYgrid, max = maxLat)
      sizes = size(*sData.data)
      imageXsize = sizes[1]
      imageYsize = sizes[2]
  
      xScale = (maxLon - minLon) / (imageXsize - 1)
      yScale = (maxLat - minLat) / (imageYsize - 1)
  
  ; prepare geokeys    
      ModelTypeGeographic  = 2   ;/* Geographic latitude-longitude System */
      RasterPixelIsArea  = 1
      GCS_WGS_84 =  4326
      
      xGrid = (*sData.pXgrid)
      if (maxLon gt 180) then xGrid = (xGrid + 180.) mod 360. -180.
      
      dataSize = size(*sData.data)
      modelTPT = dblarr(6*dataSize[1]*dataSize[2])
      
      for i = 0, dataSize[1]-1 do begin
        for j = dataSize[2]-1, 0, -1 do begin
          idx = (i*dataSize[2] + j)*6
          modelTPT[idx+0] = double(i)
          modelTPT[idx+1] = double(j)
          modelTPT[idx+2] = 0.0D
          modelTPT[idx+3] = xGrid[i]
          modelTPT[idx+4] = (*sData.pYgrid)[j]
          modelTPT[idx+5] = 0.0D
        endfor
      endfor
  
      geokeys = { MODELPIXELSCALETAG : [ xScale, yScale, 0.0D ], $
                  MODELTIEPOINTTAG : modelTPT, $
                  GTMODELTYPEGEOKEY : ModelTypeGeographic, $
                  GTRASTERTYPEGEOKEY : RasterPixelIsArea, $
                  GEOGRAPHICTYPEGEOKEY : GCS_WGS_84 $
                }
    endif else geokeys = *sData.extra
; write geotiff    
    write_tiff, in_sDestDesc.file.name, *sData.data, geotiff = geokeys, /float
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::__WriteBin, in_sData, in_sDestDesc

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::__WriteXML, in_sData, in_sDestDesc

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::__WriteASCII, in_sData, in_sDestDesc

    monNames = ['January', 'February', 'March', 'April', 'May', 'June', 'July', 'August', 'September', 'October', 'November', 'December']
    nInputs = n_elements(in_sData)
    
    for i = 0, nInputs-1 do $
      if ((*in_sData[i].data).type eq 'parameter') then begin ; find  parameter input
        sParam = *in_sData[i].data ; parameter input structure
        break
      endif

    nData = nInputs - 1 ; number of data inputs
    dataIdxs = intarr(nData, /nozero) ; array of indices of data inputs
    iData = 0
    for i = 0, nData-1 do begin ; find indices of data inputs and put them into dataIdxs array
      if ((*in_sData[i].data).type eq 'array') then begin
        dataIdxs[iData] = i
        iData += 1
      endif
    endfor

; get format string
    for iParam = 0, sParam.numParams-1 do $
      if (sParam.asParam[iParam].UID eq 'format') then begin
        fmtString = *sParam.asParam[iParam].data
        break
      endif

; open output file
    openw, outLUN, in_sDestDesc.file.name, /get_lun

; print data
    for iData = 0, nData-1 do begin ; main loop by data arrays
      dataIdx = dataIdxs[iData]

      dataName = (*in_sData[dataIdx].data).description.name ; get data name
      aData = *(*in_sData[dataIdx].data).data ; get current data input
      sz = size(aData, /dim) ; get data array dimensions
      nLons = sz[0]
      nLats = sz[1]
      nTimes = sz[2]

      printf, outLUN, dataName+': ', format = '(a50,$)'
      for iLats = 0, nLats-1 do begin
        for iTimes = 0, nTimes-1 do begin
      	  for iLons = 0, nLons-1 do begin
	    printf, outLUN, aData[iLons, iLats, iTimes], format='('+fmtString+',$)'
          endfor
	  printf, outLUN, '   ', format='(a3,$)'
        endfor
	printf, outLUN
      endfor
    endfor ; loop by data arrays

; close output file
    free_lun, outLun

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::__CheckGrid, in_sData 
    
    if (ptr_valid(in_sData.pData)) then dataSize = size(*in_sData.pData)
    if (ptr_valid(in_sData.pXgrid)) then xSize = size(*in_sData.pXgrid)
    if (ptr_valid(in_sData.pYgrid)) then ySize = size(*in_sData.pYgrid)
    if (ptr_valid(in_sData.pZgrid)) then zSize = size(*in_sData.pZgrid)
    if (ptr_valid(in_sData.pTimegrid)) then timeSize = size(*in_sData.pTimeGrid)
    gridType = in_sData.gridType
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcWrite::Run, in_pInputs


    ; destination description
    idx = where((*in_pInputs).type eq 'destination')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: No destination!'
      return, -1
    endif
    sDestDesc = *((*in_pInputs)[idx[0]].data)
    
    ; data to write
    idx = where((*in_pInputs).type eq 'data')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: Nothing to write!'
      return, -1
    endif
    sData = (*in_pInputs)[idx]

    ; modify data
    valIdxs = where(*(*sData.data).data ne (*sData.data).missingVal, cntVal)
    if (cntVal gt 0) then begin
      scale = (*sData.data).modify.scale
      offset = (*sData.data).modify.offset
      (*(*sData.data).data)[valIdxs] = scale*(*(*sData.data).data)[valIdxs] + offset
    endif
        
    if (sDestDesc.file.name ne '') then begin
      outFileType = sDestDesc.file.type 
    endif else begin
      self->printLog, 'Error: No file name specified!'
      return, -1
    endelse
    
    case outFileType of
      'netcdf': ret = self->__WriteNetCDF(sData, sDestDesc)
      'shape': ret = self->__WriteShp(sData, sDestDesc)
      'geotiff': ret = self->__WriteGeoTIFF(sData, sDestDesc)
      'bin': ret = self->__WriteBin(sData, sDestDesc)
      'xml': ret = self->__WriteXML(sData, sDestDesc)
      'ascii': ret = self->__WriteASCII(sData, sDestDesc)
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcWrite__define

    MAX_N_RESULTS = 128

    struct = { cvcWrite, $
		  oGlobal : obj_new(), $ ; global data
                  oConsts : OBJ_NEW(), $
                  INHERITS cvcError $
             }
END
