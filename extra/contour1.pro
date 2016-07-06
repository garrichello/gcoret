pro contour1, in_aData, in_aX, in_aY, nlevels = in_nlev, min_value = in_minval, max_value = in_maxval, $
              path_info = out_sPath, path_xy = out_aVerts, path_data_coords = kw_pdc, $
              path_double = kw_pathdouble, xstyle = in_xstyle, ystyle = in_ystyle, closed = kw_closed


  dims = size(in_aData, /dim)
  aData = reform(in_aData, dims[1], dims[0])
  aX = float(in_aX)
  aY = float(in_aY)

  if (n_elements(in_minval) eq 0) then minVal = min(aData) else minVal = in_minval
  if (n_elements(in_maxval) eq 0) then maxVal = max(aData) else maxVal = in_maxval

  idx = where(aData lt minVal)
  if (idx[0] ne -1) then aData[idx] = -999.0 ; minVal

  idx = where(aData gt maxVal)
  if (idx[0] ne -1) then aData[idx] = -999.0 ; maxVal

  aPaths = python('myplot', 'contour', aData, aX, aY, in_nlev, minVal, maxVal)
  pathDims = size(aPaths, /dim)
  aPaths = reform(aPaths, pathDims[1], pathDims[0])
;  help, aPaths

  aLens = python('myplot', 'get_contour_lens')
  aLens = long(aLens)
;  help, aLens

  aVals = python('myplot', 'get_contour_levs')
  nVals = n_elements(aVals)
;  help, aVals

  out_aVerts = aPaths

  if (keyword_set(kw_pathdouble)) then begin
    out_sPath = make_array(nVals, value={CONTOUR_DBL_PATH_STRUCTURE, TYPE:0B, HIGH_LOW:0B, $
                                          LEVEL:0, N:0L, OFFSET:0L, VALUE:0.0D})
  endif else begin 
    out_sPath = make_array(nVals, value={CONTOUR_PATH_STRUCTURE, TYPE:0B, HIGH_LOW:0B, $ 
                                          LEVEL:0, N:0L, OFFSET:0L, VALUE:0.0})
  endelse

  offset = 0
  for i = 0, nVals-1 do begin
    out_sPath[i].level = i
    out_sPath[i].value = aVals[i]
    out_sPath[i].n = aLens[i]
    out_sPath[i].offset = offset
    offset = offset + aLens[i]
  endfor

end
