function trigrid2, in_aX, in_aY, in_aData, in_aTriangles, nx=in_nx, ny=in_ny, $
 xgrid=in_aXOut, ygrid=in_aYOut, min_value=in_minVal, max_value=in_maxVal, missing=in_aMissing
 
  print, 'New trigrid is running...'
  
  xOutSize = n_elements(in_aXOut)
  yOutSize = n_elements(in_aYOut)
  aData = fltarr(xOutSize, yOutSize)

  aX = reform(in_aX, n_elements(in_aX))
  aY = reform(in_aY, n_elements(in_aY))
  aData = reform(in_aData, n_elements(in_aData))
  help, aX
  help, aY
  help, aData
  help, in_aXOut
  help, in_aYOut
  
  aGridData = python('common', 'trigrid', aX, aY, aData, in_aXOut, in_aYOut)

  return, aGridData
end