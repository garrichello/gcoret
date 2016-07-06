; griddata(xRq[validx], yRq[validx], IData[validx], /linear, triangles = tr, xout = [reLons], yout = [reLats], /grid)
function griddata, in_aX, in_aY, in_aData, linear=in_linear, triangles=in_aTriangles, xout=in_aXOut, yout=in_aYOut, grid=in_grid
 
  print, 'New griddata is running...'
  
  aX = reform(in_aX, n_elements(in_aX))
  aY = reform(in_aY, n_elements(in_aY))
  aData = reform(in_aData, n_elements(in_aData))
  help, aX
  help, aY
  help, aData
  help, in_aXOut
  help, in_aYOut
  help, in_aTriangles
  
;  if (n_elements(in_aTriangles) eq 0) then begin
    aGridData = python('common', 'mygriddata', aX, aY, aData, in_aXOut, in_aYOut, in_linear)
;  endif
  
;  print, aGridData
  
  return, aGridData
end