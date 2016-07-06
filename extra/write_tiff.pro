pro write_tiff, in_fileName, in_aImage, red = in_aRed, green = in_aGreen, blue = in_aBlue, $
                geotiff = in_sGeoKeys, float = in_kwFloat, planarconfig = in_plConf

  if (n_elements(in_sGeoKeys) ne 0) then begin
     keys = tag_names(in_sGeoKeys)
     nKeys = n_elements(keys)
     types = intarr(nKeys)
     lens = lonarr(nKeys)
     for i = 0, nKeys-1 do begin
        types[i] = size(in_sGeoKeys.(i), /type)
        if (types[i] ne 7) then begin
           item = in_sGeoKeys.(i) 
        endif else begin
           item = byte(in_sGeoKeys.(i))
        endelse
        lens[i] = n_elements(item)
        if (n_elements(vals) eq 0) then vals = [item] else vals = [vals, item]
     endfor
     keys = float(byte(keys))
     keys = reform(keys, nKeys, n_elements(keys)/nKeys)
  endif else begin
    keys = 0
    types = 0
    lens = 0
    vals = 0
  endelse

  if (keyword_set(in_kwFloat)) then fltflag = 1 else fltflag = 0

  dataDims = size(in_aImage, /dim)

;TODO: if PLANARCONFIG is given abort execution
  if (n_elements(in_plConf) eq 0) then plConf = 1 else plConf = in_plConf
  if (plConf ne 1) then plConf = 2 ; only 1 and 2 values are supported

; check if all three channels are given
  if ((n_elements(in_aRed) ne 0) and (n_elements(in_aGreen) eq n_elements(in_aRed)) and (n_elements(in_aBlue) eq n_elements(in_aRed)) and (n_elements(dataDims) eq 2)) then begin
     aRGBImage = bytarr(dataDims[0], dataDims[1], 4)
     aRGBImage[*, *, 0] = in_aRed[in_aImage]
     aRGBImage[*, *, 1] = in_aGreen[in_aimage]
     aRGBImage[*, *, 2] = in_aBlue[in_aimage]
;     aRGBImage[*, *, 3] = 255 * ((in_aRed[in_aImage] or in_aGreen[in_aImage] or in_aBlue[in_aImage]) ne 0)
     valid = (in_aRed[in_aImage] ne 255) or (in_aGreen[in_aImage] ne 255) or (in_aBlue[in_aImage] ne 255)
     aRGBImage[*, *, 3] = 255 * valid
     image = reform(aRGBImage, 4, dataDims[1], dataDims[0])
  endif else begin
     if (n_elements(dataDims) eq 2) then begin ; image with one channel as a 2-D array
        image = reform(in_aImage, dataDims[1], dataDims[0])
     endif else begin
        if (n_elements(dataDims) eq 3) then begin ; image with multiple channels as a 3-D array
           outImage = fltarr(dataDims[1], dataDims[2], dataDims[0])
           for k = 0, dataDims[0]-1 do begin
              outImage[*, *, k] = in_aImage[k, *, *]
           endfor
           image = reform(outImage, dataDims[0], dataDims[2], dataDims[1])
        endif else begin
           print, 'Only 2-D and 3-D images are accepted. Aborting.'
           return
        endelse
     endelse
  endelse


;  image = reform(image, 3*dataDims[1]*dataDims[0])
  
  if (n_elements(in_sGeoKeys) ne 0) then begin
    res = python('myplot', 'write_geotiff', in_fileName, float(image), float(keys), float(types), float(lens), float(vals), fltflag)
  endif else begin
    res = python('myplot', 'write_tiff', in_fileName, float(image), fltflag)  
  endelse    
;  print, 'Exit write_tiff'
end
