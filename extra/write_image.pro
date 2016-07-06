pro write_image, in_fileName, in_fileType, in_aImage, red = in_aRed, green = in_aGreen, blue = in_aBlue

  dataDims = size(in_aImage, /dim)

; check if all three channels are given
  if ((n_elements(in_aRed) ne 0) and (n_elements(in_aGreen) eq n_elements(in_aRed)) and (n_elements(in_aBlue) eq n_elements(in_aRed)) and (n_elements(dataDims) eq 2)) then begin
     aRGBImage = bytarr(dataDims[0], dataDims[1], 3)
     aRGBImage[*, *, 0] = in_aRed[in_aImage]
     aRGBImage[*, *, 1] = in_aGreen[in_aimage]
     aRGBImage[*, *, 2] = in_aBlue[in_aimage]
     image = reform(aRGBImage, 3, dataDims[1], dataDims[0])
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
  
   print, "Let's write image"
   res = python('myplot', 'write_image', in_fileName, in_fileType, float(image))  

;  print, 'Exit write_image'
end
