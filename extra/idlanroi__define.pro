;
;  NAME: 
;    IDLanROI
;
;  PURPOSE:
;    Replacement of IDL class for using in GDL
;
;  INHERITS:
;    
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION IDLanROI::Init, in_ROI
    
    size = size(in_ROI)
    if ((size[0] ne 2) or (size[1] lt 2) or (size[1] gt 3)) then begin
      print, 'ROI must be 2-dimensional array: 2xN or 3xN'
      return, 0
    endif
    
    self.nDims = size[1]
    self.nPts = size[2]
    
    self.ROI = ptr_new(in_ROI)
    
    return, 1
END
;--------------------------------------------------------------------
PRO IDLanROI::Cleanup
    if (ptr_valid(self.ROI)) then ptr_free, self.ROI
END
;--------------------------------------------------------------------
FUNCTION IDLanROI::ContainsPoints, in_X, in_Y

    nCheck1 = n_elements(in_X)
    nCheck2 = n_elements(in_Y)
    if (nCheck1 eq nCheck2) then nCheck = nCheck1 else return, -1
    
    ptinside = intarr(nCheck)

;    print, min(in_X), max(in_X)
;    print, min(in_Y), max(in_Y)
;    print, *self.ROI
    
    for pt = 0L, nCheck-1 do begin
      x = in_X[pt] & y = in_Y[pt]
      poly = *self.ROI
      p1x = poly[0, 0] & p1y = poly[1, 0]
      for i = 0L, self.nPts do begin
;          print, 'i=', i
;          print, 'p1:', p1x, p1y
          p2x = poly[0, i mod self.nPts]
          p2y = poly[1, i mod self.nPts]
;          print, y
;          print, 'p2:', p2x, p2y
          if (y gt min([p1y, p2y])) then begin
              if (y le max([p1y, p2y])) then begin
                  if (x lt max([p1x, p2x])) then begin
                      if (p1y ne p2y) then xinters = (y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x
                      if ((p1x eq p2x) or (x le xinters)) then ptinside[pt] = ~ptinside[pt]
;                      print, 'p1x=', p1x, ', p1y=', p1y, ', p2x=', p2x, ', p2y=', p2y
;                      print, 'x=', x, ', xinters=', xinters
;                      print, 'ptinside[', pt, ']=', ptinside[pt]
;                      if ((x eq -170) and (y eq 30)) then stop  
                  endif
               endif
          endif
          p1x = p2x
          p1y = p2y
;          print, 'p1:', p1x, p1y
      endfor
    endfor
    
;    stop
    return, ptinside
END
;--------------------------------------------------------------------
PRO IDLanROI__define
    
    struct = { IDLanROI, $
               ROI : ptr_new(), $
               nDims : 0, $
               nPts : 0 $
             }
END
