PRO VELOVECT2,U,V,X,Y, Missing = Missing, Length = length, Dots = dots,  $
        CLIP=clip, NOCLIP=noclip, OVERPLOT=overplot, ARROWS=arrows, MAGNITUDE=magnitude, $
        _REF_EXTRA=extra

COMPILE_OPT strictarr

        on_error,2                      ;Return to caller if an error occurs
        s = size(u)
        t = size(v)
        if s[0] ne 2 then begin
baduv:   message, 'U and V parameters must be 2D and same size.'
                endif
        if total(abs(s[0:2]-t[0:2])) ne 0 then goto,baduv
;
        if n_params(0) lt 3 then x = findgen(s[1]) else $
                if n_elements(x) ne s[1] then begin
badxy:                  message, 'X and Y arrays have incorrect size.'
                        endif
        if n_params(1) lt 4 then y = findgen(s[2]) else $
                if n_elements(y) ne s[2] then goto,badxy
;
        if n_elements(missing) le 0 then missing = 1.0e30
        if n_elements(length) le 0 then length = 1.0

        mag = sqrt(u^2.+v^2.)             ;magnitude.
                ;Subscripts of good elements
        nbad = 0                        ;# of missing points
        if n_elements(missing) gt 0 then begin
                good = where(mag lt missing)
                if keyword_set(dots) then bad = where(mag ge missing, nbad)
        endif else begin
                good = lindgen(n_elements(mag))
        endelse

        ugood = u[good]
        vgood = v[good]
        x0 = min(x)                     ;get scaling
        x1 = max(x)
        y0 = min(y)
        y1 = max(y)
    x_step=(x1-x0)/(s[1]-1.0)   ; Convert to float. Integer math
    y_step=(y1-y0)/(s[2]-1.0)   ; could result in divide by 0

    maxmag=max([max(abs(ugood/x_step)),max(abs(vgood/y_step))])
    sina = length * (ugood/maxmag)
    cosa = length * (vgood/maxmag)
;
        if n_elements(title) le 0 then title = ''
        ;--------------  plot to get axes  ---------------
        if n_elements(noclip) eq 0 then noclip = 1
        x_b0=x0-x_step
    x_b1=x1+x_step
    y_b0=y0-y_step
    y_b1=y1+y_step
        if (not keyword_set(overplot)) then begin
          if n_elements(position) eq 0 then begin
            plot,[x_b0,x_b1],[y_b1,y_b0],/nodata,/xst,/yst, $
              _EXTRA = extra
          endif else begin
            plot,[x_b0,x_b1],[y_b1,y_b0],/nodata,/xst,/yst, $
              _EXTRA = extra
          endelse
        endif
        if n_elements(clip) eq 0 then $
            clip = [!x.crange[0],!y.crange[0],!x.crange[1],!y.crange[1]]
;
        r = .3                          ;len of arrow head
        angle = 22.5 * !dtor            ;Angle of arrowhead
        st = r * sin(angle)             ;sin 22.5 degs * length of head
        ct = r * cos(angle)
;
	magnitude = fltarr(n_elements(good))
	arrows = fltarr(2, 5, n_elements(good))
;
        for i=0L,n_elements(good)-1 do begin     ;Each point
                x0 = x[good[i] mod s[1]]        ;get coords of start & end
                dx = sina[i]
                x1 = x0 + dx
                y0 = y[good[i] / s[1]]
                dy = cosa[i]
                y1 = y0 + dy
                xd=x_step
                yd=y_step
                plots,[x0,x1,x1-(ct*dx/xd-st*dy/yd)*xd, $
                      x1,x1-(ct*dx/xd+st*dy/yd)*xd], $
                      [y0,y1,y1-(ct*dy/yd+st*dx/xd)*yd, $
                      y1,y1-(ct*dy/yd-st*dx/xd)*yd], $
                      clip=clip,noclip=noclip,_EXTRA=extra
    		magnitude[i] = mag[good[i]]
    		arrows[0, *, i] = [x0,x1,x1-(ct*dx/xd-st*dy/yd)*xd, x1,x1-(ct*dx/xd+st*dy/yd)*xd]
    		arrows[1, *, i] = [y0,y1,y1-(ct*dy/yd+st*dx/xd)*yd, y1,y1-(ct*dy/yd-st*dx/xd)*yd]
        endfor
        if nbad gt 0 then $             ;Dots for missing?
                PLOTS, x[bad mod s[1]], y[bad / s[1]], psym=3, $
                       clip=clip,noclip=noclip,_EXTRA=extra
end
