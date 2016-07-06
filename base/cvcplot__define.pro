;  NAME: 
;    cvcPlot
;
;
;  PURPOSE:
;    High-level class. Provides plotting of results. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlot::Init, in_oGlobal

    self.oGlobal = in_oGlobal
    hLogFile = self.oGlobal->Get('hLogFile')
    res = self->cvcError::Init(hLogFile)
    
    self.oConsts = obj_new('cvcConstants', in_oGlobal)
    self.hLogFile = hLogFile

    return, 1
END
;--------------------------------------------------------------------
PRO cvcPlot::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
END
;-------------------------------------------------------------
;+
; NAME:
;       LEG
; PURPOSE:
;       Make a plot legend.
; CATEGORY:
; CALLING SEQUENCE:
;       leg
; INPUTS:
; KEYWORD PARAMETERS:
;       Keywords:
;         LINESTYLE=sty  An array of line styles (def=0).
;         THICK=thk      An array of line thicknesses (def=1).
;         COLOR=clr      An array of line colors (def=!p.color).
;         PSYM=sym       An array of plot symbol codes (def=none).
;         SYMSIZE=ssz    Symbol size (def=1).
;         SCOLOR=sclr    An array of symbol colors (def=COLOR).
;         /INDENT        Means indent symbols from line ends.
;         LABEL=lbl      An array of line labels (def=none).
;         LSIZE=lsz      Label text size (def=1).
;         LCOLOR=lclr    Label color (def=asme as line colors).
;         NUMBER=nums    Number of symbols per line (def=1).
;         TITLE=ttl      Legend title.
;         TSIZE=tsz      Title size (def=1).
;         POSITION=pos   Legend position in normalized data
;           coordinates.  If not given, positioning is interactive.
;         OUTPOS=opos    Returned legend position from interactive
;           mode. Useful to repeat legend plot non-interactively.
;           Also in normalized data coordinates, that is 0 to 1
;           from xmin or ymin to xmax or ymax.
; OUTPUTS:
; COMMON BLOCKS:
;       leg_com
; NOTES:
;       Notes: Unless POSITION is given, an interactive box is used
;        to position plot legend.  The first line is the top of
;        the box, the last is the bottom, and the rest are uniformly
;        spaced between.  The last value of short arrays is repeated.
;        Hint: set up array parameters in separate statements.
;
;-
;-------------------------------------------------------------
PRO cvcPlot::__leg, linestyle=sty, thick=thk, color=clr, psym=sym, symsize=ssz, $
    number=nums, label=lbl, lsize=lsz, title=ttl, ttlalign=ttlalign, tsize=tsz, $
    position=pos, outpos=outpos, scolor=sclr, lcolor=lclr, $
    indent=indent, help=hlp

  common leg_com, bx, by, bdx, bdy

  if keyword_set(hlp) then begin
    print,' Make a plot legend.'
    print,' leg'
    print,'   All args are keywords.'
    print,' Keywords:'
    print,'   LINESTYLE=sty  An array of line styles (def=0).'
    print,'   THICK=thk      An array of line thicknesses (def=1).'
    print,'   COLOR=clr      An array of line colors (def=!p.color).'
    print,'   PSYM=sym       An array of plot symbol codes (def=none).'
    print,'   SYMSIZE=ssz    Symbol size (def=1).'
    print,'   SCOLOR=sclr    An array of symbol colors (def=COLOR).'
    print,'   /INDENT        Means indent symbols from line ends.'
    print,'   LABEL=lbl      An array of line labels (def=none).'
    print,'   LSIZE=lsz      Label text size (def=1).'
    print,'   LCOLOR=lclr    Label color (def=asme as line colors).'
    print,'   NUMBER=nums    Number of symbols per line (def=1).'
    print,'   TITLE=ttl      Legend title.'
    print,'   TITLEALIGN=ttlalign Legend title alignment.'
    print,'   TSIZE=tsz      Title size (def=1).'
    print,'   POSITION=pos   Legend position in normalized data'
    print,'     coordinates.  If not given, positioning is interactive.'
    print,'   OUTPOS=opos    Returned legend position from interactive'
    print,'     mode. Useful to repeat legend plot non-interactively.'
    print,'     Also in normalized data coordinates, that is 0 to 1'
    print,'     from xmin or ymin to xmax or ymax.'
    print,' Notes: Unless POSITION is given, an interactive box is used'
    print,'  to position plot legend.  The first line is the top of'
    print,'  the box, the last is the bottom, and the rest are uniformly'
    print,'  spaced between.  The last value of short arrays is repeated.'
    print,'  Hint: set up array parameters in separate statements.'
    return
  endif

  ;-------  Make sure parameters are defined  -------------
  if n_elements(sty) eq 0 then sty = [0]
  if n_elements(thk) eq 0 then thk = [0]
  if n_elements(clr) eq 0 then clr = [!p.color]
  if n_elements(sclr) eq 0 then sclr = clr
  if n_elements(sym) eq 0 then sym = [0]
  if n_elements(ssz) eq 0 then ssz = 1
  if n_elements(lbl) eq 0 then lbl = [' ']
  if n_elements(lsz) eq 0 then lsz = 1
  if n_elements(lclr) eq 0 then lclr = clr
  if n_elements(nums) eq 0 then nums = 1
  if n_elements(ttl) eq 0 then ttl = ' '
  if n_elements(ttlalign) eq 0 then ttlalign = 0.5
  if n_elements(tsz) eq 0 then tsz = 1

  ;-------  Find last element of each array  -----------
  lst_sty = n_elements(sty)-1
  lst_thk = n_elements(thk)-1
  lst_clr = n_elements(clr)-1
  lst_sclr = n_elements(sclr)-1
  lst_lclr = n_elements(lclr)-1
  lst_sym = n_elements(sym)-1
  lst_lbl = n_elements(lbl)-1
  num = 1+(lst_sty>lst_thk>lst_clr>lst_sym>lst_lbl>lst_lclr)

  ;--------  Interactive mode  ----------------
  if n_elements(pos) eq 0 then begin
    if n_elements(bx) eq 0 then begin
      bx=100 & by=100 & bdx=100 & bdy=100
    endif
    movbox,bx,by,bdx,bdy,code,/noerase,/exiterase    ; Dev crds of legend.
    x=bx & y=by & dx=bdx & dy=bdy
    if code eq 2 then return
    xx = [x,x+dx]          ; Convert to normal coords.
    yy = [y,y+dy]
    t = convert_coord(xx,yy,/dev,/to_norm)
    x1 = t(0,0)          ; Legend box in norm coord.
    x2 = t(0,1)
    y1 = t(1,0)
    y2 = t(1,1)
    dx = !x.window(1) - !x.window(0) ; Plot window size,
    dy = !y.window(1) - !y.window(0) ;   in normalized coord.
    nx1 = (x1-!x.window(0))/dx     ; Norm. Data. coord.
    nx2 = (x2-!x.window(0))/dx
    ny1 = (y1-!y.window(0))/dy
    ny2 = (y2-!y.window(0))/dy
    pos = [nx1,ny1,nx2,ny2]
    outpos = pos
  endif

  ;--------  Plot legend  ------------
  dx = !x.window(1) - !x.window(0)   ; Convert from normalized data,
  dy = !y.window(1) - !y.window(0)   ;   to normalized.
  x1 = pos(0)*dx + !x.window(0)
  x2 = pos(2)*dx + !x.window(0)
  y1 = pos(1)*dy + !y.window(0)
  y2 = pos(3)*dy + !y.window(0)
  ; dy = (y2 - y1)/((num-1)>1)
      dy = (y2 - y1)/((num+1)>1)

  cx = lsz*!d.x_ch_size/float(!d.x_size)  ; Char size in norm units.
  cy = lsz*.778*!d.y_ch_size/!d.y_size   ; .778 = fract. filled by ch.
  sx = ssz*!d.x_ch_size/float(!d.x_size)  ; Sym size in norm units.
  if keyword_set(indent) then begin
    dx = (x2 - x1)/(nums+1.)     ; Symbol spacing (indented).
    sx1 = x1 + dx/2.
    sx2 = x2 - dx/2.
    dx = (sx2 - sx1)/((nums-1.)>1.)
  endif else begin
    sx1 = x1 + sx/2.        ; Indent by 1/2 sym size.
    sx2 = x2 - sx/2.
    dx = (sx2 - sx1)/((nums-1.)>1.)  ; (Non-indented).
  endelse

  ;for ii = 0, num-1 do begin
    ; i = num-1 - ii     ; Reverse order.

      for ii = 1, num do begin
        i = num+1 - ii                ; Reverse order.
    symc = sym(ii-1<lst_sym) ; Symbol code.

    ;-------  Plot lines  --------------

    if symc le 0 then begin  ; Plot line.
          x_angoli = [x1,x2,x2,x1]
          y_angoli = [y1+i*dy-cy/2.,y1+i*dy-cy/2.,cy+y1+i*dy-cy/2.,cy+y1+i*dy-cy/2.]
          ; y_angoli = [y1+i*dy,y1+i*dy,cy+y1+i*dy,cy+y1+i*dy]
          ; print,' Angoli Poligono n. ',ii-1,y_angoli
          polyfill, x_angoli,y_angoli,color=clr(ii-1<lst_clr),/norm
    endif
    ;-------  Plot symbols  ------------
    if abs(symc) ne 0 then begin              ;--- Plot symbols.
      if nums eq 1 then begin                ;--- Single symbol (center).
            plots, /norm, [.5*(x1+x2)],[y1+i*dy], $
              psym=abs(symc), symsize=ssz,color=sclr(ii-1<lst_sclr)
      endif else begin
        if keyword_set(indent) then begin    ;--- Indented.
          plots,/norm, sx1+dx*(indgen(nums)),y1+i*dy+0*indgen(nums), $
            psym=abs(symc), symsize=ssz,color=sclr(ii-1<lst_sclr)
        endif else begin                ;--- Non-indented.
          plots,/norm, sx1+dx*indgen(nums),y1+i*dy+0*indgen(nums), $
            psym=abs(symc), symsize=ssz,color=sclr(ii-1<lst_sclr)
        endelse
      endelse
    endif

    ;-------  Labels  ------------------
    dcy = 0
    if (ii eq 1) then dcy = -cy/2.0
    if (ii eq num) then dcy = cy/2.0
    
    xyouts, /norm, x2+cx, y1+i*dy-cy/2.0+dcy,lbl(ii-1<lst_lbl), $
       charsize=lsz, color=lclr(ii-1<lst_lclr), charthick=thk
        ;xyouts, /norm, x2+cx, y1+i*dy,lbl(ii-1<lst_lbl), $
    ;  charsize=lsz, color=lclr(ii-1<lst_lclr)

        ; print, ' Testo in y ', y1+i*dy-cy/2.
  endfor

  ;---------  Legend title  -------------
  xyouts, /norm, x1, y1+dy*num+0.05, ttl, charsize=tsz, align=ttlalign, $
      color=lclr(ii-1<lst_lclr), charthick=thk

  return
end
;--------------------------------------------------------------------
FUNCTION cvcPlot::__Preprocess2D, in_sData, in_sDestDesc, out_sData

    res = self->__CheckGrid(in_sData)
    if (self->Assert(res)) then return, -1

    if (in_sDestDesc.graphics.smoothing eq 'yes') then smoothCoef = 1.0 else smoothCoef = 1.0
    
    
; we need to convert irregular grid to regular one
    if (in_sData.gridType eq 'irregular') then begin
; find minimum X interval
      xIrregSize = size(*in_sData.pXgrid)
      int = fltarr(xIrregSize[1]-1)
      for i = 0, xIrregSize[1]-2 do begin
        int[i] = abs((*in_sData.pXgrid)[i+1, 0] - (*in_sData.pXgrid)[i, 0])
      endfor
;      minXint = min(int[where(int ne 0)]) / smoothCoef
      minXint = total(int) / n_elements(int)
; find minimum Y interval      
      yIrregSize = size(*in_sData.pYgrid)
      int = fltarr(yIrregSize[2]-1)
      for i = 0, yIrregSize[2]-2 do begin
        int[i] = abs((*in_sData.pYgrid)[0, i+1] - (*in_sData.pYgrid)[0, i])
      endfor
;      minYint = min(int[where(int ne 0)]) / smoothCoef
      minYint = total(int) / n_elements(int)
; preparing regular grids
      xMinVal = min(*in_sData.pXgrid, max=xMaxVal)
      yMinVal = min(*in_sData.pYgrid, max=yMaxVal)
      xRegSize = ceil((xMaxVal - xMinVal) / minXint)
      yRegSize = ceil((yMaxVal - yMinVal) / minYint)
      aRegXgrid = findgen(xRegSize) * minXint + xMinVal
      aRegYgrid = findgen(yRegSize) * minYint + yMinVal
; gridding      
      minVal = min((*in_sData.data)[where(*in_sData.data ne in_sData.missingVal)], max = maxVal)
;      print, minXint, minYint
;      stop
      triangulate2, *in_sData.pXgrid, *in_sData.pYgrid, aTri, border
; stop
;;      aData = trigrid2(*in_sData.pXgrid, *in_sData.pYgrid, *in_sData.data, aTri, $
;;         nx=xRegSize, ny=yRegSize, xgrid=aRegXgrid, ygrid=aRegYgrid, min_value = minVal, max_value = maxVal, $
;;         missing = in_sData.missingVal)
aData = *in_sData.data
;      aData = trigrid(*in_sData.pXgrid, *in_sData.pYgrid, *in_sData.data, aTri, $
;	nx=xRegSize, ny=yRegSize, max_value = maxVal, missing = in_sData.missingVal)
; correct range
      minIdx = where(aData lt minVal)
      maxIdx = where(aData gt maxVal)
      nanIdx = where(~finite(aData))
      if (minIdx[0] ne -1) then aData[minIdx] = in_sData.missingVal
      if (maxIdx[0] ne -1) then aData[maxIdx] = in_sData.missingVal
      if (nanIdx[0] ne -1) then aData[nanIdx] = in_sData.missingVal
      dims = size(aData, /dim)
      aData = reform(aData, dims[1], dims[0])
; create a nonconvex hull of the area
;      xsize = size(*in_sData.pXgrid)
;      xx = [(*in_sData.pXgrid)[*, 0], reform((*in_sData.pXgrid)[xsize[1]-1, *], xsize[2]), $
;            reverse((*in_sData.pXgrid)[*, xsize[2]-1]), reverse(reform((*in_sData.pXgrid)[0, *], xsize[2]))]
;      yy = [(*in_sData.pYgrid)[*, 0], reform((*in_sData.pYgrid)[xsize[1]-1, *], xsize[2]), $
;            reverse((*in_sData.pYgrid)[*, xsize[2]-1]), reverse(reform((*in_sData.pYgrid)[0, *], xsize[2]))]
;      aArea = transpose([[xx], [yy]])
;print, 'Cut ROI'
; stop
;      oPoly = Obj_New('IDLanROI', aArea)
;      vIx = replicate(1.0, n_elements(aRegXgrid))
;      vIy = replicate(1.0, n_elements(aRegYgrid))
;      aX = aRegXgrid # transpose(vIy)
;      aY = vIx # transpose(aRegYgrid)
;      mask = oPoly->ContainsPoints(aX, aY)
;      obj_destroy, oPoly
;      idxs = where(mask eq 0)
;      if (idxs[0] ne -1) then aData[idxs] = in_sData.missingVal         
;print, 'Good'
    endif else begin
      if ((in_sData.gridType eq 'regular') or (in_sData.gridType eq 'station')) then begin
        aData = *in_sData.data
        aRegXgrid = *in_sData.pXgrid
        aRegYgrid = *in_sData.pYgrid
      endif else begin
        self->printLog, 'Error: Wrong grid type for shaded plot!'
        return, -1
      endelse
    endelse
    
    minVal = min(aData[where(aData ne in_sData.missingVal)], max = maxVal)
        
    out_sData = { $
                  aData : aData, $
                  aLons : aRegXgrid, $
                  aLats : aRegYgrid, $
                  minVal : minVal, $
                  maxVal : maxVal, $
                  gridType : in_sData.gridType, $
                  missingVal : in_sData.missingVal, $
                  description : in_sData.description, $
                  extra : in_sData.extra $
                }
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__PlotALegend, OPTIONS = sLegendOptions

  if (n_elements(sLegendOptions) eq 0) then $
    sLegendOptions = { title : '', $ ; title
                       minVal : -1, $ ; minimum value
                       maxVal : 1, $ ; maximum value
                       nLabels : 10, $ ; number of major steps
                       nColor : 253, $ ; number of colors
                       colorScale : 'linear', $ ; color scaling: linear/log
                       aLim : [1.04, 0.011, 1.17, 0.95], $ ; positions of legend's borders
                       charThick : 3, $ ; thickness of characters 
                       charSize : 1.8, $ ; size of characters
                       titleSize : 2.0, $ ; size of title
                       titleAlign : 0.5, $ ; title alignment
                       labelFmt : '(f6.2)', $ ; legend label format
                       colorBG : 0, $ ; background color
                       colorFG : 255 $ ; foreground color
                     }

; set legend title
    in_legTitle = sLegendOptions.title
    in_minDataVal = sLegendOptions.minVal
    in_maxDataVal = sLegendOptions.maxVal
;    legSteps = sLegendOptions.nLabels ; number of steps on the legend

; colors and labels of the legend
;    legColors = reverse(indgen(sLegendOptions.nColor+1)+1)
    legColors = reverse(findgen(sLegendOptions.nColor)+1)/sLegendOptions.nColor*253
    legLabels = make_array(sLegendOptions.nColor, /string) ; labels array has the same number of elements as legColors array
    legLabelFmt = sLegendOptions.labelFmt
    colorFG = sLegendOptions.colorFG
    colorBG = sLegendOptions.colorBG
    
; set default number of labels if it's not specified                                                                                                          
    N = sLegendOptions.nLabels eq 0 ? 10 : sLegendOptions.nLabels                                                                                             
    idxs = fix(indgen(N)*(sLegendOptions.nColor-1+0.9999)/(N-1)) 
    
    if (sLegendOptions.colorScale eq 'linear') then begin
      colIdxs = idxs
    endif else begin
      colIdxs = bytscl(alog10(idxs+1), top = 252)+1
      in_legTitle = in_legTitle
    endelse 
    
    legVals = (legColors[colIdxs]-1)/float(sLegendOptions.nColor-1)*(in_maxDataVal-in_minDataVal)+in_minDataVal 
    legLabels[idxs] = string(legVals, format = legLabelFmt)

    legXpos = [sLegendOptions.aLim[0], sLegendOptions.aLim[2], sLegendOptions.aLim[2], $
     sLegendOptions.aLim[0], sLegendOptions.aLim[0]]
    legYpos = [sLegendOptions.aLim[1], sLegendOptions.aLim[1], sLegendOptions.aLim[3], $
     sLegendOptions.aLim[3], sLegendOptions.aLim[1]]

    legDX = !X.Window[1] - !X.Window[0]
    legDY = !Y.Window[1] - !Y.Window[0]
    legX1pos = legXpos*legDX + !X.Window[0]
    legY1pos = legYpos*legDY + !Y.Window[0]
    polyfill, legX1pos, legY1pos, color=colorBG, /normal

;    plots, legX1pos, legY1pos, color=sLegendOptions.colorFG, /normal
    
    legDelta = float(legXpos[1]-legXpos[0]) / float(n_elements(legColors)-1)
    legPos = [legXpos[0]+legDelta, legYpos[0], legXpos[0]+((legXpos[1]-legXpos[0])/3), legYpos[3]]

; plotting legend
    self->__leg, color=legColors, label=legLabels, position=legPos, thick=sLegendOptions.charthick, $
     lsize=sLegendOptions.charsize, lcolor=colorFG, title=in_legTitle, tsize=sLegendOptions.titlesize, $
     ttlalign=sLegendOptions.titleAlign, scolor=colorFG

    return, 1
END
;--------------------------------------------------------------------
PRO cvcPlot::__WriteRasterXMLLegend, fileName, legVals, legLabels, r, g, b, idxs

    self->printLog, 'Writing XML legend for RASTER!'
    openw, lun, fileName, /get_lun

    printf, lun, '<?xml version="1.0" encoding="ISO-8859-1"?>'
    printf, lun, '<StyledLayerDescriptor version="1.0.0" xmlns="http://www.opengis.net/sld" xmlns:ogc="http://www.opengis.net/ogc"'
    printf, lun, '  xmlns:xlink="http://www.w3.org/1999/xlink" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"'
    printf, lun, '  xsi:schemaLocation="http://www.opengis.net/sld http://schemas.opengis.net/sld/1.0.0/StyledLayerDescriptor.xsd">'
    printf, lun, '  <NamedLayer>'
    printf, lun, '    <Name>Raster Layer</Name>'
    printf, lun, '    <UserStyle>'
    printf, lun, '      <Name>Raster</Name>'
    printf, lun, '      <Title>Raster</Title>'
    printf, lun, '      <Abstract>Colors</Abstract>'
    printf, lun, '      <FeatureTypeStyle>'
    printf, lun, '        <Rule>'
    printf, lun, '          <RasterSymbolizer>'
    printf, lun, '            <Opacity>0.8</Opacity>'
    printf, lun, '            <ColorMap>'
    printf, lun, '              <ColorMapEntry color="#000000" quantity="', strtrim("-999.0", 2), '" label="" opacity="0.0" />', format='(3a)'
    for i = n_elements(legVals)-1, 0, -1 do begin
      j = idxs[i]
      rc = r[j] gt 15 ? strtrim(string(r[j], format='(Z2.2)'), 2) : strtrim(string('0',r[j], format='(a,Z)'), 2)
      gc = g[j] gt 15 ? strtrim(string(g[j], format='(Z2.2)'), 2) : strtrim(string('0',g[j], format='(a,Z)'), 2)
      bc = b[j] gt 15 ? strtrim(string(b[j], format='(Z2.2)'), 2) : strtrim(string('0',b[j], format='(a,Z)'), 2)
      color = string(rc, gc, bc, format='(3a)')
      qty = strtrim(legVals[i], 2)
      lbl = strtrim(legLabels[j], 2)
      printf, lun, '              <ColorMapEntry color="#', color, '" quantity="', qty, '" label="', lbl, '" />', format='(7a)'
;      print, '              <ColorMapEntry color="#', color, '" quantity="', qty, '" label="', lbl, '" />', format='(7a)'
    endfor
    printf, lun, '            </ColorMap>'
    printf, lun, '            <!--OverlapBehavior-->'
    printf, lun, '              <!--AVERAGE/-->'
    printf, lun, '            <!--/OverlapBehavior-->'
    printf, lun, '          </RasterSymbolizer>'
    printf, lun, '        </Rule>'
    printf, lun, '      </FeatureTypeStyle>'
    printf, lun, '    </UserStyle>'
    printf, lun, '  </NamedLayer>'
    printf, lun, '</StyledLayerDescriptor>'

    free_lun, lun
    
END
;--------------------------------------------------------------------
PRO cvcPlot::__WriteStationXMLLegend, fileName, legVals, legLabels, r, g, b, idxs

    symName = 'circle'
    symSize = 6 ; symbol size
    dataColumnName = 'VALUE'

    self->printLog, 'Writing XML legend for STATIONS!'

; open file
    openw, lun, fileName, /get_lun

; header
    printf, lun, '<?xml version="1.0" encoding="UTF-8"?>'
    printf, lun, '<StyledLayerDescriptor version="1.0.0"'
    printf, lun, ' xsi:schemaLocation="http://www.opengis.net/sld StyledLayerDescriptor.xsd"'
    printf, lun, ' xmlns="http://www.opengis.net/sld"'
    printf, lun, ' xmlns:ogc="http://www.opengis.net/ogc"'
    printf, lun, ' xmlns:xlink="http://www.w3.org/1999/xlink"'
    printf, lun, ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'
    printf, lun, '  <!-- a Named Layer is the basic building block of an SLD document -->'
    printf, lun, '  <NamedLayer>'
    printf, lun, '    <Name>Stations</Name>'
    printf, lun, '    <UserStyle>'
    printf, lun, '    <!-- Styles can have names, titles and abstracts -->
    printf, lun, '      <Title>Station data</Title>'
    printf, lun, '      <Abstract>Based on VALUE field</Abstract>'
    printf, lun, '      <!-- FeatureTypeStyles describe how to render different features -->
    printf, lun, '      <!-- A FeatureTypeStyle for rendering points -->
    printf, lun, '      <FeatureTypeStyle>'

; first element: less than minimum
    i = n_elements(legVals)-1
    j = idxs[i]
    rc = r[j] gt 15 ? strtrim(string(r[j], format='(Z2.2)'), 2) : strtrim(string('0',r[j], format='(a,Z)'), 2)
    gc = g[j] gt 15 ? strtrim(string(g[j], format='(Z2.2)'), 2) : strtrim(string('0',g[j], format='(a,Z)'), 2)
    bc = b[j] gt 15 ? strtrim(string(b[j], format='(Z2.2)'), 2) : strtrim(string('0',b[j], format='(a,Z)'), 2)
    color = string(rc, gc, bc, format='(3a)')
    lbl = legLabels[j]
    qty = strtrim(legVals[i], 2)

    printf, lun, '        <Rule>'    
    printf, lun, '          <Name>First level</Name>'
    printf, lun, '          <Title>&lt;', lbl, '</Title>'
    printf, lun, '          <Abstract></Abstract>'
    printf, lun, '          <ogc:Filter>'
    printf, lun, '            <ogc:PropertyIsLessThan>'
    printf, lun, '              <ogc:PropertyName>', dataColumnName, '</ogc:PropertyName>', format='(3a)'
    printf, lun, '              <ogc:Literal>', qty, '</ogc:Literal>', format='(3a)'
    printf, lun, '            </ogc:PropertyIsLessThan>'
    printf, lun, '          </ogc:Filter>'
    printf, lun, '          <PointSymbolizer>'
    printf, lun, '            <Graphic>'
    printf, lun, '              <Mark>'
    printf, lun, '                <WellKnownName>', symName, '</WellKnownName>', format='(3a)'
    printf, lun, '                <Fill>'
    printf, lun, '                  <CssParameter name="fill">#', color, '</CssParameter>', format='(3a)'
    printf, lun, '                </Fill>'
    printf, lun, '                <Stroke>'
    printf, lun, '                  <CssParameter name="stroke">#', color, '</CssParameter>', format='(3a)'
    printf, lun, '                  <CssParameter name="stroke-width">1</CssParameter>'
    printf, lun, '                </Stroke>'
    printf, lun, '              </Mark>'
    printf, lun, '              <Size>', symSize, '</Size>', format='(a, i, a)'
    printf, lun, '            </Graphic>'
    printf, lun, '          </PointSymbolizer>'
    printf, lun, '        </Rule>'

; intermediate elements
    for i = n_elements(legVals)-1, 2, -1 do begin
      j1 = idxs[i]
      j2 = idxs[i-1]
      rc = r[j2] gt 15 ? strtrim(string(r[j2], format='(Z2.2)'), 2) : strtrim(string('0',r[j2], format='(a,Z)'), 2)
      gc = g[j2] gt 15 ? strtrim(string(g[j2], format='(Z2.2)'), 2) : strtrim(string('0',g[j2], format='(a,Z)'), 2)
      bc = b[j2] gt 15 ? strtrim(string(b[j2], format='(Z2.2)'), 2) : strtrim(string('0',b[j2], format='(a,Z)'), 2)
      color = string(rc, gc, bc, format='(3a)')
      qty1 = strtrim(legVals[i], 2)
      qty2 = strtrim(legVals[i-1], 2)
      lbl1 = legLabels[j1]
      lbl2 = legLabels[j2]

      printf, lun, '        <Rule>'    
      printf, lun, '          <Name>Next level</Name>'
      printf, lun, '          <Title>', lbl1, ' .. ', lbl2, '</Title>', format='(5a)'
      printf, lun, '          <Abstract></Abstract>'
      printf, lun, '          <ogc:Filter>'
      printf, lun, '            <ogc:And>'
      printf, lun, '              <ogc:PropertyIsGreaterThanOrEqualTo>'
      printf, lun, '                <ogc:PropertyName>', dataColumnName, '</ogc:PropertyName>', format='(3a)'
      printf, lun, '                <ogc:Literal>', qty1, '</ogc:Literal>', format='(3a)'
      printf, lun, '              </ogc:PropertyIsGreaterThanOrEqualTo>'
      printf, lun, '              <ogc:PropertyIsLessThan>'
      printf, lun, '                <ogc:PropertyName>', dataColumnName, '</ogc:PropertyName>', format='(3a)'
      printf, lun, '                <ogc:Literal>', qty2, '</ogc:Literal>', format='(3a)'
      printf, lun, '              </ogc:PropertyIsLessThan>'
      printf, lun, '            </ogc:And>'
      printf, lun, '          </ogc:Filter>'
      printf, lun, '          <PointSymbolizer>'
      printf, lun, '            <Graphic>'
      printf, lun, '              <Mark>'
      printf, lun, '                <WellKnownName>', symName, '</WellKnownName>', format='(3a)'
      printf, lun, '                <Fill>'
      printf, lun, '                  <CssParameter name="fill">#', color, '</CssParameter>', format='(3a)'
      printf, lun, '                </Fill>'
      printf, lun, '                <Stroke>'
      printf, lun, '                  <CssParameter name="stroke">#', color, '</CssParameter>', format='(3a)'
      printf, lun, '                  <CssParameter name="stroke-width">1</CssParameter>'
      printf, lun, '                </Stroke>'
      printf, lun, '              </Mark>'
      printf, lun, '              <Size>', symSize, '</Size>', format='(a, i, a)'
      printf, lun, '            </Graphic>'
      printf, lun, '          </PointSymbolizer>'
      printf, lun, '        </Rule>'
    endfor

; last element: greater or equal to maximum
    j = idxs[0]
    rc = r[j] gt 15 ? strtrim(string(r[j], format='(Z2.2)'), 2) : strtrim(string('0',r[j], format='(a,Z)'), 2)
    gc = g[j] gt 15 ? strtrim(string(g[j], format='(Z2.2)'), 2) : strtrim(string('0',g[j], format='(a,Z)'), 2)
    bc = b[j] gt 15 ? strtrim(string(b[j], format='(Z2.2)'), 2) : strtrim(string('0',b[j], format='(a,Z)'), 2)
    color = string(rc, gc, bc, format='(3a)')
    lbl = legLabels[j]
    qty = strtrim(legVals[0], 2)
    
    printf, lun, '        <Rule>'    
    printf, lun, '          <Name>Last level</Name>'
    printf, lun, '          <Title>&gt;', lbl, '</Title>'
    printf, lun, '          <Abstract></Abstract>'
    printf, lun, '          <ogc:Filter>'
    printf, lun, '            <ogc:PropertyIsGreaterThanOrEqualTo>'
    printf, lun, '              <ogc:PropertyName>', dataColumnName, '</ogc:PropertyName>', format='(3a)'
    printf, lun, '              <ogc:Literal>', qty, '</ogc:Literal>', format='(3a)'
    printf, lun, '            </ogc:PropertyIsGreaterThanOrEqualTo>'
    printf, lun, '          </ogc:Filter>'
    printf, lun, '          <PointSymbolizer>'
    printf, lun, '            <Graphic>'
    printf, lun, '              <Mark>'
    printf, lun, '                <WellKnownName>', symName, '</WellKnownName>', format='(3a)'
    printf, lun, '                <Fill>'
    printf, lun, '                  <CssParameter name="fill">#', color, '</CssParameter>', format='(3a)'
    printf, lun, '                </Fill>'
    printf, lun, '                <Stroke>'
    printf, lun, '                  <CssParameter name="stroke">#', color, '</CssParameter>', format='(3a)'
    printf, lun, '                  <CssParameter name="stroke-width">1</CssParameter>'
    printf, lun, '                </Stroke>'
    printf, lun, '              </Mark>'
    printf, lun, '              <Size>', symSize, '</Size>', format='(a, i, a)'
    printf, lun, '            </Graphic>'
    printf, lun, '          </PointSymbolizer>'
    printf, lun, '        </Rule>'

; footer
    printf, lun, '      </FeatureTypeStyle>'
    printf, lun, '    </UserStyle>'
    printf, lun, '  </NamedLayer>'
    printf, lun, '</StyledLayerDescriptor>'

; close file
    free_lun, lun
    
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__WriteXMLLegend, fileName, in_aColorTable, OPTIONS = sLegendOptions

  if (n_elements(sLegendOptions) eq 0) then $
    sLegendOptions = { title : '', $ ; title
                       minVal : -1, $ ; minimum value
                       maxVal : 1, $ ; maximum value
                       nLabels : 10, $ ; number of major steps
                       nColor : 253, $ ; number of colors
                       colorScale : 'linear', $ ; color scaling: linear/log
                       aLim : [1.04, 0.011, 1.17, 0.95], $ ; positions of legend's borders
                       charThick : 3, $ ; thickness of characters 
                       charSize : 1.8, $ ; size of characters
                       titleSize : 2.0, $ ; size of title
                       titleAlign : 0.5, $ ; title alignment
                       labelFmt : '(f6.2)', $ ; legend label format
                       colorBG : 0, $ ; background color
                       colorFG : 255, $ ; foreground color
		       kind : '' $ ; legend kind
                     }

; set legend title
    in_legTitle = sLegendOptions.title
    in_minDataVal = sLegendOptions.minVal
    in_maxDataVal = sLegendOptions.maxVal

; colors and labels of the legend
    legColors = reverse(findgen(sLegendOptions.nColor)+1)/sLegendOptions.nColor*253
    legLabels = make_array(sLegendOptions.nColor, /string) ; labels array has the same number of elements as legColors array
    legLabelFmt = sLegendOptions.labelFmt
    colorFG = sLegendOptions.colorFG
    colorBG = sLegendOptions.colorBG
    legKind = sLegendOptions.kind

;    N = sLegendOptions.nLabels
    if (sLegendOptions.nLabels lt 1) then begin
      self->printLog, 'Error! Number of legend labels is less than 1. Check the input XML-file.'
      return, -1
    endif
    idxs = fix(indgen(sLegendOptions.nLabels)*(sLegendOptions.nColor-1+0.9999)/(sLegendOptions.nLabels-1))
    
    if (sLegendOptions.colorScale eq 'linear') then begin
      colIdxs = idxs
    endif else begin
      colIdxs = bytscl(alog10(idxs+1), top = 252)+1
      in_legTitle = in_legTitle
    endelse 
    
    legVals = (legColors[colIdxs]-1)/float(sLegendOptions.nColor-1)*(in_maxDataVal-in_minDataVal)+in_minDataVal 
    legAllVals = (legColors-1)/float(sLegendOptions.nColor-1)*(in_maxDataVal-in_minDataVal)+in_minDataVal
    legLabels[idxs] = string(legVals, format = legLabelFmt)

    r = in_aColorTable[legColors, 0]
    g = in_aColorTable[legColors, 1]
    b = in_aColorTable[legColors, 2]
    
; writing legend
    case legKind of
      'raster': self->__WriteRasterXMLLegend, fileName, legVals, legLabels, r, g, b, idxs
      'stations':  self->__WriteStationXMLLegend, fileName, legVals, legLabels, r, g, b, idxs
      else: begin
	self->printLog, 'Error! Unknown XML legend kind: ', legKind
        return, -1
      end
    endcase
    
    return, 1
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__GetCTIndex, in_CTName, out_colorTableId
; color table loading and modification
  aColorTableList = self.oConsts->Get('CTL')

  out_colorTableId = (where(aColorTableList eq in_CTName, cnt))[0]
  if (cnt eq 0) then begin
    self->printLog, 'Bad color table name specified! Aborting!'
    return, -1
  endif
;  loadct, colorTableId
;  tvlct, colorTable, /get
;  self.colorTable = colorTable
;  self.colorTable[0, *] = 0b
;  self.colorTable[254, *] = 192b
;  self.colorTable[255, *] = 255b
;  tvlct, self.colorTable
  
  return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__CheckGrid, in_sData 
    
    if (ptr_valid(in_sData.data)) then dataSize = size(*in_sData.data)
    if (ptr_valid(in_sData.pXgrid)) then xSize = size(*in_sData.pXgrid)
    if (ptr_valid(in_sData.pYgrid)) then ySize = size(*in_sData.pYgrid)
    if (ptr_valid(in_sData.pZgrid)) then zSize = size(*in_sData.pZgrid)
    if (ptr_valid(in_sData.pTimegrid)) then timeSize = size(*in_sData.pTimeGrid)
    gridType = in_sData.gridType
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::Run, value

    self->printLog, 'Function Run should be implemented!'
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__OpenX, in_sDestDesc

    set_plot, 'x'
    device, decomposed = 0 ;, set_font='Times', /tt_font
    window, xsize = in_sDestDesc.graphics.width, ysize = in_sDestDesc.graphics.height        
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__OpenObjX, in_sDestDesc, out_oWindow

    device, decomposed = 0 ;, set_font='Times', /tt_font
    out_oWindow = obj_new('IDLgrWindow', dimensions=[in_sDestDesc.graphics.width, in_sDestDesc.graphics.height])        

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__OpenEPS, in_sDestDesc

    set_plot, 'ps'
    device, file = in_sDestDesc.file.name, /encapsul, /inches, /color, $
     bits_per_pixel = 8, xsize = in_sDestDesc.graphics.width/71.95-1, ysize = in_sDestDesc.graphics.height/71.95-1

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__OpenObjPrint, in_sDestDesc, out_oPrinter

    out_Printer = obj_new('IDLgrClipboard', dimensions=[in_sDestDesc.graphics.width, in_sDestDesc.graphics.height])
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlot::__PlotMap, in_sData, in_sDestDesc

; load color table
    res = self->__GetCTIndex(in_sDestDesc.graphics.colortable, colorTableId)
    if (self->Assert(res)) then return, -1

;    loadct, file='usercolors.tbl', 0
    loadct, colorTableId
    tvlct, aColorTable, /get
    aColorTable[0, *] = 0b
;    aColorTable[254, *] = 192b
    aColorTable[255, *] = 255b
    tvlct, aColorTable

    if (in_sDestDesc.graphics.blackbg eq 'yes') then begin
      colorFG = 255
      colorBG = 0
    endif else begin
      colorFG = 0
      colorBG = 255
    endelse

    if (in_sDestDesc.file.name eq '') then begin ; screen output
      thickness = 1.5 ; global line/character thickness
    endif else begin
       thickness = 4.5 ; global line/character thickness
    endelse
         
;    print, 'Legend kind: ', in_sDestDesc.graphics.legend.kind
    case in_sDestDesc.graphics.legend.kind of
      'embedded': begin
        if (in_sDestDesc.graphics.legend.position eq 'right') then begin
          xmargin = [4, 27]
          ymargin = [3, 7]
        endif
      end
      'file': begin
        xmargin = [4, 4]
        ymargin = [3, 7]
      end
      'none': begin
        xmargin = [4, 4]
        ymargin = [3, 7]
      end
      else: begin
        self->printLog, 'Error: Bad legend kind!'
        return, -1
      end
    endcase
        

; prepare data and window for plotting
    if (in_sDestDesc.projection.name eq 'cylindrical') then begin
      map_set, in_sDestDesc.projection.p0lat, in_sDestDesc.projection.p0lon, in_sDestDesc.projection.rot, $
       name=in_sDestDesc.projection.name, xmargin = xmargin, ymargin = ymargin, $
       limit=[in_sDestDesc.projection.limits.bottom, in_sDestDesc.projection.limits.left, $
            in_sDestDesc.projection.limits.top, in_sDestDesc.projection.limits.right], /noborder   
    endif else begin
      llon = in_sDestDesc.projection.limits.left
      tlat = in_sDestDesc.projection.limits.top
      rlon = in_sDestDesc.projection.limits.right
      blat = in_sDestDesc.projection.limits.bottom
;      llat = (tlat+blat) / 2.5
;      rlat = llat
;      tlon = (rlon+llon) / 2.
;      blon = tlon
      lonStep = (rlon - llon) > 20 ? 5. : 1.
      latStep = (tlat - blat) > 20 ? 5. : 1.
      mapLonDel = ceil((rlon - llon) / 10.0 / lonStep)*lonStep
      mapLatDel = ceil((tlat - blat) / 10.0 / latStep)*latStep
      p0lat = in_sDestDesc.projection.p0lat 
      p0lon = in_sDestDesc.projection.p0lon
      map_set, p0lat, p0lon, in_sDestDesc.projection.rot, $
       name=in_sDestDesc.projection.name, xmargin = xmargin, ymargin = ymargin, $
;       limit=[llat, llon, tlat, tlon, rlat, rlon, blat, blon], /noborder
       limit=[blat, llon, tlat, rlon], /horizon
    endelse
;stop
    ; fill the background 
    polyfill, [-1, -1, 2, 2, -1], [-1, 2, 2, -1, -1], color = colorBG, /normal

    minLon = min(in_sData.aLons, max = maxLon)
    minLat = min(in_sData.aLats, max = maxLat)

; set min-max values
    minVal = min(in_sData.aData[where(in_sData.aData ne in_sData.missingVal)], max = maxVal)

; map image to the chosen projection
    if ((in_sDestDesc.graphics.kind eq 'shaded')) then begin
      ; for maps
      if (in_sDestDesc.graphics.smoothing eq 'yes') then begin
        aMappedData = map_image(in_sData.aData, startXPos, startYPos, $
         imageXSize, imageYSize, lonmin = in_sData.aLons[0], lonmax = in_sData.aLons[n_elements(in_sData.aLons)-1], $
         latmin = in_sData.aLats[0], latmax = in_sData.aLats[n_elements(in_sData.aLats)-1], compress = 1, missing = in_sData.missingVal, $
         min_value = in_sData.missingVal, mask = goodMask, /bilinear)
      endif else begin
        aMappedData = map_image(in_sData.aData, startXPos, startYPos, $
         imageXSize, imageYSize, lonmin = in_sData.aLons[0], lonmax = in_sData.aLons[n_elements(in_sData.aLons)-1], $
;         imageXSize, imageYSize, lonmin = min(in_sData.aLons), lonmax = max(in_sData.aLons), $
         latmin = in_sData.aLats[0], latmax = in_sData.aLats[n_elements(in_sData.aLats)-1], compress = 1, missing = in_sData.missingVal, $
         min_value = in_sData.missingVal, mask = goodMask)
      endelse
    endif else begin
      ; for everything else
      aMappedData = in_sData.aData
      goodMask = 1
    endelse

    ; update min-max values
    if (in_sDestDesc.graphics.legend.limited eq 'yes') then begin
      minVal = in_sDestDesc.graphics.legend.minimum
      maxVal = in_sDestDesc.graphics.legend.maximum
    endif else begin
      goodIdxs = where(aMappedData ne in_sData.missingVal)
      if (goodIdxs[0] ne -1) then minVal = min(aMappedData[goodIdxs], max = maxVal) else begin
        minVal =  in_sData.missingVal
        maxVal = in_sData.missingVal
      endelse
    endelse

    nMaxLegColor = 253
    if (in_sDestDesc.graphics.legend.type eq 'continuous') then begin
      nLegColor = 253
    endif else begin
      nLegColor = in_sDestDesc.graphics.legend.ncolors
    endelse      

; scale data
    idxMissing = where(aMappedData eq in_sData.missingVal, complement=idxPresent)
;    aMappedAndScaledData = bytscl(aMappedData, min=minVal, max=maxVal, top=nLegColor-1)
 
;ToDo: Bad code! Should be rewritten!   
    idxMin = where(aMappedData le minVal)
    idxMax = where(aMappedData ge maxVal)
    idxMid = where((aMappedData gt minVal) and (aMappedData lt maxVal))
    
    aMappedAndScaledData = aMappedData
    aMappedAndScaledData[idxMin] = 1

    minValMid = min(aMappedData[idxMid], max=maxValMid)
    if (in_sDestDesc.graphics.colorscale eq 'linear') then begin
;      aMappedAndScaledData[idxMid] = bytscl(aMappedData[idxMid], top=nLegColor-3)+2
      aMappedAndScaledData[idxMid] = fix((nLegColor-3+0.9999) * (aMappedData[idxMid] - minValMid)/(maxValMid - minValMid)) + 2
    endif else begin
      aTmp = alog10((nLegColor-1)*(aMappedData[idxMid] - minValMid)/(maxValMid - minValMid)+1)
      minValMid = min(aTmp, max=maxValMid)
      aMappedAndScaledData[idxMid] = fix((nLegColor-3+0.9999) * (aTmp - minValMid)/(maxValMid - minValMid)) + 2 
    endelse
    
    if (idxMax[0] ne -1) then aMappedAndScaledData[idxMax] = nLegColor
    aMappedAndScaledData = aMappedAndScaledData*goodMask
    if (idxMissing[0] ne -1) then aMappedAndScaledData[idxMissing] = colorBG

; plotting data
    case in_sDestDesc.graphics.kind of 
      'shaded':  tv, aMappedAndScaledData, startXPos, startYPos, xsize = imageXSize, ysize = imageYSize
      'contour': begin
                   dataMinVal = min(aMappedData[where(aMappedData ne in_sData.missingVal)], max = dataMaxVal)
                   beta = (252. / (maxVal - minVal)) * (dataMinVal - minVal)
                   alpha = (252. / (maxVal - minVal)) * (dataMaxVal - minVal) - beta
                   c_colors = alpha*findgen(in_sDestDesc.graphics.steps)/(in_sDestDesc.graphics.steps-1) + beta
                   idxs = where(c_colors lt 0) & if (idxs[0] ne -1) then c_colors[idxs] = 0
                   idxs = where(c_colors gt 252) & if (idxs[0] ne -1) then c_colors[idxs] = 252
                   if (size(in_sData[0].aData, /n_dim) eq 1) then begin ; station data
                     data = in_sData[0].aData & aLons = in_sData[0].aLons & aLats = in_sData[0].aLats  
                     contour, data, aLons, aLats, nlevels=in_sDestDesc.graphics.steps, /overplot, $
                     min_value = minVal, max_value = maxVal, /irregular, c_charthick = thickness, $
                     c_colors = c_colors, c_labels = replicate(1.0, in_sDestDesc.graphics.steps), thick = thickness, c_charsize = 1
;                     a = findgen(17) * (!pi*2/16.)
;                     usersym, cos(a), sin(a), /fill 
;                     plots, in_sData[0].aLons, in_sData[0].aLats, color=byte(aMappedAndScaledData), psym=8
                   endif
                   if (size(in_sData[0].aData, /n_dim) eq 2) then begin ; map data
; commented due to limited capabilities of GDL Contour
;                     contour, in_sData[0].aData, in_sData[0].aLons, in_sData[0].aLats, nlevels=in_sDestDesc.graphics.steps, /overplot, $
;                     min_value = minVal, max_value = maxVal, c_charthick = thickness, $
;                     c_colors = c_colors, c_labels = replicate(1.0, in_sDestDesc.graphics.steps), thick = thickness, c_charsize = 1
                     contour, in_sData[0].aData, in_sData[0].aLons, in_sData[0].aLats, nlevels=in_sDestDesc.graphics.steps, /overplot, $
                     min_value = minVal, max_value = maxVal;, $
;                     c_colors = c_colors, thick = thickness, c_charsize = 1
                   endif
                 end
      'station': begin
                   a = findgen(17) * (!pi*2/16.)
                   usersym, cos(a), sin(a), /fill 
                   plots, in_sData[0].aLons, in_sData[0].aLats, color=byte(aMappedAndScaledData), psym=8
                 end
       'vector': velovect, in_sData[0].aData, in_sData[1].aData, in_sData[0].aLons, in_sData[0].aLats, /overplot, color = colorFG, $
                   missing = -in_sData[0].missingVal, length = 2.0, thick = thickness ;, $
;                  min_value = minVal, max_value = maxVal
       else: begin
               self->printLog, 'Hm, should not be here... something is wrong!'
               return, -1
             end                
    endcase
    if (in_sDestDesc.coastline.visible eq 'yes') then begin
      if (in_sDestDesc.coastline.hires eq 'yes') then begin
        map_continents, /coasts, mlinethick = 1.5, color = colorFG, /hires
      endif else map_continents, /coasts, mlinethick = 1.5, color = colorFG
    endif
    if (in_sDestDesc.rivers.visible eq 'yes') then begin
      if (in_sDestDesc.rivers.hires eq 'yes') then begin
        map_continents, /rivers, mlinethick = 1.5, color = 64, /hires
      endif else map_continents, /rivers, mlinethick = 1.5, color = 64
    endif
    if (in_sDestDesc.countries.visible eq 'yes') then begin
      if (in_sDestDesc.countries.hires eq 'yes') then begin
        map_continents, /countries, mlinethick = 1.5, color = 250, /hires
      endif else map_continents, /countries, mlinethick = 1.5, color = 250
    endif
;stop
    if (in_sDestDesc.projection.name eq 'cylindrical') then begin
      map_grid, /box_axes, charsize=1.8, glinethick=2.5, charthick=thickness, color = colorFG, londel = mapLonDel, latdel = mapLatDel
    endif else begin
      map_grid, label=1, charsize=1.8, glinethick=2.5, charthick=thickness, color = colorFG, londel = mapLonDel, latdel = mapLatDel  
    endelse

; plotting title
    xyouts, /norm, 0.5, 0.96, in_sDestDesc.graphics.title, charsize=3, $
      align=0.5, color=colorFG, charthick=thickness

    if (in_sDestDesc.graphics.legend.kind ne 'none') then begin
      labelFmt = abs(maxVal) lt 1000000. ? '(f8.'+strtrim(string(fix(5-alog10(abs(maxVal)))<5), 2)+')' : '(e8.1)' 
      sLegOpts = { title : in_sDestDesc.graphics.legend.title + ', ' + in_sDestDesc.graphics.legend.units, $ ; legend title
                   minVal : minVal, $ ; minimum value
                   maxVal : maxVal, $ ; maximum value
                   nLabels : 10, $ ; number of major steps
                   nColor : nLegColor, $ ; number of colors
                   colorScale : in_sDestDesc.graphics.colorScale, $ ; color scale
                   aLim : [0.0, 0.03, 1.35, 0.88], $ ; positions of legend's borders
                   charThick : thickness, $ ; thickness of characters 
                   charSize : 1.5, $ ; size of characters
                   titleSize : 1.7, $ ; size of title
                   titleAlign : 0, $ ; title alignment
                   labelFmt : labelFmt, $ ; label format
                   colorFG : colorFG, $ ; foreground color
                   colorBG : colorBG $ ; background color
                 }
      if (in_sDestDesc.graphics.legend.kind eq 'file') then begin
        sLegOpts.aLim = [0.0, 0.03, 1.35, 0.88] ; positions of legend's borders        
        set_plot, 'z'
        device, set_resolution=[200, 500]
;        window, xsize=200, ysize=500
        polyfill, [-1, -1, 2, 2, -1], [-1, 2, 2, -1, -1], color = colorBG, /normal
        res = self->__PlotALegend(OPTIONS = sLegOpts)  
        image = tvrd()
        if (in_sDestDesc.graphics.legend.file.type eq 'tiff') then image = reverse(image, 2)
        if (in_sDestDesc.graphics.legend.file.type ne 'xml') then begin
          write_image, in_sDestDesc.graphics.legend.file.name, in_sDestDesc.graphics.legend.file.type, image, $
            aColorTable[*, 0], aColorTable[*, 1], aColorTable[*, 2]
	endif
        set_plot, 'x'
      endif else begin
        if (in_sDestDesc.graphics.legend.position eq 'right') then begin
          sLegOpts.aLim = [1.03, 0.02, 1.2, 0.92] ; positions of legend's borders
        endif
        res = self->__PlotALegend(OPTIONS = sLegOpts)
      endelse      
    endif
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlot__define

    MAX_N_RESULTS = 128

    struct = { cvcPlot, $
;                  pInputs : ptr_new(), $
;                  apResults : make_array(MAX_N_RESULTS, value=ptr_new()), $ ; array of pointers to resulting arrays
;                  nResults : 0, $ ; number of results
		  oGlobal : obj_new(), $ ; global data
                  oConsts : OBJ_NEW(), $
                  INHERITS cvcDate, $
                  INHERITS cvcError $
             }
END
