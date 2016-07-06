;
;  NAME: 
;    cvcPlotMultiline
;
;  PURPOSE:
;    High-level class. Provides plotting of results. Multiline 2D-plot.
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcPlotMultiline::Init, in_hLogFile

    res = self->cvcPlot::Init(in_hLogFile)

    return, res
END
;--------------------------------------------------------------------
PRO cvcPlotMultiline::Cleanup
 
    if (obj_valid(self.oConsts)) then obj_destroy, self.oConsts
    set_plot, 'x'
END
;--------------------------------------------------------------------
FUNCTION cvcPlotMultiline::__PlotLines, in_asData, in_sDestDesc

    aLineStyles = ['solid', 'dotted', 'dashed', 'dash dot', 'dash dot dot', 'long dashes']
    aPsym = ['none', 'plus', 'asterisk', 'period', 'diamond', 'triangle', 'square', 'X']

; set CT
    loadct, 13
    tvlct, aColorTable, /get
    aColorTable[0, *] = 0b
    aColorTable[254, *] = 192b
    aColorTable[255, *] = 255b
    tvlct, aColorTable

; set background and foreground colors
    if (in_sDestDesc.graphics.blackbg eq 'yes') then begin
      colorFG = 255
      colorBG = 0
    endif else begin
      colorFG = 0
      colorBG = 255
    endelse

    iData = 0 ; for now we are able to process only one input
    missingVal = in_asData[iData].missingVal

    case self.xaxis of
      'lon': xData = *in_asData[iData].pXgrid
      'lat': xData = *in_asData[iData].pYgrid
      'time': xData = *in_asData[iData].pTimeGrid
      'data': xData = *in_asData[iData].data
      else: begin
        print, 'Error! Unknown axis name: '+self.xaxis
        return, -1
      end
    endcase

    case self.yaxis of
      'lon': yData = *in_asData[iData].pXgrid
      'lat': yData = *in_asData[iData].pYgrid
      'time': yData = *in_asData[iData].pTimeGrid
      'data': yData = *in_asData[iData].data
      else: begin
        print, 'Error! Unknown axis name: '+self.yaxis
        return, -1
      end
    endcase
    
; number of data arrays
    nData = n_elements(in_asData)

; use a range given in input file or calculate automatically    
    if (in_sDestDesc.graphics.axis.x.minVal eq in_sDestDesc.graphics.axis.x.maxVal) then begin
      minXval = 1e33
      maxXval = -1e33
      goodIdx = where(xData ne missingVal)
      if (goodIdx[0] eq -1) then begin
        print, 'Error! All values of xGrid are missing!'
        return, -1
      endif
      for iData = 0, nData-1 do begin
        minXval = min([xData[goodidx], minXval])
        maxXval = max([xData[goodidx], maxXval])
      endfor
    endif else begin
      minXval = in_sDestDesc.graphics.axis.x.minVal
      maxXval = in_sDestDesc.graphics.axis.x.maxVal
    endelse
    
    if (in_sDestDesc.graphics.axis.y.minVal eq in_sDestDesc.graphics.axis.y.maxVal) then begin 
      minYval = 1e33
      maxYval = -1e33
      goodIdx = where(yData ne missingVal)
      if (goodIdx[0] eq -1) then begin
        print, 'Error! All values of yGrid are missing!'
        return, -1
      endif
      for iData = 0, nData-1 do begin
        minYval = min([yData[goodIdx], minYval])
        maxYval = max([yData[goodIdx], maxYval])
      endfor
    endif else begin
      minYval = in_sDestDesc.graphics.axis.y.minVal
      maxYval = in_sDestDesc.graphics.axis.y.maxVal
    endelse
    
; search for data arrays inside 2-D input grids (if any)
    xDataSize = size(xData)
    yDataSize = size(yData)
    if (xDataSize[0] eq 2) then nData = xDataSize[2]
    if (yDataSize[0] eq 2) then nData = yDataSize[2]
    
;    res = label_date(date_format='%D/%N!C%Y')
    
    if (strlowcase(in_sDestDesc.graphics.axis.x.name) eq 'time') then begin
      xTickUnits = 'xTime'
      xTickFormat = '' ;label_date'
;      xTitle = '!C'+in_sDestDesc.graphics.axis.x.name
      xTitle = in_sDestDesc.graphics.axis.x.name
    endif else begin
      xTickUnits = 'Numeric'
      xTickUnits = ''
      xTitle = in_sDestDesc.graphics.axis.x.name+' ('+in_sDestDesc.graphics.axis.x.units+')' 
    endelse
    
    if (strlowcase(in_sDestDesc.graphics.axis.y.name) eq 'time') then begin
      yTickUnits = 'Time'
      yTickFormat = '' ;label_date'
      yTitle = in_sDestDesc.graphics.axis.y.name
    endif else begin
      yTickUnits = 'Numeric'
      yTickUnits = ''
      yTitle = in_sDestDesc.graphics.axis.y.name+' ('+in_sDestDesc.graphics.axis.y.units+')'
    endelse
    
    if (strlowcase(in_sDestDesc.graphics.axis.x.visible) eq 'yes') then begin
      xStyle = 1
    endif else begin
      xStyle = 4
    endelse
    
    if (strlowcase(in_sDestDesc.graphics.axis.y.visible) eq 'yes') then begin
      yStyle = 1
    endif else begin
      yStyle = 4
    endelse

    xTickNames = replicate('a', 10)

; prepare axis    
    plot, [1, 2], [1, 2], color = colorFG, background = colorBG, charsize=1., title = in_sDestDesc.graphics.title, /nodata, $
     xStyle = xStyle, xRange=[minXval, maxXval], xtickunits=xTickUnits, xtickformat=xTickFormat, xtitle=xTitle, $
     yStyle = yStyle, yRange=[minYval, maxYval], ytickunits=yTickUnits, ytickformat=yTickFormat, ymargin=[5, 3], ytitle=yTitle, $
     charthick = 1.5
    
; check number of data and number of lines defined    
;    if (nData ne in_sDestDesc.graphics.nLines) then begin
;      self->printLog, 'Error: number of lines defined is not equal to number of datasets!'
;      return, -1
;    endif

    autoColor = 252
    colorStep = 252 / in_sDestDesc.graphics.nLines
; plot lines    
    for iLine = 0, min([in_sDestDesc.graphics.nLines, nData])-1 do begin
      if (in_sDestDesc.graphics.asLines[iLine].visible eq 'yes') then begin
        lineStyle = where(strlowcase(in_sDestDesc.graphics.asLines[iLine].style) eq aLineStyles)
        if (in_sDestDesc.graphics.asLines[iLine].color ne 'auto') then begin
          lineColor = fsc_color(in_sDestDesc.graphics.asLines[iLine].color)
        endif else lineColor = autoColor
        if (yDataSize[0] eq 1) then begin ; 1-D y-data (usually normal)
          if (in_sDestDesc.graphics.asLines[iLine].style ne 'none') then begin ; plotting lines
            oplot, xData, yData, linestyle=lineStyle, $
             color=lineColor, min_value=missingVal, $
             thick=in_sDestDesc.graphics.asLines[iLine].thick
          endif        
          if (in_sDestDesc.graphics.asLines[iLine].symbol ne 'none') then begin ; plotting symbols
            oplot, xData,  yData, $
             psym = where(strlowcase(in_sDestDesc.graphics.asLines[iLine].symbol) eq aPsym), $
             color=lineColor, min_value=missingVal
          endif
          autoColor = autoColor - colorStep
        endif
        if (yDataSize[0] eq 2) then begin ; 2-D y-data (if data are passed as one array)
;          colorStep = 252 / dataSize[1]
;          for iSet = 0, dataSize[1]-1 do begin ; loop over individual datasets
            if (in_sDestDesc.graphics.asLines[iLine].color ne 'auto') then begin
              lineColor = fsc_color(in_sDestDesc.graphics.asLines[iLine].color)
            endif else lineColor = autoColor
            if (in_sDestDesc.graphics.asLines[iLine].style ne 'none') then begin ; plotting lines
              oplot, xData, yData[iLine, *], linestyle=lineStyle, $
               color=lineColor, min_value=missingVal, $
               thick=in_sDestDesc.graphics.asLines[iLine].thick
            endif        
            if (in_sDestDesc.graphics.asLines[iLine].symbol ne 'none') then begin ; plotting symbols
              oplot, xData, yData[iLine, *], $
               psym = where(strlowcase(in_sDestDesc.graphics.asLines[iLine].symbol) eq aPsym), $
               color=lineColor, min_value=missingVal
            endif
            autoColor = autoColor - colorStep
;          endfor
        endif        
      endif
    endfor

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcPlotMultiline::Run, in_pInputs

;    self.pInputs = in_pInputs

    ; where to plot
    idx = where((*in_pInputs).type eq 'destination')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: No destination!'
      return, -1
    endif
    sDestDesc = *((*in_pInputs)[idx[0]].data)
    
    ; what to plot (only one and the first dataset is plotted!!!)
    idx = where((*in_pInputs).type eq 'data')
    if (idx[0] eq -1) then begin
      self->printLog, 'Error: Nothing to plot!'
      return, -1
    endif
    for i = 0, n_elements(idx)-1 do begin
      if ((*(*in_pInputs)[idx[i]].data).type eq 'array') then sData = *((*in_pInputs)[idx[i]].data)
      if ((*(*in_pInputs)[idx[i]].data).type eq 'parameter') then sParams = *((*in_pInputs)[idx[i]].data)
    endfor
    
    self.xaxis = 'lon'
    self.yaxis = 'lat'
    if (n_elements(sParams) ne 0) then begin
      idx = where(sParams.asParam.uid eq 'xaxis')
      if (idx[0] ne -1) then self.xaxis = *sParams.asParam[idx[0]].data
      idx = where(sParams.asParam.uid eq 'yaxis')
      if (idx[0] ne -1) then self.yaxis = *sParams.asParam[idx[0]].data
    endif
        
    if (sDestDesc.file.name ne '') then begin
      outFileType = sDestDesc.file.type 
    endif else begin
      outFileType = 'x' ; no file name - no file. output to screen then.
    endelse
    
    case outFileType of
      'x': begin
             ret = self->__OpenX(sDestDesc)
             ret = self->__PlotLines(sData, sDestDesc)
           end
      'eps': begin
               ret = self->__OpenEPS(sDestDesc)
               ret = self->__PlotLines(sData, sDestDesc)
               device, /close
               set_plot, 'x'
             end
      else: begin
        self->printLog, 'Error! Bad file type!'
        return, -1
      end
    endcase
    if (self->Assert(ret)) then return, ret
        
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcPlotMultiline__define

    MAX_N_RESULTS = 128

    struct = { cvcPlotMultiline, $
		  xaxis : '', $
		  yaxis : '', $
                  INHERITS cvcPlot $
             }
END