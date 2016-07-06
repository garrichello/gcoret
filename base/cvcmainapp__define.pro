;
;  NAME: 
;    cvcMainApp
;
;  PURPOSE:
;    Main application object
;
;  INHERITS:
;    cvcError
;    
;  Public methods:
;    cvcError::Run()
;--------------------------------------------------------------------
FUNCTION cvcMainApp::Init, in_argv

; create global object
    self.oGlobal = obj_new('cvcGlobal')

; Parse arguments to extract task file and log file names
    result1 = self->__ParseArgV(in_argv, taskFileName, logFileName)

    ; OK, let's open log file
    result2 = self->openLog(logFileName)

    if (self->Assert(result1) or self->Assert(result2)) then begin
	errMsg = '(cvcMainApp) Fatal error: bad arguments or could not open log file. Aborting...'
	if (~self->Assert(result2)) then self->printLog, errMsg else print, errMsg
	if (~self->Assert(result2)) then self->closeLog
        return, 0
    endif

    if ( self->cvcError::Init(self.hLogFile) eq 0) then return, 0

    if (file_test(taskFileName)) then begin
      self->printLog, '(Init) Reading ini...'
      result = self->__ReadTask(taskFileName)
      if (self->Assert(result)) then return, 0
    endif else begin
      self->printLog, '(cvcMainApp) ERROR: No input task file: '+taskFileName+'. Aborting...'
      self->closeLog
      return, 0
    endelse
    
    res = self.oGlobal->Add(UID='hLogFile', TYPE='integer', VALUE=self.hLogFile) ; add global value
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcMainApp::Cleanup

    if ( ptr_valid( self.pTasks ) ) then ptr_free, self.pTasks
    if ( obj_valid( self.oGlobal ) ) then obj_destroy, self.oGlobal

    self->printLog, '(cvcMainApp) Finished'

    self->closeLog
END
;--------------------------------------------------------------------
;+
; :Description:
;    Main function. Called outside to do all the work.
;
; :Return value:
;    Execution error code
;
; :Author: garry
;-
FUNCTION cvcMainApp::Run

    self->printLog, '(Main) Go-go-go...'
    result = self->__Process()
    if (self->Assert(result)) then begin
      self->printLog, '(Main) Processing error!'
    endif else begin
      self->printLog, '(Main) Processing OK!'
    endelse

    return, result
END
;--------------------------------------------------------------------
FUNCTION cvcMainApp::__ParseArgV, in_argv, out_task, out_log

    out_task = ''
    out_log = ''

    argc = n_elements(in_argv)

; extract arguments
    for i = 0, argc-1 do begin
      arg = strsplit(in_argv[i], '=', /extract, count=cnt)
      if (cnt ne 2) then begin
        print, 'Wrong argument: ', in_argv[i], '. Aborting...'
        return, -1
      endif else begin
        case arg[0] of
          'task': out_task = arg[1]
          'log': out_log = arg[1]
          else:
        endcase
      endelse
    endfor

    return, 0
END
;--------------------------------------------------------------------
PRO cvcMainApp::__ParseXMLError, line, column, message
  self->printLog, "At line: "+strtrim(string(line),2)+", column: "+strtrim(string(column),2)+" occured an error: "+message
END
;--------------------------------------------------------------------
FUNCTION cvcMainApp::__GetXMLNodeByName, oNode, name
  oItem = (oNode->GetElementsByTagName( name ))->Item(0)
  if (obj_valid(oItem)) then begin
    oChild = oItem->GetFirstChild()
    if (obj_valid(oChild)) then return, oChild->GetNodeValue() 
  endif
  return, ''
END
;--------------------------------------------------------------------
FUNCTION cvcMainApp::__GetXMLAttributeByName, oNode, name, attribute
  oItem = (oNode->GetElementsByTagName( name ))->Item(0)
  if (obj_valid(oItem)) then return, oItem->GetAttribute(attribute)
  return, ''
END
;--------------------------------------------------------------------
FUNCTION cvcMainApp::__ReadDataDesc, in_oTaskNode, in_modulePath, out_pData
    oAllData = in_oTaskNode->GetElementsByTagName('data')
    nData = oAllData->GetLength()
    if (nData eq 0) then return, self->SayError(self.ERROR_NODATADESC)
    
    pData = ptr_new(make_array(nData, value = {structDataDesc}))
    for i = 0, nData-1 do begin
      oData = oAllData->Item(i)

      ; essential info
      (*pData)[i].uid = oData->GetAttribute('uid')
      (*pData)[i].type = oData->GetAttribute('type')
      (*pData)[i].kind = oData->GetAttribute('kind')

      ; Description
      (*pData)[i].description.title = self->__GetXMLAttributeByName(oData, 'description', 'title')
      (*pData)[i].description.name = self->__GetXMLAttributeByName(oData, 'description', 'name')
      (*pData)[i].description.units = self->__GetXMLAttributeByName(oData, 'description', 'units')

      ; type-dependent info
      if (((*pData)[i].type eq 'dataset') or ((*pData)[i].type eq 'datafile')) then begin

        ; Dataset
        (*pData)[i].dataset.name = self->__GetXMLAttributeByName(oData, 'dataset', 'name')
        (*pData)[i].dataset.scenario = self->__GetXMLAttributeByName(oData, 'dataset', 'scenario')
        (*pData)[i].dataset.res = self->__GetXMLAttributeByName(oData, 'dataset', 'resolution')
        (*pData)[i].dataset.tstep = self->__GetXMLAttributeByName(oData, 'dataset', 'time_step')
        (*pData)[i].dataset.path = self->__GetXMLAttributeByName(oData, 'dataset', 'path')
        (*pData)[i].module_path = self->__GetXMLAttributeByName(oData, 'dataset', 'module_path')
        if ((*pData)[i].module_path eq '') then (*pData)[i].module_path = in_modulePath else begin
	  !PATH = (*pData)[i].module_path+':' + !PATH
        endelse
        (*pData)[i].variableName = self->__GetXMLAttributeByName(oData, 'variable', 'name')
        (*pData)[i].tempk2c = self->__GetXMLAttributeByName(oData, 'variable', 'tempk2c')
        (*pData)[i].sRegion.units = self->__GetXMLAttributeByName(oData, 'region', 'units')
  
        ; Geographical region
        oRegion = (oData->GetElementsByTagName('region'))->Item(0)
  ;      if (~obj_valid(oRegion)) then return, self->SayError(self.ERROR_NOROI)
   
        if (obj_valid(oRegion)) then begin
          (*pData)[i].sRegion.units = oRegion->GetAttribute('units')
          (*pData)[i].sRegion.lon0 = oRegion->GetAttribute('lon0')
          
          oAllPoints = oRegion->GetElementsByTagName('point')       
          nPoints = oAllPoints->GetLength()
  
          oAllTiles = oRegion->GetElementsByTagName('tile')       
          nTiles = oAllTiles->GetLength()
  
          if ((nPoints eq 0) and (nTiles eq 0)) then return, self->SayError(self.ERROR_NOROIPOINTS)
         
          if (nPoints ne 0) then begin
            pPoints = ptr_new(fltarr(2, nPoints))
            for j = 0, nPoints-1 do begin
              oPoint = oAllPoints->Item(j)
              (*pPoints)[0, j] = oPoint->GetAttribute('lon')
              (*pPoints)[1, j] = oPoint->GetAttribute('lat')
            endfor
            (*pData)[i].sRegion.pPoints = pPoints
          endif
  
          if (nTiles ne 0) then begin
            pTiles = ptr_new(make_array(nTiles, value = {structTile}))
            for j = 0, nTiles-1 do begin
              oTile = oAllTiles->Item(j)
              (*pTiles)[j].path = oTile->GetAttribute('path')
              (*pTiles)[j].row = oTile->GetAttribute('row')
            endfor
            (*pData)[i].sRegion.pTiles = pTiles
          endif
        endif

        ; Levels
        oLevels = (oData->GetElementsByTagName('levels'))->Item(0)
  ;       if (~obj_valid(oLevels)) then return, self->SayError(self.ERROR_NOLEVELS)
  
        if (obj_valid(oLevels)) then begin
          levels = oLevels->GetAttribute('values')
          (*pData)[i].aLevel = strsplit(levels, ';', count=cnt, /extract)
          (*pData)[i].numLevels = cnt
;          oAllLevels = oLevels->GetElementsByTagName('level')
;          numLevels = oAllLevels->GetLength()
;          if ((numLevels lt 1) or (numLevels gt n_elements((*pData)[i].aLevel))) then return, self->SayError(self.ERROR_LEVNUM)
;    
;          (*pData)[i].numLevels = numLevels
;          for j = 0, (*pData)[i].numLevels-1 do begin
;            (*pData)[i].aLevel[j] = (oAllLevels->Item(j))->GetAttribute('name')
;          endfor
        endif else begin
          (*pData)[i].numLevels = 1
          (*pData)[i].aLevel[0] = 'Default'
        endelse
        
        ; Time segments
        oTime = (oData->GetElementsByTagName('time'))->Item(0)
  ;      if (~obj_valid(oTime)) then return, self->SayError(self.ERROR_NOTIMESEGS)
   
        if (obj_valid(oTime)) then begin
          (*pData)[i].timeTemplate = oTime->GetAttribute('template')
          timeStep = oTime->GetAttribute('step')
          oAllSegments = oTime->GetElementsByTagName('segment')
          numSegments = oAllSegments->GetLength()
          if ((numSegments lt 1) or (numSegments gt n_elements((*pData)[i].asTimeSeg))) then return, self->SayError(self.ERROR_TIMESEGNUM)
    
          (*pData)[i].numTimeSeg = numSegments
          for j = 0, numSegments-1 do begin
            (*pData)[i].asTimeSeg[j].name = (oAllSegments->Item(j))->GetAttribute('name')
            (*pData)[i].asTimeSeg[j].beginning = (oAllSegments->Item(j))->GetAttribute('beginning')
            (*pData)[i].asTimeSeg[j].ending = (oAllSegments->Item(j))->GetAttribute('ending')
            (*pData)[i].asTimeSeg[j].step = timeStep
          endfor
        endif else begin
          (*pData)[i].numTimeSeg = 1
        endelse
      endif else begin ; (dataType eq 'dataset')
        if ((*pData)[i].type eq 'array') then begin
          ; Levels
          (*pData)[i].numLevels = 1
          (*pData)[i].aLevel[0] = 'Default'
          
        ; Time segments
          oTime = (oData->GetElementsByTagName('time'))->Item(0)
     
          if (obj_valid(oTime)) then begin
            (*pData)[i].timeTemplate = oTime->GetAttribute('template')
            timeStep = oTime->GetAttribute('step')
            oAllSegments = oTime->GetElementsByTagName('segment')
            numSegments = oAllSegments->GetLength()
            if ((numSegments lt 1) or (numSegments gt n_elements((*pData)[i].asTimeSeg))) then return, self->SayError(self.ERROR_TIMESEGNUM)
      
            (*pData)[i].numTimeSeg = numSegments
            for j = 0, numSegments-1 do begin
              (*pData)[i].asTimeSeg[j].name = (oAllSegments->Item(j))->GetAttribute('name')
              (*pData)[i].asTimeSeg[j].beginning = (oAllSegments->Item(j))->GetAttribute('beginning')
              (*pData)[i].asTimeSeg[j].ending = (oAllSegments->Item(j))->GetAttribute('ending')
              (*pData)[i].asTimeSeg[j].step = timeStep
            endfor
          endif else begin
            (*pData)[i].numTimeSeg = 1
          endelse
        endif else begin; (dataType eq 'array')
          if ((*pData)[i].type eq 'parameter') then begin
            oAllParams = oData->GetElementsByTagName('param')
            numParams = oAllParams->GetLength()
;            if ((numParams lt 1) or (numParams gt n_elements((*pData)[i].asParam))) then return, self->SayError(self.ERROR_PARAMSNUM)
      
            (*pData)[i].numParams = numParams
            for j = 0, numParams-1 do begin
              (*pData)[i].asParam[j].uid = (oAllParams->Item(j))->GetAttribute('uid')
              (*pData)[i].asParam[j].type = (oAllParams->Item(j))->GetAttribute('type')
              paramValue = ((oAllParams->Item(j))->GetFirstChild())->GetNodeValue()
              case (*pData)[i].asParam[j].type of
                'float': (*pData)[i].asParam[j].data = ptr_new(float(paramValue))
                'integer': (*pData)[i].asParam[j].data = ptr_new(fix(paramValue))
                'string': (*pData)[i].asParam[j].data = ptr_new(paramValue)
                'fltarr': (*pData)[i].asParam[j].data = ptr_new(float(strsplit(paramValue, ';', /extract)))
                'intarr': (*pData)[i].asParam[j].data = ptr_new(fix(strsplit(paramValue, ';', /extract)))
                'strarr': (*pData)[i].asParam[j].data = ptr_new(strsplit(paramValue, ';', /extract))
                'binary': (*pData)[i].asParam[j].data = ptr_new(byte(paramValue))
                else: self->printLog, 'Warning! Unknown parameter type: '+(*pData)[i].asParam[j].type+'. Skipping.'
              endcase
            endfor
            
          endif
        endelse
      endelse
    endfor

    out_pData = pData

    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcMainApp::__SetDefaultDestination, in_sDest

    in_sDest = {structDestDesc}

    in_sDest.uid = 'Image1'
    in_sDest.type = 'image'
    ; Projection properties
    in_sDest.projection.name = 'cylindrical'
    nLimits = 4
    in_sDest.projection.limits.left = -180. 
    in_sDest.projection.limits.right = 180.
    in_sDest.projection.limits.top = 90. 
    in_sDest.projection.limits.bottom = -90.
    ; Coastline
    in_sDest.coastline.visible = 'yes'
    in_sDest.coastline.hires = 'no'
    ; Rivers
    in_sDest.rivers.visible = 'no'
    in_sDest.rivers.hires = 'no'
    ; Countries
    in_sDest.countries.visible = 'no'
    in_sDest.countries.hires = 'no'
    ; Graphics properties
    in_sDest.graphics.title = 'Image title'
    in_sDest.graphics.kind = 'shaded'
    in_sDest.graphics.width_units = 'pixels'
    in_sDest.graphics.width = 1024
    in_sDest.graphics.height_units = 'pixels'
    in_sDest.graphics.height = 768
    ; axes specifications
    in_sDest.graphics.axis.x.name = 'Longitude'
    in_sDest.graphics.axis.x.units = 'degrees'
    in_sDest.graphics.axis.x.scale = 'linear'
    in_sDest.graphics.axis.x.visible = 'yes'
    in_sDest.graphics.axis.y.name = 'Latitude'
    in_sDest.graphics.axis.y.units = 'degrees'
    in_sDest.graphics.axis.y.scale = 'linear'
    in_sDest.graphics.axis.y.visible = 'yes'
    in_sDest.graphics.axis.z.name = 'Altitude'
    in_sDest.graphics.axis.z.units = 'm'
    in_sDest.graphics.axis.z.scale = 'linear'
    in_sDest.graphics.axis.z.visible = 'no'
    ; lines specifications
    in_sDest.graphics.nLines = 0
    ; legend specifications
    in_sDest.graphics.legend.title = 'Value'
    in_sDest.graphics.legend.kind = 'embedded'
    in_sDest.graphics.legend.position = 'right'
    in_sDest.graphics.legend.units = 'K'
    in_sDest.graphics.legend.limited = 'no'
    in_sDest.graphics.legend.minimum = 0
    in_sDest.graphics.legend.maximum = 0
    in_sDest.graphics.legend.file.name = ''
    in_sDest.graphics.legend.file.type = ''
    ; other graphics properties
    in_sDest.graphics.colortable = 'RAINBOW'
    in_sDest.graphics.colorscale = 'linear'
    in_sDest.graphics.blackbg = 'no'
    in_sDest.graphics.smoothing = 'yes'
    in_sDest.graphics.steps = 60
    ; File properties
    in_sDest.file.name = 'output'
    in_sDest.file.type = 'eps'
    
END
;--------------------------------------------------------------------
FUNCTION cvcMainApp::__ReadDestDesc, in_oTaskNode, out_pDest
    oAllDest = in_oTaskNode->GetElementsByTagName('destination')
    nDest = oAllDest->GetLength()
    if (nDest eq 0) then return, self->SayError(self.ERROR_NODESTDESC)
    
    pDest = ptr_new(make_array(nDest, value = {structDestDesc})) 
        
    for i = 0, nDest-1 do begin
      oDest = oAllDest->Item(i)
      ; Filling structure with default values
      self->__SetDefaultDestination, sDefDest
      (*pDest)[i] = temporary(sDefDest)

      (*pDest)[i].uid = oDest->GetAttribute('uid')
      (*pDest)[i].type = oDest->GetAttribute('type')
      ; Projection properties
      oProjection = (oDest->GetElementsByTagName('projection'))->Item(0)
      if (obj_valid(oProjection)) then begin ;return, self->SayError(self.ERROR_NOPROJ)
        (*pDest)[i].projection.name = oProjection->GetAttribute('name')
        (*pDest)[i].projection.p0lon = self->__GetXMLNodeByName(oProjection, 'p0lon')
        (*pDest)[i].projection.p0lat = self->__GetXMLNodeByName(oProjection, 'p0lat')
        (*pDest)[i].projection.rot = self->__GetXMLNodeByName(oProjection, 'rot')        
        oLimits = (oProjection->GetElementsByTagName('limits'))->Item(0)
        if (obj_valid(oLimits)) then begin ; return, self->SayError(self.ERROR_NOLIMITS)
          oAllLimits = oLimits->GetElementsByTagName('limit')
          nLimits = oAllLimits->GetLength()
          for j = 0, nLimits-1 do begin
            oLimit = oAllLimits->Item(j)
            case oLimit->GetAttribute('role') of
              'left': (*pDest)[i].projection.limits.left = (oLimit->GetFirstChild())->GetNodeValue() 
              'right': (*pDest)[i].projection.limits.right = (oLimit->GetFirstChild())->GetNodeValue()
              'top': (*pDest)[i].projection.limits.top = (oLimit->GetFirstChild())->GetNodeValue() 
              'bottom': (*pDest)[i].projection.limits.bottom = (oLimit->GetFirstChild())->GetNodeValue()
              else: return, self->SayError(self.ERROR_LIMITROLE)
            endcase
          endfor
        endif
      endif
      ; Coastline
      (*pDest)[i].coastline.visible = self->__GetXMLAttributeByName(oDest, 'coastline', 'visible')
      (*pDest)[i].coastline.hires = self->__GetXMLAttributeByName(oDest, 'coastline', 'hires')
      ; Rivers
      (*pDest)[i].rivers.visible = self->__GetXMLAttributeByName(oDest, 'rivers', 'visible')
      (*pDest)[i].rivers.hires = self->__GetXMLAttributeByName(oDest, 'rivers', 'hires')
      ; Countries
      (*pDest)[i].countries.visible = self->__GetXMLAttributeByName(oDest, 'countries', 'visible')
      (*pDest)[i].countries.hires = self->__GetXMLAttributeByName(oDest, 'countries', 'hires')
      ; Graphics properties
      oGraphics = (oDest->GetElementsByTagName('graphics'))->Item(0)
      if (obj_valid(oGraphics)) then begin ; return, self->SayError(self.ERROR_NOGRAPH)
        (*pDest)[i].graphics.title = oGraphics->GetAttribute('title')
        (*pDest)[i].graphics.kind = oGraphics->GetAttribute('kind')
        (*pDest)[i].graphics.width_units = self->__GetXMLAttributeByName(oGraphics, 'width', 'units')
        (*pDest)[i].graphics.width = self->__GetXMLNodeByName(oGraphics, 'width')
        (*pDest)[i].graphics.height_units = self->__GetXMLAttributeByName(oGraphics, 'height', 'units')
        (*pDest)[i].graphics.height = self->__GetXMLNodeByName(oGraphics, 'height')
        ; axes specifications
        oAxes = (oGraphics->GetElementsByTagName('axis'))->Item(0)
        if (obj_valid(oAxes)) then begin ; return, self->SayError(self.ERROR_NOAXES)
          (*pDest)[i].graphics.axis.x.name = self->__GetXMLAttributeByName(oAxes, 'x', 'name')
          (*pDest)[i].graphics.axis.x.units = self->__GetXMLAttributeByName(oAxes, 'x', 'units')
          (*pDest)[i].graphics.axis.x.scale = self->__GetXMLAttributeByName(oAxes, 'x', 'scale')
          (*pDest)[i].graphics.axis.x.minVal = self->__GetXMLAttributeByName(oAxes, 'x', 'min')
          (*pDest)[i].graphics.axis.x.maxVal = self->__GetXMLAttributeByName(oAxes, 'x', 'max')
          (*pDest)[i].graphics.axis.x.visible = self->__GetXMLAttributeByName(oAxes, 'x', 'visible')
          (*pDest)[i].graphics.axis.y.name = self->__GetXMLAttributeByName(oAxes, 'y', 'name')
          (*pDest)[i].graphics.axis.y.units = self->__GetXMLAttributeByName(oAxes, 'y', 'units')
          (*pDest)[i].graphics.axis.y.scale = self->__GetXMLAttributeByName(oAxes, 'y', 'scale')
          (*pDest)[i].graphics.axis.y.minVal = self->__GetXMLAttributeByName(oAxes, 'y', 'min')
          (*pDest)[i].graphics.axis.y.maxVal = self->__GetXMLAttributeByName(oAxes, 'y', 'max')
          (*pDest)[i].graphics.axis.y.visible = self->__GetXMLAttributeByName(oAxes, 'y', 'visible')
          (*pDest)[i].graphics.axis.z.name = self->__GetXMLAttributeByName(oAxes, 'z', 'name')
          (*pDest)[i].graphics.axis.z.units = self->__GetXMLAttributeByName(oAxes, 'z', 'units')
          (*pDest)[i].graphics.axis.z.scale = self->__GetXMLAttributeByName(oAxes, 'z', 'scale')
          (*pDest)[i].graphics.axis.z.minVal = self->__GetXMLAttributeByName(oAxes, 'z', 'min')
          (*pDest)[i].graphics.axis.z.maxVal = self->__GetXMLAttributeByName(oAxes, 'z', 'max')
          (*pDest)[i].graphics.axis.z.visible = self->__GetXMLAttributeByName(oAxes, 'z', 'visible')
        endif
        ; lines specifications
        oLines = (oGraphics->GetElementsByTagName('lines'))->Item(0)
        if (obj_valid(oLines)) then begin ; return, self->SayError(self.ERROR_NOLINES)
          oAllLines = oLines->GetElementsByTagName('line')
          nLines = oAllLines->GetLength()
          if ((nLines lt 1) or (nLines gt n_elements((*pDest)[i].graphics.asLines))) then return, self->SayError(self.ERROR_NOLINES)
          (*pDest)[i].graphics.nLines = nLines
          for j = 0, nLines-1 do begin
            oLine = oAllLines->Item(j)
            (*pDest)[i].graphics.asLines[j].name = oLine->GetAttribute('name')
            (*pDest)[i].graphics.asLines[j].style = oLine->GetAttribute('style')
            (*pDest)[i].graphics.asLines[j].color = oLine->GetAttribute('color')
            (*pDest)[i].graphics.asLines[j].symbol = oLine->GetAttribute('symbol')
            (*pDest)[i].graphics.asLines[j].thick = oLine->GetAttribute('thick')
            (*pDest)[i].graphics.asLines[j].visible = oLine->GetAttribute('visible')
          endfor
        endif
        ; legend specifications
        oLegend = (oGraphics->GetElementsByTagName('legend'))->Item(0)
        if (obj_valid(oLegend)) then begin ; return, self->SayError(self.ERROR_NOLEGEND)
          (*pDest)[i].graphics.legend.title = oLegend->GetAttribute('title')
          (*pDest)[i].graphics.legend.kind = oLegend->GetAttribute('kind')
          (*pDest)[i].graphics.legend.type = oLegend->GetAttribute('type')
          (*pDest)[i].graphics.legend.position = oLegend->GetAttribute('position')
          (*pDest)[i].graphics.legend.units = self->__GetXMLNodeByName(oLegend, 'units')
          (*pDest)[i].graphics.legend.ncolors = self->__GetXMLNodeByName(oLegend, 'ncolors')          
          (*pDest)[i].graphics.legend.nlabels = self->__GetXMLNodeByName(oLegend, 'nlabels')          
          (*pDest)[i].graphics.legend.limited = self->__GetXMLNodeByName(oLegend, 'limited')
          (*pDest)[i].graphics.legend.minimum = self->__GetXMLNodeByName(oLegend, 'minimum')
          (*pDest)[i].graphics.legend.maximum = self->__GetXMLNodeByName(oLegend, 'maximum')
          (*pDest)[i].graphics.legend.file.name = self->__GetXMLAttributeByName(oLegend, 'file', 'name')
          (*pDest)[i].graphics.legend.file.type = self->__GetXMLAttributeByName(oLegend, 'file', 'type')
        endif
        ; other graphics properties
        (*pDest)[i].graphics.colortable = self->__GetXMLNodeByName(oGraphics, 'colortable')
        (*pDest)[i].graphics.colorscale = self->__GetXMLNodeByName(oGraphics, 'colorscale')
        (*pDest)[i].graphics.blackbg = self->__GetXMLNodeByName(oGraphics, 'blackbg')
        (*pDest)[i].graphics.smoothing = self->__GetXMLNodeByName(oGraphics, 'smoothing')
        (*pDest)[i].graphics.steps = self->__GetXMLNodeByName(oGraphics, 'steps')
      endif
      ; File properties
      (*pDest)[i].file.name = self->__GetXMLAttributeByName(oDest, 'file', 'name')
      (*pDest)[i].file.type = self->__GetXMLAttributeByName(oDest, 'file', 'type')
    endfor
    
    out_pDest = pDest
    
    return, self.ERROR_OK
    
END
;--------------------------------------------------------------------
FUNCTION cvcMainApp::__ReadProcDesc, in_oTaskNode, out_pProc
    oAllProc = in_oTaskNode->GetElementsByTagName('processing')
    nProc = oAllProc->GetLength()
    if (nProc eq 0) then return, self->SayError(self.ERROR_NOPROCDESC)
    
    pProc = ptr_new(make_array(nProc, value = {structProcDesc})) 
    
    for i = 0, nProc-1 do begin
      oProc = oAllProc->Item(i)
      (*pProc)[i].uid = oProc->GetAttribute('uid')
      (*pProc)[i].class = oProc->GetAttribute('class')
      ; inputs
      oAllInput = oProc->GetElementsByTagName('input')
      nInputs = oAllInput->GetLength()      
      if (nInputs gt n_elements((*pProc)[i].asInputs)) then return, self->SayError(self.ERROR_BADINPUTSNUM)    
      (*pProc)[i].nInputs = nInputs
      for j = 0, nInputs-1 do begin
        oInput = oAllInput->Item(j)
        (*pProc)[i].asInputs[j].uid = oInput->GetAttribute('uid')
        (*pProc)[i].asInputs[j].data = oInput->GetAttribute('data')
      endfor
      ; outputs
      oAllOutput = oProc->GetElementsByTagName('output')
      nOutputs = oAllOutput->GetLength()      
      if (nOutputs gt n_elements((*pProc)[i].asOutputs)) then return, self->SayError(self.ERROR_BADOUTPUTSNUM)    
      (*pProc)[i].nOutputs = nOutputs
      for j = 0, nOutputs-1 do begin
        oOutput = oAllOutput->Item(j)
        (*pProc)[i].asOutputs[j].uid = oOutput->GetAttribute('uid')
        (*pProc)[i].asOutputs[j].data = oOutput->GetAttribute('data')
      endfor
;      ; writes
;      oAllWrite = oProc->GetElementsByTagName('write')
;      nWrites = oAllWrite->GetLength()      
;      if (nWrites gt n_elements((*pProc)[i].asWrites)) then return, self->SayError(self.ERROR_BADWRITESNUM)    
;      (*pProc)[i].nWrites = nWrites
;      for j = 0, nWrites-1 do begin
;        oWrite = oAllWrite->Item(j)
;        (*pProc)[i].asWrites[j].data = oOutput->GetAttribute('data')
;        (*pProc)[i].asWrites[j].destination = oInput->GetAttribute('destination')
;      endfor
    endfor

    out_pProc = pProc
    
    return, self.ERROR_OK
END

;--------------------------------------------------------------------
;+
; :Description:
;    Reads initialization information from file self.iniFileName into the internal structure 'self.ssIniData'
;
; :Return value:
;    Execution error code
;    
; :Author: garry
;-
FUNCTION cvcMainApp::__ReadTask, in_iniFileName
    
    catch, error
    if (error ne 0) then begin
      catch, /cancel
      return, self->SayError(self.ERROR_BADXML)
    endif
    oDocument = obj_new( 'IDLffXMLDOMDocument' )
    oDocument->Load, filename=in_iniFileName, msg_error='cvcMainApp::__ParseXMLError', msg_fatal='cvcMainApp::__ParseXMLError'
    catch, /cancel
    
    oAllTasks = oDocument->GetElementsByTagName('task')
    nTasks = oAllTasks->GetLength()
    pTasks = ptr_new(make_array(nTasks, value = {structTask}))
; Reading tasks
    for i = 0, nTasks-1 do begin
      oTask = oAllTasks->Item(i)
      if (~obj_valid(oTask)) then begin
          obj_destroy, oDocument
          return, self->SayError(self.ERROR_NOTASK)
      endif
      (*pTasks)[i].uid = oTask->GetAttribute('uid')
      (*pTasks)[i].owner = oTask->GetAttribute('owner')
      (*pTasks)[i].description = oTask->GetAttribute('description')
      root_path = oTask->GetAttribute('root_path')
      if (root_path ne '') then begin
        !PATH = root_path+':' + root_path+'mod/calc/:' + root_path+'mod/plot/:' + root_path+'mod/data/:' + !PATH
      endif
      (*pTasks)[i].root_path = root_path
      (*pTasks)[i].data_module_path = oTask->GetAttribute('data_module_path')
      if ((*pTasks)[i].data_module_path ne '') then begin
        !PATH = (*pTasks)[i].data_module_path+':' + !PATH
      endif else (*pTasks)[i].data_module_path = root_path+'mod/data/'

      ; Reading metadata base description
      self->printLog, 'Reading metadata base description...'
      oMDBDesc = (oTask->GetElementsByTagName('metadb'))->Item(0)
      if (obj_valid(oMDBDesc)) then begin
        (*pTasks)[i].metadb.host = oMDBDesc->GetAttribute('host')
        (*pTasks)[i].metadb.name = oMDBDesc->GetAttribute('name')
        (*pTasks)[i].metadb.user = oMDBDesc->GetAttribute('user')
        (*pTasks)[i].metadb.password = oMDBDesc->GetAttribute('password')
      endif
      
      ; Reading data descriptions
      self->printLog, 'Reading data descriptions...'
      result = self->__ReadDataDesc(oTask, (*pTasks)[i].data_module_path, pDataDesc)
      if (self->Assert(result)) then return, result
      (*pTasks)[i].pDataDesc = pDataDesc

      ; Reading destination descriptions
      self->printLog, 'Reading destination descriptions...'
      result = self->__ReadDestDesc(oTask, pDestDesc)
      if (self->Assert(result)) then return, result
      (*pTasks)[i].pDestDesc = pDestDesc
      
      ; Reading processing descriptions
      self->printLog, 'Reading processing descriptions...'
      result = self->__ReadProcDesc(oTask, pProcDesc)   
      if (self->Assert(result)) then return, result
      (*pTasks)[i].pProcDesc = pProcDesc
    endfor
      
    self.pTasks = pTasks
    
    obj_destroy, oDocument
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
;+
; :Description:
;   Processing of data. Decides what class to use for processing.
;
; :Return value:
;   Execution error code
;
; :Author: garry
;-

FUNCTION cvcMainApp::__Process

  for iTask = 0, n_elements(*self.pTasks)-1 do begin
    sTask = (*self.pTasks)[iTask]
; prepare global data (common for all modules)
    res = self.oGlobal->Add(UID='metadb', TYPE='structure', VALUE=sTask.metadb)
    for iProc = 0, n_elements(*sTask.pProcDesc)-1 do begin
      sProc = (*sTask.pProcDesc)[iProc]
      
; prepare input data
      if (sProc.nInputs eq 0) then return, self->SetErrMsg('No input for module!')
      asInputs = make_array(sProc.nInputs, value={structArgDesc})
      for iInput = 0, sProc.nInputs-1 do begin
        ; search for inputs
        idx = where((*sTask.pDataDesc).uid eq sProc.asInputs[iInput].data)
        if (idx ne -1) then begin
          asInputs[iInput].uid = sProc.asInputs[iInput].uid
          asInputs[iInput].type = 'data'
          asInputs[iInput].data = ptr_new((*sTask.pDataDesc)[idx])
        endif 
        ; search for destinations
        idx = where((*sTask.pDestDesc).uid eq sProc.asInputs[iInput].data)
        if (idx ne -1) then begin
          asInputs[iInput].uid = sProc.asInputs[iInput].uid
          asInputs[iInput].type = 'destination'
          asInputs[iInput].data = ptr_new((*sTask.pDestDesc)[idx])
        endif
; ToDo: Error handler should be here!        
      endfor
        
; prepare output data (if there any)
      if (sProc.nOutputs gt 0) then begin
        asOutputs = make_array(sProc.nOutputs, value={structArgDesc})
        for iOutput = 0, sProc.nOutputs-1 do begin
          ; search for outputs
          idx = where((*sTask.pDataDesc).uid eq sProc.asOutputs[iOutput].data)
          if (idx ne -1) then begin
            asOutputs[iOutput].uid = sProc.asOutputs[iOutput].uid
            asOutputs[iOutput].type = 'data'
            asOutputs[iOutput].data = ptr_new((*sTask.pDataDesc)[idx])
          endif 
          ; search for destinations
          idx = where((*sTask.pDestDesc).uid eq sProc.asOutputs[iOutput].data)
          if (idx ne -1) then begin
            asOutputs[iOutput].uid = sProc.asOutputs[iOutput].uid
            asOutputs[iOutput].type = 'destination'
            asOutputs[iOutput].data = ptr_new((*sTask.pDestDesc)[idx])
          endif 
  ; ToDo: Error handler should be here!        
        endfor
      endif
      
;=============================================================
; try to create object of a processing module
      catch, errorStatus
      if (errorStatus ne 0) then begin
        catch, /cancel
        return, self->SayError(self.ERROR_BADMODULENAME, sProc.class)
      endif
;---------------------------------------------------------------------------------------------------------------------       
      oProc = obj_new( sProc.class, asInputs, asOutputs, self.oGlobal ) ; <-- here we pass input and output arguments |
;---------------------------------------------------------------------------------------------------------------------      
      catch, /cancel
;=============================================================
; check if object was created
      if (~obj_valid(oProc)) then begin
        self->printLog, 'Processing module is not initialized. Aborting!'
        return, -1
      endif
        
; try to execute Run method of the processing module      
      catch, errorStatus
      if (errorStatus ne 0) then begin
        catch, /cancel
        return, self->SayError(self.ERROR_ERRORINMODULE, sProc.class)
      endif    
;     
;================RUN FOREST, RUN!!!=========================
      retCode = oProc->Run()
;===========================================================
;     
      catch, /cancel
; check result code
      if (self->Assert(retCode)) then return, self.ERROR_ERRORINMODULE

; get output data
      for iOutput = 0, sProc.nOutputs-1 do begin
        idx = where((*sTask.pDataDesc).uid eq sProc.asOutputs[iOutput].data)
        if (idx ne -1) then begin
          (*((*self.pTasks)[iTask]).pDataDesc)[idx] = *asOutputs[iOutput].data          
        endif 
; ToDo: Error handler should be here! 
      endfor
      
      if ( obj_valid( oProc ) ) then obj_destroy, oProc ; destroy process to free memory

    endfor
  endfor
    
  return, self->SayError(retCode)
END
;--------------------------------------------------------------------
PRO cvcMainApp__define

; class members definition
    struct = { cvcMainApp, $
               ; initial data             
               pTasks : ptr_new(), $ ; pointer to array of structures { structTask }
               oGlobal : obj_new(), $ ; pointer to an object with global functions and variables
               INHERITS cvcError $
             }

END
