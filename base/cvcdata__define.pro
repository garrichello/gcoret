;
;  NAME: 
;    cvcData
;
;  PURPOSE:
;    Base low-level class. Provides access to meteo- and climatic data. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcData::Init, in_pDataDesc, in_modulePath, in_oGlobal

    self.oGlobal = in_oGlobal
    hLogFile = self.oGlobal->Get('hLogFile')
    if ( self->cvcError::Init(hLogFile) eq 0 ) then return, 0
    
    self.oConst = obj_new('cvcConstants', self.oGlobal) ; constants class
    self.oDSInfo = obj_new( 'IDLffXMLDOMDocument' )
    self.modulePath = in_modulePath
    self.hLogFile = hLogFile
    
    self.dataset.name = (*in_pDataDesc).dataset.name
    self.dataset.scenario = (*in_pDataDesc).dataset.scenario
    self.dataset.res = (*in_pDataDesc).dataset.res
    self.dataset.tstep = (*in_pDataDesc).dataset.tstep
    self.dataset.path = (*in_pDataDesc).dataset.path
    self.dataset.filespan = (*in_pDataDesc).dataset.filespan
    self.dataset.fileTemplate = (*in_pDataDesc).dataset.fileTemplate
    self.levVarName = (*in_pDataDesc).levVarName
    self.modify.scale = (*in_pDataDesc).modify.scale
    self.modify.offset = (*in_pDataDesc).modify.offset

    self.prepared = 0
    self.debugMode = 1
    self->printLog, "(cvcData::Init) Data has been initialized"
    return, 1
END
;--------------------------------------------------------------------
PRO cvcData::Cleanup
 
    if (obj_valid(self.oConst)) then obj_destroy, self.oConst
    if (obj_valid(self.oDSInfo)) then obj_destroy, self.oDSInfo 
    if (ptr_valid(self.pDataFile)) then begin
      ptr_free, self.pDataFile
    endif
END
;--------------------------------------------------------------------
FUNCTION cvcData::Prepare, in_varName, in_sTimeSeg, in_aLevel, in_userModuleName
  self->printLog, 'ERROR! The function cvcData::Prepare() should be overrided from a child class! Aborting!'
  stop
END
;--------------------------------------------------------------------
FUNCTION cvcData::Read, in_aArea, in_aTime, in_aLevel, out_sResult
  self->printLog, 'ERROR! The function cvcData::Read() should be overrided from a child class! Aborting!'
  stop
END
;--------------------------------------------------------------------
PRO cvcData::GetProperty, sRequest = sRequest
  sRequest = self.sRequest
END
;--------------------------------------------------------------------
PRO cvcData__define

    struct = { cvcData, $
		  oGlobal : obj_new(), $ ; global data
                  prepared : 0, $
                  debugMode : 0, $
                  modulePath : '', $
                  dataset : {name : '', scenario : '', res : '', tstep : '', filespan : '', path : '', fileTemplate : ''}, $
		  modify : { scale : 1.0, offset : 0.0}, $
                  pDataFile : ptr_new(), $
;                  dataFileType : '', $
                  oConst : obj_new(), $
                  oDSInfo : obj_new(), $
                  levVarName : '', $
                  sRequest : {structDataReq}, $
                  sROIBounds : {ROIBounds, lon0 : 0., lat0 : 0., lon1 : 0., lat1 : 0.}, $
                  INHERITS cvcDate, $
                  INHERITS cvcError $
             }
END
