;
;  NAME: 
;    cvcError
;
;  PURPOSE:
;    Error handler class 
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcError::Init, in_hLogFile

    self.hLogFile = in_hLogFile
    
    self.ERROR_BADERR = -1
    self.ERROR_OK = 1
    self.ERROR_BADXML = -1001
    self.ERROR_NOTASK = -1010
    self.ERROR_NODATADESC = -1011
    self.ERROR_NOROI = -1012
    self.ERROR_NOROIPOINTS = -1013
    self.ERROR_NOLEVELS = -1014
    self.ERROR_LEVNUM = -1015
    self.ERROR_NOTIMESEGS = -1016
    self.ERROR_TIMESEGNUM = -1017
    self.ERROR_NODESTDESC = -1018
    self.ERROR_NOPROJ = -1019
    self.ERROR_NOLIMITS = -1020
    self.ERROR_LIMITROLE = -1024
    self.ERROR_NOGRAPH = -1025
    self.ERROR_NOAXES = -1025
    self.ERROR_NOLINES = -1026
    self.ERROR_BADLINESNUM = -1027
    self.ERROR_NOLEGEND = -1028
    self.ERROR_NOPROCDESC = -1029
    self.ERROR_BADINPUTSNUM = -1030
    self.ERROR_BADOUTPUTSNUM = -1031
    self.ERROR_WRONGLEV = -1032
    self.ERROR_PARAMSNUM = -1033
    self.ERROR_WRONGTIME = -1034
    self.ERROR_WRONGX = -1035
    self.ERROR_WRONGY = -1036
    
    
    self.ERROR_NODS = -10
    self.ERROR_NOVAR = -11
    self.ERROR_NOTIME = -12
    self.ERROR_NOLEVEL = -13
    self.ERROR_VARNUM = -14
    self.ERROR_TIMENUM = -17
    self.ERROR_BADLEV = -21
    self.ERROR_BADWTC = -22
    self.ERROR_BADLON = -23
    self.ERROR_BADLAT = -24
    self.ERROR_BADTIMESEG = -26
    self.ERROR_BADVISPROP = -27
    self.ERROR_BADPLOTLIMS = -28
    self.ERROR_BADLEGEND = -29
    self.ERROR_BADREQ = -31
    self.ERROR_FILENUM = -32
    self.ERROR_NODATAFILE = -33
    self.ERROR_BADCONST = -41
    self.ERROR_EOT = -42
    self.ERROR_BADTIMEREQ = -43
    self.ERROR_UNKFILETYPE = -44
    self.ERROR_UNKDSETTYPE = -45
    self.ERROR_BADMODULENAME = -100
    self.ERROR_ERRORINMODULE = -101
    self.ERROR_BADDSFILE = -51
    self.ERROR_NOVARNAME = -52
    self.ERROR_NOVARDS = -53
    self.ERROR_NOVARDSNAME = -54
    self.ERROR_NOVARROI = -55
    self.ERROR_NODATAREAD = -60
    self.ERROR_BADDSNAME = -61
    self.ERROR_BADDATAID = -70
    self.ERROR_NOMOREIDS = -71
    return, 1
END
;--------------------------------------------------------------------
FUNCTION cvcError::SayError, errorcode, info
    if (n_elements(info) eq 0) then info = ''
    if (n_elements(errorcode) eq 0) then errorcode = self.ERROR_BADERR 
    case errorcode of
        self.ERROR_BADXML: errmsg = "(cvcMainApp::ReadTask) Error: Error occured during the XML-parse operation"       
        self.ERROR_NOTASK: errmsg = "(cvcMainApp::ReadTask) Error: No task is given in input file" 
        self.ERROR_NODATADESC: errmsg = "(cvcMainApp::ReadDataDesc) Error: No data section in input task" 
        self.ERROR_NOROI:  errmsg = "(cvcMainApp::ReadDataDesc) Error: No region section in data in input task"
        self.ERROR_NOROIPOINTS: errmsg = "(cvcMainApp::ReadDataDesc) Error: No ROI points are given in input task" 
        self.ERROR_NOLEVELS: errmsg = "(cvcMainApp::ReadDataDesc) Error: No level section in data in input task" 
        self.ERROR_LEVNUM: errmsg = "('cvcMainApp::ReadDataDesc') Error: Bad number of levels in input task"
        self.ERROR_NOTIMESEGS: errmsg = "(cvcMainApp::ReadDataDesc) Error: No time section in data in input task" 
        self.ERROR_TIMESEGNUM: errmsg = "('cvcMainApp::ReadDataDesc') Error: Bad number of time segments in input task"+info
        self.ERROR_PARAMSNUM: errmsg = "('cvcMainApp::ReadDataDesc') Error: Bad number of parameters in input task"+info
        self.ERROR_NODESTDESC: errmsg = "(cvcMainApp::ReadDestDesc) Error: No destination section in input task"
        self.ERROR_NOPROJ: errmsg = "(cvcMainApp::ReadDestDesc) Error: No projection is given for destination in input task"
        self.ERROR_NOLIMITS: errmsg = "(cvcMainApp::ReadDestDesc) Error: No limits section in destination in input task"
        self.ERROR_LIMITROLE: errmsg = "('cvcMainApp::ReadDestDesc') Error: Bad limit role in limits for destination in input task"
        self.ERROR_NOGRAPH: errmsg = "(cvcMainApp::ReadDestDesc) Error: No graphics section in destination in input task"
        self.ERROR_NOAXES: errmsg = "(cvcMainApp::ReadDestDesc) Error: No axes section in graphics in destination in input task"
        self.ERROR_NOLINES: errmsg = "(cvcMainApp::ReadDestDesc) Error: No lines section in graphics in destination in input task"
        self.ERROR_BADLINESNUM: errmsg = "(cvcMainApp::ReadDestDesc) Error: Bad number of lines in graphics in destination in input task"
        self.ERROR_NOLEGEND: errmsg = "(cvcMainApp::ReadDestDesc) Error: No legend section in graphics in destination in input task"
        self.ERROR_NOPROCDESC: errmsg = "(cvcMainApp::ReadProcDesc) Error: No processing section in input task"
        self.ERROR_BADINPUTSNUM: errmsg = "(cvcMainApp::ReadProcDesc) Error: Too much inputs in processing in input task"
        self.ERROR_BADOUTPUTSNUM: errmsg = "(cvcMainApp::ReadProcDesc) Error: Too much outputs in processing in input task"
        self.ERROR_WRONGLEV: errmsg = "(cvcData::Read*) Error: wrong level requested (level is not prepared)"
        self.ERROR_WRONGTIME: errmsg = "(cvcData::Read*) Warning: wrong time range requested, no data is read"
        self.ERROR_WRONGX: errmsg = "(cvcData::Read*) Warning: wrong X range requested, no data is read"
        self.ERROR_WRONGY: errmsg = "(cvcData::Read*) Warning: wrong Y range requested, no data is read"

        self.ERROR_VARNUM: errmsg = "(cvcMainApp::ReadIni) Error: bad number of variables in ini-file" 
        self.ERROR_BADLEV: errmsg = "(cvcMainApp::ReadIni) Error: bad level for variable: " + info
        self.ERROR_BADLON: errmsg = "(cvcMainApp::ReadIni) Error: bad longitude for variable: " + info
        self.ERROR_BADLAT: errmsg = "(cvcMainApp::ReadIni) Error: bad latitude for variable: " + info
        self.ERROR_BADTIMESEG: errmsg = "(cvcMainApp::ReadIni) Error: bad time segment for variable: " + info
        self.ERROR_BADWTC: errmsg = "(cvcMainApp::ReadIni) Error: bad what to calc for variable: " + info
        self.ERROR_BADVISPROP: errmsg = "(cvcMainApp::ReadIni) Error: bad visualization properties in input.xml"
        self.ERROR_BADPLOTLIMS: errmsg = "(cvcMainApp::ReadIni) Error: bad plot limits in visualization properties in input.xml"
        self.ERROR_BADLEGEND: errmsg = "(cvcMainApp::ReadIni) Error: bad legend in visualization properties in input.xml"
        self.ERROR_BADMODULENAME: errmsg = "(cvcMainApp::Process) Error: Impossible to create an instance of module "+info+". "+!ERROR_STATE.MSG
        self.ERROR_ERRORINMODULE: errmsg = "(cvcManApp::Process) Error: Unexpected error occured in module "+info+". "+!ERROR_STATE.MSG
        self.ERROR_BADDSFILE: errmsg = "(cvcMainApp::ReadIni) Error: Datasets description file does not exists"
        self.ERROR_NOVARNAME: errmsg = "(cvcMainApp::ReadIni) Error: Variable name attribute is not specified"
        self.ERROR_NOVARDS: errmsg = "(cvcMainApp::ReadIni) Error: Dataset is not specified for variable "+info
        self.ERROR_NOVARDSNAME: errmsg = "(cvcMainApp::ReadIni) Error: Dataset name is not specified for variable "+info

        self.ERROR_BADERR: errmsg = "Error: Bad error code specified"
        self.ERROR_NODS: errmsg = "(cvcData::SetRegion) Error: no such dataset" 
        self.ERROR_NOVAR: errmsg = "(cvcData::SetRegion) Error: no such variable"
        self.ERROR_NOTIME: errmsg = "(cvcData::SetRegion) Error: no such time step"
        self.ERROR_NOLEVEL: errmsg = "(cvcData::SetDataFileTemplate) Error: no such level in dataset: "+info
        self.ERROR_BADREQ: errmsg = "("+info+") Error: bad request when reading data file"
        self.ERROR_FILENUM: errmsg  = "(cvcData::SetRegion) Error: too much files to open requested. Increase MAX_DATA_FILES"
        self.ERROR_BADCONST: errmsg = "("+info+") Error: bad constant requested"
        self.ERROR_NODATAFILE: errmsg = "(cvcData::SetRegion) Error: no data file "+info
        self.ERROR_EOT: errmsg = "(cvcData::GetNextArray) Warning: end of time grid. Reset time counter if necessary"
        self.ERROR_BADTIMEREQ: errmsg = "(cvcData::ReadGRIB1) Error: time requested is out of specified time segment: "+info
        self.ERROR_UNKFILETYPE: errmsg = "(cvcData::GetNextArray) Error: Unknown file type is requested for reading: "+info
        self.ERROR_UNKDSETTYPE: errmsg = "(cvcData::Prepare) Error: Unknown dataset type is specified in datasets.xml: "+info        
        self.ERROR_NODATAREAD: errmsg = "(cvcData::Read*) Error: No data was read"
        self.ERROR_BADDSNAME: errmsg = "(cvcDataAccess::Open) Error: Bad dataset name specified"
        self.ERROR_BADDATAID: errmsg = "(cvcDataAccess::Get) Error: Bad dataset ID specified"
        self.ERROR_NOMOREIDS: errmsg = "(cvcDataAccess::Open) Error: No more free data IDs!"
        self.ERROR_OK: errmsg = ""
        else: errmsg = ""
    endcase
    
    if (errorcode ne self.ERROR_OK) then self->printLog, errmsg
    return, errorcode
END
;--------------------------------------------------------------------
FUNCTION cvcError::Assert, result
    return, result ne self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcError::SetErrMsg, in_errMsg
    self.errMsg = in_errMsg
    return, -1
END
;--------------------------------------------------------------------
PRO cvcError__define
    
    struct = { cvcError, $
	       INHERITS cvcLog, $
               errMsg : '', $
               ERROR_OK : 0, $
               ERROR_BADXML: 0, $
               ERROR_NOTASK: 0, $
               ERROR_NODATADESC : 0, $
               ERROR_NOROI : 0, $
               ERROR_NOROIPOINTS : 0, $
               ERROR_NOLEVELS : 0, $
               ERROR_LEVNUM : 0, $
               ERROR_NOTIMESEGS : 0, $
               ERROR_TIMESEGNUM : 0, $
               ERROR_NODESTDESC : 0, $
               ERROR_NOPROJ : 0, $
               ERROR_NOLIMITS : 0, $
               ERROR_LIMITROLE : 0, $
               ERROR_NOGRAPH : 0, $
               ERROR_NOAXES : 0, $
               ERROR_NOLINES : 0, $
               ERROR_BADLINESNUM : 0, $
               ERROR_NOLEGEND : 0, $
               ERROR_NOPROCDESC : 0, $
               ERROR_BADINPUTSNUM : 0, $
               ERROR_BADOUTPUTSNUM : 0, $
               ERROR_WRONGLEV : 0, $
               ERROR_WRONGTIME : 0, $
               ERROR_WRONGX : 0, $
               ERROR_WRONGY : 0, $
               ERROR_PARAMSNUM : 0, $
                
               ERROR_BADERR : 0, $
               ERROR_NODS : 0, $
               ERROR_NOVAR : 0, $
               ERROR_NOTIME : 0, $
               ERROR_NOLEVEL : 0, $
               ERROR_VARNUM : 0, $
               ERROR_TIMENUM : 0, $
               ERROR_BADLEV : 0, $
               ERROR_BADLON : 0, $
               ERROR_BADLAT : 0, $
               ERROR_BADTIMESEG : 0, $
               ERROR_BADWTC : 0, $
               ERROR_BADVISPROP : 0, $
               ERROR_BADPLOTLIMS : 0, $
               ERROR_BADLEGEND : 0, $
               ERROR_BADREQ : 0, $
               ERROR_FILENUM : 0, $
               ERROR_BADCONST : 0, $
               ERROR_NODATAFILE : 0, $
               ERROR_EOT : 0, $
               ERROR_BADTIMEREQ : 0, $
               ERROR_UNKFILETYPE : 0, $
               ERROR_BADMODULENAME : 0, $
               ERROR_ERRORINMODULE : 0, $
               ERROR_UNKDSETTYPE : 0, $
               ERROR_BADDSFILE : 0, $
               ERROR_NOVARNAME : 0, $
               ERROR_NOVARDS : 0, $
               ERROR_NOVARDSNAME : 0, $
               ERROR_NOVARROI : 0, $
               ERROR_NODATAREAD : 0, $
               ERROR_BADDSNAME : 0, $
               ERROR_BADDATAID : 0, $
               ERROR_NOMOREIDS : 0 $
              }
              
END
