;
;  NAME: 
;    cvcCalcNDVI
;
;  DESCRIPTION:
;    LandSat NDVI image composition
;
;  PURPOSE:
;    Implements creation of RGB image from 3 satellite bands
;
;  INHERITS:
;    cvcCalc
;
;  CALLING SEQUENCE:
;  1) create instance: 
;     oCalcTimeMean = obj_new('cvcCalcTimeMean', sIniData)
;       where sIniData is a filled structure of type 'structIniData'
;  2) call function 'Run':
;     result = oCalcTimeMean->Run()
;
;--------------------------------------------------------------------
FUNCTION cvcCalcNDVI::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcNDVI::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcNDVI::Run

    self->printLog, "(cvcCalcNDVI::Run) Started..."
    
    ; get number of datasets
    if (self->Assert(self->GetInfo(OUT_NDATA=nData))) then return, retCode
    ; variable Id we want to process
    dataId = 0
    resultCode = self.oData->Get(dataId, sResponseNIR)
    if (self->Assert(resultCode)) then return, resultCode

    dataId = 1
    resultCode = self.oData->Get(dataId, sResponseR)
    if (self->Assert(resultCode)) then return, resultCode

;     sResponse structure contains:
;       aData - data array
;       aLons - longitudes array
;       aLats - latitudes array
;       aTimes - times (JD) array
;       minVal - minimum data value
;       maxVal - maximum data value
;       missingVal - missing data value
;       sTimeRng - date-time range of data returned
;       aLev - array of levels
;       nLev - number of levels
;       meta - geotiff metadata
;       resultCode - result code
    
;      mask = ((sResponseR.aData gt 0) or (sResponseG.aData gt 0) or (sResponseB.aData gt 0))
      
      rBand = float(sResponseR.aData)
      nirBand = float(sResponseNIR.aData)

;      res = self->BandNormalize(rBand, LIMIT=0.001)
;      res = self->BandNormalize(nirBand, LIMIT=0.001)
    
      ndvi = (nirBand - rBand) / (nirBand + rBand)
      badidx = where((nirBand - rBand) eq 0.0) 
      ndvi[badidx] = -999.0
;      negval = where(ndvi lt 0)
;      if (negval[0] ne -1) then ndvi[negval] = -999.0
      
      ; push result
      res = self->__PushResult(DATA=ndvi, GRIDTYPE=sResponseR.gridType, MISSING=-999.0, EXTRA=sResponseR.meta)
      if (self->Assert(res)) then return, res

    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcNDVI__define

    struct = { cvcCalcNDVI, $
               INHERITS cvcCalc $ 
             }
END
