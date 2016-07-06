;
;  NAME: 
;    cvcCalcRGB
;
;  DESCRIPTION:
;    Create presudo-RGB image from LandSat channels
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
FUNCTION cvcCalcRGB::Init, in_sInputs, in_sOutputs, in_hLogFile

    return, self->cvcCalc::Init(in_sInputs, in_sOutputs, in_hLogFile)
END
;--------------------------------------------------------------------
PRO cvcCalcRGB::Cleanup

    self->cvcCalc::Cleanup
END
;--------------------------------------------------------------------
FUNCTION cvcCalcRGB::Run

    self->printLog, "(cvcCalcRGB::Run) Started..."
    
    ; get number of datasets
    if (self->Assert(self->GetInfo(OUT_NDATA=nData))) then return, retCode
    ; variable Id we want to process
    dataId = 0
    resultCode = self.oData->Get(dataId, sResponseR)
    if (self->Assert(resultCode)) then return, resultCode

    dataId = 1
    resultCode = self.oData->Get(dataId, sResponseG)
    if (self->Assert(resultCode)) then return, resultCode

    dataId = 2
    resultCode = self.oData->Get(dataId, sResponseB)
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
          
      rBand = sResponseR.aData
      gBand = sResponseG.aData
      bBand = sResponseB.aData
      mask = ((sResponseR.aData gt 0) or (sResponseG.aData gt 0) or (sResponseB.aData gt 0))
    
      res = self->BandNormalize(rBand)
      res = self->BandNormalize(gBand)
      res = self->BandNormalize(bBand)

      image24 = bytarr([4, size(sResponseR.aData, /dim)])
      
      image24[0, *, *] = rBand
      image24[1, *, *] = gBand
      image24[2, *, *] = bBand
      image24[3, *, *] = mask*255B
  
      ; push result
      res = self->__PushResult(DATA=image24, GRIDTYPE=sResponseR.gridType, EXTRA=sResponseR.meta)
      if (self->Assert(res)) then return, res

    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
PRO cvcCalcRGB__define

    struct = { cvcCalcRGB, $
               INHERITS cvcCalc $ 
             }
END
