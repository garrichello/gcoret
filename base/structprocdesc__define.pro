;
;  NAME: 
;    structProcDesc
;
;  PURPOSE:
;    Structure. Contains processing sequence description.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;    <process name='Process1' class='cvcCalcTimeMean'>
;      <input name='Input1' data='Data1'/>
;      <input name='Input2' data='Data2'/>
;      <output name='Output1' data='Result1'/>
;      <output name='Output2' data='Result2'/>
;    </process>
;    
;--------------------------------------------------------------------
PRO structProcDesc__define

    MAX_N_INPUTS = 128
    MAX_N_OUTPUTS = 128
    MAX_N_WRITES = 128

    structProcDesc = { structProcDesc, $
                      uid : '', $ ; process UID
                      class : '', $ ; name of class for processing
                      nInputs : 0, $
                      asInputs : make_array(MAX_N_INPUTS, value={structProcParam}), $ ; input data, pointer to array of structProcInput structures
                      nOutputs : 0, $
                      asOutputs : make_array(MAX_N_OUTPUTS, value={structProcParam}) $ ; results outputs, pointer to array of structProcOutput structures 
                    }
END