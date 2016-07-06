;
;  NAME: 
;    structTask
;
;  PURPOSE:
;    Structure. Description of the task given.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------

PRO structTask__define
    
    structTask = { structTask, $ 
                   uid : '', $ ; Task's UID
                   owner : '', $ ; Owner's name
                   description : '', $ ' Task description
                   root_path : '', $ ; local root path (could be useful in future)
                   data_module_path : '', $ ; default path to data access modules, could be overridden by data's module_path attribute
                   metadb : {metadb, host : '', name : '', user : '', password : ''}, $ ; metadata base description
                   pDataDesc : ptr_new(), $ ; pointer to array of structDataDesc structures; defines input data arrays
                   pDestDesc : ptr_new(), $ ; pointer to array of structDestDesc strutures; defines results outputs
                   pProcDesc : ptr_new() $ ; pointer to array of structProcDesc structures; defines processing sequences
                 }
END