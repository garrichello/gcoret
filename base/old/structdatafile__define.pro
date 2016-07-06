;  NAME: 
;    structDataFile
;
;  PURPOSE:
;    Dataset's file
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
PRO structDataFile__define

    structDatavar = { structDataFile, $
               name : '', $ ; file name
               type : '', $ ; file type (GRIB1, GRIB2, NETCDF, HDF)
               level : '', $ ; vertical level name
               pInfo : ptr_new() $ ; pointer to some kind of metadata
             }
END
