;
;  NAME: 
;    structDataDesc
;
;  PURPOSE:
;    Input data description structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;    <data name='Data1' type='dataset'> <!-- type: dataset, array -->
;    <dataset name='NCEP1'/>  <!-- ERAINT -->
;    <variable name='TMP'/> <!-- TMP, air, 2T, TAV, precip-->
;    <region units='degrees'>
;      <point name='west-north' lon=60 lat=70/>  
;      <point name='east-north' lon=150 lat=70/>
;      <point name='east-south' lon=150 lat=50/>
;      <point name='west-south' lon=60 lat=50/>
;    </region>
;    <levels values='2m'/>
;    <time template='YYYYMMDDHH'>
;      <segment name='Seg1' start=1951010100 finish=1951013100 step='12h'/>
;    </time>
;    </data>
;
;--------------------------------------------------------------------

PRO structDataDesc__define

    MAX_N_TIME_SEGMENTS = 128
    MAX_N_LEVELS = 128
    MAX_N_PARAMS = 128

    structDataDesc = { structDataDesc, $
               uid : '', $ ; data's uid
               type : '', $ ; data's type: dataset
               kind : '', $ ; data's kind: field/stations/points
               description : { description, $ ; description
                 title : '', $ ; long description of dataset or processing
                 name : '', $ ; human-readable variable name
                 units : '' $ ; units of variable
               }, $
               dataset : {  $
	        name : '', $ ; dataset's name: 'NCEP1', 'NCEP2', 'ERA40', 'JRA25'
	        scenario : '', $ ; model scenario, only for modeling results
            	res : '', $ ; dataset's horizontal resolution
            	tstep : '', $ ; dataset's time step
            	filespan : '', $ ; dataset's file time span
            	path : '', $ ; absolute path to dataset
            	fileTemplate : '' $ ; template for generating dataset's filenames
               }, $
               module_path : '', $ ; path to a module for dataset reading, overrides task's data_module_path attribute
               variableName : '', $ ; standard dataset's variable name
               tempk2c : '', $ ; convert temperature variable from K to C
               sRegion : { structROI }, $  ; region of interest
               numLevels : 0, $ ; number of vertical levels
               aLevel : make_array(MAX_N_LEVELS, value=''), $ ; array of vertical levels
               levVarName : '', $ ; name of netCDF variable containing vertical levels
               timeTemplate : '', $ ; time template
               numTimeSeg : 0, $  ; number of time segments
               asTimeSeg : make_array(MAX_N_TIME_SEGMENTS, value={structTimeSeg}), $   ; array of time segments
               pXgrid : ptr_new(), $ ; pointer to longitude grid
               pYgrid : ptr_new(), $ ; pointer to latitude grid
               pZgrid : ptr_new(), $ ; pointer to time grid
               pTimeGrid : ptr_new(),  $ ; pointer to level grid
               missingVal : 0.0, $ ; missing value
               gridType : '', $ ; grid type: 'regular', 'irregular', 'station'
               data : ptr_new(), $ ; pointer to data, should be NULL for a dataset and not NULL for a memory array
	       modify : { $
		scale : 0.0, $ ; scale factor for data
	        offset : 0.0 $ ; offset factor for data
	       }, $
               numParams : 0, $ ; number of parameters
               asParam : make_array(MAX_N_PARAMS, value={structArgDesc}), $ ; array of parameters
               extra : ptr_new() $ ; extra data
             }
END
