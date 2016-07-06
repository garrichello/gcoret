;
;  NAME: 
;    structDataMeta
;
;  PURPOSE:
;    Data metadata structure.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;;
;--------------------------------------------------------------------

PRO structDataMeta__define

    MAX_LEV_GROUPS = 16

     structDataMeta = { structDataMeta, $
               levgrp : make_array(MAX_LEV_GROUPS, value = { levs, $ ; data levels arrays
                 name : '', $ ; levels array name
                 values : '' $ ; levels values delimited by semicolon
               }), $
               nlevgrps : 0, $
               vars : { vars, $ ; data variables
                 longitude : '', $ ; longitude variable name
                 latitude : '', $ ; latitude variable name
                 level : '', $ ; level variable name
                 time : '' $ ; time variable name
               }, $
               dims : { dims, $ ; data dimensions
                 longitude : '', $ ; longitude dimension name
                 latitude : '', $ ; latitude dimension name
                 level : '', $ ; level dimension name
                 time : '' $ ; time dimension name
               }, $
               timestep : '', $ ; time step in a data file
               filespan : '' $ ; time span of one data file
             }
END