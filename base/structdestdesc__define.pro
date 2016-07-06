;
;  NAME: 
;    structDestDesc
;
;  PURPOSE:
;    Desctination description. 
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;    <destination name='Image1' type='image'> <!-- type: 'image', 'raw' -->
;    <projection name='cylindrical'>
;      <limits units='degrees'>
;        <longitude role='left'>0</longitude>
;        <longitude role='right'>180</longitude>
;        <latitude role='top'>88</latitude>
;        <latitude role='bottom'>0</latitude>
;      </limits>
;    </projection>
;    <coastline visible='yes' hires='no'/>
;    <graphics title='Image title' kind='shaded'> <!-- kind: 'shaded', 'contour', 'station', 'line', 'multiline' -->
;      <width units='pixels'>1024</width>
;      <height units='pixels'>768</height>
;      <axes> 
;        <x_axis name='Longitude' units='Degrees' scale='linear' visible='yes'/> <!-- scale: 'linear', 'log10' -->
;        <y_axis name='Latitude' units='Degrees' scale='linear' visible='yes'/>
;        <z_axis name='Height' units='m' scale='linear' visible='no'/>
;      </axes>
;      <lines> <!-- for graphics kind=='line' or 'multiline' -->
;        <line name='Line1' style='solid' color='blue' visible='yes'/> <!-- style: 'solid', 'dotted', 'dashed' -->
;        <line name='Line2' style='solid' color='red' visible='yes'/>
;        <line name='Line3' style='solid' color='green' visible='yes'/>
;      </lines>
;      <legend title='degrees' kind='embedded' position='right'> <!-- kind: 'embedded', 'file'; position: 'right', 'left', 'top', 'bottom' -->
;        <units>K</units> <!-- added after legend's title -->
;        <limited>no</limited>
;        <minimum>250</minimum> <!-- ignored if limited == no -->
;        <maximum>310</maximum> <!-- ignored if limited == no -->
;        <file name='output-legend.eps' type='eps'/> <!-- file for legend (if legend kind=='file'), type: 'eps', 'png', 'jpg', 'gif' -->
;      </legend>
;      <colortable>RAINBOW</colorTable>  <!-- only for graphics kind=='shaded' -->
;      <blackbg>no</blackbg> <!-- black background -->
;      <smoothing>yes</smoothing> <!-- only for graphics kind=='shaded' -->
;      <steps>30</steps> <!-- only for graphics kind == 'contour' or for file type == 'shape' -->
;    </graphics>
;    <file name='output' type='eps'/> <!-- type for graphics kind=='shaded'/'contour'/'station': 'eps', 'geotiff', 'shape'; for kind=='line'/'multiline': 'png' -->
;    </destination>
;
;--------------------------------------------------------------------

PRO structDestDesc__define

    MAX_N_PLOT_LINES = 32

    structDestDesc = { structDestDesc, $
               uid : '', $ ; destination's name
               type : '', $ ; type of destination: 'image', 'raw'
               projection : { projection, $ ; projection properties
                 name : '', $ ; projection's name: 'cylindrical',... etc.
                 p0lon : 0.0, $ ; projection's central longitude
                 p0lat : 0.0, $ ; projection's central latitude
                 rot : 0.0, $ ; projection's rotation
                 limits : { limits, $ ; limits of the map
                    units : '', $
                    left : 0.0, $
                    right : 0.0, $
                    top: 0.0, $
                    bottom : 0.0 $
                 } $
               }, $
               coastline : { coastline, $ ; coastline properties
                  visible : '', $ ; visibility: yes/no
                  hires : '' $ ; high resolution: yes/no
               }, $
               rivers : { rivers, $ ; rivers properties
                  visible : '', $ ; visibility: yes/no
                  hires : '' $ ; high resolution: yes/no
               }, $
               countries : { countries, $ ; countries properties
                  visible : '', $ ; visibility: yes/no
                  hires : '' $ ; high resolution: yes/no
               }, $
               graphics : { graphics, $ ; graphics properties
                 title : '', $ ; graphics title
                 kind : '', $ ; graphics kind: 'shaded', 'contour'                 
                 width : 0, $ ; graphics width
                 width_units : '', $ ; width units
                 height : 0, $ ; graphics height
                 height_units : '', $ ; height units
                 axis : { axis, $ ; axes properties
                    x : { x_axis, $ ; x axis properties
                        name : '', $ ; axis name
                        units : '', $ ; axis units
                        scale : '', $ ; axis scale: 'linear', 'log10'
                        minVal : 0.0, $ ; minimum value
                        maxVal : 0.0, $ ; maximum value
                        visible : '' $ ; visibility of axis: yes/no
                    }, $
                    y : { x_axis, $ ; x axis properties
                        name : '', $ ; axis name
                        units : '', $ ; axis units
                        scale : '', $ ; axis scale: 'linear', 'log10'
                        minVal : 0.0, $ ; minimum value
                        maxVal : 0.0, $ ; maximum value
                        visible : '' $ ; visibility of axis: yes/no
                    }, $
                    z : { x_axis, $ ; x axis properties
                        name : '', $ ; axis name
                        units : '', $ ; axis units
                        scale : '', $ ; axis scale: 'linear', 'log10'
                        minVal : 0.0, $ ; minimum value
                        maxVal : 0.0, $ ; maximum value
                        visible : '' $ ; visibility of axis: yes/no
                    } $
                 }, $
                 nLines : 0, $ ; number of plot lines
                 autoline : '', $ ; is autoline mode is on: yes/no
                 asLines : make_array(MAX_N_PLOT_LINES, value={structPlotLine}), $ ; plot lines description
                 legend : { legend, $ ; legend properties
                    title : '', $ ; legend title
                    kind : '', $ ; legend kind: 'embedded', 'file'
                    type : '', $ ; legend type: 'continous', 'discrete'
                    position : '', $ ; legend position: 'right', 'left', 'top', 'bottom' (only for 'embedded' kind)
                    units : '', $ ; units of values plotted, added to the end of legend title
                    ncolors : 0, $ ; number of discrete colors in legend
                    nlabels : 0, $ ; number of labels in legend
                    limited : '', $ ; apply limits to the legend's range: 'no', 'yes'
                    minimum : 0.0, $ ; minimum value on the legend
                    maximum : 0.0, $ ; maximum value on the legend
                    file : { file, $ ; legend file properties (for legend kind=='file')
                       name : '', $ ; name of file for legend
                       type : '' $ ; type of file to output: 'eps', 'png', 'jpg', 'gif'
                    } $
                 }, $
                 colortable : '', $ ; color table to use for 'shaded' kind
                 colorscale : '', $ ; color scaling: linear/log
                 blackbg : '', $ ; black background of the plot
                 smoothing : '', $ ; apply smoothing to the graphics: 'yes', 'no' (for 'shaded' kind)
                 steps : 0 $ ; number of contour steps (for 'contour' kind and 'shape' file type)
               }, $
               file : { file, $ ; file properties
                 name : '', $ ; name of file to output
                 type : '' $ ; type of file to output: 'eps', 'geotiff', 'shape' - for output type 'image'; 'netcdf', 'geotiff' - for output type 'raw'
               } $
             }
END