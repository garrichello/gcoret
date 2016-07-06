;
;  NAME: 
;    structPlotLine
;
;  PURPOSE:
;    Structure. Contains description of a plot line.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
PRO structPlotLine__define

    structPlotLine = { structPlotLine, $
                    name : '', $ ; name of line
                    style : '', $ ; style: 'solid', 'dotted', 'dashed'
                    color : '', $ ; standard color name
                    symbol : '', $ ; symbol name: 'plus', 'asterisk', 'period', 'diamond', 'triangle', 'square', 'X'
                    thick : 0.0, $ ; thickness
                    visible : '' $ ; visibility: yes/no
                }

END