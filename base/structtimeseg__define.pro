;
;  NAME: 
;    structTimeSeg
;
;  PURPOSE:
;    Structure. Time segment parameters.
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
pro structTimeSeg__define

    struct = {structTimeSeg, $
                name : '', $ ; segment's name
                beginning : 0L, $ ; beginning of time segments
                ending : 0L, $ ; ending of the time segment
                step : '' $ ; time step of data to extract: '6h', '12h', '24h', 'day', 'month'
    }
end