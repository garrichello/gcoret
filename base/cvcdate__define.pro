;
;  NAME: 
;    cvcDate
;
;  PURPOSE:
;    Date/time processing 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcDate::Init
    
    return, 1
END
;--------------------------------------------------------------------
PRO cvcDate::Cleanup

END
;--------------------------------------------------------------------
PRO cvcDate::DayMonthYear, in_longdate, out_year, out_month, out_day, out_hour
  out_year = in_longdate / 1000000L
  out_month = (in_longdate - out_year * 1000000L) / 10000L
  out_day = ((in_longdate - out_year * 1000000L) - out_month * 10000L) / 100L
  out_hour = ((in_longdate - out_year * 1000000L) - out_month * 10000L) - out_day * 100L
END
;--------------------------------------------------------------------
FUNCTION cvcDate::LongDate, in_year, in_month, in_day, in_hour

    return, in_year*1000000L + in_month*10000L + in_day*100L + in_hour
END
;--------------------------------------------------------------------
PRO cvcDate::SetNextHour, io_year, io_month, io_day, io_hour, in_hoursToAdd
  nextJD = julday(io_month, io_day, io_year, io_hour+in_hoursToAdd)
  caldat, nextJD, io_month, io_day, io_year, io_hour
END
;--------------------------------------------------------------------
PRO cvcDate::IncHour, io_jd, in_inc
  if (n_elements(in_inc) eq 0) then in_inc = 1
  caldat, io_jd, month, day, year, hour
  io_jd = julday(month, day, year, hour+in_inc)
END
;--------------------------------------------------------------------
PRO cvcDate::SetNextDay, io_year, io_month, io_day, io_hour
  nextJD = julday(io_month, io_day, io_year, io_hour) + 1
  caldat, nextJD, io_month, io_day, io_year, io_hour
END
;--------------------------------------------------------------------
PRO cvcDate::IncDay, io_jd, in_inc
  if (n_elements(in_inc) eq 0) then in_inc = 1
  io_jd = io_jd + in_inc
END
;--------------------------------------------------------------------
PRO cvcDate::SetNextMonth, io_year, io_month
  io_month = io_month + 1
  if (io_month gt 12) then begin
    io_month = 1
    io_year = io_year + 1
  endif
END
;--------------------------------------------------------------------
PRO cvcDate::IncMonth, io_jd, in_inc
  if (n_elements(in_inc) eq 0) then in_inc = 1
  caldat, io_jd, month, day, year, hour
  month = month + 1
  if (month gt 12) then begin
    month = 1
    year = year + 1
  endif
  io_jd = julday(month, day, year, hour)
END
;--------------------------------------------------------------------
PRO cvcDate::SetNextYear, io_year
  io_year = io_year + 1
END
;--------------------------------------------------------------------
PRO cvcDate::IncYear, io_jd, in_inc
  if (n_elements(in_inc) eq 0) then in_inc = 1
  caldat, io_jd, month, day, year, hour
  io_jd = julday(month, day, year+in_inc, hour)
END
;--------------------------------------------------------------------
PRO cvcDate__define

    struct = { cvcDate, $
               nil : 0 $
             }
END