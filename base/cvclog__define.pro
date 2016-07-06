;
;  NAME: 
;    cvcLog
;
;  PURPOSE:
;    Logger class 
;
;  INHERITS:
;    None
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcLog::Init

    return, 1
END
;--------------------------------------------------------------------
PRO cvcLog::printLog, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, in_m19, NoTimeStamp=kw_noTS, _EXTRA=ex

    dubOnScr = 1

; write log to standard output or a file
    if (~keyword_set(kw_noTS)) then begin
	printf, self.hLogFile, '['+systime()+'] ', format='(a, $)'
    endif
    ; this silly hack is the only way to process unknown number of parameters
    case n_params() of
	0: printf, self.hLogFile, _EXTRA=ex
	1: printf, self.hLogFile, in_m0, _EXTRA=ex
	2: printf, self.hLogFile, in_m0, in_m1, _EXTRA=ex
	3: printf, self.hLogFile, in_m0, in_m1, in_m2, _EXTRA=ex
	4: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, _EXTRA=ex
	5: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, _EXTRA=ex
	6: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, _EXTRA=ex
	7: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, _EXTRA=ex
	8: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, _EXTRA=ex
	9: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, _EXTRA=ex
	10: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, _EXTRA=ex
	11: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, _EXTRA=ex
	12: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, _EXTRA=ex
	13: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, _EXTRA=ex
	14: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, _EXTRA=ex
	15: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, _EXTRA=ex
	16: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, _EXTRA=ex
	17: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, _EXTRA=ex
	18: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, _EXTRA=ex
	19: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, _EXTRA=ex
	20: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, in_m19, _EXTRA=ex
	else: printf, self.hLogFile, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, in_m19, _EXTRA=ex
    endcase

; if log is sent to a file then duplicate it to standard output

    if ((self.hLogFile ne -1) and (dubOnScr eq 1)) then begin ; this is printed to the screen if we're logging into file
      if (~keyword_set(kw_noTS)) then begin
	print, '['+systime()+'] ', format='(a, $)'
      endif
    ; this silly hack is the only way to process unknown number of parameters
      case n_params() of
	0: print, _EXTRA=ex
	1: print, in_m0, _EXTRA=ex
	2: print, in_m0, in_m1, _EXTRA=ex
	3: print, in_m0, in_m1, in_m2, _EXTRA=ex
	4: print, in_m0, in_m1, in_m2, in_m3, _EXTRA=ex
	5: print, in_m0, in_m1, in_m2, in_m3, in_m4, _EXTRA=ex
	6: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, _EXTRA=ex
	7: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, _EXTRA=ex
	8: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, _EXTRA=ex
	9: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, _EXTRA=ex
	10: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, _EXTRA=ex
	11: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, _EXTRA=ex
	12: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, _EXTRA=ex
	13: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, _EXTRA=ex
	14: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, _EXTRA=ex
	15: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, _EXTRA=ex
	16: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, _EXTRA=ex
	17: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, _EXTRA=ex
	18: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, _EXTRA=ex
	19: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, _EXTRA=ex
	20: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, in_m19, _EXTRA=ex
	else: print, in_m0, in_m1, in_m2, in_m3, in_m4, in_m5, in_m6, in_m7, in_m8, in_m9, in_m10, in_m11, in_m12, in_m13, in_m14, in_m15, in_m16, in_m17, in_m18, in_m19, _EXTRA=ex
      endcase
    endif

END
;--------------------------------------------------------------------
FUNCTION cvcLog::openLog, in_fileName

    iostatus = 0
    if (in_fileName ne '') then openw, hLogFile, in_fileName, error=iostatus, /get_lun else hLogFile = -1
    self.hLogFile = hLogFile

    if (iostatus ne 0) then begin
	self.hLogFile = -2
	self->printLog, !ERROR_STATE.MSG
	return, -1
    endif else self->printLog, '--- Log started ---'

;    self.logFileName = in_fileName
    return, 0        
END
;--------------------------------------------------------------------
PRO cvcLog::closeLog

    self->printLog, '--- Log finished ---'
    
    if (self.hLogFile ne -1) then free_lun, self.hLogFile

END
;--------------------------------------------------------------------
PRO cvcLog__define
    
      struct = { cvcLog, $
                 hLogFile : 0 $
;               logFileName : '' $
              }
END
