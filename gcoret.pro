pro gcoret, argv

    !PATH = '/opt/gcoret/base:' + '/opt/gcoret/extra:' + !PATH

; parameters
; passed from daemon
    if (n_elements(argv) ne 0) then begin
      print, 'Ran by daemon with arguments:', argv
    endif else begin
; passed from command line
      argv = command_line_args()
      print, 'Ran from command line with arguments:', argv      
    endelse

    oApp = OBJ_NEW( 'cvcMainApp', argv )

    if (obj_valid(oApp)) then result = oApp->Run() else result = -1

    if (obj_valid(oApp)) then obj_destroy, oApp

    if (result ne 1) then print, 'ERROR!' else print, 'SUCCESS!'

;    stop
end
