ncl_filedump 19570901.nc | awk '/float/ {isvar=0; if (($2 != "lon") && ($2 != "lat")) {print "INSERT INTO datadesc.variables (name, units, longname) VALUES (\"", $2, "\","; isvar=1}}; /long_name/ {if (isvar) {print "\"",$3,$4,$5,$6,$7,$8,$9,$10, "\");"}}; / units/ {if (isvar==1) {print "\"",$3,$4,$5,$6,$7,$8,$9,$10,"\","}}'

