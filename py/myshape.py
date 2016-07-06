import shapefile
import os
import numpy as np

def openshape(in_filename, in_enttype, in_mode):
    global filename
    global enttype
    global w
    global r
    global mode

    filename = in_filename
    enttype = in_enttype
    mode = in_mode

    if mode:
        print 'Shape writer mode'
        w = shapefile.Writer(enttype)
        w.autoBalance = 1
#        print 'w = shapefile.Writer('+str(enttype)+')'
#        print 'w.autoBalance = 1'
    else:
        print 'Shape reader mode'
        name, ext = os.path.splitext(in_filename)
        print in_filename
        if ext == '.shp':
            r = shapefile.Reader(in_filename)
        elif ext == '.dbf':
            mydbf = open(in_filename)
            r = shapefile.Reader(dbf=mydbf)
        else:
            print 'Unknown file extension: '+ext
            return -1

    return 0

def addfield(in_name, in_type, in_size, in_decimal):
    global w

    w.field(in_name, in_type, in_size, in_decimal)
#    print "w.field('"+in_name+"', '"+str(in_type)+"', "+str(in_size)+", "+str(in_decimal)+")"

    return 0

def writeent(in_type, in_parts):
    global w
    
    if in_type == 1:
        w.point(in_parts[0], in_parts[1])
#	print 'w.point('+str(in_parts[0])+', '+str(in_parts[1])+')'
    elif in_type == 3:
        w.line(parts=[in_parts.tolist()])
#        print 'w.line(parts=['+str(in_parts.tolist())+'])'
    elif in_type == 5:
        w.poly(parts=[in_parts.tolist()], shapeType=in_type)
    else:
        print 'Unknown entity type!'
        return -1

    return 0

def readent(in_index):

    return 0

def resetrec():
    global fieldval

    fieldval = []

    return 0

def addvalue(in_value):
    global fieldval

    fieldval = fieldval + [in_value]

#    print "in_value=", in_value
    
    return 0

def getvalue(in_idx):
    global fieldval

    value = fieldval[in_idx]
    if type(value) is list:
        value = long(''.join(['%02d'%i for i in value]))
#    print value

    return value

def getvalue1(in_nrec, in_natt):
    global allfields

    value = allfields[in_nrec][in_natt]
    if type(value) is list:
        value = long(''.join(['%02d'%i for i in value]))
#    print value

    return value

def writerec():
    global w
    global fieldval
    global filename

#    print 'fieldval = '+str(fieldval)
    w.record(*fieldval)
#    print 'w.record(*fieldval)'
#    w.save(filename)
#    print "w.save('"+filename+"')"

    return 0

def readrec(i):
    global r
    global fieldval

    fieldval = r.record(i)

    return len(fieldval)

def getreccnt():
    global r
       
    recs = r.records()

    return len(recs)

def readallrecs():
    global r
    global allfields

    allfields = r.records()

    return len(allfields)

def writeshape():
    global filename
    global w

#    print 'Write shape'
    w.save(filename)
    w = None

    return 0
