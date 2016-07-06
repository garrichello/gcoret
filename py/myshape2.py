import shapelib
import dbflib
import os
import numpy as np

def openshape(in_filename, in_enttype, in_mode):
    global filename
    global enttype
    global hFile
    global info
    global mode

    filename = in_filename
    enttype = in_enttype
    mode = in_mode

    if mode:
        print 'Shape writer mode'
        hShape = shapelib.Create(in_filename, in_enttype)
    else:
        print 'Shape reader mode'
        name, ext = os.path.splitext(in_filename)
        if ext == '.shp':
            hFile = shapelib.open(in_filename, "rb")
            info = hFile.info()
        elif ext == '.dbf':
            hFile = dbflib.open(in_filename, "rb")
        else:
            print '(2) Unknown file extension: '+ext
            return -1
    
    nEnts = info[0]
    
    print 'File is opened successfully'
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

def getnents():
    global hFile
    global info
    
    nEnts = info[0]

    return nEnts

def readent(in_index):
    global hFile
    global shpType
    global shpId
    global shpVerts
    global shpBounds
    global info
    
    nEnts = info[0]
    
    if in_index > nEnts-1:
	return -1
    
    if in_index > -1:
	data = hFile.read_object(in_index)
	shpType = data.type
	shpId = data.id
	shpVerts = data.vertices()
	shpBounds = data.extents()
    else:
	shpType = 0

    return shpType

def getverts():
    global hFile
    global shpVerts
    
    return np.asarray(shpVerts)

def getbounds():
    global hFile
    global shpVerts
    
    return np.asarray(shpBounds)

def resetrec():
    global fieldval

    fieldval = []

    return 0

def addvalue(in_value):
    global fieldval

    fieldval = fieldval + [in_value]
    
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
