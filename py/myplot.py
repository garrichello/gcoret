from osgeo import gdal
from osgeo import osr
#import gdal
#import osr
import sys
import numpy
import numpy.ma as ma
#import Numeric
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import math
import Image as img

def set_type(in_type, in_val):
    if in_type == 1: # byte
        val = [int(c) for c in in_val]
    elif in_type == 2: # integer
        val = [int(c) for c in in_val]
    elif in_type == 3: # long
        val = [long(c) for c in in_val]
    elif in_type == 4: # float
        val = [float(c) for c in in_val]
    elif in_type == 5: # double
        val = [float(c) for c in in_val]
    elif in_type == 7: # string
        decval = [chr(c) for c in in_val]
        val = ''.join(decval).replace('\x00', '')
    else:
        print 'Unsupported value type is given. Return unchanged.'
        val = in_val
    
    return val
    
def decode_keys(in_keys, in_types, in_lens, in_vals):
    dims = in_keys.shape
    nkeys = dims[0]
    keys = {}
    k = 0
    for i in range(nkeys):
        dec = [chr(c) for c in in_keys[i]]
        key = ''.join(dec).replace('\x00', '')
# value extraction
        val = []
        for j in range(in_lens[i]):
            val = val + [in_vals[k]]
            k = k + 1

        keys[key] = set_type(in_types[i], val)

    return keys

def write_geotiff(in_filename, in_data, in_keys, in_types, in_lens, in_vals, in_float):

#    in_data = Numeric.asarray(in_data)
# decode keys
    keys = decode_keys(in_keys, in_types, in_lens, in_vals)
    print keys

# prepare GeoTIFF driver
    
    fmt = 'GTiff'
    drv = gdal.GetDriverByName(fmt)
    meta = drv.GetMetadata()

# check if driver supports Create() method
    if meta.has_key(gdal.DCAP_CREATE) and meta[gdal.DCAP_CREATE] == 'YES':
        pass
    else:
        print "Driver %s does not support Create() method." % fmt
        sys.exit(1)

    if in_float:
        dt = gdal.GDT_Float32
#        print 'Writing float GeoTIFF'
    else:
	dt = gdal.GDT_Byte
#	print 'Writing byte GeoTIFF'
    
# write image    
    dims = in_data.shape
#    print dims
#    data = in_data #.reshape(3, 768, 1024)  #numpy.zeros((3, 768, 1024))
#    dims = data.shape
#    print dims
    if len(dims) == 3:
#        print 'RGB'
        nch = dims[0]
        xsize = dims[2]
        ysize = dims[1]
        outData = drv.Create(in_filename, xsize, ysize, nch, dt)
        for i in range(nch):
            outData.GetRasterBand(i+1).WriteArray(in_data[i, :, :])
    elif len(dims) == 2:
#        print 'Mono'
        nch = 1
        xsize = dims[1]
        ysize = dims[0]
        outData = drv.Create(in_filename, xsize, ysize, nch, dt)
        outData.GetRasterBand(1).WriteArray(in_data)        
    else:
        print "Data should be 2- or 3-dimensional. Aborting."
        return -1

    gtype = 'EPSG:'+str(keys['GEOGRAPHICTYPEGEOKEY'][0])
    cs = osr.GetWellKnownGeogCSAsWKT(gtype)
#    print cs
    outData.SetProjection(cs)
    gt = [0, 1, 0, 0, 0, 1] # default value
# pixel scale
    gt[1] = keys['MODELPIXELSCALETAG'][0]
    gt[5] = keys['MODELPIXELSCALETAG'][1]
# top left pixel position
    gt[0] = keys['MODELTIEPOINTTAG'][3]
    gt[3] = keys['MODELTIEPOINTTAG'][4]

#    print keys['MODELTIEPOINTTAG']

    outData.SetGeoTransform(gt)

    outData = None

    return 0

def write_tiff(in_filename, in_data, in_float):
    print 'python module myplot.py: write_tiff is not implemented yet!'
    return 0

def write_image(in_filename, in_filetype, in_data):
    print 'Writing image'
    dims = in_data.shape
    
    if len(dims) == 3:
        print 'RGB'
        nch = dims[0]
        xsize = dims[2]
        ysize = dims[1]
	im = img.new('RGB', (xsize, ysize))
	outdata = [tuple(in_data[:, i, j]) for i in range(dims[1]) for j in range(dims[2])]
    elif len(dims) == 2:
        print 'Mono'
        nch = 1
        xsize = dims[1]
        ysize = dims[0]
        im = img.new('L', (xsize, ysize))
        outdata = in_data.tolist()
    else:
        print "Data should be 2- or 3-dimensional. Aborting."
        return -1

    print dims
    im.putdata(outdata)
    im.save(in_filename, in_filetype)
    return 0

def contour(in_data, in_x, in_y, in_nlevels, min_val, max_val):
    
    global aLens
    global aLevs

    X, Y = numpy.meshgrid(in_x, in_y)
#    mdata = ma.masked_where(in_data == -999.0, in_data)

#    plt.figure()
    # levels = [0, 5, 10, 15, 20, 25, 30, 35, 40]
    # in_nlevels
    var = max_val - min_val
    if (var > 0):
	varpow = math.log10(var)
    else:
	varpow = 1
    nlevpow = math.log10(in_nlevels)
    roundpow = int(varpow - nlevpow) - 1
    levels = [round(float(a)/(in_nlevels-1)*var+min_val, -roundpow) for a in range(in_nlevels)]
    print 'Contour autolevels: ', levels
    cs = plt.contour(X, Y, in_data, levels)
#    print in_data
#    cs = plt.contour(X, Y, in_data, in_nlevels)
#    plt.show()
#    print sys.version
#    print cs
    lens = [] 
    paths = []
    levs = []
    levnum = 0

#    print cs.levels

    for cs1 in cs.collections:
#        print levnum
        for path1 in cs1.get_paths():
            verts = path1.vertices #[pp[0] for pp in path1.iter_segments()]
            lens = lens + [len(verts)]
            levs = levs + [cs.levels[levnum]]
            paths = paths + list(verts)
        levnum = levnum + 1
    
    aLens = numpy.asarray(lens, numpy.dtype(float))
    aPaths = numpy.asarray(paths, numpy.dtype(float))
    aLevs = numpy.asarray(levs, numpy.dtype(float))
#    print aPaths
    return aPaths

def get_contour_lens():
    global aLens

    return aLens

def get_contour_levs():
    global aLevs

    return aLevs
