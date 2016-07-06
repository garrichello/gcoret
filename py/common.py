import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.mlab as mlab
import numpy as np
from scipy import interpolate
from numpy.random import uniform, seed
import scipy.spatial as sp

# determine if a point is inside a given polygon or not
# Polygon is a list of (x,y) pairs.

def point_inside_polygon(x,y,poly):

    n = len(poly)
    inside =False

    p1x,p1y = poly[0]
    for i in range(n+1):
	print p1x, p1y
        p2x,p2y = poly[i % n]
        print y
        print p2x, p2y
        if y > min(p1y,p2y):
            if y <= max(p1y,p2y):
                if x <= max(p1x,p2x):
                    if p1y != p2y:
                        xinters = (y-p1y)*(p2x-p1x)/(p2y-p1y)+p1x
                    if p1x == p2x or x <= xinters:
                        inside = not inside
        p1x,p1y = p2x,p2y

    return inside

# just a test function    
def trigrid(x, y, z, xi, yi):

    print x.shape
    print y.shape
    print z.shape
    print xi.shape
    print yi.shape
#    mxi, myi = np.meshgrid(xi, yi)
#    zi = griddata.griddata(x, y, z, mxi, myi, masked=True)
    zi = mlab.griddata(x, y, z, xi, yi)
#    f = interpolate.interp2d(x, y, z)
#    zi = f(xi, yi)
    print zi
    fig = plt.figure
    plt.contour(zi)
    plt.show()
    return zi

def mygriddata(x, y, z, xi, yi, is_linear):

#    print x.shape
#    print y.shape
#    print z.shape
#    print xi.shape
#    print yi.shape

#    x = uniform(-2,2,20)
#    y = uniform(-2,2,20)
#    z = x*np.exp(-x**2-y**2)

#    print xi
#    print yi

#    xi = np.linspace(-2.1,2.1,10)
#    yi = np.linspace(-2.1,2.1,10)

#    print xi
#    print yi

#    zi = interpolate.griddata((x, y), z, (xi[None,:], yi[:,None]), method='cubic')
#    zi = interpolate.griddata((x, y), z, (xi, yi))
    zi = mlab.griddata(x, y, z, xi, yi)

    return zi
