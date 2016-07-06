import pflexible as pf
import numpy as np


def read_header (data_path, sp_name):
    global header
    global sp_idx

#    print 'Reading header...'
    header = pf.Header(data_path, sp_name)

    res = 0

    try:
	sp_idx = header.species.index(sp_name)
    except ValueError:
	print 'Error! No such specie!'
	sp_idx = -1
	res = -1

    return res


def read_grid (times):
    global header
    global sp_idx
    global out_grid
    global time_grid
    
#    print 'Reading grid...'
    
    avail_times = [int(x)/10000 for x in header.available_dates]
    time_idx = np.where(np.logical_and(avail_times>=times[0], avail_times<=times[1]))
    time_grid = np.array(avail_times)[time_idx[0]]
    
    file_data = pf.read_grid(header, time_ret=time_idx[0], nspec_ret=sp_idx)
    
    data = [file_data[(0, x)] for x in file_data.grid_dates]
    
    data_grid = [data[i].grid for i in range(len(data))]
    
    out_grid = np.concatenate(data_grid, axis=3)   
    
#    print np.shape(out_grid)
    
    res = 0
    
    return res

    
def get_lats ():
    global header
    
    return header.latitude

    
def get_lons ():
    global header
    
    return header.longitude


def get_times ():
    global time_grid
    
    return time_grid.astype('float64')

    
def get_heights ():
    global header
    
    return header.outheight

    
def get_grid ():
    global out_grid
    
    return out_grid
