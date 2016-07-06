import psycopg2
import numpy

def connect_db (db_name, user_name):
    global conn
    global cur
    
    print 'Connecting to DB: '+db_name+' for user: '+user_name
    conn = psycopg2.connect("dbname="+db_name+" user="+user_name)
    cur = conn.cursor()
    
    return conn.closed

def close_db (db_name, user_name):
    global conn
    global cur
    
    print 'Closing connection to DB: '+db_name+' for user: '+user_name
    conn.close()
    
    return conn.closed

def query_db (query):
    global cur
    global qry_buf
    
#    print 'Querying data...'
    
    print query
    cur.execute(query)
    qry_buf = cur.fetchall()
    nlines = len(qry_buf)
#    print 'Total lines: '+str(nlines)

    return nlines

def get_data (field_pos, data_type):
    global qry_buf
    global elem_len
    
    if data_type == 'string':
        buf = numpy.asarray([b[field_pos] for b in qry_buf])
	elem_len = buf.itemsize
	data = numpy.fromstring(buf, dtype='uint8')
    elif data_type == 'location':
        buf = numpy.asarray([b[field_pos] for b in qry_buf])
	elem_len = 1
	spl_locs = [loc[9:-1].split() for loc in buf]
	data = numpy.array(spl_locs, dtype='float')
    elif data_type == 'float':
        buf = numpy.asarray([b[field_pos] for b in qry_buf], dtype='float')
	elem_len = 1
	data = buf #numpy.array(buf, dtype='float')
    elif data_type == 'date':
        buf = numpy.asarray([b[field_pos] for b in qry_buf], dtype='float')
	elem_len = 1
	data = buf #numpy.array(buf, dtype='uint8')
#	data = numpy.asarray([b[field_pos].timetuple() for b in qry_buf], dtype='float')
    else:
	print 'Unknown data type: '+data_type
	return -1
    
    return data

def get_elem_len():
    global elem_len
    
    return elem_len

