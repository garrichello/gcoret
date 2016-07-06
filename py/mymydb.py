import MySQLdb
import numpy

def connect_db (db_host, db_name, user_name, user_pass):
    global conn
    global cur
    
    print 'Connecting to DB: '+db_name+' for user: '+user_name
    conn = MySQLdb.connect(host=db_host, db=db_name, user=user_name, passwd=user_pass)
    cur = conn.cursor()
    
    return not conn.open

def close_db ():
    global conn
    global cur
    
    print 'Closing connection to DB' #: '+db_name+' for user: '+user_name
    conn.close()
    
    return not conn.open

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

    buf = numpy.asarray([b[field_pos] for b in qry_buf])
    if len(buf) > 0:
	elem_len = buf.dtype.num
    else:
	elem_len = 0
    
    if data_type == 'string':
	data = numpy.fromstring(buf, dtype='uint8')
    elif data_type == 'location':
	spl_locs = [loc[9:-1].split() for loc in buf]
	data = numpy.array(spl_locs, dtype='float')
    elif data_type == 'numeric':
	data = numpy.asarray(buf, dtype='float')
    elif data_type == 'date':
	data = numpy.asarray([b[field_pos].timetuple() for b in qry_buf], dtype='float')
    else:
	print 'Unknown data type: '+data_type
	return -1
    
    return data

def get_elem_len():
    global elem_len
    
    return elem_len

