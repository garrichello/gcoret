ATTACH DATABASE "datadesc2.db" AS datadesc;

CREATE TABLE datadesc.dataset(
 id integer UNIQUE NOT NULL,
 name char(10) UNIQUE NOT NULL,
 longname varchar(50),
 rootpath varchar(250),
 type char(10),
PRIMARY KEY (id));

CREATE TABLE datadesc.variable(
 id integer UNIQUE NOT NULL,
 name varchar(10) UNIQUE NOT NULL,
 longname varchar(50),
 units varchar(50),
PRIMARY KEY (id));

CREATE TABLE datadesc.levels(
 id integer UNIQUE NOT NULL,
 name varchar(50) UNIQUE NOT NULL,
PRIMARY KEY (id));

CREATE TABLE datadesc.file(
 id integer UNIQUE NOT NULL,
 name varchar(150) NOT NULL,
PRIMARY KEY (id));

CREATE TABLE datadesc.timestep(
 id integer UNIQUE NOT NULL,
 name varchar(50) UNIQUE NOT NULL,
PRIMARY KEY (id));

CREATE TABLE datadesc.data(
 variable_id integer NOT NULL,
 levels_id integer NOT NULL,
 timestep_id integer NOT NULL,
 file_id integer NOT NULL,
 dataset_id integer NOT NULL,
FOREIGN KEY (variable_id) REFERENCES variable (id),
FOREIGN KEY (levels_id) REFERENCES levels (id),
FOREIGN KEY (timestep_id) REFERENCES timestep (id),
FOREIGN KEY (file_id) REFERENCES file (id),
FOREIGN KEY (dataset_id) REFERENCES dataset (id),
PRIMARY KEY (variable_id,levels_id,timestep_id,file_id,dataset_id));


INSERT INTO datadesc.variable (name, units, longname) VALUES (" O3 ",
" kg kg**-1       ",
" Ozone mass mixing ratio     ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" R ",
" %        ",
" Relative humidity       ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" D ",
" s**-1        ",
" Divergence        ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" VO ",
" s**-1        ",
" Vorticity (relative)       ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" W ",
" Pa s**-1       ",
" Vertical velocity       ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" Q ",
" kg kg**-1       ",
" Specific humidity       ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" V ",
" m s**-1       ",
" V velocity       ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" U ",
" m s**-1       ",
" U velocity       ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" T ",
" K        ",
" Temperature        ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" Z ",
" m**2 s**-2       ",
" Geopotential        ");
INSERT INTO datadesc.variable (name, units, longname) VALUES (" PV ",
" K m**2 kg**-1 s**-1     ",
" Potential vorticity       ");
