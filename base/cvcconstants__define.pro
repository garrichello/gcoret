;
;  NAME: 
;    cvcConstants
;
;  PURPOSE:
;    Provides access to different additional constants. 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------

FUNCTION cvcConstants::Init, in_oGlobal

  self.oGlobal = in_oGlobal
  hLogFile = self.oGlobal->Get('hLogFile')
  res = self->cvcError::Init(hLogFile)
  self.hLogFile = hLogFile
  self.aT62Grid =   [-88.5419464111328, -86.6531677246094, -84.7532272338867, -82.8507690429688, $
                     -80.9473571777344, -79.0434799194336, -77.1393508911133, -75.2350540161133, $
                     -73.3306579589844, -71.4261856079102, -69.5216598510742, -67.6171035766602, $
                     -65.7125091552734, -63.8078956604004, -61.9032592773438, -59.9986114501953, $
                     -58.0939483642578, -56.1892776489258, -54.2845993041992, -52.3799133300781, $
                     -50.4752197265625, -48.5705184936523, -46.6658172607422, -44.7611083984375, $
                     -42.8563995361328, -40.9516868591309, -39.0469703674316, -37.1422500610352, $
                     -35.2375297546387, -33.3328056335449, -31.4280815124512, -29.5233554840088, $
                     -27.6186275482178, -25.7138996124268, -23.8091697692871, -21.9044399261475, $
                     -19.9997081756592, -18.0949764251709, -16.1902427673340, -14.2855100631714, $
                     -12.3807764053345, -10.4760417938232, -08.5713081359863, -06.6665730476379, $
                     -04.7618379592896, -02.8571028709412, -00.9523676037788,                    $
                      00.9523676037788,  02.8571028709412,  04.7618379592896,  06.6665730476379, $
                      08.5713081359863,  10.4760417938232,  12.3807764053345,  14.2855100631714, $
                      16.1902427673340,  18.0949764251709,  19.9997081756592,  21.9044399261475, $
                      23.8091697692871,  25.7138996124268,  27.6186275482178,  29.5233554840088, $
                      31.4280815124512,  33.3328056335449,  35.2375297546387,  37.1422500610352, $
                      39.0469703674316,  40.9516868591309,  42.8563995361328,  44.7611083984375, $
                      46.6658172607422,  48.5705184936523,  50.4752197265625,  52.3799133300781, $
                      54.2845993041992,  56.1892776489258,  58.0939483642578,  59.9986114501953, $
                      61.9032592773438,  63.8078956604004,  65.7125091552734,  67.6171035766602, $
                      69.5216598510742,  71.4261856079102,  73.3306579589844,  75.2350540161133, $
                      77.1393508911133,  79.0434799194336,  80.9473571777344,  82.8507690429688, $
                      84.7532272338867,  86.6531677246094,  88.5419464111328] 
                               
  self.aVarParamsAndUnits = [ ['Reserved',                                 ''], $
                             ['Pressure',                                 'Pa'], $
                             ['Pressure reduced to MSL',                  'Pa'], $
                             ['Pressure tendency',                        'Pa/s'], $
                             ['Unknown',                                  ''], $
                             ['Unknown',                                  ''], $
                             ['Geopotential',                             'm2/s2'], $
                             ['Geopotential height',                      'gpm'], $
                             ['Geometric height',                         'm'], $
                             ['Standard deviation of height',             'm'], $
                             ['Unknown',                                  ''], $
                             ['Temperature',                              'deg. K'], $
                             ['Virtual temperature',                      'deg. K'], $
                             ['Potential temperature',                    'deg. K'], $
                             ['Pseudo-adiabatic potential temperature',   'deg. K'], $
                             ['Maximum temperature',                      'deg. K'], $
                             ['Minimum temperature',                      'deg. K'], $
                             ['Dew point temperature',                    'deg. K'], $
                             ['Dew point depression (or deficit)',        'deg. K'], $
                             ['Lapse rate',                               'deg. K/m'], $
                             ['Visibility',                               'm'], $
                             ['Radar Spectra (1)',                        ''], $
                             ['Radar Spectra (2)',                        ''], $
                             ['Radar Spectra (3)',                        ''], $
                             ['Unknown',                                  ''], $
                             ['Temperature anomaly',                      'deg. K'], $
                             ['Pressure anomaly',                         'Pa'], $
                             ['Geopotential height anomaly',              'gpm'], $
                             ['Wave Spectra (1)',                         ''], $
                             ['Wave Spectra (2)',                         ''], $
                             ['Wave Spectra (3)',                         ''], $
                             ['Wind direction',                           'deg. true'], $
                             ['Wind speed',                               'm/s'], $
                             ['u-component of wind',                      'm/s'], $
                             ['v-component of wind',                      'm/s'], $
                             ['Stream function',                          'm2/s'], $
                             ['Velocity potential',                       'm2/s'], $
                             ['Montgomery stream function',               'm2/s2'], $
                             ['Sigma coord. vertical velocity',           '/s'], $
                             ['Pressure Vertical velocity',               'Pa/s'], $
                             ['Geometric Vertical velocity',              'm/s'], $
                             ['Absolute vorticity',                       '/s'], $
                             ['Absolute divergence',                      '/s'], $
                             ['Relative vorticity',                       '/s'], $
                             ['Relative divergence',                      '/s'], $
                             ['Vertical u-component shear',               '/s'], $
                             ['Vertical v-component shear',               '/s'], $
                             ['Direction of current',                     'deg. true'], $
                             ['Speed of current',                         'm/s'], $
                             ['u-component of current',                   'm/s'], $
                             ['v-component of current',                   'm/s'], $
                             ['Specific humidity',                        'kg/kg'], $
                             ['Relative humidity',                        '%'], $
                             ['Humidity mixing ratio',                    'kg/kg'], $
                             ['Precipitable water',                       'kg/m2'], $
                             ['Vapor pressure',                           'Pa'], $
                             ['Saturation deficit',                       'Pa'], $
                             ['Evaporation',                              'kg/m2'], $
                             ['Cloud Ice',                                'kg/m2'], $
                             ['Precipitation rate',                       'kg/m2/s'], $
                             ['Thunderstorm probability',                 '%'], $
                             ['Total precipitation',                      'kg/m2'], $
                             ['Large scale precipitation',                'kg/m2'], $
                             ['Convective precipitation',                 'kg/m2'], $
                             ['Snowfall rate water equivalent',           'kg/m2s'], $
                             ['Water equiv. of accum. snow depth',        'kg/m2'], $
                             ['Snow depth',                               'm'], $
                             ['Mixed layer depth',                        'm'], $
                             ['Transient thermocline depth',              'm'], $
                             ['Main thermocline depth',                   'm'], $
                             ['Main thermocline anomaly',                 'm'], $
                             ['Total cloud cover',                        '%'], $
                             ['Convective cloud cover',                   '%'], $
                             ['Low cloud cover',                          '%'], $
                             ['Medium cloud cover',                       '%'], $
                             ['High cloud cover',                         '%'], $
                             ['Cloud water',                              'kg/m2'], $
                             ['Unknown',                                  ''], $
                             ['Convective snow',                          'kg/m2'], $
                             ['Large scale snow',                         'kg/m2'], $
                             ['Water Temperature',                        'deg K'], $
                             ['Land-sea mask',                            '(1=land;0=sea)'], $
                             ['Deviation of sea level from mean',         'm'], $
                             ['Surface roughness',                        'm'], $
                             ['Albedo',                                   '%'], $
                             ['Soil temperature',                         'deg. K'], $
                             ['Soil moisture content',                    'kg/m2'], $
                             ['Vegetation',                               '%'], $
                             ['Salinity',                                 'kg/kg'], $
                             ['Density',                                  'kg/m3'], $
                             ['Water runoff',                             'kg/m2'], $
                             ['Ice concentration',                        '(ice=1;no ice=0)'], $
                             ['Ice thickness',                            'm'], $
                             ['Direction of ice drift',                   'deg. true'], $
                             ['Speed of ice drift',                       'm/s'], $
                             ['u-component of ice drift',                 'm/s'], $
                             ['v-component of ice drift',                 'm/s'], $
                             ['Ice growth rate',                          'm/s'], $
                             ['Ice divergence',                           '/s'], $
                             ['Snow melt',                                'kg/m2'], $
                             ['Significant height of combined wind waves and swell',     'm'], $
                             ['Direction of wind waves',                  'deg. true'], $
                             ['Significant height of wind waves',         'm'], $
                             ['Mean period of wind waves',                's'], $
                             ['Direction of swell waves',                 'deg. true'], $
                             ['Significant height of swell waves',        'm'], $
                             ['Mean period of swell waves',               's'], $
                             ['Primary wave direction',                   'deg. true'], $
                             ['Primary wave mean period',                 's'], $
                             ['Secondary wave direction',                 'deg. true'], $
                             ['Secondary wave mean period',               's'], $
                             ['Net short-wave radiation (surface)',       'W/m2'], $
                             ['Net long wave radiation (surface)',        'W/m2'], $
                             ['Net short-wave radiation (top of atmos.)', 'W/m2'], $
                             ['Net long wave radiation (top of atmos.)',  'W/m2'], $
                             ['Long wave radiation',                      'W/m2'], $
                             ['Short wave radiation',                     'W/m2'], $
                             ['Global radiation',                         'W/m2'], $
                             ['Unknown',                                  ''], $
                             ['Unknown',                                  ''], $
                             ['Unknown',                                  ''], $
                             ['Latent heat net flux',                     'W/m2'], $
                             ['Sensible heat net flux',                   'W/m2'], $
                             ['Boundary layer dissipation',               'W/m2'], $
                             ['Momentum flux, u component',               'N/m2'], $
                             ['Momentum flux, v component',               'N/m2'], $
                             ['Wind mixing energy',                       'J'], $
                             ['Image data',                               ''] ]

; color tables
  self.aColorTableList = ['B-W LINEAR', $
                         'BLUE/WHITE', $
                         'GRN-RED-BLU-WHT', $
                         'RED TEMPERATURE', $ 
                         'BLUE/GREEN/RED/YELLOW', $
                         'STD GAMMA-II', $
                         'PRISM', $
                         'RED-PURPLE', $
                         'GREEN/WHITE LINEAR', $
                         'GRN/WHT EXPONENTIAL', $
                         'GREEN-PINK', $
                         'BLUE-RED', $
                         '16 LEVEL', $
                         'RAINBOW', $
                         'STEPS', $
                         'STERN SPECIAL', $
                         'Haze', $
                         'Blue - Pastel - Red', $
                         'Pastels', $
                         'Hue Sat Lightness 1', $
                         'Hue Sat Lightness 2', $
                         'Hue Sat Value 1', $
                         'Hue Sat Value 2', $
                         'Purple-Red + Stripes', $
                         'Beach', $
                         'Mac Style', $
                         'Eos A', $
                         'Eos B', $
                         'Hardcandy', $
                         'Nature', $
                         'Ocean', $
                         'Peppermint', $
                         'Plasma', $
                         'Blue-Red', $
                         'Rainbow', $
                         'Blue Waves', $
                         'Volcano', $
                         'Waves', $
                         'Rainbow18', $
                         'Rainbow + white', $
                         'Rainbow + black']
         
  self.year4Dig = '%year%'
  self.year4Dig1 = '%year1%'
  self.year4Dig2 = '%year2%'
  self.year2Dig = '%yr%'
  self.month3Small = '%mon%'
  self.month3Cap = '%Mon%'
  self.month3Big = '%MON%'
  self.month2Dig = '%mm%'
  self.day2Dig = '%dd%'
  self.hour2Dig = '%hh%'
  self.path2Dig = '%path%'
  self.row2Dig = '%row%'
  
  return, 1
END
;--------------------------------------------------------------------
PRO cvcConstants::Cleanup
 
END
;--------------------------------------------------------------------
FUNCTION cvcConstants::Get, in_constName

  case in_constName of
    'VPAU': return, self.aVarParamsAndUnits
    'CTL' : return, self.aColorTableList
    'T62' : return, self.aT62Grid
    'year4dig' : return, self.year4Dig
    'year4dig1' : return, self.year4Dig1
    'year4dig2' : return, self.year4Dig2
    'year2dig' : return, self.year2Dig
    'month3small' : return, self.month3Small
    'month3cap' : return, self.month3Cap
    'month3big' : return, self.month3Big
    'month2dig' : return, self.month2Dig
    'day2dig' : return, self.day2Dig
    'hour2dig' : return, self.hour2Dig
    'path2dig' : return, self.path2Dig
    'row2dig' : return, self.row2Dig
    else: return, self->SayError(self.ERROR_BADCONST, "cvcConstants::Get")
  endcase
end

;--------------------------------------------------------------------
PRO cvcConstants__define

    struct = { cvcConstants, $
		  oGlobal : obj_new(), $ ; global dataOC
; grib parameters and units
                  aT62Grid : fltarr(94), $
                  aVarParamsAndUnits : strarr(2, 128), $
                  aColorTableList : strarr(41), $
; file name templates
                  year4Dig : '', $
                  year4Dig1 : '', $
                  year4Dig2 : '', $
                  year2Dig : '', $
                  month3Small : '', $
                  month3Cap : '', $
                  month3Big : '', $
                  month2Dig : '', $
                  day2Dig : '', $
                  hour2Dig : '', $
                  path2Dig : '', $
                  row2Dig : '', $
                  INHERITS cvcError $
             }
END