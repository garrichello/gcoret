;
;  NAME: 
;    cvcGlobal
;
;  PURPOSE:
;    Global functions 
;
;  INHERITS:
;    cvcError
;
;  CALLING SEQUENCE:
;    None 
;
;--------------------------------------------------------------------
FUNCTION cvcGlobal::Init
    
    self.nGlobals = 0
    return, 1
END
;--------------------------------------------------------------------
PRO cvcGlobal::Cleanup

;    for i = 0, self.nGlobals-1 do if (ptr_valid(self.asGlobals[i].data)) then ptr_free(self.asGlobals[i].data)

END
;--------------------------------------------------------------------
FUNCTION cvcGlobal::isatt, inStruct, inTag
  if (n_elements(inStruct) eq 0) then return, 0
  if (size(inStruct, /type) ne size({a:0}, /type)) then return, 0
  pos = where(tag_names(inStruct) eq strupcase(inTag))
  return, pos ne -1
END
;--------------------------------------------------------------------
FUNCTION cvcGlobal::fspan, min, max, n
  return, findgen(n)/(n-1)*(max-min)+min
END
;--------------------------------------------------------------------
FUNCTION cvcGlobal::Add, UID=in_uid, TYPE=in_type, VALUE=in_value
    
    if (self.nGlobals eq 16) then begin
	print, 'ERROR! Too much globals!'
	return, -1
    endif

    if (n_elements(in_uid) ne 0) then self.asGlobals[self.nGlobals].uid = in_uid
    if (n_elements(in_type) ne 0) then self.asGlobals[self.nGlobals].type = in_type
    if (n_elements(in_value) ne 0) then self.asGlobals[self.nGlobals].data = ptr_new(in_value)
    self.nGlobals += 1
    
    return, self.ERROR_OK
END
;--------------------------------------------------------------------
FUNCTION cvcGlobal::Get, in_uid

    idx = where(self.asGlobals.uid eq in_uid)
    if (idx[0] ne -1) then res = *(self.asGlobals[idx].data) else begin
      print, 'ERROR! Wrong UID of global variable!'
      res = 0
    endelse
    
    return, res
END
;--------------------------------------------------------------------
;FUNCTION cvcGlobal::getColor, theColor, colorIndex, Triple=triple, Decomposed=decomposedState
;
;   ; Make sure you have a color name and color index.
;
;  CASE N_Elements(theColor) OF
;     0: BEGIN
;        theColor = 'White'
;        IF N_Elements(colorIndex) EQ 0 THEN colorIndex = !P.Color < (!D.Table_Size - 1) $
;           ELSE colorIndex = 0 > colorIndex < (!D.Table_Size - 1)
;        ENDCASE
;  
;     1: BEGIN
;           type = Size(theColor, /TNAME)
;           IF type NE 'STRING' THEN Message, 'The color must be expressed as a color name.'
;           IF N_Elements(colorIndex) EQ 0 THEN colorIndex = !P.Color < (!D.Table_Size - 1) $
;              ELSE colorIndex = 0 > colorIndex < (!D.Table_Size - 1)
;           ENDCASE
;  
;     ELSE: BEGIN
;           type = Size(theColor, /TNAME)
;           IF type NE 'STRING' THEN Message, 'The colors must be expressed as color names.'
;           ncolors = N_Elements(theColor)
;           CASE N_Elements(colorIndex) OF
;              0: colorIndex = Indgen(ncolors) + (!D.Table_Size - (ncolors + 1))
;              1: colorIndex = Indgen(ncolors) + colorIndex
;              ELSE: IF N_Elements(colorIndex) NE ncolors THEN $
;                 Message, 'Index vector must be the same length as color name vector.'
;           ENDCASE
;  
;              ; Did the user want color triples?
;  
;           IF Keyword_Set(triple) THEN BEGIN
;              colors = LonArr(3, ncolors)
;              FOR j=0,ncolors-1 DO colors[*, j] = getColor(theColor[j], colorIndex[j], $
;                 Decomposed=decomposedState, /Triple)
;              RETURN, colors
;           ENDIF ELSE BEGIN
;              colors = LonArr(ncolors)
;              FOR j=0,ncolors-1 DO colors[j] = getColor(theColor[j], colorIndex[j], $
;                 Decomposed=decomposedState)
;              RETURN, colors
;          ENDELSE
;        END
;  ENDCASE
;  
;     ; Make sure the color parameter is an uppercase string.
;  
;  varInfo = Size(theColor)
;  IF varInfo[varInfo[0] + 1] NE 7 THEN $
;     Message, 'The color name parameter must be a string.', /NoName
;  theColor = StrUpCase(StrCompress(StrTrim(theColor,2), /Remove_All))
;  
;     ; Check synonyms of color names.
;  
;  IF StrUpCase(theColor) EQ 'GREY' THEN theColor = 'GRAY'
;  IF StrUpCase(theColor) EQ 'LIGHTGREY' THEN theColor = 'LIGHTGRAY'
;  IF StrUpCase(theColor) EQ 'MEDIUMGREY' THEN theColor = 'MEDIUMGRAY'
;  IF StrUpCase(theColor) EQ 'SLATEGREY' THEN theColor = 'SLATEGRAY'
;  IF StrUpCase(theColor) EQ 'DARKGREY' THEN theColor = 'DARKGRAY'
;  IF StrUpCase(theColor) EQ 'AQUA' THEN theColor = 'AQUAMARINE'
;  IF StrUpCase(theColor) EQ 'SKY' THEN theColor = 'SKYBLUE'
;  IF StrUpCase(theColor) EQ 'NAVY BLUE' THEN theColor = 'NAVY'
;  IF StrUpCase(theColor) EQ 'NAVYBLUE' THEN theColor = 'NAVY'
;  
;   ; Set up the color vectors.
;  
;  colors = ['White']
;  rvalue = [ 255]
;  gvalue = [ 255]
;  bvalue = [ 255]
;  colors = [ colors,       'Snow',     'Ivory','Light Yellow',   'Cornsilk',      'Beige',   'Seashell' ]
;  rvalue = [ rvalue,          255,          255,          255,          255,          245,          255 ]
;  gvalue = [ gvalue,          250,          255,          255,          248,          245,          245 ]
;  bvalue = [ bvalue,          250,          240,          224,          220,          220,          238 ]
;  colors = [ colors,      'Linen','Antique White',    'Papaya',     'Almond',     'Bisque',  'Moccasin' ]
;  rvalue = [ rvalue,          250,          250,          255,          255,          255,          255 ]
;  gvalue = [ gvalue,          240,          235,          239,          235,          228,          228 ]
;  bvalue = [ bvalue,          230,          215,          213,          205,          196,          181 ]
;  colors = [ colors,      'Wheat',  'Burlywood',        'Tan', 'Light Gray',   'Lavender','Medium Gray' ]
;  rvalue = [ rvalue,          245,          222,          210,          230,          230,          210 ]
;  gvalue = [ gvalue,          222,          184,          180,          230,          230,          210 ]
;  bvalue = [ bvalue,          179,          135,          140,          230,          250,          210 ]
;  colors = [ colors,       'Gray', 'Slate Gray',  'Dark Gray',   'Charcoal',      'Black', 'Light Cyan' ]
;  rvalue = [ rvalue,          190,          112,          110,           70,            0,          224 ]
;  gvalue = [ gvalue,          190,          128,          110,           70,            0,          255 ]
;  bvalue = [ bvalue,          190,          144,          110,           70,            0,          255 ]
;  colors = [ colors,'Powder Blue',   'Sky Blue', 'Steel Blue','Dodger Blue', 'Royal Blue',       'Blue' ]
;  rvalue = [ rvalue,          176,          135,           70,           30,           65,            0 ]
;  gvalue = [ gvalue,          224,          206,          130,          144,          105,            0 ]
;  bvalue = [ bvalue,          230,          235,          180,          255,          225,          255 ]
;  colors = [ colors,       'Navy',   'Honeydew', 'Pale Green','Aquamarine','Spring Green',       'Cyan' ]
;  rvalue = [ rvalue,            0,          240,          152,          127,            0,            0 ]
;  gvalue = [ gvalue,            0,          255,          251,          255,          250,          255 ]
;  bvalue = [ bvalue,          128,          240,          152,          212,          154,          255 ]
;  colors = [ colors,  'Turquoise', 'Sea Green','Forest Green','Green Yellow','Chartreuse', 'Lawn Green' ]
;  rvalue = [ rvalue,           64,           46,           34,          173,          127,          124 ]
;  gvalue = [ gvalue,          224,          139,          139,          255,          255,          252 ]
;  bvalue = [ bvalue,          208,           87,           34,           47,            0,            0 ]
;  colors = [ colors,      'Green', 'Lime Green', 'Olive Drab',     'Olive','Dark Green','Pale Goldenrod']
;  rvalue = [ rvalue,            0,           50,          107,           85,            0,          238 ]
;  gvalue = [ gvalue,          255,          205,          142,          107,          100,          232 ]
;  bvalue = [ bvalue,            0,           50,           35,           47,            0,          170 ]
;  colors = [ colors,      'Khaki', 'Dark Khaki',     'Yellow',       'Gold','Goldenrod','Dark Goldenrod']
;  rvalue = [ rvalue,          240,          189,          255,          255,          218,          184 ]
;  gvalue = [ gvalue,          230,          183,          255,          215,          165,          134 ]
;  bvalue = [ bvalue,          140,          107,            0,            0,           32,           11 ]
;  colors = [ colors,'Saddle Brown',       'Rose',       'Pink', 'Rosy Brown','Sandy Brown',       'Peru' ]
;  rvalue = [ rvalue,          139,          255,          255,          188,          244,          205 ]
;  gvalue = [ gvalue,           69,          228,          192,          143,          164,          133 ]
;  bvalue = [ bvalue,           19,          225,          203,          143,           96,           63 ]
;  colors = [ colors,  'Indian Red',  'Chocolate',     'Sienna','Dark Salmon',    'Salmon','Light Salmon' ]
;  rvalue = [ rvalue,          205,          210,          160,          233,          250,          255 ]
;  gvalue = [ gvalue,           92,          105,           82,          150,          128,          160 ]
;  bvalue = [ bvalue,           92,           30,           45,          122,          114,          122 ]
;  colors = [ colors,     'Orange',      'Coral', 'Light Coral',  'Firebrick',      'Brown',  'Hot Pink' ]
;  rvalue = [ rvalue,          255,          255,          240,          178,          165,          255 ]
;  gvalue = [ gvalue,          165,          127,          128,           34,           42,          105 ]
;  bvalue = [ bvalue,            0,           80,          128,           34,           42,          180 ]
;  colors = [ colors,  'Deep Pink',    'Magenta',     'Tomato', 'Orange Red',        'Red', 'Violet Red' ]
;  rvalue = [ rvalue,          255,          255,          255,          255,          255,          208 ]
;  gvalue = [ gvalue,           20,            0,           99,           69,            0,           32 ]
;  bvalue = [ bvalue,          147,          255,           71,            0,            0,          144 ]
;  colors = [ colors,     'Maroon',    'Thistle',       'Plum',     'Violet',    'Orchid','Medium Orchid']
;  rvalue = [ rvalue,          176,          216,          221,          238,          218,          186 ]
;  gvalue = [ gvalue,           48,          191,          160,          130,          112,           85 ]
;  bvalue = [ bvalue,           96,          216,          221,          238,          214,          211 ]
;  colors = [ colors,'Dark Orchid','Blue Violet',     'Purple' ]
;  rvalue = [ rvalue,          153,          138,          160 ]
;  gvalue = [ gvalue,           50,           43,           32 ]
;  bvalue = [ bvalue,          204,          226,          240 ]
;  
;     ; How many colors do we have?
;  
;  ncolors = N_Elements(colors)
;  
;     ; Process the color names.
;  
;  theNames = StrUpCase( StrCompress( StrTrim( colors, 2 ), /Remove_All ) )
;  
;     ; Find the asked-for color in the color names array.
;  
;  theIndex = Where(theNames EQ theColor, foundIt)
;  theIndex = theIndex[0]
;  
;     ; If the color can't be found, report it and continue with
;     ; the first color in the color names array.
;  
;  IF foundIt EQ 0 THEN BEGIN
;     Message, "Can't find color " + theColor + ". Substituting " + StrUpCase(colors[0]) + ".", /Informational
;     theColor = theNames[0]
;     theIndex = 0
;  ENDIF
;  
;     ; Get the color triple for this color.
;  
;  r = rvalue[theIndex]
;  g = gvalue[theIndex]
;  b = bvalue[theIndex]
;  
;     ; Did the user want a color triple? If so, return it now.
;  
;  IF Keyword_Set(triple) THEN BEGIN
;     RETURN, [r, g, b]
;  ENDIF
;  
;     ; Otherwise, we are going to return either an index
;     ; number where the color has been loaded, or a 24-bit
;     ; value that can be decomposed into the proper color.
;  
;  IF N_Elements(decomposedState) EQ 0 THEN BEGIN
;     IF Float(!Version.Release) GE 5.2 THEN BEGIN
;        IF (!D.Name EQ 'X' OR !D.Name EQ 'WIN' OR !D.Name EQ 'MAC') THEN BEGIN
;           Device, Get_Decomposed=decomposedState
;        ENDIF ELSE decomposedState = 0
;     ENDIF ELSE decomposedState = 0
;  ENDIF ELSE decomposedState = Keyword_Set(decomposedState)
;  
;     ; Return the color value or values.
;  
;  IF decomposedState THEN BEGIN
;  
;        ; Need a color structure?
;    
;    RETURN, r + (g * 2L^8) + (b * 2L^16)
;  
;  ENDIF ELSE BEGIN
;  
;        IF N_Elements(colorIndex) EQ 0 THEN colorIndex = !D.Table_Size - ncolors - 1
;        IF colorIndex LT 0 THEN $
;           Message, 'Number of colors exceeds available color table values. Returning.', /NoName
;        IF (colorIndex + ncolors) GT 255 THEN $
;           Message, 'Number of colors exceeds available color table indices. Returning.', /NoName
;        IF !D.Name NE 'PRINTER' THEN TVLCT, rvalue, gvalue, bvalue, colorIndex
;        RETURN, IndGen(ncolors) + colorIndex
;  
;  ENDELSE
;
;END 
;--------------------------------------------------------------------
PRO cvcGlobal__define

    MAX_N_GLOBALS = 16

    struct = { cvcGlobal, $
	       INHERITS cvcError, $
	       nGlobals : 0, $
	       asGlobals : make_array(MAX_N_GLOBALS, value={structArgDesc}), $
               null : 0 $
             }
END