/*
   Include file parameters:
      1 = buffer handle of tablename
      2 = separator ( :: or . )
      3 = delimiter ( for example objDumpBase:lcKeyDelimiter )
      4...12 = keyvalue fields (at least one is required)
*/
&IF '{4}' NE ''
&THEN
SUBSTITUTE("&1",{1}{2}{4})
&ENDIF
&IF '{5}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{5})
&ENDIF
&IF '{6}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{6})
&ENDIF
&IF '{7}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{7})
&ENDIF
&IF '{8}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{8})
&ENDIF
&IF '{9}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{9})
&ENDIF
&IF '{10}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{10})
&ENDIF
&IF '{11}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{11})
&ENDIF
&IF '{12}' NE ''
&THEN
+ {3} + SUBSTITUTE("&1",{1}{2}{12})
&ENDIF