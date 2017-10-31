DEF VAR qu-i AS INT NO-UNDO.


DO qu-i = 1 TO num-entries({1}):
   IF NOT Syst.CUICommon:qupd THEN ASSIGN Syst.CUICommon:ufk [ INTEGER (  entry(qu-i,{1}) ) ] = 0.
END.             
