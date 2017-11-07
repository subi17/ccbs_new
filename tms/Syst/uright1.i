DEF VAR qu-i AS INT NO-UNDO.


DO qu-i = 1 TO num-entries({1}):
   IF NOT Syst.Var:qupd THEN ASSIGN Syst.Var:ufk [ INTEGER (  entry(qu-i,{1}) ) ] = 0.
END.             
