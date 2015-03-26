DEF VAR qu-i AS INT NO-UNDO.


DO qu-i = 1 TO num-entries({1}):
   IF NOT qupd THEN ASSIGN ufk [ INTEGER (  entry(qu-i,{1}) ) ] = 0.
END.             
