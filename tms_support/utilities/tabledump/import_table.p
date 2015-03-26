DEF VAR lcInputFile AS CHAR NO-UNDO. 

lcInputFile = "{2}/{1}.d".

FILE-INFO:FILE-NAME = lcInputFile.
if FILE-INFO:FILE-TYPE EQ ? OR not FILE-INFO:FILE-TYPE begins "F" then do:
   MESSAGE lcInputFile "does not exists" VIEW-AS ALERT-BOX ERROR.
   RETURN.
end.

input from value(lcInputFile).

disp 'Deleting {1}'.
pause 0.

for each {1} exclusive-lock:
  delete {1}.
end.

disp 'Importing {1}'.
pause 0.

repeat:
  create {1}.
  import {1}.
end.
