DEF VAR lcFolder AS CHAR NO-UNDO FORMAT "x(40)".
DEF VAR lcInputFile AS CHAR NO-UNDO init "utilities/tabledump/config_tables.txt" FORMAT "x(40)".

lcFolder = "utilities/tabledump/config_tables/".

UPDATE 
    lcInputFile LABEL "Input file   " SKIP
    lcFolder    LABEL "Output folder" 
WITH FRAME a TITLE "Table Dump Export" SIDE-LABELS.

FILE-INFO:FILE-NAME = lcFolder.

if FILE-INFO:FILE-TYPE EQ ? OR not FILE-INFO:FILE-TYPE begins "D" then do:
   MESSAGE lcFolder "does not exists" VIEW-AS ALERT-BOX ERROR.
   RETURN.
end.

FILE-INFO:FILE-NAME = lcInputFile.

if FILE-INFO:FILE-TYPE EQ ? OR not FILE-INFO:FILE-TYPE begins "F" then do:
   MESSAGE lcInputFile "does not exists" VIEW-AS ALERT-BOX ERROR.
   RETURN.
end.

input from value(lcInputFile).

DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcOutputFolder AS CHAR NO-UNDO.

repeat:
   import unformatted lcLine.
   lcLine = TRIM(ENTRY(1,lcLine,":")).
   if lcLine eq "" or lcLine begins "#" then next.
   run utilities/tabledump/export_table.p lcLine lcFolder.
end.
