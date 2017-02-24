DEF VAR lcFolder AS CHAR NO-UNDO FORMAT "x(40)" init "utilities/tabledump/config_tables/".
DEF VAR lcInputFile AS CHAR NO-UNDO init "utilities/tabledump/config_tables.txt" FORMAT "x(60)".

DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,'sadachbia,alpheratz,angetenar') = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

UPDATE 
    lcInputFile LABEL "Input file   " SKIP
    lcFolder    LABEL "Input folder" WITH FRAME a TITLE
   "Table Dump Import" SIDE-LABELS.

FILE-INFO:FILE-NAME = lcInputFile.

if FILE-INFO:FILE-TYPE EQ ? OR not FILE-INFO:FILE-TYPE begins "F" then do:
   MESSAGE lcInputFile "does not exists" VIEW-AS ALERT-BOX ERROR.
   RETURN.
end.

FILE-INFO:FILE-NAME = lcFolder.

if FILE-INFO:FILE-TYPE EQ ? OR not FILE-INFO:FILE-TYPE begins "D" then do:
   MESSAGE lcFolder "does not exists" VIEW-AS ALERT-BOX ERROR.
   RETURN.
end.

input from value(lcInputFile).

DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcOutputFolder AS CHAR NO-UNDO.

repeat:
   import unformatted lcLine.
   lcLine = TRIM(ENTRY(1,lcLine,":")).
   if lcLine eq "" or lcLine begins "#" then next.
   RUN utilities/tabledump/import_table.p lcLine lcFolder.
end.

run utilities/tabledump/tmsparam_staging_update.p.

IF LOOKUP(lcHostName,'merga') > 0 THEN
   RUN utilities/tabledump/tmsparam_merga_update.p.

MESSAGE "Import done" VIEW-AS ALERT-BOX.
QUIT.
