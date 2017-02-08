{Func/multitenantfunc.i}
{Syst/tmsconst.i}
DEFINE VARIABLE lcTenant AS CHARACTER NO-UNDO INIT "tyoigo" format "x(10)".

DEF VAR lcFolder AS CHAR NO-UNDO FORMAT "x(60)" init "../tms_support/utilities/tabledump/yoigo_dump_data/".
DEF VAR lcInputFile AS CHAR NO-UNDO init "../tms_support/utilities/tabledump/business_tables.txt" FORMAT "x(60)".

DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,SUBST("&1,&2",
          {&HOSTNAME_STAGING},{&HOSTNAME_DEVEL})) = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

UPDATE 
    lcTenant    LABEL "Tenant ......" 
      validate (lookup(input lcTenant,"tyoigo,tmasmovil") > 0,
      "invalid tenant")  SKIP
    lcInputFile LABEL "Input file .." SKIP
    lcFolder    LABEL "Input folder " WITH FRAME a TITLE
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

fsetEffectiveTenantForAllDB(lcTenant).

input from value(lcInputFile).

DEF VAR lcLine AS CHAR NO-UNDO. 
DEF VAR lcOutputFolder AS CHAR NO-UNDO.

repeat:
   import unformatted lcLine.
   lcLine = TRIM(ENTRY(1,lcLine,":")).
   if lcLine eq "" or lcLine begins "#" then next.
   RUN ../tms_support/utilities/tabledump/import_table.p lcLine lcFolder.
end.

MESSAGE "Import done" VIEW-AS ALERT-BOX.
QUIT.
