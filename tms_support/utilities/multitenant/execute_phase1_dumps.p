/* Script for generating phase1 dumps */
{Syst/commpaa.i}
gcbrand = "1".
{Func/multitenantfunc.i}
DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM outFile.
DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcDFIdList AS CHAR NO-UNDO.
DEF VAR liId AS INT NO-UNDO.
DEF VAR liDumpID      AS INT  NO-UNDO.
DEF VAR liDumped      AS INT  NO-UNDO.
DEF VAR lcDumpMode    AS CHAR NO-UNDO INIT "Full".
DEF VAR llOk          AS LOG  NO-UNDO.
DEF VAR lcFileNameTag AS CHAR NO-UNDO.

OUTPUT STREAM outfile TO VALUE("../tms_support/utilities/multitenant/dump_log.txt").

FUNCTION fMakeDump RETURNS LOGICAL (INPUT icdumpName AS CHAR):
   DEF VAR lcFilename AS CHAR NO-UNDO.
   FIND FIRST Dumpfile WHERE
              Dumpfile.brand EQ "1" AND
              Dumpfile.dumpname EQ icdumpname NO-ERROR.
   IF ERROR-STATUS:ERROR THEN DO:
      MESSAGE "No dumpfile: " + icDumpname VIEW-AS ALERT-BOX.
      NEXT.
   END.
   RUN Syst/dumpfile_run.p(DumpFile.dumpid,
                       lcDumpMode,
                       lcFileNameTag,
                       FALSE,
                       OUTPUT liDumped).
   PUT STREAM Outfile UNFORMATTED
      STRING(dumpfile.dumpid) + ";" + Dumpfile.dumpname + ";" + 
      DumpFile.transdir + "; NBR of Rows: " + STRING(liDumped).


END.

INPUT STREAM sin FROM VALUE("../tms_support/utilities/multitenant/dumpfiles_phase1.txt").

REPEAT:
   IMPORT STREAM sin UNFORMATTED lcLine.
   IF TRIM(lcLine) EQ "" THEN NEXT.
   fMakeDump(lcLine).
END.
