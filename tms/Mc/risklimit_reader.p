/*------------------------------------------------------------------------
  MODULE .......: risklimit_reader.p
  TASK .........: Read risk limits for customers from file
  APPLICATION ..: TMS
  AUTHOR .......: ivekov
  CREATED ......: 02.10.15
  CHANGED ......:
  Version ......: yoigo
-------------------------------------------------------------------------- */

{commpaa.i}
ASSIGN
   katun   = "Cron"
   gcBrand = "1".
{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}
   DEFINE VARIABLE lhLimit     AS HANDLE    NO-UNDO.
   lhLimit = BUFFER Limit:HANDLE.
   RUN StarEventInitialize(lhLimit).
END.

DEF VAR lcIncDir        AS CHAR NO-UNDO.
DEF VAR lcProcDir       AS CHAR NO-UNDO.
DEF VAR lcSpoolDir      AS CHAR NO-UNDO.
DEF VAR lcFileName      AS CHAR NO-UNDO.
DEF VAR lcInputFile     AS CHAR NO-UNDO.
DEF VAR lcLine          AS CHAR NO-UNDO.
DEF VAR lcSep           AS CHAR NO-UNDO.

ASSIGN
   lcIncDir   = fCParam("RiskLimit","IncDir")
   lcProcDir  = fCParam("RiskLimit","ProcDir")
   lcSpoolDir = fCParam("RiskLimit","SpoolDir")
   lcSep      = ",".

DEF STREAM sIn.
DEF STREAM sFile.

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN
      INPUT STREAM sIn FROM VALUE(lcInputFile).
   ELSE NEXT.
   
   LINE_LOOP:
   REPEAT:

      IMPORT STREAM sIn UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      RUN pUpdateLimit(lcLine).

   END.

   fMove2TransDir(lcInputFile, "", lcProcDir).

   INPUT STREAM sIn CLOSE.

END.

INPUT STREAM sFile CLOSE.

fCleanEventObjects().

PROCEDURE pUpdateLimit:

   DEF INPUT PARAMETER pcLine AS CHAR NO-UNDO.

   DEF VAR liCustNum  AS INT  NO-UNDO.
   DEF VAR ldLimitAmt AS DEC  NO-UNDO.

   ASSIGN liCustNum  = INT(ENTRY(1,pcLine,lcSep))
          ldLimitAmt = DEC(ENTRY(2,pcLine,lcSep)).

   FIND FIRST Limit EXCLUSIVE-LOCK WHERE
              Limit.CustNum   = liCustNum AND
              limit.LimitType = {&LIMIT_TYPE_RISKLIMIT}
              NO-ERROR.   
   IF AVAILABLE Limit THEN DO:
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).
      Limit.LimitAmt = ldLimitAmt.
   END.
   ELSE DO:
      CREATE Limit.
      ASSIGN
         Limit.CustNum   = liCustNum
         Limit.LimitAmt  = ldLimitAmt
         Limit.LimitType = {&LIMIT_TYPE_RISKLIMIT}.
   END.

   IF llDoEvent THEN DO:
      IF NEW Limit THEN RUN StarEventMakeCreateEvent(lhLimit).
      ELSE RUN StarEventMakeModifyEvent(lhLimit).
   END.

   RELEASE Limit.

END PROCEDURE.
