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
DEF VAR lcLogDir       AS CHAR NO-UNDO.
DEF VAR lcFileName      AS CHAR NO-UNDO.
DEF VAR lcInputFile     AS CHAR NO-UNDO.
DEF VAR lcLine          AS CHAR NO-UNDO.
DEF VAR lcSep           AS CHAR NO-UNDO.
DEF VAR lcLogFile       AS CHAR NO-UNDO.
DEF VAR liEntries       AS INT  NO-UNDO. 

ASSIGN
   lcIncDir   = fCParam("RiskLimit","IncDir")
   lcProcDir  = fCParam("RiskLimit","ProcDir")
   lcSpoolDir = fCParam("RiskLimit","SpoolDir")
   lcLogDir   = fCParam("RiskLimit","LogDir")
   lcSep      = ",".

DEF STREAM sIn.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGICAL
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGICAL
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
END FUNCTION.

INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
   lcInputFile = lcIncDir + lcFileName.

   IF SEARCH(lcInputFile) NE ? THEN
      INPUT STREAM sIn FROM VALUE(lcInputFile).
   ELSE NEXT.

   lcLogFile = lcSpoolDir + lcFileName + ".LOG".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
   
   LINE_LOOP:
   REPEAT:

      IMPORT STREAM sIn UNFORMATTED lcLine.

      IF lcLine EQ "" THEN NEXT.

      FIND FIRST Limit EXCLUSIVE-LOCK WHERE
                 Limit.CustNum   = INT(ENTRY(1,lcLine,lcSep)) AND
                 limit.LimitType = 5 /* {&LIMIT_TYPE_RISKLIMIT} */
                 NO-ERROR.   
      IF AVAILABLE Limit THEN
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhLimit).
      ELSE CREATE Limit.

      ASSIGN
         liEntries       = NUM-ENTRIES(lcLine,lcSep)
         Limit.CustNum   = INT(ENTRY(1,lcLine,lcSep))
         Limit.LimitAmt  = DEC(ENTRY(2,lcLine,lcSep))
         Limit.LimitType = 5  /* {&LIMIT_TYPE_RISKLIMIT} */
         Limit.FromDate  = TODAY.

      IF ERROR-STATUS:ERROR OR liEntries NE 2 THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.

      IF llDoEvent THEN DO:
         IF NEW Limit THEN RUN StarEventMakeCreateEvent(lhLimit).
         ELSE RUN StarEventMakeModifyEvent(lhLimit).
      END.

      RELEASE Limit.

   END.

   INPUT STREAM sIn CLOSE.
   OUTPUT STREAM sLog CLOSE.

   fMove2TransDir(lcInputFile, "", lcProcDir).
   fMove2TransDir(lcLogFile, "", lcLogDir).

END.

INPUT STREAM sFile CLOSE.

fCleanEventObjects().
