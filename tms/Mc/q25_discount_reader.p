/*------------------------------------------------------------------------
  MODULE .......: q25_discount_reader.p
  TASK .........: BOB tool to handle Q25 discount per subscription file
  APPLICATION ..: TMS
  AUTHOR .......: ivekov
  CREATED ......: 09.03.16
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

DEF VAR lcIncDir        AS CHAR NO-UNDO.
DEF VAR lcProcDir       AS CHAR NO-UNDO.
DEF VAR lcSpoolDir      AS CHAR NO-UNDO.
DEF VAR lcLogDir        AS CHAR NO-UNDO.
DEF VAR lcFileName      AS CHAR NO-UNDO.
DEF VAR lcInputFile     AS CHAR NO-UNDO.
DEF VAR lcLine          AS CHAR NO-UNDO.
DEF VAR lcSep           AS CHAR NO-UNDO.
DEF VAR lcLogFile       AS CHAR NO-UNDO.
DEF VAR lcMoveProc      AS CHAR NO-UNDO.
DEF VAR lcMoveLog       AS CHAR NO-UNDO.
DEF VAR lcMSISDN        AS CHAR NO-UNDO.
DEF VAR ldLimitAmt      AS DEC  NO-UNDO.
DEF VAR ldaValidTo      AS DATE NO-UNDO.

ASSIGN
   lcIncDir   = fCParam("Q25DiscReader","IncDir")
   lcProcDir  = fCParam("Q25DiscReader","ProcDir")
   lcSpoolDir = fCParam("Q25DiscReader","SpoolDir")
   lcLogDir   = fCParam("Q25DiscReader","LogDir")
   lcSep      = ";".

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
   REPEAT TRANSACTION:

      IMPORT STREAM sIn UNFORMATTED lcLine.

      IF NUM-ENTRIES(lcLine,lcSep) NE 3 THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.

      ASSIGN lcMSISDN   = ENTRY(1,lcLine,lcSep)
             ldLimitAmt = DEC(ENTRY(2,lcLine,lcSep))
             ldaValidTo = DATE(ENTRY(3,lcLine,lcSep)).

      IF ERROR-STATUS:ERROR THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.

      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.Brand = gcBrand AND
                 MobSub.CLI = lcMSISDN NO-ERROR.
      IF NOT AVAILABLE MobSub THEN DO:
         fError("Unknown Subscription").
         NEXT.
      END.

      FIND FIRST Limit EXCLUSIVE-LOCK WHERE
                 Limit.MsSeq = MobSub.MsSeq AND
                 Limit.LimitType = {&LIMIT_TYPE_Q25_DISCOUNT} AND
                 Limit.ToDate >= TODAY
                 NO-ERROR.

      IF AVAILABLE Limit THEN Limit.ToDate = TODAY - 1.

      CREATE Limit.
      ASSIGN
         Limit.Brand     = gcBrand
         Limit.CustNum   = MobSub.CustNum
         Limit.MsSeq     = MobSub.MsSeq
         Limit.LimitAmt  = ldLimitAmt
         Limit.LimitType = {&LIMIT_TYPE_Q25_DISCOUNT}
         Limit.ValueType = 1
         Limit.FromDate  = TODAY
         Limit.ToDate    = 12/31/2049
         Limit.DefValue  = FALSE.

      RELEASE Limit.

   END.

   INPUT STREAM sIn CLOSE.
   OUTPUT STREAM sLog CLOSE.

   lcMoveProc = fMove2TransDir(lcInputFile, "", lcProcDir).
   lcMoveLog  = fMove2TransDir(lcLogFile, "", lcLogDir).

END.

INPUT STREAM sFile CLOSE.
