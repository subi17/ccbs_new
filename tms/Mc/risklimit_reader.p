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
DEF VAR liCustNum       AS INT  NO-UNDO.
DEF VAR ldLimitAmt      AS DEC  NO-UNDO.

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
   REPEAT TRANSACTION:

      IMPORT STREAM sIn UNFORMATTED lcLine.

      IF lcLine EQ "" THEN NEXT.

      IF NUM-ENTRIES(lcLine,lcSep) NE 2 THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.

      ASSIGN liCustNum  = INT(ENTRY(1,lcLine,lcSep))
             ldLimitAmt = DEC(REPLACE(ENTRY(2,lcLine,lcSep),",","."))
             NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         fError("Incorrect input data format").
         NEXT.
      END.

      IF NOT CAN-FIND(FIRST Customer WHERE
                            Customer.Brand = gcBrand AND
                            Customer.CustNum = liCustNum)
      THEN DO:
         fError("Unknown Customer").
         NEXT.
      END.

      FIND FIRST Limit EXCLUSIVE-LOCK WHERE
                 Limit.CustNum    = liCustNum AND
                 Limit.LimitType  = {&LIMIT_TYPE_RISKLIMIT} AND
                 Limit.ToDate    >= TODAY
                 NO-ERROR.

      IF AVAILABLE Limit THEN Limit.ToDate = TODAY - 1.
      
      CREATE Limit.
      ASSIGN
         Limit.Brand     = gcBrand
         Limit.CustNum   = liCustNum
         Limit.LimitAmt  = ldLimitAmt
         Limit.LimitType = {&LIMIT_TYPE_RISKLIMIT}
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
