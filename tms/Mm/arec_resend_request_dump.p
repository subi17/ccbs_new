/* ----------------------------------------------------------------------
  MODULE .......: arec_resend_request_dump.p 
  TASK .........  YDR-2149

   Dump is done daily.

   Dump to send AREC resent requests information to DWH  

   Fields in dump collected from Eventlog
   1. MSISDN
   2. AREC Time stamp: First time in which MNP was set to AREC ENUME.
   3. Type of AREC: AREC description (AREC ENUME, AREC CUPO...).
   4. Entry Operator: Operator chosen during order process. (Wrong operator)
   5. Processed Operator: assigned operator when re-launched automatically (Correct operator).
   6. Processed Time Stamp: time when it was re-launched.

  APPLICATION ..: TMS
  AUTHOR .......: Subhash Sanjeevi 
  CREATED ......: 19.04.16
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commpaa.i}
katun = "Qvantel".
gcBrand = "1".

{tmsconst.i}
{timestamp.i}
{dumpfile_run.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcDelimiter     AS CHAR NO-UNDO.
DEF VAR ldaDumpDate     AS DATE NO-UNDO.
DEF VAR lcDumpEventTime AS CHAR NO-UNDO.
DEF VAR liDumpTime      AS INT  NO-UNDO. 
DEF VAR lcFieldName     AS CHAR NO-UNDO. 
DEF VAR ldeEventTS      AS DEC  NO-UNDO.
DEF VAR liOrderID       AS INT  NO-UNDO. 

DEF STREAM sFile.

DEFINE TEMP-TABLE ttDumpData NO-UNDO
   FIELD MSISDN        AS CHARACTER
   FIELD ARECTimeStamp AS CHARACTER
   FIELD ARECType      AS CHARACTER
   FIELD WrongOperator AS CHARACTER
   FIELD NewOperator   AS CHARACTER
   FIELD ProcTimeStamp AS CHARACTER.

FUNCTION fDumpArecRejections RETURNS LOGICAL 
   (INPUT ldDumpDate AS DATE,
    INPUT lcDumpTime AS CHAR):

   FOR EACH EventLog NO-LOCK WHERE 
            EventLog.EventDate  = ldDumpDate AND
            EventLog.EventTime >= lcDumpTime AND
            EventLog.TableName  = "Order"
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE. 
         LEAVE.
      END.

      ASSIGN
         liOrderID   = 0
         lcFieldName = ""
         ldeEventTS  = 0
         liOrderID   = INT(ENTRY(2, EventLog.key,CHR(255))) 
         lcFieldName = ENTRY(1, EventLog.Datavalues,CHR(255))
         ldeEventTS  = fHMS2TS(EventLog.EventDate,EventLog.EventTime) NO-ERROR.  
            
      IF EventLog.UserCode = "MNP"      AND 
         lcFieldName       = "CurrOper" THEN DO:
          
         FIND FIRST MNPProcess NO-LOCK WHERE 
                    MNPProcess.OrderID    = liOrderID      AND 
                    MNPProcess.MNPType    = {&MNP_TYPE_IN} AND 
                    MNPProcess.CreatedTS <= ldeEventTS     NO-ERROR.

         FIND FIRST Order NO-LOCK WHERE 
                    Order.Brand   = gcBrand   AND 
                    Order.OrderID = liOrderID NO-ERROR.

         IF NOT AVAIL MNPProcess OR 
            NOT AVAIL Order      THEN NEXT.

         CREATE ttDumpData.
         ASSIGN
            ttDumpData.MSISDN        = Order.CLI
            ttDumpData.ARECTimeStamp = STRING(MNPProcess.CreatedTS) 
            ttDumpData.ARECType      = MNPProcess.StatusReason
            ttDumpData.WrongOperator = ENTRY(2, EventLog.Datavalues,CHR(255)) 
            ttDumpData.NewOperator   = ENTRY(3, EventLog.Datavalues,CHR(255))
            ttDumpData.ProcTimeStamp = STRING(ldeEventTS). 
      END.

   END.

END FUNCTION.    

/***** Main Start *****/

FIND FIRST DumpFile NO-LOCK WHERE 
           DumpFile.DumpID = iiDumpID NO-ERROR.

IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
      THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter            = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

OUTPUT STREAM sFile TO VALUE(icFile).

EMPTY TEMP-TABLE ttDumpData.

/* YDR-2147 has been deployed on 22-03-2016,
   as a process before running any modified dump we need to 
   run full dump */

IF icDumpMode EQ "Full" THEN 
   idLastDump = 20160322.0000.

REPEAT:
   fSplitTS(idLastDump, 
            OUTPUT ldaDumpDate, 
            OUTPUT liDumpTime).
 
   lcDumpEventTime = STRING(liDumpTime,"HH:MM:SS").

   fDumpArecRejections(ldaDumpDate,
                       lcDumpEventTime).

   ldaDumpDate = ldaDumpDate + 1. 

   IF ldaDumpDate EQ TODAY THEN LEAVE.
END.

FOR EACH ttDumpData NO-LOCK:
   PUT STREAM sFile UNFORMATTED
      ttDumpData.MSISDN        lcDelimiter
      ttDumpData.ARECTimeStamp lcDelimiter
      ttDumpData.ARECType      lcDelimiter
      ttDumpData.WrongOperator lcDelimiter
      ttDumpData.NewOperator   lcDelimiter
      ttDumpData.ProcTimeStamp SKIP.

   oiEvents = oiEvents + 1.    
END.

OUTPUT STREAM sFile CLOSE.

/**** MAIN End ****/
