/* ----------------------------------------------------------------------
  MODULE .......: active_service_dump.p
  TASK .........: Create a dump file from active services
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 26.08.10 
  Version ......: yoigo
---------------------------------------------------------------------- */


{Syst/commali.i}
{Syst/dumpfile_run.i}
{Func/create_eventlog.i}
{Func/timestamp.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric      AS CHAR NO-UNDO.
DEF VAR lcDelimiter    AS CHAR NO-UNDO.
DEF VAR lcOrgID        AS CHAR NO-UNDO.
DEF VAR lcServices     AS CHAR NO-UNDO.

DEF TEMP-TABLE ttPicked NO-UNDO
   FIELD MsSeq AS INT 
   INDEX MsSeq MsSeq.
 
DEF STREAM sLog.

FUNCTION fPick RETURNS LOGICAL
   (iiMsSeq AS INT):

   IF CAN-FIND(FIRST ttPicked WHERE ttPicked.MsSeq = iiMsSeq) THEN
      RETURN FALSE.
   
   CREATE ttPicked.
   ttPicked.MsSeq = iiMsSeq.
   RETURN TRUE.
   
END FUNCTION.


/***** Main start **********/

OUTPUT STREAM sLog TO VALUE(icFile).

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.

lcNumeric = SESSION:NUMERIC-FORMAT.

IF AVAILABLE DumpFile THEN DO:
   lcDelimiter  = fInitDelimiter(DumpFile.DumpDelimiter).
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDelimiter = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

IF icDumpMode = "modified" THEN DO:
   RUN pModifiedSubscriptions. 
END.   

/* full dumps */
ELSE DO:
   RUN pAllSubscriptions.
END.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
OUTPUT STREAM sLog CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

/******** Main end *******/


PROCEDURE pModifiedSubscriptions:

   DEF VAR liReqType  AS INT  NO-UNDO.
   DEF VAR lcTypeList AS CHAR NO-UNDO.
   DEF VAR liCnt      AS INT  NO-UNDO.
   
   lcTypeList = "0,1,10,13,19,35,78". 
   
   DO liCnt = 1 TO NUM-ENTRIES(lcTypeList):
   
      liReqType = INTEGER(ENTRY(liCnt,lcTypeList)).
      
      FOR EACH MsRequest NO-LOCK WHERE
               MsRequest.Brand   = gcBrand   AND
               MsRequest.ReqType = liReqType AND
               MsRequest.ReqStat = 2         AND
               MsRequest.ActStamp > idLastDump
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

         IF RETRY THEN DO:
            olInterrupted = TRUE.
            LEAVE.
         END.

         IF NOT fPick(MsRequest.MsSeq) THEN NEXT.
              
         FIND FIRST MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq 
            NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MobSub THEN NEXT.
      
         RUN pDumpActiveServices(MobSub.MsSeq,
                                 MobSub.AgrCust,
                                 MobSub.CLI).
      END.
   END.

END PROCEDURE.


PROCEDURE pAllSubscriptions:

   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand = gcBrand
   ON QUIT UNDO, RETRY
   ON STOP UNDO, RETRY:

      IF RETRY THEN DO:
         olInterrupted = TRUE.
         LEAVE.
      END.

      RUN pDumpActiveServices(MobSub.MsSeq,
                              MobSub.AgrCust,
                              MobSub.CLI).
   END.

END PROCEDURE.

PROCEDURE pDumpActiveServices:

   DEF INPUT PARAMETER iiMsSeq   AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCli     AS CHAR NO-UNDO.
   
   lcOrgID = "".
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN lcOrgID = Customer.OrgID.

   lcServices = "".
   FOR EACH SubSer NO-LOCK WHERE
            SubSer.MsSeq = iiMsSeq 
            BREAK BY SubSer.ServCom 
                  BY SubSer.SSDate DESC:
            
      /* use newest */
      IF FIRST-OF(SubSer.ServCom) THEN DO:          
         IF SubSer.SSStat <= 0 THEN NEXT.
         lcServices = lcServices + SubSer.ServCom + "=" 
                      + STRING(SubSer.SSStat) + ":".
      END.   
   END.

   PUT STREAM sLog UNFORMATTED
      iiMsSeq      lcDelimiter
      iiCustNum    lcDelimiter
      icCLI        lcDelimiter
      lcOrgID      lcDelimiter
      lcServices   SKIP.
      
   oiEvents = oiEvents + 1.
END.
