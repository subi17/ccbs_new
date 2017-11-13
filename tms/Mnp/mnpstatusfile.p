/* ----------------------------------------------------------------------
  MODULE .......: mnpstatusfile.p 
  TASK .........: To check and update MNP process statuses
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 01/2010 
  Version ......: yoigo
----------------------------------------------------------------------- */
{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Syst/eventval.i}
{Func/email.i}
{Func/msisdn.i}
{Func/msisdn_prefix.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMNPProcess AS HANDLE NO-UNDO.
   lhMNPProcess = BUFFER MNPProcess:HANDLE.

END.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcCli AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcNCStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRefCode AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcNCTime AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcPortTime AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldeNCTime AS DECIMAL NO-UNDO. 
DEFINE VARIABLE lcDonor AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcRecipient AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT ";".

/* files and dirs */
DEFINE VARIABLE lcRootDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER EXTENT 3 NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcLogFile AS CHAR NO-UNDO. 
DEFINE VARIABLE liMNPSeq AS INTEGER NO-UNDO.
DEFINE VARIABLE lcTimeValue AS CHARACTER NO-UNDO.
DEF VAR liMSISDNStatus AS INT NO-UNDO. 
DEFINE VARIABLE lii AS INTEGER NO-UNDO.

/* counters */
DEFINE VARIABLE liNumOK AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNumErr AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSkipped AS INTEGER NO-UNDO. 

DEFINE VARIABLE liErrorSkips AS INTEGER NO-UNDO. 

ASSIGN
   lcRootDir = fCParam("MNP","StatusFileRootDir") 
   lcIncDir = lcRootDir + "incoming/incoming/". 
   lcProcDir = lcRootDir + "incoming/processed/".
   lcSpoolDir = lcRootDir + "outgoing/spool/".
   lcOutDir = lcRootDir + "outgoing/outgoing/".
   lcConfDir = fCParamC("RepConfDir").

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.
DEF STREAM sLogAPOR.
DEF STREAM sLogASOL.

DEFINE VARIABLE lhsLog AS HANDLE EXTENT 3 NO-UNDO.

ASSIGN
   lhsLog[1] = STREAM sLog:HANDLE
   lhsLog[2] = STREAM sLogAPOR:HANDLE
   lhsLog[3] = STREAM sLogASOL:HANDLE.

DEF STREAM sMSISDN.

/* temporary logging (YTS-3289) */
OUTPUT STREAM sMSISDN TO VALUE(lcRootDir + "msisdn_status_change.log") APPEND.


FUNCTION fLog RETURNS LOGICAL
   (ihStream AS HANDLE,
    icMessage AS CHAR):

   IF ihStream:PRIVATE-DATA BEGINS "!"
   THEN DO:
      ihStream:PRIVATE-DATA = SUBSTRING(ihStream:PRIVATE-DATA,2).
      OUTPUT STREAM-HANDLE ihStream TO VALUE(ihStream:PRIVATE-DATA).
   END.

   PUT STREAM-HANDLE ihStream UNFORMATTED
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGICAL
   (icMessage AS CHAR):

   fLog(lhsLog[1], lcLine + "|" + icMessage).

END FUNCTION.

FUNCTION fCharToDate RETURNS DATE
   (icCharDate AS CHARACTER):

   DEFINE VARIABLE lcc AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lda AS DATE      NO-UNDO.
   lcc = ENTRY(1,icCharDate,"/").

   ASSIGN
      lda = DATE(INTEGER(SUBSTRING(lcc,5,2)),
                 INTEGER(SUBSTRING(lcc,7,2)),
                 INTEGER(SUBSTRING(lcc,1,4)))
      NO-ERROR.

   IF ERROR-STATUS:ERROR
   THEN RETURN ?.

   RETURN lda.

END FUNCTION.

FUNCTION fErrorMulti RETURNS LOGICAL
   (icMessage  AS CHAR,
    icTime     AS CHAR,
    icNCStatus AS CHAR,
    icDonor    AS CHAR,
    icPortTime   AS CHAR):

   fLog(lhsLog[1], lcLine + "|" + icMessage).

   IF icMessage = "ERROR: MNP process not found" AND
      icDonor   = "005"
   THEN DO:
      IF icTime      = "0800" AND
         icNCStatus  = "APOR" AND
         fCharToDate(icPortTime) EQ TODAY
      THEN fLog(lhsLog[2], lcLine + "|" + icMessage).

      ELSE IF LOOKUP(icTime, "0800,1400") > 0 AND
              icNCStatus = "ASOL"
      THEN fLog(lhsLog[3], lcLine + "|" + icMessage).
   END.

END FUNCTION.

IF llDoEvent THEN RUN StarEventInitialize(lhMNPProcess).

DEFINE VARIABLE ldeTime AS DECIMAL NO-UNDO. 

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1 " + lcIncDir + "*.dat").
REPEAT:
   
   ASSIGN
      liSkipped = 0
      liNumOk = 0
      liNumErr = 0
      liErrorSkips = 0.

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcFileName.
   
   ldeTime = int(entry(2,lcFileName,"_")) + (int(substring(entry(3,lcFileName,"_"),1,2)) * 0.036) no-error.
   if error-status:error or ldeTime = ? or ldeTime = 0 then next.

   /* 5 minute buffer */
   ldeTime = ldeTime - 0.00300.

   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   ASSIGN
      lcTimeValue = SUBSTRING(ENTRY(3,lcFileName,"_"),1,4)
      lcLogFile = lcSpoolDir + entry(num-entries(lcInputFile,"/"), lcInputFile, "/").
   
   fBatchLog("START", lcInputFile).
   
   DO lii = 1 TO 3:
      lhsLog[lii]:PRIVATE-DATA = SUBSTITUTE("!&1&2.log",lcLogFile,ENTRY(lii,",_APOR,_ASOL")).
   END.

   fLog(lhsLog[1],
        lcFilename + " " +
        STRING(TODAY,"99.99.99") + " " +
        STRING(TIME,"hh:mm:ss")).
   
   IMPORT STREAM sin UNFORMATTED lcLine. /* skip header line */
  
   LINE_LOOP:
   REPEAT:

      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.
   
      ASSIGN
         lcRefCode = ENTRY(1,lcLine,lcSep)
         lcNCStatus = ENTRY(2,lcLine,lcSep)
         lcNCTime = ENTRY(3,lcLine,lcSep)
         lcPortTime = ENTRY(5,lcline,lcSep)
         lcCLI = ENTRY(6,lcLine,lcSep)
         lcDonor = ENTRY(8,lcLine,lcSep)
         lcRecipient = ENTRY(9,lcLine,lcSep) NO-ERROR.

      IF ERROR-STATUS:ERROR THEN DO:
         fError("ERROR: Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      /* not yoigos case */
      IF lcDonor NE "005" AND lcRecipient NE "005" THEN DO:
         liSkipped = liSkipped + 1.
         NEXT.
      END.
      
      IF lcRecipient EQ "005" AND LOOKUP(lcNCStatus,"BNOT,BDEF,BDET,BFIN,BCAN") > 0 
         THEN DO:
         liSkipped = liSkipped + 1.
         NEXT.
      END.
      
      ldeNCTime = ?.
      ldeNCTime = Func.Common:mHMS2TS(date(int(substring(lcNCTime,5,2)),
          int(substring(lcNCTime,7,2)),
          int(substring(lcNCTime,1,4))),
          substring(lcNCTime,10)) NO-ERROR.
      
      IF ERROR-STATUS:ERROR THEN DO:
         fError("ERROR: Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      RUN pMNPStatusCheck(lcRefCode,
                        lcNCStatus,
                        lcCLI,
                        ldeNCTime).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         if RETURN-VALUE BEGINS "ERROR: current status is" THEN DO:
            liErrorSkips = liErrorSkips + 1.
         END.
         ELSE DO:
            fErrorMulti(RETURN-VALUE, lcTimeValue, lcNCStatus, lcDonor, lcPortTime).
            liNumErr = liNumErr + 1 .
         END.
      END.
      ELSE DO:
   /*      fLogLine("OK").  */
         IF RETURN-VALUE EQ "SKIPPED" THEN liSkipped = liSkipped + 1.
         ELSE liNumOK = liNumOK + 1 .
      END.
   END.

   fLog(lhsLog[1],
        "total: " + STRING(liSkipped + liNumOK + liNumErr + liErrorSkips) + ", " +
        "ok: " + STRING(liNumOK) + ", " +
        "skipped (not yoigo): " + STRING(liSkipped) + ", " +
        "errors: " + STRING(liNumErr) + ", " +
        "status errors skipped: " + STRING(liErrorSkips)).

   INPUT STREAM sin CLOSE.

   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 

   DO lii = 1 TO 3:
      IF NOT lhsLog[lii]:PRIVATE-DATA BEGINS "!"
      THEN DO:
         OUTPUT STREAM-HANDLE lhsLog[lii] CLOSE.
         lcReportFileOut[lii] = fMove2TransDir(lhsLog[lii]:PRIVATE-DATA, "", lcOutDir).

         IF liNumErr > 0 THEN DO:
            /* mail recipients */
            GetRecipients(lcConfDir + SUBSTITUTE("mnpstatusfile&1.email",ENTRY(lii,",_apor,_asol"))).
            /* send via mail */
            SendMail(lcReportFileOut[lii],"").
         END.
      END.
   END.

   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
END.

INPUT STREAM sFile CLOSE.
OUTPUT STREAM sMSISDN CLOSE.
IF llDoEvent THEN fCleanEventObjects().


PROCEDURE pMNPStatusCheck:

   DEF INPUT PARAMETER icRefCode AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icNCStatus AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icCli AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideNCTime AS DEC NO-UNDO.

   DEF BUFFER bMNPSub FOR MNPSub.

   DEFINE VARIABLE liNCStatus AS INTEGER NO-UNDO. 
   DEFINE VARIABLE lcTMSStatus AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liTMSStatus AS INTEGER NO-UNDO.

   FIND MNPProcess NO-LOCK WHERE 
        MNPProcess.PortRequest = icRefCode NO-ERROR.
   IF NOT AVAIL MNPProcess THEN RETURN "ERROR: MNP process not found".

   IF MNPProcess.StatusCode = {&MNP_ST_AREC_CLOSED} THEN
      liTMSStatus = {&MNP_ST_AREC}.
   ELSE liTMSStatus = MNPProcess.StatusCode.

   /* Convert TMS status to character value */
   FIND TMSCodes WHERE 
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeValue = STRING(MNPProcess.StatusCode)
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN lcTMSStatus = TMSCodes.CodeName.
   ELSE lcTMSStatus = STRING(MNPProcess.StatusCode).
   
   /* Convert NC status to integer value */
   FIND TMSCodes WHERE 
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeName = icNCStatus
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN liNCStatus = INT(TMSCodes.CodeValue).
   ELSE liNCStatus = -1.

   /* Set Ported status for MNP OUT processes without subscriptions */
   IF MNPProcess.MNPType = {&MNP_TYPE_OUT} AND
      MNPProcess.StatusCode = {&MNP_ST_ACON} AND
      icNCStatus = "APOR" THEN DO:
      
      FOR EACH MNPSub WHERE
               MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK:
         FIND MobSub WHERE
              MobSub.MsSeq = MNPSub.MsSeq NO-LOCK NO-ERROR.
         IF AVAIL MobSub THEN RETURN
            "ERROR: subscription exists: " + STRING(MNPSub.MsSeq).
      END.
         
      FIND CURRENT MNPProcess EXCLUSIVE-LOCK.
      
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMNPProcess).
      
      ASSIGN 
         MNPProcess.UpdateTS = Func.Common:mMakeTS()
         MNPProcess.MNPUpdateTS = ideNCTime
         MNPProcess.StatusCode = {&MNP_ST_APOR}
         liMNPSeq = MNPProcess.MNPSeq.
      
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMNPProcess).
      
      RELEASE MNPProcess.
      
      FOR EACH MNPSub WHERE
               MNPSub.MNPSeq = liMNPSeq NO-LOCK:
         
         /* cancel possible number return process that is on hold */ 
         FOR EACH bMNPSub WHERE
                  bMNPSub.CLI = MNPSub.CLI NO-LOCK,
            FIRST MNPProcess WHERE
               MNPProcess.MNPSeq = bMNPSub.MNPSeq AND
               MNPProcess.MNPType = ({&MNP_TYPE_TERMINATION}) AND
               (MNPProcess.StatusCode = ({&MNP_ST_BDET}) OR
                MNPProcess.StatusCode = ({&MNP_ST_BNOT})) EXCLUSIVE-LOCK:
      
            FIND msisdn where
               msisdn.brand = Syst.Var:gcBrand and
               msisdn.cli = bMNPSub.CLI  AND
               msisdn.statuscode = ({&MSISDN_ST_RETURN_NOTICE_SENT}) AND
               msisdn.validto > Func.Common:mMakeTS() NO-LOCK NO-ERROR.

            IF AVAIL msisdn THEN DO:
            
               liMSISDNStatus = (IF fIsYoigoCLI(msisdn.CLI) THEN 
                     {&MSISDN_ST_MNP_OUT_YOIGO} ELSE
                     {&MSISDN_ST_MNP_OUT_OTHER}).
      
               fMakeMsidnHistory(RECID(MSISDN)).
               MSISDN.StatusCode = liMSISDNStatus.

               IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMNPProcess).
               
               ASSIGN
                  MNPProcess.UpdateTS = Func.Common:mMakeTS()
                  MNPProcess.StatusCode = ({&MNP_ST_BCAN}).
      
               IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMNPProcess).

               RELEASE msisdn.
               
            END.
         END.
            
         FIND FIRST msisdn where
                    msisdn.brand = Syst.Var:gcBrand and
                    msisdn.cli = MNPSub.CLI
         NO-LOCK USE-INDEX CLI NO-ERROR.

         /* handle possible scenario where subscription is manually
            terminated during the mnp out process. YTS-3289 */
         IF AVAIL msisdn AND
                 (msisdn.statuscode EQ {&MSISDN_ST_AVAILABLE} OR
                  msisdn.statuscode EQ {&MSISDN_ST_QUARANTINE} OR
                  msisdn.statuscode EQ {&MSISDN_ST_WAITING_RETURN}) THEN DO:
            
            PUT STREAM sMSISDN UNFORMATTED 
               Func.Common:mTS2HMS(Func.Common:mMakeTS()) "|"
               msisdn.cli "|"
               MNPSub.mnpseq "|"
               msisdn.statuscode skip.

            fMakeMsidnHistory(recid(msisdn)).

            liMSISDNStatus = (IF fIsYoigoCLI(MNPSub.CLI) THEN 
                  {&MSISDN_ST_MNP_OUT_YOIGO} ELSE
                  {&MSISDN_ST_MNP_OUT_OTHER}).
         
            assign
               msisdn.custnum = 0
               msisdn.statuscode = liMSISDNStatus
               msisdn.outoperator = MNPSub.NRN.
            release msisdn.

         END.
         
         /* fLogLine("UPDATE: finalize mnp out"). */

      END.
   END. 

   /* Finalize Number termination process  */
   ELSE IF MNPProcess.StatusCode EQ ({&MNP_ST_BNOT}) AND
           icNCStatus = "BDEF" THEN DO:

      FIND MNPSub WHERE
           MNPSub.MNPSeq = MNPProcess.MNPSeq NO-LOCK.

      FIND msisdn where
         msisdn.brand = Syst.Var:gcBrand and
         msisdn.cli = MNPSub.CLI  AND
         msisdn.validto > Func.Common:mMakeTS() NO-LOCK NO-ERROR.

      IF NOT AVAIL msisdn OR 
         msisdn.statuscode NE ({&MSISDN_ST_RETURN_NOTICE_SENT}) THEN DO:
         RETURN "ERROR: Incorrect MSISDN status".
      END.
      
      FIND CURRENT MNPProcess EXCLUSIVE-LOCK.
      
      fMakeMsidnHistory(RECID(MSISDN)).
      MSISDN.StatusCode = ({&MSISDN_ST_RETURNED}).
      RELEASE Msisdn.
      
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMNPProcess).
      
      ASSIGN 
         MNPProcess.UpdateTS = Func.Common:mMakeTS()
         MNPProcess.MNPUpdateTS = ideNCTime
         MNPProcess.StatusCode = ({&MNP_ST_BDEF}).

      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMNPProcess).

      /* fLogLine("UPDATE: finalize number termination"). */
      
   END.
   ELSE IF liTMSStatus NE liNCStatus THEN DO:
      IF MNPProcess.updatets > ldeTime THEN 
      RETURN "ERROR: current status is " + lcTMSStatus.
      ELSE 
      RETURN "ERROR: status is " + lcTMSStatus.
   END.
      
   RELEASE MNPProcess.

   RETURN "".

END.
