/* ----------------------------------------------------------------------
  MODULE .......: read_topup_fatime.p
  TASK .........: cron add topup of fatime
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{ftaxdata.i}
{ftopup.i}

{eventlog.i}
{tmsconst.i}
{ftransdir.i}
{cparam2.i}
{eventval.i}
{email.i}

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO INIT ";".
DEFINE VARIABLE lcError AS CHAR NO-UNDO. 
DEFINE VARIABLE lcLogFile AS CHAR NO-UNDO. 

/* files and dirs */
DEFINE VARIABLE lcFileName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcIncDir  AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInputFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSpoolDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcConfDir AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liNumOK AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNumErr AS INTEGER NO-UNDO. 

/* field variables */
DEFINE VARIABLE lcType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCLI AS CHARACTER NO-UNDO.
DEFINE VARIABLE liMsSeq AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldAmt AS DECIMAL NO-UNDO.
DEFINE VARIABLE liPeriod AS INT NO-UNDO.
DEFINE VARIABLE lcKey  AS CHARACTER NO-UNDO. 

ASSIGN
   lcIncDir   = fCParam("TopupFat","IncomingDir") 
   lcProcDir  = fCParam("TopupFat","IncProcDir")
   lcSpoolDir = fCParam("TopupFat","OutSpoolDir")
   lcOutDir   = fCParam("TopupFat","OutDir")
   lcConfDir  = fCParamC("RepConfDir").

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.
      
END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).
END FUNCTION.

/* File reading and parsing */
INPUT STREAM sFile THROUGH VALUE("ls -1tr " + lcIncDir).
REPEAT:

   IMPORT STREAM sFile UNFORMATTED lcFileName.
 
   lcInputFile = lcIncDir + lcFileName.
   
   IF SEARCH(lcInputFile) NE ? THEN DO:
      IF fCheckFileNameChars(lcFileName) EQ FALSE THEN NEXT.
      INPUT STREAM sin FROM VALUE(lcInputFile).
   END.
   ELSE NEXT.
   
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".log".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
  
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF lcLine EQ "" THEN NEXT.

      ASSIGN 
         lcType       = ENTRY(1,lcLine,lcSep)
         lcCLI        = ENTRY(2,lcLine,lcSep)         
         liMsSeq      = INTEGER(ENTRY(3,lcLine,lcSep))
         ldAmt        = DECIMAL(REPLACE(ENTRY(4,lcLine,lcSep),",",".")) 
         liPeriod     = INTEGER(ENTRY(5,lcLine,lcSep))       
         lcKey        = ENTRY(6,lcLine,lcSep)
         NO-ERROR.
 
      IF ERROR-STATUS:ERROR THEN DO:
         fError("Wrong file format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      /* check MsSeq and CLI */
      FIND MobSub  WHERE 
           MobSub.MsSeq = liMsSeq AND 
           MobSub.CLI = lcCLI  NO-LOCK NO-ERROR. 
      IF NOT AVAIL MobSub THEN DO:
         fError("Not found MobSub with MSISDN and Subscription ID").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      /* check amount is not cero */
      IF ldAmt = 0 THEN DO:
         fError("Amount is zero").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      CASE lcType:
         WHEN "F" THEN RUN pCreateFatime.
         WHEN "T" THEN RUN pCreateTopup.
         OTHERWISE DO:
            fError("Wrong Type").
            liNumErr = liNumErr + 1 .
            NEXT.
         END.
      END CASE.
     

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE DO:
         fLogLine("OK").
         liNumOK = liNumOK + 1 .
      END.
   END.
  
   PUT STREAM sLog UNFORMATTED 
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir). 
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.
   
   /*
   IF liNumErr > 0 THEN DO:
      /* mail recipients */
      GetRecipients(lcConfDir + "topup_fatime.email").
      /* send via mail */
      SendMail(lcReportFileOut,"").
   END.
   */

END.

INPUT STREAM sFile CLOSE.


PROCEDURE pCreateFatime :

   DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

   FIND FIRST MsOwner USE-INDEX cli_s where
              MsOwner.cli = lcCLI and
              msowner.paytype = false no-lock no-error.

   if not available msowner then
      if can-find(first msowner where msowner.cli = lcCLI) then 
         RETURN "ERROR:Incorrect payment type".
      else 
         RETURN "ERROR:Unknown MSISDN".
      
   run creafat (msowner.invcust,
                msowner.msseq,
                lcKey, /* FatGroup */
                ldAmt,
                0,
                ?,
                liPeriod,
                999999,
                OUTPUT lcError).
   if lcError > "" then
      RETURN "ERROR:FATime could not be created, " + lcError.

  RETURN "".
END PROCEDURE.

PROCEDURE pCreateTopUp :

   DEFINE VARIABLE lcTaxZone AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 

   find first msowner use-index cli_s where
              msowner.cli = lccli and
              msowner.paytype = true no-lock no-error.
   if not available msowner then
      if can-find(first msowner where msowner.cli = lccli) then
         RETURN "ERROR:Incorrect payment type". 
      else 
         RETURN "ERROR:Unknown MSISDN".
       
   IF NOT CAN-FIND(FIRST TMSCodes WHERE
                         TMSCodes.TableName = "PrepaidRequest" AND
                         TMSCodes.FieldName = "PPReqPrefix" AND
                         TMSCodes.CodeValue = lcKey) THEN 
     RETURN "ERROR:Invalid prefix".
        
   find first customer where customer.custnum = msowner.invcust no-lock.
   lcTaxZone = fRegionTaxZone(Customer.Region).
      
   liRequest = fCreateTopUpRequest(msowner.msseq,
                                   msowner.cli,
                                   "RefillTRequest",
                                   "COMP",
                                   "RefillTRequest",
                                   lcKey, /* Prefix */
                                   "Cron script",
                                   lcTaxZone,
                                   0,
                                   ldAmt * 100,
                                   0.0).
   
   if liRequest = 0 then
      RETURN "ERROR:Request creation failed".

RETURN "".
END PROCEDURE.


