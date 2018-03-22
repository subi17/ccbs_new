/* ----------------------------------------------------------------------
  MODULE .......: nw_profile_bob.p
  TASK .........: Back door tools: Automate network profile changes.
                  Changes on subscription level.
                  RES-885 National Roaming traffic restrictions.
  APPLICATION ..: TMS
  AUTHOR .......: kahannul
  CREATED ......: 08.03.18
  Version ......: Yoigo
----------------------------------------------------------------------- */
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Syst/eventval.i}
{Func/fmakemsreq.i}

DEF VAR lcLine AS CHAR NO-UNDO.
DEF VAR lcSep AS CHAR NO-UNDO INIT ";".
DEF VAR liNumOK AS INT NO-UNDO. 
DEF VAR liNumErr AS INT NO-UNDO. 

/* files and dirs */
DEF VAR lcLogFile AS CHAR NO-UNDO. 
DEF VAR lcFileName AS CHAR NO-UNDO. 
DEF VAR lcIncDir  AS CHAR NO-UNDO. 
DEF VAR lcInputFile AS CHAR NO-UNDO. 
DEF VAR lcProcDir AS CHAR NO-UNDO. 
DEF VAR lcProcessedFile AS CHAR NO-UNDO. 
DEF VAR lcSpoolDir AS CHAR NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR liEntries AS INT NO-UNDO. 

/* field variables */
DEF VAR lcMsisdn AS CHAR NO-UNDO. 
DEF VAR lcNwProfile AS CHAR NO-UNDO. 

ASSIGN
   lcRootDir = fCParam("NWProfileBob","RootDir").

IF NOT lcRootDir > "" THEN RETURN.

ASSIGN
   lcIncDir  = lcRootDir + "incoming/" 
   lcProcDir = lcRootDir + "processed/"
   lcSpoolDir = lcRootDir + "spool/"
   lcOutDir   = lcRootDir + "outgoing/".

DEF STREAM sin.
DEF STREAM sFile.
DEF STREAM sLog.

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine lcSep 
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
   
   IF SEARCH(lcInputFile) NE ? THEN 
      INPUT STREAM sin FROM VALUE(lcInputFile).
   ELSE NEXT.

   ASSIGN
      liNumOk = 0
      liNumErr = 0.
   
   fBatchLog("START", lcInputFile).
   lcLogFile = lcSpoolDir + lcFileName + ".LOG".
   OUTPUT STREAM sLog TO VALUE(lcLogFile) append.

   PUT STREAM sLog UNFORMATTED
              lcFilename  " "
              STRING(TODAY,"99.99.99") " "
              STRING(TIME,"hh:mm:ss") SKIP.
  
   LINE_LOOP:
   REPEAT:
      
      IMPORT STREAM sin UNFORMATTED lcLine.
      IF TRIM(lcLine) EQ "" THEN NEXT.

      ASSIGN 
         liEntries   = NUM-ENTRIES(lcLine,lcSep)
         lcMsisdn    = TRIM(ENTRY(1,lcLine,lcSep))
         lcNwProfile = TRIM(ENTRY(2,lcLine,lcSep)) NO-ERROR.

      IF ERROR-STATUS:ERROR OR liEntries NE 2 THEN DO:
         fError("Incorrect input data format").
         liNumErr = liNumErr + 1 .
         NEXT.
      END.

      RUN pUpdateProfile(lcMsisdn,
                         lcNwProfile).

      IF RETURN-VALUE BEGINS "ERROR" THEN DO:
         fError(ENTRY(2,RETURN-VALUE,":")).
         liNumErr = liNumErr + 1 .
      END.
      ELSE liNumOK = liNumOK + 1 .
   END.
  
   PUT STREAM sLog UNFORMATTED 
       "input: " STRING(liNumOK + liNumErr) ", "
       "updated: " STRING(liNumOK) ", "
       "errors: " STRING(liNumErr) SKIP.
   
   INPUT STREAM sin CLOSE.
   OUTPUT STREAM sLog CLOSE.

   fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).  
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END.

INPUT STREAM sFile CLOSE.

/*
   Procedure updates subscription level profile.
   Subscription can be new or existing one.
*/
PROCEDURE pUpdateProfile:

   DEF INPUT PARAM pcMsisdn AS CHAR NO-UNDO. 
   DEF INPUT PARAM pcNewProfile AS CHAR NO-UNDO.

   DEF VAR lcError    AS CHAR NO-UNDO.
   DEF VAR liReq      AS INT  NO-UNDO.

   IF NOT Func.Common:mTMSCodeChk("Customer","NWProfiles",pcNewProfile) THEN
      RETURN "ERROR:Incorrect subscription profile value parameter".

   /* request is under work */
   /* IF NOT fReqStatus(1,"") THEN RETURN "ERROR:Request already ongoing".*/ 

   FIND MobSub WHERE MobSub.CLI EQ pcMsisdn NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN
      RETURN "ERROR:MobSub not found".

   /* CREATE SubSer */
   liReq = fServiceRequest(MobSub.MsSeq,
                           "NW", /* Service */
                           INTEGER(pcNewProfile), /* profile */
                           "",
                           Func.Common:mMakeTS(),
                           "",
                           TRUE,       /* fees */
                           FALSE,      /* sms */
                           "",
                           {&REQUEST_SOURCE_SCRIPT},
                           0, /* father request */
                           FALSE, /* mandatory for father request */
                           OUTPUT lcError).

   IF liReq = 0 THEN
      RETURN SUBST("ERROR:Service (NW) request not created: &1", lcError).

   RETURN "OK".
END.

