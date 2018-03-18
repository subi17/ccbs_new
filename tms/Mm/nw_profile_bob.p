
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

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

{Syst/tmsconst.i}
{Func/ftransdir.i}
{Func/cparam2.i}
{Syst/eventlog.i}
{Syst/eventval.i}
{Func/msreqfunc.i}
{Func/orderfunc.i}
{Mc/orderfusion.i}
{Func/fixedlinefunc.i}
{Func/fsubstermreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun 
   {Func/lib/eventlog.i}
   DEF VAR lhOrderFusion AS HANDLE NO-UNDO.
   lhOrderFusion = BUFFER OrderFusion:HANDLE.
   RUN StarEventInitialize(lhOrderFusion).
END.

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
DEF VAR lcReportFileOut AS CHAR NO-UNDO. 
DEF VAR lcOutDir AS CHAR NO-UNDO. 
DEF VAR lcRootDir AS CHAR NO-UNDO. 
DEF VAR liEntries AS INT NO-UNDO. 

/* field variables */
DEF VAR lcMsisdn AS CHAR NO-UNDO. 
DEF VAR lcNwProfile AS CHAR NO-UNDO. 

ASSIGN
   lcRootDir = "/store/riftp/nwprofile/".
 /*  lcRootDir = fCParam("OrderFusionBob","RootDir").*/

IF NOT lcRootDir > "" THEN RETURN.

ASSIGN
/* lcIncDir  = lcRootDir + "incoming/incoming/" 
   lcProcDir = lcRootDir + "incoming/processed/"
   lcSpoolDir = lcRootDir + "outgoing/spool/"
   lcOutDir   = lcRootDir + "outgoing/outgoing/".*/
   lcIncDir  = "incoming/incoming" 
   lcProcDir = "incoming/processed"
   lcSpoolDir = "outgoing/spool" /* record of the wrong files */
   lcOutDir   = "outgoing/outgoing".

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
         lcMsisdn    = ENTRY(1,lcLine,lcSep)
         lcNwProfile = ENTRY(2,lcLine,lcSep) NO-ERROR.

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

   lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir).
   lcProcessedFile = fMove2TransDir(lcInputFile, "", lcProcDir).  
   IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).

END.

INPUT STREAM sFile CLOSE.

IF llDoEvent THEN fCleanEventObjects().


/*
   Procedure updates subscription level profile.
   Subscription can be new or existing one.
*/
PROCEDURE pUpdateProfile:

   DEF INPUT PARAM pcMsisdn AS CHAR NO-UNDO. 
   DEF INPUT PARAM pcNewProfile AS CHAR NO-UNDO.

   DEF VAR cAllProfValues AS CHAR NO-UNDO.
   DEF VAR iReqCnt    AS INT NO-UNDO.
   DEF VAR ldtActDate AS DATE NO-UNDO.
   DEF VAR liActTime  AS INT NO-UNDO.
   DEF VAR liOldValue AS INT  NO-UNDO. 
   DEF VAR liNewValue AS INT  NO-UNDO.
   DEF VAR lcOldParam AS CHAR NO-UNDO.
   DEF VAR lcSMSTxt   AS CHAR NO-UNDO.

/* Kayta funkkaria tsekkiin? */
   /* Gather all profile values from TMSCodes to cAllProfValues
      comma separated list */
   cAllProfValues = "".
   FOR EACH TMSCodes WHERE 
            TMSCodes.TableName = "Customer" AND 
            TMSCodes.FieldName = "NWProfiles" AND
            TMSCodes.CodeGroup = "NWProfile" AND
            TMSCodes.inUse = 1 NO-LOCK:
     IF cAllProfValues = "" THEN
        cAllProfValues = TMSCodes.CodeValue.
     ELSE
        cAllProfValues = cAllProfValues + "," + TMSCodes.CodeValue.
   END.

   IF LOOKUP(pcNewProfile,cAllProfValues,",") = 0 THEN
      RETURN "ERROR:Incorrect profile value parameter".

   /* request is under work */
   /* IF NOT fReqStatus(1,"") THEN RETURN "ERROR:Request already ongoing".*/ 

   FIND MobSub WHERE MobSub.CLI EQ pcMsisdn NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE MobSub THEN
      RETURN "ERROR:MobSub not found".

   Func.Common:mSplitTS(MsRequest.ActStamp,
                        OUTPUT ldtActDate,
                        OUTPUT liActTime).

   FIND MsRequest WHERE MsRequest.MsSeq = MobSub.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 1 OR
                       MsRequest.ReqCparam1 EQ "" THEN 
         RETURN "ERROR:Request not found".

   /* Find "NW" */
   FIND ServCom NO-LOCK WHERE
        ServCom.Brand   = Syst.Var:gcBrand AND
        ServCom.ServCom = MsRequest.ReqCParam1 NO-ERROR.
   IF NOT AVAILABLE ServCom THEN
      RETURN ("ERROR:Unknown service " + MsRequest.ReqCParam1).
   
   /* is component / change allowed for clitype */
   liReqCnt = fServComValue(MobSub.CLIType,
                            MsRequest.ReqCParam1,
                            OUTPUT llAllowed).
   IF liReqCnt = ? AND MsRequest.ReqIParam1 > 0 THEN
      RETURN ("ERROR:Service is not allowed for " + MobSub.CLIType).

   /* UPDATE or CREATE */
   FIND FIRST SubSer NO-LOCK WHERE
              SubSer.MsSeq   = MobSub.MsSeq         AND
              SubSer.ServCom = MsRequest.ReqCParam1 NO-ERROR.
   IF AVAILABLE SubSer THEN DO:         
      IF Subser.SSStat  = MsRequest.ReqIParam1 AND
         SubSer.SSParam = MsRequest.ReqCParam2 
      THEN DO:
         /* accept request as done, but mark that TMS was not updated */
         fReqStatus(2,"Nothing to do").
         RETURN "ERROR:Done nothing to do". /* is it error? */ 
      END.
      /* new request is further in the past than latest one */
      IF SubSer.SSDate > ldtActDate THEN
         RETURN "ERROR:A newer setting exists for service".
      ASSIGN liOldValue = SubSer.SSStat
             lcOldParam = SubSer.SSParam.
   END.

   IF AVAILABLE SubSer AND SubSer.SSDate = ldtActDate THEN DO:
      FIND CURRENT SubSer EXCLUSIVE-LOCK.
   END.
   ELSE DO:
      CREATE SubSer.
      ASSIGN SubSer.MsSeq   = MobSub.MsSeq
             SubSer.ServCom = MsRequest.ReqCParam1
             SubSer.SSDate  = ldtActDate
             Subser.ssParam = MSrequest.ReqCParam2.
   END.
    
   ASSIGN SubSer.SSStat  = INTEGER(pcNewProfile) /* MsRequest.ReqIParam1*/
          liNewValue     = SubSer.SSStat
          SubSer.SSParam = MsRequest.ReqCParam2
          SubSer.ServPac = ""
          llReRate       = FALSE
          lcSMSTxt       = "".

   ldCurrStamp = Func.Common:mMakeTS().

   /* Needed ??? */
   /* check links to other components */
   RUN pCheckServiceLinks (MobSub.MsSeq,
                           SubSer.ServCom,
                           liOldValue,
                           SubSer.SSStat,
                           MsRequest.ActStamp,
                           MsRequest.Salesman,
                           "",
                           MsRequest.ReqSource,
                           MsRequest.MsRequest,
                           FALSE,
                           OUTPUT lcReqChar).
 
   /* mark the list of created subrequests */
   IF lcReqChar > "" THEN DO:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      MsRequest.ReqCParam4 = lcReqChar.
   END.

   RELEASE SubSer.

   /* SMS ??*/
   /* text assigned to request itself has always priority 1 */
   /* IF MsRequest.SMSText > "" THEN lcSMSTxt = MsRequest.SMSText.*/

   /* send SMS if needed */

   RELEASE MobSub.

   /* Create memo ?? */

   /* request handled succesfully */   
   fReqStatus(2,"").

END.

