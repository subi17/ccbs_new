/* ----------------------------------------------------------------------
  MODULE .......: plcrequest.p 
  TASK .........: Handles prepaid lifecycle requests (type 75)
  APPLICATION ..: TMS
  AUTHOR .......: as 
  CREATED ......: 03/2009
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */

{commali.i}
{msreqfunc.i}
{fsubstermreq.i}
{msisdn_prefix.i}

DEFINE INPUT PARAMETER iiMSrequest AS INT NO-UNDO.

DEFINE VARIABLE lcOrigCDRType AS CHAR NO-UNDO.
DEFINE VARIABLE llYoigoCLI  AS LOG     NO-UNDO.
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liReq AS INTEGER NO-UNDO.

DEFINE VARIABLE liMsisdnStat AS INTEGER NO-UNDO.
DEFINE VARIABLE liSimStat AS INTEGER NO-UNDO.  
DEFINE VARIABLE liQuarTime AS INTEGER NO-UNDO.  
DEFINE VARIABLE llPenalty AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liOrderer AS INTEGER NO-UNDO INIT 6.

DEF BUFFER bMsRequest FOR MSRequest.

FIND MSRequest WHERE 
     MSRequest.MSRequest = iiMSRequest
EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 75 THEN DO:
   RETURN.
END.

/* request is under work */
IF NOT fReqStatus(1,"") THEN DO:
   RETURN.
END.

ASSIGN
   lcOrigCDRType = MsRequest.ReqCParam1.

FIND MobSub NO-LOCK WHERE
   MobSub.Cli = MsRequest.Cli NO-ERROR.

IF NOT AVAIL MobSub THEN DO:
   fReqError(SUBST("Subscription &1 doesn't exist", MsRequest.Cli)).
   RETURN.
END.

ASSIGN MsRequest.MsSeq = MobSub.MsSeq.

IF CAN-FIND(FIRST bMsRequest WHERE
              bMsRequest.MsSeq = MobSub.MsSeq AND
              bMsRequest.ReqType = 75 AND
              bMsRequest.ReqStatus NE 2 AND
              bMsRequest.ReqStatus NE 4 AND
              bMsRequest.ReqStatus NE 9 AND
              ROWID(bMsRequest) NE ROWID(MsRequest)) THEN DO:
   fReqStatus(9, "Subscription is already terminating").
   RETURN.
END.

IF CAN-FIND(FIRST bMsRequest WHERE
              bMsRequest.MsSeq = MobSub.MsSeq AND
              bMsRequest.ReqType = 18 AND
              bMsRequest.ReqStatus NE 2 AND
              bMsRequest.ReqStatus NE 4) THEN DO:
   fReqStatus(9, "Subscription is already terminating").
   RETURN.
END.

IF MobSub.PayType = FALSE THEN DO:
   fReqError("Wrong subscription type: " + MobSub.CliType). 
   RETURN.
END.

IF lcOrigCDRType NE "LCC" THEN DO:
   fReqError("Unknown Original CDR Type: " + lcOrigCDRType). 
   RETURN.
END.

IF fDeleteMsValidation(MobSub.MsSeq, OUTPUT lcError) > 0 THEN DO:
   fReqError(lcError). 
   RETURN.
END.

fInitialiseValues(
   liOrderer,
   llYoigoCLI,
   OUTPUT liMSISDNStat,
   OUTPUT liSimStat,
   OUTPUT liQuarTime).

llYoigoCLI = fIsYoigoCLI(MobSub.CLI).            
llPenalty  = fIsPenalty(liOrderer, MobSub.MsSeq).

liReq = fTerminationRequest(
   MobSub.MSSeq,
   fMakeTS(),
   liMsisdnStat,
   liSimStat,
   liQuarTime,
   INT(llPenalty),
   "",
   STRING(liOrderer),
   "10", /* prepaid life cycle cdr */
   katun,
   MsRequest.MsRequest,
   OUTPUT lcError).

/* Creation of subrequests failed, "fail" master request too */
IF liReq = 0 OR liReq = ? THEN DO:
   fReqError(lcError).
   RETURN.
END.

fReqStatus(2,""). /* request handled succesfully */
