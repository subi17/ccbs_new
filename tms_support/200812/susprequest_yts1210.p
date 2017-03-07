
/* ----------------------------------------------------------------------
  MODULE .......: susprequest.p 
  TASK .........:
  APPLICATION ..: TMS
  AUTHOR .......: jukka 
  CREATED ......: 30.10.07
  CHANGED ......: 05.02.08 "pending main request" as a part of new
                           barring handling
  Version ......: xfera
----------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/msreqfunc.i}
{Func/fmakemsreq.i}

DEFINE INPUT PARAMETER iiRequest AS INTEGER   NO-UNDO.
DEF VAR ideActStamp     AS DEC  NO-UNDO FORMAT "99999999.99999".
DEF VAR liReq           AS INT  NO-UNDO.
DEF VAR ocResult        AS CHAR NO-UNDO.
DEF VAR lcValidate      AS CHAR NO-UNDO.
DEF VAR liMasterRequest AS INT  NO-UNDO.

/*For servicerequest handling */
DEF BUFFER bMsRequest FOR MsRequest. 

/* For Checking mandatory unbarring */
DEF BUFFER bMsReq     FOR MsRequest.


/* Valid suspend functions, USER TMSCodes later */

FIND MsRequest WHERE
     MsRequest.MsRequest = iiRequest AND
     MsRequest.Brand     = gcBrand
NO-LOCK NO-ERROR.

IF NOT AVAIL MsRequest THEN DO:
   fReqError("ERROR, request lost!"). 
   RETURN.
END.

/* Store Master request request Id to be used in subrequest */
liMasterRequest = MsRequest.MsRequest.

FIND FIRST Mobsub WHERE
           Mobsub.MSSeq = MSRequest.MsSeq NO-LOCK NO-ERROR.

IF NOT AVAILABLE MobSub THEN DO:
   fReqError("MobSub not found"). 
   RETURN.
END.

/* note: ReqIParam3 and ReqIParam4 are in use for fraud tool limits */

/* Request handling by status: */


CASE MsRequest.ReqStat:
   /* New request, create subrequest for servicepac */
   WHEN 3 THEN DO:
      /* Request cannot be handled until barring removal has been 
      handled */
      IF MsRequest.ReqIParam2 > 0 THEN DO: 
         /* Is this mandatory for next barring package to be activated */
         FIND bMsReq NO-LOCK WHERE
              bMsReq.MsRequest = MsRequest.ReqIParam2
         NO-ERROR.
         
         CASE bMsReq.ReqStat:
          
            WHEN 2 THEN DO:
               /* Mandatory unbarring complete, run pending */
               RUN pNew(MsRequest.ReqCParam1,
                        TODAY,
                        fMakeTs(),
                        /*MobSub.CliType*/ "CONTRD1",
                        MobSub.MsSeq).

               RETURN.
            END.
            WHEN 3 THEN DO:
               fReqStatus(3,"Package change failed see requests "  + 
                             STRING(MsRequest.MsRequest) + " and " +
                             STRING(MsRequest.ReqIParam2)).
               /* Unarring failed, discontinue further handling */
            END.
            OTHERWISE RETURN. /* Handling not done yet */
         END.
      END.
      /* No mandatory requests, handle immediately */ 
      ELSE RUN pNew(MsRequest.ReqCParam1, /*ServPac*/
                        TODAY,
                        fMakeTs(),
                        /*MobSub.CliType*/ "CONTRD1",
                        MobSub.MsSeq).
   END.
   /* Done */
   /*************************************************************
   WHEN 3 THEN DO:
      /* Do check on failed temporary sub-requests */
      CASE fGetSubRequestState(MsRequest.MsRequest):
         WHEN 3 THEN fReqStatus(3,"Handling of subrequests failed!").
         WHEN 2 THEN fReqStatus(2,"").
      END.
   END. 
   *************************************************************/
   WHEN 7 OR WHEN 8 THEN RUN pDone.

END.

PROCEDURE pNew:
    
   DEFINE INPUT PARAMETER icServPac  AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER idtDate    AS DATE      NO-UNDO.
   DEFINE INPUT PARAMETER ideActTime AS DECIMAL   NO-UNDO.
   DEFINE INPUT PARAMETER icCliType  AS CHAR      NO-UNDO.
   DEFINE INPUT PARAMETER iiMsSeq    AS INTEGER   NO-UNDO. 
   
   IF idtDate = TODAY THEN ideActTime = fMakeTS().
   ELSE ideActTime = fMake2Dt(idtDate,10800).
   
   /* Package from MsRequest */
   FIND FIRST CTServPac NO-LOCK WHERE
              CTServPac.Brand     = gcBrand   AND
              CTServPac.CLIType   = icCLIType AND
              CTServPac.ServPac   = icServPac AND
              CTServPac.FromDate <= idtDate   AND
              CTServPac.ToDate   >= idtDate
   NO-ERROR.
   
   IF NOT AVAIL CtServPac THEN DO:
      fReqError("ERROR, Unknown servpac!"). 
      RETURN.
   END.

   /* Now create components from service pac */
   
   /* Assign master request (35) to 1 */
   IF NOT fReqStatus(1,"") THEN RETURN.
    
   FOR EACH CTServEl NO-LOCK WHERE
            CTServEl.Brand     = gcBrand            AND
            CTServEl.CLIType   = CTServPac.CLIType  AND
            CTServEl.ServPac   = CTServPac.ServPac  AND
            CTServEl.FromDate >= CTServPac.FromDate AND
            CTServEl.FromDate <= CTServPac.ToDate   AND
            CTServEl.FromDate <= idtDate,
         FIRST ServCom NO-LOCK WHERE
               ServCom.Brand    = gcBrand          AND
               ServCom.ServCom  = CTServEl.ServCom AND
               ServCom.Target   = 0
         BREAK BY CTServEl.ServPac
               BY ServCom.ScPosition
               BY CTServEl.ServCom
               BY CTServEl.FromDate DESC:

      /* use newest */
      IF NOT FIRST-OF(CTServEl.ServCom) THEN NEXT.
      
      /* Create subrequests (set mandataory and orig request) */ 
      liReq = fServiceRequest (iiMsSeq,
                               CTServEl.ServCom,
                               CTServEl.DefValue,
                               CTServEl.DefParam,
                               ideActTime + 0.00005, /* 5 sec delay */ 
                               "",                /* SalesMan */
                               FALSE,             /* Set fees */
                               FALSE,             /* SMS */
                               "",
                               OUTPUT ocResult).
      /* Change this request to subrequest */ 
      
      /* Creation of subrequests failed, "fail" master request too */
      IF liReq = 0 OR liReq = ? THEN DO:
         
         fReqStatus(3,"ServiceRequest failure: " + ocResult).
         RETURN.
      END.

      FIND bMsRequest EXCLUSIVE-LOCK WHERE
           bMsRequest.MsRequest = liReq
      NO-ERROR.
      
      ASSIGN bMsRequest.Mandatory = 1
             bMsRequest.OrigRequest = liMasterRequest.
      
      RELEASE bMsRequest.
   END.
   
   /* Subrequests created, assign Master request 
      to status 7 (it has new unhandled requests */
   IF NOT fReqStatus(7,"") THEN RETURN.

END PROCEDURE.
 

PROCEDURE pDone.
 
   DEF VAR lcSMSText  AS CHAR NO-UNDO.
   DEF VAR ldSMSStamp AS DEC  NO-UNDO.
    
   DEF BUFFER bOLBRequest FOR MsRequest.

   CASE fGetSubRequestState(MsRequest.MsRequest):

      WHEN 2 THEN DO:
      /* request handled succesfully */
      
         FIND MobSub WHERE
              MobSub.MsSeq = MsRequest.MsSeq
         EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

         IF LOCKED(MobSub) THEN LEAVE.         
         
         ELSE DO:
            IF MsRequest.ReqCParam1 BEGINS "UN" THEN ASSIGN MobSub.MsStatus = 4.
            ELSE ASSIGN MobSub.MsStatus = 8.
         END.
   
         /* send SMS */
         IF MsRequest.SendSMS = 1 AND MsRequest.SMSText > "" THEN DO:

            FIND Customer WHERE Customer.CustNum = MobSub.CustNum NO-LOCK.
            
            lcSMSText = fGetTxt("SMS",
                                MsRequest.SMSText,
                                TODAY,
                                Customer.Language).

            IF lcSMSText > "" THEN DO:                    
             
              lcSMSText = REPLACE(lcSMSText,"#MSISDN",MsRequest.CLI).
                
              /* don't send messages in weird hours */
              ldSMSStamp = DYNAMIC-FUNCTION("fMakeOfficeTS" in ghFunc1).
              IF ldSMSStamp = ? THEN ldSMSStamp = fMakeTS().

              fMakeSchedSMS(MobSub.CustNum,
                            MobSub.CLI,
                            24,
                            lcSMSText,
                            ldSMSStamp).
            END.          
         END. 
          
         /* remove old olb tags if this a new one */
         IF MsRequest.ReqCParam2 = "OLB" THEN
         FOR EACH bOLBRequest EXCLUSIVE-LOCK USE-INDEX MsSeq WHERE
                  bOLBRequest.MsSeq      = MsRequest.MsSeq AND
                  bOLBRequest.Reqtype    = 35              AND
                  bOLBRequest.ReqStat    = 2               AND
                  bOLBRequest.ReqCParam2 = "OLB"           AND
                  RECID(bOLBRequest) NE RECID(MsRequest):
            bOLBRequest.ReqCParam2 = "".      
         END.
         
         /* MobSub status update ok, Master request OK */
         fReqStatus(2,"").
      
      END.
      /* All other statuses keep mainrequest waiting until HLR
         is done correctly */
         
      OTHERWISE DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.

   END CASE.


END PROCEDURE.

