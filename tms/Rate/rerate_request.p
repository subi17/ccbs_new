/*-----------------------------------------------------------------------------
  MODULE .......: rerate_request.p
  FUNCTION .....: rerate subscription or customer
  AUTHOR .......: aam 
  CREATED ......: 10.03.11
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/msreqfunc.i}
{Syst/tmsconst.i}
{Func/fdss.i}
{Func/direct_dbconnect.i}

DEF INPUT  PARAMETER iiRequest AS INT NO-UNDO.

DEF VAR lhSubsRerate    AS HANDLE NO-UNDO.
DEF VAR lhCustRerate    AS HANDLE NO-UNDO.
DEF VAR ldActStamp      AS DEC  NO-UNDO.
DEF VAR liHandled       AS INT  NO-UNDO.
DEF VAR liSkipped       AS INT  NO-UNDO. 
DEF VAR llReport        AS LOG  NO-UNDO INIT TRUE.
DEF VAR llReportStarted AS LOG  NO-UNDO.
DEF VAR liMsSeq         AS INT  NO-UNDO.
DEF VAR liCustNum       AS INT  NO-UNDO.
DEF VAR lcMsSeqList     AS CHAR NO-UNDO.
DEF VAR ldeDSSActStamp  AS DEC  NO-UNDO.
DEF VAR liActiveDB      AS INT  NO-UNDO.
DEF VAR ldaActiveFrom   AS DATE NO-UNDO.
DEF VAR ldaActiveTo     AS DATE NO-UNDO.

DEF BUFFER bSubRequest FOR MsRequest.


/******* Main start ********/

ASSIGN 
   lcReqType  = "Rerate"
   ldActstamp = fMakeTS().

IF iiRequest >= 0 THEN DO:
   FIND FIRST MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 65 THEN
      RETURN "ERROR:Unknown request".
END.
ELSE DO:
   IF SOURCE-PROCEDURE:PRIVATE-DATA BEGINS "Rerate_Param:" THEN 
      liCustNum  = INT(ENTRY(2,SOURCE-PROCEDURE:PRIVATE-DATA,":")) NO-ERROR.

   IF liCustNum = 0 THEN 
      RETURN "ERROR:Invalid parameters".
END.

RUN cli_ratep.p PERSISTENT SET lhSubsRerate.
RUN pInitializeRerate IN lhSubsRerate.

RUN cust_ratep.p PERSISTENT SET lhCustRerate.
RUN pInitializeRerate IN lhCustRerate.

llReportStarted = FALSE.

/* period for current active cdr db */ 
liActiveDB = fGetCurrentDB(gcBrand,
                           "MobCDR",
                           OUTPUT ldaActiveFrom,
                           OUTPUT ldaActiveTo).
IF ldaActiveFrom = ? THEN ASSIGN
   ldaActiveFrom = 1/1/2010
   ldaActiveTo   = 12/31/2049.
   
IF iiRequest > 0 THEN RUN pGetAllRequests.
ELSE RUN pGetCustomerRequests(liCustNum).

IF VALID-HANDLE(lhSubsRerate) THEN DO:
   IF llReportStarted THEN RUN pFinalizeRerateReport IN lhSubsRerate.
   DELETE PROCEDURE lhSubsRerate.
END.
IF VALID-HANDLE(lhCustRerate) THEN DO:
   IF llReportStarted THEN RUN pFinalizeRerateReport IN lhCustRerate.
   DELETE PROCEDURE lhCustRerate.
END.
  
/* must somehow break request handler loop (1st day of the month) */
IF liSkipped > liHandled THEN RETURN STRING(liSkipped).
RETURN STRING(liHandled). 

/******  Main end ********/

PROCEDURE pGetAllRequests:

   /* exceptionally handle several type 65 requests at the same time, 
      in order to avoid doing rerate initializing for each request */
   RERATE_REQUEST:
   FOR EACH MsRequest NO-LOCK WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.ReqType   = 65      AND
            MsRequest.ReqStat   = 0       AND
            MsRequest.ActStamp <= ldActStamp
   BY MsRequest.ActStamp
   BY MsRequest.MsRequest:

      RUN pHandleRequest.
      IF RETURN-VALUE BEGINS "NEXT" THEN DO:
         liSkipped = liSkipped + 1.
         NEXT RERATE_REQUEST.
      END.
     
      liHandled = liHandled + 1.
      /* don't try to do all at once, give time to other requests also */
      IF liHandled >= 20 THEN LEAVE.
   END.   
   
END PROCEDURE.

PROCEDURE pGetCustomerRequests:

   DEF INPUT PARAMETER iiInvCust AS INT  NO-UNDO. 

   CUSTOMER_REQUEST:
   FOR EACH MsRequest NO-LOCK USE-INDEX CustNum WHERE
            MsRequest.Brand     = gcBrand AND
            MsRequest.CustNum   = iiInvCust AND
            MsRequest.ReqType   = 65      AND
            MsRequest.ReqStat   = 0       AND
            MsRequest.ActStamp <= ldActStamp
   BY MsRequest.ActStamp
   BY MsRequest.MsRequest:

      RUN pHandleRequest.
      IF RETURN-VALUE BEGINS "NEXT" THEN 
         NEXT CUSTOMER_REQUEST.
     
      liHandled = liHandled + 1.
   END.   
   
END PROCEDURE.

PROCEDURE pHandleRequest:

   /* should this wait for other subrequests to finish */
   IF MsRequest.OrigRequest > 0 AND MsRequest.ReqIParam1 > 0 THEN 
   FOR EACH bSubRequest NO-LOCK USE-INDEX OrigRequest WHERE
             bSubRequest.OrigRequest = MsRequest.OrigRequest AND
             bSubRequest.MsRequest NE MsRequest.MsRequest AND
             bSubRequest.MsSeq = MsRequest.MsSeq AND
             LOOKUP(STRING(bSubRequest.ReqStatus),
                    {&REQ_INACTIVE_STATUSES}) = 0:
      /* to skip some sub-request which have been delayed (dss termination) */
      IF bSubRequest.ActStamp > foffset(MsRequest.ActStamp,1) THEN NEXT.
      RETURN "NEXT".
   END.

   /* if period to be rerated is newer than what the current connected cdr db
      contains then move the request to be performed later (cdr dbs have been
      renewed but the new dbs have not been connected yet) 
   */
   IF MsRequest.ReqDtParam1 > ldaActiveTo THEN DO TRANS:
      FIND CURRENT MsRequest EXCLUSIVE-LOCK.
      IF MsRequest.ReqDParam2 = 0 THEN 
         MsRequest.ReqDParam2 = MsRequest.ActStamp.
      MsRequest.ActStamp = fSecOffSet(MsRequest.ActStamp,3600).
      RETURN "NEXT".
   END.
   
   IF llReport AND NOT llReportStarted THEN DO:
      IF VALID-HANDLE(lhSubsRerate) THEN 
         RUN pInitializeRerateReport IN lhSubsRerate(katun,
                                                     TODAY,
                                                     TODAY,
                                                     "").
      IF VALID-HANDLE(lhCustRerate) THEN 
         RUN pInitializeRerateReport IN lhCustRerate(katun,
                                                     TODAY,
                                                     TODAY,
                                                     "").
      llReportStarted = TRUE.
   END.

   DO TRANS:
      RUN pRerate. 
   END.

END PROCEDURE. 
 
PROCEDURE pRerate:

   DEF VAR liDoubles AS INT  NO-UNDO.
    
   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   IF MsRequest.ReqDtParam1 = ? OR MsRequest.ReqDtParam2 = ? OR
      MsRequest.ReqDtParam1 > MsRequest.ReqDtParam2 THEN DO:
      fReqError("Invalid period").
      RETURN.
   END.

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   IF MsRequest.MsSeq > 0 THEN DO:
      FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE MobSub THEN DO:
         FIND FIRST MsOwner USE-INDEX MsSeq WHERE
                    MsOwner.MsSeq   = MsRequest.MsSeq AND
                    MsOwner.InvCust = Customer.CustNum NO-LOCK NO-ERROR.
         IF NOT AVAILABLE MsOwner THEN DO:           
            fReqError("Subscription not found").
            RETURN.
         END.
      END.
   END.
   /* Check DSS is active or not based on the last second of month */
   ldeDSSActStamp = fMake2DT(MsRequest.ReqDtParam2,86399).

   IF MsRequest.MsSeq > 0 AND
      NOT fIsDSSActive(INPUT Customer.CustNum,INPUT ldeDSSActStamp) THEN DO:

      /* double check first */
      IF MsRequest.ReqIParam2 = 1 THEN 
         RUN mobcdr_double_check.p ("",
                                    MsRequest.ReqDtParam1,
                                    MsRequest.ReqDtParam2,
                                    MsRequest.CLI,
                                    TRUE,
                                    FALSE,
                                    "",
                                    0,
                                    0,
                                    "", 
                                    OUTPUT liDoubles).

      RUN pRunRerate IN lhSubsRerate(MsRequest.CLI,
                                     MsRequest.ReqDtParam1,
                                     MsRequest.ReqDtParam2,
                                     TRUE).
   END.                              

   ELSE RUN pRunRerate IN lhCustRerate(MsRequest.CustNum,
                                       MsRequest.ReqDtParam1,
                                       MsRequest.ReqDtParam2,
                                       TRUE).
 
   /* request handled succesfully */   
   fReqStatus(2,""). 
 
END PROCEDURE.


