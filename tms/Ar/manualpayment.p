/* ----------------------------------------------------------------------------
  MODULE .......: manualpayment.p
  FUNCTION .....: Handle manual payment requests
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 05.11.07
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Func/msreqfunc.i}

{Func/faccper.i}
{Func/fcustbal.i}
{Syst/eventval.i} 
{Func/fbankday.i}
{Func/fhdrtext.i}
{Func/frefundreq.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}
END.

DEF BUFFER bSubRequest FOR MsRequest.


DEF INPUT PARAMETER iiRequest AS INT NO-UNDO.

FIND MsRequest WHERE MsRequest.MsRequest = iiRequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest OR MsRequest.ReqType NE 34 THEN RETURN "ERROR".


CASE MsRequest.ReqStat:
WHEN 0 OR 
WHEN 15 THEN RUN pManualPayment.
WHEN 8  THEN RUN pDone.
END CASE.

/* eventlog temp-tables */
fCleanEventObjects().

RETURN RETURN-VALUE.



PROCEDURE pManualPayment:

   DEF VAR ldtAccDate    AS DATE NO-UNDO.

   /* request is under work */
   IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

   FIND Customer WHERE Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
      
   IF NOT AVAILABLE Customer THEN DO:
      fReqError("Customer not found").
      RETURN.
   END.

   IF MsRequest.ReqIParam1 > 0 THEN DO:
      FIND Invoice WHERE Invoice.InvNum = MsRequest.ReqIParam1 
         NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE Invoice THEN DO:
         fReqError("Invoice not found").
         RETURN.
      END.
   END.
   
   IF MsRequest.ReqDParam1 <= 0 THEN DO:
      fReqError("Invalid refund amount").
      RETURN.
   END.
   
   fSplitTS(MsRequest.ActStamp,
            OUTPUT ldtAccDate,
            OUTPUT liReqCnt).
            
   /* never date payments to past */
   ldtAccDate = MAX(ldtAccDate,TODAY).
 
   /* check period */
   IF fPeriodLocked(ldtAccDate,FALSE) THEN DO:
      fReqError("Period for " + STRING(ldtAccDate,"99.99.99") + 
                " is locked.").
      RETURN.
   END.

   CASE MsRequest.ReqIParam2:
   WHEN 1 THEN RUN pAdvPaym2Refund (ldtAccDate).
   OTHERWISE DO:
      fReqError("Unknown manual payment type").
   END.
   END CASE. 
   
END.
      
PROCEDURE pAdvPaym2Refund:
      
   DEF INPUT PARAMETER idtAccDate AS DATE NO-UNDO.

   DEF VAR ldBalance     AS DEC  NO-UNDO.
   DEF VAR ldActStamp    AS DEC  NO-UNDO. 
   DEF VAR ldtDate       AS DATE NO-UNDO.
   DEF VAR liTime        AS INT  NO-UNDO.
   DEF VAR liAccount     AS INT  NO-UNDO EXTENT 10.
   DEF VAR ldPosting     AS DEC  NO-UNDO EXTENT 10.
   DEF VAR liSubReq      AS INT  NO-UNDO.
            
   ldBalance = fGetCustBal(MsRequest.Custnum,"TOTAL","AP"). 
   
   IF ldBalance < MsRequest.ReqDParam1 THEN DO:
      fReqError("Adv.payment balance is less than refund amount").
      RETURN. 
   END.
   
   /* refund request can be activated (almost) immediatelly */
   ASSIGN 
      ldtDate = TODAY
      /* give me sometime to mark the link and move the amount to refund
         balance after subrequest creation */
      liTime  = TIME + 180.
   IF liTime > 86399 THEN ASSIGN
      ldtDate = ldtDate + 1
      liTime  = liTime - 86400. 
      
   ldActStamp = fMake2Dt(ldtDate,liTime).
          
   liSubReq = fRefundRequest(MsRequest.CustNum,
                             MsRequest.ReqIParam1,  /* invoice */
                             0,
                             2,                     /* refund to bank */
                             MsRequest.ReqDParam1,  /* amount */
                             3,                     /* type of amount */
                             Customer.BankAcc,
                             1,
                             MsRequest.ReqCParam2,  /* memo */
                             ldActStamp,
                             FALSE,
                             "",
                             OUTPUT lcReqChar).
                                 
   IF liSubReq > 0 THEN DO:

      /* link main request to subrequest */
      FIND bSubRequest WHERE bSubRequest.MsRequest = liSubReq EXCLUSIVE-LOCK.
      ASSIGN 
         bSubRequest.OrigRequest = MsRequest.MsRequest
         bSubRequest.Mandatory   = 0.
      RELEASE bSubRequest.      
         
      /* move the amount to be refunded into customer's refund balance */
      ASSIGN
         /* let createpaym get the default accounts */   
         liAccount    = 0
         ldPosting[1] = MsRequest.ReqDParam1
         ldPosting[2] = -1 * MsRequest.ReqDParam1.
   
      RUN createpaym (MsRequest.CustNum,
                      MsRequest.ReqIParam1,      /* invoice */
                      MsRequest.ReqCParam1,      /* cli */
                      idtAccDate,                /* posting date */
                      MsRequest.ReqDtParam1,     /* payment date */
                      ldPosting,
                      liAccount,
                      "APRF",
                      4,                         /* payment type */
                      "",
                      MsRequest.ReqCParam2,      /* memo */
                      OUTPUT liReqCnt).

      IF liReqCnt > 0 THEN DO:
         /* request handled succesfully */   
         fReqStatus(2,""). 
      END.
      
      ELSE DO:
         fReqError("Adv.payment balance transfer to refund balance failed").
         
         /* mark subrequest to error also */   
         FIND MsRequest WHERE MsRequest.MsRequest = liSubReq EXCLUSIVE-LOCK.
         fReqError("Main request failed").
      END.

   END.
   
   ELSE DO:
      fReqError("Refund request creation failed").
   END.
   
END PROCEDURE.

PROCEDURE pDone:

   /* check that all subrequests have been handled */
   CASE fGetSubRequestState(MsRequest.MsRequest):
    
   WHEN 2 THEN DO:
      /* request handled succesfully */
      fReqStatus(2,""). 
   END.
   
   WHEN 3 OR WHEN 4 THEN DO:
      fReqError("Subrequest has failed").
   END.

   OTHERWISE DO:
      /* back to waiting mode */
      fReqStatus(7,"").
   END.

   END CASE.
   
END PROCEDURE.

