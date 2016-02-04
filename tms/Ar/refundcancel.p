/* ----------------------------------------------------------------------------
  MODULE .......: refundcancel.p
  FUNCTION .....: cancel a refund payment request
  APPLICATION ..: TMS
  AUTHOR .......: aam 
  CREATED ......: 04.09.07
  Changed . ....: 
  Version ......: Yoigo
  --------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/eventval.i} 
{Func/fcustbal.i}
{Func/msreqfunc.i}

DEF INPUT PARAMETER iiMsRequest  AS INT  NO-UNDO.
DEF INPUT PARAMETER icCancelType AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiInvNum     AS INT  NO-UNDO.
DEF INPUT PARAMETER iiCustNum    AS INT  NO-UNDO.
DEF INPUT PARAMETER icReason     AS CHAR NO-UNDO.
DEF INPUT PARAMETER iiToStatus   AS INT  NO-UNDO.
 
DEF VAR liVoucher    AS INT  NO-UNDO.
DEF VAR ldRefundBal  AS DEC  NO-UNDO.
DEF VAR lcCLI        AS CHAR NO-UNDO.
DEF VAR lcError      AS CHAR NO-UNDO.
DEF VAR ldPosting    AS DEC  NO-UNDO EXTENT 10.
DEF VAR liAccount    AS DEC  NO-UNDO EXTENT 10.
DEF VAR liPaymType   AS INT  NO-UNDO.
DEF VAR liMsSeq      AS INT  NO-UNDO.

IF LOOKUP(STRING(iiToStatus),"4,9") = 0 THEN 
   RETURN "ERROR:Invalid target status".
   
IF iiMsRequest > 0 THEN 
FOR FIRST MsRequest EXCLUSIVE-LOCK WHERE
          MsRequest.MsRequest = iiMsRequest:

   IF MsRequest.ReqType NE 23 THEN RETURN "ERROR:Not a refund request".

   IF LOOKUP(STRING(MsRequest.ReqStat),"0,3,16,19") = 0 THEN 
      RETURN "ERROR:Request status doesn't allow cancelling".
      
   ASSIGN
      liMsSeq = MsRequest.MsSeq
      lcCLI   = MsRequest.CLI.
      
   RUN pCancelRefund.
END.
          
ELSE 
FOR EACH MsRequest EXCLUSIVE-LOCK USE-INDEX CustNum WHERE
         MsRequest.Brand      = gcBrand    AND
         MsRequest.ReqType    = 23         AND
         MsRequest.CustNum    = iiCustNum  AND
         MsRequest.ReqIParam1 = iiInvNum   AND
         LOOKUP(STRING(MsRequest.ReqStat),"0,16,19") > 0:
   
   ASSIGN 
      liMsSeq = MsRequest.MsSeq
      lcCLI   = MsRequest.CLI.
   
   RUN pCancelRefund.
END.

RETURN lcError.

         
PROCEDURE pCancelRefund:
         
   IF lcCLI = "" THEN DO:
      IF liMsSeq > 0 THEN 
      FOR FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq = liMsSeq:
         lcCLI = MsOwner.CLI.
      END.
      ELSE lcCLI = "TOTAL".
   END.
        
   ldRefundBal = fGetCustBal(MsRequest.CustNum,
                             "TOTAL",
                             "REF").
   
   lcError = "".
                                    
   IF ldRefundBal >= MsRequest.ReqDParam1 THEN DO:
        
      ASSIGN
         liAccount[1] = fCParamI("RefundBalAcc")
         ldPosting[1] = MsRequest.ReqDParam1
         ldPosting[2] = -1 * MsRequest.ReqDParam1.
            
      /* if dd-cancellation then deduct from bank account */
      CASE icCancelType:
      WHEN "AP" THEN ASSIGN
         liAccount[2] = fCParamI("AdvPaymAcc")
         liPaymType   = 4.
      WHEN "DD" THEN ASSIGN
         liAccount[2] = fCParamI("BankAcc")
         liPaymType   = 0.
      OTHERWISE ASSIGN
         liPaymType = ?
         liVoucher  = 0.
      END CASE.    

      IF liPaymType NE ? THEN 
      RUN Ar/createpaym (MsRequest.CustNum,
                      MsRequest.ReqIParam1,   /* invoice */
                      lcCLI,
                      TODAY,
                      TODAY,
                      ldPosting,
                      liAccount,
                      "CRRF",
                      liPaymType,
                      "R" + STRING(MsRequest.MsRequest),
                      "Refund cancelled: " + icReason,
                      OUTPUT liVoucher).

      IF liVoucher = 0 THEN
         lcError = "ERROR:Refund cancellation payment was not created".
   END.

   ELSE lcError = "ERROR:Refund balance is less than refund amount".  
   
   /* needs attention */
   IF lcError > "" THEN DO:
      fReqStatus(99,
                 icReason + " " + lcError).
   END.
      
   ELSE DO:
      fReqStatus(iiToStatus,    
                 icReason).
   END.
         
END.
         


