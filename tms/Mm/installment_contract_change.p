/* ----------------------------------------------------------------------
  MODULE .......: installment_contract_change_request.p
  TASK .........: Change installment contract to another
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 25.03.13
---------------------------------------------------------------------- */

{commali.i}
{tmsconst.i}
{timestamp.i}
{fmakemsreq.i}
{msreqfunc.i}
{fcreditreq.i}
{fixedfee.i}
{ordercancel.i}

DEF INPUT PARAMETER iiMsRequest AS INT  NO-UNDO.

DEF VAR liOrigStatus            AS INT  NO-UNDO.
DEF VAR ldaActivationDate       AS DATE NO-UNDO.
DEF VAR liActivationTime        AS INT  NO-UNDO.

/******** Main start *********/

FIND FIRST MsRequest WHERE
           MsRequest.MsRequest = iiMsRequest AND
           MsRequest.ReqType   = {&REQTYPE_INSTALLMENT_CONTRACT_CHANGE}
     NO-LOCK NO-ERROR.
IF NOT AVAIL MsRequest THEN
   RETURN "ERROR:Unknown MsRequest " + STRING(iiMsRequest).

liOrigStatus = MsRequest.ReqStatus.

fSplitTS(MsRequest.ActStamp,
         OUTPUT ldaActivationDate,
         OUTPUT liActivationTime).

/* request is under work */
IF NOT fReqStatus(1,"") THEN RETURN "ERROR".

FIND FIRST MobSub WHERE
           MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN DO:
   fReqError("ERROR:Subscription not available").
   RETURN "ERROR:Subscription not available".
END. /* IF NOT AVAILABLE MobSub THEN DO: */

CASE liOrigStatus:
WHEN {&REQUEST_STATUS_NEW} THEN DO:
   RUN pInstallmentContractChange.
END.
WHEN {&REQUEST_STATUS_SUB_REQUEST_DONE} THEN DO:
   RUN pFinalize.
END.
OTHERWISE RETURN "ERROR:Current status is not handled".
END CASE. 

IF RETURN-VALUE BEGINS "ERROR:" THEN DO:
   fReqError(RETURN-VALUE).
END.

RETURN RETURN-VALUE.

/******** Main end *********/

PROCEDURE pInstallmentContractChange:

   DEF VAR lcError                AS CHAR NO-UNDO.
   DEF VAR liTermReq              AS INT  NO-UNDO.
   DEF VAR liActReq               AS INT  NO-UNDO.
   DEF VAR liFFBegPeriod          AS INT  NO-UNDO.
   DEF VAR liLastUnBilledPeriod   AS INT  NO-UNDO.
   DEF VAR ldPeriodTo             AS DEC  NO-UNDO.
   DEF VAR ldeCreditNoteAmount    AS DEC  NO-UNDO.
   DEF VAR ldaFirstDayOfLastMonth AS DATE NO-UNDO.
   DEF VAR ldaLastDayOfLastMonth  AS DATE NO-UNDO.
   DEF VAR ldaLastUnBilledDate    AS DATE NO-UNDO.
   DEF VAR liPercontrId           AS INT  NO-UNDO.

   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = gcBrand AND
              DayCampaign.DCEvent = MsRequest.ReqCParam2 NO-LOCK NO-ERROR.
   IF NOT AVAIL DayCampaign OR 
      DayCampaign.ValidFrom > TODAY OR
      DayCampaign.ValidTo   < TODAY THEN
      RETURN "ERROR:New installment contract is not valid".

   FIND FIRST DCCLI WHERE
              DCCLI.MsSeq         = MobSub.MsSeq         AND
              DCCLI.DCEvent       = MsRequest.ReqCParam1 AND
              DCCLI.PerContractId = MsRequest.ReqIParam3 AND
              DCCLI.ValidTo      >= ldaActivationDate NO-LOCK NO-ERROR.
   IF NOT AVAIL DCCLI THEN
      RETURN "ERROR:Current installment contract is not valid".

   liFFBegPeriod = YEAR(DCCLI.ValidFrom) * 100 + MONTH(DCCLI.ValidFrom).

   FIND FIRST FixedFee NO-LOCK USE-INDEX CustNum WHERE
              FixedFee.Brand     = gcBrand   AND
              FixedFee.CustNum   = MobSub.CustNum AND
              FixedFee.HostTable = "MobSub"  AND
              FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
              FixedFee.CalcObj   = DCCLI.DCEvent AND
              FixedFee.SourceTable = "DCCLI" AND
              FixedFee.SourceKey = STRING(DCCLI.PerContractId) NO-ERROR.
   IF NOT AVAIL FixedFee THEN
      FIND FixedFee NO-LOCK USE-INDEX CustNum WHERE
           FixedFee.Brand     = gcBrand   AND
           FixedFee.CustNum   = MobSub.CustNum AND
           FixedFee.HostTable = "MobSub"  AND
           FixedFee.KeyValue  = STRING(MsRequest.MsSeq) AND
           FixedFee.CalcObj   = DCCLI.DCEvent AND
           FixedFee.EndPeriod > (YEAR(ldaActivationDate) * 100 +
                                 MONTH(ldaActivationDate)) NO-ERROR.
   IF AVAIL FixedFee THEN DO:
      FOR EACH FFItem OF FixedFee NO-LOCK:

         IF FFItem.Billed = TRUE AND
            CAN-FIND (FIRST Invoice USE-INDEX InvNum WHERE
                            Invoice.Brand   = gcBrand AND
                            Invoice.InvNum  = FFItem.InvNum AND
                            Invoice.InvType = 1 NO-LOCK) THEN
            ldeCreditNoteAmount = ldeCreditNoteAmount + FFItem.Amt.
         ELSE DO:
            liLastUnBilledPeriod = FFItem.BillPeriod.
   UNDO,          LEAVE.
         END.
      END.
   END.

   IF liLastUnBilledPeriod = 0 THEN
      liLastUnBilledPeriod = liFFBegPeriod.

   ldaLastUnBilledDate = fPer2Date(liLastUnBilledPeriod,0).

   IF MONTH(ldaLastUnBilledDate) = 1 THEN
      ldaFirstDayOfLastMonth = DATE(12,1,YEAR(ldaLastUnBilledDate) - 1).
   ELSE
      ldaFirstDayOfLastMonth = DATE((MONTH(ldaLastUnBilledDate) - 1),
                                     1,YEAR(ldaLastUnBilledDate)).

   ASSIGN
      ldaLastDayOfLastMonth = fLastDayOfMonth(ldaFirstDayOfLastMonth)
      ldPeriodTo = fMake2Dt(ldaLastDayOfLastMonth,86399).
   
   /* Q25 contract change */
   IF MsRequest.ReqCparam1 = "RVTERM12" AND
      MsRequest.ReqCparam2 = "RVTERM12" THEN
      liPercontrId = DCCLI.PerContractId.
   ELSE
      liPercontrId = 0.

   /* Terminate current payterm contract */
   liTermReq = fPCActionRequest(MobSub.MsSeq,
                                DCCLI.DCEvent,
                                "term",
                                ldPeriodTo,
                                FALSE,   /* create fee */
                                {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE},
                                "",
                                MsRequest.MsRequest,
                                TRUE,
                                "",
                                0,
                                DCCLI.PerContractId,
                                OUTPUT lcError).
   IF liTermReq = 0 OR liTermReq = ? THEN
      RETURN "ERROR:Current Installment Contract termination request " +
             "creation failed; " + lcError.

   /* activate new payterm contract */
   liActReq = fPCActionRequest(MobSub.MsSeq,
                               DayCampaign.DCEvent, 
                               "act:wait" + STRING(liTermReq),
                               MsRequest.ActStamp,
                               TRUE,   /* create fee */
                               {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE},
                               "",
                               MsRequest.MsRequest,
                               TRUE,
                               "",
                               MsRequest.ReqDParam2, /* residual fee */
                               liPercontrId,
                               OUTPUT lcError).
   IF liActReq = 0 OR liActReq = ? THEN
       UNDO, RETURN "ERROR:New Installment Contract termination request " +
             "creation failed; " + lcError.   
      
   /* wait for subrequests */
   fReqStatus(7,"").

   /* Specify Credit Note Amount */
   IF ldeCreditNoteAmount > 0 AND AVAIL FixedFee THEN DO:
      FIND FIRST MsRequest WHERE MsRequest.MsRequest = iiMsRequest
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL MsRequest THEN
         ASSIGN MsRequest.ReqDParam1  = ldeCreditNoteAmount
                MsRequest.ReqDtParam1 = ldaLastDayOfLastMonth
                MsRequest.ReqIparam1  = FixedFee.FFNum.
   END. /* IF ldeCreditNoteAmount > 0 THEN DO: */

   RETURN "".
   
END PROCEDURE.

PROCEDURE pFinalize:

   DEF BUFFER bSubRequest     FOR MsRequest.
   DEF VAR llCreditCommission AS LOG NO-UNDO. 

   /* check that subrequests really are ok */
   IF fGetSubRequestState(MsRequest.MsRequest) NE 2 THEN DO:
      IF MsRequest.ReqStat NE 7 THEN DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.   
      RETURN "".
   END.

   /* Make Credit Note(s) */
   IF MsRequest.ReqDParam1 > 0 THEN
      FOR FIRST bSubRequest WHERE
                bSubRequest.OrigRequest = MsRequest.MsRequest AND
                bSubRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                bSubRequest.ReqStatus = {&REQUEST_STATUS_DONE} NO-LOCK,
          FIRST DCCLI WHERE
                DCCLI.MsSeq   = bSubRequest.MsSeq AND
                DCCLI.DCEvent = bSubRequest.ReqCParam3 AND
                DCCLI.PerContractId = MsRequest.ReqIParam3 AND
                DCCLI.ValidTo = MsRequest.ReqDtParam1 NO-LOCK,
          FIRST FixedFee NO-LOCK WHERE
                FixedFee.FFNum = MsRequest.ReqIparam1:
                
         IF LOOKUP(FixedFee.FinancedResult, {&TF_STATUSES_BANK}) > 0 OR
                   FixedFee.FinancedResult EQ {&TF_STATUS_SENT_TO_BANK} THEN DO:

            FIND FIRST FixedFeeTF NO-LOCK WHERE
                       FixedFeeTF.FFNum = FixedFee.FFNum NO-ERROR.
                               
            llCreditCommission = (AVAIL FixedFeeTF AND
                                  FixedFeeTF.BankDate NE ? AND 
                                  TODAY - FixedFeeTF.BankDate <= 30).
         END.

         RUN pCreditInstallment(FixedFee.FFNum,
                                llCreditCommission,
                                MsRequest.MsRequest).
      END.

   fReqStatus(2,"").

   RETURN "".

END PROCEDURE.



