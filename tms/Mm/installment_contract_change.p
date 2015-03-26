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

DEF INPUT PARAMETER iiMsRequest AS INT  NO-UNDO.

DEF VAR liOrigStatus            AS INT  NO-UNDO.
DEF VAR ldaActivationDate       AS DATE NO-UNDO.
DEF VAR liActivationTime        AS INT  NO-UNDO.

DEFINE TEMP-TABLE ttInvoice NO-UNDO
       FIELD InvNum         AS INT
       FIELD SubInvNum      AS INT
       FIELD InvRowDetail   AS CHAR.

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

   DEF BUFFER bActRequest FOR MsRequest.
   
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
      FIND FIRST FixedFee NO-LOCK USE-INDEX CustNum WHERE
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
            LEAVE.
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
                               "act",
                               MsRequest.ActStamp,
                               TRUE,   /* create fee */
                               {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE},
                               "",
                               MsRequest.MsRequest,
                               TRUE,
                               "",
                               0,
                               0,
                               OUTPUT lcError).
   IF liActReq = 0 OR liActReq = ? THEN
      RETURN "ERROR:New Installment Contract termination request " +
             "creation failed; " + lcError.   
      
   IF liActReq > 0 AND liTermReq > 0 THEN DO:
      find first bActRequest where
                 bActRequest.msrequest = liActReq
           exclusive-lock no-error.
      if avail bActRequest then
         bActRequest.ReqIParam2 = liTermReq.
      RELEASE bActRequest.
   END.

   /* wait for subrequests */
   fReqStatus(7,"").

   /* Specify Credit Note Amount */
   IF ldeCreditNoteAmount > 0 THEN DO:
      FIND FIRST MsRequest WHERE MsRequest.MsRequest = iiMsRequest
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL MsRequest THEN
         ASSIGN MsRequest.ReqDParam1  = ldeCreditNoteAmount
                MsRequest.ReqDtParam1 = ldaLastDayOfLastMonth
                MsRequest.ReqIparam1  = (IF AVAIL FixedFee THEN FixedFee.FFNum
                                         ELSE 0).
   END. /* IF ldeCreditNoteAmount > 0 THEN DO: */

   RETURN "".
   
END PROCEDURE.

PROCEDURE pFinalize:

   DEF VAR liReq           AS INT  NO-UNDO.
   DEF VAR lcError         AS CHAR NO-UNDO.
   DEF VAR lcInvRowDetail  AS CHAR NO-UNDO.

   DEF BUFFER bSubRequest     FOR MsRequest.
   DEF BUFFER bCreditRequest  FOR MsRequest.

   EMPTY TEMP-TABLE ttInvoice.

   /* check that subrequests really are ok */
   IF fGetSubRequestState(MsRequest.MsRequest) NE 2 THEN DO:
      IF MsRequest.ReqStat NE 7 THEN DO:
         /* back to waiting mode */
         fReqStatus(7,"").
      END.   
      RETURN "".
   END.

   /* Make Credit Note(s) */
   IF MsRequest.ReqDParam1 > 0 THEN DO:
      FOR FIRST bSubRequest WHERE
                bSubRequest.OrigRequest = MsRequest.MsRequest AND
                bSubRequest.ReqType = {&REQTYPE_CONTRACT_TERMINATION} AND
                bSubRequest.ReqStatus = {&REQUEST_STATUS_DONE} NO-LOCK,
          FIRST DCCLI WHERE
                DCCLI.MsSeq   = bSubRequest.MsSeq AND
                DCCLI.DCEvent = bSubRequest.ReqCParam3 AND
                DCCLI.ValidTo = MsRequest.ReqDtParam1 NO-LOCK,
          FIRST FixedFee NO-LOCK WHERE
                FixedFee.FFNum = MsRequest.ReqIparam1,
          FIRST FMItem WHERE
                FMItem.Brand     = gcBrand AND
                FMItem.FeeModel  = FixedFee.FeeModel AND
                FMItem.FromDate <= FixedFee.BegDate  AND
                FMItem.ToDate   >= FixedFee.BegDate NO-LOCK:

         /* Check billed Fixed Fee Items */
         FOR EACH FFItem OF FixedFee WHERE
                  FFItem.Billed = TRUE NO-LOCK,
            FIRST Invoice USE-INDEX InvNum WHERE
                  Invoice.Brand   = gcBrand AND
                  Invoice.InvNum  = FFItem.InvNum AND
                  Invoice.InvType = {&INV_TYPE_NORMAL} NO-LOCK,
            FIRST SubInvoice OF Invoice WHERE
                  SubInvoice.MsSeq = MsRequest.MsSeq NO-LOCK:

            lcInvRowDetail = "".

            FOR EACH InvRow OF SubInvoice WHERE
                     InvRow.BillCode = FFItem.BillCode AND
                     InvRow.CreditInvNum = 0 NO-LOCK:

               lcInvRowDetail = lcInvRowDetail + "," +
                                "InvRow=" + STRING(InvRow.InvRowNum) + "|" +
                                "InvRowAmt=" + STRING(InvRow.Amt).
            END. /* FOR EACH InvRow OF SubInvoice WHERE */

            lcInvRowDetail = TRIM(lcInvRowDetail,",").

            /* Avoid duplicate Invoice */
            IF NOT CAN-FIND (FIRST ttInvoice WHERE
                                   ttInvoice.InvNum = Invoice.InvNum) THEN DO:
               CREATE ttInvoice.
               ASSIGN ttInvoice.InvNum       = Invoice.InvNum
                      ttInvoice.SubInvNum    = SubInvoice.SubInvNum
                      ttInvoice.InvRowDetail = lcInvRowDetail.
            END. /* IF NOT CAN-FIND (FIRST ttInvoice WHERE */

         END. /* FOR EACH FFItem OF FixedFee WHERE */
      END. /* FOR FIRST bSubRequest WHERE */

      /* Create credit note */
      FOR EACH ttInvoice NO-LOCK:
         liReq = fFullCreditNote(ttInvoice.InvNum,
                                 STRING(ttInvoice.SubInvNum),
                                 ttInvoice.InvRowDetail,
                                 "Correct",
                                 "2013",
                                 "",
                                 OUTPUT lcError).
         IF liReq = 0 THEN
            DYNAMIC-FUNCTION("fWriteMemo" IN ghFunc1,
                             "MobSub",
                             STRING(MobSub.MsSeq),
                             MobSub.Custnum,
                             "CREDIT NOTE CREATION FAILED",
                             "ERROR:" + lcError).
         ELSE DO TRANS:
            FIND FIRST bCreditRequest WHERE
                       bCreditRequest.MsRequest = liReq
                 EXCLUSIVE-LOCK NO-ERROR.
            IF AVAIL bCreditRequest THEN
               bCreditRequest.OrigRequest = MsRequest.MsRequest.
         END.
      END. /* FOR EACH ttInvoice NO-LOCK: */
   END. /* IF MsRequest.ReqDParam1 > 0 THEN DO: */

   fReqStatus(2,"").

   RETURN "".

END PROCEDURE.



