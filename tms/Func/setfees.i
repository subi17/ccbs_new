/* ----------------------------------------------------------------------
  MODULE .......: setfees.i
  TASK .........: Set contract fees from Billing Events FOR a customer
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 05.10.1999 pt
  CHANGED ......: 03.11.1999 pt also an empty FeeModel can be received
                  14.11.1999 pt check that Event contains items FOR this
                                Price List
                  04.01.2000 pt Show ConPer WITH FRAME info, FUNCTION fNextP
                  11.01.2000 jp FMItem.BillType = SingleFee.BillType
                  26.11.2001 jp setfees -> setfees2 , customer contracts
                  11.10.2002 jr Removed BillLevel
                  04.03.2002 aam input parameter iiBillTarget,
                                 pricelist with fFeeModelPriceList
                  24.06.2003 aam FMItem.FromDate, ToDate, FFItemQty, FFEndDate
                  09.09.2003 aam brand
                  25.09.2003 jp  Contractid input parameter
                  24.11.2003/aam ilActive -> FixedFee.InUse & SingleFee.Active,
                                 set FeeModel to FixedFee & SingleFee
                  08.12.2003/aam FixedFee & SingleFee.VatIncl 
                  29.01.2004/aam price from FMItem.Amount if input price = ?,
                                 return liCreated,
                                 input iiMSSeq
                  17.05.2004/aam find MsOwner if MobSub not available
                  28.06.2004/aam divide icMemo to separate lines from "¤" 
                  31.12.2004 aam ldActStamp for fMakeServLimit
                  14.12.2005/aam username from customer
                  26.01.2006/aam fees to invoice customer
                  20.03.2006/aam take invoice customer primarily from subscr.
                  31.08.2006/aam contractid may contain pricelist
                  10.09.2007  vk changed the counting of xSingleFee.Amt in                                        Penalty Fee case
  Version ......: M15
  -------------------------------------------------------------------------- */

&IF "{&SETFEES_I}" NE "YES"
&THEN


&GLOBAL-DEFINE SETFEES_I YES

{Syst/commali.i}
{Func/fmakeservlimit.i}
{Func/create_eventlog.i}
{Func/nncoit2.i}
{Func/fcustpl.i}
{Syst/eventval.i}
{Func/financed_terminal.i}

FUNCTION fMakeSetfees RETURNS INTEGER
   ( icFeeModel    AS CHARACTER, 
     iiCustNum     AS INTEGER, 
     iiMSSeq       AS INTEGER,
     iiBillTarget  AS INTEGER,
     icCalcObj     AS CHARACTER, 
     icMemo        AS CHARACTER,
     iiPeriod      AS INTEGER,
     idaValidFrom  AS DATE,
     idPrice       AS DECIMAL,
     icContractID  AS CHARACTER,
     icUserCode    AS CHARACTER,
     icFeememo     AS CHARACTER,
     iiOrderId     AS INTEGER,
     icSourceTable AS CHARACTER,
     icSourceKey   AS CHARACTER): 
   DEFINE VARIABLE lcRequestSource  AS CHARACTER NO-UNDO. 

   DEFINE VARIABLE lcFMPriceList    AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liEndPeriod      AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liCreated        AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE liFCnt           AS INTEGER   NO-UNDO. 
   DEFINE VARIABLE ldActStamp       AS DECIMAL   NO-UNDO. 
   DEFINE VARIABLE liFeeCust        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liPenaltyFee     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcError          AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFinancedResult AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liIFSStatus      AS INTEGER   NO-UNDO. 
   
   DEFINE BUFFER xSingleFee FOR SingleFee.
   DEFINE BUFFER xFixedFee FOR FixedFee.

   liFeeCust = 0.
   
   IF iiMSSeq > 0
   THEN DO:
      FIND MobSub WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
      IF NOT AVAILABLE mobsub
      THEN DO:
         FIND FIRST MsOwner NO-LOCK WHERE
            MsOwner.MSSeq = iiMSSeq AND
            (IF iiCustNum > 0 THEN MsOwner.CustNum = iiCustNum ELSE TRUE)
         NO-ERROR.
         
         IF NOT AVAILABLE MsOwner
         THEN DO:
           MESSAGE
           "Unknown Mobile Subscription sequence no. '" STRING(iiMsSeq) "'" SKIP
           "Fee" icFeeModel "not created."
           VIEW-AS ALERT-BOX.
           RETURN 0.
         END.
      END.

      ELSE
      FIND FIRST MsOwner NO-LOCK USE-INDEX MsSeq WHERE 
         MsOwner.MsSeq   = MobSub.MsSeq   AND
         MsOwner.CustNum = MobSub.CustNum
      NO-ERROR.

      IF icContractID = ""
      THEN icContractID = MSOwner.Contract.
      IF iiCustNum = 0
      THEN iiCustNum = MsOwner.CustNum.

      IF iiBillTarget = 0 THEN
         iiBillTarget = IF AVAILABLE MobSub
                        THEN MobSub.BillTarget
                        ELSE MsOwner.BillTarget.

      IF AVAILABLE MobSub
      THEN liFeeCust = MobSub.InvCust.
      ELSE liFeeCust = MsOwner.InvCust.
   END.

   /* Wee need TO know the Price List Code FOR this customer ... */
   FIND FIRST Customer NO-LOCK WHERE
      Customer.CustNum = iiCustNum
   NO-ERROR.
   
   IF liFeeCust = 0
   THEN liFeeCust = Customer.InvCust.

   /* pricelist may be given */
   IF iiBillTarget = -1
   THEN ASSIGN
           lcFMPriceList = icContractID
           icContractID  = ""
           iiBillTarget  = 1.
   ELSE lcFMPriceList = fFeeModelPriceList(iiCustNum,
                                           iiBillTarget,
                                           icFeeModel,
                                           idaValidFrom).

   IF idaValidFrom <= TODAY 
   THEN ldActStamp = 0.
   ELSE ldActStamp = fMake2DT(idaValidFrom,1).

   IF iiOrderId > 0
   THEN lcFinancedResult = fOrderContainsFinancedTerminal(iiOrderId,icCalcObj).

   IF NUM-ENTRIES(icFeememo,";") >= 2
   THEN DO:
      lcRequestSource = ENTRY(2,icFeememo,";").

      IF lcRequestSource NE {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} AND
         lcRequestSource NE {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE}
      THEN liIFSStatus = {&IFS_STATUS_WAITING_SENDING}.

      IF lcRequestSource EQ {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} OR
         lcRequestSource EQ {&REQUEST_SOURCE_NEWTON}
      THEN lcFinancedResult = {&TF_STATUS_YOIGO}.
   END.

   /******************************************************
   * Search THRU whole billing Event BillCode package of  *
   * this Price list code                                *
   ******************************************************/
   FOR
      EACH FMItem NO-LOCK WHERE
         FMItem.Brand     = gcBrand       AND
         FMItem.FeeModel  = icFeeModel    AND
         FMItem.PriceList = lcFMPriceList AND
         FMItem.FromDate <= idaValidFrom  AND
         FMItem.ToDate   >= idaValidFrom,

         BillItem NO-LOCK WHERE
         BillItem.Brand    = gcBrand      AND
         BillItem.BillCode = FMItem.BillCode,
      FIRST PriceList NO-LOCK WHERE
         PriceList.Brand = gcBrand AND
         PriceList.PriceList = FMItem.PriceList:

      IF FMItem.BillType EQ "NF"
      THEN DO:
         liCreated = liCreated + 1.
         NEXT.
      END.

      IF FMItem.BillMethod = TRUE /* a SINGLE FEE */
      THEN DO FOR xSingleFee TRANSACTION:
   
         /* make a OBI record (a single payment) */
         CREATE xSingleFee.

         ASSIGN
            xSingleFee.Brand       = gcBrand 
            xSingleFee.FMItemId    = NEXT-VALUE(bi-seq)
            xSingleFee.CustNum     = liFeeCust      /* customer number */
            xSingleFee.BillTarget  = iiBillTarget
            xSingleFee.CalcObj     = STRING(ENTRY(1,icCalcObj,"¤"))
            xSingleFee.BillCode    = FMItem.BillCode    /* BillCode Code      */
            xSingleFee.BillPeriod  = iiPeriod           /* billing InstDuePeriod  */
            xSingleFee.Concerns[1] = YEAR(idaValidFrom) * 10000 + 
                                     MONTH(idaValidFrom) * 100  +
                                     DAY(idaValidFrom)  /* InstDuePeriod concerned */
            xSingleFee.Amt         = IF idPrice NE ?
                                     THEN idPrice
                                     ELSE FMItem.Amount
            xSingleFee.HostTable   = IF iiMSSeq > 0         
                                     THEN "MobSub"
                                     ELSE "Customer"
            xSingleFee.KeyValue    = IF iiMSSeq > 0
                                     THEN STRING(iiMSSeq)
                                     ELSE STRING(iiCustNum)
            xSingleFee.BillType    = FMItem.BillType
            xSingleFee.Contract    = icContractID
            xSingleFee.Active      = TRUE
            xSingleFee.FeeModel    = FMItem.FeeModel
            xSingleFee.VATIncl     = PriceList.InclVat
            xSingleFee.SourceTable = icSourceTable  
            xSingleFee.SourceKey   = icSourceKey  
            xSingleFee.OrderId     = iiOrderId
            xSinglefee.ServiceLimitGroup = FMItem.ServiceLimitGroup
            liCreated              = liCreated + 1.
   
         /* divide memo into separate lines */
         DO liFCnt = 1 TO NUM-ENTRIES(icMemo,"¤"):
            IF liFCnt > 5 THEN LEAVE.
            xSingleFee.Memo[liFCnt] = ENTRY(liFCnt,icMemo,"¤").
         END.
         
         IF NUM-ENTRIES(icCalcObj,"¤") > 1
         THEN DO:
            liPenaltyFee = 
               TRUNCATE(DECIMAL(ENTRY(2,icCalcObj,"¤")) * xSingleFee.Amt,0).
            IF liPenaltyFee >= 0 THEN xSingleFee.Amt = liPenaltyFee.  
         END.     

         IF llDoEvent
         THEN fMakeCreateEvent(BUFFER xSingleFee:HANDLE,
                               "Brand,FMItemId",
                               icUserCode,
                               icFeeMemo).
      END. /* DO FOR xSingleFee */

      ELSE DO FOR xFixedFee TRANSACTION: /* A PERIODICAL FEE * */

         IF FMItem.FFEndDate NE ? AND FMItem.FFItemQty = 0
         THEN liEndPeriod = YEAR(FMItem.FFEndDate) * 100 +
                            MONTH(FMItem.FFEndDate).
         ELSE liEndPeriod = 999999.

         /* We have TO make a contract fee + Billable contract items */
         CREATE xFixedFee.
         /* get an individual sequence FOR a NEW coint record */
         ASSIGN         
            xFixedFee.Brand     = gcBrand 
            xFixedFee.FFNum     = NEXT-VALUE(Contract) /* sequence FOR contract */
            xFixedFee.BegPeriod = iiPeriod            /* beginning InstDuePeriod  */
            xFixedFee.CustNum   = liFeeCust  /* customer no.            */
            xFixedFee.CalcObj   = icCalcObj     
            xFixedFee.BegDate   = idaValidFrom
            xFixedFee.BillCode  = FMItem.BillCode   /* BillCode code           */
            xFixedFee.Amt       = IF idPrice NE ?
                                  THEN idPrice
                                  ELSE FMItem.Amount
            xFixedFee.BillMethod = FMItem.BillCycle /* TO be Billed BEFOREHAND */
            xFixedFee.Interval  = FMItem.Interval   /* Billing Interval MONTHS */
            xFixedFee.EndPeriod = liEndPeriod
            xFixedFee.HostTable = IF iiMSSeq > 0
                                  THEN "MobSub"
                                  ELSE "Customer"
            xFixedFee.KeyValue  = IF iiMSSeq > 0
                                  THEN STRING(iiMSSeq)
                                  ELSE STRING(Customer.CustNum)
            xFixedFee.CLI       = MsOwner.CLI WHEN AVAILABLE MsOwner
            xFixedFee.InclAmt      = FMItem.InclAmt
            xFixedFee.InclUnit     = FMItem.InclUnit
            xFixedFee.InclBillCode = FMItem.InclBillCode
            xFixedFee.ServiceLimitGroup = FMItem.ServiceLimitGroup
            xFixedFee.Contract     = icContractID
            xFixedFee.InUse        = TRUE
            xFixedFee.FeeModel     = FMItem.FeeModel
            xFixedFee.VATIncl      = PriceList.InclVat
            xFixedFee.SourceTable  = icSourceTable  
            xFixedFee.SourceKey    = icSourceKey  
            /* order id should be assigned only for payterm contracts which 
               are sent to terminal financing bank file*/
            xFixedFee.OrderID      = iiOrderId 
            xFixedFee.FinancedResult = lcFinancedResult WHEN iiOrderId > 0
            xFixedFee.IFSstatus    = liIFSStatus 
            xFixedFee.CalcAmt      = xFixedFee.Amt
            liCreated              = liCreated + 1.

         /* divide memo into separate lines */
         DO liFCnt = 1 TO NUM-ENTRIES(icMemo,"¤"):
            IF liFCnt > 5 THEN LEAVE.
            xFixedFee.Memo[liFCnt] = ENTRY(liFCnt,icMemo,"¤").
         END.

         fMakeContract(xFixedFee.FFNum,
                       FMItem.FFItemQty).

      END. /* DO FOR xFixedFee */

      IF FMItem.ServiceLimitGroup NE ""
      THEN fMakeServLimit(INPUT  FMItem.ServiceLimitGroup,
                          iimsseq,
                          (IF FMItem.ServiceLimitGroup BEGINS {&DSS}
                           THEN Customer.CustNum ELSE ?),
                          ldActStamp,
                          ?,
                          OUTPUT lcError).
   END. /* FOR EACH FMItem */

   RETURN liCreated.

END.

&ENDIF