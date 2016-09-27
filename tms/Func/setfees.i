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

{commali.i}
{fmakeservlimit.i}
{create_eventlog.i}
{nncoit2.i}
{fcustpl.i}
{eventval.i}
{terminal_financing.i}

FUNCTION fMakeSetfees RETURN INT
( INPUT  FeeModel     AS CHAR, 
         asnro        AS INT, 
         iiMSSeq      AS INT,
         iiBillTarget AS INT,
         icCalcObj    AS CHAR , 
         icMemo       AS CHAR  ,
         Period       AS INT ,
         ValidFrom    AS Date ,
         InterAct     AS LOGICAL ,
         idPrice      AS DEC,
         ContractID   AS CHAR,
         ilActive     AS LOG,
         icUserCode   AS CHAR,
         icFeememo    AS CHAR,
         iiOrderId    AS INT,
         icSourceTable AS CHAR,
         icSourceKey   AS CHAR): 


DEF VAR InstDuePeriod   AS I  NO-UNDO.
DEF VAR ConPer   AS I  NO-UNDO.

DEF VAR rc       AS I  NO-UNDO.
DEF VAR yy       AS I  NO-UNDO.
DEF VAR mm       AS I  NO-UNDO.
DEF VAR ask-data AS LO NO-UNDO.
DEF VAR UserName AS C  NO-UNDO.

DEF VAR lcFMPriceList AS CHAR NO-UNDO. 
DEF VAR liEndPeriod   AS INT  NO-UNDO. 
DEF VAR liCreated     AS INT  NO-UNDO. 
DEF VAR liFCnt        AS INT  NO-UNDO. 
DEF VAR ldActStamp    AS DEC  NO-UNDO. 
DEF VAR liFeeCust     AS INT  NO-UNDO.
DEF VAR liPenaltyFee  AS INT  NO-UNDO.
DEF VAR lcError       AS CHAR NO-UNDO.
DEF VAR lcFinancedResult AS CHAR NO-UNDO. 
DEF VAR liIFSStatus   AS INT  NO-UNDO. 
DEF VAR lcRequestSource AS CHAR NO-UNDO. 

DEF BUFFER xSingleFee FOR SingleFee.
DEF BUFFER xFixedFee FOR FixedFee.

FORM
SKIP(1)
" NOTE:  You should now create a set of Billable fees for "    SKIP
"        a mobile subscriber"                                  SKIP
"        Customer No...........:" UserName FORMAT "x(24)"    SKIP 
"        according to a pre-defined BILLING EVENT"             SKIP(1)
         FeeModel FORMAT "x(16)"  AT 9 
HELP "Code of a Billing Event  (empty: RETURN)"         
         SPACE(0) ":" FeeName FORMAT "x(15)"                    SKIP(1)
"        Invoicable earliest ..:" InstDuePeriod FORMAT "999999"
HELP "FIRST Period (YYYYMM) when this Event shall be billed"   SKIP 
"        1. Period CONCERNED ..:" ConPer FORMAT "999999"       SKIP         

"        You can now:"                                         SKIP
"           - CHANGE another Billing Event    (F1)"            SKIP
"           - CHECK the contents of the Event (F4)"            SKIP
"           - CREATE the fees                 (F5)"            SKIP
"           - SKIP the creation and return    (F8)"            SKIP
WITH
   CENTERED OVERLAY ROW 1 NO-LABELS
   TITLE " BILLING Event ACTIVATION "
   FRAME info.

liFeeCust = 0.

IF iiMSSeq > 0 THEN DO:

   FIND MobSub  WHERE MobSub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL mobsub THEN DO:
      FIND FIRST MsOwner NO-LOCK WHERE
                 MsOwner.MSSeq = iiMSSeq AND
                 (IF asnro > 0 THEN MsOwner.CustNum = asnro ELSE TRUE)
                 NO-ERROR.
      IF NOT AVAILABLE MsOwner THEN DO:
        MESSAGE 
        "Unknown Mobile Subscription sequence no. '" string(iimsseq) "'" SKIP
        "Fee" FeeModel "not created."
        VIEW-AS ALERT-BOX.

        RETURN 0. 
      END.    
   END.

   ELSE 
   FIND FIRST msowner USE-INDEX MsSeq WHERE 
              MsOwner.MsSeq   = MobSub.MsSeq AND
              MsOwner.CustNum = MobSub.CustNum no-lock no-error.

   IF ContractID = "" THEN ContractID = MSOwner.Contract.
   IF asnro = 0 THEN asnro = MsOwner.CustNum.

   IF iiBillTarget = 0 THEN
      iiBillTarget = IF AVAILABLE MobSub
                     THEN MobSub.BillTarget 
                     ELSE MsOwner.BillTarget.
                                                   
   IF AVAILABLE MobSub THEN ASSIGN 
      liFeeCust = MobSub.InvCust.
   ELSE ASSIGN    
      liFeeCust = MsOwner.InvCust.
   
END.

/* Wee need TO know the Price List Code FOR this customer ... */
FIND FIRST Customer WHERE
           Customer.CustNum = asnro NO-LOCK NO-error.
IF liFeeCust = 0 THEN liFeeCust = Customer.InvCust.

username = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                            BUFFER Customer).

/* Invoicing InstDuePeriod */
IF Period = 0 THEN
   InstDuePeriod = YEAR(TODAY) * 100 + MONTH(TODAY) .
ELSE
   InstDuePeriod = INT(SUBSTRING(STRING(Period),1,6)).

IF ilActive = ? THEN ilActive = TRUE.

/* pricelist may be given */
IF iiBillTarget = -1 THEN ASSIGN
   lcFMPriceList = ContractID
   ContractID    = ""
   iiBillTarget  = 1.
ELSE 
   lcFMPriceList = fFeeModelPriceList(asnro,
                                      iiBillTarget,
                                      FeeModel,
                                      ValidFrom).

IF ValidFrom <= TODAY 
THEN ldActStamp = 0.
ELSE ldActStamp = fMake2DT(ValidFrom,1).

IF iiOrderId > 0 THEN
   lcFinancedResult = fOrderContainsFinancedTerminal(iiOrderId,icCalcObj).

IF NUM-ENTRIES(icFeememo,";") >= 2 THEN DO:

   lcRequestSource = ENTRY(2,icFeememo,";").

   IF lcRequestSource NE {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION} AND
      lcRequestSource NE {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE}
      THEN liIFSStatus = {&IFS_STATUS_WAITING_SENDING}.

   IF lcRequestSource EQ {&REQUEST_SOURCE_INSTALLMENT_CONTRACT_CHANGE} OR
      lcRequestSource EQ {&REQUEST_SOURCE_NEWTON} THEN
      lcFinancedResult = {&TF_STATUS_YOIGO}.
END.

/******************************************************
* Search THRU whole billing Event BillCode package of  *
* this Price list code                                *
******************************************************/
FOR EACH FMItem NO-LOCK  WHERE
         FMItem.Brand     = gcBrand       AND
         FMItem.FeeModel  = FeeModel      AND
         FMItem.PriceList = lcFMPriceList AND
         FMItem.FromDate <= ValidFrom     AND
         FMItem.ToDate   >= ValidFrom,

         BillItem no-lock WHERE
         BillItem.Brand    = gcBrand      AND
         BillItem.BillCode = FMItem.BillCode,
         
   FIRST PriceList NO-LOCK WHERE
         PriceList.Brand = gcBrand AND
         PriceList.PriceList = FMItem.PriceList:

     IF FMItem.BillType EQ "NF" THEN DO:
        liCreated = liCreated + 1.
        next.
     END.
     
     IF FMItem.BillMethod = TRUE /* a SINGLE FEE */ THEN 
     DO FOR xSingleFee TRANS:

        /* calculate InstDuePeriod that this fee concerns (NEXT MONTH) */
        ConPer = YEAR(ValidFrom) * 10000 + 
                 MONTH(ValidFrom) * 100  + day(ValidFrom).
        /* make a OBI record (a single payment) */
        CREATE xSingleFee.

        ASSIGN
        xSingleFee.Brand       = gcBrand 
        xSingleFee.FMItemId    = NEXT-VALUE(bi-seq)
        xSingleFee.CustNum     = liFeeCust      /* customer number */
        xSingleFee.BillTarget  = iiBillTarget
        xSingleFee.CalcObj     = STRING(ENTRY(1,icCalcObj,"¤"))
        xSingleFee.BillCode    = FMItem.BillCode    /* BillCode Code      */
        xSingleFee.BillPeriod  = Period           /* billing InstDuePeriod  */
        xSingleFee.Concerns[1] = ConPer           /* InstDuePeriod concerned */
        xSingleFee.Amt         = IF idPrice NE ?
                                 THEN idPrice
                                 ELSE FMItem.Amount
        xSingleFee.HostTable   = IF iiMSSeq > 0         
                                 THEN "MobSub"
                                 ELSE "Customer"
        xSingleFee.KeyValue    = IF iiMSSeq > 0
                                 THEN STRING(iiMSSeq)
                                 ELSE STRING(asnro)
        xSingleFee.BillType    = FMItem.BillType
        xSingleFee.Contract    = ContractID
        xSingleFee.Active      = ilActive
        xSingleFee.FeeModel    = FMItem.FeeModel
        xSingleFee.VATIncl     = PriceList.InclVat
        xSingleFee.SourceTable = icSourceTable  
        xSingleFee.SourceKey   = icSourceKey  
        xSingleFee.OrderId     = iiOrderId
        xSinglefee.ServiceLimitGroup = FMItem.ServiceLimitGroup
        rc                     = 1
        liCreated              = liCreated + 1.

        /* divide memo into separate lines */
        DO liFCnt = 1 TO NUM-ENTRIES(icMemo,"¤"):
           IF liFCnt > 5 THEN LEAVE.
           xSingleFee.Memo[liFCnt] = ENTRY(liFCnt,icMemo,"¤").
        END.
        IF NUM-ENTRIES(icCalcObj,"¤") > 1 THEN DO:
            liPenaltyFee = 
            TRUNCATE(DECIMAL(ENTRY(2,icCalcObj,"¤")) * xSingleFee.Amt,0).
            IF liPenaltyFee >= 0 THEN xSingleFee.Amt = liPenaltyFee.  
        END.     

        IF llDoEvent THEN fMakeCreateEvent((BUFFER xSingleFee:HANDLE),
                                           "Brand,FMItemId",
                                           icUserCode,
                                           icFeeMemo).
     END.  /* single fee */

     ELSE DO FOR xFixedFee TRANS: /* A PERIODICAL FEE * */  

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
        xFixedFee.BegPeriod = Period            /* beginning InstDuePeriod  */
        xFixedFee.CustNum   = liFeeCust  /* customer no.            */
        xFixedFee.CalcObj   = icCalcObj     
        xFixedFee.BegDate   = ValidFrom
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
        xFixedFee.CLI       = MsOwner.CLI WHEN AVAIL MsOwner
        
        xFixedFee.InclAmt      = FMItem.InclAmt
        xFixedFee.InclUnit     = FMItem.InclUnit
        xFixedFee.InclBillCode = FMItem.InclBillCode
        xFixedFee.ServiceLimitGroup = FMItem.ServiceLimitGroup
        xFixedFee.Contract     = ContractID
        xFixedFee.InUse        = ilActive
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

        rc = fmakecontract(xFixedFee.FFNum,
                           FMItem.FFItemQty).

        IF InterAct THEN DO:
           MESSAGE 
           "Totally" rc "individual fee items created" 
           "for customer" Customer.CustNum 
           VIEW-AS ALERT-BOX
           TITLE "PERIODICAL FEE CREATED".

           /* THEN we SHOW always ALL Billable items ... */          
           RUN nncobi(xFixedFee.FFNum).
        END.

     END.

     IF FMItem.ServiceLimitGroup ne "" THEN DO:
        fMakeServLimit(INPUT  FMItem.ServiceLimitGroup,
                              iimsseq,
                              (IF FMItem.ServiceLimitGroup BEGINS {&DSS}
                               THEN Customer.CustNum ELSE ?),
                              ldActStamp,
                              ?,
                       OUTPUT lcError).
        IF interact then
        MESSAGE
        "Service Limit Group "  FMItem.ServiceLimitGroup SKIP
        "created"
        VIEW-AS ALERT-BOX.
     END.


END. /* FOR EACH FMItem */

RETURN liCreated.

END.

