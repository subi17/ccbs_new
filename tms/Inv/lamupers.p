/* Inv/lamupers.p    29.11.2006/aam yoigo version
   7/Oct/2015 hugo.lujan YTS-7139 Mandate fetch should not use MSISDN, instead it should be based on subscription ID only
*/
&GLOBAL-DEFINE EDRHandling NO

{Syst/commali.i}                                                                   
{Syst/tmsconst.i}
{Func/fixedfee.i}
{Func/cparam2.i}
{Func/timestamp.i}
{Func/fapvat.i}
{Inv/billrund.i {1}}
{Func/fcustbal.i}
{Func/fcustcnt.i}
{Func/nncoit2.i}
{Func/fcurrency.i}
{Syst/eventval.i}
{Func/fduedate.i}
{Func/fsubser.i}
{Syst/eventlog.i}
{Func/finvnum.i}
{Func/ftaxdata.i}
{Func/log.i}
{Func/finvoiceacc.i}
{Syst/funcrunprocess_update.i}
{Rate/rerate_request.i}
{Inv/lamupers_temptable.i}
{Inv/invrowcounter_move.i}
{Inv/old_unbilled_events.i}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSingleFee AS HANDLE NO-UNDO.
   lhSingleFee = BUFFER SingleFee:HANDLE.
END.

DEF VAR fhVDHandle AS HANDLE  NO-UNDO. 
&IF "{&InitPersistent}" NE "NO" 
&THEN 
/* volume discount functions */
FUNCTION fVolDiscMob RETURNS LOG (BUFFER VMobCDR FOR MobCDR,iiAgrCust AS INT) 
   IN fhVDHandle. 
RUN Inv/voldisc.p PERSISTENT SET fhVDHandle. 

&IF "{&EDRHandling}" NE "NO"
&THEN
DEF VAR fhDCHandle AS HANDLE  NO-UNDO.
DEF VAR fhRRHandle AS HANDLE  NO-UNDO.

RUN Mm/domcopers.p  PERSISTENT SET fhDCHandle.
RUN Rate/cust_ratep.p PERSISTENT SET fhRRHandle.

&ENDIF
&ENDIF

DEF VAR lUpdacc       AS LOG  NO-UNDO.
DEF VAR lCustVat      AS LOG  NO-UNDO.
DEF VAR lDBLVat       AS LOG  NO-UNDO.
DEF VAR lPrevVAT      AS LOG  NO-UNDO.
DEF VAR llAllowDbl    AS LOG  NO-UNDO.   
DEF VAR ldCustOP      AS DEC  NO-UNDO.
DEF VAR ldCustAP      AS DEC  NO-UNDO.
DEF VAR ldCredLoss    AS DEC  NO-UNDO.
DEF VAR liMinDays     AS INT  NO-UNDO. 
DEF VAR liCLossCust   AS INT  NO-UNDO. 
DEF VAR ldNet         AS DEC  NO-UNDO. 
DEF VAR liDepoAcc     AS INT  NO-UNDO. 
DEF VAR lcDepoItem    AS CHAR NO-UNDO. 
DEF VAR liDepoPTerm   AS INT  NO-UNDO. 
DEF VAR lcMSSeqList   AS CHAR NO-UNDO. 
DEF VAR liIRLQty      AS INT  NO-UNDO.
DEF VAR ldIRLAmt      AS DEC  NO-UNDO. 
DEF VAR liSilent      AS LOG  NO-UNDO INIT FALSE. 
DEF VAR lcCLI         AS CHAR NO-UNDO. 
DEF VAR liOrderID     AS INT  NO-UNDO. 
DEF VAR liOwnUsePA    AS INT  NO-UNDO.
DEF VAR liVipUsePA    AS INT  NO-UNDO.
DEF VAR ldtBSDate     AS DATE NO-UNDO.
DEF VAR liBSTime      AS INT  NO-UNDO. 
DEF VAR lRejBill      AS LOG  NO-UNDO. 
DEF VAR lcRejReason   AS CHAR NO-UNDO. 
DEF VAR lcRejCLI      AS CHAR NO-UNDO. 
DEF VAR liArAccNum    AS INT  NO-UNDO. 
DEF VAR liEventQty    AS INT  NO-UNDO.
DEF VAR ldTotAmt      AS DEC  NO-UNDO.
DEF VAR ldaFirstCall  AS DATE NO-UNDO.
DEF VAR ReceivAcc     AS INT  NO-UNDO.
DEF VAR RoundAcc      AS INT  NO-UNDO.
DEF VAR OTIntAcc      AS INT  NO-UNDO.
DEF VAR OverPayAcc    AS INT  NO-UNDO.
DEF VAR AdvPaymAcc    AS INT  NO-UNDO.
DEF VAR liCLossAcc    AS INT  NO-UNDO. 
DEF VAR lNotBilled    AS CHAR NO-UNDO. 
DEF VAR llBillInt     AS LOG  NO-UNDO.
DEF VAR llInvComb     AS LOG  NO-UNDO INIT FALSE. 
DEF VAR liVASAcc      AS INT  NO-UNDO. 
DEF VAR ldVatFactor   AS DEC  NO-UNDO. 
DEF VAR liFatPer      AS INT  NO-UNDO.
DEF VAR lcBillRun     AS CHAR NO-UNDO.
DEF VAR liLastCreated AS INT  NO-UNDO.
DEF VAR liEndInvType  AS INT  NO-UNDO. 
DEF VAR lcIntItem     AS CHAR NO-UNDO.
DEF VAR lcBillDeny    AS CHAR NO-UNDO.
DEF VAR liAdd2InvDate AS INT  NO-UNDO.
DEF VAR llSetExtInvID AS LOG  NO-UNDO INIT TRUE.
DEF VAR llRunSpecFee  AS LOG  NO-UNDO INIT FALSE.
DEF VAR llRunVolDisc  AS LOG  NO-UNDO INIT FALSE.
DEF VAR lcNoMinCons   AS CHAR NO-UNDO.
DEF VAR lcMinConsFatime AS CHAR NO-UNDO.
DEF VAR lcMinConsFatimeBillCode AS CHAR NO-UNDO.
DEF VAR liFRProcessID AS INT  NO-UNDO.
DEF VAR liUpdInterval AS INT  NO-UNDO.
DEF VAR llCashInvoice AS LOG  NO-UNDO. 
DEF VAR liLoop        AS INT  NO-UNDO. 
DEF VAR lcZeroItem    AS CHAR NO-UNDO.
DEF VAR ldaOldEventDate  AS DATE NO-UNDO.
DEF VAR liOldEventPeriod AS INT  NO-UNDO.
DEF VAR liNotPrintedDelType AS INT  NO-UNDO.

DEF BUFFER xFATime FOR FATime.
DEF BUFFER bFATime FOR FATime.

DEF TEMP-TABLE ttNewInv NO-UNDO
   FIELD InvNum     AS INT
   FIELD Qty        AS DEC
   FIELD vatamt     AS DEC 
   FIELD InvGroup   AS CHAR
   FIELD InvType    AS INT
   FIELD SeqPrefix  AS CHAR 
   FIELD ExtInvID   AS CHAR 
   FIELD UpdCustBal AS LOG
   INDEX InvGroup InvGroup InvType ExtInvID.

DEF TEMP-TABLE ttInv NO-UNDO LIKE Invoice
   FIELD Vat0Amt AS DEC.

DEF TEMP-TABLE ttSubInv NO-UNDO LIKE SubInvoice
   FIELD AgrCust    AS INT 
   FIELD ITGroupID  AS INT 
   FIELD Vat0Amt    AS DEC
   FIELD VatPos     AS INT
   FIELD ITGDelType AS INT
   INDEX SubInvNum SubInvNum
   INDEX ITGroupID ITGroupID SubInvNum.

DEF TEMP-TABLE ttRowVat NO-UNDO
   FIELD VatPerc    AS DEC 
   FIELD VatCode    AS INT 
   FIELD VatAcc     AS INT
   FIELD VatAmt     AS DEC 
   FIELD VatBasis   AS DEC
   FIELD AccVatBasis AS DEC
   FIELD CLI        AS CHAR
   FIELD MsSeq      AS INT
   FIELD ITGroupID  AS INT 
   FIELD SubInvNum  AS INT 
   FIELD AgrCust    AS INT 
   FIELD ITGDelType AS INT
   FIELD InvSeq     AS INT
   INDEX CLI CLI MsSeq VatPerc
   INDEX ITGroupID ITGroupID.

DEF BUFFER bttIR  FOR ttIR.
DEF BUFFER bttCLI FOR ttCLI.

DEF TEMP-TABLE ttInvoiceItem NO-UNDO
   FIELD tType  AS INT
   FIELD MsSeq  AS INT
   FIELD tRecId AS RECID
   INDEX tType AS PRIMARY tType MsSeq.

DEF TEMP-TABLE ttEventCust NO-UNDO
   FIELD CustNum AS INT.

DEF TEMP-TABLE ttCustBal NO-UNDO
   FIELD CLI    AS CHAR
   FIELD Type   AS CHAR
   FIELD Amt    AS DEC
   FIELD Used   AS DEC
   FIELD ITGroupID AS INT
   INDEX CLI CLI Type.

DEF TEMP-TABLE ttFATime NO-UNDO
   FIELD AgrCust AS INT
   FIELD CLI AS CHAR
   FIELD MsSeq AS INT
   INDEX AgrCust AgrCust CLI.
   
DEF TEMP-TABLE ttDiscounts NO-UNDO
   FIELD DPId       AS INT
   FIELD BillCode   AS CHAR
   FIELD ProcStopper AS LOG
   FIELD Priority   AS INT
   FIELD FromDate   AS DATE
   FIELD ToDate     AS DATE
   FIELD TargetType AS CHAR 
   FIELD Unit       AS CHAR 
   FIELD Amount     AS DEC
   FIELD MsSeq      AS INT
   FIELD MinBase    AS DEC
   FIELD MaxAmount  AS DEC
   FIELD Used       AS DEC
   FIELD OrderId    AS INT
   INDEX MsSeq MsSeq. 

DEF TEMP-TABLE ttDPTarget NO-UNDO
   LIKE DPTarget.
 

FORM
   Invoice.InvNum   column-label "Invoice"
   Invoice.CustNum  column-label "Cust.nr"
   Invoice.CustName column-label "Customer name"  format "x(20)"
   liEventQty       column-label "Events"  FORMAT "->>>>>>>9"
   Invoice.AmtExclVAT column-label "Value ex. VAT"
WITH ROW 3 CENTERED OVERLAY 
   TITLE " HANDLING  Started " + 
         STRING(ldtBSDate,"99.99.99") + " " +
         STRING(liBSTime,"hh:mm:ss") + " " 
   12 DOWN FRAME LOG.


&IF "{&GetAllParams}" NE "NO"
&THEN
ASSIGN 
   RoundAcc    = fCParamI("RoundAcc")
   OTIntAcc    = fCParamI("OTIntAcc")
   OverPayAcc  = fCParamI("OverPayAcc")
   liCLossAcc  = fCParamI("CreditLossAcc")
   liCLossCust = fCParamI("CLossCustomer")
   lNotBilled  = fCParamC("ProdNoBill")
   AdvPaymAcc  = fCParamI("AdvPaymAcc")
   llBillInt   = (fCParamI("BillInterest") = 1)
   llInvComb   = (fCParamI("InvCustCombine") = 1)
   llAllowDbl  = (fCParamI("BillDblVat") = 1)
   lcIntItem   = fCParamC("InterestItem")
   llRunSpecFee = (fCParamI("BillRunSpecFee") = 1)
   llRunVolDisc = (fCParamI("BillRunVolDisc") = 1)
   lcZeroItem   = fCParamC("ZeroInvoiceItem")
   lcNoMinCons  = fCParamC("NoPostMinCons")
   lcMinConsFatime = fCParam("Billing","MinConsFatime").

IF lcMinConsFatime > "" THEN DO:
   DO liLoop = 1 TO NUM-ENTRIES(lcMinConsFatime):
      FIND FIRST FatGroup NO-LOCK WHERE
                 FatGroup.Brand = gcBrand AND
                 FatGroup.FtGrp = ENTRY(liLoop,lcMinConsFatime) NO-ERROR.
      IF AVAIL FatGroup AND FatGroup.BillCode > "" THEN
         lcMinConsFatimeBillCode = lcMinConsFatimeBillCode + "," + 
                                   FatGroup.BillCode.
   END.
   lcMinConsFatimeBillCode = SUBSTRING(lcMinConsFatimeBillCode,2).
END.

{Func/cparam2.i}
IF AdvPaymAcc = 0 THEN AdvPaymAcc = OverPayAcc. 
IF lcNoMinCons = ? THEN lcNoMinCons = "".
IF lcMinConsFatime = ? THEN lcMinConsFatime = "".
&ENDIF

&IF "{&InitPersistent}" NE "NO" AND "{&EDRHandling}" = "NO" 
&THEN
DEF VAR clsInvRowCounter AS Inv.billrun_invrowcounter NO-UNDO.
clsInvRowCounter = NEW Inv.billrun_invrowcounter(gcBrand).
&ENDIF

&IF "{&InitPersistent}" = "NO"
&THEN llRunVolDisc = FALSE.
&ENDIF

ASSIGN 
   ReceivAcc  = fCParamI("ReceivAcc")     
   liOwnUsePA = fCParamI("OwnUsePaymAcc")
   liVipUsePA = fCParamI("VipUsePaymAcc").
       
IF liOwnUsePA = ? THEN liOwnUsePA = 0.
IF liVipUsePA = ? THEN liVipUsePA = liOwnUsePA.
       
&IF "{&InitPersistent}" NE "NO" AND "{&EDRHandling}" NE "NO"
&THEN 
RUN pInitializeRerate IN fhRRHandle.
&ENDIF


FUNCTION fInvoiceItem RETURNS LOGICAL
  (INPUT iiType  AS INT,
   INPUT iiMsSeq AS INT,
   INPUT irRecId AS RECID):

   CREATE ttInvoiceItem.
   ASSIGN
      ttInvoiceItem.tType  = iiType
      ttInvoiceItem.MsSeq  = iiMsSeq
      ttInvoiceItem.tRecId = irRecId.
   
   RETURN TRUE.

END.
                          
FUNCTION fMarkInvoiceItems RETURNS DECIMAL
  (INPUT iiMsSeq  AS INT,
   INPUT iiInvNum AS INT,
   INPUT iiSubInv AS INT,
   INPUT iiInvType AS INT):
  
   DEF VAR lValue AS DECIMAL NO-UNDO.
   
   /* TYPE 1: mark ALL contract fee rows */
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 1 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
       EACH FFItem EXCLUSIVE-LOCK WHERE
            RECID(FFItem) = ttInvoiceItem.tRecId:
  
      ASSIGN
         FFItem.InvNum    = iiInvNum WHEN iiInvNum NE 0
         FFItem.SubInvNum = iiSubInv WHEN iiInvNum NE 0
         FFItem.BILLED    = FALSE    WHEN iiInvNum  = 0
         lValue           = lValue + FFItem.Amt.
  
   END.
  
   /* TYPE 2: mark ALL single fee rows */  
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 2 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
            EACH SingleFee EXCLUSIVE-LOCK WHERE
            RECID(SingleFee) = ttInvoiceItem.tRecId:
  
      IF llDoEvent AND iiInvType NE 99 THEN DO:
         RUN StarEventInitialize(lhSingleFee).
         RUN StarEventSetOldBuffer(lhSingleFee).
      END.
      
      ASSIGN
         SingleFee.InvNum    = iiInvNum WHEN iiInvNum NE 0
         SingleFee.SubInvNum = iiSubInv WHEN iiInvNum NE 0
         SingleFee.BILLED    = FALSE    WHEN iiInvNum  = 0
         lValue              = lValue + SingleFee.Amt.
          
      IF llDoEvent AND iiInvType NE 99
         THEN RUN StarEventMakeModifyEventWithMemo(
                              lhSingleFee,katun,"InvoiceCreation").

      /* if created within this run -> delete */
      IF iiInvNum = 0 AND SingleFee.CalcObj > "" AND
         SingleFee.CalcObj = lcBillRun
      THEN DO:
         IF llDoEvent AND iiInvType NE 99
            THEN RUN StarEventMakeDeleteEventWithMemo(
                              lhSingleFee,katun,"InvoiceCreation").
         DELETE SingleFee.
      END.
                             
   END.
  
   /* TYPE 3: mark ALL FATIME fee rows */
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 3 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
       EACH FATime EXCLUSIVE-LOCK WHERE
            RECID(FATime) = ttInvoiceitem.tRecId:
  
      ASSIGN
         FATime.InvNum    = iiInvNum WHEN iiInvNum NE 0
         FATime.SubInvNum = iiSubInv WHEN iiInvNum NE 0
         FATime.Used      = 0        WHEN iiInvNum = 0
         FATime.TransQty  = 0        WHEN iiInvNum = 0
         lValue           = lValue + FATime.Amt.
  
   END.
  
   /* TYPE 4: mark all interest records */
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 4 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
       EACH CustIntEvent EXCLUSIVE-LOCK WHERE
            RECID(CustIntEvent) = ttInvoiceitem.tRecId:
  
      ASSIGN
         CustIntEvent.BilledInvNum = iiInvNum WHEN iiInvNum NE 0
         CustIntEvent.BilledSubInv = iiSubInv WHEN iiInvNum NE 0
         lValue                    = lValue + CustIntEvent.Amt.
     
   END.

   /* TYPE 5: delete transferred FAT rows if invoice was rejected */
   IF iiInvNum = 0 THEN 
   FOR EACH ttInvoiceItem NO-LOCK WHERE
            ttInvoiceItem.tType = 5 AND
            (IF iiMsSeq = ?
             THEN TRUE
             ELSE ttInvoiceItem.MsSeq = iiMsSeq),
      FIRST FATime EXCLUSIVE-LOCK WHERE
            RECID(FATime) = ttInvoiceitem.tRecId:

      DELETE FATime.
   END. 

   IF iiInvNum = 0 THEN 
   FOR EACH ttInvSeq WHERE
      (IF iiMsSeq = ?
       THEN TRUE
       ELSE ttInvSeq.MsSeq = iiMsSeq):
      FOR FIRST InvSeq EXCLUSIVE-LOCK USE-INDEX InvSeq WHERE
                InvSeq.InvSeq = ttInvSeq.InvSeq AND
                InvSeq.Billed = TRUE AND
                InvSeq.InvNum = 0:
          InvSeq.Billed = FALSE.
      END.
   END.    
   RETURN lValue.

END.

FUNCTION fErrorLog RETURNS LOGIC
  (iiCustNum   AS INT,
   icCLI       AS CHAR,     
   iiITGroupID AS INT,
   icError     AS CHAR):
 
   /* save to db for reporting */
   CREATE ErrorLog.
   ASSIGN ErrorLog.Brand     = gcBrand
          ErrorLog.ActionID  = "BRUN"
          ErrorLog.TableName = "Customer"
          ErrorLog.KeyValue  = STRING(iiCustnum)
          ErrorLog.ErrorChar = icCLI 
          ErrorLog.ErrorMsg  = icError + CHR(10) + 
                               "Target group: " + STRING(iiITGroupID)
          ErrorLog.UserCode  = katun
          ErrorLog.ActionTS  = fMakeTS().
    
END FUNCTION.

FUNCTION IsFATAllowed RETURNS LOGICAL (INPUT icFTGrp     AS CHAR,
                                       INPUT icBillCode  AS CHAR):
   DEF BUFFER bBillItem   FOR BillItem.
   DEF BUFFER bBItemGroup FOR BItemGroup.

   IF icBillCode > "" AND
      (CAN-FIND(FIRST FATConfig WHERE
                      FATConfig.Brand      = gcBrand        AND
                      FATConfig.FTGrp      = icFtGrp AND
                      FATConfig.ConfType   = 6              AND
                      FATConfig.ConfTarget = "BillingItem"  AND
                      FATConfig.ValidFrom <= TODAY          AND
                      FATConfig.ValidTo   >= TODAY) OR
       CAN-FIND(FIRST FATConfig WHERE
                      FATConfig.Brand      = gcBrand        AND
                      FATConfig.FTGrp      = icFtGrp AND
                      FATConfig.ConfType   = 7              AND
                      FATConfig.ConfTarget = "BillingItemG" AND
                      FATConfig.ValidFrom <= TODAY          AND
                      FATConfig.ValidTo   >= TODAY)) THEN DO:

      FOR FIRST bBillItem WHERE
                bBillItem.Brand    = gcBrand    AND
                bBillItem.BillCode = icBillCode NO-LOCK,
          FIRST bBItemGroup WHERE
                bBItemGroup.Brand   = gcBrand AND
                bBItemGroup.BIGroup = bBillItem.BIGroup NO-LOCK:
          IF CAN-FIND(FIRST FATConfig WHERE
                            FATConfig.Brand      = gcBrand        AND
                            FATConfig.FTGrp      = icFTGrp        AND
                            FATConfig.ConfType   = 6              AND
                            FATConfig.ConfTarget = "BillingItem"  AND
                            FATConfig.ValidFrom <= TODAY          AND
                            FATConfig.ValidTo   >= TODAY          AND
                            LOOKUP(bBillItem.BillCode,FATConfig.ConfRule1) > 0)
          THEN RETURN FALSE.
          IF CAN-FIND(FIRST FATConfig WHERE
                            FATConfig.Brand      = gcBrand        AND
                            FATConfig.FTGrp      = icFTGrp        AND
                            FATConfig.ConfType   = 7              AND
                            FATConfig.ConfTarget = "BillingItemG" AND
                            FATConfig.ValidFrom <= TODAY          AND
                            FATConfig.ValidTo   >= TODAY          AND
                            LOOKUP(bBItemGroup.BIGroup,FATConfig.ConfRule1) > 0)
          THEN RETURN FALSE.
      END. /* FOR FIRST bBillItem WHERE */

   END. /* IF icBillCodes > "" AND */

   RETURN TRUE.

END FUNCTION. /* FUNCTION IsFATCONFRule RETURNS LOGIC */

FUNCTION fMakeFATimeRow RETURNS LOGICAL
   (INPUT-OUTPUT idAmt   AS DEC,
    INPUT-OUTPUT iiQty   AS INT,
    INPUT        iiMsSeq AS INT,
    INPUT        iiAgrCust AS INT,
    INPUT        idVatPerc AS DEC,
    INPUT        idaTodate AS DATE):

   DEF VAR ldtPerDate  AS DATE NO-UNDO.
   DEF VAR DiscAmt     AS DEC  NO-UNDO.
   DEF VAR DiscQty     AS INT  NO-UNDO.
   DEF VAR ldBaseAmt   AS DEC  NO-UNDO.
   
   IF FATime.TransPeriod > 0 THEN DO:
      IF FATime.TransPeriod <= liOldEventPeriod THEN RETURN FALSE.
   END.
   ELSE IF FATime.Period <= liOldEventPeriod THEN RETURN FALSE.

   IF idAmt <= 0 OR
      (FATime.Amt > 0 AND FATime.amt - FATime.used - FATime.TransQty <= 0) OR
      (FATime.FatPerc > 0 AND FATime.Used > 0)
   THEN RETURN FALSE.

   /* check that period is valid */ 
   ldtPerDate = fInt2Date(Fatime.Period,1).
   IF ldtPerDate = ? THEN DO:
      ASSIGN lRejBill    = TRUE
             lcRejReason = "FATPeriod"
             lcRejCLI    = FATime.CLI.
      RETURN FALSE.
   END. 

   ASSIGN 
      DiscAmt = 0
      DiscQty = 0
      ldBaseAmt = idAmt.
      
   /* use same vat method for the base amount as fatime has */
   IF Fatime.VatIncl NE lCustVat THEN DO:
      IF Fatime.VatIncl THEN 
         ldBaseAmt = ROUND(ldBaseAmt * (1 + idVatPerc / 100),3).
      ELSE ldBaseAmt = ROUND(ldBaseAmt / (1 + idVatPerc / 100),3).
   END.

   FIND bFatime WHERE RECID(bFatime) = RECID(Fatime) EXCLUSIVE-LOCK.

   /* Calculate amount of discount */
   IF FATime.QtyUnit = "Amt" THEN DO:

      /* fixed amount */  
      IF FATime.Amt > 0 THEN DO:
         DiscAmt = FATime.Amt - FATime.Used - FATime.TransQty.

         IF ldBaseAmt >= DiscAmt 
         THEN ASSIGN iiQty     = iiQty * (1 - (DiscAmt / ldBaseAmt))
                     ldBaseAmt = ldBaseAmt - DiscAmt.

         ELSE ASSIGN DiscAmt   = ROUND(ldBaseAmt,3)
                     ldBaseAmt = 0
                     iiQty     = 0.
      END.
      
      /* percentage */  
      ELSE DO:
         ASSIGN DiscAmt   = ROUND(FATime.FatPerc * ldBaseAmt / 100,3)
                iiQty     = iiQty * (1 - (DiscAmt / ldBaseAmt))
                ldBaseAmt = ldBaseAmt - DiscAmt.
      END.
      
      bFATime.Used = bFATime.Used + DiscAmt.
   END.

   ELSE IF FATime.QtyUnit = "Qty" AND
           FATime.FATType NE 2
   THEN DO:

      IF iiQty >= FATime.Amt - FATime.Used - FATime.TransQty
      THEN ASSIGN DiscQty   = FATime.Amt - FATime.Used - FATime.TransQty
                  DiscAmt   = ROUND((ldBaseAmt / iiQty) * DiscQty,3)
                  ldBaseAmt = ldBaseAmt - DiscAmt
                  iiQty     = iiQty - DiscQty.

      ELSE IF iiQty > 0
      THEN ASSIGN DiscAmt   = ROUND(ldBaseAmt,3) 
                  ldBaseAmt = 0
                  DiscQty   = iiQty
                  iiQty     = 0.

      ELSE RETURN FALSE. 

      bFATime.Used = bFATime.Used + DiscQty.
   END.

   ELSE RETURN FALSE.

   IF DiscAmt = 0 AND DiscQty = 0 THEN RETURN FALSE.
   
   /* return the base amount with correct vat */
   IF Fatime.VatIncl NE lCustVat THEN DO:
      IF lCustVat THEN 
         ldBaseAmt = ROUND(ldBaseAmt * (1 + idVatPerc / 100),3).
      ELSE ldBaseAmt = ROUND(ldBaseAmt / (1 + idVatPerc / 100),3).
   END.
   idAmt = ldBaseAmt.
   
   FIND FIRST ttIR WHERE
              ttIR.BillCode = FatGroup.BillCode AND
              ttIR.CLI      = Fatime.CLI        AND      
              ttIR.CCN      = 0                 AND
              ttIR.Period   = 0                 AND
              ttIR.ToDate   = idaTodate         AND
              ttIR.VatIncl  = Fatime.VatIncl    AND
              ttIR.RowType  = 7                 AND
              ttIR.AgrCust  = iiAgrCust NO-ERROR.

   IF NOT AVAIL ttIR THEN DO:
      CREATE ttIR.
      ASSIGN ttIR.FromDate  = ?
             ttIR.ToDate    = idaTodate
             ttIR.VatIncl   = Fatime.VatIncl
             ttIR.BillCode  = FatGroup.BillCode
             ttIR.CCN       = 0
             ttIR.Period    = 0
             ttIR.CLI       = Fatime.CLI 
             ttIR.MsSeq     = iiMsSeq
             ttIR.FFRow     = FALSE
             ttIR.RowType   = 7
             ttIR.AgrCust   = iiAgrCust
             ttIR.Qty       = 1.
   END.

   ttIR.dnet = ttIR.dnet + (DiscAmt * -1).
  
   fInvoiceItem(3,
                ttIR.MsSeq,
                RECID(bFATime)).
   
   RELEASE bFatime.
   
   RETURN TRUE. 

END FUNCTION.

/* transfer remaining fat amount to be used later */
FUNCTION fTransFatime RETURNS LOGICAL
   (iiMsSeq AS INT,
    iiFeePeriod AS INT):
 
   DEF VAR ldFatRemain AS DEC NO-UNDO.
   
   IF Fatime.Transfer = FALSE THEN RETURN FALSE.
   
   IF FATime.Amt > 0 
   THEN ldFatRemain = FATime.Amt - FATime.Used - FATime.TransQty.
   ELSE ldFatRemain = 0.
   
   IF ldFatRemain > 0 AND ldFatRemain NE FATime.Amt THEN DO:

      CREATE xFATime.
      BUFFER-COPY FATime EXCEPT FATime.FATnum TO xFATime.
         
      ASSIGN xFATime.FatNum      = NEXT-VALUE(ftseq)
             xFATime.OrigFat     = FATime.FatNum
             xFATime.TransPeriod = iiFeePeriod
             xFatime.Amt         = ldFatRemain
             xFATime.Invnum      = 0
             xFATime.Used        = 0
             xFATime.TransQty    = 0.
      /* no need to alter period, because transferrables are all checked
         regardless of how old their period is */
              
      FIND bFatime WHERE RECID(bFatime) = RECID(Fatime) EXCLUSIVE-LOCK.
      bFATime.TransQty = bFatime.TransQty + ldFatRemain.
      RELEASE bFatime.       

      /* these need to be removed in case invoice is rejected */
      fInvoiceItem(5,
                   iiMsSeq,
                   RECID(xFATime)).
      
      RELEASE xFATime.
   END.
 
   RETURN (ldFatRemain > 0).
   
END FUNCTION.

FUNCTION fInvRowVAT RETURNS LOGICAL
   (iiVATUsage AS INT,
    icRegion   AS CHAR,
    icCategory AS CHAR,
    idaInvDate AS DATE):
   
   FOR EACH ttIR NO-LOCK:
   
      /* determine vat codes and percents (if SlsAccNum > 0 -> already handled)
      */
      IF ttIR.SlsAccNum = 0 THEN DO:
            
         ASSIGN ttIR.VatPerc = 0
                ttIR.VatCode = 0.
                  
         FIND BillItem NO-LOCK WHERE 
              BillItem.Brand    = gcBrand AND
              BillItem.BillCode = ttir.BillCode NO-ERROR.
         IF NOT AVAILABLE BillItem THEN NEXT. /* will be rejected later on */

         /* set this as default, also for vatusage 3/4 in case prices
            including vat have been used -> remove vat using domestic code,
            vatcode is emptied from row before invoice creation (pCounterVat
            needs also ttIr.vc-perc)
         */
         ASSIGN 
            ttIR.VatCode  = fRegionTaxCode(icRegion,
                                           BillItem.TaxClass,
                                           idaInvDate)
            ttIR.TaxClass = BillItem.TaxClass 
            ttIR.DiscBase = ttIR.dNet
            ttIR.DiscQty  = ttIR.Qty
            
            ttIR.SlsAccNum = fInvRowAccount(icCategory,
                                            iiVATUsage).
                                         
         IF ttIR.VatCode > 0 THEN DO:
            FIND VatCode NO-LOCK WHERE 
                 VATCode.VatCode = ttIR.VatCode NO-ERROR.
            IF AVAILABLE VatCode 
            THEN ttIR.VatPerc = VatCode.VatPerc.
         END.    
      END.

      /* make sure that all rows use the same vat method (incl/excl) */
      IF ttIR.VatIncl NE lCustVat THEN DO:

         IF lCustVat = TRUE THEN ASSIGN 
            ttIR.dgross   = ttIR.dgross   * (1 + ttIR.VatPerc / 100)
            ttIR.dnet     = ttIR.dnet     * (1 + ttIR.VatPerc / 100)
            ttIR.DiscBase = ttIR.DiscBase * (1 + ttIR.VatPerc / 100).
         ELSE ASSIGN
            ttIR.dgross   = ttIR.dgross   / (1 + ttIR.VatPerc / 100)
            ttIR.dnet     = ttIR.dnet     / (1 + ttIR.VatPerc / 100)
            ttIR.DiscBase = ttIR.DiscBase / (1 + ttIR.VatPerc / 100).

         FIND FIRST ttInvSplit WHERE
                    ttInvSplit.AgrCust = ttIR.AgrCust AND
                    ttInvSplit.MsSeq   = ttIR.MsSeq NO-ERROR.

         IF AVAIL ttInvSplit THEN DO:
            
            IF ttIR.Todate < ttInvSplit.SplitDate THEN
               FIND FIRST bttIR WHERE
                          bttIR.BillCode = ttIR.BillCode AND
                          bttIR.CLI      = ttIR.CLI AND
                          bttIR.CCN      = ttIR.CCN AND
                          bttIR.Period   = ttIR.Period   AND
                          bttIR.VatIncl  = lCustVat      AND
                          bttIR.RowType  = ttIR.RowType  AND
                          bttIR.AgrCust  = ttIR.AgrCust  AND
                          bttIR.SlsAccNum > 0 AND
                          bttIR.ToDate   < ttInvSplit.SplitDate NO-ERROR.
            ELSE
               FIND FIRST bttIR WHERE
                          bttIR.BillCode = ttIR.BillCode AND
                          bttIR.CLI      = ttIR.CLI AND
                          bttIR.CCN      = ttIR.CCN AND
                          bttIR.Period   = ttIR.Period   AND
                          bttIR.VatIncl  = lCustVat      AND
                          bttIR.RowType  = ttIR.RowType  AND
                          bttIR.AgrCust  = ttIR.AgrCust  AND
                          bttIR.SlsAccNum > 0 AND
                          bttIR.ToDate  >= ttInvSplit.SplitDate NO-ERROR.
         END.
         ELSE
            FIND FIRST bttIR WHERE
                       bttIR.BillCode = ttIR.BillCode AND
                       bttIR.CLI      = ttIR.CLI AND
                       bttIR.CCN      = ttIR.CCN AND
                       bttIR.Period   = ttIR.Period   AND
                       bttIR.VatIncl  = lCustVat      AND
                       bttIR.RowType  = ttIR.RowType  AND
                       bttIR.AgrCust  = ttIR.AgrCust  AND
                       bttIR.SlsAccNum > 0 NO-ERROR.

         IF AVAIL bttIR THEN DO:

            ASSIGN
            bttIR.Qty        = bttIR.Qty        + ttIR.Qty  
            bttIR.Minutes    = bttIR.Minutes    + ttIR.Minutes
            bttIR.DataAmt    = bttIR.DataAmt    + ttIR.DataAmt
            bttIR.PeakMin    = bttIR.PeakMin    + ttIR.PeakMin
            bttIR.OffPeakMin = bttIR.OffPeakMin + ttIR.OffPeakMin
            bttIR.dgross     = bttIR.dgross     + ttIR.dgross
            bttIR.dnet       = bttIR.dnet       + ttIR.dnet
            bttIR.DiscBase   = bttIR.DiscBase   + ttIR.DiscBase
            bttIR.DiscQty    = bttIR.DiscQty    + ttIR.DiscQty
            bttIR.FromDate   = MIN(bttIR.FromDate,ttIR.FromDate)
            bttIR.ToDate     = MAX(bttIR.ToDate,ttIR.ToDate).

            DELETE ttIR.     

         END.

         ELSE ttIR.VatIncl = lCustVat.
      END.
      
   END.  /* TTIR */         

END FUNCTION.

FUNCTION fCombineInvRows RETURNS LOGIC:

   FOR EACH ttIR WHERE ttIR.CCN > 0:

      FIND FIRST ttInvSplit WHERE
                 ttInvSplit.AgrCust  = ttIR.AgrCust AND
                 ttInvSplit.MsSeq    = ttIR.MsSeq NO-ERROR.

      IF AVAIL ttInvSplit THEN DO:

         IF ttIR.Todate < ttInvSplit.SplitDate THEN
            FIND FIRST bttIR WHERE
                       bttIR.BillCode = ttIR.BillCode AND
                       bttIR.CLI      = ttIR.CLI      AND
                       bttIR.CCN      = 0             AND
                       bttIR.Period   = ttIR.Period   AND
                       bttIR.VatIncl  = ttIR.VatIncl  AND
                       bttIR.RowType  = ttIR.RowType  AND
                       bttIR.AgrCust  = ttIR.AgrCust  AND
                       bttIR.Todate   < ttInvSplit.SplitDate AND
                       bttIR.OrderId  = ttIR.OrderId NO-ERROR.
         ELSE
            FIND FIRST bttIR WHERE
                       bttIR.BillCode = ttIR.BillCode AND
                       bttIR.CLI      = ttIR.CLI      AND
                       bttIR.CCN      = 0             AND
                       bttIR.Period   = ttIR.Period   AND
                       bttIR.VatIncl  = ttIR.VatIncl  AND
                       bttIR.RowType  = ttIR.RowType  AND
                       bttIR.AgrCust  = ttIR.AgrCust  AND
                       bttIR.Todate  >= ttInvSplit.SplitDate AND
                       bttIR.OrderId  = ttIR.OrderId NO-ERROR.
      END.
      ELSE 
         FIND FIRST bttIR WHERE
                    bttIR.BillCode = ttIR.BillCode AND
                    bttIR.CLI      = ttIR.CLI      AND
                    bttIR.CCN      = 0             AND
                    bttIR.Period   = ttIR.Period   AND
                    bttIR.VatIncl  = ttIR.VatIncl  AND
                    bttIR.RowType  = ttIR.RowType  AND
                    bttIR.AgrCust  = ttIR.AgrCust  AND
                    bttIR.OrderId  = ttIR.OrderId NO-ERROR.

      IF AVAIL bttIR THEN DO:
         ASSIGN
            bttIR.Qty        = bttIR.Qty        + ttIR.Qty  
            bttIR.Minutes    = bttIR.Minutes    + ttIR.Minutes
            bttIR.DataAmt    = bttIR.DataAmt    + ttIR.DataAmt
            bttIR.PeakMin    = bttIR.PeakMin    + ttIR.PeakMin
            bttIR.OffPeakMin = bttIR.OffPeakMin + ttIR.OffPeakMin
            bttIR.dgross     = bttIR.dgross     + ttIR.dgross
            bttIR.dnet       = bttIR.dnet       + ttIR.dnet
            bttIR.DiscBase   = bttIR.DiscBase   + ttIR.DiscBase
            bttIR.DiscQty    = bttIR.DiscQty    + ttIR.DiscQty
            bttIR.FromDate   = MIN(bttIR.FromDate,ttIR.FromDate)
            bttIR.ToDate     = MAX(bttIR.ToDate,ttIR.ToDate).

         DELETE ttIR.     
      END.
      ELSE ttIR.CCN = 0.
   END.

END FUNCTION.

/* get next external invoice id */
FUNCTION fLocalNextExtID RETURNS CHARACTER
   (icSeqPrefix AS CHAR,
    icExtInvID  AS CHAR):

   DEF VAR liSeqInvNum AS INT NO-UNDO.
   
   /* remove prefix (don't use replace) */
   IF icSeqPrefix > "" AND icExtInvID BEGINS icSeqPrefix THEN DO:
      IF LENGTH(icExtInvID) > LENGTH(icSeqPrefix)
      THEN icExtInvID = SUBSTRING(icExtInvID,LENGTH(icSeqPrefix) + 1).
      ELSE icExtInvID = "".
   END.
         
   liSeqInvNum = INTEGER(icExtInvID) NO-ERROR.
         
   /* invalid integer value */
   IF ERROR-STATUS:ERROR THEN RETURN "".

   RETURN icSeqPrefix + STRING(liSeqInvNum + 1,"99999999").
   
END FUNCTION.

/* get the group into which subinvoice belongs */
FUNCTION fGetInvoiceTargetGroup RETURNS INTEGER
   (iiMsSeq    AS INT,
    iiAgrCust  AS INT,
    iiInvCust  AS INT,
    idaInvDate AS DATE,
    OUTPUT oiDelType AS INT):
    
   DEF VAR liGroupID AS INT  NO-UNDO.
       
   /* group subscriptions (or plain customer events) with no target definition
      to same invoice */
   liGroupID = -1. 

   /* subscription based */
   IF iiMsSeq > 0 THEN DO:
      FOR EACH InvoiceTarget NO-LOCK WHERE
               InvoiceTarget.MsSeq     = iiMsSeq    AND
               InvoiceTarget.ToDate   >= idaInvDate AND
               InvoiceTarget.FromDate <= idaInvDate,
         FIRST InvoiceTargetGroup NO-LOCK WHERE
               InvoiceTargetGroup.ITGroupID = InvoiceTarget.ITGroupID AND
               InvoiceTargetGroup.AgrCust   = iiAgrCust AND
               InvoiceTargetGroup.CustNum   = iiInvCust:
         ASSIGN
           liGroupID = InvoiceTarget.ITGroupID
           oiDelType = InvoiceTargetGroup.DelType WHEN
                       InvoiceTargetGroup.DelType NE ? AND
                       InvoiceTargetGroup.DelType > 0.
         LEAVE.
      END.

      /* get last active if none exist anymore */
      IF liGroupID < 0 THEN 
      FOR EACH InvoiceTarget NO-LOCK WHERE
               InvoiceTarget.MsSeq = iiMsSeq,
         FIRST InvoiceTargetGroup NO-LOCK WHERE
               InvoiceTargetGroup.ITGroupID = InvoiceTarget.ITGroupID AND
               InvoiceTargetGroup.AgrCust   = iiAgrCust AND
               InvoiceTargetGroup.CustNum   = iiInvCust
      BY InvoiceTarget.ToDate DESC:
         liGroupID = InvoiceTarget.ITGroupID.
         LEAVE.
      END.
   END.
   
   /* customer based */
   ELSE DO:
      FOR FIRST InvoiceTargetGroup NO-LOCK WHERE
                InvoiceTargetGroup.Brand    = gcBrand     AND
                InvoiceTargetGroup.AgrCust  = iiAgrCust   AND
                InvoiceTargetGroup.CustNum  = iiInvCust   AND
                InvoiceTargetGroup.DefaultGroup = TRUE    AND
                InvoiceTargetGroup.ToDate   >= idaInvDate AND
                InvoiceTargetGroup.FromDate <= idaInvDate:
         liGroupID = InvoiceTargetGroup.ITGroupID.
      END.

      /* get first active if no default defined */
      IF liGroupID < 0 THEN 
      FOR FIRST InvoiceTargetGroup NO-LOCK WHERE
                InvoiceTargetGroup.Brand    = gcBrand     AND
                InvoiceTargetGroup.AgrCust  = iiAgrCust   AND
                InvoiceTargetGroup.CustNum  = iiInvCust   AND
                InvoiceTargetGroup.ToDate   >= idaInvDate AND
                InvoiceTargetGroup.FromDate <= idaInvDate:
         liGroupID = InvoiceTargetGroup.ITGroupID.
      END.
   END.
     
   RETURN liGroupID.
         
END FUNCTION.

FUNCTION fGetDiscounts RETURNS LOGIC
   (icTable      AS CHAR,
    icKey        AS CHAR,
    icSubject    AS CHAR,
    idaValidFrom AS DATE,
    idaValidTo   AS DATE,
    iiMsSeq      AS INT):

   FOR EACH DPMember NO-LOCK WHERE
            DPMember.HostTable  = icTable  AND 
            DPMember.KeyValue   = icKey    AND
            DPMember.ValidFrom <= idaValidTo AND
            DPMember.ValidTo   >= idaValidFrom AND
            DPMember.ValidTo >= DPMember.ValidFrom,
      FIRST DiscountPlan NO-LOCK WHERE 
            DiscountPlan.DPId = DPMember.DPId AND
            DiscountPlan.Subject = icSubject
   BY DPMember.ValidTo DESC:         
            
      CREATE ttDiscounts.
      ASSIGN 
         ttDiscounts.DPId        = DiscountPlan.DPId
         ttDiscounts.BillCode    = DiscountPlan.BillCode
         ttDiscounts.ProcStopper = DiscountPlan.ProcessStopper
         ttDiscounts.Priority    = DiscountPlan.Priority
         ttDiscounts.TargetType  = DiscountPlan.TargetType
         ttDiscounts.Unit        = DiscountPlan.DPUnit
         ttDiscounts.MinBase     = DiscountPlan.MinBaseAmount
         ttDiscounts.MaxAmount   = DiscountPlan.MaxAmount
         ttDiscounts.FromDate    = DPMember.ValidFrom
         ttDiscounts.ToDate      = DPMember.ValidTo
         ttDiscounts.Amount      = DPMember.DiscValue
         ttDiscounts.OrderId     = DPMember.OrderId
         ttDiscounts.MsSeq       = iiMsSeq.
      
      IF ttDiscounts.Amount = 0 THEN DO:
         FIND FIRST DPRate WHERE 
            DPRate.DPId = DiscountPlan.DPId AND
            DPRate.ValidFrom <= DPMember.ValidTo AND
            DPRate.ValidTo   >= DPMember.ValidFrom NO-LOCK NO-ERROR.
         IF AVAILABLE DPRate THEN ttDiscounts.Amount = DPRate.DiscValue.
         
         IF ttDiscounts.Amount = 0 THEN DELETE ttDiscounts.
      END.
   END.

END FUNCTION.

FUNCTION fGetMandateForITGroup RETURNS CHAR
      (iiITGroup  AS INT,
       iiCustnum AS INT,
       idaFromPer  AS DATE,
       idaToPer  AS DATE):

   DEF VAR ldaMDate AS DATE NO-UNDO INIT 1/1/2049.
   DEF VAR lcMandateId AS CHAR NO-UNDO. 

   DEF VAR idFromPer AS DEC NO-UNDO. 
   DEF VAR idToPer AS DEC NO-UNDO. 

   DEF BUFFER MsOwner FOR MsOwner.

   idFromPer = fMake2Dt(idaFromPer,0).
   idToPer = fMake2Dt(idaToPer,86399).

   FOR EACH ttSubInv NO-LOCK WHERE
            ttSubInv.ITGroup = iiITGroup:

       FIND FIRST MsOwner NO-LOCK WHERE /* YTS-7139 */
                  MsOwner.MsSeq   =  ttSubInv.MsSeq   AND
                  MsOwner.TsBeg   <= idToPer          AND
                  MsOwner.TsEnd   >= idFromPer        AND
                  MsOwner.CustNum =  ttSubInv.CustNum AND
                  MsOwner.PayType = FALSE NO-ERROR.
                  
       IF NOT AVAIL MsOwner THEN
          FIND FIRST MsOwner NO-LOCK WHERE
                     MsOwner.CustNum = iiCustNum AND
                     MsOwner.CLI = ttSubInv.CLI AND
                     MsOwner.PayType = FALSE AND
                     MsOwner.TsEnd < idFromPer AND
                     MsOwner.MsSeq = ttSubInv.MsSeq USE-INDEX Custnum_s NO-ERROR.

      IF NOT AVAIL MsOwner THEN NEXT.

      IF MsOwner.MandateDate EQ ? THEN NEXT.

      IF ldaMDate > MsOwner.MandateDate THEN ASSIGN
         ldaMDate = MsOwner.MandateDate 
         lcMandateId = MsOwner.MandateID.
   END.
   /* Mandate generation in a fly for cases when no Mandate available. 
      This came via YDR-2057 */
   IF lcMandateId = "" THEN DO:
      lcMandateId = STRING(iiCustNum) + "X" +
                    FILL("0",29 - LENGTH(STRING(iiCustNum))).
      fErrorLog(iiCustNum,
                "",
                iiITGroup,
                "Temporary MandateId generation, when no MandateId available").
      fELog(katun,"INVOICE:MandateId:Customer:" + 
                  STRING(iiCustNum)).

   END.
   RETURN lcMandateId.

END FUNCTION.

PROCEDURE pCreateInv:

   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiFeePeriod AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaIberPayDueDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaEBADueDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFusionDueDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER ilRerate    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiInvType   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icBillRun   AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER ilSetExtID  AS LOG  NO-UNDO.

   DEF BUFFER bCustomer    FOR Customer.
   DEF BUFFER bEventCust   FOR Customer.

   DEF VAR ldBasis     AS DEC  NO-UNDO.
   DEF VAR liCnt       AS INT  NO-UNDO. 
   DEF VAR lcInfo      AS CHAR NO-UNDO. 
   DEF VAR ldFromPer   AS DEC  NO-UNDO.
   DEF VAR ldToPer     AS DEC  NO-UNDO. 
   DEF VAR lcMobRep    AS CHAR NO-UNDO. 
   DEF VAR liCustQty   AS INT  NO-UNDO.
   DEF VAR ldVatPerc   AS DEC  NO-UNDO.
   DEF VAR ldBillPer   AS DEC  NO-UNDO EXTENT 2. 
   DEF VAR llHandle    AS LOG  NO-UNDO. 
   DEF VAR liBillPer   AS INT  NO-UNDO.
   DEF VAR lcResult    AS CHAR NO-UNDO. 
   DEF VAR liRunStatus AS INT  NO-UNDO.
   DEF VAR liPause     AS INT  NO-UNDO.
   DEF VAR liRetries   AS INT  NO-UNDO INIT 20.
   DEF VAR ldaDiscFrom AS DATE NO-UNDO. 
   DEF VAR liITGroupID AS INT  NO-UNDO.
   DEF VAR liITGDeltype AS INT NO-UNDO.
   DEF VAR liInvseq    AS INT NO-UNDO. 
   DEF VAR idaDueDate  AS DATE NO-UNDO.
   DEF VAR llMultipleDueDate AS LOG NO-UNDO.
   DEF VAR lcRegion AS CHAR NO-UNDO. 
   
   ASSIGN ldtBSDate     = TODAY
          liBSTime      = TIME
          liLastCreated = 0
          ldBillPer[1]  = fMake2Dt(idaFromDate,0)
          ldBillPer[2]  = fMake2Dt(idaToDate,86399)
          liBillPer     = TRUNCATE(ldBillPer[1] / 100,0)
          llSetExtInvID = ilSetExtID WHEN ilSetExtID NE ?.

   ldaOldEventDate = fOldUnbilledEventLimit(INT(iiInvType = 1)).
   IF ldaOldEventDate = ? THEN ldaOldEventDate = 1/1/2006.
   liOldEventPeriod = YEAR(ldaOldEventDate) * 100 + MONTH(ldaOldEventDate).
 
   HIDE MESSAGE NO-PAUSE.
   PAUSE 0.
 
   /* id for this run */
   lcBillRun = icBillRun.
     
   IF lcBillRun BEGINS "*" THEN lcBillRun = SUBSTRING(lcBillRun,2).
   ELSE DO:
      IF lcBillRun = "" THEN lcBillRun = "BR".
   
      lcBillRun = lcBillRun +
                  STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") +
                  STRING(DAY(TODAY),"99") + STRING(TIME,"99999").
   END.
   
   IF iiInvType = 0 THEN iiInvType = 1.
   llCashInvoice = (iiInvType >= 6 AND iiInvType <= 7).

   IF idaEBADueDate = ? AND idaIberPayDueDate <> ? THEN
      idaEBADueDate = idaIberPayDueDate.
   IF idaIberPayDueDate = ? AND idaEBADueDate <> ? THEN
      idaIberPayDueDate = idaEBADueDate.

   IF idaIberPayDueDate <> ? AND idaEBADueDate <> ? AND
      idaIberPayDueDate <> idaEBADueDate THEN
      llMultipleDueDate = TRUE.

   &IF "{&InitPersistent}" NE "NO" AND 
       "{&EDRHandling}" NE "NO" 
   &THEN 
   IF ilRerate THEN RUN pInitializeRerateReport IN fhRRHandle (
         katun,
         idaFromDate, /* from */
         idaToDate, /* to */
         lcBillRun). /* icInvRunCode*/
   &ENDIF

   DO liRunStatus = 0 TO liRetries:
   
   liPause = 0.
    
   INVRUNLOOP:
   FOR EACH ttInvCust WHERE 
            ttInvCust.RunStatus = liRunStatus,
      FIRST bCustomer NO-LOCK WHERE
            bCustomer.CustNum = ttInvCust.CustNr,
      FIRST InvGroup  NO-LOCK WHERE
            InvGroup.Brand    = bCustomer.Brand AND
            InvGroup.InvGroup = bCustomer.InvGroup:

      /* end invoice has already been created */  
      IF bCustomer.EndInvType > 0 THEN NEXT.

      liCustQty = liCustQty + 1.
      
      IF NOT SESSION:BATCH AND liCustQty MOD 10 = 0 THEN
      PUT SCREEN row 18 col 2 STRING(liCustQty,">>>>>>9") + "/" +
                              TRIM(STRING(iiCustQty,">>>>>>9")).

      /* invoices */
      EMPTY TEMP-TABLE ttInv.
      EMPTY TEMP-TABLE ttSubInv.
      /* invoice rows */
      EMPTY TEMP-TABLE ttIR.
      /* CLI counters */ 
      EMPTY TEMP-TABLE ttCLI.
      /* vat amounts */
      EMPTY TEMP-TABLE ttRowVat.
      /* invoice items */
      EMPTY TEMP-TABLE ttInvoiceItem.
      /* event customers */
      EMPTY TEMP-TABLE ttEventCust.
      /* invseq handling */
      EMPTY TEMP-TABLE ttInvSeq.
      /* balances */
      EMPTY TEMP-TABLE ttCustBal.
      /* discount plan */
      EMPTY TEMP-TABLE ttDiscounts.
      /* immediate stc split */
      EMPTY TEMP-TABLE ttInvSplit.

      ASSIGN
         gcBrand = bCustomer.Brand
         lcRegion = bCustomer.Region.
   
      IF llCashInvoice AND liOrderID > 0 THEN DO:
         FIND FIRST OrderCustomer NO-LOCK WHERE   
                    OrderCustomer.Brand = gcBrand AND
                    OrderCustomer.OrderId = liOrderID AND
                    OrderCustomer.RowType = 1 NO-ERROR.
         IF AVAIL OrderCustomer THEN
            lcRegion = OrderCustomer.Region.
      END.

      /* get a list of subscriptions that should not be billed */
      lcBillDeny = "".
      IF iiInvType = 1 OR iiInvType = 99 THEN 
      FOR EACH Limit NO-LOCK WHERE
               Limit.CustNum   = bCustomer.CustNum AND
               Limit.LimitType = 3                 AND
               Limit.TMRuleSeq = 0                 AND
               Limit.LimitID   = 0                 AND
               Limit.ToDate   >= idaInvDate        AND
               Limit.FromDate <= idaInvDate        AND
               Limit.LimitAmt > 0:
         lcBillDeny = lcBillDeny + (IF lcBillDeny > "" THEN "," ELSE "") +
                      STRING(Limit.MsSeq).
      END.          
      
      &IF "{&InitPersistent}" NE "NO" AND
          "{&EDRHandling}" NE "NO"
      &THEN 
      
      /* if all subscriptions that have cdrs on the period are in the denial
         list then skip double check and rerate */
      llHandle = FALSE.
      IF iiInvType = 1 OR iiInvType = 99 THEN 
      FOR EACH MsOwner NO-LOCK USE-INDEX InvCust WHERE
               MsOwner.InvCust = bCustomer.CustNum AND   
               MsOwner.TsEnd  >= ldBillPer[1] AND
               MsOwner.PayType = FALSE,
          EACH SaldoCounter NO-LOCK WHERE
               SaldoCounter.MsSeq  = MsOwner.MsSeq AND
               SaldoCounter.Period = liBillPer:
 
         IF LOOKUP(STRING(MsOwner.MsSeq),lcBillDeny) = 0 THEN 
            llHandle = TRUE.

         /* if not handled now then schedule rerate for later; own request
            for each subscription because handling of customer based request
            could take a long time */
         ELSE DO:
            fRerateRequest(bCustomer.CustNum,
                           MsOwner.MsSeq,
                           idaFromDate,
                           idaToDate,
                           FALSE,    /* wait for other possible subrequests */
                           TRUE,     /* do double check */
                           fMake2Dt(TODAY + 2,1800),       /* activate */
                           "",      /* creator */
                           {&REQUEST_SOURCE_BILLING},     /* source */
                           0,
                           0,       /* not mandatory */
                           OUTPUT lcInfo).
         END.
      END.
      ELSE llHandle = TRUE.
      
      IF ilDouble AND llHandle THEN 
         RUN pDoubleCalls IN fhDCHandle (ttInvCust.CustNr).
       
      IF ilRerate AND llHandle THEN 
         RUN pRunRerate IN fhRRHandle (ttInvCust.CustNr, 
                                       idaFromDate, 
                                       idaToDate, 
                                       TRUE).   
      IF NOT SESSION:BATCH THEN PUT SCREEN ROW 2 col 78 "1".
         
      &ENDIF
         
      /* tax presentation method from customer,
         "including" is not sensible for vat0 sales */
      IF bCustomer.VatUsage >= 3 AND bCustomer.VatUsage <= 4
      THEN lCustVat = FALSE.
      ELSE lCustVat = bCustomer.VatIncl. 

      /* collect event customers = only invoice customer itself */
      CREATE ttEventCust.
      ttEventCust.CustNum = bCustomer.CustNum.

      ASSIGN
         liEventQty  = 0
         lRejBill    = FALSE
         lcRejReason = ""
         lcRejCLI    = ""
         liVASAcc    = 0
         liArAccNum  = 0.

      IF lcDepoItem = "" AND NOT llCashInvoice THEN DO:

         ASSIGN
            ldaFirstCall = idaFromDate
            lDBLVat = FALSE.

         /* get volume discount definitions; just empties temp-table for 
            voldiscs if customer hasn't got any */
         IF llRunVolDisc THEN 
            RUN pGetVolDiscDefinition IN fhVDHandle (bCustomer.CustNum,
                                                     idaFromDate,
                                                     idaToDate).
 
         &IF "{&InitPersistent}" NE "NO" AND "{&EDRHandling}" = "NO" 
         &THEN
         
         RUN pInvoiceSplit(bCustomer.Custnum,
                           idaFromDate,
                           idaToDate,
                           ldBillPer[1],
                           ldBillPer[2]).

         clsInvRowCounter:GetInvSeqs(bCustomer.CustNum,
                                     ldaOldEventDate,
                                     idaToDate,
                                     lcBillDeny,
                                     lcMsSeqList,
                                     OUTPUT TABLE ttInvSeq BY-REFERENCE).
                                     
         IF llRunVolDisc THEN 
         FOR EACH ttInvSeq:
            RUN pInitializeVolDisc IN fhVDHandle (ttInvSeq.InvSeq).
         END.    

         lcResult = clsInvRowCounter:MarkInvSeqs(
                        bCustomer.CustNum,
                        idaToDate,
                        INPUT TABLE ttInvSeq BY-REFERENCE).

         IF lcResult BEGINS "RETRY" THEN DO:

            /* there are unhandled rating or counter issues */
            ttInvCust.RunStatus = ttInvCust.RunStatus + 1.
            
            IF liRunStatus < liRetries THEN DO:
               
               /* if not the first try for this customer then try to 
                  perform queues directly */
               IF ttInvCust.RunStatus > 1 AND NUM-ENTRIES(lcResult,":") > 1
               THEN RUN pRatingQueues(bCustomer.CustNum,
                                      idaToDate,
                                      ENTRY(2,lcResult,":")).

               /* give other processes some time to do their thing */
               liPause = liRunStatus.
            END.
            ELSE DO:
               fErrorLog(bCustomer.CustNum,
                         "",
                         0,
                         "Pending rating/counter issues prevented billing").
               fELog(katun,"INVOICE:PENDING_RATING:Customer:" + 
                           STRING(bCustomer.CustNum)).
            END.
            NEXT INVRUNLOOP.
         END.

         IF CAN-FIND(FIRST ttInvSplit NO-LOCK WHERE
                           ttInvSplit.CLiEvent = "iSS") THEN DO:

            lcResult = clsInvRowCounter:SplitInvSeqs (
                           INPUT-OUTPUT TABLE ttInvSplit BY-REFERENCE,
                           INPUT-OUTPUT TABLE ttInvSeq BY-REFERENCE).

            IF lcResult BEGINS "ERROR" THEN DO:
               fErrorLog(bCustomer.CustNum,
                         "",
                         0,
                         "Erroneous billing counters iSS").
               fELog(katun,"INVOICE:ERRONEOUS_COUNTERS:Customer:" + 
                           STRING(bCustomer.CustNum)).
               /* mark invseqs back to unbilled*/ 
               fMarkInvoiceItems(?,0,0,iiInvType).
               NEXT INVRUNLOOP.
            END.
         END.
         
         lcResult = clsInvRowCounter:InvoiceRowsFromCounters(
                             INPUT bCustomer.CustNum,
                             INPUT idaFromDate,
                             INPUT idaToDate,
                             INPUT TABLE ttInvSeq BY-REFERENCE,
                             INPUT TABLE ttInvSplit BY-REFERENCE,
                             OUTPUT TABLE ttIR BY-REFERENCE,
                             OUTPUT TABLE ttCLI BY-REFERENCE,
                             INPUT-OUTPUT ldaFirstCall).
         
         IF lcResult BEGINS "ERROR" THEN DO:
            fErrorLog(bCustomer.CustNum,
                      "",
                      0,
                      "Erroneous billing counters").
            fELog(katun,"INVOICE:ERRONEOUS_COUNTERS:Customer:" + 
                        STRING(bCustomer.CustNum)).
            fMarkInvoiceItems(?,0,0,iiInvType).
            NEXT INVRUNLOOP.
         END.
            
         &ELSE
         RUN pCleanInvSeq(INPUT idaToDate, 
                          ttInvCust.CustNr).

         IF llRunVolDisc THEN 
         FOR EACH ttInvSeq:
            RUN pInitializeVolDisc IN fhVDHandle (ttInvSeq.InvSeq).
         END.    

         RUN pMobCDR
           (INPUT bCustomer.CustNum,
            INPUT idaFromDate,
            INPUT idaToDate,
            INPUT-OUTPUT ldaFirstCall).
         &ENDIF    

         /* duplicated VAT handling was found */
         IF NOT llAllowDbl AND lDBLVat THEN DO:
            fELog(katun,"INVOICE:DBLVAT:Customer:" + STRING(bCustomer.CustNum)).
            fErrorLog(bCustomer.CustNum,
                      "",  
                      0,
                      "DBLVAT").
            fMarkInvoiceItems(?,0,0,iiInvType).
            NEXT InvRunLoop.
         END.

         /* vat handling, run this already here so that counters (pCounterVat) 
            will get correct results */
         fInvRowVAT(bCustomer.VATUsage,
                    lcRegion,
                    bCustomer.Category,
                    idaInvDate). 
     
         &IF "{&EDRHandling}" NE "NO" 
         &THEN
         /* unify vat handling for counters */
         RUN pCounterVAT. 
         &ENDIF
 
      END. /* AVAIL InvSeq */

      /* if calls found -> create fee for call spec */
      IF llRunSpecFee AND
         CAN-FIND(FIRST ttIR) AND NOT llCashInvoice THEN 
      DO liCnt = 1 TO LENGTH(bCustomer.RepCodes):
         RUN Mc/creasfee.p (bCustomer.CustNum,
                         0,
                         idaToDate,
                         "InvSpec",
                         SUBSTRING(bCustomer.RepCodes,liCnt,1),
                         1,
                         ?,
                         "¤" + lcBillRun,
                         FALSE,
                         katun,
                         "InvoiceCreation",
                         0,
                         "",
                         "",
                         OUTPUT lcInfo).
      END.
            
      /* fee also for cli level */
      IF llRunSpecFee THEN 
      FOR EACH ttCLI 
      BREAK BY ttCLI.CLI:
         
         IF FIRST-OF(ttCLI.CLI) THEN DO:

            lcMobRep = "".
            
             ASSIGN ldFromPer = fMake2Dt(IF ttCLI.FromDate NE ?
                                         THEN ttCLi.FromDate
                                         ELSE ldaFirstCall,86399)
                    ldToPer   = fMake2Dt(IF ttCLI.ToDate NE ?
                                         THEN ttCLI.ToDate
                                         ELSE idaToDate,0). 
                                     /* use 0 -> get owner of last full day */
             
             /* all calls on one day */       
             IF TRUNCATE(ldFromPer,0) = TRUNCATE(ldToPer,0)
             THEN ldToPer = ldToPer + 0.86399.

            FIND FIRST MSOwner NO-LOCK WHERE
                       MSOwner.Brand = gcBrand   AND
                       MsOwner.CLI   = ttCLI.CLI AND
                       MsOwner.TsBeg <= ldToPer  AND
                       MsOwner.TsEnd >= ldFromPer NO-ERROR.

            IF NOT AVAIL MSOwner THEN
               FIND FIRST MSOwner NO-LOCK WHERE
                          MSOwner.Brand       = gcBrand   AND
                          MsOwner.Fixednumber = ttCLI.CLI AND
                          MsOwner.TsBeg       <= ldToPer  AND
                          MsOwner.TsEnd       >= ldFromPer NO-ERROR.


            IF AVAIL MSOwner THEN DO:

               lcMobRep = STRING(fCallSpecDuring(MsOwner.MsSeq,
                                                 idaInvDate)).
               
               IF lcMobRep > "" THEN 
               DO liCnt = 1 TO LENGTH(lcMobRep):
                  RUN Mc/creasfee.p (MsOwner.CustNum,
                                  MsOwner.MSSeq,
                                  idaToDate,
                                  "CLISpec",
                                  SUBSTRING(lcMobRep,liCnt,1),
                                  1,
                                  ?,
                                  "¤" + lcBillRun,
                                  FALSE,
                                  katun,
                                  "InvoiceCreation",
                                  0,
                                  "",
                                  "",
                                  OUTPUT lcInfo).
               END.                 
            END.
         END.
      END.

      /* monthly fees */
      IF NOT llCashInvoice THEN
         RUN pFixedFee (bCustomer.CustNum,
                        iiFeePeriod,
                        idaToDate,
                        lcRegion,
                        bCustomer.VatUsage,
                        idaInvDate).
                        
      /* cash invoice for order */
      IF llCashInvoice THEN 
         RUN pOrderSingleFee(liOrderID,     
                             bCustomer.CustNum).

      /* normal invoices */
      ELSE RUN pSingleFee (bCustomer.CustNum,
                           iiFeePeriod,
                           idaFromDate,
                           idaToDate,
                           lcRegion,
                           bCustomer.VatUsage,
                           idaInvDate).
       
      /* check vat handling for rows created so far */
      fInvRowVAT (bCustomer.VATUsage,
                  lcRegion,  
                  bCustomer.Category,
                  idaInvDate). 

      liArAccNum = ReceivAcc.
      
      IF NOT llCashInvoice THEN DO:
         
         /* amounts need to be rounded before discounts and fatimes to the 
            same level as those will be */
         FOR EACH ttIR:
            ASSIGN
               ttIR.dNet     = ROUND(ttIR.dNet,3)
               ttIR.dGross   = ROUND(ttIR.dGross,3)
               ttIR.DiscBase = ROUND(ttIR.DiscBase,3).
               
            IF ttIR.AgrCust = 0 THEN 
               ttIR.AgrCust = bCustomer.CustNum.
         END.

         IF NOT llCashInvoice THEN DO:
         
            /* Check & apply discounts */ 
            ldaDiscFrom = ADD-INTERVAL(idaFromDate,-4,"months").
            fGetDiscounts("Customer", 
                          STRING(bCustomer.CustNum), 
                          "Invoice", 
                          ldaDiscFrom, 
                          idaToDate, 
                          0). 
             
            FOR EACH bttIR WHERE
                     LOOKUP(STRING(bttIR.RowType),"5,7,9") = 0 AND
                     bttIR.MsSeq > 0
            BREAK BY bttIR.AgrCust
                  BY bttIR.MsSeq:
            
               IF FIRST-OF(bttIR.AgrCust) THEN 
                  fGetDiscounts("Customer",
                                STRING(bttIR.AgrCust), 
                                "Agreement",
                                ldaDiscFrom, 
                                idaToDate, 
                                0).
                
               IF FIRST-OF(bttIR.MsSeq) THEN 
               FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                         MSOwner.MsSeq   = bttIR.MsSeq AND
                         MsOwner.InvCust = bCustomer.CustNum AND
                         MsOwner.AgrCust = bttIR.AgrCust:
               
                  fGetDiscounts("MobSub", 
                                STRING(MsOwner.Msseq), 
                                "Contract Target",  
                                ldaDiscFrom, 
                                idaToDate, 
                                MsOwner.MsSeq).

                  RUN pDiscount(MsOwner.AgrCust,          
                                MsOwner.MsSeq,
                                MsOwner.CLI,
                                TRUNCATE(ldBillPer[2] / 100,0)).
               END.                   
            END.
         END.
           
         RUN pFatime (bCustomer.CustNum,
                      iiFeePeriod,
                      TRUE).
      
         fInvRowVAT (bCustomer.VATUsage,
                     lcRegion,  
                     bCustomer.Category,
                     idaInvDate). 
 
         /* minimum consumption */  
         RUN pMinimumConsumption (bCustomer.CustNum,
                                  idaFromDate,
                                  idaToDate,
                                  ldaFirstCall,      
                                  ldBillPer[1],
                                  ldBillPer[2],
                                  iiInvType).

         /* check vat handling for rows created so far
            -> possible fatime ("all") gets correct sum */
         fInvRowVAT (bCustomer.VATUsage,
                     lcRegion,  
                     bCustomer.Category,
                     idaInvDate). 
      
         /* FATime for all events (after all rows including minimumfee 
            are done); collect cli based sums */
         RUN pFatime (bCustomer.CustNum,
                      iiFeePeriod,
                      FALSE).
                      
      END.
      
      /* check that there are other products besides the one that has
         been defined as not billable when alone */
      IF NOT lRejBill THEN DO:
         ASSIGN lRejBill    = TRUE
                lcRejReason = "ProdNoBill".
         FOR FIRST ttIr WHERE
            ttIr.BillCode NE lNotBilled:
            ASSIGN lRejBill    = FALSE
                   lcRejReason = "".
         END.
      END.
 
      DO TRANS:

         /* invoice is created only if invoice rows are made */
         IF CAN-FIND(FIRST ttIR) THEN DO:

            /* vat handling for rows (fat created after last run) */
            fInvRowVAT (bCustomer.VATUsage,
                        lcRegion,    
                        bCustomer.Category,
                        idaInvDate). 

            IF NOT llCashInvoice THEN 
               /* combine rows into upper level, fatimes and discounts have 
                  been handled now */
               fCombineInvRows().

            /* calculate overtime Interest */
            IF llBillInt AND lcDepoItem = "" AND lcIntItem > "" AND
               NOT llCashInvoice THEN 
            FOR EACH CustIntEvent NO-LOCK USE-INDEX CustNum WHERE
                     CustIntEvent.Brand        = gcBrand           AND 
                     CustIntEvent.CustNum      = bCustomer.CustNum AND
                     CustIntEvent.BilledInvNum = 0,
               FIRST SubInvoice NO-LOCK WHERE
                     SubInvoice.InvNum    = CustIntEvent.InvNum AND
                     SubInvoice.SubInvNum = CustIntEvent.SubInvNum: 

               /* specific clis */
               IF lcMsSeqList > "" AND
                  LOOKUP(STRING(SubInvoice.MsSeq),lcMsSeqList) = 0
               THEN NEXT.

               /* billing denied */
               IF SubInvoice.MsSeq > 0 AND 
                  LOOKUP(STRING(SubInvoice.MsSeq),lcBillDeny) > 0 
               THEN NEXT.
 
               FIND FIRST ttCustBal WHERE
                          ttCustBal.CLI  = SubInvoice.CLI AND
                          ttCustBal.Type = "INT" NO-ERROR.
               IF NOT AVAILABLE ttCustBal THEN DO:
                  CREATE ttCustBal.
                  ASSIGN ttCustBal.CLI  = SubInvoice.CLI
                         ttCustBal.Type = "INT"
                         ttCustBal.ITGroupID = ?.
               END.
                                                                 
               ttCustBal.Amt = ttCustBal.Amt + CustIntEvent.Amt.

               fInvoiceItem(4,
                            SubInvoice.MsSeq,
                            RECID(CustIntEvent)).
            END.

            /* get general (not cli based) customer balances */
            IF lcDepoItem = "" AND NOT llCashInvoice THEN DO:
               ldBasis = fGetCustBal(bCustomer.CustNum,
                                     "",
                                     "OP").
               IF ldBasis NE 0 THEN DO:
                  CREATE ttCustBal.
                  ASSIGN ttCustBal.CLI    = ""
                         ttCustBal.Type   = "OP"
                         ttCustBal.Amt    = ldBasis
                         ttCustBal.ITGroupID = ?.
               END.                          

               ldBasis = fGetCustBal(bCustomer.CustNum,
                                     "",
                                     "AP").
               IF ldBasis NE 0 THEN DO:
                  CREATE ttCustBal.
                  ASSIGN ttCustBal.CLI    = ""
                         ttCustBal.Type   = "AP"
                         ttCustBal.Amt    = ldBasis
                         ttCustBal.ITGroupID = ?.
               END.                          
            END.
            
            /* total invoicable sum */
            FOR EACH ttIR
                BREAK BY ttIR.MsSeq
                      BY ttIR.ToDate:
                     
               IF FIRST-OF(ttIR.MsSeq) THEN DO:

                  ASSIGN liITGroupID  = 0 
                         liITGDeltype = 0
                         liInvSeq     = 0.

                  FIND FIRST ttInvSplit WHERE
                             ttInvSplit.AgrCust   = ttIR.AgrCust AND
                             ttInvSplit.MsSeq     = ttIR.MsSeq AND
                             ttInvSplit.CLIevent  = "iSS" NO-ERROR.
               END.

               IF FIRST-OF(ttIR.ToDate) AND
                  AVAIL ttInvSplit THEN DO:

                  IF ttIR.ToDate < ttInvSplit.SplitDate THEN ASSIGN
                     liITGroupID  = ttInvSplit.ITGroupID[1]
                     liITGDeltype = ttInvSplit.ITGDeltype[1]
                     liInvSeq     = ttInvSplit.InvSeq[1].
                  ELSE ASSIGN 
                     liITGroupID  = ttInvSplit.ITGroupID[2]
                     liITGDeltype = ttInvSplit.ITGDeltype[2]
                     liInvSeq     = ttInvSplit.InvSeq[2].
               END. /* IF FIRST-OF(ttIR.ToDate) THEN DO: */

               ASSIGN
                  /* from several to 3 decimals */
                  ttIR.Amt        = round(ttIR.dNet,3)
                  ttIR.GrossAmt   = round(ttIR.dgross,3)
                  liEventQty      = liEventQty + ttIR.Qty
                  /* into Kbs */
                  ttIR.DataAmt    = ttIR.DataAmt / 1024
                  ttIR.PeakMin    = int(ttIR.PeakMin / 60)
                  ttIR.OffPeakMin = int(ttIR.OffPeakMin / 60)
                  ttIR.ITGroupID  = liITGroupID
                  ttIR.ITGDeltype = liITGDeltype.

               /* possible vat has now been removed -> temporary settings
                  can be cleared */
               IF bCustomer.VATUsage >= 3 AND bCustomer.VatUsage <= 4 
               THEN ASSIGN ttIR.VatCode = 0
                           ttIR.VatPerc = 0.
 
               IF NOT CAN-FIND(BillItem WHERE 
                               BillItem.Brand    = gcBrand AND
                               BillItem.BillCode = ttir.BillCode)
               THEN DO:
                  ASSIGN lRejBill    = TRUE
                         lcRejReason = "Product" + ttir.BillCode + "Missing" +
                                       "(" + STRING(ttIR.RowType) + ")"
                         lcRejCLI    = ttIR.CLI.              
                  LEAVE. 
               END.

               IF ttIR.AgrCust = 0 THEN ttIR.AgrCust = bCustomer.CustNum.

               /* amounts by vat percent (one invoice can include only one 
                  type of vat usage -> one vatcode per percent) */
               FIND FIRST ttRowVat WHERE
                          ttRowVat.CLI     = ttIR.CLI   AND
                          ttRowVat.MsSeq   = ttIR.MsSeq AND
                          ttRowVat.VatPerc = ttIR.VatPerc AND
                          ttRowVat.AgrCust = ttIR.AgrCust AND
                          ttRowVat.ITGroupID = ttIR.ITGroupID NO-ERROR.
               
               IF NOT AVAILABLE ttRowVat THEN DO:
                  CREATE ttRowVat.
                  ASSIGN ttRowVat.CLI     = ttIR.CLI
                         ttRowVat.MsSeq   = ttIR.MsSeq
                         ttRowVat.VatPerc = ttIR.VatPerc
                         ttRowVat.VatCode = ttIR.VatCode
                         ttRowVat.AgrCust = ttIR.AgrCust
                         ttRowVat.ITGroupID  = ttIR.ITGroupID
                         ttRowVat.ITGDeltype = ttIR.ITGDeltype
                         ttRowVat.InvSeq = liInvSeq.
                  
                  IF ttRowVat.VatCode > 0 THEN DO:  
                     FIND VATCode WHERE VATCode.VatCode = ttRowVat.VATCode
                     NO-LOCK NO-ERROR. 
                     IF AVAILABLE VatCode
                     THEN ttRowVat.VatAcc = VatCode.AccNum.
                  END.
                  
               END.
 
               ttRowVat.VatBasis = ttRowVat.VatBasis + ttIR.Amt.

               /* If it is cash invoice then no need to round the value */
               IF llCashInvoice THEN
                  ttRowVat.AccVatBasis = ttRowVat.AccVatBasis + ttIR.dNet.

            END.

            /* reject reason found, write it to log */
            IF lRejBill THEN DO:    
                fELog(katun,"INVOICE:" + lcRejReason + 
                        ":Customer:" + STRING(bCustomer.CustNum)).
                fErrorLog(bCustomer.CustNum,
                          lcRejCLI,
                          0,
                          lcRejReason).

                /* unmark events */ 
                fMarkInvoiceItems(?,0,0,iiInvType).
                 
                NEXT INVRUNLOOP.
            END.
            
            FIND Customer WHERE RECID(Customer) = RECID(bCustomer) NO-LOCK.

            /* Check customer bank account and use correct due date */
            IF (iiInvType = 1 OR iiInvType = 99) AND llMultipleDueDate THEN DO:
               FIND FIRST BankIdCode WHERE
                          BankIdCode.BankCode = SUBSTRING(Customer.BankAcct,5,4)
                    NO-LOCK NO-ERROR.
               IF NOT AVAIL BankIdCode THEN 
                  idaDueDate = idaEBADueDate.
               ELSE IF BankIdCode.BankType = "IberPay" THEN
                  idaDueDate = idaIberPayDueDate.
               ELSE
                  idaDueDate = idaEBADueDate.
               /* Force No Direct Debit customer's invoices to use Iberpay due date */
               IF LOOKUP(STRING(Customer.ChargeType),"5,6") > 0 THEN
                  idaDueDate = idaIberPayDueDate.
            END. /* IF iiInvType = 1 OR iiInvType = 99 THEN DO: */
            ELSE IF idaIberPayDueDate <> ? THEN idaDueDate = idaIberPayDueDate.
            ELSE IF idaEBADueDate <> ? THEN idaDueDate = idaEBADueDate.

            /* make an invoice for each cli that had events */
            RUN pInvoiceHeader(idaInvDate,
                               idaFromDate,
                               idaToDate,
                               idaDueDate,
                               idaFusionDueDate,
                               iiCustQty,
                               iiInvType).
                               
            IF RETURN-VALUE BEGINS "ERROR" THEN RETURN RETURN-VALUE.
         END. /* TRANS */

      END. /* can-find first ttir */

   END.   /* invrunloop */

   PAUSE liPause NO-MESSAGE.
   
   END.  /* liRunStatus */

   &IF "{&InitPersistent}" NE "NO" AND
       "{&EDRHandling}" NE "NO"
   &THEN 
   IF ilRerate THEN RUN pFinalizeRerateReport IN fhRRHAndle.
   &ENDIF

   HIDE MESSAGE NO-PAUSE.

   RETURN "".
   
END PROCEDURE.

PROCEDURE pCancel:

   /* delete invoices and rows recently made */
   IF NOT SESSION:BATCH THEN message "Cancelling invoices, wait ...".

   FOR EACH ttNewInv:

      RUN Inv/del_inv.p (ttNewInv.InvNum). 

      /* no need to save eventlog to db */
      FOR EACH EventLog EXCLUSIVE-LOCK WHERE
               EventLog.TableName = "Invoice" AND
               EventLog.Key       = STRING(ttNewInv.InvNum):
          DELETE EventLog.
      END.

      /* row to external eventlog however */
      fELog(katun,"INVOICE:" + STRING(ttNewInv.InvNum) + ":Cancelled").

      DELETE ttNewInv.
   END.

END PROCEDURE.

&IF "{&EDRHandling}" NE "NO"
&THEN
PROCEDURE pCleanInvSeq:

   DEF INPUT PARAMETER idaInvDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER iiInvCust  AS INT  NO-UNDO.
  
   DEF BUFFER bufseq1   FOR InvSeq.
   DEF BUFFER bufseq2   FOR InvSeq.
   DEF BUFFER bufseq3   FOR InvSeq.
   DEF BUFFER mcall     FOR MobCDR.

   FOR EACH InvSeq NO-LOCK WHERE
            InvSeq.CustNum = iiInvCust  AND
            InvSeq.ToDate <= idaInvDate AND
            InvSeq.Billed = FALSE
   BREAK BY InvSeq.MsSeq
         BY InvSeq.ToDate DESC:

      IF NOT FIRST-OF(InvSeq.MsSeq) THEN NEXT.

      /* billing denied */
      IF LOOKUP(STRING(InvSeq.MsSeq),lcBillDeny) > 0 THEN NEXT.
  
      /* specific clis */
      IF lcMsSeqList > "" AND 
         LOOKUP(STRING(InvSeq.MsSeq),lcMsSeqList) = 0
      THEN NEXT.
 
      CREATE ttInvSeq.
      BUFFER-COPY InvSeq TO ttInvSeq.
   END.
   
   /* separate loops so that delete bufseq doesn't mess break by */    
   FOR EACH ttInvSeq,
      FIRST InvSeq NO-LOCK WHERE
            InvSeq.InvSeq = ttInvSeq.InvSeq:
            
      /* UPDATE InvSeq FOR Calls & remove unused InvSeq record */
      FOR EACH bufseq1 NO-LOCK USE-INDEX MsSeq WHERE
               bufseq1.MsSeq   = InvSeq.MsSeq   AND
               bufseq1.CustNum = InvSeq.CustNum AND
               bufseq1.ToDate <= InvSeq.ToDate  AND
               bufseq1.Billed  = FALSE          AND
               bufseq1.AgrCust = InvSeq.AgrCust AND
               recid(bufseq1) NE recid(InvSeq):

         /* change NEW InvSeq TO previously uninvoiced Calls */
         FOR EACH MobCDR NO-LOCK WHERE
                  MobCDR.InvCust = bufseq1.CustNum AND
                  MobCDR.InvSeq  = bufseq1.InvSeq  AND
                  MobCDR.MsSeq   = ttInvSeq.MsSeq:

            /* reduce TRANSACTION lockings */
            DO TRANS:
               FIND mcall WHERE recid(mcall) = recid(MobCDR) EXCLUSIVE-LOCK.
               mcall.InvSeq = InvSeq.InvSeq.
            END.      

         END.

         /* move counters */
         fMoveInvrowCounters(InvSeq.CustNum,
                             bufseq1.InvSeq,
                             InvSeq.InvSeq).
 
         /* reduce TRANSACTION lockings */
         DO TRANS:

            FIND bufseq2 WHERE recid(bufseq2) = recid(InvSeq) EXCLUSIVE-LOCK.
            bufseq2.FromDate = min(bufseq1.FromDate,InvSeq.FromDate).

            /* remove combined one */
            FIND bufseq3 WHERE recid(bufseq3) = recid(bufseq1) EXCLUSIVE-LOCK.
            DELETE bufseq3.
         END.

      END.
   END.

END PROCEDURE.   
&ENDIF

PROCEDURE pFixedFee:

   DEF INPUT PARAMETER iiCustNum    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiFeePeriod  AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaPeriodEnd AS DATE NO-UNDO.
   DEF INPUT PARAMETER icRegion     AS CHAR NO-UNDO. 
   DEF INPUT PARAMETER iiVatUsage   AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaInvDate   AS DATE NO-UNDO.
   
   DEF VAR lcCLI        AS CHAR NO-UNDO.
   DEF VAR liMsSeq      AS INT  NO-UNDO.
   DEF VAR liPeriodFrom AS INT  NO-UNDO.
   DEF VAR liPeriodTo   AS INT  NO-UNDO.
   DEF VAR ldaFrom      AS DATE NO-UNDO.
   DEF VAR ldaTo        AS DATE NO-UNDO.
   DEF VAR ldVatPerc    AS DEC  NO-UNDO.
   DEF VAR liAgrCust    AS INT  NO-UNDO.
   DEF VAR liRowPeriod  AS INT  NO-UNDO.
   DEF VAR ldaFeesTo    AS DATE NO-UNDO.
   
   FOR EACH ttEventCust WHERE
            (IF llInvComb 
             THEN TRUE
             ELSE ttEventCust.CustNum = iiCustNum),
       EACH FixedFee NO-LOCK WHERE
            FixedFee.Brand   = gcBrand             AND 
            FixedFee.CustNum = ttEventCust.CustNum AND
            FixedFee.InUse   = TRUE:

      /* Create more items, if needed! */
      IF FixedFee.EndPeriod = 999999 THEN DO:

         FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
         IF NOT AVAIL FFItem THEN NEXT.

         ldaFeesTo = ADD-INTERVAL(TODAY,6,"months").
         /* At least 6 months of unbilled fixed fee items */
         IF FFItem.BillPeriod <= YEAR(ldaFeesTo) * 100 + MONTH(ldaFeesTo) 
         THEN DO:
            fMakeContractMore(INPUT Fixedfee.FFNum, 
                              INPUT FFItem.Concerns[2]).
         END.
      END.
               
      /* if deposit item is set then bill only that */
      IF lcDepoItem NE "" AND
         LOOKUP(FixedFee.BillCode,lcDepoItem) = 0 THEN NEXT.

      /* specific order */
      IF liOrderID > 0 THEN DO:
         IF FixedFee.HostTable NE "Order" OR
            FixedFee.KeyValue NE STRING(liOrderID)
         THEN NEXT. 
      END.
               
      /* specific clis */
      IF lcMsSeqList > "" AND 
         (FixedFee.HostTable NE "MobSub" OR
          LOOKUP(FixedFee.KeyValue,lcMsSeqList) = 0)
      THEN NEXT.

      ASSIGN 
         lcCLI   = ""
         liMsSeq = 0
         liAgrCust = iiCustNum.

      IF FixedFee.HostTable = "MobSub" THEN DO:
         
         /* billing denied */
         IF LOOKUP(FixedFee.KeyValue,lcBillDeny) > 0 THEN NEXT.
             
         FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                   MSOwner.MsSeq   = INTEGER(FixedFee.KeyValue) AND
                   MsOwner.InvCust = FixedFee.CustNum:
            ASSIGN 
               lcCLI     = MsOwner.CLI
               liMsSeq   = MsOwner.MsSeq
               liAgrCust = MsOwner.AgrCust.
         END.   
      END.
 
      FOR EACH FFItem of FixedFee exclusive-lock WHERE 
               FFItem.BillPeriod <= iiFeePeriod     AND
               FFItem.BillPeriod > liOldEventPeriod AND
               FFItem.Billed     = FALSE:

         /* concerns must be checked for end invoices, future items
            cannot be billed */
         IF liEndInvType > 0 THEN DO:
            IF FFItem.Concerns[1] > YEAR(idaPeriodEnd) * 10000 + 
                                    MONTH(idaPeriodEnd) * 100 +
                                    DAY(idaPeriodEnd) 
            THEN NEXT.                         
         END.

         /* FOR calculating the time Period FOR invoice ROW */
         liPeriodFrom  = IF FFItem.Concerns[1] > 0 THEN                  
                         FFItem.Concerns[1] ELSE  
                         FFItem.BillPeriod.
         liPeriodTo    = IF FFItem.Concerns[2] > 0 THEN
                         FFItem.Concerns[2] ELSE
                         liPeriodFrom.
          
         /* check that periods are valid */ 
         ldaFrom = fInt2Date(liPeriodFrom,1).
         ldaTo   = fInt2Date(liPeriodTo,2).
         IF ldaFrom = ? OR ldaTo = ? THEN DO:
            ASSIGN lRejBill    = TRUE
                   lcRejReason = "FFeePeriod"
                   lcRejCLI    = lcCLI.
            NEXT.
         END. 

         liRowPeriod = liPeriodTo.
         IF liRowPeriod > 999999 THEN 
            liRowPeriod = TRUNCATE(liRowPeriod / 100,0).
         
         /* change payterm/rvterm billcodes if the contract is financed 
            by bank */
         IF LOOKUP(FixedFee.BillCode,"PAYTERM,RVTERM") > 0 AND
            LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 THEN DO:
            IF FixedFee.TFBank EQ {&TF_BANK_SABADELL} THEN /*0081*/
               FFItem.BillCode = FFItem.BillCode + "BS".
            ELSE IF FixedFee.TFBank EQ {&TF_BANK_CETELEM} THEN /*0225*/
               FFItem.BillCode = FFItem.BillCode + "BC".
            ELSE
               FFItem.BillCode = FFItem.BillCode + "1E".
         END.
          
         FIND FIRST ttIR WHERE
                    ttIR.BillCode = FFItem.BillCode  AND
                    ttIR.CLI      = lcCLI            AND 
                    ttIR.CCN      = 0                AND
                    ttIR.Period   = liRowPeriod      AND
                    ttIR.VatIncl  = FixedFee.VATIncl AND
                    ttIR.RowType  = 3                AND
                    ttIR.AgrCust  = liAgrCust        AND
                    ttIR.OrderId  = FixedFee.OrderId NO-ERROR.

         IF NOT AVAIL ttIR THEN DO:
            CREATE ttIR.
            ASSIGN
               ttIR.FromDate  = ldaFrom
               ttIR.ToDate    = ldaTo
               ttIR.Period    = liRowPeriod
               ttIR.BillCode  = FFItem.BillCode
               ttIR.CLI       = lcCLI
               ttIR.CCN       = 0
               ttIR.MsSeq     = liMsSeq
               ttIR.FFRow     = TRUE
               ttIR.RowType   = 3
               ttIR.VatIncl   = FixedFee.VatIncl
               ttIR.AgrCust   = liAgrCust
               ttIR.OrderId   = FixedFee.OrderId.
         END.

         ASSIGN
            ttIR.FromDate = MIN(ttir.FromDate,ldaFrom)
            ttIR.ToDate   = MAX(ttir.ToDate,ldaTo)
            ttIR.Qty      = ttIR.Qty + 1
            ttIR.dNet     = ttIR.dNet + FFItem.Amt
            FFItem.Billed = TRUE.

         fInvoiceItem(1,
                      ttIR.MsSeq,
                      RECID(FFItem)).
           
      END. /* ffitem */
   END. 

END PROCEDURE.   /* pFixedFee */

PROCEDURE pSingleFee:

   DEF INPUT PARAMETER iiCustNum    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiFeePeriod  AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaPeriodBeg AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaPeriodEnd AS DATE NO-UNDO.
   DEF INPUT PARAMETER icRegion     AS CHAR NO-UNDO. 
   DEF INPUT PARAMETER iiVatUsage   AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaInvDate   AS DATE NO-UNDO.
   
   DEF VAR lcCLI        AS CHAR NO-UNDO.
   DEF VAR liMsSeq      AS INT  NO-UNDO.
   DEF VAR ldaFrom      AS DATE NO-UNDO.
   DEF VAR ldaTo        AS DATE NO-UNDO.
   DEF VAR ldVatPerc    AS DEC  NO-UNDO.
   DEF VAR liAgrCust    AS INT  NO-UNDO.
   DEF VAR liRowPeriod  AS INT  NO-UNDO.
   DEF VAR liFFNum      AS INT NO-UNDO. 
   
   FOR EACH ttEventCust WHERE
            (IF llInvComb 
             THEN TRUE
             ELSE ttEventCust.CustNum = iiCustNum),
       EACH SingleFee EXCLUSIVE-LOCK WHERE
            SingleFee.CustNum     = ttEventCust.CustNum AND
            SingleFee.BillPeriod <= iiFeePeriod         AND
            SingleFee.BillPeriod  > liOldEventPeriod    AND
            SingleFee.Brand       = gcBrand             AND 
            SingleFee.Billed      = FALSE               AND
            SingleFee.Active      = TRUE:

      /* if deposit item is set then bill only that */
      IF lcDepoItem > "" AND
         LOOKUP(SingleFee.BillCode,lcDepoItem) = 0 THEN NEXT.

      /* specific clis */
      IF lcMsSeqList > "" AND 
         (SingleFee.HostTable NE "MobSub" OR
          LOOKUP(SingleFee.KeyValue,lcMsSeqList) = 0)
      THEN NEXT.
         
      /* specific order */
      IF liOrderID > 0 THEN DO:
         IF LOOKUP(SingleFee.HostTable,"Order,MsRequest") = 0 OR
            SingleFee.KeyValue NE STRING(liOrderID)
         THEN NEXT.
      END.  
      ELSE IF SingleFee.HostTable = "Order" AND 
              SingleFee.CalcObj = "CASHFEE" THEN NEXT.

      ASSIGN
         lcCLI   = ""
         liMsSeq = 0
         liAgrCust = iiCustNum.

      IF SingleFee.HostTable = "MobSub" THEN DO:
         FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                   MSOwner.MsSeq   = INTEGER(SingleFee.KeyValue) AND
                   MSOwner.InvCust = SingleFee.CustNum:
            ASSIGN 
               lcCLI     = MsOwner.CLI
               liMsSeq   = MsOwner.MsSeq
               liAgrCust = MsOwner.AgrCust.
         END.    
      END.
      ELSE IF SingleFee.HostTable = "Order" THEN 
      FOR FIRST Order NO-LOCK WHERE
                Order.Brand   = gcBrand AND
                Order.OrderID = INTEGER(SingleFee.KeyValue):
         ASSIGN 
            lcCLI   = Order.CLI
            liMsSeq = Order.MsSeq.
      END.

      /* billing denied */
      IF liMsSeq > 0 AND 
         LOOKUP(STRING(liMsSeq),lcBillDeny) > 0 THEN NEXT.
 
      ASSIGN 
         liRowPeriod = IF SingleFee.Concerns[1] > 0
                       THEN SingleFee.Concerns[1]
                       ELSE SingleFee.BillPeriod
         ldaFrom = fInt2Date(liRowPeriod,1).

      IF SingleFee.Concerns[2] > 0 THEN 
         liRowPeriod = SingleFee.Concerns[2].
      ldaTo = fInt2Date(liRowPeriod,2).

      IF liRowPeriod > 999999 THEN 
         liRowPeriod = TRUNCATE(liRowPeriod / 100,0).

      /* change PAYTERMEND and RVTERMEND billcodes if original terminated 
         contract is financed by bank */
      IF LOOKUP(SingleFee.BillCode,"PAYTERMEND,RVTERMEND") > 0 AND
         SingleFee.SourceTable = "FixedFee" THEN DO:
         
         ASSIGN
            liFFNum = 0
            liFFNum = INT(SingleFee.SourceKey) NO-ERROR. 

         IF liFFNum > 0 THEN DO:

            FIND FIRST FixedFee NO-LOCK WHERE
                       FixedFee.FFnum = liFFNum NO-ERROR.
            IF AVAIL FixedFee AND 
               LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 THEN DO:
               IF FixedFee.TFBank EQ {&TF_BANK_SABADELL} THEN /*0081*/
                  SingleFee.BillCode = SingleFee.BillCode + "BS".
               ELSE IF FixedFee.TFBank EQ {&TF_BANK_CETELEM} THEN /*0225*/
                  Singlefee.BillCode = Singlefee.BillCode + "BC".
               ELSE
                  SingleFee.BillCode = SingleFee.BillCode + "1E".
            END.
         END.
      END.
      FIND FIRST ttIR WHERE
                 ttIR.BillCode = SingleFee.BillCode AND
                 ttIR.CLI      = lcCLI              AND
                 ttIR.CCN      = 0                  AND
                 ttIR.Period   = liRowPeriod        AND
                 ttIR.VatIncl  = SingleFee.VatIncl  AND
                 ttIR.RowType  = 4                  AND
                 ttIR.AgrCust  = liAgrCust          AND
                 ttIR.OrderId  = SingleFee.OrderID  NO-ERROR.

      IF NOT AVAIL ttIR THEN DO:
         CREATE ttIR.
         ASSIGN                                      
            ttIR.FromDate  = ldaFrom 
            ttIR.ToDate    = ldaTo
            ttIR.BillCode  = SingleFee.BillCode
            ttIR.CCN       = 0 
            ttIR.Period    = liRowPeriod
            ttIR.CLI       = lcCLI
            ttIR.MsSeq     = liMsSeq
            ttIR.FFRow     = FALSE
            ttIR.RowType   = 4
            ttIR.AgrCust   = liAgrCust
            ttIR.VatIncl   = SingleFee.VatIncl
            ttIR.OrderId   = SingleFee.OrderId.
      END.

      ASSIGN
         ttIR.FromDate    = MIN(ttIR.FromDate,ldaFrom)
         ttIR.ToDate      = MAX(ttIR.ToDate,ldaTo)
         ttIR.Qty         = ttIR.Qty  + 1
         ttIR.dNet        = ttIR.dNet + SingleFee.Amt
         SingleFee.Billed = TRUE.

      fInvoiceItem(2,
                   ttIR.MsSeq,
                   RECID(SingleFee)).
   END. 

END PROCEDURE.  /* pSingleFee */

PROCEDURE pOrderSingleFee:

   DEF INPUT PARAMETER iiOrderID AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiCustNum AS INT  NO-UNDO.
   
   FOR EACH SingleFee EXCLUSIVE-LOCK USE-INDEX HostTable WHERE
            SingleFee.Brand     = gcBrand AND
            SingleFee.HostTable = "Order" AND
            SingleFee.KeyValue  = STRING(iiOrderId) AND
            SingleFee.Billed    = FALSE   AND
            SingleFee.Active    = TRUE    AND
            SingleFee.CalcObj   = "CASHFEE",
      FIRST Order NO-LOCK WHERE
            Order.Brand   = gcBrand AND
            Order.OrderID = iiOrderId:
               
      /* combine by product code */
      FIND FIRST ttIR WHERE
                 ttIR.BillCode = SingleFee.BillCode AND
                 ttIR.CLI      = Order.CLI          AND
                 ttIR.CCN      = 0                  AND
                 ttIR.Period   = 0                  AND
                 ttIR.VatIncl  = SingleFee.VatIncl  AND
                 ttIR.RowType  = 4                  AND
                 ttIR.AgrCust  = iiCustNum NO-ERROR.

      IF NOT AVAIL ttIR THEN DO:
         CREATE ttIR.
         ASSIGN                                      
            ttIR.FromDate  = ? 
            ttIR.ToDate    = ? 
            ttIR.Period    = 0
            ttIR.BillCode  = SingleFee.BillCode
            ttIR.CCN       = 0 
            ttIR.CLI       = Order.CLI
            ttIR.MsSeq     = Order.MsSeq
            ttIR.AgrCust   = iiCustNum 
            ttIR.FFRow     = FALSE
            ttIR.RowType   = 4
            ttIR.VatIncl   = SingleFee.VatIncl.
      END.

      ASSIGN
         ttIR.Qty         = ttIR.Qty    + 1
         ttIR.dNet        = ttIR.dNet   + SingleFee.Amt
         SingleFee.Billed = TRUE.

      fInvoiceItem(2,
                   ttIR.MsSeq,
                   RECID(SingleFee)).
   END. 

END PROCEDURE.  /* pOrderSingleFee */

&IF "{&EDRHandling}" NE "NO"
&THEN
PROCEDURE pMobCDR:

   DEF INPUT PARAMETER iiCustNum   AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT-OUTPUT PARAMETER idaFirstCall AS DATE NO-UNDO.

   DEF VAR ldGross  AS DEC  NO-UNDO.

   lPrevVat = ?.

   FOR EACH ttInvSeq,
       EACH MobCDR NO-LOCK USE-INDEX InvSeq WHERE
            MobCDR.InvCust   = iiCustNum       AND
            MobCDR.InvSeq    = ttInvSeq.InvSeq AND
            MobCDR.ErrorCode = 0:
      
      &IF "{&InitPersistent}" NE "NO" 
      &THEN 
      /* check / calculate volume discount (disctype 1 = fixed%) */
      IF MobCDR.DiscType = 2 THEN fVolDiscMob(BUFFER MobCDR,
                                              ttInvSeq.AgrCust). 
      &ENDIF
      
      /* different vat handling */   
      IF lPrevVat = ? THEN lPrevVat = MobCDR.VatIncl.
      ELSE IF MobCDR.VatIncl NE lPrevVat THEN lDblVat = TRUE.

       /* DO TRANS TO reduce lockings */
      DO TRANS:

         /* older that CURRENT invoicing Period ? */
         ASSIGN
            liFatPer = YEAR(MobCDR.DateSt) * 100 + MONTH(MobCDR.DateSt).
            idaFirstCall = MIN(idaFirstCall,MobCDR.DateSt).

         FIND FIRST ttIR WHERE
                    ttIR.BillCode = MobCDR.BillCode AND
                    ttIR.CLI      = MobCDR.CLI      AND
                    ttIR.CCN      = MobCDR.CCN      AND
                    ttIR.Period   = liFatPer        AND
                    ttIR.VatIncl  = MOBCDR.VatIncl  AND
                    ttIR.RowType  = 2               AND
                    ttIR.MsSeq    = MobCDR.MsSeq    AND 
                    ttIR.AgrCust  = ttInvSeq.AgrCust NO-ERROR.

         IF NOT AVAIL ttIR THEN DO:
            CREATE ttIR.
            ASSIGN
               ttIR.BillCode  = MobCDR.BillCode
               ttIR.CLI       = MobCDR.CLI
               ttIR.MsSeq     = MobCDR.MsSeq
               ttIR.AgrCust   = ttInvSeq.AgrCust
               ttIR.CCN       = MobCDR.CCN 
               ttIR.Period    = liFatPer
               ttIR.VatIncl   = MobCDR.VatIncl
               ttIR.RowType   = 2.

            /* CURRENT invoicing Period */
            IF MobCDR.DateST >= idaFromDate AND
               MobCDR.DateST <= idaToDate THEN ASSIGN
               ttIR.FromDate = idaFromDate
               ttIR.ToDate = idaToDate.
            /* older Calls */   
            ELSE ASSIGN
               ttIR.FromDate = MobCDR.DateSt
               ttIR.ToDate = MobCDR.DateSt.
         END.

         ASSIGN 
            ttIR.FromDate = min(ttIR.FromDate,MobCDR.DateSt)
            ttIR.ToDate = max(ttIR.ToDate,MobCDR.DateSt). 

         /* is currency unit full or sub (assign ldnet & ldgross) */
         fCurrUnit(MobCDR.Amount,
                   MobCDR.MPMAmt,   /* mpm */
                   MobCDR.CurrUnit,
                   "",
                   MobCDR.TariffNum,
                   gcBrand,
                   OUTPUT ldNet,
                   OUTPUT ldGross).

         ASSIGN
            ttIR.Qty      = ttIR.Qty     + 1
            ttIR.dNet     = ttIR.dNet    + ldNet
            ttIR.dgross   = ttIR.dgross  + ldGross
            ttIR.Minutes  = ttIR.Minutes + MobCDR.BillDur
            ttIR.DataAmt  = ttIR.DataAmt + MobCDR.DataIn + MobCDR.DataOut.

         FIND FIRST ttCLI WHERE
                    ttCLI.MsSeq     = MobCDR.MsSeq     AND
                    ttCLI.InvSeq    = MobCDR.InvSeq    AND
                    ttCLI.CLI       = MobCDR.CLI       AND
                    ttCLI.CCN       = MobCDR.CCN       AND
                    ttCLI.BillCode  = MobCDR.BillCode  AND
                    ttCLI.TariffNum = MobCDR.TariffNum AND
                    ttCLI.VatIncl   = MobCDR.VatIncl   AND
                    ttCLI.ServRid   = MobCDR.ServRid   AND
                    ttCLI.MPMRid    = MobCDR.MPMRid NO-ERROR.

         IF NOT AVAIL ttCLI THEN DO:
            CREATE ttCLI.
            ASSIGN ttCLI.InvNum    = -1 * MobCDR.InvSeq
                   ttCLI.InvSeq    = MobCDR.InvSeq 
                   ttCLI.MsSeq     = MobCDR.MsSeq
                   ttCLI.CCN       = MobCDR.CCN     
                   ttCLI.BillCode  = MobCDR.BillCode   
                   ttCLI.CLI       = MobCDR.CLI
                   ttCLI.TariffNum = MobCDR.TariffNum
                   ttCli.VatIncl   = MobCDR.VatIncl
                   ttCLI.FromDate  = MobCDR.DateSt
                   ttCLI.ToDate    = MobCDR.DateSt
                   ttCLI.ServRid   = MobCDR.ServRid
                   ttCLI.MPMRid    = MobCDR.MPMRid. 
         END.

         ASSIGN ttCLI.Qty      = ttCLI.Qty      + 1
                ttCLI.Minutes  = ttCLI.Minutes  + MobCDR.BillDur
                ttCLI.Amt      = ttCLI.Amt      + ldNet
                ttCLI.GenPrice = ttCLI.GenPrice + ldGross
                ttCLI.MPMAmt   = ttCLI.MPMAmt   + ldGross
                ttCLI.DataAmt  = ttCLI.DataAmt  + MobCDR.DataIn + 
                                                  MobCDR.DataOut
                ttCLI.FromDate = MIN(ttCLI.FromDate,MobCDR.DateSt)
                ttCLI.ToDate   = MAX(ttCLI.ToDate,MobCDR.DateSt).
 
      END. /* DO TRANS */

   END. /* MobCDR  */

END PROCEDURE.
&ENDIF 

PROCEDURE pFatime:

   DEF INPUT PARAMETER iiCustNum     AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiFeePeriod   AS INT  NO-UNDO.
   DEF INPUT PARAMETER ilBeforeMinCons AS LOG NO-UNDO.
 
   DEF VAR llMembers     AS LOG  NO-UNDO.
   DEF VAR liRowType     AS INT  NO-UNDO.
   DEF VAR liFatType     AS INT  NO-UNDO.
   DEF VAR liTypeLoop    AS INT  NO-UNDO.
   DEF VAR ldaFatimeTo AS DATE NO-UNDO. 
 
   DEF BUFFER bBaseIR FOR ttIR.
   
   IF ilBeforeMinCons AND NOT lcMinConsFatime > "" THEN RETURN.
   
   EMPTY TEMP-TABLE ttFATime.
   
   /* get all clis (also empty) */   
   FOR EACH ttIR WHERE
            ttIR.DiscBase > 0
   BREAK BY ttIR.AgrCust
         BY ttIR.CLI 
         BY ttIR.MsSeq:
               
      IF NOT FIRST-OF(ttIR.MsSeq) THEN NEXT. 
      
      CREATE ttFATime.
      ASSIGN 
         ttFATime.AgrCust = ttIR.AgrCust
         ttFATime.CLI     = ttIR.CLI
         ttFATime.MsSeq   = ttIR.MsSeq.
         
      IF NOT CAN-FIND(FIRST ttFATime WHERE
                            ttFATime.AgrCust = ttIR.AgrCust AND
                            ttFATime.CLI = "") THEN DO:
         CREATE ttFATime.
         ASSIGN 
            ttFATime.AgrCust = ttIR.AgrCust
            ttFATime.CLI     = ""
            ttFATime.MsSeq   = 0.
      END.
   END.
   
   FOR EACH ttFATime 
   BY ttFATime.CLI DESC:  /* customer based last */
 
      DO liTypeLoop = 1 TO 4:
   
         CASE liTypeLoop:
         /* fixed fees */
         WHEN 1 THEN ASSIGN
            liFatType = 1
            liRowType = 3. 
         /* single fees */
         WHEN 2 THEN ASSIGN
            liFatType = 3
            liRowType = 4. 
         /* cdrs */
         WHEN 3 THEN ASSIGN
            liFatType = 0  
            liRowType = 2.
         /* all */
         WHEN 4 THEN ASSIGN
            liFatType = 2
            liRowType = ?.
         END CASE. 
 
         IF ilBeforeMinCons AND liFatType NE 2 THEN NEXT.
         
         FOR EACH ttEventCust,
             EACH FATime NO-LOCK USE-INDEX FATType WHERE
                  Fatime.CustNum = ttEventCust.CustNum AND 
                  FATime.CLI     = ttFATime.CLI AND     
                  FATime.FatType = liFatType AND
                  FATime.InvNum  = 0 AND
                  FATime.MsSeq   = ttFatime.MsSeq,
            FIRST FatGroup NO-LOCK WHERE
                  FatGroup.Brand = gcBrand AND 
                  FatGroup.FTGrp = FATime.FTGrp,
            FIRST BillItem NO-LOCK WHERE
                  BillItem.Brand = gcBrand AND
                  BillItem.BillCode = FatGroup.BillCode
         BY FATGroup.Priority 
         BY FATime.Period:

            /* is target / customer level correct */
            IF FatGroup.FatTarget > "0" THEN DO:
               IF FATime.CLI > "" THEN NEXT.
            
               /* invoice/agreement */
               IF (FatGroup.FatTarget = "1" OR FatGroup.FatTarget = "2") AND 
                  FATime.CustNum NE iiCustNum THEN NEXT.
            END.
            /* subscription level */
            ELSE IF FATime.CLI = "" THEN NEXT.

            IF liFatType = 2 THEN DO:
               IF ilBeforeMinCons THEN DO:
                  IF LOOKUP(FatGroup.FtGrp,lcMinConsFatime) = 0 THEN NEXT.
               END.      
               ELSE IF LOOKUP(FatGroup.FtGrp,lcMinConsFatime) > 0 THEN NEXT.
            END.

            llMembers = CAN-FIND(FIRST FatGMember OF FatGroup).
            
            FIND FIRST ttInvSplit WHERE
                       ttInvSplit.AgrCust  = ttFatime.AgrCust AND
                       ttInvSplit.MsSeq    = ttFatime.MsSeq AND 
                       ttInvSplit.CLIEvent = "iSS" NO-ERROR.

            FOR EACH bBaseIR WHERE
                     bBaseIR.CLI   = ttFATime.CLI AND
                     bBaseIR.MsSeq = ttFATime.MsSeq AND
                     bBaseIR.AgrCust = ttFATime.AgrCust AND
                     bBaseIR.TaxClass = BillItem.TaxClass AND
                     bBaseIR.DiscBase > 0 AND
                     (IF liRowType NE ?
                      THEN bBaseIR.RowType = liRowType
                      ELSE LOOKUP(STRING(bBaseIR.RowType),"7,9") = 0)
            BY bBaseIR.Period:
                  
               IF (Fatime.Transfer AND FATime.Period > bBaseIR.Period) OR
                  (NOT Fatime.Transfer AND FATime.Period NE bBaseIR.Period)
               THEN NEXT.
               
               IF FATime.LastPeriod > 0 AND FATime.LastPeriod < bBaseIR.Period
               THEN NEXT. 

               /* if members defined then they must match */
               IF llMembers THEN DO:

                  IF NOT CAN-FIND(FIRST FatGMember OF FatGroup WHERE
                               FatGMember.MemberType = 1 AND
                               FatGMember.FTGMember  = bBaseIR.BillCode)
                  THEN DO:
                     IF liFatType = 0 THEN DO:
                        IF NOT CAN-FIND(FIRST FatGMember OF FatGroup WHERE
                                  FatGMember.MemberType = 0 AND
                                  FatGMember.FTGMember  = STRING(bBaseIR.CCN))
                        THEN NEXT.
                     END.
                     ELSE NEXT.
                  END.
               END.

               /* Check if there is BillingItem/BillingItem Group specified */
               /* in the exclude list to restrict applying the FAT */
               IF NOT IsFATAllowed(INPUT FatGroup.FTGrp,
                                   INPUT bBaseIR.BillCode) THEN NEXT.

               /* This is to split Fat rows with iSTC cases */
               IF AVAIL ttInvSplit THEN DO:
                  IF bBaseIR.ToDate < ttInvSplit.SplitDate THEN
                     ldaFatimeTo = ttInvSplit.Splitdate - 1.
                  ELSE ldaFatimeTo = ttInvSplit.ToDate.
               END.
               ELSE ldaFatimeTo = ?.
               
               fMakeFATimeRow(INPUT-OUTPUT bBaseIR.DiscBase,
                              INPUT-OUTPUT bBaseIR.DiscQty,
                              bBaseIR.MsSeq, 
                              bBaseIR.AgrCust,
                              bBaseIR.VatPerc,
                              ldaFatimeTo).
      
               IF (Fatime.Amt > 0 AND 
                   FATime.Amt - FATime.Used - FATime.TransQty <= 0) OR
                  (Fatime.FatPerc > 0 AND Fatime.Used > 0)
               THEN LEAVE.
            END.
             
            fTransFatime(ttIR.MsSeq,iiFeePeriod).
             
         END. /* FATime */
         
      END.
   END.

END PROCEDURE.   /* pFatime */

PROCEDURE pMinimumConsumption:

   DEF INPUT PARAMETER iiCustNum    AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaPeriodBeg AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaPeriodEnd AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaFirstCall AS DATE NO-UNDO.
   DEF INPUT PARAMETER idePeriodBeg AS DEC  NO-UNDO.
   DEF INPUT PARAMETER idePeriodEnd AS DEC  NO-UNDO.
   DEF INPUT PARAMETER iiInvType    AS INT  NO-UNDO.
    
   DEF VAR ldBaseSum     AS DEC EXTENT 2 NO-UNDO.
   DEF VAR ldBasis       AS DEC  NO-UNDO.
   DEF VAR ldeTerminated AS DEC  NO-UNDO.
   DEF VAR liActivated   AS DEC NO-UNDO. 
   DEF VAR ldMinCons     AS DEC  NO-UNDO.
   DEF VAR lcMinItem     AS CHAR NO-UNDO. 
   DEF VAR liTemp1       AS INT  NO-UNDO.
   DEF VAR liTemp2       AS INT  NO-UNDO.
   DEF VAR liLastDay     AS INT  NO-UNDO.
   DEF VAR llMinRowDone  AS LOG  NO-UNDO.
   DEF VAR liLoop AS INT NO-UNDO. 
   DEF VAR ldePeriodEnd AS DEC NO-UNDO. 
   DEF VAR ldePeriodBeg AS DEC NO-UNDO. 

   DEF BUFFER bOwner FOR MsOwner.

   /* if nothing to bill was found then create fake rows to each 
      invoice customer's cli which were active on the billing period,
      in order to get the minimum consumption */
   FOR EACH MsOwner NO-LOCK USE-INDEX InvCust WHERE 
            MsOwner.InvCust  = iiCustNum    AND
            MsOwner.PayType  = FALSE        AND 
            MsOwner.TsBeg   <= idePeriodEnd AND
            MsOwner.TsEnd   >= idePeriodBeg:
      
      /* specific clis */
      IF lcMsSeqList > "" AND 
         LOOKUP(STRING(MsOwner.MsSeq),lcMsSeqList) = 0
      THEN NEXT.

      /* billing denied */
      IF LOOKUP(STRING(MsOwner.MsSeq),lcBillDeny) > 0 THEN NEXT.
 
      IF CAN-FIND(FIRST ttIR WHERE ttIR.MsSeq = MsOwner.MsSeq) THEN NEXT.
                     
      CREATE ttIR.
      ASSIGN ttIR.RowType = 99
             ttIR.CLI     = MsOwner.CLI
             ttIR.MsSeq   = MsOwner.MsSeq
             ttIR.AgrCust = MsOwner.AgrCust
             ttIR.dNet    = 0.
   END.

   /* receivable account and minimum consumption (account for 
      cash invoices is updated in the end of the collection routine) */    
   FOR EACH bttIR WHERE
            LOOKUP(STRING(bttIR.RowType),"5,7,9") = 0 OR
            /* YOT-1538 */
           (bttIR.RowType EQ 7 AND 
            LOOKUP(bttIR.BillCode,lcMinConsFatimeBillCode) > 0)
   BREAK BY bttIR.MsSeq
         BY bttIR.RowType
         BY bttIR.BillCode
         BY bttIR.Period:
   
      IF FIRST-OF(bttIR.MsSeq) THEN DO:
         ldBaseSum = 0.
         FIND FIRST ttInvSplit WHERE
                    ttInvSplit.AgrCust   = bttIR.AgrCust AND
                    ttInvSplit.MsSeq     = bttIR.MsSeq NO-ERROR.
      END.
          
      IF LOOKUP(bttIR.BillCode,lcNoMinCons) = 0 THEN DO:        
         ldBasis = bttIR.dNet.
         /* minimum limit is vat excluded */
         IF lCustVat THEN ldBasis = ldBasis / (1 + bttIR.VatPerc / 100).
         
         IF AVAIL ttInvSplit AND
                  ttInvSplit.SplitDate <= bttIR.Todate THEN
            ldBaseSum[2] = ldBaseSum[2] + ldBasis.
         ELSE ldBaseSum[1] = ldBaseSum[1] + ldBasis.   

         /* invrow combining is not yet done, so do subroundings */
         IF LAST-OF(bttIR.Period) THEN ASSIGN
            ldBaseSum[1] = ROUND(ldBaseSum[1],3)
            ldBaseSum[2] = ROUND(ldBaseSum[2],3).
      END.
            
      IF LAST-OF(bttIR.MsSeq) AND bttIR.MsSeq > 0 AND
         /* this period already handled, applies both to actual 
            minimum consumption and zero event invoices */
         NOT CAN-FIND(FIRST MinConsumption WHERE
                            MinConsumption.MsSeq = bttIR.MsSeq AND
                            MinConsumption.ToDate = idaPeriodEnd)
      THEN DO liLoop = 1 TO (IF AVAIL ttInvSplit THEN 2 ELSE 1):

         ASSIGN
            ldeTerminated = 99999999.99999
            ldMinCons     = 0
            lcMinItem     = ""
            liActivated   = 0
            llMinRowDone  = FALSE.

         IF AVAIL ttInvSplit THEN DO:
            IF liLoop EQ 1 THEN ASSIGN
               ldePeriodBeg = idePeriodBeg
               ldePeriodEnd = fMake2Dt(ttInvSplit.SplitDate - 1, 86399).
            ELSE ASSIGN
               ldePeriodBeg = fMake2Dt(ttInvSplit.SplitDate,0)
               ldePeriodEnd = idePeriodEnd.
         END.
         ELSE ASSIGN
            ldePeriodBeg = idePeriodBeg
            ldePeriodEnd = idePeriodEnd.

         FOR FIRST MSOwner NO-LOCK USE-INDEX InvCust WHERE
                   MSOwner.InvCust = iiCustNum  AND
                   MsOwner.MsSeq   = bttIR.MsSeq AND
                   MsOwner.PayType = FALSE      AND 
                   MsOwner.TsBeg  <= ldePeriodEnd AND
                   MsOwner.TsEnd  >= ldePeriodBeg,
            FIRST CLIType NO-LOCK WHERE
                  CLIType.Brand   = gcBrand AND
                  CLIType.CLIType = MsOwner.CLIType AND
                  CLIType.MinimAmt > 0:

            ldeTerminated = MsOwner.TsEnd.

            /* minimum consumption only for normal invoices */       
            IF iiInvType <= 1 OR iiInvType EQ 99 THEN DO:
            
               /* check if subscription has been activated on the billing 
                  period */
               IF MsOwner.TsBeg >= ldePeriodBeg AND 
                  MsOwner.TsBeg <= ldePeriodEnd THEN DO:
                         
                  /* YDR-353 */   
                  IF NOT CAN-FIND
                     (FIRST bOwner WHERE
                            bOwner.MsSeq   = MsOwner.MsSeq   AND
                            bOwner.TsBeg < ldePeriodBeg) THEN DO:
                     FIND FIRST Order WHERE
                                Order.MsSeq = MsOwner.MsSeq AND
                                Order.OrderType < 2 NO-LOCK NO-ERROR.
                     IF AVAIL Order AND 
                        LOOKUP(Order.OrderChannel,{&ORDER_CHANNEL_DIRECT}) > 0
                        THEN NEXT.
                     liActivated = INT(SUBSTR(STRING(MsOwner.TSBeg),7,2)).
                  END.
               END. 
   
               ASSIGN 
                 ldMinCons = CLIType.MinimAmt
                 lcMinItem = CLIType.BillCode.
            END.   
         END.  
               
         ldBaseSum[liLoop] = ROUND(ldBaseSum[liLoop],3). 
                  
         IF ldBaseSum[liLoop] >= 0 AND ldBaseSum[liLoop] < ldMinCons AND lcMinItem > "" 
         THEN DO:

            /* Minimum Consumption for Leaving Customer */
            liTemp2 = INT(SUBSTR(STRING(idePeriodEnd),7,2)).
            IF ldeTerminated < ldePeriodEnd THEN ASSIGN
               liLastDay = INT(SUBSTR(STRING(ldeTerminated),7,2))
               liTemp1 = liLastDay - 1.
            ELSE IF AVAIL ttInvSplit AND liLoop EQ 1 THEN ASSIGN
               liLastDay = INT(SUBSTR(STRING(ldeTerminated),7,2))
               liTemp1 = liLastDay.
            ELSE ASSIGN
               liTemp1 = liTemp2
               liLastDay = liTemp2.

            IF AVAIL ttInvSplit AND liLoop = 2 THEN
               liActivated = DAY(ttInvSplit.SplitDate).
            
            /* YDR-353 */   
            IF liActivated > 0 THEN DO:
               IF liActivated EQ 1 AND liLastDay EQ 1 THEN liTemp1 = 0.
               ELSE liTemp1 = liLastDay - liActivated + 1.
            END.

            IF liTemp1 > 0 THEN   
               ldMinCons = liTemp1 / liTemp2 * ldMinCons.
            ELSE ldMinCons = 0.
                     
            IF ldBaseSum[liLoop] < ldMinCons THEN DO:
               CREATE ttIR.
               ASSIGN                                      
                  ttIR.FromDate  = idaPeriodBeg
                  ttIR.ToDate    = idaPeriodEnd
                  ttIR.BillCode  = lcMinItem
                  ttIR.CLI       = bttIR.CLI
                  ttIR.MsSeq     = bttIR.MsSeq
                  ttIR.Period    = YEAR(idaPeriodEnd) * 100 + 
                                   MONTH(idaPeriodEnd)
                  ttIR.AgrCust   = bttIR.AgrCust
                  ttIR.FFRow     = FALSE
                  ttIR.RowType   = 5
                  ttIR.VatIncl   = FALSE
                  ttIR.Qty       = 1
                  ttIR.dNet      = ldMinCons - ldBaseSum[liLoop]
                  ttIR.dgross    = ttIR.dNet
                  llMinRowDone   = TRUE.

               IF AVAIL ttInvSplit THEN DO:
                  IF liLoop EQ 1 THEN ttIR.ToDate = ttInvSplit.SplitDate - 1.
                  ELSE ttIR.FromDate = ttInvSplit.SplitDate.
               END.

            END.

         END.
         
         /* create a zero invoice for those subscriptions that did not have
            any events (and no minimum consumption either) */
         IF bttIR.RowType = 99 AND NOT llMinRowDone AND lcZeroItem > ""
         THEN DO:
            CREATE ttIR.
            ASSIGN                                      
               ttIR.FromDate  = idaPeriodBeg
               ttIR.ToDate    = idaPeriodEnd
               ttIR.BillCode  = lcZeroItem
               ttIR.CLI       = bttIR.CLI
               ttIR.MsSeq     = bttIR.MsSeq
               ttIR.AgrCust   = bttIR.AgrCust
               ttIR.FFRow     = FALSE
               ttIR.RowType   = 5
               ttIR.VatIncl   = FALSE
               ttIR.Qty       = 1
               ttIR.dNet      = 0
               ttIR.dgross    = ttIR.dNet.
               
            IF AVAIL ttInvSplit THEN DO:
               IF liLoop EQ 1 THEN ttIR.ToDate = ttInvSplit.SplitDate - 1.
               ELSE ttIR.FromDate = ttInvSplit.SplitDate.
            END.
         END.
 
      END.
               
      /* dummy rows that were created for min.consumption */
      IF bttIR.RowType = 99 THEN DELETE bttIR.   
   END.

END PROCEDURE.  /* pMinimumConsumption */

PROCEDURE pDiscount:

   DEF INPUT PARAMETER iiAgrCust    AS INT  NO-UNDO.
   DEF INPUT PARAMETER iiMsSeq      AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCLI        AS CHAR NO-UNDO.
   DEF INPUT PARAMETER iiMaxPeriod  AS INT  NO-UNDO. 

   DEF VAR ldaValidFrom AS DATE NO-UNDO.
   DEF VAR ldaValidTo   AS DATE NO-UNDO.
   DEF VAR ldDiscount   AS DEC  NO-UNDO EXTENT 2.
   DEF VAR ldBaseAmount AS DEC  NO-UNDO EXTENT 2.
   DEF VAR llInclMatch  AS LOG  NO-UNDO. 
   DEF VAR llExclMatch  AS LOG  NO-UNDO.
   DEF VAR ldDivide     AS DEC  NO-UNDO.
   DEF VAR ldPortion    AS DEC  NO-UNDO. 
   DEF VAR liDiscPeriod AS INT  NO-UNDO.
   DEF VAR liFromPeriod AS INT  NO-UNDO.
   DEF VAR liToPeriod   AS INT  NO-UNDO.
   DEF VAR llDiscUsed   AS LOG  NO-UNDO.
   DEF VAR liSplitPeriod AS INT  NO-UNDO. 
   DEF VAR ldaDiscValidTo AS DATE NO-UNDO. 
   DEF VAR i AS INT NO-UNDO. 

   DEF BUFFER bBasettIR FOR ttIR.
   DEF BUFFER bBaseItem FOR BillItem.

   /* Note; discounts are agreed to be used on subscription level. So limits
      (MinBase, MaxAmount) are checked here only for the subscription even if
      the customer level discount option (msseq=0) is noted */
   Discounts:
   FOR EACH ttDiscounts WHERE
       ttDiscounts.MsSeq = 0 OR
       ttDiscounts.MsSeq = iiMsSeq
   BY ttDiscounts.Priority:

      IF ttDiscounts.Amount - ttDiscounts.Used = 0 THEN NEXT. 
      
      FIND FIRST BillItem WHERE 
           BillItem.Brand    = gcBrand AND
           BillItem.BillCode = ttDiscounts.BillCode NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BillItem THEN NEXT. 
   
      FIND FIRST ttInvSplit WHERE
                 ttInvSplit.AgrCust  = iiAgrCust AND
                 ttInvSplit.MsSeq    = iiMsSeq NO-ERROR.
      IF AVAIL ttInvSplit AND
               ttInvSplit.CLIEvent EQ "iSS" THEN
         liSplitPeriod = YEAR(ttInvSplit.SplitDate) * 100 + 
                         MONTH(ttInvSplit.SplitDate).
      ELSE liSplitPeriod = -1. /* no split */
      
      ASSIGN
         liFromPeriod = YEAR(ttDiscounts.FromDate) * 100 + 
                        MONTH(ttDiscounts.FromDate)
         liToPeriod   = MIN(YEAR(ttDiscounts.ToDate) * 100 +
                               MONTH(ttDiscounts.ToDate),
                            iiMaxPeriod)
         llDiscUsed   = FALSE.                   
      
      Periods:
      DO liDiscPeriod = liFromPeriod TO liToPeriod:
      
         IF liDiscPeriod MOD 100 >= 13 THEN DO:
            liDiscPeriod = (TRUNCATE(liDiscPeriod / 100,0) + 1) * 100.
            NEXT.
         END.
 
         ASSIGN 
            ldBaseAmount = 0
            ldDiscount   = 0
            ldaValidFrom = fInt2Date(liDiscPeriod,1)
            ldaValidTo   = fInt2Date(liDiscPeriod,2).
            
         IF ttDiscounts.TargetType = "All" THEN 
         FOR EACH bBasettIR WHERE
                  bBasettIR.MsSeq   = iiMsSeq  AND
                  bBasettIR.AgrCust = iiAgrCust AND
                  bBasettIR.TaxClass = BillItem.TaxClass AND
                  bBasettIR.Period = liDiscPeriod AND
                  LOOKUP(STRING(bBasettIR.RowType),"5,7,9") = 0:

            IF liDiscPeriod EQ liSplitPeriod THEN DO:
               IF bBasettIR.ToDate < ttInvSplit.SplitDate THEN
                  ldBaseAmount[1] = ldBaseAmount[1] + bBasettIR.DiscBase.
               ELSE
                  ldBaseAmount[2] = ldBaseAmount[2] + bBasettIR.DiscBase.
            END.
            ELSE ASSIGN ldBaseAmount[1] = ldBaseAmount[1] + bBasettIR.DiscBase.

            /* previous values do not matter, no need to empty previous 
               either */
             bBasettIR.DPId = RECID(ttDiscounts).
         END.

         ELSE IF ttDiscounts.TargetType = "List" THEN DO:

            /* criteria to temp table when first addressed to */
            IF NOT CAN-FIND(FIRST ttDPTarget WHERE 
               ttDPTarget.DPId = ttDiscounts.DPId) THEN 
            FOR EACH DPTarget NO-LOCK WHERE
                  DPTarget.DPId = ttDiscounts.DPId:
               CREATE ttDPTarget.
               BUFFER-COPY DPTarget TO ttDPTarget.
            END.      
               
            /* get rows that match (all) criteria */
            FOR EACH bBasettIR USE-INDEX MsSeq WHERE 
                     bBasettIR.MsSeq    = iiMsSeq AND
                     bBasettIR.AgrCust  = iiAgrCust AND
                     bBasettIR.TaxClass = BillItem.TaxClass AND
                     bBasettIR.Period   = liDiscPeriod AND
                     LOOKUP(STRING(bBasettIR.RowType),"5,7,9") = 0,
               FIRST bBaseItem NO-LOCK WHERE 
                     bBaseItem.Brand = gcBrand AND
                     bBaseItem.BillCode = bBasettIR.BillCode:
                    
               ASSIGN 
                  llInclMatch = FALSE
                  llExclMatch = FALSE.
               FOR EACH ttDPTarget WHERE 
                        ttDPTarget.DPId = ttDiscounts.DPId AND
                        ttDPTarget.ValidFrom <= ldaValidTo AND
                        ttDPTarget.ValidTo   >= ldaValidFrom:
                     
                  CASE ttDPTarget.TargetTable:
                  WHEN "BillItem" THEN DO:
                     IF bBasettIR.BillCode MATCHES(ttDPTarget.TargetKey) 
                     THEN DO:
                        IF ttDPTarget.Included THEN llInclMatch = TRUE.
                        ELSE llExclMatch = TRUE.
                     END.
                  END.
                  WHEN "BItemGroup" THEN DO:
                     IF bBaseItem.BIGroup MATCHES(ttDPTarget.TargetKey) 
                     THEN DO:
                        IF ttDPTarget.Included THEN llInclMatch = TRUE.
                        ELSE llExclMatch = TRUE.
                     END.
                  END.
                  END CASE.

                  IF llExclMatch THEN LEAVE.
               END.
            
               IF llInclMatch AND NOT llExclMatch THEN DO:
                  
                  IF liDiscPeriod EQ liSplitPeriod THEN DO:
                     IF bBasettIR.ToDate < ttInvSplit.SplitDate THEN
                        ldBaseAmount[1] = ldBaseAmount[1] + bBasettIR.DiscBase.
                     ELSE
                        ldBaseAmount[2] = ldBaseAmount[2] + bBasettIR.DiscBase.
                  END.
                  ELSE ASSIGN ldBaseAmount[1] = ldBaseAmount[1] + bBasettIR.DiscBase.

                  bBasettIR.DPId = RECID(ttDiscounts).
               END.
            END.
         
         END.
      
         DISCT_CREATE_LOOP:
         DO i = 1 TO (IF liDiscPeriod EQ liSplitPeriod THEN 2 ELSE 1):
            
            IF NOT (ldBaseAmount[i] > 0 AND
                    ldBaseAmount[1] + ldBaseAmount[2] >= ttDiscounts.MinBase)
            THEN NEXT.
         
               IF ttDiscounts.Unit = "Percentage" THEN 
                  ldDiscount[i] = ROUND(ldBaseAmount[i] * 
                               (ttDiscounts.Amount / 100),3).
               ELSE IF ttDiscounts.Unit = "Fixed" THEN DO:
                  IF i EQ 1 THEN 
                     ldDiscount[i] = MIN(ldBaseAmount[i],ttDiscounts.Amount).
                  ELSE
                     ldDiscount[i] = MIN(ldBaseAmount[i],
                                     MAX(ttDiscounts.Amount - ldDiscount[1], 0)).
               END.

               IF ldDiscount[i] EQ 0 THEN NEXT.

               IF ttDiscounts.MaxAmount > 0 THEN DO:
                  IF i EQ 1 THEN
                     ldDiscount[i] = MIN(ldDiscount[i],ttDiscounts.MaxAmount).
                  ELSE DO:
                     ldDiscount[i] = MIN(ldDiscount[i],
                                    MAX(ttDiscounts.MaxAmount - ldDiscount[1], 0)).
                     IF ldDiscount[i] EQ 0 THEN NEXT.
                  END.
               END.

               IF liDiscPeriod EQ liSplitPeriod AND i EQ 1
               THEN ldaDiscValidTo   = ttInvSplit.SplitDate - 1.
               ELSE ldaDiscValidTo   = (IF AVAIL ttInvSplit AND /* YTS-7265 */
                                           liSplitPeriod EQ -1 AND
                                           liDiscPeriod EQ liToPeriod
                                        THEN MIN(ldaValidTo,ttDiscounts.ToDate)
                                        ELSE ldaValidTo).
               
               FIND FIRST ttIR WHERE 
                          ttIR.BillCode  = ttDiscounts.BillCode AND
                          ttIR.CLI       = icCLI AND
                          ttIR.CCN       = 0 AND
                          ttIR.Period    = liDiscPeriod AND
                          ttIR.ToDate    = ldaDiscValidTo AND
                          ttIR.VatIncl   = lCustVat AND
                          ttIR.RowType   = 9 AND 
                          ttIR.AgrCust   = iiAgrCust AND
                          ttIR.OrderId   = ttDiscounts.OrderId NO-ERROR.

               IF NOT AVAILABLE ttIR THEN DO:
                  CREATE ttIR.
                  ASSIGN 
                     ttIR.FromDate  = ldaValidFrom
                     ttIR.ToDate    = ldaDiscValidTo
                     ttIR.Period    = liDiscPeriod
                     ttIR.BillCode  = ttDiscounts.BillCode
                     ttIR.CCN       = 0
                     ttIR.CLI       = icCLI
                     ttIR.MsSeq     = iiMsSeq
                     ttIR.AgrCust   = iiAgrCust
                     ttIR.RowType   = 9
                     ttIR.VatIncl   = lCustVat
                     ttIR.TaxClass  = BillItem.TaxClass
                     ttIR.OrderId   = ttDiscounts.OrderId.
               END.
         
               ASSIGN
                  ttIR.Qty         = ttIR.Qty    + 1
                  ttIR.dNet        = ttIR.dNet   - ldDiscount[i]
                  ttIR.dgross      = ttIR.dGross - ldDiscount[i]
                  ttIR.FromDate    = MIN(ttIR.FromDate,ldaValidFrom)
                  ldDivide         = ldDiscount[i]
                  llDiscUsed       = TRUE.

               /* deduct used discount from base amounts
                  -> no discount on already discounted amounts */ 
               FOR EACH bBasettIR USE-INDEX MsSeq WHERE 
                  bBasettIR.MsSeq    = iiMsSeq AND
                  bBasettIR.AgrCust  = iiAgrCust AND
                  bBasettIR.TaxClass = BillItem.TaxClass AND
                  bBasettIR.Period   = liDiscPeriod AND
                  bBasettIR.DPId = INT(RECID(ttDiscounts)) AND
                  bBasettIR.dNet > 0
               BREAK BY bBasettIR.dNet:
               
                  IF liDiscPeriod EQ liSplitPeriod THEN DO:
                     IF i EQ 1 AND
                        bBasettIR.ToDate >= ttInvSplit.SplitDate THEN NEXT.
                     ELSE IF i EQ 2 AND
                        bBasettIR.ToDate < ttInvSplit.SplitDate THEN NEXT.
                  END.

                  IF LAST(bBasettIR.dNet) THEN 
                     bBasettIR.DiscBase = MAX(0,bBasettIR.DiscBase - ldDivide).
                  ELSE ASSIGN 
                     ldPortion = ROUND(bBasettIR.DiscBase * ldDiscount[i] / 
                                          ldBaseAmount[i],3)
                     ldPortion = MAX(0,ldPortion)
                     bBasettIR.DiscBase = bBasettIR.DiscBase - ldPortion
                     ldDivide = ldDivide - ldPortion.
               END.

         END.
      
      END.  /* Periods */
      
      IF llDiscUsed AND ttDiscounts.ProcStopper THEN LEAVE Discounts.
   END.   /* Discounts */
   
END PROCEDURE.  /* pDiscount */

PROCEDURE pCounterVAT:

   DEF VAR ldVatFactor AS DEC  NO-UNDO.
   
   FOR EACH ttCLI:
      /* data amounts to Kb (do this in a separate loop) */
      ttCLI.DataAmt = ttCLI.DataAmt / 1024.
   END.
                  
   FOR EACH ttCLI,
      FIRST ttIR WHERE
            ttIR.BillCode = ttCLI.BillCode:

      ldVatFactor = (1 + ttIR.VatPerc / 100).
      
      /* amount with vat is needed for fat and minfee */
      IF ttCLI.VatIncl 
      THEN ttCLI.AmtWVat = ttCLI.Amt.
      ELSE ttCLI.AmtWVat = ttCLI.Amt * ldVatFactor. 
      
      IF ttCLI.VatIncl NE lCustVat THEN DO:
      
         IF lCustVat = TRUE THEN ASSIGN 
            ttCLI.GenPrice = ttCLI.GenPrice * ldVatFactor
            ttCLI.Amt      = ttCLI.Amt  * ldVatFactor
            ttCLI.MPMAmt   = ttCLI.MPMAmt * ldVatFactor.
         ELSE ASSIGN
            ttCLI.GenPrice = ttCLI.GenPrice / ldVatFactor
            ttCLI.Amt      = ttCLI.Amt  / ldVatFactor
            ttCLI.MPMAmt   = ttCLI.MPMAmt / ldVatFactor.

         FIND FIRST bttCLI WHERE
                    bttCLI.MsSeq     = ttCLI.MsSeq     AND
                    bttCLI.invseq    = ttCLI.invseq    AND
                    bttCLI.CLI       = ttCLI.CLI       AND                 
                    bttCLI.CCN       = ttCLI.CCN       AND
                    bttCLI.BillCode  = ttCLI.BillCode  AND
                    bttCLI.TariffNum = ttCLI.TariffNum AND
                    bttCLI.VatIncl      = lCustVat        AND
                    bttCLI.ServRid   = ttCLI.ServRid   AND
                    bttCLI.MPMRid    = ttCLI.MPMRid NO-ERROR.

         IF AVAIL bttCLI THEN DO:
            ASSIGN
            bttCLI.Qty      = bttCLI.Qty     + ttCLI.Qty    
            bttCLI.min      = bttCLI.min     + ttCLI.min
            bttCLI.Amt      = bttCLI.Amt     + ttCLI.Amt
            bttCLI.GenPrice = bttCLI.GenPrice + ttCLI.GenPrice
            bttCLI.MPMAmt   = bttCLI.MPMAmt  + ttCLI.MPMAmt
            bttCLI.DataAmt  = bttCLI.DataAmt + ttCLI.DataAmt
            bttCLI.AmtWVat  = bttCLI.AmtWVat + ttCLI.AmtWVat
            bttCLI.FromDate = MIN(bttCLI.FromDate,ttCLI.FromDate)
            bttCLI.ToDate   = MAX(bttCLI.ToDate,ttCLI.ToDate).

            DELETE ttCLI.     
         END.
      
         ELSE ttCLI.VatIncl = lCustVat.
      END.
      
   END.  /*ttCLI */        

END PROCEDURE.


PROCEDURE pUpdInvGroup:

   FOR EACH ttNewInv 
   BREAK BY ttNewInv.InvGroup
         BY ttNewInv.InvType
         BY ttNewInv.ExtInvID:

      IF last-of(ttNewInv.InvType) THEN DO:

         FIND Invoice WHERE Invoice.InvNum = ttNewInv.InvNum NO-LOCK NO-ERROR.
         IF AVAILABLE Invoice THEN    
         /* update last used invoice number */
         fUpdateInvNum(ttNewInv.InvGroup,
                       Invoice.InvType,
                       Invoice.InvDate,
                       Invoice.ExtInvID).
      END.

      DELETE ttNewInv.  
   END.

END PROCEDURE.

PROCEDURE pGetAmt:

   DEF OUTPUT PARAMETER pQty  AS INT NO-UNDO.
   DEF OUTPUT PARAMETER pAmt  AS DEC NO-UNDO.
   DEF OUTPUT PARAMETER pVAmt AS DEC NO-UNDO. 

   FOR EACH ttNewInv NO-LOCK:
      ASSIGN pQty  = pQty  + 1
             pAmt  = pAmt  + ttNewInv.Qty
             pVAmt = pVAmt + ttNewInv.VATAmt. 
   END.

END PROCEDURE.

PROCEDURE pGetBillRunID:

   DEF OUTPUT PARAMETER ocBillRun AS CHAR NO-UNDO.
   
   FOR FIRST ttNewInv,
       FIRST Invoice NO-LOCK WHERE
             Invoice.InvNum = ttNewInv.InvNum:
      ocBillRun = Invoice.BillRun.       
   END.
   
END PROCEDURE.

/* called from cleanrunui, in order to pass few extra parameters */
PROCEDURE pCleanRun:

   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiFeePeriod AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idCredLoss  AS DEC  NO-UNDO.
   DEF INPUT  PARAMETER iiMinDays   AS INT  NO-UNDO.

   ASSIGN ldCredLoss    = idCredLoss
          liMinDays     = iiMinDays
          liAdd2InvDate = extratime.

   run pCreateInv (idaInvDate,
                   idaFromDate,
                   idaToDate,
                   iiFeePeriod,
                   ?, /* IberPay Due Date */
                   ?, /* EBA Due date */
                   ?, /* Fusion due date */
                   TRUE,
                   TRUE,
                   0,
                   1,
                   "",
                   TRUE).

END PROCEDURE.

PROCEDURE pEndInvoice:

   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiFeePeriod AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiEndType   AS INT  NO-UNDO.
   DEF OUTPUT PARAMETER oiCreated   AS INT  NO-UNDO.

   liEndInvType = iiEndType.

   RUN pCreateInv (idaInvDate,
                   idaFromDate,
                   idaToDate,
                   iiFeePeriod,
                   ?, /* IberPay Due Date */
                   ?, /* EBA Due date */
                   ?, /* Fusion due date */
                   TRUE,
                   TRUE,
                   0,
                   1,
                   "",
                   TRUE).

   /* was invoice created */
   oiCreated = liLastCreated.
   
END PROCEDURE.

PROCEDURE pCleanMem:

   &IF "{&InitPersistent}" NE "NO" 
   &THEN 
   IF VALID-HANDLE(fhVDHandle) THEN DELETE PROCEDURE fhVDHandle.
   &IF "{&EDRHandling}" NE "NO"
   &THEN
      IF VALID-HANDLE(fhRRHandle) THEN DELETE PROCEDURE fhRRHandle.
      IF VALID-HANDLE(fhDCHandle) THEN DELETE PROCEDURE fhDCHandle.
   &ELSE 
      DELETE OBJECT clsInvRowCounter NO-ERROR.
   &ENDIF   
   &ENDIF

   IF llDoEvent THEN fCleanEventObjects().
   
END PROCEDURE. 

/* create deposit/adv.payment invoices */
PROCEDURE pDepositInvoice:

   DEF INPUT  PARAMETER iiInvType   AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER iiOrderID   AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER icMSSeq     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiFeePeriod AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiPaymTerm  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER ilSilent    AS LOG  NO-UNDO. 

   /* normal invoice */
   IF iiInvType = 1 THEN ASSIGN 
      liDepoAcc   = 0
      lcDepoItem  = fCParamC("FirstInvItem")
      liDepoPTerm = 0.
   /* deposit */
   ELSE IF iiInvType = 3 THEN ASSIGN 
      liDepoAcc   = fCParamI("ResDepositsAcc")
      lcDepoItem  = fCParamC("DepositItem")
      liDepoPTerm = fCParamI("DepositPayTerm").
   /* adv.payment */
   ELSE IF iiInvType = 4 THEN ASSIGN 
      liDepoAcc   = fCParamI("AdvPaymAcc")
      lcDepoItem  = fCParamC("AdvPaymItem")
      liDepoPTerm = fCParamI("AdvPaymPayTerm").
      
   IF iiInvType NE 1 THEN DO:
      IF iiPaymTerm > 0 THEN liDepoPTerm = iiPaymTerm.
   END. 
      
   ASSIGN liSilent   = ilSilent
          lcMsSeqList = icMSSeq
          liOrderID  = iiOrderID
          liAdd2InvDate = extratime.
          
   IF liDepoPTerm = ? THEN liDepoPTerm = 0.    

   /* billing item is mandatory */       
   IF lcDepoItem = "" OR lcDepoItem = ? THEN RETURN. 

   /* account is mandatory (if not normal invoice) */       
   IF (liDepoAcc = 0 OR liDepoAcc = ?) AND iiInvType >= 3
   THEN RETURN. 

   /* when lcDepoItem is set then only single fees with that item
      are billed, and invoice type is set as 3/4 */
   run pCreateInv (idaInvDate,
                   idaFromDate,
                   idaToDate,
                   iiFeePeriod,
                   ?, /* IberPay Due date */
                   ?, /* EBA Due date */
                   ?, /* Fusion due date */
                   FALSE,
                   FALSE,
                   1,
                   iiInvType,
                   "",
                   TRUE).
END PROCEDURE.

/* create cash invoices */
PROCEDURE pCashInvoice:

   DEF INPUT  PARAMETER iiInvType   AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER iiOrderID   AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER icMSSeq     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiFeePeriod AS INT  NO-UNDO.
   DEF INPUT  PARAMETER extratime   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiPaymTerm  AS INT  NO-UNDO. 
   DEF INPUT  PARAMETER ilSilent    AS LOG  NO-UNDO. 

   ASSIGN liSilent  = ilSilent
          liOrderID = iiOrderID
          liAdd2InvDate = extratime.

   run pCreateInv (idaInvDate,
                   idaFromDate,
                   idaToDate,
                   iiFeePeriod,
                   ?, /* IberPay Due date */
                   ?, /* EBA Due date */
                   ?, /* Fusion due date */
                   FALSE,
                   FALSE,
                   1,
                   iiInvType,
                   "",
                   TRUE).
END PROCEDURE.

/* create actual invoice and subinvoices to db */
PROCEDURE pInvoiceHeader:

   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaDueDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFusionDueDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiInvType   AS INT  NO-UNDO. 

   DEF VAR liMonth     AS INT  NO-UNDO.
   DEF VAR liYear      AS INT  NO-UNDO.
   DEF VAR lCurRate    AS DEC  NO-UNDO.
   DEF VAR lInterest   AS DEC  NO-UNDO.
   DEF VAR lReduce     AS DEC  NO-UNDO.
   DEF VAR lAdvRed     AS DEC  NO-UNDO. 
   DEF VAR lInvNo      AS INT  NO-UNDO.
   DEF VAR lNotInv     AS DEC  NO-UNDO.
   DEF VAR lLoop       AS INT  NO-UNDO.
   DEF VAR llCreate    AS LOG  NO-UNDO. 
   DEF VAR liVoucher   AS INT  NO-UNDO. 
   DEF VAR ldBasis     AS DEC  NO-UNDO.
   DEF VAR ldVat       AS DEC  NO-UNDO.
   DEF VAR ldVatPort   AS DEC  NO-UNDO.
   DEF VAR ldRedBasis  AS DEC  NO-UNDO.
   DEF VAR ldRedVat    AS DEC  NO-UNDO.
   DEF VAR liCnt       AS INT  NO-UNDO. 
   DEF VAR ldtFrom     AS DATE NO-UNDO.
   DEF VAR ldtTo       AS DATE NO-UNDO. 
   DEF VAR ldAPBase    AS DEC  NO-UNDO. 
   DEF VAR ldVatPerc   AS DEC  NO-UNDO.
   DEF VAR lcExtInvID  AS CHAR NO-UNDO. 
   DEF VAR lcSeqPref   AS CHAR NO-UNDO. 
   DEF VAR liInvRecid  AS RECID NO-UNDO. 
   DEF VAR liSubInv    AS INT  NO-UNDO.
   DEF VAR liAgrCust   AS INT  NO-UNDO.
   DEF VAR liIgnoMin   AS INT  NO-UNDO.
   DEF VAR ldTotalInv  AS DEC  NO-UNDO.
   DEF VAR ldCLIAP     AS DEC  NO-UNDO.
   DEF VAR ldCLIOP     AS DEC  NO-UNDO. 
   DEF VAR liUserCust  AS INT  NO-UNDO.
   DEF VAR liPaymAcc   AS INT  NO-UNDO.
   DEF VAR lcPaymSrc   AS CHAR NO-UNDO.
   DEF VAR lcPaymMemo  AS CHAR NO-UNDO. 
   DEF VAR liPaymType  AS INT  NO-UNDO. 
   DEF VAR liITGroupID AS INT  NO-UNDO.
   DEF VAR liSeq       AS INT  NO-UNDO. 
   DEF VAR ldMinConsAmt  AS DEC  NO-UNDO. 
   DEF VAR liITGDeltype  AS INT  NO-UNDO.
   DEF VAR llNextInvNdd  AS LOG  NO-UNDO.
   DEF VAR lcRegion      AS CHAR NO-UNDO. 
   DEF VAR lcFixedNumber AS CHAR NO-UNDO INIT ?. 
   DEF VAR ldeSplitTS    AS DEC NO-UNDO. 
   DEF VAR ldeToTS     AS DEC NO-UNDO. 
   DEF VAR ldeTotalInvRow AS DEC NO-UNDO.

   DEF BUFFER bufseq  FOR InvSeq.
   DEF BUFFER bChkInv FOR Invoice. 
   DEF BUFFER bttSubInv FOR ttSubInv.
   DEF BUFFER bttCustBal FOR ttCustBal.
   
   ASSIGN 
      lCurRate   = fCurrRate(Customer.Currency,idaInvDate)
      ldeToTS    = fMake2Dt(idaToDate + 1, 0)
      ldTotalInv = 0
      liAgrCust  = 0.
   
   /* one subinvoice per each cli and one invoice per customer to combine them, 
      transaction covers all invoices created here, so if an undo happens
      rollback concerns this customer totally */

   /* first just create headers for subinvoices, in order to get agr.customer
      and see how subscriptions are grouped */
   FOR EACH ttRowVat 
   BREAK BY ttRowVat.AgrCust
         BY ttRowVat.ITGroupID
         BY ttRowVat.MsSeq DESC:
    
      IF FIRST-OF(ttRowVat.MsSeq) THEN DO:

         IF ttRowVat.InvSeq > 0 THEN
            FIND FIRST ttInvSeq WHERE 
               ttInvSeq.MsSeq = ttRowVat.MsSeq AND
               ttInvSeq.AgrCust = ttRowVat.AgrCust AND
               ttInvSeq.InvSeq  = ttRowVat.InvSeq NO-ERROR.
         ELSE FIND FIRST ttInvSeq WHERE 
            ttInvSeq.MsSeq = ttRowVat.MsSeq AND
            ttInvSeq.AgrCust = ttRowVat.AgrCust NO-ERROR.
         IF AVAILABLE ttInvSeq 
         THEN liSeq = ttInvSeq.InvSeq.
         ELSE liSeq = 0. 

         ASSIGN 
            lcCLI      = ttRowVat.CLI
            liAgrCust  = ttRowVat.AgrCust
            liUserCust = Customer.CustNum.
            
         /* get the latest with msseq index in case msisdn has been changed */
         FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                   MSOwner.MsSeq   = ttRowVat.MsSeq AND
                   MSOwner.TSBegin < ldeToTS AND 
                   MsOwner.InvCust = Customer.CustNum AND
                   MsOwner.AgrCust = ttRowVat.AgrCust AND
                   MsOwner.PayType = FALSE:
            ASSIGN
               lcCLI         = MsOwner.CLI
               lcFixedNumber = MsOwner.FixedNumber
               liUserCust    = MsOwner.CustNum.
         END.

         IF ttRowVat.ITGDeltype EQ {&INV_DEL_TYPE_FUSION_EMAIL} OR
            ttRowVat.ITGDeltype EQ {&INV_DEL_TYPE_FUSION_EMAIL_PENDING} THEN
            lcFixedNumber = "".
         /* iSTC from convergent to mobile */
         ELSE IF lcFixedNumber EQ ? OR lcFixedNumber EQ "" THEN DO:

            FIND FIRST ttInvSplit WHERE
                       ttInvSplit.AgrCust  = ttRowVat.AgrCust AND
                       ttInvSplit.MsSeq    = ttRowVat.MsSeq NO-ERROR.

            IF AVAIL ttInvSplit THEN DO:

               ldeSplitTS = fMake2Dt(ttInvSplit.SplitDate, 0).

               FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                         MSOwner.MsSeq   = ttRowVat.MsSeq AND
                         MsOwner.TsBegin < ldeSplitTS AND
                         MsOwner.InvCust = Customer.CustNum AND
                         MsOwner.AgrCust = ttRowVat.AgrCust:
                  lcFixedNumber = MsOwner.FixedNumber.
               END.
            END.
         END.
                  
         /* no grouping for cash invoices */
         IF llCashInvoice THEN ASSIGN
            liITGroupID = 0
            liITGDeltype = 0.
         ELSE IF ttRowVat.ITGroupID > 0 THEN ASSIGN
            liITGroupID  = ttRowVat.ITGroupID
            liITGDeltype = ttRowVat.ITGDeltype.
         ELSE liITGroupID = fGetInvoiceTargetGroup(ttRowVat.MsSeq,
                                                   liAgrCust,
                                                   Customer.CustNum,
                                                   idaTodate,
                                                   OUTPUT liITGDeltype).
         
         liSubInv = 1.
         FIND LAST ttSubInv WHERE ttSubInv.ITGroupID = liITGroupID NO-ERROR.
         IF AVAILABLE ttSubInv THEN liSubInv = ttSubInv.SubInvNum + 1.
 
         CREATE ttSubInv.
         ASSIGN
            ttSubInv.InvNum       = liITGroupID
            ttSubInv.InvSeq       = lISeq
            ttSubInv.ITGroupID    = liITGroupID
            ttSubInv.ITGDelType   = liITGDeltype
            ttSubInv.SubInvNum    = liSubInv
            ttSubInv.MsSeq        = ttRowVat.MsSeq
            ttSubInv.CLI          = lcCLI
            ttSubInv.FixedNumber  = lcFixedNumber
            ttSubInv.AgrCust      = liAgrCust
            ttSubInv.CustNum      = liUserCust
            ttSubInv.VatPos       = 0.
      END.

      /* For partially terminated use billing period ttRowVat.CLI 
         if there were events with mobile number */
      IF AVAIL ttSubInv AND
               ttSubInv.CLI EQ ttSubInv.FixedNumber AND
              (ttRowVat.CLI BEGINS "6" OR
               ttRowVat.CLI BEGINS "7") AND
               ttRowVat.MsSeq EQ ttSubInv.MsSeq THEN
         ASSIGN ttSubInv.CLI = ttRowVat.CLI.

      ASSIGN 
         ttRowVat.SubInvNum = liSubInv
         ttRowVat.ITGroupID = liITGroupID.
   END.         

   /* try to combine orphans to an existing group */
   IF CAN-FIND(FIRST ttSubInv WHERE ttSubInv.ITGroupID < 0) THEN DO:

      /* Do not combine to Fusion invoice group */
      FIND FIRST ttSubInv WHERE
                 ttSubInv.ITGroupID > 0 AND
                 NOT ttSubInv.ITGDelType > 0 NO-ERROR. 
      IF AVAILABLE ttSubInv THEN DO:
         ASSIGN 
            liITGroupID = ttSubInv.ITGroupID
            liSubInv    = 1.
         FIND LAST ttSubInv WHERE ttSubInv.ITGroupID = liITGroupID NO-ERROR.
         IF AVAILABLE ttSubInv THEN liSubInv = ttSubInv.SubInvNum + 1.
          
         FOR EACH ttSubInv WHERE 
                  ttSubInv.ITGroupID < 0:
            
            FOR EACH ttRowVat WHERE
                     ttRowVat.ITGroupID = ttSubInv.ITGroupID AND
                     ttRowVat.SubInvNum = ttSubInv.SubInvNum:
               ASSIGN 
                  ttRowVat.ITGroupID = liITGroupID
                  ttRowVat.SubInvNum = liSubInv.
            END.

            ASSIGN 
               ttSubInv.ITGroupID = liITGroupID
               ttSubInv.SubInvNum = liSubInv
               liSubInv           = liSubInv + 1.
         END.
      END.
   END.
   
   /* then calculate amounts; this way vat data can be set in ascending order
      by vat percent to all subinvoices */
   FOR EACH ttRowVat 
   BY ttRowVat.VatPerc:

      FIND FIRST ttSubInv WHERE 
                 ttSubInv.ITGroupID = ttRowVat.ITGroupID AND
                 ttSubInv.SubInvNum = ttRowVat.SubInvNum.

      ASSIGN 
         ttSubInv.VatPos = ttSubInv.VatPos + 1
         lLoop           = ttSubInv.VatPos.
         
      ASSIGN 
         ttSubInv.VatBasis[lloop]   = ttSubInv.VatBasis[lloop] +
                                         ttRowVat.VatBasis
         ttSubInv.VatPercent[lloop] = ttRowVat.VatPerc
         ttSubInv.VatAccount[lloop] = ttRowVat.VatAcc.

      IF ttRowVat.VatPerc = 0 
      THEN ttSubInv.Vat0Amt = ttSubInv.Vat0Amt + ttRowVat.VatBasis.

      /* VAT included in prices */
      IF lCustVat THEN ASSIGN
         ldBasis                   = ROUND(ttRowVat.VatBasis * 
                                           ttRowVat.VatPerc /
                                        (100 + ttRowVat.VATPerc),3) 
         ttSubInv.VatAmount[lloop] = ttSubInv.VatAmount[lloop] + ldBasis        
         ttSubInv.VATAmt           = ttSubInv.VatAmt + ldBasis
         ttSubInv.InvAmt           = ttSubInv.InvAmt + ttRowVat.VatBasis
         ttSubInv.AmtExclVat       = ttSubInv.InvAmt - ttSubInv.VatAmt
         ldTotalInv                = ldTotalInv + ttRowVat.VatBasis - ldBasis.

      /* VAT excluded from prices */
      ELSE DO:
         ttSubInv.AmtExclVAT = ROUND(ttSubInv.AmtExclVat + ttRowVat.VatBasis,2).

         /* VAT excluded from prices */
         IF iiInvType = 6 OR iiInvType = 7 THEN
            ldBasis = ROUND(((ttRowVat.AccVatBasis *
                             (1 + ttRowVat.VATPerc / 100)) -
                             ttRowVat.VatBasis),2).
         ELSE ASSIGN
            ldeTotalInvRow = ROUND(ttRowVat.VatBasis * (1 + ttRowVat.VatPerc / 100),2)
            ldBasis        = ROUND(ldeTotalInvRow - ttRowVat.VatBasis,2).

         ASSIGN
            ttSubInv.VatAmount[lloop] = ttSubInv.VatAmount[lloop] + ldBasis
            ttSubInv.VATAmt           = ttSubInv.VATAmt + ldBasis
            ttSubInv.InvAmt           = ROUND(ttSubInv.AmtExclVAT + ttSubInv.VATAmt,2)
            ldTotalInv                = ldTotalInv + ttRowVat.VatBasis.

      END.

   END.  /* vat data from ttRowVat */

   /* use this temp-table for slightly other purpose from this on 
      (adv.payment may change vat data on subinvoices) */
   EMPTY TEMP-TABLE ttRowVat.
  
   /* finalize or cancel invoices */
   FOR EACH ttSubInv 
   BREAK BY ttSubInv.AgrCust
         BY ttSubInv.ITGroupID
         BY ttSubInv.MsSeq:
   
      IF FIRST-OF(ttSubInv.ITGroupID) THEN DO:
         /* combined invoice */ 
         CREATE ttInv.
         ASSIGN 
            ttInv.InvNum     = ttSubInv.ITGroupID
            ttInv.CustNum    = Customer.CustNum
            ttInv.DDBankAcc  = Customer.BankAcct WHEN iiInvType EQ 1 OR
                                                      iiInvType EQ 99
            ttInv.AgrCust    = ttSubInv.AgrCust
            ttInv.ITGroupID  = ttSubInv.ITGroupID
            ttInv.Deltype    = ttSubInv.ITGDelType
            ttInv.EndInvoice = liEndInvType.
      END.

      ELSE FIND FIRST ttInv WHERE 
                      ttInv.AgrCust   = ttSubInv.AgrCust AND
                      ttInv.ITGroupID = ttSubInv.ITGroupID.
      
      /* portion of vat related sales */
      ldVatPort = IF ttSubInv.Vat0Amt = 0 OR ttSubInv.AmtExclVat = 0
                  THEN 1
                  ELSE (ttSubInv.AmtExclVat - ttSubInv.Vat0Amt)
                            / ttSubInv.AmtExclVat.
                    
      /* overtime interest */                 
      IF NOT llCashInvoice THEN DO:
         FOR FIRST ttCustBal WHERE
                   ttCustBal.CLI  = ttSubInv.CLI AND
                   ttCustBal.Type = "INT" AND
                   ttCustBal.Amt > ttCustBal.Used:
            ASSIGN 
               ttSubInv.InterestAmt = ttCustBal.Amt - ttCustBal.Used
               ttCustBal.Used       = ttCustBal.Amt
               ttCustBal.ITGroupID  = ttSubInv.ITGroupID.
         END.
      
         /* general interest is added to first subscription (to subinvoice 
            with empty subscription if one exists) */
         IF FIRST(ttSubInv.MsSeq) THEN 
         FOR FIRST ttCustBal WHERE
                   ttCustBal.CLI  = "" AND
                   ttCustBal.Type = "INT" AND
                   ttCustBal.Used = 0:
            ASSIGN 
               ttSubInv.InterestAmt = ttSubInv.InterestAmt + ttCustBal.Amt 
               ttCustBal.Used       = ttCustBal.Amt
               ttCustBal.ITGroupID  = ttSubInv.ITGroupID.
         END.
        
         ttSubInv.InvAmt = ttSubInv.InvAmt + ttSubInv.InterestAmt.
      END.
      
      ASSIGN lAdvRed = 0
             lReduce = 0
             ldCLIOP = 0
             ldCLIAP = 0. 

      /* get customer balances */
      IF lcDepoItem = "" AND NOT llCashInvoice AND ttSubInv.InvAmt > 0 
      THEN DO:
         
         IF ttSubInv.Cli > "" THEN DO:
            ASSIGN 
               ldCLIOP = fGetCustBal(Customer.CustNum,
                                     ttSubInv.CLI,
                                     "OP")
               ldCLIAP = fGetCustBal(Customer.CustNum,
                                     ttSubInv.CLI,
                                     "AP").
         END.
         
         /* advance payment */
         IF ldCLIAP NE 0 THEN DO:
            IF ttSubInv.InvAmt - ldCLIAP < 0 
            THEN lAdvRed = ttSubInv.InvAmt.
            ELSE lAdvRed = ldCLIAP.
            
            /* this must be kept separate from customer level ttCustBal */  
            CREATE ttCustBal.
            ASSIGN ttCustBal.CLI  = ttSubInv.CLI
                   ttCustBal.Type = "AP"
                   ttCustBal.Used = lAdvRed
                   ttCustBal.ITGroupID = ttSubInv.ITGroupID.
         END.
                   
         /* if not enough to nul the amount then check if there is 
            something on customer level */
         IF lAdvRed NE ttSubInv.InvAmt THEN 
         FOR FIRST ttCustBal WHERE 
                   ttCustBal.CLI  = "" AND
                   ttCustBal.Type = "AP" AND
                   ttCustBal.ITGroupID = ?:

            IF LAST(ttSubInv.MsSeq) THEN DO:
               IF ttCustBal.Amt - ttCustBal.Used > 0 
               THEN ldCLIAP = MIN(ttSubInv.InvAmt - lAdvRed,
                                  ttCustBal.Amt - ttCustBal.Used).
               ELSE ldCLIAP = ttCustBal.Amt - ttCustBal.Used.
            END.
                  
            /* general (not cli based) balances are divided to subinvoices
               in proportion to their total amounts */
            ELSE IF ttSubInv.AmtExclVat > 0 THEN ASSIGN
               ldCLIAP = ROUND(ttCustBal.Amt * ttSubInv.AmtExclVat /
                               ldTotalInv,2)
               ldCLIAP = MIN(ttSubInv.InvAmt - lAdvRed,ldCLIAP).             
            ASSIGN 
               ttCustBal.Used = ttCustBal.Used + ldCLIAP
               lAdvRed        = lAdvRed + ldCLIAP.

            IF ldCLIAP NE 0 THEN DO:
               CREATE bttCustBal.
               ASSIGN bttCustBal.CLI  = ""
                      bttCustBal.Type = "AP"
                      bttCustBal.Used = ldCLIAP
                      bttCustBal.ITGroupID = ttSubInv.ITGroupID.
            END.          
         END.
      
         /* advance payments (advance payment includes VAT),
            reduce from total  */
         ASSIGN
            ttSubInv.AdvPaym = -1 * lAdvRed  
            ttSubInv.InvAmt  = ttSubInv.InvAmt - lAdvRed.
                     
         /* overpayment */
         IF ldCLIOP NE 0 AND ttSubInv.InvAmt > 0 THEN DO:
         
            IF ttSubInv.InvAmt - ldCLIOP < 0 
            THEN lReduce = ttSubInv.InvAmt.
            ELSE lReduce = ldCLIOP.
            
            /* this must be kept separate from customer level ttCustBal */    
            CREATE ttCustBal.
            ASSIGN ttCustBal.CLI  = ttSubInv.CLI
                   ttCustBal.Type = "OP"
                   ttCustBal.Used = lReduce
                   ttCustBal.ITGroupID = ttSubInv.ITGroupID.
         END.
                   
         /* if not enough to nul the amount then check if there is 
            something on customer level */
         IF lReduce NE ttSubInv.InvAmt THEN 
         FOR FIRST ttCustBal WHERE 
                   ttCustBal.CLI  = "" AND
                   ttCustBal.Type = "OP" AND
                   ttCustBal.ITGroupID = ?:

            IF LAST(ttSubInv.MsSeq) THEN DO:
               IF ttCustBal.Amt - ttCustBal.Used > 0 
               THEN ldCLIOP = MIN(ttSubInv.InvAmt - lReduce,
                                  ttCustBal.Amt - ttCustBal.Used).
               ELSE ldCLIOP = ttCustBal.Amt - ttCustBal.Used.
            END.
                  
            /* general (not cli based) balances are divided to subinvoices
               in proportion to their total amounts */
            ELSE IF ttSubInv.AmtExclVat > 0 THEN ASSIGN
               ldCLIOP = ROUND(ttCustBal.Amt * ttSubInv.AmtExclVat /
                               ldTotalInv,2)
               ldCLIOP = MIN(ttSubInv.InvAmt - lReduce,ldCLIOP).             

            ASSIGN 
               ttCustBal.Used = ttCustBal.Used + ldCLIOP
               lReduce        = lReduce + ldCLIOP.
               
            IF ldCLIOP NE 0 THEN DO:
               CREATE bttCustBal.
               ASSIGN bttCustBal.CLI  = ""
                      bttCustBal.Type = "OP"
                      bttCustBal.Used = ldCLIOP
                      bttCustBal.ITGroupID = ttSubInv.ITGroupID.
            END.          
               
         END.

         ASSIGN
            ttSubInv.OverPaym = -1 * lReduce
            ttSubInv.InvAmt   = ttSubInv.InvAmt - lReduce.
      END.     
              
      /* base for advance payment vat */                       
      ldAPBase = lAdvRed. 

      /* VAT handling FOR advance payment */
      IF ldAPBase NE 0 AND ttSubInv.VATAmt NE 0 THEN DO:
         ASSIGN 
            ttSubInv.VatPos           = ttSubInv.VatPos + 1
            lloop                     = ttSubInv.VatPos
            ttSubInv.VatAmount[lLoop] = IF ttSubInv.AmtExclVat + 
                                           ttSubInv.VatAmt = ldAPBase
                                        THEN ttSubInv.VatAmt
                                        ELSE fAPVatAmt(Customer.Region,
                                                       ldAPBase * ldVatPort,
                                                       idaInvDate).
      
         /* not more than original vat */
         IF ABS(ttSubInv.VatAmt) < ABS(ttSubInv.VatAmount[lLoop])
         THEN ttSubInv.VatAmount[lLoop] = ttSubInv.VatAmt.
      
         ASSIGN                                  
            ttSubInv.VatAcc[lLoop]  = liAPVatAcc
            ttSubInv.VatPerc[lLoop] = ldeAPVatPerc
            ttSubInv.VatBasis[lLoop]= IF ttSubInv.AmtExclVat + 
                                         ttSubInv.VatAmt = ldAPBase
                                      THEN (IF lCustVat
                                            THEN ttSubInv.AmtExclVat + 
                                                 ttSubInv.VatAmt - 
                                                 ttSubInv.Vat0Amt
                                            ELSE ttSubInv.AmtExclVat -
                                                 ttSubInv.Vat0Amt)
                                      ELSE (IF lCustVat
                                            THEN ldAPBase * ldVatPort
                                            ELSE ldAPBase * ldVatPort - 
                                                  ttSubInv.VatAmount[lLoop])
            ldBasis                 = ttSubInv.VatBasis[lLoop]
            ldVat                   = ttSubInv.VatAmount[lLoop].

         /* reduce from normal vat amounts (vat percents are in 
            ascending order) */  
         DO liCnt = 10 TO 1 BY -1:
            IF liCnt = lLoop OR ttSubInv.VatAmount[liCnt] = 0 THEN NEXT.
         
            IF ldBasis >= 0 THEN ASSIGN 
               ldRedBasis = MIN(ttSubInv.VatBasis[liCnt],ldBasis)
               ldRedVat   = MIN(ttSubInv.VatAmount[liCnt],ldVat).
            ELSE ASSIGN 
               ldRedBasis = MAX(ttSubInv.VatBasis[liCnt],ldBasis)
               ldRedVat   = MAX(ttSubInv.VatAmount[liCnt],ldVat).
         
            ASSIGN
               ttSubInv.VatBasis[liCnt]  = ttSubInv.VatBasis[liCnt] - 
                                           ldRedBasis
               ttSubInv.VatAmount[liCnt] = ttSubInv.VatAmount[liCnt] - 
                                           ldRedVat
               ldBasis                   = ldBasis - ldRedBasis
               ldVat                     = ldVat - ldRedVat.
         
         END.         
      END.
 
      ASSIGN 
         ttSubInv.InvAmt   = ROUND(ttSubInv.InvAmt,2)
         ttInv.AmtExclVat  = ttInv.AmtExclVat + ttSubInv.AmtExclVat
         ttInv.VatAmt      = ttInv.VatAmt + ttSubInv.VatAmt
         ttInv.InterestAmt = ttInv.InterestAmt + ttSubInv.InterestAmt
         ttInv.AdvPaym     = ttInv.AdvPaym + ttSubInv.AdvPaym
         ttInv.OverPaym    = ttInv.OverPaym + ttSubInv.OverPaym
         ttInv.InvAmt      = ttInv.InvAmt + ttSubInv.InvAmt.

      /* collect vat data into temp-table so that it can be saved to 
         combined invoice in ascending order by vatpercent */ 
      DO liCnt = 1 TO 10:  
      
         IF ttSubInv.VatBasis[liCnt] = 0 AND ttSubInv.VatPerc[liCnt] = 0
         THEN NEXT.
         
         FIND FIRST ttRowVat WHERE 
                    ttRowVat.ITGroupID = ttSubInv.ITGroupID AND
                    ttRowVat.VatPerc = ttSubInv.VatPercent[liCnt] NO-ERROR.
         IF NOT AVAILABLE ttRowVat THEN DO:
            CREATE ttRowVat.
            ASSIGN 
               ttRowVat.ITGroupID = ttSubInv.ITGroupID
               ttRowVat.VatPerc   = ttSubInv.VatPercent[liCnt]
               ttRowVat.VatAcc    = ttSubInv.VatAccount[liCnt].
         END.
         
         ASSIGN 
            ttRowVat.VatBasis = ttRowVat.VatBasis + ttSubInv.VatBasis[liCnt]
            ttRowVat.VatAmt   = ttRowVat.VatAmt + ttSubInv.VatAmount[liCnt].
      END.
            
      /* invoice could be created in "last-of(ttsubinv.itgroupid)", but better
         to make a clean cut here */

   END. /* foreach ttsubinv */
   
   llNextInvNdd = FALSE.
   /* check if invoice scetches are valid and if so then save them to db */
   FOR EACH ttInv:
 
      ttInv.CurrAmt = fToHomeCurr(ttInv.InvAmt,lCurRate). 
      
      RUN pValidateInvoice (idaToDate,
                            OUTPUT llCreate,
                            OUTPUT liIgnoMin). 
   
      /* cancel creation */   
      IF NOT llCreate THEN DO:
               
         /* calculate amount of NOT Billed Calls */
         lNotInv = lNotInv + ttInv.AmtExclVAT.

         /* unmark events */
         FOR EACH ttSubInv WHERE ttSubInv.ITGroupID = ttInv.ITGroupID:
            fMarkInvoiceItems(ttSubInv.MsSeq,0,0,iiInvType).
         END.   
               
         IF lcRejReason = "" THEN 
            lcRejReason = "ValueTooLow:" + STRING(ttInv.InvAmt).
               
         /* ROW FOR NOT created invoices */
         fELog(katun,"INVOICE:" + lcRejReason + 
                      ":Customer:" + STRING(ttInv.CustNum)).
         fErrorLog(ttInv.CustNum,
                   "", 
                   ttInv.ITGroupID,
                   lcRejReason).

         DELETE ttInv.
         NEXT.
      END.
      
      ASSIGN lUpdAcc  = IF NOT llCashInvoice 
                        THEN InvGroup.UpdCustBal
                        ELSE FALSE
             lcSeqPref = ""
             lcRegion = Customer.Region.
      
      IF llCashInvoice AND liOrderID > 0 THEN DO:
         FIND FIRST OrderCustomer NO-LOCK WHERE   
                    OrderCustomer.Brand = gcBrand AND
                    OrderCustomer.OrderId = liOrderID AND
                    OrderCustomer.RowType = 1 NO-ERROR.
         IF AVAIL OrderCustomer THEN
            lcRegion = OrderCustomer.Region.
      END.

      /* update the rest of the fields */
      ASSIGN 
         ttInv.ChgStamp     = fMakeTS()
         ttInv.Brand        = Customer.Brand 
         ttInv.InvDate      = idaInvDate
         ttInv.FirstCall    = MIN(ldaFirstCall,idaFromDate)
         ttInv.FromDate     = idaFromDate
         ttInv.ToDate       = idaToDate
         ttInv.InvType      = IF iiInvType > 0 THEN iiInvType ELSE 1 
         ttInv.ArAccNum     = liArAccNum                     
         ttInv.RoundAccNum  = RoundAcc
         ttInv.IntAccNum    = OTIntAcc
         ttInv.OPAccNum     = OverPayAcc
         ttInv.APAccNum     = AdvPaymAcc
         ttInv.InterestAmt  = lInterest
         ttInv.Currency     = Customer.Currency 
         ttInv.ExchRate     = lCurRate
         ttInv.InterestPerm = Customer.InterestPerm
         ttInv.ClaimPerm    = IF iiInvType >= 3 AND iiInvType <= 4
                              THEN FALSE
                              ELSE Customer.ClaimPerm
         ttInv.VATIncl      = lCustVat
         ttInv.VatUsage     = Customer.VatUsage
         ttInv.DelType      = Customer.DelType WHEN NOT ttInv.DelType > 0
         ttInv.SpecDel      = Customer.SpecDel
         ttInv.WInvDisp     = FALSE
         ttInv.BillRun      = lcBillRun
         ttInv.Region       = lcRegion
         ttInv.InvGroup     = Customer.InvGroup.

      CASE iiInvType:
         WHEN 6 THEN ttInv.ChargeType = 1.
         WHEN 7 THEN DO:
            FIND FIRST Order NO-LOCK WHERE
                       Order.Brand   = gcBrand AND
                       Order.OrderId = liOrderID NO-ERROR.
            IF AVAILABLE Order AND
               INDEX(Order.OrderChannel,"POS") = 0 AND
               CAN-FIND(FIRST OrderPayment WHERE
                              OrderPayment.Brand   = gcBrand AND
                              OrderPayment.OrderId = liOrderID AND
                              OrderPayment.Method  = {&ORDERPAYMENT_M_PAYPAL})
            THEN ttInv.ChargeType = 6. /* PayPal */
            ELSE ttInv.ChargeType = 3.
         END.
         OTHERWISE   ttInv.ChargeType = IF Customer.ChargeType = 6 THEN 5
                                        ELSE Customer.ChargeType.
      END CASE.
         
      FIND Region WHERE Region.Region = ttInv.Region NO-LOCK NO-ERROR.
      IF AVAILABLE Region THEN ttInv.TaxZone = Region.TaxZone.
            
      /* adv.payment / deposit */
      IF iiInvType >= 3 AND iiInvType <= 4 THEN ASSIGN
         ttInv.DueDate  = fChkDueDate(ttInv.InvDate + liAdd2InvDate + 
                                      liDepoPTerm)
         ttInv.ARAccNum = liDepoAcc.
         
      /* cash invoices */
      ELSE IF llCashInvoice THEN
         ttInv.DueDate = ttInv.InvDate + 1.
               
      /* od invoices, first working day after invoice date */
      ELSE IF ttInv.BillRun BEGINS "OD" THEN DO:
         ASSIGN 
            ttInv.DueDate = fChkDueDate(ttInv.InvDate + 1)
            ttInv.ToDate  = ttInv.InvDate.
      END.
         
      /* due day is the 7th (working) day of invoicing month
         for normal invoices */
      ELSE IF idaDueDate = ? THEN DO:
                                  
         IF DAY(ttInv.InvDate) > 7 THEN ASSIGN
            liCnt   = DAY(ttInv.InvDate)
            liMonth = MONTH(ttInv.InvDate)
            liYear  = YEAR(ttInv.InvDate).
         ELSE ASSIGN
            liCnt   = 7     
            liMonth = MONTH(ttInv.InvDate)
            liYear  = YEAR(ttInv.InvDate).

         ldtFrom = fChkDueDate(DATE(liMonth,liCnt,liYear)).
            
         IF ldtFrom = ? THEN ldtFrom = ttInv.InvDate.
                       
         ttInv.DueDate = ldtFrom.
      END.    

      IF ttInv.InvType = 1 OR ttInv.InvType EQ 99 THEN
         ttInv.MandateId = fGetMandateForITGroup(ttInv.ITGroupID,
                                                 ttInv.Custnum,
                                                 ttInv.FromDate,
                                                 ttInv.ToDate).
 
      /* spesific due date given, overrides everything */
      IF idaFusionDueDate NE ? AND
         (ttInv.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL} OR
          ttInv.DelType EQ {&INV_DEL_TYPE_FUSION_EMAIL_PENDING})
      THEN ttInv.DueDate = idaFusionDueDate.
      ELSE IF idaDueDate NE ? THEN ttInv.DueDate = idaDueDate.

      lLoop = 0.

      /* names and addresses */               
      IF llCashInvoice AND liOrderID > 0 THEN DO:
            
         /* is there a delivery address */
         FIND FIRST OrderCustomer WHERE
                    OrderCustomer.Brand   = gcBrand   AND
                    OrderCustomer.OrderID = liOrderID AND
                    OrderCustomer.RowType = 4 
         NO-LOCK NO-ERROR.
         IF AVAILABLE OrderCustomer THEN DO:
            ASSIGN 
               ttInv.IDelName    = DYNAMIC-FUNCTION("fPrintOrderName"
                                                       IN ghFunc1,
                                                    BUFFER OrderCustomer)
               ttInv.IDelAddr    = OrderCustomer.Address
               ttInv.IDelZipCode = OrderCustomer.ZipCode
               ttInv.IDelPost    = OrderCustomer.PostOffice.
                     
            IF OrderCustomer.Region > "" THEN DO:
               FIND Region WHERE Region.Region = OrderCustomer.Region
                  NO-LOCK NO-ERROR.
               IF AVAILABLE Region THEN 
                  ttInv.IDelPost = ttInv.IDelPost + ", " + 
                                   Region.RgName.
            END.
         END.
                     
         /* actual invoice customer */             
         FIND FIRST OrderCustomer WHERE
                    OrderCustomer.Brand   = gcBrand   AND
                    OrderCustomer.OrderID = liOrderID AND
                    OrderCustomer.RowType = Order.InvCustRole 
         NO-LOCK NO-ERROR.
         IF AVAILABLE OrderCustomer THEN DO:
            ASSIGN 
               ttInv.CustName    = DYNAMIC-FUNCTION("fPrintOrderName"
                                                       IN ghFunc1,
                                                    BUFFER OrderCustomer)
               ttInv.Address    = OrderCustomer.Address
               ttInv.PostOffice = OrderCustomer.ZipCode + " " + 
                                  OrderCustomer.PostOffice.

            IF OrderCustomer.Region > "" THEN DO:
               FIND Region WHERE Region.Region = OrderCustomer.Region
                  NO-LOCK NO-ERROR.
               IF AVAILABLE Region THEN 
                  ttInv.PostOffice = ttInv.PostOffice + ", " + 
                                     Region.RgName.
            END.
                                    
         END.
      END.
            
      ELSE ASSIGN 
         ttInv.COName      = Customer.COName
         ttInv.Address     = Customer.Address
         ttInv.PostOffice  = Customer.ZipCode + " " + 
                              Customer.PostOffice
         ttInv.IDelName    = Customer.IDelName
         ttInv.IDelCOName  = Customer.IDelCOName
         ttInv.IDelAddr    = Customer.IDelAddr
         ttInv.IDelZipCode = Customer.IDelZipCode
         ttInv.IDelPost    = Customer.IDelPost
         ttInv.IDelCountry = Customer.IDelCountry                        
         ttInv.FirstName   = Customer.FirstName
         ttInv.SurName2    = Customer.SurName2
         ttInv.CustName    = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                              BUFFER Customer).
            
      /* for VAS invoice take ar-account from operator */
      IF liVASAcc > 0 THEN ttInv.ARAccNum = liVASAcc.
         
      CREATE Invoice.

      /* internal invoice nbr */ 
      REPEAT:
         Invoice.InvNum = NEXT-VALUE(IntInvNum) NO-ERROR. 
                   
         VALIDATE Invoice NO-ERROR.
                 
         /* another process has just used the same number */
         IF ERROR-STATUS:ERROR OR Invoice.InvNum = 0 THEN NEXT.
         ELSE LEAVE.
      END.

      ASSIGN 
         lInvno     = Invoice.InvNum
         liInvRecid = RECID(Invoice).

      /* copy TEMP-TABLE TO Invoice */
      BUFFER-COPY ttInv except ttInv.InvNum ttInv.ExtInvID TO Invoice.

      ASSIGN 
         /* total amount with 2 decimals */
         Invoice.InvAmt    = ROUND(Invoice.InvAmt,2)
         Invoice.ITGroupID = MAX(0,ttInv.ITGroupID).
   
      IF Invoice.InvType NE 1 AND Invoice.InvType NE 99 THEN 
         Invoice.DeliveryState = 1.

      /* mark "Next invoice not direct debit" as "Direct Debit" */
      IF Invoice.ChargeType  = 5 AND
         Invoice.InvType     = 1 AND
         Customer.ChargeType = 6
      THEN llNextInvNdd = TRUE.

      /* YDR-1767 */
      IF Invoice.ChargeType = 5 THEN 
         Invoice.ClaimStatus = "2". 

      FOR EACH ttSubInv WHERE
               ttSubInv.ITGroupID = ttInv.ITGroupID
      BY ttSubInv.SubInvNum:
      
         CREATE SubInvoice.
         BUFFER-COPY ttSubInv EXCEPT ttSubInv.InvNum TO SubInvoice.
         ASSIGN 
            SubInvoice.Brand    = Invoice.Brand 
            SubInvoice.InvNum   = Invoice.InvNum
            ldMinConsAmt        = 0.
       
         FOR EACH ttIR WHERE 
                  ttIR.MsSeq = ttSubInv.MsSeq AND
                  ttIR.AgrCust = ttSubInv.AgrCust:
                  
            IF ttIR.ITGroupID > 0 AND 
               ttIR.ITGroupID NE ttSubInv.ITGroupID THEN NEXT.

            CREATE InvRow.
            ASSIGN
               InvRow.InvNum    = lInvNo
               InvRow.SubInvNum = SubInvoice.SubInvNum
               InvRow.InvRowNum = next-value(irid).

            /* copy TEMP-TABLE TO InvRow */
            BUFFER-COPY ttIR 
               EXCEPT ttIR.InvNum ttIR.InvRowNum ttIR.SubInvNum
               TO InvRow.

            IF ttIR.RowType = 5 THEN ldMinConsAmt = ldMinConsAmt + ttIR.Amt.
         
         END.
                                    
         /* mark events */
         fMarkInvoiceItems(SubInvoice.MsSeq,
                           Invoice.InvNum,
                           SubInvoice.SubInvNum,
                           iiInvType).
                                    
         &IF "{&EDRHandling}" NE "NO" 
         &THEN
         /* CLI counters to database */
         FOR EACH ttCLI WHERE 
            ttCLI.MsSeq = SubInvoice.MsSeq AND
            ttCLI.InvSeq = SubInvoice.InvSeq:
            CREATE InvASub.
            BUFFER-COPY ttCLI TO InvASub.
            ASSIGN InvASub.InvNum    = Invoice.InvNum
                   InvASub.SubInvNum = SubInvoice.SubInvNum.
         END.
         &ENDIF

         IF NOT llCashInvoice THEN DO:

           /* mark invoice sequence as billed */
            FIND LAST bufseq USE-INDEX InvSeq WHERE 
                      bufseq.InvSeq  = SubInvoice.InvSeq AND
                      bufseq.CustNum = Customer.CustNum AND
                      bufseq.InvNum  = 0
            NO-LOCK no-error.
            IF AVAILABLE bufseq THEN DO:
               FIND CURRENT bufseq EXCLUSIVE-LOCK.
               ASSIGN 
                  bufseq.Billed = TRUE
                  bufseq.InvNum    = Invoice.InvNum
                  bufseq.SubInvNum = SubInvoice.SubInvNum.

               &IF "{&InitPersistent}" NE "NO" AND "{&EDRHandling}" = "NO" 
               &THEN
               clsInvRowCounter:UnifyInvRowCounterTaxHandling(
                                            Invoice.CustNum,
                                            bufseq.InvSeq,
                                            Invoice.VatIncl,
                                            TABLE ttIR BY-REFERENCE).

               clsInvRowCounter:MarkInvRowCountersBilled(Customer.CustNum,
                                                         bufseq.InvSeq,
                                                         Invoice.InvNum,
                                                         SubInvoice.SubInvNum).
               &ENDIF                                          
            END.
            
            /* mark this period as handled for minimum consumption
              always, even if this invoice exceeded min.consumption limit */
            IF NOT CAN-FIND(FIRST MinConsumption WHERE
                                  MinConsumption.MsSeq = SubInvoice.MsSeq AND
                                  MinConsumption.ToDate = Invoice.ToDate)
            THEN DO:
               CREATE MinConsumption.
               ASSIGN 
                  MinConsumption.MsSeq    = SubInvoice.MsSeq
                  MinConsumption.FromDate = Invoice.FromDate
                  MinConsumption.ToDate   = Invoice.ToDate
                  MinConsumption.InvNum   = Invoice.InvNum
                  MinConsumption.Amount   = ldMinConsAmt.
            END.      

         END.                                           

         fLatestInv(Customer.CustNum,
                    SubInvoice.CLI, 
                    Invoice.FromDate). 

         /* nul current month's saldo counter */
         IF Invoice.ToDate >= TODAY AND NOT llCashInvoice AND
            Invoice.InvType NE 99 THEN 
         FOR FIRST SaldoCounter EXCLUSIVE-LOCK WHERE
                   SaldoCounter.MsSeq  = SubInvoice.MsSeq AND
                   SaldoCounter.Period = YEAR(Invoice.ToDate) * 100 + 
                                         MONTH(Invoice.ToDate):
            SaldoCounter.Amt = 0.                             
         END.
 
         IF llRunVolDisc THEN 
            /* update CALL counters 
               (voldisc may have changed call amounts) */
            RUN pUpdateCallCounter IN fhVDHandle (INPUT SubInvoice.InvSeq).
          
         RELEASE SubInvoice.
      END.
      
      FIND CURRENT Invoice EXCLUSIVE-LOCK.
      
      liLastCreated = lInvno.
                
      IF llRunVolDisc THEN 
         /* update monthly counters 
            (voldisc may have changed call amounts) */
         RUN pUpdateMthCounter IN fhVDHandle (Customer.CustNum). 
               
      /* mark invoice nbr to order */
      IF llCashInvoice AND liOrderID > 0 THEN 
      FOR FIRST Order EXCLUSIVE-LOCK WHERE
                Order.Brand   = gcBrand AND
                Order.OrderID = liOrderID:
         Order.InvNum = Invoice.InvNum.

         /* ar account for cash invoices */
         FOR FIRST CLIType NO-LOCK WHERE
                   CLIType.Brand   = gcBrand AND
                   CLIType.CLIType = Order.CLIType:
            IF CLIType.ArAccNum > 0 THEN 
               Invoice.ArAccNum = CLIType.ArAccNum.
         END.
      END.

      /* vat data to combined invoice */
      lLoop = 0.
      FOR EACH ttRowVat WHERE
               ttRowVat.ITGroupID = ttInv.ITGroupID 
      BY ttRowVat.VatPerc:
         ASSIGN 
            lLoop                     = lLoop + 1
            Invoice.VatBasis[lLoop]   = ttRowVat.VatBasis
            Invoice.VatPercent[lLoop] = ttRowVat.VatPerc
            Invoice.VatAccount[lLoop] = ttRowVat.VatAcc
            Invoice.VatAmount[lLoop]  = ttRowVat.VatAmt.
      END.

      /* balances */
      FOR EACH ttCustBal WHERE
               ttCustBal.ITGroupID = ttInv.ITGroupID AND 
               ttCustBal.Used NE 0:

         IF LOOKUP(ttCustBal.Type,"OP,AP") > 0 THEN DO:

            fCustBal(Customer.CustNum,
                     ttCustBal.CLI,
                     ttCustBal.Type,
                     -1 * ttCustBal.Used).

            CREATE OPLog.
            ASSIGN OPLog.CustNum   = Invoice.CustNum
                   OPLog.EventDate = Invoice.InvDate
                   OPLog.UserCode  = katun
                   OPLog.EventType = IF ttCustBal.Type = "AP"
                                     THEN 11
                                     ELSE 3
                   OPLog.InvNum    = Invoice.InvNum
                   OPLog.SubInvNum = 0
                   OPLog.CLI       = ttCustBal.CLI
                   OPLog.Amt       = -1 * ttCustBal.Used.
                   OPLog.CreStamp  = fMakeTS().
         END.
         
         /* interest event */
         ELSE IF ttCustBal.Type = "INT" AND Invoice.InterestAmt NE 0 THEN DO:
             fCustBal(Customer.CustNum,
                      ttCustBal.CLI,
                      "INT",
                      -1 * ttCustBal.Used). 
         END.
      END.

      /* memo to invoice of the reason for ignoring min limit */
      IF liIgnoMin > 0 THEN DO:
         CREATE Memo.
         ASSIGN Memo.Brand     = gcBrand
                Memo.HostTable = "Invoice"
                Memo.KeyValue  = STRING(Invoice.InvNum)
                Memo.CustNum   = Invoice.CustNum
                Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
                Memo.CreUser   = katun
                Memo.MemoTitle = "Minimum Amount Limit Ignored"
                Memo.MemoText  = "Reason: " + STRING(liIgnoMin)
                Memo.CreStamp  = fMakeTS().
      END.
 
      /* update accounts */
      IF lUpdAcc THEN DO:
   
         /* a/r balance */
         fCustBal(Customer.CustNum,
                  "",
                  "ARBAL",
                  Invoice.InvAmt). 

         /* unbilled balance */
         fCustCount(Customer.CustNum,
                    "UB",
                    -1 * Invoice.AmtExclVat). 
      END.

      ASSIGN liPaymAcc  = 0
             lcPaymSrc  = ""
             lcPaymMemo = "".
           
      FIND CURRENT Invoice EXCLUSIVE-LOCK.
             
      /* IF total value is less than given limit or
         customer is a credit loss customer then post TO credit loss  */
      IF Invoice.CustNum = liCLossCust OR 
         (ldCredLoss       > 0 AND        
          Invoice.AdvPaym  = 0 AND
          Invoice.OverPaym = 0 AND
          Invoice.InvAmt   > 0 AND 
          Invoice.InvAmt   < ldCredLoss)
      THEN DO:
         ASSIGN 
            liPaymAcc         = liCLossAcc
            lcPaymSrc         = "CL"
            liPaymType        = 1
            lcPaymMemo        = (IF Customer.CustNum = liCLossCust
                                 THEN "Credit loss customer"
                                 ELSE "Cleaning run") +
                                " Handler: " + katun
            Invoice.PaymState = 3. 
      END. 
 
      /* IF customer is in "own use" category or in "vip use" then post 
         to own use */
      ELSE IF Customer.Category = lcOwnUse OR Customer.Category = lcVipUse
      THEN DO:
         ASSIGN 
            liPaymAcc   = IF Customer.Category = lcOwnUse
                          THEN liOwnUsePA
                          ELSE liVipUsePA
            lcPaymSrc   = "BR"
            liPaymType  = 0
            lcPaymMemo  = (IF Customer.Category = lcOwnUse
                           THEN "Own" ELSE "VIP") +
                          " use.  Handler: " + katun.
      END.

      /* if total amount is zero then mark as paid */
      ELSE IF Invoice.InvAmt = 0 THEN DO:
         Invoice.PaymState = 2.
         
         FOR EACH SubInvoice OF Invoice EXCLUSIVE-LOCK:
            SubInvoice.PaymState = 2.
         END.
      END.

      /* write payment */
      IF lcPaymSrc > "" THEN DO:
         RUN Ar/makepaym.p(BUFFER Invoice,
                        Invoice.InvAmt,
                        Invoice.InvDate,
                        liPaymAcc,
                        lcPaymSrc,
                        liPaymType,
                        FALSE,
                        FALSE,
                        "",
                        lcPaymMemo,
                        OUTPUT liVoucher).
      END.
 
      /* Showing EACH customers transactions */
      IF NOT SESSION:BATCH AND NOT liSilent THEN DO:
         PAUSE 0.
         DISPLAY
            Invoice.InvNum FORMAT ">>>>>>>>9"
            Invoice.CustNum
            Invoice.CustName
            liEventQty
            Invoice.AmtExclVAT
         WITH FRAME LOG.
         DOWN WITH FRAME LOG.
      END.   

      ldTotAmt = ldTotAmt + Invoice.AmtExclVAT.
            
      IF NOT SESSION:BATCH AND NOT liSilent THEN 
         /* Updating the grand total on the screen */
         PUT SCREEN row 18 col 40 " * Total (ex VAT)" +
            STRING(ldTotAmt,"zzzzzzzz9.99-").
      PAUSE 0.
 
      /* external invoice id */
      IF llSetExtInvID THEN DO:
         FIND LAST ttNewInv WHERE
                   ttNewInv.InvGroup = Customer.InvGroup AND
                   ttNewInv.InvType  = iiInvType no-error.
         IF NOT AVAIL ttNewInv THEN 
            lcExtInvID = fGetInvNum(InvGroup.InvGroup,
                                    iiInvType,
                                    idaInvDate,
                                    OUTPUT lcSeqPref).
         ELSE DO:
            lcExtInvID = fLocalNextExtID(ttNewInv.SeqPrefix,
                                         ttNewInv.ExtInvID).
            lcSeqPref  = ttNewInv.SeqPrefix.                              
         END.
               
         FIND CURRENT Invoice EXCLUSIVE-LOCK.
            
         REPEAT:
            Invoice.ExtInvID = lcExtInvID NO-ERROR.
  
            /* make sure that new field values are visible to other
               sessions */
            FIND Invoice WHERE RECID(Invoice) = liInvRecid EXCLUSIVE-LOCK.
               
            /* another process has used the same number */
            IF ERROR-STATUS:ERROR OR Invoice.ExtInvID = ""  OR
               CAN-FIND(FIRST bChkInv USE-INDEX ExtInvID WHERE
                           bChkInv.Brand    = gcBrand AND 
                           bChkInv.ExtInvID = lcExtInvID AND
                           RECID(bChkInv) NE RECID(Invoice)) 
            THEN DO:
               lcExtInvID = fLocalNextExtID(lcSeqPref,
                                            lcExtInvID).
               NEXT.                             
            END.   

            LEAVE.
         END.

         Invoice.RefNum = Invoice.ExtInvID.
      END.
            
      /* list of newly created invoices */
      CREATE ttNewInv.
      ASSIGN 
         ttNewInv.InvNum     = Invoice.InvNum
         ttNewInv.ExtInvID   = Invoice.ExtInvID
         ttNewInv.InvGroup   = Customer.InvGroup
         ttNewInv.UpdCustBal = InvGroup.UpdCustBal
         ttNewInv.InvType    = Invoice.InvType
         ttNewInv.SeqPrefix  = lcSeqPref
         ttNewInv.Qty        = Invoice.AmtExclVAT
         ttNewInv.VATAmt     = Invoice.InvAmt
         liIRLQty            = liIRLQty + 1
         ldIRLAmt            = ldIRLAmt + Invoice.InvAmt.

      RELEASE Invoice.
 
      /* function execution logging */
      IF liUpdInterval > 0 AND liIRLQty MOD liUpdInterval = 0 THEN DO:
         IF NOT fUpdateFuncRunProgress(liFRProcessID,liIRLQty) THEN
            RETURN "ERROR:Stopped".
      END.   
 
   END.   /* foreach ttInv */

   /* mark "Next invoice not direct debit" as "Direct Debit" */
   IF llNextInvNdd THEN DO:
      FIND CURRENT Customer EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN
         Customer.ChargeType = 2.
      FIND CURRENT Customer NO-LOCK NO-ERROR.
   END.
 
   RETURN "".

END PROCEDURE.

PROCEDURE pValidateInvoice:
      
   DEF INPUT  PARAMETER idaPeriodEnd  AS DATE NO-UNDO.
   DEF OUTPUT PARAMETER olCreate      AS LOG  NO-UNDO.   
   DEF OUTPUT PARAMETER oiIgnoreLimit AS INT  NO-UNDO.

   DEF VAR liLastBill  AS INT  NO-UNDO. 

   ASSIGN 
      olCreate      = FALSE
      oiIgnoreLimit = 0
      liLastBill    = 9999. 

   IF lRejBill THEN RETURN.
   
   /* if datelimit is given then check when customer was last billed */
   IF liMinDays > 0 THEN DO:

      FOR EACH Invoice NO-LOCK WHERE
               Invoice.Brand   = gcBrand AND
               Invoice.CustNum = Customer.CustNum:
         IF idaPeriodEnd - Invoice.ToDate < liLastBill
         THEN liLastBill = idaPeriodEnd - Invoice.ToDate.
      END.

      /* check calls if customer has no invoices yet */
      IF liLastBill = 9999 THEN DO:
         FIND FIRST InvSeq WHERE
                    InvSeq.CustNum = Customer.CustNum AND
                    InvSeq.Billed = FALSE
         NO-LOCK NO-ERROR.
         IF AVAIL InvSeq THEN liLastBill = idaPeriodEnd - InvSeq.FromDate.
      END.    
   END. 

   /* minimum limit reached */
   IF ttInv.InvAmt >= ttInvCust.MinInv THEN olCreate = TRUE.
                           
   /* permission to ignore min limit */                
   ELSE IF ttInvCust.LowVal = TRUE THEN ASSIGN 
      olCreate      = TRUE
      oiIgnoreLimit = 1. 
 
   IF NOT olCreate THEN 
   IgnoreMin:
   DO WHILE TRUE:
            
      /* over/adv.payment causes the value to decrease under limit */
      IF ttInv.AdvPaym + ttInv.OverPaym  < 0 AND
         ROUND(ttInv.InvAmt - ttInv.AdvPaym - ttInv.OverPaym,2) >
            ttInvCust.MinInv   
      THEN DO:
         oiIgnoreLimit = 2.
         LEAVE IgnoreMin.
      END.
                     
      /* limit for credit loss postings has been given */
      IF ldCredLoss     > 0 AND 
         ttInv.AdvPaym  = 0 AND
         ttInv.OverPaym = 0 AND
         ttInv.InvAmt   > 0 AND 
         ttInv.InvAmt   < ldCredLoss
      THEN DO:
         oiIgnoreLimit = 3.
         LEAVE IgnoreMin.
      END.

      LEAVE IgnoreMin.              
   END.
            
   /* minimum invoice amount can be ignored */    
   IF oiIgnoreLimit > 0 THEN olCreate = TRUE.
            
   /* when minimum days from last bill is given it overrides 
      other terms */
   IF liMinDays > 0 AND liLastBill < liMinDays THEN olCreate = FALSE. 

   IF olCreate AND ttInv.InvAmt <= 0 THEN DO: 
      olCreate = FALSE.

      IF ROUND(ttInv.InvAmt,2) = 0 THEN DO:
         IF llCashInvoice OR ttInv.AdvPaym + ttInv.OverPaym < 0 OR
            CAN-FIND(FIRST ttIR WHERE ttIR.Qty NE 0) THEN 
            olCreate = TRUE.
      END.
   END.
   
END PROCEDURE. /* pValidateInvoice */

/* create TEST invoices */
PROCEDURE pCreateTestInv:
   
   DEF INPUT  PARAMETER icMsSeqList AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaDueDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFromDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaToDate   AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiFeePeriod AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiExtratime AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilRerate    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty   AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icBillRun   AS CHAR NO-UNDO.

   ASSIGN
      lcMsSeqList = icMsSeqList
      liAdd2InvDate = iiExtratime.

   IF icBillRun = "" THEN icBillRun = "TEST-BR".
   
   run pCreateInv (idaInvDate,
                   idaFromDate,
                   idaToDate,
                   iiFeePeriod,
                   idaDueDate, /* IberPay Due Date */
                   idaDueDate, /* EBA Due Date */
                   ?, /* Fusion due date */
                   ilRerate,
                   ilDouble,
                   iiCustQty,
                   99,  /* force test invtype */
                   icBillRun,
                   TRUE).

END PROCEDURE.

/* create on demand invoices */
PROCEDURE pCreateODInv:

   DEF INPUT  PARAMETER iiMsSeq     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaInvDate  AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaDate1    AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaDate2    AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiPeriod    AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiExtraTime AS INT  NO-UNDO.
   DEF INPUT  PARAMETER ilRerate    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble    AS LOG  NO-UNDO.
   DEF OUTPUT PARAMETER oiInvNum    AS INT  NO-UNDO.

   ASSIGN
      lcMsSeqList = STRING(iiMsSeq)
      liAdd2InvDate = iiExtratime.

   run pCreateInv (idaInvDate,
                   idaDate1,
                   idaDate2,
                   iiPeriod,
                   ?, /* IberPay Due Date */
                   ?, /* EBA Due Date */
                   ?, /* Fusion due date */
                   ilRerate,
                   ilDouble,
                   1,
                   99, /* temporary test version, normally 1 */     
                   "OD-BR",
                   TRUE).

   /* was invoice created */
   oiInvNum = liLastCreated.
 
END PROCEDURE.

PROCEDURE pFunctionQueueRun:

   DEF INPUT  PARAMETER idaInvDate    AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaDate1      AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaDate2      AS DATE NO-UNDO.
   DEF INPUT  PARAMETER iiPeriod      AS INT  NO-UNDO.
   DEF INPUT  PARAMETER idaIberPayDueDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaEBADueDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER idaFusionDueDate AS DATE NO-UNDO.
   DEF INPUT  PARAMETER ilRerate      AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER ilDouble      AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER iiCustQty     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiInvType     AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icBillRun     AS CHAR NO-UNDO.
   DEF INPUT  PARAMETER ilSetExtID    AS LOG  NO-UNDO.
   DEF INPUT  PARAMETER iiFRProcessID AS INT  NO-UNDO.
   DEF INPUT  PARAMETER iiUpdInterval AS INT  NO-UNDO.
   DEF INPUT  PARAMETER icMsSeqList   AS CHAR NO-UNDO.

   ASSIGN
      liFRProcessID = iiFRProcessID
      liUpdInterval = iiUpdInterval
      lcMsSeqList   = icMsSeqList.

   run pCreateInv (idaInvDate,
                   idaDate1,
                   idaDate2,
                   iiPeriod,
                   idaIberPayDueDate,
                   idaEBADueDate,
                   idaFusionDueDate,
                   ilRerate,
                   ilDouble,
                   iiCustQty,
                   iiInvType,
                   icBillRun,
                   ilSetExtID).

   RETURN RETURN-VALUE.
   
END PROCEDURE.

PROCEDURE pGenerateReport:
   fLog("new invoices generated by billing run",katun).
   FOR EACH ttNewInv NO-LOCK,
      FIRST invoice NO-LOCK WHERE
            invoice.invnum = ttNewInv.invnum:
     
      fLog(STRING(invoice.invnum) + "|" +
           STRING(invoice.custnum) + "|" + 
           STRING(invoice.amt),"NewInv").
   END.
END PROCEDURE. 

PROCEDURE pRatingQueues:

   DEF INPUT PARAMETER iiInvCust   AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaPeriodTo AS DATE NO-UNDO.
   DEF INPUT PARAMETER icQueue     AS CHAR NO-UNDO.
   
   DEF VAR lcParamList AS CHAR NO-UNDO.
   DEF VAR liDone      AS INT  NO-UNDO.

   CASE icQueue:
   WHEN "TMQueue" THEN DO:

      FOR EACH InvSeq NO-LOCK USE-INDEX CustNum WHERE
               InvSeq.CustNum = iiInvCust AND
               InvSeq.Billed  = FALSE AND
               InvSeq.ToDate <= idaPeriodTo:
         lcParamList = lcParamList + 
                        (IF lcParamList > "" THEN "," ELSE "") + 
                        STRING(InvSeq.InvSeq).
      END.
   
      IF lcParamList > "" THEN DO:
         THIS-PROCEDURE:PRIVATE-DATA = "TMQueue_Param:" + lcParamList.
         RUN Rate/tmqueue_analysis.p.
      END.   
   END.

   WHEN "TriggerRate" THEN DO:
      THIS-PROCEDURE:PRIVATE-DATA = "TriggerRate_Param:" + STRING(iiInvCust).
      RUN Rate/triggerrate.p(0,0,OUTPUT liDone).
   END.

   WHEN "RerateRequest" THEN DO:
      THIS-PROCEDURE:PRIVATE-DATA = "Rerate_Param:" + STRING(iiInvCust).
      RUN Rate/rerate_request.p(-1).
   END.
   
   END CASE.

   THIS-PROCEDURE:PRIVATE-DATA = ?.

END PROCEDURE.

PROCEDURE pInvoiceSplit:

   DEF INPUT PARAMETER iiCustNum    AS INT  NO-UNDO.
   DEF INPUT PARAMETER idaPeriodBeg AS DATE NO-UNDO.
   DEF INPUT PARAMETER idaPeriodEnd AS DATE NO-UNDO.
   DEF INPUT PARAMETER idePeriodBeg AS DEC  NO-UNDO.
   DEF INPUT PARAMETER idePeriodEnd AS DEC  NO-UNDO.
    
   DEF VAR liITGroupID   AS INT  NO-UNDO.
   DEF VAR liITGroupID2   AS INT  NO-UNDO.
   DEF VAR liITGDeltype  AS INT  NO-UNDO.
   DEF VAR liITGDeltype2  AS INT  NO-UNDO.
   DEF VAR ldaMsOwnerFromDate AS DATE NO-UNDO.
   DEF VAR liMsOwnerFromTime  AS INT  NO-UNDO.
   DEF VAR lcCliEvent AS CHAR NO-UNDO. 

   /* if nothing to bill was found then create fake rows to each 
      invoice customer's cli which were active on the billing period,
      in order to get the minimum consumption */
   FOR EACH MsOwner NO-LOCK USE-INDEX InvCust WHERE 
            MsOwner.InvCust  = iiCustNum    AND
            MsOwner.TsEnd   >= idePeriodBeg AND
            MsOwner.TsBeg   <= idePeriodEnd AND
            MsOwner.TsBeg   >= idePeriodBeg AND
            MsOwner.PayType  = FALSE        AND 
            MsOwner.CLIEvent BEGINS "iS"
      BREAK BY MsOwner.MsSeq:
      
      IF NOT FIRST-OF(MsOwner.MsSeq) THEN NEXT.
      
      /* specific clis */
      IF lcMsSeqList > "" AND 
         LOOKUP(STRING(MsOwner.MsSeq),lcMsSeqList) = 0
      THEN NEXT.

      /* billing denied */
      IF LOOKUP(STRING(MsOwner.MsSeq),lcBillDeny) > 0 THEN NEXT.

      /* Check iSTC from non-fusion to fusion or vice-versa */

      fSplitTS(MsOwner.TsBeg,OUTPUT ldaMsOwnerFromDate,
               OUTPUT liMsOwnerFromTime).
      
      liITGroupID = fGetInvoiceTargetGroup(MsOwner.MsSeq,
                                           MsOwner.AgrCust,
                                           MsOwner.CustNum,
                                           (ldaMsOwnerFromDate - 1),
                                           OUTPUT liITGDeltype).

      liITGroupID2 = fGetInvoiceTargetGroup(MsOwner.MsSeq,
                                            MsOwner.AgrCust,
                                            MsOwner.CustNum,
                                            idaPeriodEnd,
                                            OUTPUT liITGDeltype2).

      /* There should be always be a different ITGroupID 
         since the plit is a between Fusion and non-Fusion subscription */
      IF NOT (liITGroupID > 0 AND liITGroupID2 > 0) OR 
         liITGroupID EQ liITGroupID2 THEN lcCliEvent = "iS". 
      ELSE lcCliEvent = MsOwner.CLIEvent.

      CREATE ttInvSplit.
      ASSIGN ttInvSplit.AgrCust    = MsOwner.AgrCust
             ttInvSplit.MsSeq      = MsOwner.MsSeq
             ttInvSplit.FromDate   = idaPeriodBeg
             ttInvSplit.SplitDate  = ldaMsOwnerFromDate
             ttInvSplit.ToDate     = idaPeriodEnd
             ttInvSplit.CLiEvent   = lcCliEvent
             ttInvSplit.ITGroupID[1]  = liITGroupID
             ttInvSplit.ITGDeltype[1] = liITGDeltype
             ttInvSplit.ITGroupID[2] = liITGroupID2
             ttInvSplit.ITGDeltype[2] = liITGDeltype2.
   END.
END.

