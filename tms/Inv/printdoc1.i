&GLOBAL-DEFINE AllIncludes YES

{Syst/commali.i}
{Func/cparam2.i}
{Inv/printdoc1tt.i}
{Func/transname.i}
{Func/ftransdir.i}
{Func/email.i}
{Func/fsubser.i}
{Func/customer_address.i}
{Func/fdestcountry.i}
{Syst/tmsconst.i}
{Func/ftaxdata.i}
{Mm/fbundle.i}
{Func/q25functions.i}

&GLOBAL-DEFINE INSTALLMENT_DISCOUNT_BILLCODES "DISCPAYTERMDIR,DISCPAYTERMINDIR"

DEF STREAM sLog.

DEF VAR lcRefAmt    AS CHAR  NO-UNDO. 
DEF VAR lcErrTxt    AS CHAR  NO-UNDO. 
DEF VAR lcLine      AS CHAR  NO-UNDO. 
DEF VAR lcLastError AS CHAR  NO-UNDO.
DEF VAR lcTransDir  AS CHAR  NO-UNDO.
DEF VAR lcFileExt   AS CHAR  NO-UNDO.
DEF VAR liPCnt      AS INT   NO-UNDO.
DEF VAR lcErrFile   AS CHAR  NO-UNDO. 
DEF VAR lcConfDir   AS CHAR  NO-UNDO. 
DEF VAR ldAmt       AS DEC   NO-UNDO.
DEF VAR ldMinRow    AS DEC   NO-UNDO.
DEF VAR ldMaxRow    AS DEC   NO-UNDO.
DEF VAR lcNonCombinedData AS CHAR  NO-UNDO.
DEF VAR ldFromPer   AS DEC   NO-UNDO.
DEF VAR ldInvoiceFromPer AS DEC   NO-UNDO.
DEF VAR ldToPer     AS DEC   NO-UNDO.
DEF VAR ldEventTS   AS DEC   NO-UNDO.
DEF VAR llInterrupt AS LOG   NO-UNDO.
DEF VAR lcCurrency    AS CHAR NO-UNDO. 
DEF VAR lcSesNum      AS CHAR NO-UNDO. 
DEF VAR lcNewLine     AS CHAR NO-UNDO.
DEF VAR ldMinInvAmt   AS DEC  NO-UNDO.
DEF VAR lcMinInvAmt   AS CHAR NO-UNDO. 
DEF VAR lcTaxZone     AS CHAR NO-UNDO.
DEF VAR lcBIName      AS CHAR NO-UNDO.
DEF VAR lcTipoName    AS CHAR NO-UNDO.
DEF VAR lcCTName      AS CHAR NO-UNDO.
DEF VAR ghttCall      AS HANDLE NO-UNDO.
DEF VAR lcBundleCLITypes AS CHAR NO-UNDO.
DEF VAR lcFusionCLITypes AS CHAR NO-UNDO.
DEF VAR gcCallQueryBegin AS CHAR NO-UNDO.
DEF VAR gcCallQueryEnd AS CHAR NO-UNDO.

DEFINE VARIABLE objDBConn AS CLASS Syst.CDRConnect NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable      AS CHAR
   FIELD GroupOrder    AS INT
   FIELD BillItemName  AS CHARACTER
   FIELD BIGroup     LIKE BillItem.BIGroup
   FIELD GroupType   LIKE BItemGroup.GroupType
   INDEX BIGroup GroupOrder BIGroup.


ASSIGN
   ghttCall         = BUFFER ttCall:HANDLE
   lcFusionCLITypes = fCParamC("FUSION_SUBS_TYPE")
   lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES")
   gcCallQueryBegin = "FOR EACH &1.MobCDR NO-LOCK WHERE "
   gcCallQueryEnd   = "MobCDR.InvCust = &1 AND " +
                      "MobCDR.InvSeq  = &2 AND " +
                      "MobCDR.DateST >= &3 AND " +
                      "MobCDR.DateST <= &4 " +
                      "BREAK BY MobCDR.BillCode BY MobCDR.DateSt".

DEFINE TEMP-TABLE ttExcludedRow NO-UNDO
   FIELD Rowid AS ROWID
   INDEX Rowid Rowid.

DEF TEMP-TABLE ttRow NO-UNDO
   FIELD RowGroup      AS CHAR  
   FIELD RowType       AS CHAR
   FIELD RowCode       AS CHAR
   FIELD RowBillCode   AS CHAR
   FIELD RowName       AS CHAR 
   FIELD RowAmtExclVat AS DEC
   FIELD RowVatAmt     AS DEC
   FIELD RowAmt        AS DEC
   FIELD RowQty        AS INT
   FIELD RowDur        AS INT
   FIELD RowData       AS DEC
   FIELD SubInvNum     AS INT
   FIELD GroupOrder    AS INT 
   FIELD RowOrder      AS INT
   FIELD RowToDate     AS DATE
   FIELD VoiceLimit    AS INT
   FIELD DataLimit     AS INT
   INDEX RowCode SubInvNum RowCode.

DEF TEMP-TABLE ttGraph NO-UNDO
   FIELD GraphGroup AS CHAR
   FIELD GraphAmt   AS DEC
   FIELD Order      AS INT
   INDEX GraphGroup GraphGroup
   INDEX Order Order.
   
DEF TEMP-TABLE ttVat NO-UNDO
   FIELD VatPerc  AS DEC
   FIELD VatAmt   AS DEC
   FIELD VatBasis AS DEC
   INDEX VatPerc VatPerc.

DEF TEMP-TABLE ttError NO-UNDO
    FIELD Inv    AS CHAR
    FIELD Cust   AS INT
    FIELD ErrMsg AS CHAR
    FIELD TableName AS CHAR
    FIELD KeyValue AS CHAR.

DEF TEMP-TABLE ttHead NO-UNDO
    FIELD Lang  AS INT
    FIELD Nbr   AS INT
    FIELD HTxt  AS CHAR
    INDEX Lang IS UNIQUE Lang Nbr.

DEF TEMP-TABLE ttSub NO-UNDO
   FIELD CLI          AS CHAR
   FIELD FixedNumber  AS CHAR
   FIELD CLIType      AS CHAR
   FIELD CliEvent     AS CHAR
   FIELD CTName       AS CHAR
   FIELD MsSeq        AS INT
   FIELD CallSpec     AS INT
   FIELD DataConv     AS INT 
   FIELD UserName     AS CHAR
   FIELD InstallmentAmt  AS DEC
   FIELD PenaltyAmt   AS DEC
   FIELD InstallmentDiscAmt AS DEC  
   FIELD TFBankBeforeAmt AS DEC EXTENT 6
   FIELD TFBankAfterAmt  AS DEC EXTENT 6 
   FIELD TFBankFooterText AS CHAR EXTENT 2
   FIELD OldCLIType      AS CHAR
   FIELD OldCTName       AS CHAR
   FIELD TariffActDate   AS CHAR
   FIELD MessageType     AS CHAR
   FIELD GBValue         AS DEC
   FIELD PrintCLI        AS LOGICAL INITIAL FALSE
   FIELD IUA             AS CHAR
   INDEX CLI CLI.
   
DEF TEMP-TABLE ttCLIType NO-UNDO
   FIELD BaseBundle AS CHAR
   FIELD CLI      AS CHAR
   FIELD CLIType  AS CHAR
   FIELD CTName   AS CHAR
   FIELD RateName AS CHAR
   FIELD TSBeg    AS DEC
   FIELD TSEnd    AS DEC
   INDEX TSBeg CLI TSBeg.

DEF TEMP-TABLE ttData NO-UNDO
   FIELD BIName AS CHAR
   FIELD DataAmt  AS DEC
   FIELD DataSum  AS DEC
   INDEX BIName BIName.

DEFINE TEMP-TABLE ttBillItemAndGroup NO-UNDO
   FIELD BillCode        AS CHARACTER
   FIELD BiGroup         AS CHARACTER
   FIELD BIName          AS CHARACTER
   FIELD GroupOrder      AS INTEGER
   FIELD GroupType       AS INTEGER
   FIELD PremiumBillCode AS LOGICAL INITIAL FALSE
   FIELD GBBillCode      AS LOGICAL INITIAL FALSE
   INDEX BillCode IS PRIMARY UNIQUE BillCode.

DEFINE TEMP-TABLE ttBillItemName NO-UNDO
   FIELD BillCode        AS CHARACTER
   FIELD Name            AS CHARACTER
   FIELD Language        AS INTEGER
   FIELD ToDate          AS DATE
   FIELD FromDate        AS DATE
   INDEX BillCode IS PRIMARY BillCode Language ToDate FromDate.

DEFINE TEMP-TABLE ttMSOwner NO-UNDO
   FIELD Type     AS INTEGER    
   FIELD TSBegin  AS DECIMAL
   FIELD TSEnd    AS DECIMAL
   FIELD PayType  AS LOGICAL
   FIELD CLIEvent AS CHARACTER
   FIELD InvCust  AS INTEGER
   FIELD CLIType  AS CHARACTER
   FIELD FusionCLIType AS LOGICAL INITIAL FALSE
   INDEX Type Type InvCust TSBegin DESCENDING TSEnd DESCENDING.

DEF BUFFER bttRow FOR ttRow.

FUNCTION fPopulateBillItemAndGroup RETURNS LOGICAL:
   
   DEFINE VARIABLE liGroupOrder AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liGroupType  AS INTEGER   NO-UNDO.
   FOR EACH BillItem NO-LOCK WHERE
      BillItem.Brand = Syst.Var:gcBrand
      BREAK BY BillItem.BIGroup:
      
      IF FIRST-OF(BillItem.BIGroup)
      THEN DO:
         liGroupOrder = 0.
         liGroupType  = 0.
         FOR BItemGroup NO-LOCK WHERE
            BItemGroup.Brand   = Syst.Var:gcBrand AND
            BItemGroup.BIGroup = BillItem.BIGroup:

            liGroupOrder = BItemGroup.InvoiceOrder.
            liGroupType  = BItemGroup.GroupType.
         END.
      END.

      FOR
         EACH Language NO-LOCK,
         EACH RepText NO-LOCK WHERE
            RepText.Brand = Syst.Var:gcBrand                AND
            RepText.TextType  = 1                  AND 
            RepText.LinkCode  = BillItem.BillCode  AND
            RepText.Language  = Language.Language:
      
         CREATE ttBillItemName.
         ASSIGN
            ttBillItemName.BillCode = BillItem.BillCode
            ttBillItemName.Name     = RepText.RepText
            ttBillItemName.Language = Language.Language
            ttBillItemName.ToDate   = RepText.ToDate
            ttBillItemName.FromDate = RepText.FromDate
            .
      END.

      CREATE ttBillItemAndGroup.
      ASSIGN
         ttBillItemAndGroup.BillCode        = BillItem.BillCode
         ttBillItemAndGroup.BIGroup         = BillItem.BIGroup
         ttBillItemAndGroup.BIName          = BillItem.BIName
         ttBillItemAndGroup.PremiumBillCode = TRUE WHEN BillItem.BIGroup = "6"
         ttBillItemAndGroup.GBBillCode      = TRUE WHEN BillItem.BIGroup = {&BITEM_GRP_GB}
         ttBillItemAndGroup.GroupOrder      = liGroupOrder
         ttBillItemAndGroup.GroupType       = liGroupType
         .
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fBillItemName RETURNS CHARACTER
   (icBillCode AS CHARACTER,
    idaDate    AS DATE,
    iiLanguage AS INTEGER):

   DEFINE VARIABLE lcReturnValue AS CHARACTER INITIAL ? NO-UNDO.
   DEFINE BUFFER lbttBillItemAndGroup FOR ttBillItemAndGroup.

   DO WHILE TRUE:
      FOR FIRST ttBillItemName WHERE
          ttBillItemName.BillCode = icBillCode  AND
          ttBillItemName.Language = iiLanguage  AND
          ttBillItemName.ToDate   >= idaDate    AND
          ttBillItemName.FromDate <= idaDate:
   
         lcReturnValue = ttBillItemName.Name.
      END.

      /* use basic language if nothing was defined in the desired one */
      IF lcReturnValue = ? AND iiLanguage NE 1 THEN DO:
         iiLanguage = 1.
         NEXT.
      END.

      LEAVE.
   END.
   
   IF lcReturnValue = ?
   THEN FOR FIRST lbttBillItemAndGroup WHERE
           lbttBillItemAndGroup.BillCode = icBillCode:
           lcReturnValue = lbttBillItemAndGroup.BIName.
        END.
   
   IF lcReturnValue = ?
   THEN RETURN "".
   
   RETURN lcReturnValue.

END FUNCTION.

FUNCTION fHeadTxt RETURNS CHARACTER
   (iNbr AS INT,
    iLang  AS INT).
    
   FIND FIRST ttHead WHERE
      ttHead.Lang = iLang AND
      ttHead.Nbr  = iNbr NO-ERROR.
   IF NOT AVAILABLE ttHead THEN 
   FIND FIRST ttHead WHERE
      ttHead.Lang = 1 AND
      ttHead.Nbr  = iNbr NO-ERROR.
   
   IF AVAILABLE ttHead THEN RETURN ttHead.HTxt.
   ELSE RETURN "".
   
END FUNCTION.
 
FUNCTION fErrLine RETURNS LOGICAL
    (iMessage AS CHAR).

    CREATE ttError.
    ASSIGN ttError.Inv    = Invoice.ExtInvID
           ttError.Cust   = Invoice.CustNum
           ttError.ErrMsg = iMessage
           lcLastError    = iMessage.

    /* delete the temp-table, so that "printstate" doesn't get marked */
    IF AVAILABLE ttInvoice THEN ASSIGN 
       ttInvoice.ErrMsg = iMessage
       ttInvoice.Printed = 2. /* not printed + error happened */

END FUNCTION.

FUNCTION fDispXMLDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>>>>>>9.99")).
   
END FUNCTION.

FUNCTION fDispXMLDecimal3 RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>>>>>>9.999")).
   
END FUNCTION.

/* get name for an item */
FUNCTION fLocalItemName RETURNS CHARACTER
   (icItem     AS CHAR,
    icCode     AS CHAR,
    iiLanguage AS INT,
    idtDate    AS DATE):
   
   DEF VAR lcName AS CHAR NO-UNDO. 

   /* translation or basic name */  
   lcName = fGetItemName(Syst.Var:gcBrand,
                         icItem,
                         icCode,
                         iiLanguage,
                         idtDate).
   
   IF lcName = "" OR lcName = ? THEN lcName = icCode.
      
   /* undefined value, stop handling */
   IF lcName = "#UNDEFINED" THEN DO:
      fErrLine("Undefined name for " + icItem + " " + icCode).
      QUIT.   /* retry of PrintMainLoop catches this */
   END.
      
   RETURN lcName.

END FUNCTION.

FUNCTION fDispDate RETURNS CHARACTER
   (idtDate    AS DATE,
    iiLanguage AS INT):

   RETURN STRING(DAY(idtDate),"99") + 
                 "-" +
                 fHeadTxt(270 + MONTH(idtDate),iiLanguage) +
                 "-" +
                 STRING(YEAR(idtDate),"9999").
   
END FUNCTION.

FUNCTION fDispDayMonth RETURNS CHARACTER
   (idtDate    AS DATE,
    iiLanguage AS INT):

   RETURN STRING(DAY(idtDate),"99") + 
                 " " +
                 fHeadTxt(270 + MONTH(idtDate),iiLanguage).
   
END FUNCTION.

FUNCTION fLocalCCName RETURNS CHARACTER:

   DEF VAR lcCCName AS CHAR NO-UNDO.
   
   lcCCName = "".
   
   /* country name for roaming */
   IF LOOKUP(STRING(ttCall.SpoCMT),{&ROAMING_CALLCASE}) > 0 THEN DO:
      lcCCName = fDestCountryName(Syst.Var:gcBrand,
                                  liLanguage,
                                  ttCall.SpoCMT,
                                  ttCall.DateSt,
                                  ttCall.DtlSeq,
                                  ttCall.GsmBnr,
                                  ttCall.BType,
                                  ttCall.MSCID,
                                  ttCall.ServiceName).
      /* undefined value, stop handling */
      IF lcCCName = "#UNDEFINED" THEN DO:
         fErrLine("Undefined name for country").
         QUIT.   /* retry of PrintMainLoop catches this */
      END.
   END.
   /* ServiceName for Premium Number */
   ELSE IF ttCall.BIGroup = "6" THEN
      lcCCName = ttCall.ServiceName.
   /* ServiceName for Google Billing */
   ELSE IF ttCall.BIGroup = {&BITEM_GRP_GB} THEN
      lcCCName = ttCall.ServiceName.


   /* ccn for others or if roaming country was not found */
   IF lcCCName = "" THEN 
      lcCCName = fLocalItemName("CCN",
                                STRING(ttCall.CCN),
                                liLanguage,
                                ttCall.DateSt).

   IF lcCCName > "" THEN
      lcCCName = SUBSTRING(lcCCName,1,34).

   RETURN lcCCName.
   
END FUNCTION.

FUNCTION fTFBankFooterText RETURNS LOGICAL
   (icBillCode AS CHAR):

   DEF VAR lcTFRVTermBillCode   AS CHAR NO-UNDO. 
   DEF VAR liFooterConf1        AS INT  NO-UNDO.
   DEF VAR liFooterConf2        AS INT  NO-UNDO.
   DEF VAR ldeRVPerc            AS DEC  NO-UNDO. 
   DEF VAR ldeTotalAmount       AS DEC  NO-UNDO. 
   DEF VAR ldaOrderDate         AS DATE NO-UNDO. 
   DEF VAR lcPenaltyBillCode    AS CHAR NO-UNDO.
   DEF VAR liFFCount            AS INT  NO-UNDO. 
   DEF VAR lcPaytermEndCodes    AS CHAR NO-UNDO. 
   DEF VAR liAmtPos             AS INT  NO-UNDO.
   DEF VAR liFtrPos             AS INT  NO-UNDO.
   DEF VAR ldTAE                AS DEC  NO-UNDO.
   DEF VAR liPeriodFrom         AS INT NO-UNDO. 
   DEF VAR liPeriodTo           AS INT NO-UNDO. 
   DEF VAR lcLogText            AS CHAR NO-UNDO. 
   DEF VAR liPhase              AS INT NO-UNDO.
   DEF VAR ldtInvToDate         AS DATE NO-UNDO.
   DEF VAR liPeriod             AS INT NO-UNDO.

   DEF BUFFER bPenaltySingleFee FOR SingleFee.
   DEF BUFFER bQ25SingleFee FOR SingleFee.
      
   /* 1 month reduced due to possible Q25 fee */
   ASSIGN
      ldtInvToDate = ADD-INTERVAL(Invoice.ToDate, -1, "months") 
      liPeriod    = YEAR(ldtInvToDate) * 100 + MONTH(ldtInvToDate).
   
   /* PAYTERM values to pos. 4-6, footer 1
      RVTERM  values to pos. 1-3, footer 2
   */
   IF icBillCode EQ "PAYTERM" THEN ASSIGN
      liAmtPos = 4
      liFtrPos = 1.
   ELSE ASSIGN
      liAmtPos = 1
      liFtrPos = 2. 

   liFFCount = liAmtPos. 
   
   FOR EACH FixedFee NO-LOCK WHERE
            FixedFee.Brand      = Syst.Var:gcBrand                    AND
            FixedFee.CustNum    = Invoice.CustNum            AND
            FixedFee.HostTable  = "MobSub"                   AND
            FixedFee.KeyValue   = STRING(SubInvoice.MsSeq)   AND
            FixedFee.BillCode   = icBillCode                 AND
            FixedFee.EndPeriod >= liPeriod                   AND
            FixedFee.BegDate   <= Invoice.Todate BY FixedFee.BegDate:
      
      IF FixedFee.BillCode EQ "PAYTERM" THEN
         FIND FIRST bQ25SingleFee NO-LOCK WHERE
                    bQ25SingleFee.Brand       = Syst.Var:gcBrand              AND
                    bQ25SingleFee.Custnum     = FixedFee.Custnum     AND
                    bQ25SingleFee.HostTable   = FixedFee.HostTable   AND
                    bQ25SingleFee.KeyValue    = Fixedfee.KeyValue    AND
                    bQ25SingleFee.SourceKey   = FixedFee.SourceKey   AND
                    bQ25SingleFee.SourceTable = FixedFee.SourceTable AND
                    bQ25SingleFee.SourceTable = FixedFee.SourceTable AND
                    bQ25SingleFee.CalcObj     = "RVTERM"             AND
             LOOKUP(bQ25SingleFee.BillCode,{&TF_RVTERM_BILLCODES}) > 0 NO-ERROR.
      ELSE RELEASE bQ25SingleFee.

      /* Q25 M22-M24 picture */
      IF AVAIL bQ25SingleFee AND
         NOT bQ25SingleFee.Billed AND
         ttInvoice.q25Phase > 0 THEN DO:

         ASSIGN
            liPeriodFrom = YEAR(Invoice.InvDate) * 100 + 
                           MONTH(Invoice.InvDate)
            liPeriodTo = fper2peradd(liPeriodFrom,2).

         IF bQ25SingleFee.BillPeriod <= liPeriodTo AND
            bQ25SingleFee.BillPeriod >= liPeriodFrom THEN DO:
         
            IF bQ25SingleFee.BillPeriod EQ liPeriodFrom THEN 
               liphase = {&Q25_MONTH_24}.
            ELSE IF bQ25SingleFee.BillPeriod EQ liPeriodTo THEN
               liphase = {&Q25_MONTH_22}.
            ELSE liphase = {&Q25_MONTH_23}.

            IF liphase < ttInvoice.q25Phase AND
               fisQ25ExtensionAllowed(BUFFER bQ25SingleFee, 
                                      OUTPUT lcLogText) THEN 
               ttinvoice.q25Phase = liPhase.  
         END. 
      END.
     
      /* Footer text for financed by bank installments */
      IF LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) = 0 THEN NEXT.
      IF liFFCount > liAmtPos + 2 THEN LEAVE.
            
      IF FixedFee.TFBank = {&TF_BANK_UNOE} THEN
         ASSIGN lcTFRVTermBillCode    = "RVTERM1EF" 
                liFooterConf1         = 536
                liFooterConf2         = 557
                lcPenaltyBillCode     = FixedFee.BillCode + "END1E".
      ELSE IF FixedFee.TFBank = {&TF_BANK_SABADELL} THEN
         ASSIGN lcTFRVTermBillCode    = "RVTERMBSF"
                liFooterConf1         = 558
                liFooterConf2         = 559
                lcPenaltyBillCode     = FixedFee.BillCode + "ENDBS".
      ELSE IF FixedFee.TFBank = {&TF_BANK_CETELEM} THEN
         ASSIGN lcTFRVTermBillCode    = "RVTERMBCF"
                liFooterConf1         = 575
                liFooterConf2         = 576
                lcPenaltyBillCode     = FixedFee.BillCode + "ENDBC".
      ELSE NEXT.

      IF AVAIL bQ25SingleFee AND
               bQ25SingleFee.BillCode NE lcTFRVTermBillCode THEN
         RELEASE bQ25SingleFee.
      
      FOR EACH FFItem NO-LOCK WHERE
               FFItem.FFNum = FixedFee.FFNum AND
               FFItem.BillCode BEGINS FixedFee.BillCode:
            
         IF NOT FFItem.Billed THEN ASSIGN
            ttSub.TFBankAfterAmt[liFFCount]  = ttSub.TFBankAfterAmt[liFFCount] +
                                               FFItem.Amt.
         ELSE IF FFItem.InvNum = Invoice.InvNum AND FFItem.Billed THEN ASSIGN
            ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                               FFItem.Amt.
      END.
            
      ASSIGN ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                                ttSub.TFBankAfterAmt[liFFCount].
   
      IF AVAIL bQ25SingleFee THEN DO:
          
         IF NOT bQ25SingleFee.Billed THEN ASSIGN
            ttSub.TFBankAfterAmt[liFFCount]  = ttSub.TFBankAfterAmt[liFFCount] +
                                               bQ25SingleFee.Amt
            ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                               bQ25SingleFee.Amt.
        
         ELSE IF bQ25SingleFee.InvNum = Invoice.InvNum AND bQ25SingleFee.Billed
                 /* Q25 fee should be excluded if it's filtered out
                    due to Q25 extension.
                    TODO: This doesn't work if the subinvoice contains
                    more than one Q25 invoice row for the same period */
                 AND CAN-FIND(  
                 FIRST ttRow NO-LOCK WHERE
                       ttRow.SubInvNum = SubInvoice.SubInvNum AND
                       ttRow.RowCode BEGINS "33" AND
                       LOOKUP(ttRow.RowBillCode,
                       {&TF_RVTERM_BILLCODES}) > 0) THEN
            ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                               bQ25SingleFee.Amt.
      END. 
        
      FOR FIRST bPenaltySingleFee NO-LOCK WHERE
                bPenaltySingleFee.Brand       = Syst.Var:gcBrand              AND
                bPenaltySingleFee.Custnum     = FixedFee.Custnum     AND
                bPenaltySingleFee.HostTable   = FixedFee.HostTable   AND
                bPenaltySingleFee.KeyValue    = Fixedfee.KeyValue    AND
                bPenaltySingleFee.SourceTable = "FixedFee" AND
                bPenaltySingleFee.SourceKey   = STRING(FixedFee.FFNUM) AND
                bPenaltySingleFee.BillCode    = lcPenaltyBillCode:
         
         IF NOT bPenaltySingleFee.Billed THEN ASSIGN
            ttSub.TFBankAfterAmt[liFFCount]  = ttSub.TFBankAfterAmt[liFFCount] +
                                               bPenaltySingleFee.Amt
            ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                               bPenaltySingleFee.Amt.
        
         ELSE IF bPenaltySingleFee.InvNum = Invoice.InvNum AND 
                 bPenaltySingleFee.Billed THEN ASSIGN
            ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                               bPenaltySingleFee.Amt.
      END. 
      
      IF ttSub.TFBankBeforeAmt[liFFCount] EQ 0 THEN NEXT.
              
      ldTAE = ?.
         
      IF FixedFee.BillCode EQ "RVTERM" THEN ldTAE = 0.
      ELSE DO:
      
         ldeRVPerc = 0.
      
         IF AVAIL bQ25SingleFee THEN DO: 

            FOR FIRST FMItem NO-LOCK WHERE
                      FMItem.Brand     = Syst.Var:gcBrand            AND
                      FMItem.FeeModel  = FixedFee.FeeModel  AND
                      FMItem.ToDate   >= FixedFee.BegDate   AND
                      FMItem.FromDate <= FixedFee.BegDate:
               ldeTotalAmount = ROUND(fmitem.FFItemQty * fmitem.Amount,2).
               ldeRVPerc = TRUNC(bQ25SingleFee.Amt /
                          (ldeTotalAmount + bQ25SingleFee.Amt) * 100 + 0.05,1).
            END.
         END.
      
         ldaOrderDate = FixedFee.BegDate.

         IF FixedFee.OrderId > 0 THEN DO:
            FIND FIRST Order NO-LOCK WHERE
                       Order.Brand = Syst.Var:gcBrand AND
                       Order.OrderId = FixedFee.OrderID NO-ERROR.
            IF AVAIL Order THEN Func.Common:mTS2Date(Order.CrStamp, 
                                         OUTPUT ldaOrderDate).
         END.
    
         FIND FIRST TFConf NO-LOCK WHERE
                    TFConf.RVPercentage = ldeRVPerc    AND
                    TFConf.ValidTo     >= ldaOrderDate AND
                    TFConf.ValidFrom   <= ldaOrderDate NO-ERROR.
       
         IF AVAIL TFConf THEN ASSIGN 
            ldTAE = TFConf.TAE.
      END.
      
      IF ldTAE NE ? AND NOT
      /*  YOT-4808 For Cetelem no footer text if there is also PAYTERMENDBC */
        (FixedFee.TFBank = {&TF_BANK_CETELEM} AND
         AVAIL bPenaltySingleFee AND
         bPenaltySingleFee.BillCode EQ "PAYTERMENDBC")
      THEN ASSIGN
         ttSub.TFBankFooterText[liFtrPos] = ttSub.TFBankFooterText[liFtrPos] + 
                                 (IF ttSub.TFBankFooterText[liFtrPos] > ""
                                  THEN CHR(10) ELSE "") +
                                 fHeadTxt(liFooterConf1,liLanguage)
         ttSub.TFBankFooterText[liFtrPos] = 
            REPLACE(ttSub.TFBankFooterText[liFtrPos],"#TAE",
                       REPLACE(fDispXMLDecimal(ldTAE),".",",")).
          
      IF AVAIL bPenaltySingleFee AND 
         LOOKUP(bPenaltySingleFee.BillCode,lcPaytermEndCodes) = 0 THEN ASSIGN
         ttSub.TFBankFooterText[liFtrPos] = ttSub.TFBankFooterText[liFtrPos] + 
                                 (IF ttSub.TFBankFooterText[liFtrPos] > ""
                                  THEN CHR(10) ELSE "") +
                                 fHeadTxt(liFooterConf2,liLanguage)
         lcPaytermEndCodes = lcPaytermEndCodes + "," + lcPenaltyBillCode.
      
      liFFCount = liFFCount + 1.
     
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fAddToExcludedRow RETURNS LOGICAL
   (irid AS ROWID):

   CREATE ttExcludedRow.
   ttExcludedRow.Rowid = irid.

   RETURN FALSE.

END FUNCTION.

PROCEDURE pGetInvoiceHeaderData:
   
   DEF BUFFER bOldInvoice FOR Invoice. 
   DEF VAR ldaInvoiceFrom AS DATE NO-UNDO. 
   DEF VAR lcGraphGroup AS CHAR NO-UNDO.
   DEF VAR liOrder AS INT NO-UNDO. 
  
   EMPTY TEMP-TABLE ttGraph.
   
   ASSIGN 
      ldMinRow   = 0  /* 0 is used as min if no negative amounts */
      ldMaxRow   = 0.

   ASSIGN
      ldFromPer   = Func.Common:mMake2DT(IF Invoice.FirstCall NE ? 
                             THEN Invoice.FirstCall
                             ELSE Invoice.FromDate,0)
      ldInvoiceFromPer = Func.Common:mMake2DT(Invoice.FromDate,0)
      ldToPer     = Func.Common:mMake2DT(Invoice.ToDate,86399).

   /* get customer names, payment terms etc. */
   fSetCustData().

   /* check that address etc. are valid */
   lcErrTxt = fCheckAddress(lcCustName,
                            lcZipCode,
                            lcCountry,
                            OUTPUT lcCountryName).

   IF lcErrTxt NE "" THEN DO:
      fErrLine(lcErrTxt).
      RETURN "ERROR".
   END. 

   IF lcAddress = "" AND lcZipCode = "" AND lcPost = "" THEN DO:
      fErrLine("Address data is missing").
      RETURN "ERROR".
   END.

   /* Total consumption of 6 last invoices */
   ASSIGN
      ldaInvoiceFrom = invoice.todate - (32 * 5)
      ldaInvoiceFrom = DATE(MONTH(ldaInvoiceFrom), 1, YEAR(ldaInvoiceFrom))
      liOrder = 1.

   FOR EACH bOldInvoice NO-LOCK WHERE
            bOldInvoice.Brand = Syst.Var:gcBrand AND
            bOldInvoice.Custnum = Invoice.Custnum AND
            bOldInvoice.InvDate >= ldaInvoiceFrom AND
            bOldInvoice.ITGroupID = Invoice.ITGroupID AND
            bOldInvoice.Todate >= ldaInvoiceFrom AND
            bOldInvoice.Todate <= Invoice.Todate AND
            bOldInvoice.InvType = 1 USE-INDEX Custnum BY ToDate:

      lcGraphGroup = STRING(MONTH(bOldInvoice.ToDate),"99") + "-" + 
                     STRING(YEAR(bOldInvoice.ToDate)).
      
      FIND FIRST ttGraph EXCLUSIVE-LOCK WHERE 
                 ttGraph.GraphGroup = lcGraphGroup NO-ERROR.

      IF NOT AVAILABLE ttGraph THEN DO:
         CREATE ttGraph.
         ASSIGN ttGraph.GraphGroup = lcGraphGroup
                ttGraph.Order = liOrder
                liOrder = liOrder + 1.
      END.

      ASSIGN
         ttGraph.GraphAmt = ttGraph.GraphAmt + bOldInvoice.InvAmt
         ldMinRow = MIN(ldMinRow,ttGraph.GraphAmt) 
         ldMaxRow = MAX(ldMaxRow,ttGraph.GraphAmt).
   END.
   
   RETURN "".
   
END PROCEDURE.


PROCEDURE pttMSOwner:

   DEFINE INPUT  PARAMETER iiMSSeq                  AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER iiCustNum                AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER ideFirstCall             AS DECIMAL NO-UNDO.
   DEFINE INPUT  PARAMETER ideFromInvoice           AS DECIMAL NO-UNDO.
   DEFINE INPUT  PARAMETER ideToInvoice             AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER olShouldCheckTermination AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER olPostPreDetected        AS LOGICAL NO-UNDO.

   EMPTY TEMP-TABLE ttMSOwner.

   DEFINE VARIABLE llHasBeenPre AS LOGICAL INITIAL FALSE NO-UNDO.
   DEFINE VARIABLE llFirst      AS LOGICAL INITIAL TRUE  NO-UNDO.
   DEFINE VARIABLE ldeFirst     AS DECIMAL               NO-UNDO.

   ASSIGN
      olShouldCheckTermination = FALSE
      olPostPreDetected        = FALSE
      ldeFirst                 = Func.Common:mSecOffSet(ideFirstCall,-1)
      .


   FOR EACH MSOwner NO-LOCK USE-INDEX MSSeq WHERE
      MSOwner.MSSeq = iiMSSeq:
         
      IF MSOwner.TSEnd < MSOwner.TSBegin 
      THEN NEXT.  

      IF llFirst AND MSOwner.InvCust = iiCustNum
      THEN DO:
         CREATE ttMSOwner.
         ASSIGN
            llFirst           = FALSE
            ttMSOwner.Type    = 1
            ttMSOwner.CLIType = IF MsOwner.TariffBundle > ""
                                THEN MsOwner.TariffBundle
                                ELSE MsOwner.CLIType 
            .
      END.

      IF MSOwner.TSBegin > ideToInvoice
      THEN NEXT.
      
      IF MSOwner.TSEnd < ldeFirst 
      THEN LEAVE.

      IF olShouldCheckTermination = FALSE AND
         MsOwner.TsEnd >= ideFirstCall    AND
         MsOwner.TsEnd <= ideToInvoice
      THEN olShouldCheckTermination = TRUE.

      IF olPostPreDetected = FALSE
      THEN DO:
         IF llHasBeenPre = FALSE AND MSOwner.PayType
         THEN llHasBeenPre = TRUE.

         ELSE IF llHasBeenPre AND MSOwner.PayType = FALSE  
         THEN olPostPreDetected = TRUE.
      END.

      /* We won't store is TSEnd is equal to ldeFirst as
         that time is special time used only for making
         post to pre detection work correctly */ 
      IF MSOwner.TSEnd > ldeFirst 
      THEN DO:
         CREATE ttMSOwner.
         ASSIGN
            ttMSOwner.Type          = 2
            ttMSOwner.TSBegin       = MSOwner.TSBegin
            ttMSOwner.TSEnd         = MSOwner.TSEnd
            ttMSOwner.PayType       = MSOwner.PayType
            ttMSOwner.CLIEvent      = MSOwner.CLIEvent
            ttMSOwner.InvCust       = MSOwner.InvCust
            /* Special handling to display the CLIType name        */
            /* as Bundle name for IPL or FLAT TARIFF subscriptions */
            ttMSOwner.CLIType       = IF LOOKUP(MSOwner.CLIType,lcBundleCLITypes) > 0
                                      THEN MSOwner.TariffBundle
                                      ELSE MsOwner.CLIType
            .
            
            IF ttMSOwner.CLIEvent = "iSS" AND
               LOOKUP(MsOwner.CLIType,lcFusionCLITypes) > 0
            THEN ttMSOwner.FusionCLIType = TRUE.
      END.
   END.

END PROCEDURE.


PROCEDURE pGetSubInvoiceHeaderData:

   DEF VAR liCount            AS INT  NO-UNDO.
   DEF VAR ldaOwnerDate       AS DATE NO-UNDO.
   DEF VAR liOwnerTime        AS INT  NO-UNDO.
   DEF VAR ldPeriodFrom       AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo         AS DEC  NO-UNDO.
   DEF VAR lcGroupCode        AS CHAR NO-UNDO.
   DEF VAR llPTFinancedByBank AS LOG  NO-UNDO.
   DEF VAR llRVFinancedByBank AS LOG  NO-UNDO.
   DEF VAR liQ25Phase         AS INT  NO-UNDO.
   
   DEFINE VARIABLE llShouldCheckTermination AS LOGICAL NO-UNDO.
   DEFINE VARIABLE llPostPreDetected        AS LOGICAL NO-UNDO.
   DEFINE VARIABLE ldeActStamp              AS DECIMAL NO-UNDO.
   DEFINE VARIABLE lliSTR                   AS LOGICAL NO-UNDO.
   DEFINE VARIABLE liLoop                   AS INTEGER NO-UNDO.
   DEFINE VARIABLE lcBundleName             AS CHARACTER NO-UNDO.

   DEF BUFFER UserCustomer    FOR Customer.
   DEF BUFFER bServiceLimit   FOR ServiceLimit.
   DEF BUFFER bMServiceLimit  FOR MServiceLimit.

   EMPTY TEMP-TABLE ttSub.
   EMPTY TEMP-TABLE ttCLIType.
          
   FOR EACH SubInvoice OF Invoice NO-LOCK:

      CREATE ttSub.
      ASSIGN ttSub.CLI         = SubInvoice.CLI
             ttSub.FixedNumber = SubInvoice.FixedNumber
             ttSub.MsSeq       = SubInvoice.MsSeq. 
      
      /*NEBACO-7*/
      /*Find IUA for the subscription*/
      IF ttSub.FixedNumber NE "" AND ttSub.FixedNumber NE ? THEN DO:
         FIND FIRST OrderFusion NO-LOCK WHERE 
                    OrderFusion.FixedNumber EQ ttSub.Fixednumber NO-ERROR.
         IF AVAIL OrderFusion AND OrderFusion.IUA NE "" THEN
            ttSub.IUA = OrderFusion.IUA.
      END.
      /* user name */
      IF SubInvoice.CustNum = Invoice.CustNum OR SubInvoice.CustNum = 0 THEN 
         ttSub.UserName = lcCustName.
      ELSE DO:
         FIND FIRST UserCustomer WHERE 
            UserCustomer.CustNum = SubInvoice.CustNum NO-LOCK NO-ERROR.
         IF AVAILABLE UserCustomer THEN 
            ttSub.UserName = Func.Common:mPrintCustName(BUFFER UserCustomer).
      END.

      RUN pttMSOwner(INPUT  SubInvoice.MSSeq,
                     INPUT  Customer.CustNum,
                     INPUT  ldFromPer,
                     INPUT  ldInvoiceFromPer,
                     INPUT  ldToPer,
                     OUTPUT llShouldCheckTermination,
                     OUTPUT llPostPreDetected).

      IF llShouldCheckTermination
      THEN DO:
         /* subscription has been terminated during billing period */
         FIND FIRST MsRequest WHERE
                    MsRequest.MsSeq     = SubInvoice.MsSeq AND
                    MsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TERMINATION} AND
                    MsRequest.ReqStat   = {&REQUEST_STATUS_DONE}              AND
                    MsRequest.ActStamp >= ldFromPer        AND
                    MsRequest.ActStamp <= ldToPer          NO-LOCK NO-ERROR.

         IF AVAILABLE MsRequest THEN DO:

            ldeActStamp = MsRequest.ActStamp.

            IF NOT ttInvoice.PostPoned AND
               NOT CAN-FIND(FIRST MsRequest WHERE
                              MsRequest.MsSeq     = SubInvoice.MsSeq    AND
                              MsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_REACTIVATION} AND
                              MsRequest.ReqStat   = {&REQUEST_STATUS_DONE}               AND
                              MsRequest.ActStamp  >  ldeActStamp        AND
                              MsRequest.ActStamp <= ldToPer) THEN
               FOR FIRST SingleFee WHERE
                         SingleFee.Brand     = Syst.Var:gcBrand                  AND
                         SingleFee.CustNum   = SubInvoice.CustNum       AND
                         SingleFee.HostTable = "MobSub"                 AND
                         SingleFee.KeyValue  = STRING(SubInvoice.MsSeq) AND
                         SingleFee.Billed    = FALSE NO-LOCK:
                  ASSIGN ttInvoice.PostPoned = YES.
               END.

            ttSub.MessageType   = "13".   
         END. 
         
         IF llPostPreDetected
         THEN ttSub.MessageType = "15".
      END.
    
      /* if specifications on cli level wanted -> check cli data */
      IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:

         ASSIGN ldPeriodFrom       = ldFromPer
                ldPeriodTo         = ldToPer
                liCount            = 0
                lcGroupCode        = ""
                lliSTR             = FALSE
                .

         /* Check immediate STC */
         FIND FIRST ttMSOwner WHERE
            ttMsOwner.Type   = 2                 AND
            ttMSOwner.InvCust = Customer.CustNum AND
            ttMsOwner.TsBeg  >= ldInvoiceFromPer AND
            ttMsOwner.TsBeg  <= ldToPer          AND
            ttMsOwner.CLIEvent BEGINS "iS" NO-ERROR.

         IF AVAIL ttMsOwner THEN DO:
            
            IF ttMSOwner.CLIEvent = "iS"
            THEN lliSTR = TRUE.
            
            Func.Common:mSplitTS(ttMsOwner.TsBeg,OUTPUT ldaOwnerDate,OUTPUT liOwnerTime).

            IF ttMsOwner.CLIEvent = "iSS" THEN DO:
               IF Invoice.DelType = {&INV_DEL_TYPE_FUSION_EMAIL} OR
                  Invoice.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}
               THEN DO:
                  IF ttMSOwner.FusionCLIType
                  THEN ldPeriodFrom = ttMsOwner.TsBeg.
                  ELSE ldPeriodTo   = Func.Common:mSecOffSet(ttMsOwner.TsBeg,-1).
               END.
               ELSE DO:
                  IF ttMSOwner.FusionCLIType
                  THEN ldPeriodTo   = Func.Common:mSecOffSet(ttMsOwner.TsBeg,-1).
                  ELSE ldPeriodFrom = ttMsOwner.TsBeg.
               END.
            END.
         END.

         /* get clis that are involved in this invoice */
         FOR EACH ttMSOwner WHERE
            ttMSOwner.Type  = 2                  AND
            ttMSOwner.InvCust = Customer.CustNum AND
            ttMsOwner.TsBeg  <= ldPeriodTo       AND
            ttMsOwner.TsEnd  >= ldPeriodFrom     AND
            ttMsOwner.PayType = FALSE:

            liCount = liCount + 1.

            CREATE ttCLIType.            
            ASSIGN
               ttCLIType.CLIType = ttMSOwner.CLIType
               ttCLIType.CTName  = fLocalItemName("CLIType",
                                                  ttCLIType.CLIType,
                                                  liLanguage,
                                                  Invoice.ToDate)
               ttCLIType.CLI     = SubInvoice.CLI
               ttCLIType.TsBeg   = ttMsOwner.TsBeg
               ttCLIType.TsEnd   = ttMsOwner.TsEnd.

            FIND FIRST CLIType WHERE 
                       CLIType.Brand   = Syst.Var:gcBrand AND
                       CLIType.CLIType = ttCLIType.CLIType NO-LOCK NO-ERROR.
            IF AVAILABLE CLIType THEN DO:
               ttCLIType.BaseBundle = CLIType.BaseBundle.

               /* Common Rate Plan for all FLAT tariffs CONTF */
               IF CLIType.CLIType BEGINS "CONTF" AND
                  NOT CLIType.CLIType BEGINS "CONTFH" THEN
                  ttCLIType.RateName = ttCLIType.CTName.
               ELSE
                  ttCLIType.RateName = fLocalItemName("RatePlan",
                                                      CLIType.PricePlan,
                                                      liLanguage,
                                                      Invoice.ToDate).
            END.

            lcBundleName = "".
            IF (ttCLIType.CLIType BEGINS "CONTF" AND
                NOT ttCLIType.CLIType BEGINS "CONTFH") OR 
               ttCLIType.BaseBundle = "CONT15" THEN DO:

               lcGroupCode = ttCLIType.CLIType.
               IF ttCLIType.CLIType = "CONTFF" THEN
                  lcGroupCode = "CONTFF2".
               ELSE IF ttCLIType.CLIType = "CONT15" THEN
                  lcGroupCode = "VOICE100".
               ELSE lcGroupCode = "VOICE100,VOICE200". /* BaseBundle = "CONT15" = Verde */

               RELEASE ttRow.

               IF lcGroupCode BEGINS "CONTF" THEN
                  FOR FIRST DayCampaign NO-LOCK WHERE 
                            DayCampaign.Brand = Syst.Var:gcBrand AND 
                            DayCampaign.DCEvent = lcGroupCode,
                      FIRST FMItem NO-LOCK WHERE
                            FMItem.Brand = Syst.Var:gcBrand AND
                            FMItem.FeeModel = DayCampaign.FeeModel:
                     FIND FIRST ttRow WHERE  
                                ttRow.SubInvNum = SubInvoice.SubInvNum AND
                                ttRow.RowBillCode = FMItem.BillCode NO-ERROR.   
                  END.

               DO liLoop = 1 TO NUM-ENTRIES(lcGroupCode):
                  FOR EACH bServiceLimit NO-LOCK WHERE
                           bServiceLimit.GroupCode = ENTRY(liLoop, lcGroupCode),
                      EACH bMServiceLimit NO-LOCK WHERE
                           bMServiceLimit.MsSeq = SubInvoice.MsSeq  AND
                           bMServiceLimit.DialType = bServiceLimit.DialType AND
                           bMServiceLimit.SlSeq   = bServiceLimit.SlSeq AND
                           bMServiceLimit.FromTS <= ldPeriodTo          AND
                           bMServiceLimit.EndTS  >= ldPeriodFrom:
                                                   
                     IF AVAIL ttRow THEN DO:
                        IF bMServiceLimit.DialType = {&DIAL_TYPE_VOICE} THEN
                           ttRow.VoiceLimit = bMServiceLimit.InclAmt.
                        ELSE IF bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} THEN
                           ttRow.DataLimit = bMServiceLimit.InclAmt.
                     END.

                     IF LOOKUP(bServiceLimit.GroupCode,"VOICE100,VOICE200") > 0 THEN
                        lcBundleName = REPLACE(bServiceLimit.GroupCode,"OICE",""). /* V100 or V200 */
                  END.
               END.
            END.
            IF lcBundleName NE "" THEN ttCLIType.CLIType = ttCLIType.CLIType + lcBundleName.

            /* latest type is stored -> same type as was used in
               billing run for min. consumption */
            IF liCount = 1 THEN
               ASSIGN ttSub.CLIType  = ttCLIType.CLIType
                      ttSub.CTName   = ttCLIType.CTName
                      ttSub.CliEvent = ttMSOwner.CLIEvent.

            /* Mobile provisioned during month. Used for convergent */
            IF ttMSOwner.CliEvent NE "F" THEN ttSub.PrintCLI = TRUE.

            /* Immediate STC logic non-fusion to non-fusion */
            IF lliSTR                AND
               ttSub.OldCLIType = "" AND
               ttSub.CLIType <> ttCLIType.CLIType
            THEN DO:
               ASSIGN ttSub.OldCLIType = ttCLIType.CLIType
                      ttSub.OldCTName  = ttCLIType.CTName.
               IF ldaOwnerDate = ? THEN
                  ttSub.TariffActDate = "".
               ELSE
                  ttSub.TariffActDate = STRING(ldaOwnerDate).
            END.

         END.

         /* If there is no MsOwner found for billing period
            then use latest MsOwner */
         IF liCount = 0 THEN
            FOR FIRST ttMSOwner WHERE ttMSOwner.Type = 1:
               ASSIGN
                  ttSub.CLIType  = ttMsOwner.CLIType
                  ttSub.CliEvent = ttMSOwner.CLIEvent
                  ttSub.CTName   = fLocalItemName("CLIType",
                                                   ttSub.CLIType,
                                                   liLanguage,
                                                   Invoice.ToDate).
            END.
      END.

      /* All data is displayed in MB, 
         unit for data in cdrs is bytes */
      ASSIGN
         ttSub.DataConv = 1024 * 1024
         llPTFinancedByBank = FALSE
         llRVFinancedByBank = FALSE.

      /* is call itemization printed */
      ttSub.CallSpec = fCallSpecDuring(SubInvoice.MsSeq,Invoice.InvDate).

      /*Google billing*/
      FOR EACH ttRow WHERE
               ttRow.SubInvNum = SubInvoice.SubInvNum AND
               ttRow.RowCode BEGINS "44" NO-LOCK:
         ttSub.GBValue = ttSub.GBValue + ttRow.RowAmt.
      END.
      FOR EACH ttRow WHERE
               ttRow.SubInvNum = SubInvoice.SubInvNum AND
               ttRow.RowCode BEGINS "45" NO-LOCK:
         ttSub.GBValue = ttSub.GBValue + ttRow.RowAmt.
      END.

      /* ttRows contains combined invrows => no need to check duplicates */
      FOR EACH ttRow WHERE
               ttRow.SubInvNum = SubInvoice.SubInvNum AND
               ttRow.RowCode BEGINS "33" AND
               ttRow.RowType = "":

         IF (ttRow.RowBillCode EQ "TERMPERIOD" OR
             ttRow.RowBillCode EQ "FTERMPERIOD") AND
             ttRow.RowVatAmt EQ 0 THEN
             ttSub.PenaltyAmt = ttSub.PenaltyAmt + ttRow.RowAmt.
                  
         IF NOT (ttRow.RowBillCode BEGINS "PAYTERM" OR
                 ttRow.RowBillCode BEGINS "RVTERM") THEN NEXT.
            
         ASSIGN
            ttSub.InstallmentAmt = ttSub.InstallmentAmt + ttRow.RowAmt.

         IF (LOOKUP(ttRow.RowBillCode,{&TF_BANK_RVTERM_BILLCODES})           > 0) OR
            (LOOKUP(ttRow.RowBillCode,{&TF_BANK_UNOE_PAYTERM_BILLCODES})     > 0) OR 
            (LOOKUP(ttRow.RowBillCode,{&TF_BANK_SABADELL_PAYTERM_BILLCODES}) > 0) OR
            (LOOKUP(ttRow.RowBillCode,{&TF_BANK_CETELEM_PAYTERM_BILLCODES}) > 0) OR
            /* included due to Q25 picture check */
            ttRow.RowBillCode EQ "PAYTERM" THEN 
            llPTFinancedByBank = TRUE.
            
         ELSE IF 
            LOOKUP(ttRow.RowBillCode,{&TF_BANK_UNOE_RVTERM_BILLCODES}) > 0 OR
            LOOKUP(ttRow.RowBillCode,{&TF_BANK_SABADELL_RVTERM_BILLCODES}) > 0 OR
            LOOKUP(ttRow.RowBillCode,{&TF_BANK_CETELEM_RVTERM_BILLCODES}) > 0
         THEN llRVFinancedByBank = TRUE.
      END.

      IF ttSub.InstallmentAmt > 0 THEN
         FOR EACH ttRow NO-LOCK WHERE 
                  ttRow.SubInvNum = SubInvoice.SubInvNum AND 
                  ttRow.RowCode BEGINS "13" AND
                  ttRow.RowGroup  = "13" AND
                  LOOKUP(ttRow.RowBillCode,{&INSTALLMENT_DISCOUNT_BILLCODES}) > 0:
            ttSub.InstallmentDiscAmt = ttSub.InstallmentDiscAmt + ttRow.RowAmt.
         END.              

      IF llPTFinancedByBank THEN 
         fTFBankFooterText("PAYTERM"). 
      IF llRVFinancedByBank THEN
         fTFBankFooterText("RVTERM").
   END.
   
   RETURN "".
   
END PROCEDURE.


PROCEDURE pGetInvoiceRowData:

   DEF VAR liRowOrder   AS INT  NO-UNDO.
   DEF VAR ldVatTot     AS DEC  NO-UNDO.
   DEF VAR ldVatAmt     AS DEC  NO-UNDO.
   DEF VAR liVatCnt     AS INT  NO-UNDO.
   DEF VAR ldAmtExclVat AS DEC  NO-UNDO.
   DEF VAR lcRowCode    AS CHAR NO-UNDO. 
   DEF VAR lcRowName    AS CHAR NO-UNDO. 
   DEF VAR ldeQ25DiscAmt  AS DEC  NO-UNDO.
   DEFINE VARIABLE llTemp AS LOGICAL NO-UNDO.

   EMPTY TEMP-TABLE ttRow.
   
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      EMPTY TEMP-TABLE ttExcludedRow.
   
      ASSIGN
         ldVatTot   = 0
         liRowOrder = 0.

      DO liVatCnt = 1 TO 5:
         ldVatTot = ldVatTot + SubInvoice.VatAmount[liVatCnt].
      END.
         
      /* if Q25 extension is done (= RVTERMDTEQ25 is billed),
         the originnl q25 fee and related disounts should not be printed.
         YPR-3532 */
      IF CAN-FIND(FIRST InvRow NO-LOCK WHERE
                        InvRow.Invnum = SubInvoice.InvNum AND
                        InvRow.SubInvNum = SubInvoice.SubInvNum AND
                        InvRow.BillCode EQ "RVTERMDTEQ25" AND
                        InvRow.VatPerc = 0) THEN DO:

         ldeQ25DiscAmt = 0.

         FOR EACH InvRow NO-LOCK WHERE
                  InvRow.InvNum = SubInvoice.InvNum AND
                  InvRow.SubInvNum = SubInvoice.SubInvNum AND
                  InvRow.VatPerc = 0 AND
                  LOOKUP(InvRow.BillCode, 
                         {&Q25_RVTERM_RENEWAL_DISCOUNTS}) > 0:
            
            
            ASSIGN 
               ldeQ25DiscAmt = ldeQ25DiscAmt + InvRow.Amt
               llTemp        = fAddToExcludedRow(ROWID(InvRow)).
         END.

         FOR FIRST InvRow NO-LOCK WHERE
                  InvRow.Invnum = SubInvoice.InvNum AND
                  InvRow.SubInvNum = SubInvoice.SubInvNum AND
                  InvRow.VatPerc = 0 AND
                  InvRow.Amt EQ (ldeQ25DiscAmt * -1) AND
                  LOOKUP(InvRow.BillCode, {&TF_RVTERM_BILLCODES}) > 0:
            fAddToExcludedRow(ROWID(InvRow)).
         END.         
         IF NOT AVAIL InvRow
         THEN EMPTY TEMP-TABLE ttExcludedRow.
      END.

      /* handle rows in percentage order so that total tax amounts
         can be matched to row taxes */
      FOR EACH InvRow OF Invoice NO-LOCK WHERE
               InvRow.SubInvNum = SubInvoice.SubInvNum AND
               NOT (InvRow.RowType = 5 AND InvRow.Amt = 0),
         FIRST ttBillItemAndGroup NO-LOCK WHERE
               ttBillItemAndGroup.BillCode = InvRow.BillCode
         BREAK BY InvRow.VatPerc
            BY ABS(InvRow.Amt):

         /* Term Penalty amount counted in the same for each loop */
         IF InvRow.RowType = 4 AND
            InvRow.Amt NE 0 AND
           (InvRow.BillCode = "TERMPERIOD" OR
            InvRow.BillCode = "FTERMPERIOD") AND
            InvRow.VatPerc = 0 THEN
            ttInvoice.PenaltyAmt = ttInvoice.PenaltyAmt + InvRow.Amt.

         ASSIGN ldVatAmt = 0.

         CASE ttBillItemAndGroup.BIGroup:
            WHEN "44" /* Google purchase */
            THEN ttInvoice.GBValue = ttInvoice.GBValue + InvRow.Amt.
            WHEN "45" /* Google refund */
            THEN ttInvoice.GBDiscValue = ttInvoice.GBDiscValue + InvRow.Amt.
            WHEN "33"
            THEN DO:
               IF CAN-FIND(FIRST ttExcludedRow WHERE ttExcludedRow.rowid = ROWID(InvRow))
               THEN NEXT.
   
               IF InvRow.BillCode BEGINS "PAYTERM" OR
                  InvRow.BillCode BEGINS "RVTERM"
               THEN ttInvoice.InstallmentAmt = ttInvoice.InstallmentAmt + InvRow.Amt.               
            END.
         END CASE.

         IF InvRow.RowType EQ 9 AND
            LOOKUP(InvRow.BillCode, {&INSTALLMENT_DISCOUNT_BILLCODES}) > 0 THEN
            ttInvoice.InstallmentDiscAmt = ttInvoice.InstallmentDiscAmt + 
                                           InvRow.Amt.

         /* separate tax from row amount */
         IF InvRow.VATPerc > 0 AND InvRow.Amt NE 0 THEN DO:
                 
            IF LAST(ABS(InvRow.Amt)) THEN ldVATAmt = ldVATTot.
            ELSE DO:
               IF Invoice.VatIncl = TRUE THEN
                  ldVATAmt = ROUND(InvRow.Amt * InvRow.VATPerc /
                                  (100 + InvRow.VATPerc),2).
               ELSE ldVatAmt = ROUND(InvRow.Amt * InvRow.VatPerc / 100,2).
            END.
         
            ldVATTot = ldVATTot - ldVATAmt.
         END.
                        
         IF Invoice.VatIncl THEN ASSIGN
             ldAmtExclVat = InvRow.Amt - ldVatAmt
             ldAmt        = InvRow.Amt.
         ELSE ASSIGN
             ldAmtExclVat = InvRow.Amt
             ldAmt        = ldAmtExclVat + ldVatAmt.

         lcRowName = RIGHT-TRIM(fBillItemName(InvRow.BillCode,
                                              IF InvRow.ToDate NE ?
                                              THEN InvRow.ToDate
                                              ELSE Invoice.ToDate,
                                              liLanguage)).

         lcRowCode = STRING(ttBillItemAndGroup.BiGroup) + lcRowName.

         FIND FIRST ttRow WHERE
                    ttRow.SubInvNum = InvRow.SubInvNum AND
                    ttRow.RowCode   = lcRowCode NO-ERROR.
            
         IF NOT AVAILABLE ttRow THEN DO:
      
            CREATE ttRow.
            ASSIGN 
               ttRow.SubInvNum  = InvRow.SubInvNum
               ttRow.RowGroup   = ttBillItemAndGroup.BIGroup
               ttRow.RowCode    = lcRowCode
               liRowOrder       = liRowOrder + 2
               ttRow.RowOrder   = liRowOrder
               ttRow.GroupOrder = ttBillItemAndGroup.GroupOrder.
         END.

         ASSIGN 
            ttRow.RowType       = ""
            ttRow.RowAmtExclVat = ttRow.RowAmtExclVat + ldAmtExclVat
            ttRow.RowVatAmt     = ttRow.RowVatAmt + ldVatAmt
            ttRow.RowAmt        = ttRow.RowAmt + ldAmt
            ttRow.RowData       = ttRow.RowData + InvRow.DataAmt
            ttRow.RowQty        = ttRow.RowQty + InvRow.Qty
            ttRow.RowDur        = ttRow.RowDur + InvRow.Min
            ttRow.RowBillCode   = ttBillItemAndGroup.BillCode
            ttRow.RowName       = lcRowName
            ttRow.RowToDate     = InvRow.ToDate.
      END.

      /* subtotals are wanted as headers, so calculate them here and make
         rows out of them */
      FOR EACH ttRow WHERE ttRow.SubInvNum = SubInvoice.SubInvNum
      BREAK BY ttRow.RowGroup
            BY ttRow.RowOrder:
      
         IF FIRST-OF(ttRow.RowGroup) THEN liRowOrder = ttRow.RowOrder.

         ACCUMULATE 
            ttRow.RowQty    (TOTAL BY ttRow.RowGroup)
            ttRow.RowDur    (TOTAL BY ttRow.RowGroup)
            ttRow.RowData   (TOTAL BY ttRow.RowGroup)
            ttRow.RowAmtExclVat (TOTAL BY ttRow.RowGroup)
            ttRow.RowVatAmt (TOTAL BY ttRow.RowGroup)
            ttRow.RowAmt    (TOTAL BY ttRow.RowGroup).
            
         IF LAST-OF(ttRow.RowGroup) THEN DO:
            CREATE bttRow.
            ASSIGN 
               bttRow.SubInvNum = ttRow.SubInvNum
               bttRow.RowGroup  = ttRow.RowGroup
               bttRow.GroupOrder = ttRow.GroupOrder
               bttRow.RowType   = "SubTotal"
               /* position as first of the group */
               bttRow.RowOrder  = liRowOrder - 1
               bttRow.RowName   = fLocalItemName("BItemGroup",
                                                 ttRow.RowGroup,
                                                 liLanguage,
                                                 Invoice.ToDate)
               bttRow.RowQty    = (ACCUM TOTAL BY ttRow.RowGroup ttRow.RowQty)
               bttRow.RowDur    = (ACCUM TOTAL BY ttRow.RowGroup ttRow.RowDur)
               bttRow.RowData   = (ACCUM TOTAL BY ttRow.RowGroup ttRow.RowData)
               bttRow.RowAmtExclVat = 
                  (ACCUM TOTAL BY ttRow.RowGroup ttRow.RowAmtExclVat)
               bttRow.RowVatAmt = 
                  (ACCUM TOTAL BY ttRow.RowGroup ttRow.RowVatAmt)
               bttRow.RowAmt    = (ACCUM TOTAL BY ttRow.RowGroup ttRow.RowAmt).
         END.
      END.

   END.

   RETURN "".
   
END PROCEDURE.

/* note: should be called only after pGetInvoiceRowData */
PROCEDURE pGetInvoiceVatData:

   EMPTY TEMP-TABLE ttVat.

   DEF VAR ldeExclSum AS DEC NO-UNDO.

   /* vat amount */
   DO liPCnt = 1 TO 10:
      IF Invoice.VatBasis[liPCnt] NE 0 OR Invoice.VatPerc[liPCnt] NE 0 
      THEN DO:
         FIND FIRST ttVat WHERE ttVat.VatPerc = Invoice.VatPerc[liPCnt]
            NO-ERROR.
         IF NOT AVAILABLE ttVat THEN DO:
            CREATE ttVat.
            ttVat.VatPerc = Invoice.VatPerc[liPCnt].
         END.
         ASSIGN
            ttVat.VatAmt   = ttVat.VatAmt + Invoice.VatAmount[liPCnt]
            ttVat.VatBasis = ttVat.VatBasis + Invoice.VatBasis[liPCnt].
      END.
   END.
   
   /* exclude installment amount from 0% VAT row YDR-185 */
   /* exclude Google Billing amount from 0% VAT row YPR-3890*/
   ldeExclSum = ttInvoice.InstallmentAmt +
                ttInvoice.PenaltyAmt +
                ttInvoice.InstallmentDiscAmt +
                ttInvoice.GBValue +
                ttInvoice.GBDiscValue.

   IF ldeExclSum NE 0 THEN
      FOR FIRST ttVat WHERE ttVat.VatPerc = 0:
         IF ttVat.VatBasis EQ ldeExclSum THEN DELETE ttVat.
         ELSE ttVat.VATBasis = ttVAT.VATBasis - ldeExclSum.
      END.

   /* taxzone */
   lcTaxZone = fLocalItemName("TaxZone",
                              Invoice.TaxZone,
                              liLanguage,
                              Invoice.ToDate).

END PROCEDURE.

PROCEDURE pMarkPrinted:

   DEF INPUT PARAMETER icSendMethod AS CHAR NO-UNDO.
   DEF INPUT PARAMETER icPrintHouse AS CHAR NO-UNDO.
   
   DEF VAR liSendMethod AS INT NO-UNDO.
   
   CASE icSendMethod:
   WHEN "XML" THEN liSendMethod = 5.
   OTHERWISE liSendMethod = 2.
   END CASE. 
   
   /* mark invoices AS printed */
   FOR EACH ttInvoice WHERE
            ttInvoice.Printed = 1,
   FIRST Invoice EXCLUSIVE-LOCK WHERE
         Invoice.InvNum = ttInvoice.InvNum:
       
      Invoice.PrintState = 1.

      /* log from print */
      /* dont log TEST invoices of type 99 */
      IF Invoice.InvType NE 99 THEN DO:

         CREATE ITSendLog.
         ASSIGN ITSendLog.Brand      = Syst.Var:gcBrand 
                ITSendLog.TxtType    = 3
                ITSendLog.ITNum      = 0
                ITSendLog.CustNum    = Invoice.CustNum
                ITSendLog.InvNum     = Invoice.InvNum
                ITSendLog.SendMethod = liSendMethod
                ITSendLog.EMail      = ""
                ITSendLog.RepType    = "Inv"
                ITSendLog.SendInfo   = icPrintHouse
                ITSendLog.UserCode   = Syst.Var:katun
                ITSendLog.SendStamp  = Func.Common:mMakeTS().
      END.
 
      RELEASE Invoice.    
   END. 

END PROCEDURE.

PROCEDURE pErrorFile:

   DEF INPUT PARAMETER ilDBWrite  AS LOG  NO-UNDO.
   DEF INPUT PARAMETER icActionID AS CHAR NO-UNDO.

   DEFINE VARIABLE ldCurrStamp AS DECIMAL NO-UNDO.

   lcErrFile = fCParamC("Doc1ErrorFile").
   IF lcErrFile = "" OR lcErrFile = ? THEN lcErrFile = "/tmp/doc1err".
    
   lcErrFile = lcErrFile + "_" + 
                           STRING(YEAR(TODAY),"9999") +
                           STRING(MONTH(TODAY),"99")  +
                           STRING(DAY(TODAY),"99")    + 
                           "_" + STRING(TIME) + ".txt".                       

   OUTPUT STREAM slog TO VALUE(lcErrFile).
   PUT STREAM slog UNFORMATTED
       "Invoice"   CHR(9)
       "Customer"  CHR(9)
       "Error"     lcNewLine.

   ldCurrStamp = Func.Common:mMakeTS().
    
   FOR EACH ttError TRANS:
      PUT STREAM slog UNFORMATTED
           ttError.Inv    CHR(9)
           ttError.Cust   CHR(9)
           ttError.ErrMsg lcNewLine.
           
      /* save to db for reporting */
      IF ilDBWrite THEN DO:
         CREATE ErrorLog.
         ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
                ErrorLog.ActionID  = icActionID
                ErrorLog.TableName = IF ttError.TableName > ""
                                     THEN ttError.TableName
                                     ELSE "Invoice"
                ErrorLog.KeyValue  = IF ttError.KeyValue > ""
                                     THEN ttError.KeyValue
                                     ELSE ttError.Inv
                ErrorLog.ActionTS  = ldCurrStamp
                ErrorLog.UserCode  = Syst.Var:katun
                ErrorLog.ErrorMsg  = ttError.ErrMsg.
      END.          
   END.

   OUTPUT STREAM slog CLOSE. 

   /* send the report AS email */
   ASSIGN xMailAttach = lcErrFile
          lcErrFile    = "/tmp/invdoc1_errmsg.txt".
   OUTPUT STREAM slog TO VALUE(lcErrFile).
   PUT STREAM slog UNFORMATTED
       "Errors from creating an invoice DOC1-file " + 
       STRING(TODAY,"99.99.9999") + "." + lcNewLine + lcNewLine +
       "Open the attachment file in Excel." + lcNewLine + lcNewLine + "  ".
   OUTPUT STREAM slog CLOSE.

   lcConfDir = fCParamC("RepConfDir").
    
   /* mail recipients AND actual sending */
   GetRecipients(lcConfDir + "invdoc1_error.email").
   SendMail(lcErrFile,xMailAttach).

END PROCEDURE.

PROCEDURE pCollectCDR:

   DEFINE INPUT PARAMETER iiInvSeq                    AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER iolPremiumNumberText AS LOGICAL NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER iolGBText            AS LOGICAL NO-UNDO.

   DEFINE BUFFER bCallInvSeq FOR InvSeq.
   
   DEFINE VARIABLE lcQuery        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liFound        AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lii            AS INTEGER   NO-UNDO.
   DEFINE VARIABLE llOK           AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lhQuery        AS HANDLE    NO-UNDO.
   DEFINE VARIABLE lcBillCode     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBIGroup      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcBillItemName AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liGroupOrder   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liGroupType    AS INTEGER   NO-UNDO.

   EMPTY TEMP-TABLE ttCall.
  
   FIND FIRST bCallInvSeq WHERE bCallInvSeq.InvSeq = iiInvSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bCallInvSeq THEN RETURN. 
   
   objDBConn:mSetQueryHandlesConnectIfNeeded(bCallInvSeq.FromDate,
                                             bCallInvSeq.ToDate).

   lcQuery = SUBSTITUTE(gcCallQueryEnd,
                        STRING(bCallInvSeq.CustNum),
                        STRING(bCallInvSeq.InvSeq),
                        STRING(bCallInvSeq.FromDate),
                        STRING(bCallInvSeq.ToDate)).
   
   DO lii = 1 TO objDBConn:liCurrentQueryHandleCount:

      lhQuery = objDBConn:lhCurrentQueryHandle[lii].
      
      llOK = lhQuery:QUERY-PREPARE(SUBSTITUTE(gcCallQueryBegin, lhQuery:PRIVATE-DATA) + lcQuery).
      
      IF NOT llOK
      THEN NEXT.

      lhQuery:QUERY-OPEN().
      
      DO WHILE TRUE:

         llOK = lhQuery:GET-NEXT(NO-LOCK).

         /* Query handle is invalid, no more records, or query is not open */
         IF llOK = ? OR NOT llOK
         THEN LEAVE.

         /* Billcode changes */
         IF lhQuery:FIRST-OF(1)
         THEN DO:
            lcBillCode = lhQuery:GET-BUFFER-HANDLE(1)::BillCode.
            
            FIND ttBillItemAndGroup WHERE ttBillItemAndGroup.BillCode = lcBillCode NO-ERROR.
            IF AVAILABLE ttBillItemAndGroup
            THEN DO:
               ASSIGN
                  liGroupOrder = ttBillItemAndGroup.GroupOrder
                  lcBIGroup    = ttBillItemAndGroup.BIGroup
                  liGroupType  = ttBillItemAndGroup.GroupType
                  .
               IF iolPremiumNumberText = FALSE AND
                  ttBillItemAndGroup.PremiumBillCode
               THEN iolPremiumNumberText = TRUE.

               IF iolGBText = FALSE AND
                  ttBillItemAndGroup.GBBillCode
               THEN iolGBText = TRUE.
            END.
            ELSE ASSIGN
                    liGroupOrder = 0
                    lcBiGroup    = ""
                    liGroupType  = 0
                    .
         END.

         /* DateSt changes */
         IF lhQuery:FIRST-OF(2)
         THEN lcBillItemName = fBillItemName(lcBillCode,
                                             lhQuery:GET-BUFFER-HANDLE(1)::DateSt,
                                             liLanguage).

         CREATE ttCall.
         
         ghttCall:BUFFER-COPY(lhQuery:GET-BUFFER-HANDLE(1)) NO-ERROR.
         
         IF ERROR-STATUS:ERROR
         THEN DELETE ttCall.
         ELSE DO:
            ASSIGN
                 ttCall.GroupOrder   = liGroupOrder
                 ttCall.BIGroup      = lcBIGroup
                 ttCall.GroupType    = liGroupType
                 ttCall.BillItemName = lcBillItemName
                 ttCall.CDRTable     = "MobCDR"
                 liFound             = liFound + 1
                 .
               
            IF liFound >= 99999999
            THEN LEAVE.               
         END.

      END.

      lhQuery:QUERY-CLOSE().

   END.

END PROCEDURE.
