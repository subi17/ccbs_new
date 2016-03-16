&GLOBAL-DEFINE AllIncludes YES

{commali.i}
{cparam2.i}
{timestamp.i}
{printdoc1tt.i}
{refcode.i}
{transname.i}
{ftransdir.i}
{email.i}
{fsubser.i}
{nnpura.i}
{customer_address.i}
{fdestcountry.i}
{tmsconst.i}
{callquery.i}
{ftaxdata.i}
{fbundle.i}
{q25functions.i}

&GLOBAL-DEFINE INSTALLMENT_DISCOUNT_BILLCODES "DISCPAYTERMDIR,DISCPAYTERMINDIR"

DEF STREAM sLog.

DEF VAR lcRefAmt    AS CHAR  NO-UNDO. 
DEF VAR lcErrTxt    AS CHAR  NO-UNDO. 
DEF VAR lcLine      AS CHAR  NO-UNDO. 
DEF VAR lcLastError AS CHAR  NO-UNDO.
DEF VAR lcTransDir  AS CHAR  NO-UNDO.
DEF VAR lcFileExt   AS CHAR  NO-UNDO.
DEF VAR liPCnt      AS INT   NO-UNDO.
DEF VAR lcMessage   AS CHAR  NO-UNDO. 
DEF VAR llgPostPay  AS LOG   NO-UNDO.
DEF VAR lcErrFile   AS CHAR  NO-UNDO. 
DEF VAR lcConfDir   AS CHAR  NO-UNDO. 
DEF VAR ldAmt       AS DEC   NO-UNDO.
DEF VAR ldMinRow    AS DEC   NO-UNDO.
DEF VAR ldMaxRow    AS DEC   NO-UNDO.
DEF VAR lcNonCombinedData AS CHAR  NO-UNDO.
DEF VAR ldFromPer   AS DEC   NO-UNDO.
DEF VAR ldInvoiceFromPer AS DEC   NO-UNDO.
DEF VAR ldToPer     AS DEC   NO-UNDO.
DEF VAR liPeriod    AS INT   NO-UNDO.
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
DEF VAR tthCDR         AS HANDLE NO-UNDO.
DEF VAR lcBundleCLITypes  AS CHAR NO-UNDO.
DEF VAR ldtInvToDate      AS DATE NO-UNDO.

DEF TEMP-TABLE ttCall NO-UNDO LIKE MobCDR
   FIELD CDRTable      AS CHAR
   FIELD GroupOrder    AS INT 
   FIELD BIGroup LIKE BillItem.BIGroup
   INDEX BIGroup GroupOrder BIGroup.

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
   FIELD GraphName  AS CHAR
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
   FIELD CLIType      AS CHAR
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
   FIELD Q25Phase        AS INT
   INDEX CLI CLI.
   
DEF TEMP-TABLE ttCLIType NO-UNDO
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

DEF BUFFER bInv FOR Invoice.
DEF BUFFER bttRow FOR ttRow.

tthCDR = TEMP-TABLE ttCall:HANDLE.

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

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

FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
        
   RETURN TRIM(STRING(idAmt,"->>>,>>>,>>9.99")).
   
END FUNCTION.

FUNCTION fDispDecimal3 RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>,>>>,>>9.999")).
   
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
   lcName = fGetItemName(gcBrand,
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
      lcCCName = fDestCountryName(gcBrand,
                                  liLanguage,
                                  ttCall.SpoCMT,
                                  ttCall.DateSt,
                                  ttCall.DtlSeq,
                                  ttCall.GsmBnr,
                                  ttCall.BType,
                                  ttCall.MSCID).
      /* undefined value, stop handling */
      IF lcCCName = "#UNDEFINED" THEN DO:
         fErrLine("Undefined name for country").
         QUIT.   /* retry of PrintMainLoop catches this */
      END.
   END.
   /* ServiceName for Premium Number */
   ELSE IF ttCall.BIGroup = "6" THEN
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
   DEF VAR lcTFPayTermEndBillCode AS CHAR NO-UNDO.
   DEF VAR liFFCount              AS INT  NO-UNDO. 
   DEF VAR lcPaytermEndCodes      AS CHAR NO-UNDO. 
   DEF VAR llPenaltyFound         AS LOG  NO-UNDO. 
   DEF VAR liAmtPos               AS INT  NO-UNDO.
   DEF VAR liFtrPos               AS INT  NO-UNDO.
   DEF VAR llFooter               AS LOG  NO-UNDO.
   DEF VAR ldTAE                  AS DEC  NO-UNDO.
   
   /* PAYTERM values to pos. 1-3, footer 1
      RVTERM  values to pos. 4-6, footer 2
   */
   IF icBillCode BEGINS "PAYTERM" THEN ASSIGN
      liAmtPos = 1
      liFtrPos = 1.
   ELSE ASSIGN
      liAmtPos = 4
      liFtrPos = 2. 

   liFFCount = liAmtPos. 
   
   FOR EACH FixedFee NO-LOCK WHERE
            FixedFee.Brand      = gcBrand                    AND
            FixedFee.CustNum    = Invoice.CustNum            AND
            FixedFee.HostTable  = "MobSub"                   AND
            FixedFee.KeyValue   = STRING(SubInvoice.MsSeq)   AND
            FixedFee.BillCode BEGINS icBillCode              AND
     LOOKUP(FixedFee.FinancedResult,{&TF_STATUSES_BANK}) > 0 AND
            FixedFee.TFBank    <> ""                         AND
            FixedFee.BegDate   <= Invoice.Todate             AND
            FixedFee.EndPeriod >= liPeriod BY FixedFee.BegDate:
      
      IF liFFCount > liAmtPos + 2 THEN LEAVE.
            
      IF FixedFee.TFBank = {&TF_BANK_UNOE} THEN
         ASSIGN lcTFRVTermBillCode    = "RVTERM1EF" 
                                        WHEN icBillCode BEGINS "PAYTERM"
                liFooterConf1         = 536
                liFooterConf2         = 557
                lcTFPayTermEndBillCode = icBillCode + "END1E".
      ELSE IF FixedFee.TFBank = {&TF_BANK_SABADELL} THEN
         ASSIGN lcTFRVTermBillCode    = "RVTERMBSF"
                                        WHEN icBillCode BEGINS "PAYTERM"
                liFooterConf1         = 558
                liFooterConf2         = 559
                lcTFPayTermEndBillCode = icBillCode + "ENDBS".
      ELSE NEXT.
      
      FOR EACH FFItem NO-LOCK WHERE
               FFItem.FFNum = FixedFee.FFNum AND
               FFItem.BillCode BEGINS icBillCode:
            
         IF NOT FFItem.Billed THEN ASSIGN
            ttSub.TFBankAfterAmt[liFFCount]  = ttSub.TFBankAfterAmt[liFFCount] +
                                               FFItem.Amt.
         ELSE IF FFItem.InvNum = Invoice.InvNum AND FFItem.Billed THEN ASSIGN
            ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                               FFItem.Amt.
      END.
            
      ASSIGN ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                                ttSub.TFBankAfterAmt[liFFCount]
             ldeRVPerc                        = 0
             llPenaltyFound                   = FALSE.
   
      /* Find the correct TAE value */
      IF lcTFRVTermBillCode > "" THEN
      FOR FIRST SingleFee NO-LOCK WHERE
                SingleFee.Brand       = gcBrand              AND
                SingleFee.Custnum     = FixedFee.Custnum     AND
                SingleFee.HostTable   = FixedFee.HostTable   AND
                SingleFee.KeyValue    = Fixedfee.KeyValue    AND
                SingleFee.SourceKey   = FixedFee.SourceKey   AND
                SingleFee.SourceTable = FixedFee.SourceTable AND
                SingleFee.BillCode    = lcTFRVTermBillCode:

            FOR FIRST DayCampaign NO-LOCK WHERE
                      DayCampaign.Brand = gcBrand AND
                      DayCampaign.DCEvent = FixedFee.CalcObj,
               FIRST FMItem NO-LOCK WHERE
                     FMItem.Brand     = gcBrand              AND
                     FMItem.FeeModel  = DayCampaign.FeeModel AND
                     FMItem.ToDate   >= FixedFee.BegDate    AND
                     FMItem.FromDate <= FixedFee.BegDate:
               ldeTotalAmount = ROUND(fmitem.FFItemQty * fmitem.Amount,2).
               ldeRVPerc      = TRUNC(SingleFee.Amt /
                                (ldeTotalAmount + SingleFee.Amt) * 100 + 0.05,1).
            END.
         
            IF NOT SingleFee.Billed THEN ASSIGN
               ttSub.TFBankAfterAmt[liFFCount]  = ttSub.TFBankAfterAmt[liFFCount] +
                                                  SingleFee.Amt
               ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                                  SingleFee.Amt.
           
            ELSE IF SingleFee.InvNum = Invoice.InvNum AND SingleFee.Billed THEN ASSIGN
               ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                                  SingleFee.Amt.
     
      END. 
        
      FOR FIRST SingleFee NO-LOCK WHERE
                SingleFee.Brand       = gcBrand              AND
                SingleFee.Custnum     = FixedFee.Custnum     AND
                SingleFee.HostTable   = FixedFee.HostTable   AND
                SingleFee.KeyValue    = Fixedfee.KeyValue    AND
                SingleFee.SourceTable = "FixedFee" AND
                SingleFee.SourceKey   = STRING(FixedFee.FFNUM) AND
                SingleFee.BillCode    BEGINS icBillCode + "END":
         
            IF NOT SingleFee.Billed THEN ASSIGN
               ttSub.TFBankAfterAmt[liFFCount]  = ttSub.TFBankAfterAmt[liFFCount] +
                                                  SingleFee.Amt
               ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                                  SingleFee.Amt.
           
            ELSE IF SingleFee.InvNum = Invoice.InvNum AND SingleFee.Billed THEN ASSIGN
               ttSub.TFBankBeforeAmt[liFFCount] = ttSub.TFBankBeforeAmt[liFFCount] +
                                                  SingleFee.Amt
               llPenaltyFound = TRUE.
      END. 

      IF ttSub.TFBankBeforeAmt[liFFCount] EQ 0 THEN NEXT.
              
      IF FixedFee.OrderId > 0 THEN DO:
         FIND FIRST Order NO-LOCK WHERE
                    Order.Brand = gcBrand AND
                    Order.OrderId = FixedFee.OrderID NO-ERROR.
         IF AVAIL Order THEN fTS2Date(Order.CrStamp, 
                                      OUTPUT ldaOrderDate).
         ELSE ldaOrderDate = FixedFee.BegDate.
      END.
      ELSE ldaOrderDate = FixedFee.BegDate.
    
      ASSIGN 
         llFooter = FALSE
         ldTAE    = 0.
         
      IF icBillCode BEGINS "PAYTERM" THEN DO: 
         FIND FIRST TFConf NO-LOCK WHERE
                    TFConf.RVPercentage = ldeRVPerc    AND
                    TFConf.ValidTo     >= ldaOrderDate AND
                    TFConf.ValidFrom   <= ldaOrderDate NO-ERROR.
       
         IF AVAIL TFConf THEN ASSIGN 
            llFooter = TRUE
            ldTAE    = TFConf.TAE.
      END.
      ELSE llFooter = TRUE.
      
      IF llFooter THEN ASSIGN
         ttSub.TFBankFooterText[liFtrPos] = ttSub.TFBankFooterText[liFtrPos] + 
                                 (IF ttSub.TFBankFooterText[liFtrPos] > ""
                                  THEN CHR(10) ELSE "") +
                                 fHeadTxt(liFooterConf1,liLanguage)
         ttSub.TFBankFooterText[liFtrPos] = 
            REPLACE(ttSub.TFBankFooterText[liFtrPos],"#TAE",
                       REPLACE(fDispXMLDecimal(ldTAE),".",",")).
          
      IF llPenaltyFound AND 
         LOOKUP(lcTFPayTermEndBillCode,lcPaytermEndCodes) = 0 THEN ASSIGN
         ttSub.TFBankFooterText[liFtrPos] = ttSub.TFBankFooterText[liFtrPos] + 
                                 (IF ttSub.TFBankFooterText[liFtrPos] > ""
                                  THEN CHR(10) ELSE "") +
                                 fHeadTxt(liFooterConf2,liLanguage)
         lcPaytermEndCodes = lcPaytermEndCodes + "," + lcTFPayTermEndBillCode.
      
      liFFCount = liFFCount + 1.
     
   END.

RETURN TRUE.

END FUNCTION.

PROCEDURE pGetInvoiceHeaderData:
   
   DEF BUFFER bOldInvoice FOR Invoice. 
   DEF VAR ldaInvoiceFrom AS DATE NO-UNDO. 
   DEF VAR lcGraphGroup AS CHAR NO-UNDO.
   DEF VAR liOrder AS INT NO-UNDO. 
  
   DEF BUFFER bReq FOR Msrequest.
   DEF BUFFER bttRow FOR ttRow.

   EMPTY TEMP-TABLE ttGraph.
   
   ASSIGN 
      ldMinRow   = 0  /* 0 is used as min if no negative amounts */
      ldMaxRow   = 0.

   ASSIGN
      ldFromPer   = fMake2Dt(IF Invoice.FirstCall NE ? 
                             THEN Invoice.FirstCall
                             ELSE Invoice.FromDate,0)
      ldInvoiceFromPer = fMake2Dt(Invoice.FromDate,0)
      ldToPer     = fMake2DT(Invoice.ToDate,86399)
      ldtInvToDate = ADD-INTERVAL(Invoice.ToDate, -1, "months") 
      liPeriod    = YEAR(ldtInvToDate) * 100 + MONTH(ldtInvToDate).

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

   /* check penalty fees; only 1 fee per invoice allowed (for now) */
   FOR EACH InvRow OF Invoice NO-LOCK WHERE
            InvRow.RowType = 4 AND
            InvRow.Amt NE 0 AND
            InvRow.BillCode = "TERMPERIOD"
   BREAK BY InvRow.SubInvNum:
      IF FIRST-OF(InvRow.SubInvNum) THEN liPCnt = 0.
      liPCnt = liPCnt + 1.
      IF LAST-OF(InvRow.SubInvNum) AND liPCnt > 1 THEN DO:
         fErrLine("More than 1 penalty fee for " + InvRow.CLI).
         RETURN "ERROR".
      END.
      IF InvRow.VatPerc = 0 THEN
         ttInvoice.PenaltyAmt = ttInvoice.PenaltyAmt + InvRow.Amt.
   END.

   /* Total consumption of 6 last invoices */
   ASSIGN
      ldaInvoiceFrom = invoice.todate - (32 * 5)
      ldaInvoiceFrom = DATE(MONTH(ldaInvoiceFrom), 1, YEAR(ldaInvoiceFrom))
      liOrder = 1.

   FOR EACH bOldInvoice NO-LOCK WHERE
            bOldInvoice.Brand = gcBrand AND
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
                ttGraph.GraphName = lcGraphGroup
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

PROCEDURE pGetSubInvoiceHeaderData:

   DEF VAR liCount            AS INT  NO-UNDO.
   DEF VAR ldaOwnerDate       AS DATE NO-UNDO.
   DEF VAR liOwnerTime        AS INT  NO-UNDO.
   DEF VAR lcFusionCLITypes   AS CHAR NO-UNDO.
   DEF VAR ldPeriodFrom       AS DEC  NO-UNDO.
   DEF VAR ldPeriodTo         AS DEC  NO-UNDO.
   DEF VAR lcGroupCode        AS CHAR NO-UNDO.
   DEF VAR llPTFinancedByBank AS LOG  NO-UNDO.
   DEF VAR llRVFinancedByBank AS LOG  NO-UNDO.
   DEF VAR llDeletettRow      AS LOG  NO-UNDO.
   DEF VAR liRowOrder         AS INT  NO-UNDO.
   DEF VAR liQ25Phase         AS INT  NO-UNDO.

   DEF BUFFER UserCustomer    FOR Customer.
   DEF BUFFER bServiceLimit   FOR ServiceLimit.
   DEF BUFFER bMServiceLimit  FOR MServiceLimit.
   DEF BUFFER bMsOwner        FOR MsOwner.

   DEF BUFFER bType FOR CLIType.
   DEF BUFFER bReq  FOR MsRequest.

   EMPTY TEMP-TABLE ttSub.
   EMPTY TEMP-TABLE ttCLIType.
          
   FOR EACH SubInvoice OF Invoice NO-LOCK:

      CREATE ttSub.
      ASSIGN ttSub.CLI   = SubInvoice.CLI
             ttSub.MsSeq = SubInvoice.MsSeq. 

      /* user name */
      IF SubInvoice.CustNum = Invoice.CustNum OR SubInvoice.CustNum = 0 THEN 
         ttSub.UserName = lcCustName.
      ELSE DO:
         FIND FIRST UserCustomer WHERE 
            UserCustomer.CustNum = SubInvoice.CustNum NO-LOCK NO-ERROR.
         IF AVAILABLE UserCustomer THEN 
            ttSub.UserName = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                              BUFFER UserCustomer).
      END.

      IF CAN-FIND(FIRST MsOwner WHERE
                        Msowner.Brand  = gcBrand        AND
                        MsOwner.CLI    = SubInvoice.CLI AND
                        MsOwner.TsEnd >= ldFromPer      AND
                        MsOwner.TsEnd <= ldToPer)
      THEN DO:
         /* subscription has been terminated during billing period */
         FIND FIRST MsRequest WHERE
                    MsRequest.MsSeq     = SubInvoice.MsSeq AND
                    MsRequest.ReqType   = 18               AND
                    MsRequest.ReqStat   = 2                AND
                    MsRequest.ActStamp >= ldFromPer        AND
                    MsRequest.ActStamp <= ldToPer          NO-LOCK NO-ERROR.

         IF AVAILABLE MsRequest THEN DO:

            IF NOT ttInvoice.PostPoned AND
               NOT CAN-FIND(FIRST bReq WHERE
                              bReq.MsSeq     = SubInvoice.MsSeq    AND
                              bReq.ReqType   = 82                  AND
                              bReq.ReqStat   = 2                   AND
                              bReq.ActStamp >= ldFromPer           AND
                              bReq.ActStamp <= ldToPer             AND
                              bReq.ActStamp >  MsRequest.ActStamp) THEN
               FOR FIRST SingleFee WHERE
                         SingleFee.Brand    = gcBrand                  AND
                         SingleFee.CustNum  = SubInvoice.CustNum       AND
                         SingleFee.KeyValue = STRING(SubInvoice.MsSeq) AND
                     NOT SingleFee.Billed                              NO-LOCK:
                  ASSIGN ttInvoice.PostPoned = YES.
               END.

            ttSub.MessageType   = "13".   
         END. 
         
         /* stc during billing period, from post to pre  */
          FOR FIRST MsRequest NO-LOCK WHERE
                    MsRequest.MsSeq     = SubInvoice.MsSeq AND
                    MsRequest.ReqType   = 0                AND
                    MsRequest.ReqStat   = 2                AND
                    MsRequest.ActStamp >= ldFromPer        AND
                    MsRequest.ActStamp <= ldToPer,
              FIRST CLIType NO-LOCK WHERE
                    CLIType.Brand   = gcBrand AND
                    CLIType.CLIType = MsRequest.ReqCParam1,
              FIRST bType NO-LOCK WHERE
                    bType.Brand = gcBrand AND
                    bType.CLIType = MsRequest.ReqCParam2:
    
             IF CLIType.PayType = 1 AND bType.PayType = 2 THEN ttSub.MessageType = "15".
          END.
      
      END.
    
      /* if specifications on cli level wanted -> check cli data */
      IF Invoice.InvType NE 3 AND Invoice.InvType NE 4 THEN DO:

         ASSIGN ldPeriodFrom = ldFromPer
                ldPeriodTo   = ldToPer
                liCount      = 0
                lcGroupCode  = "".

         /* Check immediate STC */
         FIND FIRST bMsOwner NO-LOCK WHERE
                    bMsOwner.MsSeq   = SubInvoice.MsSeq AND
                    bMsOwner.TsBeg  >= ldInvoiceFromPer AND
                    bMsOwner.TsBeg  <= ldToPer         AND
                    bMSOwner.InvCust = Customer.CustNum AND
                    bMsOwner.CLIEvent BEGINS "iS" NO-ERROR.
         IF AVAIL bMsOwner THEN DO:
            fSplitTS(bMsOwner.TsBeg,OUTPUT ldaOwnerDate,OUTPUT liOwnerTime).

            IF bMsOwner.CLIEvent = "iSS" THEN DO:
               lcFusionCLITypes = fCParamC("FUSION_SUBS_TYPE").
               IF Invoice.DelType = {&INV_DEL_TYPE_FUSION_EMAIL} OR
                  Invoice.DelType = {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}
               THEN DO:
                  IF LOOKUP(bMsOwner.CLIType,lcFusionCLITypes) > 0 THEN
                     ASSIGN ldPeriodFrom = bMsOwner.TsBeg
                            ldPeriodTo   = ldToPer.
                  ELSE
                     ASSIGN ldPeriodFrom = ldFromPer
                            ldPeriodTo   = fOffSet(bMsOwner.TsBeg,-1).
               END.
               ELSE DO:
                  IF LOOKUP(bMsOwner.CLIType,lcFusionCLITypes) > 0 THEN
                     ASSIGN ldPeriodFrom = ldFromPer
                            ldPeriodTo   = fOffSet(bMsOwner.TsBeg,-1).
                  ELSE
                     ASSIGN ldPeriodFrom = bMsOwner.TsBeg
                            ldPeriodTo   = ldToPer.
               END.
            END.
         END.

         /* get clis that are involved in this invoice */
         FOR EACH MSOwner NO-LOCK USE-INDEX InvCust WHERE
                  MSOwner.InvCust = Customer.CustNum AND
                  MsOwner.CLI     = SubInvoice.CLI   AND
                  MsOwner.MsSeq   = SubInvoice.MsSeq AND
                  MsOwner.TsBeg  <= ldPeriodTo       AND
                  MsOwner.TsEnd  >= ldPeriodFrom     AND
                  MsOwner.PayType = FALSE :

            liCount = liCount + 1.

            CREATE ttCLIType.
                   ttCLIType.CLIType = MSOwner.CLIType.

            /* Special handling to display the CLIType name        */
            /* as Bundle name for IPL or FLAT TARIFF subscriptions */
            IF LOOKUP(ttCLIType.CLIType,lcBundleCLITypes) > 0 THEN
               ttCLIType.CLIType = MsOwner.TariffBundle.

            IF ttCLIType.CTName = "" OR ttCLIType.CTName = ? THEN
               ttCLIType.CTName = fLocalItemName("CLIType",
                                                 ttCLIType.CLIType,
                                                 liLanguage,
                                                 Invoice.ToDate).

            ASSIGN
               ttCLIType.CLI    = SubInvoice.CLI
               ttCLIType.TsBeg  = MsOwner.TsBeg
               ttCLIType.TsEnd  = MsOwner.TsEnd.

            FIND FIRST CLIType WHERE 
                       CLIType.Brand   = gcBrand AND
                       CLIType.CLIType = ttCLIType.CLIType NO-LOCK NO-ERROR.
            IF AVAILABLE CLIType THEN DO:

               /* Common Rate Plan for all FLAT tariffs CONTF */
               IF CLIType.CLIType BEGINS "CONTF" THEN
                  ttCLIType.RateName = ttCLIType.CTName.
               ELSE
                  ttCLIType.RateName = fLocalItemName("RatePlan",
                                                      CLIType.PricePlan,
                                                      liLanguage,
                                                      Invoice.ToDate).
            END.

            IF ttCLIType.CLIType BEGINS "CONTF" OR
               ttCLIType.CLIType = "CONT15" THEN DO:

               lcGroupCode = ttCLIType.CLIType.
               IF ttCLIType.CLIType = "CONTFF" THEN
                  lcGroupCode = "CONTFF2".
               ELSE IF ttCLIType.CLIType = "CONT15" THEN
                  lcGroupCode = "VOICE100".

               RELEASE ttRow.

               IF lcGroupCode BEGINS "CONTF" THEN
                  FOR FIRST DayCampaign NO-LOCK WHERE 
                            DayCampaign.Brand = gcBrand AND 
                            DayCampaign.DCEvent = lcGroupCode,
                      FIRST FMItem NO-LOCK WHERE
                            FMItem.Brand = gcBrand AND
                            FMItem.FeeModel = DayCampaign.FeeModel:
                     FIND FIRST ttRow WHERE  
                                ttRow.SubInvNum = SubInvoice.SubInvNum AND
                                ttRow.RowBillCode = FMItem.BillCode NO-ERROR.   
                  END.

               FOR EACH bServiceLimit NO-LOCK WHERE
                        bServiceLimit.GroupCode = lcGroupCode,
                   EACH bMServiceLimit NO-LOCK WHERE
                        bMServiceLimit.MsSeq = MSOwner.MsSeq  AND
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

                  IF bServiceLimit.GroupCode = "VOICE100" THEN
                     ttCLIType.CLIType = ttCLIType.CLIType + "V100".
               END.
            END.

            /* latest type is stored -> same type as was used in
               billing run for min. consumption */
            IF liCount = 1 THEN
               ASSIGN ttSub.CLIType = ttCLIType.CLIType
                      ttSub.CTName  = ttCLIType.CTName.

            /* Immediate STC logic non-fusion to non-fusion */
            IF AVAIL bMsOwner AND bMsOwner.CLIEvent = "iS" AND
               ttSub.OldCLIType = "" AND ttSub.CLIType <> ttCLIType.CLIType
            THEN DO:
               ASSIGN ttSub.OldCLIType = ttCLIType.CLIType
                      ttSub.OldCTName  = ttCLIType.CTName.
               IF ldaOwnerDate = ? THEN
                  ttSub.TariffActDate = "".
               ELSE
                  ttSub.TariffActDate = STRING(ldaOwnerDate).
            END. /* IF AVAIL bMsOwner AND bMsOwner.CLIEvent = "iS" AND */

         END.

         /* If there is no MsOwner found for billing period
            then find latest MsOwner */
         IF liCount = 0 THEN
            FOR FIRST MSOwner NO-LOCK USE-INDEX MsSeq WHERE
                      MSOwner.MsSeq = SubInvoice.MsSeq AND
                      MSOwner.InvCust = Customer.CustNum:

               ttSub.CLIType = (IF MsOwner.TariffBundle > "" THEN
                                MsOwner.TariffBundle ELSE MsOwner.CLIType).

               ttSub.CTName = fLocalItemName("CLIType",
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

      /* ttRows contains combined invrows => no need to check duplicates */
      FOR EACH ttRow WHERE
               ttRow.SubInvNum = SubInvoice.SubInvNum AND
               ttRow.RowCode BEGINS "33" AND
               ttRow.RowType = "":
         llDeletettrow = FALSE.
         IF ttRow.RowBillCode EQ "TERMPERIOD" AND
            ttRow.RowVatAmt EQ 0 THEN
            ttSub.PenaltyAmt = ttSub.PenaltyAmt + ttRow.RowAmt.
                  
         IF NOT (ttRow.RowBillCode BEGINS "PAYTERM" OR
                 ttRow.RowBillCode BEGINS "RVTERM") THEN NEXT.
         /* if Q25 discounts exist, these and original q25 fee are not 
            needed to print */
         FOR EACH bttRow WHERE 
                  bttRow.SubInvNum = ttRow.SubInvNum AND
                  bttrow.RowCode BEGINS "10":
            IF bttRow.RowBillCode EQ "RVTERMDT2DISC" OR
               bttRow.RowBillCode EQ "RVTERMDT4DISC" THEN DO:
               DELETE bttRow.
               llDeletettrow = TRUE.
            END.
         END.
         IF llDeletettRow THEN DELETE ttrow.
         ASSIGN
            ttSub.InstallmentAmt = ttSub.InstallmentAmt + ttRow.RowAmt.

         IF (LOOKUP(ttRow.RowBillCode,{&TF_BANK_RVTERM_BILLCODES})           > 0) OR
            (LOOKUP(ttRow.RowBillCode,{&TF_BANK_UNOE_PAYTERM_BILLCODES})     > 0) OR 
            (LOOKUP(ttRow.RowBillCode,{&TF_BANK_SABADELL_PAYTERM_BILLCODES}) > 0) THEN 
            llPTFinancedByBank = TRUE.
            
         ELSE IF 
            LOOKUP(ttRow.RowBillCode,{&TF_BANK_UNOE_RVTERM_BILLCODES}) > 0 OR
            LOOKUP(ttRow.RowBillCode,{&TF_BANK_SABADELL_RVTERM_BILLCODES}) > 0
         THEN llRVFinancedByBank = TRUE.
         /* if already found subscription in q24 phase, no need to search 
            further. Otherwise check if this subscription is on some Q22-Q24
            phase and no actions done. 0 means q24, 1 q23 and 2 q22, 99 some
            other phase or no q25. */
         IF (ttinvoice.q25Phase GT 0) THEN DO:
            liQ25Phase = getQ25Phase(SubInvoice.msseq, subinvoice.custnum).
            IF (liQ25Phase LT ttinvoice.q25Phase) THEN 
               ttinvoice.q25Phase = liQ25Phase.         
         END.
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

   EMPTY TEMP-TABLE ttRow.
   
   FOR EACH SubInvoice OF Invoice NO-LOCK:
   
      ASSIGN
         ldVatTot   = 0
         liRowOrder = 0.
      DO liVatCnt = 1 TO 5:
         ldVatTot = ldVatTot + SubInvoice.VatAmount[liVatCnt].
      END.

      /* handle rows in percentage order so that total tax amounts
         can be matched to row taxes */
      FOR EACH InvRow OF Invoice NO-LOCK WHERE
               InvRow.SubInvNum = SubInvoice.SubInvNum AND
               NOT (InvRow.RowType = 5 AND InvRow.Amt = 0),
         FIRST BillItem NO-LOCK WHERE
               BillItem.Brand    = gcBrand AND
               BillItem.BillCode = InvRow.BillCode,
         FIRST BItemGroup NO-LOCK WHERE
               BItemGroup.Brand = gcBrand AND
               BItemGroup.BIGroup = BillItem.BIGroup
      BREAK BY InvRow.VatPerc
            BY ABS(InvRow.Amt):
            
         ASSIGN ldVatAmt   = 0
            ttInvoice.InstallmentDiscAmt = ttInvoice.InstallmentDiscAmt + 
                                           InvRow.Amt WHEN InvRow.RowType EQ 9 AND
                                           LOOKUP(InvRow.BillCode, 
                                                  {&INSTALLMENT_DISCOUNT_BILLCODES}) > 0.
         IF BillItem.BIGroup = "33" AND
            (InvRow.BillCode BEGINS "PAYTERM" OR
             InvRow.BillCode BEGINS "RVTERM") THEN
            ttInvoice.InstallmentAmt = ttInvoice.InstallmentAmt + InvRow.Amt.

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

         lcRowName = RIGHT-TRIM(fLocalItemName("BillItem",
                                    InvRow.BillCode,
                                    liLanguage,
                                    IF InvRow.ToDate NE ?
                                    THEN InvRow.ToDate
                                    ELSE Invoice.ToDate)).

         lcRowCode = STRING(BItemGroup.BiGroup) + lcRowName.

         FIND FIRST ttRow WHERE
                    ttRow.SubInvNum = InvRow.SubInvNum AND
                    ttRow.RowCode   = lcRowCode NO-ERROR.
            
         IF NOT AVAILABLE ttRow THEN DO:
      
            CREATE ttRow.
            ASSIGN 
               ttRow.SubInvNum  = InvRow.SubInvNum
               ttRow.RowGroup   = BItemGroup.BIGroup
               ttRow.RowCode    = lcRowCode
               liRowOrder       = liRowOrder + 2
               ttRow.RowOrder   = liRowOrder
               ttRow.GroupOrder = BItemGroup.InvoiceOrder.
         END.

         ASSIGN 
            ttRow.RowType       = ""
            ttRow.RowAmtExclVat = ttRow.RowAmtExclVat + ldAmtExclVat
            ttRow.RowVatAmt     = ttRow.RowVatAmt + ldVatAmt
            ttRow.RowAmt        = ttRow.RowAmt + ldAmt
            ttRow.RowData       = ttRow.RowData + InvRow.DataAmt
            ttRow.RowQty        = ttRow.RowQty + InvRow.Qty
            ttRow.RowDur        = ttRow.RowDur + InvRow.Min
            ttRow.RowBillCode   = BillItem.BillCode
            ttRow.RowName       = lcRowName
            ttRow.RowToDate     = InvRow.ToDate.
      END.

   END.

   RETURN "".
   
END PROCEDURE.

/* note: should be called only after pGetInvoiceRowData */
PROCEDURE pGetInvoiceVatData:

   EMPTY TEMP-TABLE ttVat.
   DEF VAR ldeInstallmentSum AS DEC NO-UNDO. 

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
   ldeInstallmentSum = ttInvoice.InstallmentAmt +
                       ttInvoice.PenaltyAmt +
                       ttInvoice.InstallmentDiscAmt.

   IF ldeInstallmentSum NE 0 THEN
      FOR FIRST ttVat WHERE ttVat.VatPerc = 0 EXCLUSIVE-LOCK:
         IF ttVat.VatBasis EQ ldeInstallmentSum THEN DELETE ttVat.
         ELSE ttVat.VATBasis = ttVAT.VATBasis - ldeInstallmentSum.
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
         ASSIGN ITSendLog.Brand      = gcBrand 
                ITSendLog.TxtType    = 3
                ITSendLog.ITNum      = 0
                ITSendLog.CustNum    = Invoice.CustNum
                ITSendLog.InvNum     = Invoice.InvNum
                ITSendLog.SendMethod = liSendMethod
                ITSendLog.EMail      = ""
                ITSendLog.RepType    = "Inv"
                ITSendLog.SendInfo   = icPrintHouse
                ITSendLog.UserCode   = katun.
                ITSendLog.SendStamp  = fMakeTS().
      END.
 
      RELEASE Invoice.    
   END. 

END PROCEDURE.

PROCEDURE pErrorFile:

   DEF INPUT PARAMETER ilDBWrite  AS LOG  NO-UNDO.
   DEF INPUT PARAMETER icActionID AS CHAR NO-UNDO.
   
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

   ldCurrStamp = fMakeTS().
    
   FOR EACH ttError TRANS:
      PUT STREAM slog UNFORMATTED
           ttError.Inv    CHR(9)
           ttError.Cust   CHR(9)
           ttError.ErrMsg lcNewLine.
           
      /* save to db for reporting */
      IF ilDBWrite THEN DO:
         CREATE ErrorLog.
         ASSIGN ErrorLog.Brand     = gcBrand
                ErrorLog.ActionID  = icActionID
                ErrorLog.TableName = IF ttError.TableName > ""
                                     THEN ttError.TableName
                                     ELSE "Invoice"
                ErrorLog.KeyValue  = IF ttError.KeyValue > ""
                                     THEN ttError.KeyValue
                                     ELSE ttError.Inv
                ErrorLog.ActionTS  = ldCurrStamp
                ErrorLog.UserCode  = katun
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

   DEF INPUT PARAMETER iiInvSeq AS INT NO-UNDO.

   DEF VAR liErrorCodeOut AS INT  NO-UNDO.

   DEF BUFFER bCallInvSeq FOR InvSeq.
   
   EMPTY TEMP-TABLE ttCall.
  
   FIND FIRST bCallInvSeq WHERE bCallInvSeq.InvSeq = iiInvSeq NO-LOCK NO-ERROR.
   IF NOT AVAILABLE bCallInvSeq THEN RETURN. 
   
   fMobCDRCollect(INPUT "post",
                  INPUT gcBrand,
                  INPUT katun,
                  INPUT bCallInvSeq.FromDate,   
                  INPUT bCallInvSeq.ToDate,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT iiInvSeq,
                  INPUT 0,
                  INPUT "",
                  INPUT "",
                  INPUT "",
                  INPUT 0,
                  INPUT-OUTPUT liErrorCodeOut,
                  INPUT-OUTPUT tthCDR).

   FOR EACH ttCall EXCLUSIVE-LOCK:
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand AND
                 BillItem.BillCode = ttCall.BillCode NO-LOCK NO-ERROR.
      IF AVAIL BillItem THEN DO:
         FIND FIRST BItemGroup WHERE
                    BItemGroup.Brand   = gcBrand AND
                    BItemGroup.BIGroup = ttCall.BIGroup NO-LOCK NO-ERROR.
         ASSIGN
            ttCall.GroupOrder = BItemGroup.InvoiceOrder WHEN AVAIL BItemGroup
            ttCall.BIGroup = BillItem.BIGroup.
      END.
   END.

END PROCEDURE.


