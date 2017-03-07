/* fprintinv.i          28.04.03/aam 
   common functions for all invoice printing routines
   
   callers: eplfile.p
            nnlaki.p
            xmlinv.p

   changed:             04.07.03/aam text from InvText (Invoice/Gen)
                        12.08.03/aam serviceaddress for vas rows
                        05.12.03/aam name and address from IDel,
                                     fVatUsageTitle()
                        09.02.04/aam fFormRefNum()
                        16.02.04/aam fdivtxt.i
                        22.03.04/aam invoice rows on cli level
                        28.05.04/aam SMS
                        30.06.04/aam header9 to reminder
                        26.10.04/aam new parameter to nnpura4: full b-numbers
                        14.12.05/aam username from customer, not msowner
                        18.01.06/aam use fPrintCustName()
                        22.06.06/aam next due date not printed for end invoices
                        07.08.06/aam letterclass to nnpura3/4
                        20.11.06/aam new db structure (ordercustomer)
*/

&IF "{&AllIncludes}" = "YES"
&THEN
{Syst/commali.i}
{Func/cparam2.i}
{Func/refcode.i}
{Inv/edefine.i NEW}
{Func/fotint.i}
{Func/feplform.i}
&ENDIF
{Func/transname.i}
{Func/timestamp.i}
{Func/invotxtp.i}
{Func/ftmscode.i}
{Func/fcustbal.i}
{Func/frefnum.i}
{Func/fdivtxt.i}
{Func/ftxttag.i}
{Func/fconinfo.i}
{Func/fduedate.i}
{Func/fmakesms.i}
{Func/fgettxt.i}

DEF VAR lcPaymTerm   AS CHAR FORMAT "x(15)"          NO-UNDO.
DEF VAR lcCustName   LIKE Customer.CustName          NO-UNDO.
DEF VAR lcConnPer    LIKE Customer.Contact           NO-UNDO.
DEF VAR lcOurCon     AS CHAR FORMAT "X(30)"          NO-UNDO. 
DEF VAR lcCoName     LIKE Customer.COName            NO-UNDO.
DEF VAR lcAddress    LIKE Customer.Address           NO-UNDO.
DEF VAR lcPost       LIKE Customer.PostOffice        NO-UNDO.
DEF VAR lcCountry    AS CHAR FORMAT "x(24)"          NO-UNDO.
DEF VAR lcRefNum     AS CHAR FORMAT "x(22)"          NO-UNDO.

DEF VAR liNotTime    AS INT                          NO-UNDO.
DEF VAR lcNotTime    AS CHAR                         NO-UNDO.
DEF VAR lcDelInt     AS CHAR                         NO-UNDO.
DEF VAR lcRemFee     AS CHAR                         NO-UNDO. 
DEF VAR lcRowHeader  AS CHAR                         NO-UNDO EXTENT 18.
DEF VAR lcInvHeader  AS CHAR                         NO-UNDO EXTENT 10.
DEF VAR llGsmLine    AS LOG                          NO-UNDO.
DEF VAR llFeeLine    AS LOG                          NO-UNDO. 
DEF VAR liCount      AS INT                          NO-UNDO.
DEF VAR liOrder      AS INT                          NO-UNDO. 
DEF VAR liFeeOrder   AS INT                          NO-UNDO. 
DEF VAR liTxtOrd     AS INT                          NO-UNDO. 
DEF VAR xFormType    AS INT                          NO-UNDO.
DEF VAR lcMainTitle  AS CHAR                         NO-UNDO. 
DEF VAR lcDateHead   AS CHAR                         NO-UNDO. 

DEF VAR lcBoxText     AS CHAR                        NO-UNDO. 
DEF VAR lcVatID       AS CHAR                        NO-UNDO. 
DEF VAR lcBarCodeBank AS CHAR                        NO-UNDO. 
DEF VAR lcInvForm     AS CHAR                        NO-UNDO. 
DEF VAR liTextLength  AS INT                         NO-UNDO INIT 68.
DEF VAR lcText        AS CHAR                        NO-UNDO.
 
DEF VAR ldtNextDueDate AS DATE                       NO-UNDO. 
DEF VAR lcRowUFName    AS CHAR                       NO-UNDO.
DEF VAR lcRowCLI       AS CHAR                       NO-UNDO.
DEF VAR lcDefSMS       AS CHAR                       NO-UNDO. 
DEF VAR lcSMSMessage   AS CHAR                       NO-UNDO.

DEF VAR DDCustomer   AS LO                           NO-UNDO.
DEF VAR DDText       AS CHAR                         NO-UNDO EXTENT 3.
DEF VAR DDBAcc       AS CHAR                         NO-UNDO. 
DEF VAR DDBAccBT     AS CHAR                         NO-UNDO. 

DEF TEMP-TABLE ttLine NO-UNDO   
    FIELD RowName  AS CHAR
    FIELD FromDate AS DATE
    FIELD ToDate   AS DATE
    FIELD VatPerc  AS DEC
    FIELD Amount   AS DEC
    FIELD Order    AS INT
    FIELD Memo     AS CHAR 
    FIELD RowID    AS CHAR 
    FIELD RowFont  AS CHAR
    INDEX RowName RowName VatPerc. 

DEF TEMP-TABLE ttVat NO-UNDO
    FIELD VatPerc AS DEC
    FIELD SlsAmt  AS DEC
    FIELD VatAmt  AS DEC
    FIELD Text1   AS CHAR
    FIELD Text2   AS CHAR
    FIELD Text3   AS CHAR 
    INDEX VatPerc VatPerc. 

DEF TEMP-TABLE ttBankAcc NO-UNDO
   FIELD BankAccount AS CHAR
   FIELD BankData    AS CHAR
   FIELD EplForm     AS CHAR
   FIELD BarCode     AS CHAR
   INDEX BarCode BarCode.

DEF TEMP-TABLE ttGenText NO-UNDO
   FIELD ITNum   AS INT 
   FIELD ITText  AS CHAR
   INDEX ITNum ITNum. 
    
DEF BUFFER bAgrCust  FOR Customer.

IF AVAILABLE Company THEN lcTagPhone = Company.Phone.

/* notice time */
liNotTime = fCParamI("NoticeTime").
IF liNotTime = ? OR liNotTime = 0 THEN liNotTime = 7.

/* default sms-message */
lcDefSMS = fGetTxt("SMS",
                   "laskutul",
                   TODAY,
                   1).
                       
       /* reminder fee */
ASSIGN lcRemFee      = TRIM(STRING(fCParamDE("DefRemindFee"),">9.99")) + " EUR"
       /* name for usage fee rows */
       lcRowUFName   = fTeksti(173,1)
       lcRowCLI      = fTeksti(174,1)
       /* headers for contact info */
       lcBTHeader[1] = fTeksti(176,1)
       lcBTHeader[2] = fTeksti(177,1)
       lcBTHeader[3] = fTeksti(182,1)
       lcHSender     = fTeksti(178,1).

/* get bankaccounts for sheets into temp-table */
FOR EACH BankAccount NO-LOCK WHERE
         BankAccount.Brand = gcBrand:

   DO liCount = 1 TO NUM-ENTRIES(BankAccount.InvForm):
      CREATE ttBankAcc.
      ASSIGN ttBankAcc.BankAccount = BankAccount.BankOffice + " " + 
                                     BankAccount.BankAccount
             ttBankAcc.BankData    = BankAccount.BankData
             ttBankAcc.EplForm     = ENTRY(liCount,BankAccount.InvForm).
   END.              

   DO liCount = 1 TO NUM-ENTRIES(BankAccount.BarCode):
      CREATE ttBankAcc.
      ASSIGN ttBankAcc.BankAccount = BankAccount.BankAccount
             ttBankAcc.BankData    = BankAccount.BankData
             ttBankAcc.BarCode     = ENTRY(liCount,BankAccount.BarCode).
   END.   

END. 


/* get customer names, payment terms etc. */
FUNCTION fSetHeaders RETURNS LOGICAL
   (iiPrintType AS INT).

   IF (iiPrintType NE 1 AND NOT AVAILABLE Invoice) OR
      NOT AVAILABLE Customer 
   THEN RETURN FALSE. 
   
   ASSIGN lcCustName     = ""
          lcTagCLI       = ""
          lcTagUser      = ""
          ldtNextDueDate = ?
          lcTagOwner     = "".
   
   /* for deposit invoices get receiver through order */
   IF iiPrintType NE 1 AND (Invoice.InvType = 3 OR Invoice.InvType = 4)
   THEN DO:
      FOR FIRST SingleFee NO-LOCK WHERE
                SingleFee.InvNum    = Invoice.InvNum    AND
                SingleFee.HostTable = "Order",
          FIRST Order NO-LOCK WHERE
                Order.Brand   = Invoice.Brand AND
                Order.OrderID = INTEGER(SingleFee.KeyValue):
                
         FOR FIRST OrderCustomer NO-LOCK WHERE
                   OrderCustomer.Brand   = gcBrand AND
                   OrderCustomer.OrderID = Order.OrderID AND
                   OrderCustomer.RowType = 1:

            ASSIGN 
               lcCustName = DYNAMIC-FUNCTION("fDispOrderName" IN ghFunc1,
                                             BUFFER OrderCustomer)
               lcCoName   = ""
               lcAddress  = OrderCustomer.Address
               lcPost     = OrderCustomer.ZipCode + " " + 
                            OrderCustomer.PostOffice
               lcCountry  = OrderCustomer.Country
               lcTagCLI   = Order.CLI
               lcTagOwner = lcCustName
               lcTagUser  = lcCustName.
         END.
                
         /* is there a separate user */
         IF Order.UserRole NE 1 THEN
         FOR FIRST OrderCustomer NO-LOCK WHERE
                   OrderCustomer.Brand   = gcBrand AND
                   OrderCustomer.OrderID = Order.OrderID AND
                   OrderCustomer.RowType = Order.UserRole:

            lcTagUser = DYNAMIC-FUNCTION("fDispOrderName" IN ghFunc1,
                                         BUFFER OrderCustomer).
         END.                                
      END.
      
      IF lcCustName = "" THEN 
      FOR FIRST SingleFee NO-LOCK WHERE
                SingleFee.InvNum    = Invoice.InvNum    AND
                SingleFee.HostTable = "mobsub",
          FIRST MsOwner NO-LOCK WHERE
                MsOwner.MsSeq   = INTEGER(SingleFee.KeyValue),
          FIRST Customer NO-LOCK WHERE
                Customer.CustNum = MsOwner.CustNum AND
                Customer.InvCust = Invoice.CustNum,
          FIRST bAgrCust NO-LOCK WHERE
                bAgrCust.CustNum = Customer.AgrCust:

                /* receiver is always the owner */
         ASSIGN lcCustName = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                               BUFFER bAgrCust)
                lcCoName   = bAgrCust.COName
                lcAddress  = bAgrCust.Address
                lcPost     = bAgrCust.ZipCode + " " + bAgrCust.PostOffice
                lcCountry  = bAgrCust.Country
                lcTagCLI   = MsOwner.CLI
                lcTagUser  = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                              BUFFER Customer)
                lcTagOwner = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                              BUFFER bAgrCust).
      END.       
   END.
   
   /* order not found or normal invoice */
   IF lcCustName = "" THEN DO:

      /* delivery address */
      IF Customer.IDelName = "" THEN ASSIGN 
         lcCustName = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                       BUFFER Customer)
         lcCoName   = Customer.COName
         lcAddress  = Customer.Address
         lcPost     = Customer.ZipCode + " " + Customer.PostOffice
         lcCountry  = Customer.Country.
      ELSE ASSIGN 
         lcCustName = Customer.IDelName
         lcCOName   = Customer.IDelCOName
         lcAddress  = Customer.IDelAddr
         lcPost     = Customer.IDelZipCode + " " + Customer.IDelPost
         lcCountry  = Customer.IDelCountry.
          
      IF Customer.AgrCust NE Customer.CustNum THEN DO:
         FIND bAgrCust WHERE bAgrCust.CustNum = Customer.AgrCust 
         NO-LOCK NO-ERROR.
         IF AVAILABLE bAgrCust THEN 
            lcTagOwner = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                          BUFFER bAgrCust).
      END.
      ELSE lcTagOwner = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                         BUFFER Customer).
      
      lcConnPer = Customer.Contact.
      
      /* next due date */
      IF iiPrintType NE 1 AND Invoice.EndInvoice = 0 THEN DO:
         IF MONTH(Invoice.InvDate) = 12
         THEN ldtNextDueDate = DATE(1,
                                    DAY(Invoice.InvDate),
                                    YEAR(Invoice.InvDate) + 1).
         ELSE DO:
            liCount = DAY(Invoice.InvDate).
            IF liCount > 28 THEN DO:
               IF MONTH(Invoice.InvDate) = 1 THEN liCount = 28.
               ELSE IF liCount = 31 AND MONTH(Invoice.InvDate) NE 7 
               THEN liCount = 30.
            END.
            ldtNextDueDate = DATE(MONTH(Invoice.InvDate) + 1,
                                  liCount,
                                  YEAR(Invoice.InvDate)).
         END.                         
         ldtNextDueDate = ldtNextDueDate + Customer.PaymTerm.
         
         /* check for weekend and holidays */
         ldtNextDueDate = fChkDueDate(ldtNextDueDate).
         
      END.
      
   END.
      
   ASSIGN liKieli     = Customer.Language
          lcPaymTerm  = IF iiPrintType = 1 OR Invoice.EndInvoice > 0
                        THEN fTeksti(163,liKieli)
                        ELSE STRING(Customer.PaymTerm) + 
                             " " + fTeksti(55,liKieli)
          lcOurCon    = "" 
          lcMainTitle = fTeksti(IF Invoice.InvAmt < 0 OR Invoice.InvType = 5 
                                THEN 162
                                ELSE IF Invoice.PrintState > 0
                                     THEN 183
                                     ELSE 161,liKieli).

   /* reference nbr */
   ASSIGN lcRefNum   = fFormRefNum(Invoice.CustNum,
                                   Invoice.InvNum,
                                   IF iiPrintType = 1
                                   THEN 0
                                   ELSE Invoice.InvType)
          lcTagRefNum = lcRefNum.                        
   
   /* get the interest percent */ 
   lcDelInt   = TRIM(STRING(fOTIntPerc(Customer.Category,
                                       Invoice.InvDate),">9.99")) + " " + "%".
   
   IF iiPrintType = 1 THEN lcMainTitle = fTeksti(175,liKieli).
   
   ASSIGN /* notice time */
          lcNotTime  = STRING(liNotTime) + " " + 
                       fTeksti(56,liKieli)
          /* vatid */
          lcVatId    = fTeksti(152,liKieli)
          lcDateHead = fTeksti(136,liKieli).
          
   /* direct debit */
   ASSIGN DDCustomer = (iiPrintType NE 1 AND
                        Invoice.ChargeType = 2 AND 
                        Invoice.DDBankAcc NE "")
          DDText     = ""
          DDBAcc     = ""
          DDBAccBT   = "". 

   IF DDCustomer THEN DO:
      ASSIGN DDText[1] = fTeksti(96,liKieli)
             DDText[2] = fTeksti(97,liKieli)
             DDText[3] = fTeksti(98,liKieli) + " " + 
                         TRIM(STRING(Invoice.InvAmt,"->>>>>>9.99")) + " " +
                         Invoice.Currency
             DDBAcc    = Invoice.DDBankAcc.

      IF INDEX(DDBAcc,"-") = 0 THEN 
          DDBAcc = SUBSTR(Invoice.DDBankAcc,5,6) + "-" +
                   SUBSTR(Invoice.DDBankAcc,11). 
 
      /* modified layout for bank-transfer (space between each number) */
      DO liCount = 1 TO LENGTH(DDBAcc):

         IF SUBSTRING(DDBAcc,liCount,1) = "-"
         THEN DDBAccBT = DDBAccBT + " ".
         ELSE DDBAccBT = DDBAccBT + SUBSTRING(DDBAcc,liCount,1).
          
         DDBAccBT = DDBAccBT + " ".
      END.
   END. 

          
   ASSIGN /* header texts for upper right corner */
          lcInvHeader[1] = fTeksti(142,liKieli)
          lcInvHeader[2] = fTeksti(141,liKieli)
          lcInvHeader[3] = fTeksti(143,liKieli)
          lcInvHeader[4] = fTeksti(140,liKieli)
          lcInvHeader[5] = fTeksti(137,liKieli)
          lcInvHeader[6] = fTeksti(138,liKieli)
          lcInvHeader[7] = fTeksti(139,liKieli)
          lcInvHeader[8] = fTeksti(171,liKieli)
          lcRowHeader    = "".
          
   IF iiPrintType NE 1 THEN DO:
       ASSIGN        
          /* header texts for lines */
          lcRowHeader[1] = fTeksti(110,liKieli)
          lcRowHeader[2] = fTeksti(84,liKieli)
          lcRowHeader[6] = IF Invoice.VatIncl = FALSE
                           THEN fTeksti(158,liKieli)
                           ELSE fTeksti(159,liKieli).
 
       IF INDEX(lcRowHeader[6],"!") > 0
       THEN ASSIGN lcRowHeader[16] = ENTRY(1,lcRowHeader[6],"!")
                   lcRowHeader[6]  = ENTRY(2,lcRowHeader[6],"!").
            
       ASSIGN 
       lcRowHeader[2]  = FILL(" ",5 - LENGTH(lcRowHeader[2])) + lcRowHeader[2]
       lcRowHeader[6]  = FILL(" ",13 - LENGTH(lcRowHeader[6])) + lcRowHeader[6]
       lcRowHeader[16] = FILL(" ",13 - LENGTH(lcRowHeader[16])) +
                         lcRowHeader[16].
   END.
 
   ELSE DO:
       ASSIGN        
          lcInvHeader[7] = fTeksti(172,liKieli)
          
          /* header texts for inv.lines */
          lcRowHeader[1] = fTeksti(164,liKieli)
          lcRowHeader[2] = fTeksti(165,liKieli)
          lcRowHeader[3] = fTeksti(166,liKieli)
          lcRowHeader[4] = fTeksti(169,liKieli)
          lcRowHeader[5] = fTeksti(167,liKieli)
          lcRowHeader[6] = fTeksti(168,liKieli)
          lcRowHeader[7] = fTeksti(169,liKieli)
          lcRowHeader[8] = fTeksti(170,liKieli)
          lcRowHeader[9] = fTeksti(186,liKieli).
          
 
       ASSIGN 
       lcRowHeader[2] = FILL(" ",8 - LENGTH(lcRowHeader[2])) + lcRowHeader[2]
       lcRowHeader[4] = FILL(" ",10 - LENGTH(lcRowHeader[4])) + lcRowHeader[4]
       lcRowHeader[5] = FILL(" ",10 - LENGTH(lcRowHeader[5])) + lcRowHeader[5]
       lcRowHeader[7] = FILL(" ",13 - LENGTH(lcRowHeader[7])) + lcRowHeader[7].
       
   END.

   EMPTY TEMP-TABLE ttVat. 

   /* text before bank transfer */ 
   lcBoxText = fTeksti(500,liKieli) + " " +
               fTeksti(501,liKieli).
   
   RETURN TRUE. 
   
END FUNCTION.   

/* collect vat amounts by vat percent -> print a summary after invoice rows */
FUNCTION fCollVat RETURNS LOGICAL.

   /* totals by vat percent */
   FIND FIRST ttVat WHERE ttVat.VatPerc = InvRow.VatPerc NO-ERROR.
   IF NOT AVAILABLE ttVat THEN DO:
      CREATE ttVat.
      ASSIGN ttVat.VatPerc = InvRow.VatPerc.
             ttVat.Text1   = fTeksti(75,liKieli).
             ttVat.Text2   = fTeksti(76,liKieli).
             
      IF Invoice.VatUsage = 2 THEN DO:
         FIND VatCode WHERE VatCode.VatCode = InvRow.VatCode NO-LOCK NO-ERROR.
         IF AVAILABLE VatCode THEN ttVat.Text3 = VatCode.VCName.
      END.
   END.
                     
   ttVat.SlsAmt = ttVat.SlsAmt + InvRow.Amt. 
 
END FUNCTION.

/* get name for billing item (invoice row) */
FUNCTION fProdName RETURNS CHARACTER.

   DEF VAR lcName AS CHAR NO-UNDO. 

   lcName = fTranslationName(gcBrand,
                             1,
                             InvRow.BillCode,
                             liKieli,
                             IF InvRow.ToDate NE ? 
                             THEN InvRow.ToDate
                             ELSE Invoice.ToDate).
   
   IF lcName = "" OR lcName = ? THEN DO:

      FIND FIRST BillItem WHERE 
                 BillItem.Brand = gcBrand AND
                 BillItem.BillCode = InvRow.BillCode 
      NO-LOCK NO-ERROR.
      lcName = IF AVAILABLE BillItem 
               THEN BillItem.BIName 
               ELSE InvRow.BillCode.

   END.
   
   /* serviceaddress for vas rows */
   IF InvRow.RowType = 8 
   THEN lcName = InvRow.ServiceAddress + " " + lcName.
 
   RETURN lcName.

END FUNCTION.

/* sort invoice rows according to their type and billing item code */
FUNCTION fSortInvRows RETURNS LOGICAL.
 
   DEF VAR lcRowName AS CHAR NO-UNDO.
    
   /* first go through lines in order to sort them for printing */
   EMPTY TEMP-TABLE ttLine.

   ASSIGN llGsmLine  = FALSE
          llFeeLine  = FALSE
          liOrder    = 500
          liFeeOrder = 0.
      
   FOR EACH InvRow of Invoice NO-LOCK
   BY InvRow.RowType
   BY InvRow.BillCode
   BY InvRow.CLI 
   BY (IF InvRow.RowType = 8
       THEN InvRow.ServiceAddress
       ELSE "")
   BY InvRow.FromDate:

      /* all usage fees are combined by cli */  
      IF InvRow.RowType <= 2 THEN DO:
         lcRowName = (IF liKieli = 1 
                      THEN lcRowUFName
                      ELSE fTeksti(173,liKieli)) + " " + InvRow.CLI.
         IF InvRow.RowType NE 1 THEN llGSMLine = TRUE.
      END. 
      
      /* contract fees (and fatime) are combined by product by cli */          
      ELSE DO:
         ASSIGN llFeeLine = TRUE
                lcRowName = fProdName().
         IF InvRow.CLI > "" THEN
            lcRowName = lcRowname + ", " + 
                         InvRow.CLI.
      END.
                     
      FIND FIRST ttLine WHERE
                 ttLine.RowName = lcRowName AND
                 ttLine.VatPerc = InvRow.VatPerc NO-ERROR.
      IF NOT AVAILABLE ttLine THEN DO:
         CREATE ttLine.
         ASSIGN ttLine.RowName = lcRowName
                ttLine.VatPerc = InvRow.VatPerc.
                
         IF InvRow.RowType <= 2 OR InvRow.RowType = 7 THEN ASSIGN
            liOrder      = liOrder + 1
            ttLine.Order = liOrder.
         ELSE ASSIGN 
            liFeeOrder   = liFeeOrder + 1
            ttLine.Order = liFeeOrder.
      END.
         
      ASSIGN ttLine.Amount = ttLine.Amount + InvRow.Amt
             ttLine.RowID  = ttLine.RowID + 
                             (IF ttLine.RowID > "" THEN "," ELSE "") +
                             STRING(InvRow.InvRowNum). 
         
      IF InvRow.FromDate NE ? AND
         (ttLine.FromDate = ? OR InvRow.FromDate < ttLine.FromDate)
      THEN ttLine.FromDate = InvRow.FromDate.
      
      IF InvRow.ToDate NE ? AND
         (ttLine.ToDate = ? OR InvRow.ToDate > ttLine.ToDate)
      THEN ttLine.ToDate = InvRow.ToDate.
         
      DO liCount = 1 TO 5:
         IF InvRow.Memo[liCount] > ""
         THEN ttLine.Memo = ttLine.Memo + 
                            (IF ttLine.Memo > "" THEN "¤" ELSE "") +
                            InvRow.Memo[liCount].
      END. 
         
      /* invoice texts */
      fCollRowTxt().
        
      /* totals by vat percent */
      fCollVat().
      
   END. 

   /* if no gsm-fees then check if a subscription exists */
   IF NOT llGsmLine THEN DO:
      IF CAN-FIND(FIRST Mobsub OF Customer) THEN llGsmLine = TRUE.
   END. 

   /* interests */
   IF Invoice.InterestAmt NE 0 THEN DO:
      CREATE ttLine.
      ASSIGN ttLine.RowName = fTeksti(180,liKieli)
             ttLine.VatPerc = -1
             liOrder        = liOrder + 1
             ttLine.Order   = liOrder
             ttLine.Amount  = Invoice.InterestAmt.
   END.
   
   /* adv.payment */
   IF Invoice.AdvPaym NE 0 THEN DO:
      CREATE ttLine.
      ASSIGN ttLine.RowName = fTeksti(181,liKieli)
             ttLine.VatPerc = -1
             liOrder        = liOrder + 1
             ttLine.Order   = liOrder
             ttLine.Amount  = Invoice.AdvPaym.
   END.
 
   /* overpayment */
   IF Invoice.OverPaym NE 0 THEN DO:
      CREATE ttLine.
      ASSIGN ttLine.RowName = fTeksti(IF Invoice.OverPaym < 0
                                      THEN 81
                                      ELSE 82,
                                      liKieli)
             ttLine.VatPerc = -1
             liOrder        = liOrder + 1
             ttLine.Order   = liOrder
             ttLine.Amount  = Invoice.OverPaym.
   END.

END FUNCTION.

/* get header (subtitle) for invoice row group */
FUNCTION fGetSubHeader RETURNS CHARACTER
   (iiHeaderNbr AS INT).
   
   DEF VAR liGetHeader  AS INT  NO-UNDO. 
   
   CASE iiHeaderNbr:
   WHEN 1  THEN liGetHeader = 91.
   WHEN 2  THEN liGetHeader = 92.
   WHEN 3  THEN liGetHeader = 93.
   OTHERWISE liGetHeader = 0.
   END CASE.
            
   IF liGetHeader = 0 
   THEN RETURN "".            
   ELSE RETURN fTeksti(liGetHeader,liKieli).
 
END FUNCTION.

FUNCTION fCreInvTxt RETURNS LOGICAL
   (icText AS CHAR,
    iiPos  AS INT).
    
  CREATE ttInvoTxt.
  ASSIGN ttInvoTxt.ITPos    = iiPos
         liTxtOrd           = liTxtOrd + 1
         ttInvoTxt.ITOrd    = liTxtOrd 
         ttInvoTxt.ITTxt    = icText
         ttInvoTxt.ITTarg   = ""
         ttInvoTxt.ITKey    = "".
   
   RETURN TRUE.
   
END FUNCTION.

/* get all sorts of texts that should be printed on invoice, and collect
   then all into one temp-table */
FUNCTION fGetInvoiceTxt RETURNS LOGICAL.

   DEF VAR lcOpAmt    AS CHAR NO-UNDO.
   DEF VAR ldOpAmt    AS DEC  NO-UNDO. 
   DEF VAR lcTxt      AS CHAR NO-UNDO. 
   DEF VAR llPrefCall AS LOG  NO-UNDO.

   /* information texts */
   fCollInvTxt().
 
   /* check if balance still remains after this invoice */
   ldOpAmt  = fGetCustBal(Customer.CustNum,"TOTAL","AP") + 
              fGetCustBal(Customer.CustNum,"TOTAL","OP").

   /* but only if balance is used for this invoice */   
   IF (Invoice.AdvPaym < 0 OR Invoice.OverPaym < 0) AND
      ldOpAmt > 0 
   THEN DO:

      lcOpAmt = TRIM(STRING(ldOpAmt,"->>>>>>9.99")).

      DO liCount = 85 TO 86:

         /* two text lines reserved, balance sum can be in either one
            of them */
         lcTxt = REPLACE(fTeksti(liCount,liKieli),"#",lcOpAmt).
        
         IF lcTxt NE "" THEN DO:
         
            /* empty line first */
            IF liCount = 85 THEN DO:
               fCreInvTxt(".",2).
            END.
            
            fCreInvTxt(lcTxt,2).
         END.
         
      END.
      
   END.

END FUNCTION.

FUNCTION fInvoiceForm RETURNS LOGICAL
   (iiPrintType AS INT). 
   
    /* reminder */
    IF iiPrintType = 1 THEN ASSIGN xFormType = 1. 

    /* normal invoice, deposit invoice OR advance payment invoice */
    ELSE DO:
        CASE Invoice.InvType:
        WHEN 3 THEN ASSIGN xFormType = 3.   /* deposit */
        WHEN 4 THEN ASSIGN xFormType = 4.   /* adv. payment */ 
        OTHERWISE   ASSIGN xFormType = 0.
        END CASE. 
    END.

    lcInvForm = fEplForm(xFormType).

    /* get the appropiate bank accounts according to Form Type */
    ASSIGN lcBarCodeBank = ""
           lcBank        = ""
           liCount       = 0.
    
    /* for bank-transfer */
    FOR EACH ttBankAcc WHERE 
             ttBankAcc.EPLForm = lcInvForm
    BY ttBankAcc.BankAccount:
    
       liCount = liCount + 1.
       IF liCount > 3 THEN LEAVE.
       
       lcBank[liCount] = ttBankAcc.BankAccount.
    END.
    
    /* for bar code */
    FOR FIRST ttBankAcc WHERE
              ttBankAcc.BarCode = lcInvForm:
       lcBarCodeBank = ttBankAcc.BankData.
    END.
 
END FUNCTION.

FUNCTION fVatUsageTitle RETURNS CHARACTER
   (iiVatUsage AS INT).
   
   IF iiVatUsage >= 3 AND
      iiVatUsage <= 4
   THEN RETURN fTMSCodeName("Invoice",
                            "VatUsage",
                            STRING(iiVatUsage)).
   ELSE RETURN "". 
   
END FUNCTION.

FUNCTION fPrintSpecRep RETURNS LOGICAL
   (icRepCodes    AS CHAR,
    icCLI         AS CHAR,
    iiTarget      AS INT,  /* 2=local 1=epl */
    iiLetterClass AS INT).

   DEF VAR liRepNum AS INT NO-UNDO. 
   DEF VAR liDumCnt AS INT NO-UNDO. 
    
   DO liRepNum = 3 TO 4:
    
      IF index(icRepCodes,string(liRepNum)) > 0 THEN DO:

         case liRepNum:
         when 3 THEN 
            RUN Inv/nnpura3.p (Invoice.CustNum,
                         Invoice.FirstCall,
                         Invoice.ToDate,
                         1,
                         Invoice.InvNum,
                         icCLI,
                         iiTarget,
                         iiLetterClass,
                         OUTPUT liDumCnt).

         WHEN 4 THEN
            RUN Inv/nnpura4.p (Invoice.CustNum,
                         Invoice.CustNum,
                         Invoice.FirstCall,
                         Invoice.ToDate,
                         1,                        /* invoiced */
                         Invoice.InvNum,
                         FALSE,                    /* not full b-numbers */
                         icCLI,                    /* cli */
                         iiTarget,                 /* epl/local */
                         0,                        /* not mailed */ 
                         IF iiTarget = 2 
                         THEN "local"
                         ELSE IF iiTarget = 1
                              THEN "epl"
                              ELSE "",
                         iiLetterClass,     
                         OUTPUT liDumCnt,       /* counters are irrelevant */
                         OUTPUT liDumCnt,
                         OUTPUT liDumCnt).
                         
         END CASE.
         
      END.
   END.
     
END FUNCTION.


