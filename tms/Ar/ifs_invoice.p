/* ----------------------------------------------------------------------
  MODULE .......: ifs_invoice.p
  TASK .........: Create a dump file for IFS from Invoices
  APPLICATION ..: tms
  AUTHOR .......: aam 
  CREATED ......: 28.05.09
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Func/finvbal.i}
{Syst/tmsconst.i}
{Func/wayofpayment.i}
{Func/multitenantfunc.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

/* YOT-4132 Translate the billing items to dump file in case a credit note is created */
DEF VAR lcbillcodes_from_set1 AS CHAR NO-UNDO INIT "PAYTERM,PAYTERM18,PAYTERM1E,PAYTERM24,PAYTERMBS,PAYTERMEND,PAYTERMEND1E,PAYTERMENDBS,PAYTERMBC,PAYTERMENDBC".
DEF VAR lcbillcodes_to_set1   AS CHAR NO-UNDO INIT "CNPAYTERM".
DEF VAR lcbillcodes_from_set2 AS CHAR NO-UNDO INIT "RVTERM1EF,RVTERMBSF,RVTERMF,RVTERMBCF".
DEF VAR lcbillcodes_to_set2   AS CHAR NO-UNDO INIT "CNRV".
DEF VAR lcbillcodes_from_set3 AS CHAR NO-UNDO INIT "RVTERM,RVTERM1E,RVTERMBS,RVTERMEND,RVTERMEND1E,RVTERMENDBS,RVTERMBC,RVTERMENDBC".
DEF VAR lcbillcodes_to_set3   AS CHAR NO-UNDO INIT "CNRVTERM".
DEF VAR lcbillcodes_from_set4 AS CHAR NO-UNDO INIT "RVTERMDTTR,RVTERMDTRW,RVTERMDTEQ25,RVTERMDTTD".
DEF VAR lcbillcodes_to_set4   AS CHAR NO-UNDO INIT "CRVTERMDT".

DEF VAR liCnt         AS INT    NO-UNDO.
DEF VAR ldVATTot      AS DEC    NO-UNDO.
DEF VAR ldVATAmt      AS DEC    NO-UNDO.
DEF VAR ldaFrom       AS DATE   NO-UNDO.
DEF VAR ldaDate       AS DATE   NO-UNDO.
DEF VAR ldBalance     AS DEC    NO-UNDO.
DEF VAR lcInvoiceTZ   AS CHAR   NO-UNDO. 
DEF VAR lcTaxZones    AS CHAR   NO-UNDO.
DEF VAR lcSkipSlsCode AS CHAR   NO-UNDO.
DEF VAR lcDelimiter   AS CHAR   NO-UNDO INIT ";".
DEF VAR liItemID      AS INT    NO-UNDO.
DEF VAR liRowID       AS INT    NO-UNDO.
DEF VAR lcInvoiceType AS CHAR   NO-UNDO.
DEF VAR lcPaymMethod  AS CHAR   NO-UNDO.
DEF VAR lcSeriesRef   AS CHAR   NO-UNDO.
DEF VAR lcNumberRef   AS CHAR   NO-UNDO.
DEF VAR liInvNum      AS INT    NO-UNDO.
DEF VAR ldFromPeriod  AS DEC    NO-UNDO.
DEF VAR ldToPeriod    AS DEC    NO-UNDO.
DEF VAR lcPayType     AS CHAR   NO-UNDO.
DEF VAR lcSegment     AS CHAR   NO-UNDO.
DEF VAR lcPickType    AS CHAR   NO-UNDO.
DEF VAR lcFusionDelType AS CHAR NO-UNDO. 
DEF VAR liDeliveryTo  AS INT    NO-UNDO.
DEF VAR llSalesInv    AS LOG    NO-UNDO.
DEF VAR lcSalesProd   AS CHAR   NO-UNDO.
DEF VAR lcSalesman    AS CHAR   NO-UNDO. 
DEF VAR lcNCFRef      AS CHAR   NO-UNDO.
DEF VAR lcOrderId     AS CHAR   NO-UNDO.
DEF VAR ldOperationDate AS DATE NO-UNDO.
DEF VAR ldOperationTime AS INT  NO-UNDO.
DEF VAR ldeInstallmentAmt AS DEC NO-UNDO. 
DEF VAR liInstallmentQty AS INT NO-UNDO. 
DEF VAR ldeInstallmentVAT AS DEC NO-UNDO. 
DEF VAR lcPayTermBillCode AS CHAR NO-UNDO.
DEF VAR llFusion AS LOG NO-UNDO.
DEF VAR ldeResidualAmount AS DEC NO-UNDO.
DEF VAR ldeResidualAmountVAT AS DEC NO-UNDO.
DEF VAR llPayPal AS LOG NO-UNDO.

DEF TEMP-TABLE ttRow NO-UNDO
   LIKE InvRow
   FIELD ItemID      AS INT
   FIELD TaxCode     AS CHAR
   FIELD AmtExclVat  AS DEC
   FIELD ProductCode AS CHAR
   FIELD Operator    AS CHAR
   FIELD PayType     AS CHAR
   FIELD Segment     AS CHAR
   FIELD SlsChannel  AS CHAR 
   FIELD MsSeq       AS INT 
   FIELD Quantity    AS CHAR
   FIELD BankCode    AS CHAR
   INDEX ItemID ItemID
   INDEX BillCode MsSeq BillCode.
   
DEF TEMP-TABLE ttTax NO-UNDO
   FIELD TaxCode   AS CHAR
   FIELD TaxPerc   AS DEC
   FIELD TaxBase   AS DEC
   FIELD TaxAmount AS DEC
   FIELD ItemID    AS INT
   INDEX TaxPerc TaxPerc.

DEF TEMP-TABLE ttSub NO-UNDO
   FIELD SubInvNum  AS INT
   FIELD PayType    AS CHAR
   FIELD Salesman   AS CHAR
   FIELD SlsChannel AS CHAR 
   INDEX SubInvNum SubInvNum.
   
DEF BUFFER bInv      FOR Invoice.
DEF BUFFER bCredited FOR Invoice.
DEF BUFFER bttRow FOR ttRow.
   
DEFINE STREAM sLog.


FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   DO TRANS:
      CREATE ErrorLog.
      ASSIGN ErrorLog.Brand     = Syst.Var:gcBrand
             ErrorLog.ActionID  = "IFSDUMP"
             ErrorLog.TableName = "Invoice"
             ErrorLog.KeyValue  = STRING(Invoice.InvNum)
             ErrorLog.ErrorChar = Invoice.ExtInvID
             ErrorLog.ErrorMsg  = icMessage
             ErrorLog.UserCode  = Syst.Var:katun.
             ErrorLog.ActionTS  = Func.Common:mMakeTS().
   END.
   
END FUNCTION.

FUNCTION fDispDecimal RETURNS CHAR
   (idAmount AS DEC):

   RETURN TRIM(REPLACE(STRING(idAmount,"->>>>>>>>>9.99"),",",".")).
      
END FUNCTION.

FUNCTION fDate2String RETURNS CHAR
   (idaDate AS DATE):
   
   IF idaDate = ? THEN RETURN "".
   
   RETURN STRING(YEAR(idaDate),"9999") +
          STRING(MONTH(idaDate),"99")  +
          STRING(DAY(idaDate),"99").
   
END FUNCTION.

FUNCTION fPrintHeader RETURNS LOGIC:

   PUT STREAM sLog UNFORMATTED 
      "H"                             lcDelimiter  /*  1: line_type */
      Invoice.CustNum                 lcDelimiter  /*  2: identity  */
      Invoice.ExtInvID                lcDelimiter  /*  3: invoice_no */
      fDate2String(Invoice.InvDate)   lcDelimiter  /*  4: transaction_date */
      lcInvoiceType                   lcDelimiter  /*  5: invoice_type */
      fDate2String(Invoice.InvDate)   lcDelimiter  /*  6: invoice_date */
      fDate2String(Invoice.DueDate)   lcDelimiter  /*  7: due_date */
      lcPaymMethod                    lcDelimiter  /*  8: pay_term_id */
      Invoice.Currency                lcDelimiter  /*  9: currency */
      Invoice.ExchRate                lcDelimiter  /* 10: curr_rate */
      "1"                             lcDelimiter  /* 11: div_factor */
      lcPaymMethod                    lcDelimiter  /* 12: way_pay_id */
      lcNCFRef                        lcDelimiter  /* 13: ncf_reference */
      lcSeriesRef                     lcDelimiter  /* 14: series_reference */
      lcNumberRef                     lcDelimiter  /* 15: number_reference */
      lcOrderId                       lcDelimiter  /* 16: Order Id */
      fDate2String(ldOperationDate)   lcDelimiter  /* 17: Operation Date */
      fgetCompanyId()                 lcDelimiter  /* 18: Company ID */
      SKIP.
       
END FUNCTION.

FUNCTION fPrintItem RETURNS LOGIC:

   PUT STREAM sLog UNFORMATTED 
      "I"                             lcDelimiter  /*  1: line_type */
      Invoice.ExtInvID                lcDelimiter  /*  2: invoice_no */
      ttTax.ItemID                    lcDelimiter  /*  3: item_id */
      ttTax.TaxCode                   lcDelimiter  /*  4: vat_code */
      fDispDecimal(ttTax.TaxBase)     lcDelimiter  /*  5: net_curr_amount */
      fDispDecimal(ttTax.TaxBase)     lcDelimiter  /*  6: net_dom_amount */
      fDispDecimal(ttTax.TaxAmount)   lcDelimiter  /*  7: vat_curr_amount */
      fDispDecimal(ttTax.TaxAmount)   lcDelimiter  /*  8: vat_dom_amount */
      SKIP.
      
END FUNCTION.

FUNCTION fPrintPosting RETURNS LOGIC:

   PUT STREAM sLog UNFORMATTED 
      "P"                                 lcDelimiter  /*  1: line_type */
      Invoice.ExtInvID                    lcDelimiter  /*  2: invoice_no */
      ttRow.ItemID                        lcDelimiter  /*  3: item_id */
      liRowID                             lcDelimiter  /*  4: row_id */
      ttRow.SlsAcc                        lcDelimiter  /*  5: code_a */
      ttRow.CostCentre                    lcDelimiter  /*  6: code_b */
      ttRow.ProductCode                   lcDelimiter  /*  7: code_c */
      ttRow.PayType                       lcDelimiter  /*  8: code_d */
      ttRow.Segment                       lcDelimiter  /*  9: code_e */
      ttRow.Operator                      lcDelimiter  /* 10: code_f */
      ttRow.SlsChannel                    lcDelimiter  /* 11: code_g */
      fDispDecimal(-1 * ttRow.AmtExclVat) lcDelimiter  /* 12: curr_amount */
      fDispDecimal(-1 * ttRow.AmtExclVat) lcDelimiter  /* 13: dom_amount */
      ttRow.TaxCode                       lcDelimiter  /* 14: optional_code */
      ttRow.MsSeq                         lcDelimiter  /* 15: subcription_id */
      ttRow.Quantity                      lcDelimiter  /* 16: quantity */
      ttRow.BillCode                      lcDelimiter  /* 17: billing code */
      ttRow.BankCode                      lcDelimiter  /* 18: sales invoice - Residual Amount service invoice - bank code */
      (IF ttRow.OrderId EQ -1 THEN "MANUAL"
       ELSE IF ttRow.OrderId EQ 0 THEN "" 
       ELSE STRING(ttRow.OrderId))        lcDelimiter  /* 19: order id */
      SKIP.
       
END FUNCTION.


FUNCTION fTaxCode RETURNS CHAR
  (icTaxZone AS CHAR,
   idPercent AS DEC):

   /* 
    TMS InvGroups/taxzones
    InvGroup TZ Group Name                              
    IGIC1    2  Canary Islands 1                        
    IPSIC1   3  Ceuta 1                                 
    IPSIM1   4  Melilla 1                               
    UNKNOWN  -  Unknown Pre paid Customer               
    VAT1     1  Mainland 1                              

    SAP taxcodes:
    Type of Taxes | RMCA tax code
    IPSI Rep. exento Ceuta 0%          |CC
    IPSI repercutido  Ceuta 3%         |CR
    IPSI Ceuta 8%                      |C8
    IPSI Ceuta 10%                     |C1
    IPSI Rep. exento Melilla 0%        |MC
    IPSI repercutido  Melilla 4%       |MR
    IPSI repercutido  Melilla 8%       |MU
    IVA repercutido exento 0%          |FC
    IVA repercutido 4%                 |R1
    IVA repercutido 7%                 |R2
    IVA repercutido 16%                |F3
    IGIC repercutido 5%                |RB
    IGIC repercutido exento 0%         |RC
    IGIC repercutido 13%               |RI
    IGIC repercutido 9%                |RR
    IGIC repercutido 2%                |RS
   */

   CASE icTaxZone:
   
   WHEN "1" THEN DO:
      CASE idPercent:
      WHEN 0.0 THEN RETURN "FC".
      WHEN 4.0 THEN RETURN "R1".
      WHEN 7.0 THEN RETURN "R2".
      WHEN 16.0 THEN RETURN "F3".
      WHEN 18.0 THEN RETURN "F4".
      WHEN 21.0 THEN RETURN "F5".
      END CASE.
   END.
   
   WHEN "2" THEN DO:
      CASE idPercent:
      WHEN 0.0 THEN RETURN "RC".
      WHEN 2.0 THEN RETURN "RS".
      WHEN 3.0 THEN RETURN "RA".
      WHEN 5.0 THEN RETURN "RB".
      WHEN 7.0 THEN RETURN "RD".
      WHEN 9.0 THEN RETURN "RR".
      WHEN 13.0 THEN RETURN "RI".
      END CASE.
   END.
   
   WHEN "3" THEN DO:
      CASE idPercent:
      WHEN 0.0 THEN RETURN "CC".
      WHEN 3.0 THEN RETURN "CR".
      WHEN 8.0 THEN RETURN "C8".
      WHEN 10.0 THEN RETURN "C1".
      END CASE.
   END.
   
   WHEN "4" THEN DO:
      CASE idPercent:
      WHEN 0.0 THEN RETURN "MC".
      WHEN 4.0 THEN RETURN "MR".
      WHEN 8.0 THEN RETURN "MU".
      END CASE.
   END.
   
   END CASE.

   RETURN "ERROR".

END FUNCTION.

FUNCTION fGetTaxZone RETURNS CHAR
  (icTaxZone  AS CHAR,
   icInvGroup AS CHAR):

   IF LOOKUP(icTaxZone,lcTaxZones) > 0 THEN RETURN icTaxZone.
   
   FIND FIRST InvGroup WHERE 
              InvGroup.Brand    = Syst.Var:gcBrand AND
              InvGroup.InvGroup = icInvGroup NO-LOCK NO-ERROR.
   IF AVAILABLE InvGroup 
   THEN RETURN InvGroup.TaxZone.
   ELSE RETURN "1".

END FUNCTION.

FUNCTION fGetInvoiceTax RETURNS CHAR
  (icTaxZone  AS CHAR,
   idVATAmt   AS DEC EXTENT 10,
   idPercent  AS DEC EXTENT 10):

   DEF VAR liVAT     AS INT  NO-UNDO.
   DEF VAR ldPercent AS INT  NO-UNDO. 

   ldPercent = 0.
   DO liVAT = 1 TO 5:
      IF idPercent[liVAT] NE 0 THEN DO:
         ldPercent = idPercent[liVAT].
         LEAVE.
      END.
   END.
   
   RETURN fTaxCode(icTaxZone,
                   ldPercent).
            
END FUNCTION.


/******* MAIN start *********/

OUTPUT STREAM sLog TO VALUE(icFile).

ASSIGN
   lcSkipSlsCode = fCParamC("SalesInvSkipProdCode")
   lcFusionDelType = SUBST("&1,&2",
               {&INV_DEL_TYPE_FUSION_EMAIL},
               {&INV_DEL_TYPE_FUSION_EMAIL_PENDING}).

IF icDumpMode = "modified" THEN ASSIGN
   ldaFrom      = (TODAY - 15)
   liDeliveryTo = 1.
ELSE ASSIGN
   ldaFrom      = 7/1/9
   /* full dump takes also previously dumped  */
   liDeliveryTo = 99.

FOR EACH TaxZone NO-LOCK:
   lcTaxZones = lcTaxZones + (IF lcTaxZones > "" THEN "," ELSE "") +
                TaxZone.TaxZone.
END. 

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcPickType = ENTRY(1,DumpFile.ConfigParam,";").
   IF NUM-ENTRIES(DumpFile.ConfigParam,";") > 1 THEN ASSIGN
      llFusion = (TRIM(ENTRY(2,DumpFile.ConfigParam,";")) EQ "fusion")
      llPayPal = (TRIM(ENTRY(2,DumpFile.ConfigParam,";")) EQ "paypal").
END.

DAYLOOP:
DO ldaDate = TODAY TO ldaFrom BY -1:

   InvoiceLoop:
   FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
            Invoice.Brand   = Syst.Var:gcBrand   AND
            Invoice.InvDate = ldaDate   AND
            Invoice.InvType NE 99       AND
            Invoice.ExtInvID > ""       AND
            Invoice.DeliveryState > 0   AND
            Invoice.DeliveryState <= liDeliveryTo
      BY Invoice.InvNum
      ON QUIT UNDO, RETRY
      ON STOP UNDO, RETRY:

   IF RETRY THEN DO:
      olInterrupted = TRUE.
      LEAVE.
   END.

   ASSIGN lcSeriesRef = ""
          lcNumberRef = ""
          lcNCFRef    = ""
          lcOrderId   = ""
          ldOperationDate = ?.

   IF lcPickType > "" AND
      LOOKUP(STRING(Invoice.InvType),lcPickType) = 0 THEN NEXT.

   /* IFSInvoiceSA6 Dump Config (ConfigParam = "7,9;paypal")
      collect only PayPal invoices/credit notes and exclude 
      them from IFSInvoiceSA7 (ConfigParam = "7,9") */
   IF NOT llPayPal AND Invoice.ChargeType = 6 THEN NEXT.
   ELSE IF llPayPal AND Invoice.ChargeType <> 6 THEN NEXT.

   IF llFusion THEN DO:
      IF LOOKUP(STRING(Invoice.DelType),lcFusionDelType) = 0 THEN NEXT.
   END.
   ELSE IF LOOKUP(STRING(Invoice.DelType),lcFusionDelType) > 0 THEN NEXT.

   IF icDumpMode = "Full" THEN DO:
      ldBalance = fCalcInvBal(BUFFER Invoice, 
                              TODAY,
                              FALSE).
      /* only unpaid invoices and new 0-amount invoices to control data,
         payments are handled in ifs so if invoice is paid it must have 
         been transferred to ifs */
      IF ldBalance = 0 AND 
         (Invoice.InvAmt NE 0 OR Invoice.InvDate < TODAY - 35) 
      THEN NEXT.
   END.
   
   FIND FIRST Customer NO-LOCK WHERE Customer.CustNum = Invoice.CustNum 
      NO-ERROR.
   IF NOT AVAIL Customer THEN DO:
      fError("Customer not available for invoice " + 
             STRING(Invoice.InvNum)).
      NEXT InvoiceLoop.
   END.

   ASSIGN
      llSalesInv  = (Invoice.InvType >= 6 AND Invoice.InvType <= 9)
      lcSalesProd = "037".
 
   /* customer segment */
   CASE Customer.Category:
      WHEN "10" OR
      WHEN "40" THEN lcSegment = "10".
      WHEN "11" OR
      WHEN "13" OR
      WHEN "41" THEN lcSegment = "11".
      WHEN "42" OR
      WHEN "43" OR
      WHEN "44" OR
      WHEN "45" THEN lcSegment = "12".
      WHEN "20" THEN lcSegment = "20".
      WHEN "21" THEN lcSegment = "21".
      WHEN "22" OR
      WHEN "23" THEN lcSegment = "22".
      WHEN "30" THEN lcSegment = "30".
      WHEN "31" THEN lcSegment = "31".
      OTHERWISE lcSegment = "99".
   END CASE.
   
   ASSIGN
      lcSalesman   = "XX"
      ldFromPeriod = Func.Common:mMake2DT(Invoice.FromDate,0)
      ldToPeriod   = Func.Common:mMake2DT(Invoice.ToDate,86399).
  
   EMPTY TEMP-TABLE ttSub.
   
   IF llSalesInv THEN DO:
      
      IF Invoice.InvType <= 7 THEN liInvNum = Invoice.InvNum.
      ELSE liInvNum = Invoice.CrInvNum.
      
      FOR FIRST Order NO-LOCK WHERE
                Order.InvNum = liInvNum:

         lcSalesman = Order.Salesman.
         
         IF Order.OrderType = 2 THEN lcSalesProd = "050".
         ELSE IF Order.MnpStatus > 0 THEN lcSalesProd = "038".

         IF Order.OrderType = 2 THEN lcPayType = "50".  /* renove */
         ELSE DO:
            /* ifs-paytype should be configured to clitype */
            CASE Order.CliType:
            WHEN "CONTRD1" OR 
            WHEN "CONTRD2" OR
            WHEN "CONTRD3" OR
            WHEN "CONTRD" OR
            WHEN "CONTD" THEN lcPayType = "30".
            WHEN "TARJRD1" THEN lcPayType = "40".
            OTHERWISE DO:
               /* YOT-5126 Convergent, Fixed and Additional line */
               IF fIsFixedOnly(Order.CliType) THEN DO: /* FIXED DSL/TFH */
                  IF Order.CliType BEGINS "CONTDSL" THEN lcPayType = "62".
                  ELSE lcPayType = "63". /* CONTTFH */
               END.
               ELSE IF fIsConvergentORFixedOnly(Order.CliType) THEN DO:
                  IF Order.CliType BEGINS "CONTDSL" THEN lcPayType = "60".
                  ELSE lcPayType = "61". /* Convergent CONTTFH */
               END.
               ELSE IF (fIsAddLineOrder(Order.OrderID) OR fCLITypeIsExtraLine(Order.CLIType)) AND
                  NOT Order.PayType THEN DO:
                  /* YOT-5618 Handle correctly Way of payment for 66 and 67 */
                  lcPayType = fGetPayType(Order.CustNum).
               END.
               ELSE IF Order.PayType THEN lcPayType = "10".
               ELSE lcPayType = "20".
            END.
            END CASE.
         END.
      END.

      FOR FIRST SubInvoice OF Invoice NO-LOCK:
         CREATE ttSub.
         ASSIGN
            ttSub.SubInvNum = SubInvoice.SubInvNum
            ttSub.PayType   = lcPayType
            ttSub.Salesman  = lcSalesman.
      END.      
   END.

   /* service invoices */
   ELSE DO:
      FOR EACH SubInvoice OF Invoice NO-LOCK:

         lcSalesman = "XX".
         FOR FIRST Order NO-LOCK WHERE
                   Order.MsSeq = SubInvoice.MsSeq AND
                   Order.StatusCode = "6"  AND
                   Order.OrderType  < 2:
            lcSalesman = Order.Salesman.
         END.

         /*  Way of Payment value YDR-2883 */
         lcPayType = fIfsWayOfPayment(SubInvoice.CLI,
                                      SubInvoice.MsSeq,
                                      ldFromPeriod,
                                      ldToPeriod).

         CREATE ttSub.
         ASSIGN
            ttSub.SubInvNum = SubInvoice.SubInvNum
            ttSub.PayType   = lcPayType
            ttSub.Salesman  = lcSalesman.
      END.
   END.
      
   IF NOT CAN-FIND(FIRST ttSub) THEN DO:
      fError("Subinvoice is missing").
      NEXT InvoiceLoop.                   
   END.
     
   /* customer source (channel) */
   FOR EACH ttSub:

      ttSub.SlsChannel = "99".
   
      FIND FIRST TMSCodes WHERE 
                 TMSCodes.TableName = "Salesman" AND
                 TMSCodes.FieldName = "SmPrefix" AND
                 TMSCodes.CodeValue = SUBSTRING(ttSub.Salesman,1,2)
         NO-LOCK NO-ERROR.
      IF AVAILABLE TMSCodes THEN ttSub.SlsChannel = TMSCodes.ConfigValue.

      /* only three values for sales invoices */
      IF llSalesInv AND LOOKUP(ttSub.SlsChannel,"14,15,99") = 0 THEN 
         ttSub.SlsChannel = "14".
   END.
   
   /* payment method */
   CASE Invoice.ChargeType:             
   WHEN 1 THEN lcPaymMethod = "2".
   WHEN 2 THEN lcPaymMethod = "1".
   WHEN 3 THEN lcPaymMethod = "7".
   WHEN 5 THEN lcPaymMethod = "3". /* Bank Transfer */
   WHEN {&INV_CHARGE_T_PAYPAL} THEN /*6*/
               lcPaymMethod = {&PAYM_METHOD_PAYPAL}. /* Paypal "6" */ 
   OTHERWISE lcPaymMethod = "1".
   END CASE.

   /* Document type */
   CASE Invoice.InvType:
   WHEN {&INV_TYPE_NORMAL}    THEN lcInvoiceType = "10".
   WHEN 5                     THEN lcInvoiceType = "15".
   WHEN {&INV_TYPE_CASH}      THEN lcInvoiceType = "70".
   WHEN {&INV_TYPE_RECEIPT}   THEN lcInvoiceType = "70". /* card, paypal*/
   WHEN 8                     THEN lcInvoiceType = "15". 
   WHEN 9                     THEN lcInvoiceType = "15".
   OTHERWISE                       lcInvoiceType = "10".
   END CASE.

   IF lcInvoiceType = "15" THEN DO:
      lcSeriesRef = "CI".
      
      FIND FIRST bCredited WHERE bCredited.InvNum = Invoice.CrInvNum
         NO-LOCK NO-ERROR.
      IF AVAILABLE bCredited THEN lcNumberRef = bCredited.ExtInvId.
   END. /* IF lcInvoiceType = "15" THEN DO: */

   /* NPM Changes */
   IF lcPaymMethod = "1" THEN lcNCFRef = "GROUP".

   IF (lcPaymMethod = "2" OR 
       lcPaymMethod = "7" OR 
       lcPaymMethod = {&PAYM_METHOD_PAYPAL} ) AND
      lcInvoiceType <> "15" THEN DO:
      IF (Invoice.InvType = 6 OR Invoice.InvType = 7) THEN
         liInvNum = Invoice.InvNum.
      ELSE liInvNum = Invoice.CrInvNum.

      FOR FIRST Order WHERE
                Order.InvNum = liInvNum NO-LOCK,
          FIRST OrderPayment WHERE
                OrderPayment.Brand   = Syst.Var:gcBrand AND
                OrderPayment.OrderId = Order.OrderId NO-LOCK:
          ASSIGN lcNCFRef = OrderPayment.BinNumber + OrderPayment.AuthNumber
                 lcNumberRef = OrderPayment.CCReference
                 lcOrderId   = STRING(Order.OrderId).

          FIND FIRST EventLog WHERE
                     EventLog.TableName = "Order" AND
                     EventLog.Key = "1" + CHR(255) + STRING(Order.OrderId) AND
                     EventLog.Action = "Create" NO-LOCK NO-ERROR.
          IF AVAILABLE EventLog THEN
             ldOperationDate = EventLog.EventDate.

          IF ldOperationDate = ? THEN
             Func.Common:mSplitTS(Order.CrStamp,
                      OUTPUT ldOperationDate,
                      OUTPUT ldOperationTime).

          IF lcPaymMethod = "2" THEN
             ASSIGN lcNumberRef = ""
                    lcNCFRef    = "".
          IF lcPaymMethod EQ {&PAYM_METHOD_PAYPAL} THEN 
             lcNCFRef = OrderPayment.CCNumber.

      END. /* FOR FIRST Order NO-LOCK WHERE */
   END. /* ELSE IF lcPaymMethod = "7" AND lcInvoiceType <> "15" THEN DO: */

   ldVATTot = 0.
   DO liCnt = 1 TO 5:
      ldVATTot = ldVATTot + Invoice.VATAmount[liCnt].
   END.   

   lcInvoiceTZ = fGetTaxZone(Invoice.TaxZone,Invoice.InvGroup).

   IF lcInvoiceTZ = "" THEN DO:
      fError("Invalid tax zone").
      NEXT InvoiceLoop.                   
   END.
   
   EMPTY TEMP-TABLE ttRow.
   EMPTY TEMP-TABLE ttTax.
   
   ASSIGN
      liItemID = 0
      liRowID  = 0
      liInstallmentQty  = 0
      ldeInstallmentAmt = 0
      ldeResidualAmount = 0
      lcPayTermBillCode = "".
      
   IF llSalesInv THEN
   FOR FIRST Order NO-LOCK WHERE
             Order.InvNum = liInvNum,
        EACH OfferItem NO-LOCK WHERE
             OfferItem.Brand = Syst.Var:gcBrand AND
             OfferItem.Offer = Order.Offer AND
             OfferItem.BeginStamp <= Order.CrStamp AND
             OfferItem.EndStamp >= Order.CrStamp AND
             OfferItem.ItemType = "PerContract", 
       FIRST DayCampaign NO-LOCK WHERE
             DayCampaign.Brand = Syst.Var:gcBrand AND
             DayCampaign.DCEvent = OfferItem.ItemKey AND
             DayCampaign.DCType = {&DCTYPE_INSTALLMENT},
       FIRST FMItem NO-LOCK WHERE
             FMItem.Brand = Syst.Var:gcBrand AND
             FMItem.FeeModel = DayCampaign.FeeModel:

       ASSIGN
         liInstallmentQty  = FMItem.FFItemQty
         ldeInstallmentAmt = FMItem.Amount * FMItem.FFItemQty
         lcPayTermBillCode = FMItem.BillCode.

       IF OfferItem.Amount > 0 THEN
         ldeResidualAmount = OfferItem.Amount.

       /* check if credit invoice */
       IF NOT (Invoice.InvType = 6 OR Invoice.InvType = 7) THEN
          ASSIGN ldeInstallmentAmt = -1 * ldeInstallmentAmt
                 ldeResidualAmount = -1 * ldeResidualAmount.
       LEAVE.
   END.
      
   /* copy rows to temp-table and count tax for them */
   FOR EACH InvRow NO-LOCK WHERE
            InvRow.InvNum = Invoice.InvNum,
      FIRST SubInvoice NO-LOCK WHERE
            SubInvoice.InvNum = Invoice.InvNum AND
            SubInvoice.SubInvNum = InvRow.SubInvNum,
      FIRST ttSub WHERE
            ttSub.SubInvNum = SubInvoice.SubInvNum
   BREAK BY InvRow.VATPerc
         BY ABS(InvRow.Amt):

      CREATE ttRow.
      BUFFER-COPY InvRow TO ttRow.
      ttRow.MsSeq = SubInvoice.MsSeq.
 
      ldVATAmt = 0.

      /* if VAT is included then remove it */
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

      ttRow.VatAmt = ldVatAmt.
      
      IF Invoice.VatIncl THEN 
         ttRow.AmtExclVat = ttRow.Amt - ttRow.VatAmt.
      ELSE ASSIGN
         ttRow.AmtExclVat = ttRow.Amt
         ttRow.Amt = ttRow.AmtExclVat + ttRow.VatAmt.
         
      FIND FIRST BillItem WHERE
                 BillItem.Brand    = Syst.Var:gcBrand AND
                 BillItem.BillCode = InvRow.BillCode NO-LOCK NO-ERROR.
                 
       IF NOT AVAILABLE BillItem THEN 
       DO:           
           fError(InvRow.BillCode + ": missing").
           NEXT InvoiceLoop.
       END.   
       FIND FIRST CCRule NO-LOCK WHERE 
                  CCRule.Brand      =   BillItem.Brand      AND 
                  CCRule.Category   =   "*"                 AND 
                  CCRule.BillCode   =   BillItem.BillCode   AND
                  CCRule.CLIType    =   ""                  AND 
                  CCRule.ValidTo    >=  TODAY NO-ERROR.

      IF llSalesInv THEN DO:
         IF LOOKUP(STRING(InvRow.SlsAcc),lcSkipSlsCode) > 0 THEN
            ttRow.ProductCode = "".
         ELSE ttRow.ProductCode = lcSalesProd.
         IF ttRow.ProductCode > "" THEN ttRow.Operator = "010".
      END.
      
      ELSE DO:         
         IF AVAILABLE CCRule AND CCRule.ReportingID > "" THEN DO:
            ASSIGN
               ttRow.Operator    = "010"
               ttRow.ProductCode = CCRule.ReportingID. 
         END.
      END.
       
      IF NOT CAN-FIND(FIRST Account WHERE
                            Account.Brand  = Syst.Var:gcBrand AND
                            Account.AccNum = InvRow.SlsAcc) THEN DO:
         fError(InvRow.BillCode + ": Invalid account").
         NEXT InvoiceLoop.                   
      END.

      IF LOOKUP(SUBSTRING(STRING(InvRow.SlsAcc),1,1),"6,7") > 0 THEN 
      ASSIGN          
         ttRow.CostCentre = ( IF AVAILABLE CCRule THEN CCRule.CostCentre ELSE "" )
         ttRow.PayType    = ttSub.PayType
         ttRow.Segment    = lcSegment
         ttRow.SlsChannel = ttSub.SlsChannel.
      
      FIND FIRST ttTax WHERE 
                 ttTax.TaxPerc = ttRow.VatPerc NO-ERROR.
      IF NOT AVAILABLE ttTax THEN DO:
         CREATE ttTax.
         ASSIGN
            liItemID      = liItemID + 1
            ttTax.ItemID  = liItemID
            ttTax.TaxPerc = ttRow.VatPerc.
            ttTax.TaxCode = fTaxCode(lcInvoiceTZ,ttRow.VatPerc).

         IF ttTax.TaxCode BEGINS "ERROR" THEN DO:
            fError("Invalid tax data on invoice " + STRING(Invoice.InvNum)).
            NEXT InvoiceLoop.
         END.
       
      END.
      
      /* YTS-3901 - Exclude Campaign invoice row from calculation */
      IF BillItem.BIGroup EQ {&BITEM_GRP_TERMINAL} AND
         llSalesInv AND liInstallmentQty > 0 AND
         NOT (BillItem.BillCode BEGINS "CPDISC") THEN DO:

         ASSIGN
            ldeInstallmentVAT = ROUND (ldeInstallmentAmt * InvRow.VATPerc /
                                      (100 + InvRow.VATPerc),2).
            ldeResidualAmountVAT = ROUND (ldeResidualAmount * InvRow.VATPerc /
                                      (100 + InvRow.VATPerc),2).

         ASSIGN
            ttRow.AmtExclVat = ttRow.AmtExclVat +
                               (ldeInstallmentAmt - ldeInstallmentVAT) +
                               (ldeResidualAmount - ldeResidualAmountVAT)
            ttRow.VatAmt = ttRow.VatAmt + ldeInstallmentVAT + ldeResidualAmountVAT.
      END.
      
      ASSIGN 
         ttRow.ItemID    = ttTax.ItemID
         ttRow.TaxCode   = ttTax.TaxCode
         ttTax.TaxBase   = ttTax.TaxBase   + ttRow.AmtExclVat
         ttTax.TaxAmount = ttTax.TaxAmount + ttRow.VatAmt.

      /* Merge payterm/rvterm rows. YDR-694 */
      IF NOT llSalesInv AND
         LOOKUP(ttRow.BillCode,
                "PAYTERM,PAYTERMEND," +
                {&TF_BANK_UNOE_PAYTERM_BILLCODES} + "," +
                {&TF_BANK_SABADELL_PAYTERM_BILLCODES} + "," +
                {&TF_BANK_CETELEM_PAYTERM_BILLCODES} + "," +
                "RVTERM,RVTERMEND," + 
                {&TF_BANK_UNOE_RVTERM_BILLCODES} + "," +
                {&TF_BANK_SABADELL_RVTERM_BILLCODES} + "," +
                {&TF_BANK_CETELEM_RVTERM_BILLCODES}) > 0 THEN DO: 

         FIND FIRST bttRow WHERE
                    bttRow.MsSeq = ttRow.MsSeq AND
                    bttRow.BillCode = ttRow.Billcode AND
                    bttRow.OrderId = ttRow.OrderID AND
              ROWID(bttRow) NE ROWID(ttRow) NO-ERROR.
         IF AVAIL bttRow THEN DO:
            ASSIGN
               bttRow.AmtExclVat = bttRow.AmtExclVat + ttRow.AmtExclVat
               bttRow.VatAmt = bttRow.VatAmt + ttRow.VatAmt.
            DELETE ttRow.
         END.
         ELSE IF 
            LOOKUP(ttRow.BillCode,{&TF_BANK_UNOE_PAYTERM_BILLCODES}) > 0 OR
            LOOKUP(ttRow.BillCode,{&TF_BANK_UNOE_RVTERM_BILLCODES}) > 0 
         THEN ttRow.BankCode = {&TF_BANK_UNOE}.
          ELSE IF 
            LOOKUP(ttRow.BillCode,{&TF_BANK_SABADELL_PAYTERM_BILLCODES}) > 0 OR
            LOOKUP(ttRow.BillCode,{&TF_BANK_SABADELL_RVTERM_BILLCODES}) > 0 
         THEN ttRow.BankCode = {&TF_BANK_SABADELL}.
         ELSE IF 
            LOOKUP(ttRow.BillCode,{&TF_BANK_CETELEM_PAYTERM_BILLCODES}) > 0 OR
            LOOKUP(ttRow.BillCode,{&TF_BANK_CETELEM_RVTERM_BILLCODES}) > 0 
         THEN ttRow.BankCode = {&TF_BANK_CETELEM}.
      END.

      IF NOT llSalesInv AND AVAIL ttRow THEN DO:
         IF ttRow.BillCode = "RVTERM1EF" THEN
            ttRow.BankCode = {&TF_BANK_UNOE}.
         ELSE IF ttRow.BillCode = "RVTERMBSF" THEN
            ttRow.BankCode = {&TF_BANK_SABADELL}.
         ELSE IF ttRow.BillCode = "RVTERMBCF" THEN
            ttRow.BankCode = {&TF_BANK_CETELEM}.
      END.

   END.
   
   /* sales invoice installment handling, YDR-328 */
   IF llSalesInv AND
      liInstallmentQty > 0 THEN DO:
         
         CREATE ttTax.
         ASSIGN
            liItemID      = liItemID + 1
            ttTax.ItemID  = liItemID
            ttTax.TaxPerc = 0.0
            ttTax.TaxCode = "NR"
            ttTax.TaxBase   = 0.0
            ttTax.TaxAmount = 0.0.

         CREATE ttRow.
         ASSIGN
            ttRow.ItemID = ttTax.ItemID
            ttRow.SlSAcc = 43008888
            ttRow.AmtExclVat = ldeInstallmentAmt 
            ttRow.TaxCode = ttTax.TaxCode 
            ttRow.MsSeq = Order.MsSeq
            ttRow.Quantity = STRING(liInstallmentQty)
            ttRow.BillCode = lcPayTermBillCode
            ttRow.BankCode = (IF ldeResidualAmount = 0 THEN ""
                              ELSE STRING(-1 * ldeResidualAmount)).
         
         CREATE ttRow.
         ASSIGN
            ttRow.ItemID = ttTax.ItemID
            ttRow.SlSAcc = 43008888
            ttRow.AmtExclVat = -1 * ldeInstallmentAmt 
            ttRow.TaxCode = ttTax.TaxCode 
            ttRow.MsSeq = Order.MsSeq
            ttRow.Quantity = STRING(liInstallmentQty)
            ttRow.BillCode = lcPayTermBillCode
            ttRow.BankCode = (IF ldeResidualAmount = 0 THEN ""
                              ELSE STRING(ldeResidualAmount)).
         
   END.

   oiEvents = oiEvents + 1.
   IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP oiEvents LABEL "Invoices" 
      WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
         TITLE " Collecting " FRAME fQty.
   END.
 
   fPrintHeader().

   /* tax line(s) */
   FOR EACH ttTax:
      fPrintItem().
   END.
 
   FOR EACH ttRow
   BY ttRow.ItemId
   BY ttRow.Amt:

      /* YOT-4132 billing code conversion for Credit Notes */
      IF lcInvoiceType = "15" THEN DO:
         
         IF LOOKUP(STRING(ttRow.BillCode),lcbillcodes_from_set1) > 0 THEN
            ASSIGN ttRow.BillCode = lcbillcodes_to_set1.

         IF LOOKUP(STRING(ttRow.BillCode),lcbillcodes_from_set2) > 0 THEN
            ASSIGN ttRow.BillCode = lcbillcodes_to_set2.

         IF LOOKUP(STRING(ttRow.BillCode),lcbillcodes_from_set3) > 0 THEN
            ASSIGN ttRow.BillCode = lcbillcodes_to_set3.

         IF LOOKUP(STRING(ttRow.BillCode),lcbillcodes_from_set4) > 0 THEN
            ASSIGN ttRow.BillCode = lcbillcodes_to_set4.

         /* If change was done then get other values from billing item */
         IF LOOKUP(STRING(ttRow.BillCode),lcbillcodes_to_set1 + "," +
                                          lcbillcodes_to_set2 + "," +
                                          lcbillcodes_to_set3 + "," +
                                          lcbillcodes_to_set4) > 0 THEN DO:
            FIND FIRST BillItem WHERE
                       BillItem.Brand    = Syst.Var:gcBrand AND
                       BillItem.BillCode = ttRow.BillCode NO-LOCK NO-ERROR.
                       
            IF AVAILABLE BillItem THEN DO:
               
               FIND FIRST CCRule NO-LOCK WHERE 
                          CCRule.Brand      =   BillItem.Brand      AND 
                          CCRule.Category   =   "*"                 AND  
                          CCRule.BillCode   =   BillItem.BillCode   AND
                          CCRule.CLIType    =   ""                  AND 
                          CCRule.ValidTo    >=  TODAY NO-ERROR.
                
               IF AVAILABLE CCRule       
               THEN ASSIGN ttRow.SlsAcc     = CCRule.AccNum /* Only used account number currently */
                           ttRow.CostCentre = CCRule.CostCentre.

               IF AVAILABLE CCRule    AND 
                   NOT llSalesInv     AND 
                   CCRule.ReportingID  > "" 
               THEN ASSIGN ttRow.Operator     =   "010"
                           ttRow.ProductCode  =   CCRule.ReportingID.
                      
            END.
            ELSE DO:
               fError(ttRow.BillCode + ": missing").
            END.
            
         END.
      END. /* IF lcInvoiceType = "15" THEN DO: */

      liRowID = liRowID + 1.
      fPrintPosting().
   END.   

   /* mark as transferred only when picking modified ones, full dump 
      (control) must not have any influence on daily dumps */
   IF icDumpMode = "modified" AND Invoice.DeliveryState = 1 THEN DO TRANS:
      FIND bInv WHERE RECID(bInv) = RECID(Invoice) EXCLUSIVE-LOCK.
      bInv.DeliveryState = 2.
      RELEASE bInv.
   END.
   
   END. /* Invoice loop */
END. /* DAYLOOP */

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
   
OUTPUT STREAM sLog CLOSE.


/******* MAIN end *********/

