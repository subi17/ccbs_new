/* ----------------------------------------------------------------------
  MODULE .......: taxvouchrep.p
  TASK .........: Print a tax report from payments
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: xx.12.06
  CHANGED ......: 02.02.07/aam taxzone through taxpercent
                  27.03.07/aam called from ui
  Version ......: yoigo
---------------------------------------------------------------------- */


{Syst/commali.i}
{Func/transname.i}
{Func/cparam2.i}
{Func/ftransdir.i}

DEF INPUT  PARAMETER icTaxZone   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icCustID    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiPaymType  AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtAccDate1 AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtAccDate2 AS DATE NO-UNDO.
DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiPaymCount AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocFileCount AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError     AS CHAR NO-UNDO.



DEF VAR lcFile     AS CHAR NO-UNDO.
DEF VAR ldtFrom    AS DATE NO-UNDO.
DEF VAR ldtTo      AS DATE NO-UNDO.
DEF VAR liType1    AS INT  NO-UNDO.
DEF VAR liType2    AS INT  NO-UNDO.
DEF VAR liQty      AS INT  NO-UNDO.
DEF VAR liCnt      AS INT  NO-UNDO.
DEF VAR lcZone     AS CHAR NO-UNDO. 
DEF VAR liInvNum   AS INT  NO-UNDO.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO.
DEF VAR lcTypeName AS CHAR NO-UNDO.

DEF TEMP-TABLE ttPaym NO-UNDO
   FIELD ExtInvID AS CHAR
   FIELD PaymType AS INT
   FIELD CustNum  AS INT
   FIELD CustName AS CHAR
   FIELD TaxZone  AS CHAR
   FIELD AccDate  AS DATE
   FIELD AmtVat0  AS DEC
   FIELD VatAmt   AS DEC
   FIELD VatPerc  AS DEC
   FIELD TotAmt   AS DEC
   INDEX TaxZone TaxZone PaymType ExtInvID.

DEF TEMP-TABLE ttTaxZone NO-UNDO
   FIELD TaxZone AS CHAR
   INDEX TaxZone TaxZone.
   
DEF STREAM sFile.

DEF BUFFER bAgrCust FOR Customer.


FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>,>>>,>>9.99")).
      
END FUNCTION.
      
FUNCTION fCollectPayment RETURNS LOGIC:
        
   DEF VAR lcTaxZone AS CHAR NO-UNDO.
   DEF VAR ldVatPerc AS DEC  NO-UNDO.

   IF iiPaymType > 0 AND Payment.PaymType NE iiPaymType THEN RETURN FALSE.
   
   IF LOOKUP(STRING(Payment.PaymType),"7,9,10,12") = 0 THEN RETURN FALSE.
   
   ASSIGN lcTaxZone = ""
          ldVatPerc = INTEGER(100 * Payment.Posting[3] /
                                    Payment.Posting[2]).
   
   FIND Region WHERE Region.Region = Customer.Region NO-LOCK NO-ERROR.
   IF AVAILABLE Region THEN lcTaxZone = Region.TaxZone.

   FOR FIRST VatCode NO-LOCK WHERE
             VatCode.TaxClass = "1" AND
             VatCode.VatPerc  = ldVatPerc:
      lcTaxZone = VatCode.TaxZone.
   END.

   IF icTaxZone > "" AND lcTaxZone NE icTaxZone THEN RETURN FALSE.
           
   CREATE ttPaym.
   ASSIGN ttPaym.ExtInvID   = Payment.ExtVoucher
          ttPaym.PaymType   = Payment.PaymType
          ttPaym.AccDate    = Payment.AccDate
          ttPaym.CustNum    = Payment.CustNum
          ttPaym.TaxZone    = lcTaxZone
          ttPaym.AmtVat0    = Payment.Posting[2] * -1
          ttPaym.VatAmt     = Payment.Posting[3] * -1
          ttPaym.TotAmt     = Payment.Posting[1]
          ttPaym.VatPerc    = ldVatPerc.
   
   /* temporary customer used */       
   IF Customer.CustNum < 1000 AND Payment.InvNum > 0 THEN DO:
      
      FIND FIRST Invoice WHERE
                 Invoice.InvNum = Payment.InvNum
      NO-LOCK NO-ERROR.
      
      /* credit invoice */
      IF AVAILABLE Invoice AND 
        (Invoice.InvType = 8 OR Invoice.InvType = 9) THEN DO:
         FIND FIRST Invoice WHERE
                    Invoice.InvNum = Payment.InvNum
         NO-LOCK NO-ERROR.
         liInvNum = Invoice.CrInvNum.
      END.
      ELSE liInvNum = Payment.InvNum.
      
      FOR FIRST Order NO-LOCK WHERE
                Order.InvNum = liInvNum,
          FIRST OrderCustomer OF Order NO-LOCK WHERE
                OrderCustomer.RowType = Order.InvCustRole:
                
         ttPaym.CustName = DYNAMIC-FUNCTION("fPrintOrderName" IN ghFunc1,
                                           BUFFER OrderCustomer).
      END.                                     

   END.        
          
   IF ttPaym.CustName = "" THEN         
      ttPaym.CustName   = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                          BUFFER Customer).

   
   IF NOT CAN-FIND(FIRST ttTaxZone WHERE ttTaxZone.TaxZone = ttPaym.TaxZone)
   THEN DO:
      CREATE ttTaxZone.
      ttTaxZone.TaxZone = ttPaym.TaxZone.
   END.
   
   liQty = liQty + 1.
   
   RETURN TRUE.
   
END FUNCTION.


ASSIGN 
   lcTransDir = fCParamC("TaxRepTransDir")
   lcSpoolDir = fCParamC("TaxRepSpoolDir").
   
IF lcTransDir = ? THEN lcTransDir = "".
IF lcSpoolDir = ? OR lcSpoolDir = "" THEN lcSpoolDir = "/tmp".


/* all invoice customers for chosen id */
IF icCustID > "" THEN 
FOR EACH bAgrCust NO-LOCK WHERE
         bAgrCust.Brand    = gcBrand AND
         bAgrCust.OrgID    = icCustID,
    EACH Customer NO-LOCK WHERE
         Customer.AgrCust = bAgrCust.CustNum,
    EACH Payment NO-LOCK USE-INDEX CustNum WHERE
         Payment.Brand    = gcBrand          AND
         Payment.CustNum  = Customer.CustNum AND
         Payment.AccDate >= idtAccDate1      AND
         Payment.AccDate <= idtAccDate2:  

   fCollectPayment().
END.
    
ELSE       
FOR EACH Payment NO-LOCK WHERE
         Payment.Brand     = gcBrand     AND
         Payment.AccDate  >= idtAccDate1 AND
         Payment.AccDate  <= idtAccDate2,
   FIRST Customer OF Payment NO-LOCK:
 
   fCollectPayment().

   IF NOT SESSION:BATCH AND liQty MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liQty  
              LABEL "Payment Qty" 
              FORMAT ">>>>>>>9"
           Payment.AccDate 
              LABEL "Acc. Date"
              FORMAT "99-99-99"
      WITH SIDE-LABELS 1 DOWN ROW 15 COL 55 OVERLAY TITLE " Collecting "
           FRAME fQty.
   END.

END.
     
IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
     
FOR EACH ttTaxZone:

   FIND TaxZone WHERE TaxZone.TaxZone = ttTaxZone.TaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN DO:
      lcZone = fTranslationName(gcBrand,
                                7,
                                TaxZone.TaxZone,
                                1,
                                idtAccDate2).
      
      IF lcZone = "" OR lcZone = ? THEN lcZone = TaxZone.TZName.
   END.
   ELSE lcZone = ttTaxZone.TaxZone.

   ASSIGN 
      lcFile = lcSpoolDir + "/" + icFile
      lcFile = REPLACE(lcFile,"#TAXZONE",ttTaxZone.TaxZone)
      lcFile = REPLACE(lcFile,"#TZNAME",lcZone)
      lcFile = REPLACE(lcFile,"#PERIOD",STRING(YEAR(idtAccDate1)) +
                                        STRING(MONTH(idtAccDate1))).
             
   OUTPUT STREAM sFile TO VALUE(lcFile).
   
   PUT STREAM sFile UNFORMATTED
      "Número de ticket"     CHR(9)
      "Fecha de ticket"      CHR(9)
      "Nombre de cliente"    CHR(9)
      "Base imponible"       CHR(9)
      "%"                    CHR(9)
      "Quota"                CHR(9)
      "Total"                SKIP.
      
   FOR EACH ttPaym WHERE
            ttPaym.TaxZone = ttTaxZone.TaxZone
   BREAK BY ttPaym.PaymType
         BY ttPaym.ExtInvId:
            
      PUT STREAM sFile UNFORMATTED
         ttPaym.ExtInvID               CHR(9)
         ttPaym.AccDate                CHR(9)
         ttPaym.CustName               CHR(9)
         fDispDecimal(ttPaym.AmtVat0)  CHR(9)
         fDispDecimal(ttPaym.VatPerc)  CHR(9)
         fDispDecimal(ttPaym.VatAmt)   CHR(9)
         fDispDecimal(ttPaym.TotAmt)   SKIP.

      ACCUMULATE ttPaym.AmtVat0 (TOTAL BY ttPaym.PaymType)
                 ttPaym.VatAmt  (TOTAL BY ttPaym.PaymType)
                 ttPaym.TotAmt  (TOTAL BY ttPaym.PaymType).
                 
      oiPaymCount = oiPaymCount + 1.   
      
      IF LAST-OF(ttPaym.PaymType) THEN DO:
      
         lcTypeName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "Payment",
                                       "PaymType",
                                       STRING(ttPaym.PaymType)).
         
         PUT STREAM sFile UNFORMATTED
            SKIP(1)
            lcTypeName " total"   CHR(9)
            CHR(9)
            CHR(9)
            fDispDecimal(ACCUM TOTAL BY ttPaym.PaymType ttPaym.AmtVat0) CHR(9)
            CHR(9)
            fDispDecimal(ACCUM TOTAL BY ttPaym.PaymType ttPaym.VatAmt)  CHR(9)
            fDispDecimal(ACCUM TOTAL BY ttPaym.PaymType ttPaym.TotAmt)  
            SKIP(1).
      END.
      
   END.         

   OUTPUT STREAM sFile CLOSE.

   ocFileCount = ocFileCount + 1.

   /* move the file to the transfer directory */
   IF lcTransDir > "" THEN DO:
      fTransDir(lcFile,
                ".txt",
                lcTransDir).
   END.
 
END.            

