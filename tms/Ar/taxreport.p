/* ----------------------------------------------------------------------
  MODULE .......: taxreport.p
  TASK .........: Print a tax report from invoices
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.12.06
  CHANGED ......: 27.03.07/aam called from ui
                  25.04.07/aam also upper limit for InvType
  Version ......: yoigo
---------------------------------------------------------------------- */


{commali.i}
{transname.i}
{cparam2.i}
{ftransdir.i}

DEF INPUT  PARAMETER icTaxZone   AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icCustID    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiInvType1  AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiInvType2  AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtInvDate1 AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtInvDate2 AS DATE NO-UNDO.
DEF INPUT  PARAMETER icFile      AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiInvCount  AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocFileCount AS INT  NO-UNDO.
DEF OUTPUT PARAMETER ocError     AS CHAR NO-UNDO.


DEF VAR lcFile     AS CHAR NO-UNDO.
DEF VAR liQty      AS INT  NO-UNDO.
DEF VAR liCnt      AS INT  NO-UNDO.
DEF VAR lcZone     AS CHAR NO-UNDO. 
DEF VAR liInvNum   AS INT  NO-UNDO.
DEF VAR lcSpoolDir AS CHAR NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO.
DEF VAR lcTypeName AS CHAR NO-UNDO. 

DEF TEMP-TABLE ttInv NO-UNDO
   FIELD ExtInvID AS CHAR
   FIELD InvType  AS INT
   FIELD CustNum  AS INT
   FIELD CustName AS CHAR
   FIELD TaxZone  AS CHAR
   FIELD InvDate  AS DATE
   FIELD AmtVat0  AS DEC
   FIELD VatAmt   AS DEC
   FIELD VatPerc  AS DEC
   FIELD TotAmt   AS DEC
   INDEX TaxZone TaxZone ExtInvID.

DEF TEMP-TABLE ttTaxZone NO-UNDO
   FIELD TaxZone AS CHAR
   INDEX TaxZone TaxZone.
   
DEF STREAM sFile.

DEF BUFFER bAgrCust FOR Customer.


FUNCTION fDispDecimal RETURNS CHARACTER
   (idAmt AS DEC):
   
   RETURN TRIM(STRING(idAmt,"->>>,>>>,>>9.99")).
      
END FUNCTION.
      
FUNCTION fCollectInvoice RETURNS LOGIC:
        
   DEF VAR lcTaxZone AS CHAR NO-UNDO.
   
   lcTaxZone = "".
   
   IF Invoice.TaxZone > "" THEN lcTaxZone = Invoice.TaxZone.
   ELSE DO: 
      IF Invoice.Region > "" THEN 
         FIND Region WHERE Region.Region = Invoice.Region NO-LOCK NO-ERROR.
      ELSE 
         FIND Region WHERE Region.Region = Customer.Region NO-LOCK NO-ERROR.
      IF AVAILABLE Region THEN lcTaxZone = Region.TaxZone.
   END.
   
   IF icTaxZone > "" AND lcTaxZone NE icTaxZone THEN RETURN FALSE.
    
   CREATE ttInv.
   ASSIGN ttInv.ExtInvID   = Invoice.ExtInvID
          ttInv.InvType    = Invoice.InvType  
          ttInv.InvDate    = Invoice.InvDate
          ttInv.CustNum    = Invoice.CustNum
          ttInv.TaxZone    = lcTaxZone
          ttInv.AmtVat0    = Invoice.AmtExclVat
          ttInv.VatAmt     = Invoice.VatAmt
          ttInv.TotAmt     = Invoice.InvAmt.
          
   /* temporary customer used */       
   IF Customer.CustNum < 1000 THEN DO:
      
      /* credit invoice */
      IF Invoice.InvType >= 8 AND Invoice.InvType <= 11 THEN
         liInvNum = Invoice.CrInvNum.
      ELSE liInvNum = Invoice.InvNum.
      
      FOR FIRST Order NO-LOCK WHERE
                Order.InvNum = liInvNum,
          FIRST OrderCustomer OF Order NO-LOCK WHERE
                OrderCustomer.RowType = Order.InvCustRole:
                
         ttInv.CustName = DYNAMIC-FUNCTION("fPrintOrderName" IN ghFunc1,
                                           BUFFER OrderCustomer).
      END.                                     

   END.        
          
   IF ttInv.CustName = "" THEN         
      ttInv.CustName   = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                          BUFFER Customer).

   DO liCnt = 1 TO 10:
      IF Invoice.VatPerc[liCnt] > 0 AND Invoice.VatAmount[liCnt] NE 0 THEN DO:
         ttInv.VatPerc = Invoice.VatPerc[liCnt].
         LEAVE.
      END.
   END.
   
   IF NOT CAN-FIND(FIRST ttTaxZone WHERE ttTaxZone.TaxZone = ttInv.TaxZone)
   THEN DO:
      CREATE ttTaxZone.
      ttTaxZone.TaxZone = ttInv.TaxZone.
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
    EACH Invoice NO-LOCK USE-INDEX CustNum WHERE
         Invoice.Brand    = gcBrand          AND
         Invoice.CustNum  = Customer.CustNum AND
         Invoice.InvDate >= idtInvDate1      AND
         Invoice.InvDate <= idtInvDate2      AND
         Invoice.InvType >= iiInvType1       AND
         Invoice.InvType <= iiInvType2:  

   fCollectInvoice(). 

END.
            


ELSE 
FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand    = gcBrand     AND
         Invoice.InvDate >= idtInvDate1 AND
         Invoice.InvDate <= idtInvDate2 AND  
         Invoice.InvType >= iiInvType1  AND
         Invoice.InvType <= iiInvType2,  
   FIRST Customer OF Invoice NO-LOCK:

   fCollectInvoice().     

   IF NOT SESSION:BATCH AND liQty MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISP liQty  
              LABEL "Invoice Qty" 
              FORMAT ">>>>>>>9"
           Invoice.InvDate 
              LABEL "Invoice Date"
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
                                idtInvDate2).
      
      IF lcZone = "" OR lcZone = ? THEN lcZone = TaxZone.TZName.
   END.
   ELSE lcZone = ttTaxZone.TaxZone.

   ASSIGN 
      lcFile = lcSpoolDir + "/" + icFile
      lcFile = REPLACE(lcFile,"#TAXZONE",ttTaxZone.TaxZone)
      lcFile = REPLACE(lcFile,"#TZNAME",lcZone)
      lcFile = REPLACE(lcFile,"#PERIOD",STRING(YEAR(idtInvDate1)) +
                                        STRING(MONTH(idtInvDate1))).
      
   OUTPUT STREAM sFile TO VALUE(lcFile).
   
   PUT STREAM sFile UNFORMATTED
      "Número de factura"    CHR(9)
      "Fecha de factura"     CHR(9)
      "Nombre de cliente"    CHR(9)
      "Base imponible"       CHR(9)
      "%"                    CHR(9)
      "Quota"                CHR(9)
      "Total"                SKIP.
      
   FOR EACH ttInv WHERE
            ttInv.TaxZone = ttTaxZone.TaxZone
   BREAK BY ttInv.InvType
         BY ttInv.ExtInvID:
            
      PUT STREAM sFile UNFORMATTED
         ttInv.ExtInvID               CHR(9)
         ttInv.InvDate                CHR(9)
         ttInv.CustName               CHR(9)
         fDispDecimal(ttInv.AmtVat0)  CHR(9)
         fDispDecimal(ttInv.VatPerc)  CHR(9)
         fDispDecimal(ttInv.VatAmt)   CHR(9)
         fDispDecimal(ttInv.TotAmt)   SKIP.
         
      oiInvCount = oiInvCount + 1.   

      ACCUMULATE ttInv.AmtVat0 (TOTAL BY ttInv.InvType)
                 ttInv.VatAmt  (TOTAL BY ttInv.InvType)
                 ttInv.TotAmt  (TOTAL BY ttInv.InvType).
 
      IF LAST-OF(ttInv.InvType) THEN DO:
      
         lcTypeName = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                       "Invoice",
                                       "InvType",
                                       STRING(ttInv.InvType)).
         
         PUT STREAM sFile UNFORMATTED
            SKIP(1)
            lcTypeName " total"   CHR(9)
            CHR(9)
            CHR(9)
            fDispDecimal(ACCUM TOTAL BY ttInv.InvType ttInv.AmtVat0) CHR(9)
            CHR(9)
            fDispDecimal(ACCUM TOTAL BY ttInv.InvType ttInv.VatAmt)  CHR(9)
            fDispDecimal(ACCUM TOTAL BY ttInv.InvType ttInv.TotAmt)  
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


