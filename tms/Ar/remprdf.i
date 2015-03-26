/* remprdf.i        11.03.04/aam
                    18.04.05/aam Debt, Closed, TargCust
                    19.06.06/aam InvCust,AgrCust
                    07.09.06/aam note target customer when checking for
                                 foreign country vrs. letterclass
*/


DEF {1} SHARED TEMP-TABLE ttCust NO-UNDO
    FIELD InvCust     AS INT
    FIELD AgrCust     AS INT
    FIELD Debt        AS DEC
    FIELD Closed      AS LOG
    FIELD TargCust    AS INT
    FIELD Printed     AS INT
    FIELD PaymPlan    AS INT
    FIELD LetterClass AS INT
    FIELD ChargeFee   AS LOG 
    INDEX InvCust IS UNIQUE InvCust.

DEF {1} SHARED TEMP-TABLE ttInv NO-UNDO
    FIELD InvCust AS INT
    FIELD InvNum  AS INT
    FIELD Amt     AS DEC
    INDEX InvCust InvCust
    INDEX InvNum IS UNIQUE InvNum.

DEF BUFFER bttCust  FOR ttCust.
DEF BUFFER bttInv   FOR ttInv.
DEF BUFFER bRemCust FOR Customer.
DEF BUFFER bCredInv FOR Invoice.


FUNCTION fValidateInvoice RETURNS LOGIC
   (INPUT  ilCheckDueDate AS LOG,
    INPUT  idtPrintDate   AS DATE, 
    OUTPUT odBal AS DEC).
   
   odBal = 0.
   
   IF Invoice.InvType = 3 OR
      Invoice.InvType = 4 OR                
      Invoice.PrintState = 0          OR
      Invoice.InvCfg[1]  = TRUE       OR
      Invoice.ClaimPerm  = FALSE     
   THEN RETURN FALSE.  

   /* only overdue invoices */
   IF ilCheckDueDate AND Invoice.DueDate >= idtPrintDate THEN RETURN FALSE. 

   /* make sure that invoice is NOT paid */
   odBal = fInvBal(BUFFER Invoice,
                   idtPrintDate). 
   IF odBal = 0 THEN RETURN FALSE.
        
   /* check on credit cases that payment is not missing */
   IF odBal = Invoice.InvAmt AND Invoice.CrInvNum > 0 THEN DO:
      FIND bCredInv WHERE bCredInv.InvNum = Invoice.CrInvNum 
         NO-LOCK NO-ERROR.
      IF AVAILABLE bCredInv THEN odBal = odBal + bCredInv.InvAmt.
      IF odBal = 0 THEN RETURN FALSE.
   END.
    
   RETURN TRUE.
   
END FUNCTION.


FUNCTION fCollInv RETURNS LOGICAL
   (iiInvCust  AS INT,
    iiAgrCust  AS INT,
    iiTargCust AS INT,   /* 1=agr, 2=inv.cust */
    idDebt     AS DEC):

   IF CAN-FIND(FIRST ttInv WHERE
                     ttInv.InvNum  = Invoice.InvNum)
   THEN RETURN FALSE.
   
   CREATE ttInv.
   ASSIGN ttInv.InvCust = iiInvCust
          ttInv.InvNum  = Invoice.InvNum
          ttInv.Amt     = idDebt.

   FIND FIRST ttCust WHERE ttCust.InvCust = iiInvCust NO-ERROR.
   IF NOT AVAILABLE ttCust THEN DO:
      CREATE ttCust.
      ASSIGN ttCust.InvCust     = iiInvCust
             ttCust.AgrCust     = iiAgrCust
             ttCust.Closed      = FALSE
             ttCust.TargCust    = iiTargCust
             ttCust.ChargeFee   = TRUE.
          
      IF iiTargCust = 1 
      THEN FIND bRemCust WHERE bRemCust.CustNum = iiAgrCust NO-LOCK NO-ERROR.
      ELSE FIND bRemCust WHERE bRemCust.CustNum = iiInvCust NO-LOCK NO-ERROR.
        
      /* for foreign customers use always 1. class */       
      IF AVAILABLE bRemCust THEN DO:
         IF LOOKUP(bRemCust.Country,"FI,FIN,FINLAND,") = 0 
         THEN ttCust.LetterClass = 1.
      END.                     
   END.
   
   ttCust.Debt = ttCust.Debt + idDebt.
             
END FUNCTION.

FUNCTION fDeleteTemp RETURNS LOGICAL:

   FOR EACH ttInv OF ttCust:
      DELETE ttInv.
   END.
   
   DELETE ttCust.

END FUNCTION.


