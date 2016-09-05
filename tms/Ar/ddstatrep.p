/* ------------------------------------------------------
  MODULE .......: DDSTATREP
  FUNCTION .....: List DD status 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 01.11.04
  MODIFIED .....: 
  VERSION ......: SL 
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/cparam2.i}
{Syst/utumaa.i}

DEF INPUT PARAMETER idtAuthDate1  AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtAuthDate2  AS DATE  NO-UNDO.
DEF INPUT PARAMETER ilListUnsent  AS LOG   NO-UNDO. 
DEF INPUT PARAMETER icToFile      AS CHAR  NO-UNDO. 
                     
DEF VAR lcLine1      AS CHAR NO-UNDO FORMAT "x(78)".
DEF VAR lcLine2      LIKE lcLine1.
DEF VAR lcLine3      LIKE lcLine1.
DEF VAR lcLine4      LIKE lcLine1.
DEF VAR liPage       AS INT  NO-UNDO.
DEF VAR liLine       AS INT  NO-UNDO.
DEF VAR liWidth      AS INT  NO-UNDO INIT 78.

DEF VAR lcDateHeader AS CHAR NO-UNDO.
DEF VAR liQty        AS INT  NO-UNDO.
DEF VAR lcSessionNum AS CHAR NO-UNDO.
DEF VAR lcBankAcc    AS CHAR NO-UNDO.

DEF TEMP-TABLE ttAuth NO-UNDO
   FIELD InvGroup AS CHAR
   FIELD DDNew    AS INT
   FIELD DDTerm   AS INT
   FIELD DDChange AS INT
   FIELD DDActive AS INT
   FIELD InvSent  AS INT
   FIELD DDSent   AS INT
   FIELD ToBank   AS INT
   INDEX InvGroup InvGroup.

DEF TEMP-TABLE ttCust NO-UNDO
   FIELD CustNum  AS INT
   FIELD InvGroup AS CHAR
   INDEX CustNum CustNum.
   
DEF TEMP-TABLE ttInv NO-UNDO
   FIELD InvNum AS INT
   INDEX InvNum InvNum.
   
ASSIGN 
    lcLine1   = FILL("=",liWidth)
    lcLine2   = FILL("=",liWidth)
    lcLine3   = FILL("-",liWidth)
    lcLine4   = FILL("-",liWidth)
    lcSessionNum           = SESSION:NUMERIC-FORMAT
    SESSION:NUMERIC-FORMAT = "European".

form header
   lcLine1 AT 1 SKIP
   ynimi  AT 1 FORMAT "x(30)" 
      "DIRECT DEBIT STATUS" AT 40
      "Page" AT 71
      liPage FORMAT "ZZ9" SKIP
   lcDateHeader AT 40 FORMAT "X(30)"
      pvm FORMAT "99.99.99" AT 71 SKIP
   lcLine2 AT 1 SKIP
   "InvGroup"    AT 1
   "Name"        AT 10
   "New Auth"    TO 32
   "Active"      TO 43
   "Inv. Sent"   TO 54 
   "DD Invoices" TO 67 
   "To Bank"     TO 78 
   SKIP
   lcLine3 AT 1 SKIP
   WITH WIDTH 112 NO-LABEL NO-BOX FRAME fPageHead.

FORM "Browsing:" AT 2 liQty   FORMAT ">>,>>>,>>9" SKIP
WITH NO-LABELS OVERLAY ROW 12 CENTERED TITLE " Collecting DD events "
   FRAME fQty.


FUNCTION fChkPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF icToFile > "" THEN RETURN FALSE.
    
    IF liLine + iAddLine >= skayt1 THEN DO:

        IF liPage > 0 THEN DO:
           {Syst/uprfeed.i liLine}
        END.
        
        liPage = liPage + 1.
        VIEW STREAM tul FRAME fPageHead.
        liLine = 6.

        RETURN TRUE.
    END.    
    
    ELSE RETURN FALSE.
END.

lcDateHeader = STRING(idtAuthDate1,"99.99.99") + " - " +
               STRING(idtAuthDate2,"99.99.99").

PAUSE 0.
VIEW FRAME fQty. 

/* authorization status */
FOR EACH DDAuth NO-LOCK WHERE
         DDAuth.Brand = gcBrand,
   FIRST Customer OF DDAuth NO-LOCK:
     
   IF DDAuth.AuthDate >= idtAuthDate1 AND
      DDAuth.AuthDate <= idtAuthDate2 
   THEN DO:

      FIND FIRST ttAuth WHERE 
                 ttAuth.InvGroup = Customer.InvGroup NO-ERROR.
      IF NOT AVAILABLE ttAuth THEN DO:
         CREATE ttAuth.
         ttAuth.InvGroup = Customer.InvGroup.
      END.

      CASE DDAuth.DDProcess:
      WHEN 1 OR 
      WHEN 11 THEN ttAuth.DDNew    = ttAuth.DDNew + 1.
      WHEN 2 OR
      WHEN 4  THEN ttAuth.DDChange = ttAuth.DDChange + 1.
      WHEN 3  THEN ttAuth.DDTerm   = ttAuth.DDTerm + 1.
      END CASE.
   END.
      
   liQty = liQty + 1.
   IF liQty < 100 OR liQty MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liQty WITH FRAME fQty.
   END.
 
   /* gather customers for activity check */
   IF NOT CAN-FIND(FIRST ttCust WHERE ttCust.CustNum = DDAuth.CustNum) THEN DO:
      CREATE ttCust.
      ASSIGN ttCust.CustNum  = DDAuth.CustNum
             ttCust.InvGroup = Customer.InvGroup.
   END.
    
END.

/* active customers */
FOR EACH ttCust:

   RUN Ar/nnsvte.p (ttCust.CustNum,
               idtAuthDate2,
               OUTPUT lcBankAcc).
   
   IF lcBankAcc > "" THEN DO:
      FIND FIRST ttAuth WHERE 
                 ttAuth.InvGroup = ttCust.InvGroup NO-ERROR.
      IF NOT AVAILABLE ttAuth THEN DO:
         CREATE ttAuth.
         ttAuth.InvGroup = ttCust.InvGroup.
      END.
      
      ttAuth.DDActive = ttAuth.DDActive + 1.
      
   END.
END.

/* sent invoices */
FOR EACH Invoice NO-LOCK WHERE
         Invoice.InvDate    >= idtAuthDate1 AND
         Invoice.InvDate    <= idtAuthDate2 AND
         Invoice.PrintState > 0,
   FIRST Customer OF Invoice NO-LOCK:

   FIND FIRST ttAuth WHERE 
              ttAuth.InvGroup = Customer.InvGroup NO-ERROR.
   IF NOT AVAILABLE ttAuth THEN DO:
      CREATE ttAuth.
      ttAuth.InvGroup = Customer.InvGroup.
   END.
   ttAuth.InvSent = ttAuth.InvSent + 1.
   
   /* dd-invoice */
   IF Invoice.DDBankAcc > "" AND Invoice.ChargeType = 2 THEN DO:

      ttAuth.DDSent = ttAuth.DDSent + 1.
      
      /* sent to bank */
      IF Invoice.DDState = 1 THEN ttAuth.ToBank = ttAuth.ToBank + 1.
      
      ELSE DO:
         CREATE ttInv.
         ttInv.InvNum = Invoice.InvNum.
      END. 
      
      /* in case of termination authorization has been deleted, and this
         customer could be missing from "active" counter */
      IF NOT CAN-FIND(FIRST ttCust WHERE ttCust.CustNum = Invoice.CustNum)
      THEN DO:
         CREATE ttCust.
         ASSIGN ttCust.CustNum  = Invoice.CustNum
                ttCust.InvGroup = Customer.InvGroup.
         
         ttAuth.DDActive = ttAuth.DDActive + 1.
      END.
   END. 
 
   liQty = liQty + 1.
   IF liQty < 100 OR liQty MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY liQty WITH FRAME fQty.
   END.
          
END.

PAUSE 0.
DISPLAY liQty WITH FRAME fQty.


/* headers to excel-file */
IF icToFile > "" THEN DO:

   OUTPUT STREAM tul TO VALUE(icToFile).
   
   PUT STREAM tul UNFORMATTED
      "DIRECT DEBIT"  SKIP
      lcDateHeader SKIP(1).
      
   PUT STREAM tul UNFORMATTED   
      "Inv.Group"       CHR(9)
      "New"             CHR(9)
      "Active"          CHR(9)
      "Inv. Sent"       CHR(9)
      "DD Invoices"     CHR(9)
      "To Bank"         SKIP.
      
END.

ELSE DO:
   fChkPage(99).
END.

FOR EACH ttAuth:

   IF icToFile = "" THEN DO:
      fChkPage(0).
   
      FIND InvGroup WHERE 
           InvGroup.Brand    = gcBrand AND
           InvGroup.InvGroup = ttAuth.InvGroup NO-LOCK.
      
      PUT STREAM tul
         ttAuth.InvGroup  TO 8  FORMAT "X(8)"
         InvGroup.IGName  AT 10 FORMAT "X(11)"
         ttAuth.DDNew     TO 32 FORMAT ">>>>>>9"
         ttAuth.DDActive  TO 43 FORMAT ">>>>>>9"
         ttAuth.InvSent   TO 54 FORMAT ">>>>>>9"
         ttAuth.DDSent    TO 67 FORMAT ">>>>>>9"
         ttAuth.ToBank    TO 78 FORMAT ">>>>>>9"
         SKIP.
      
      liLine = liLine + 1.
   END.

   ELSE DO:
      PUT STREAM tul UNFORMATTED   
         ttAuth.InvGroup   CHR(9)
         ttAuth.DDNew      CHR(9)
         ttAuth.DDActive   CHR(9)
         ttAuth.InvSent    CHR(9)
         ttAuth.DDSent     CHR(9)
         ttAuth.ToBank     SKIP.
   END.
      
   ACCUMULATE ttAuth.DDNew    (TOTAL)
              ttAuth.DDActive (TOTAL)
              ttAuth.InvSent  (TOTAL)
              ttAuth.DDSent   (TOTAL)
              ttAuth.ToBank   (TOTAL).

END.

/* grand total */
IF icToFile = "" THEN DO:
   fChkPage(2).
   
   PUT STREAM tul
      FILL("=",78) AT 1 FORMAT "X(78)" SKIP
      "Total:" AT 1
      (ACCUM TOTAL ttAuth.DDNew)    TO 32 FORMAT ">>>>>>9"
      (ACCUM TOTAL ttAuth.DDActive) TO 43 FORMAT ">>>>>>9"
      (ACCUM TOTAL ttAuth.InvSent)  TO 54 FORMAT ">>>>>>9"
      (ACCUM TOTAL ttAuth.DDSent)   TO 67 FORMAT ">>>>>>9"
      (ACCUM TOTAL ttAuth.ToBank)   TO 78 FORMAT ">>>>>>9"
      SKIP.
          
   liLine = liLine + 2.
END.
   
   
/* show invoices that have not been sent to bank */
IF ilListUnsent AND CAN-FIND(FIRST ttInv) THEN DO:

   IF icToFile = "" THEN DO:
      fChkPage(6).
      
      PUT STREAM tul UNFORMATTED
         SKIP(2)
         "Invoices that have not been sent to bank:" AT 1 SKIP
         "Invoice"   TO 12
         "Date"      AT 14
         "Due Date"  AT 23
         "Amount"    TO 43 SKIP.
      liLine = liLine + 4.
   END.
      
   ELSE DO:
      PUT STREAM tul UNFORMATTED
          SKIP(2)
          "Unsent Invoice"  CHR(9)
          "Date"            CHR(9)
          "Due Date"        CHR(9)
          "Amount"          SKIP.
   END.
          
          
   FOR EACH ttInv,
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = ttInv.InvNum:
            
      IF icToFile = "" THEN DO:   
 
         fChkPage(0).
               
         PUT STREAM tul 
            Invoice.InvNum  AT 5  FORMAT ">>>>>>>9"  
            Invoice.InvDate AT 14 FORMAT "99-99-99"
            Invoice.DueDate AT 23 FORMAT "99-99-99"
            Invoice.InvAmt  TO 43 FORMAT "->>>>>>9.99"
            SKIP.
         
         liLine = liLine + 1.
      END.
      
      ELSE PUT STREAM tul UNFORMATTED
         Invoice.InvNum  CHR(9)
         Invoice.InvDate CHR(9)
         Invoice.DueDate CHR(9)
         Invoice.InvAmt  SKIP.
         
   END.
   
   {Syst/uprfeed.i liLine}
END.

HIDE FRAME fQty NO-PAUSE. 

SESSION:NUMERIC-FORMAT = lcSessionNum.

