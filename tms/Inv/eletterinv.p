/* ----------------------------------------------------------------------------
  MODULI .......: ELETTERINV.P
  TEHTAVA ......: CREATE INVOICES WITH ELETTER LIMITS
                  GET'S INPUT FROM ELETTERLIM.P
  SOVELLUTUS ...: NN
  TEKIJA .......: JR
  LUONTIPVM ....: 27.08.01
  MUUTOSPVM ....: 25.10.2001/jr Choose all/not printed invoices
                  21.11.2001 ht few functions into refcode.i
                  04.12.2001 ht run reports
                  07.12.2001/aam variables to edefine.i
                  07.12.2001 ht sv-Customer
                  12.12.2001/aam get product name from invlang,
                                 display country for customer,
                  21.12.2001/aam for Jippii,
                                 use cparam2.i etc.
                  06.02.2002/aam advance payments
                  10.02.2002/aam new report for contracts (nncore1),
                                 test sp-code[1] (deny printing),
                                 separate files for invoices containing calls
                  11.02.2002/aam invgroup.ig-cpers left out 
                  13.02.2002/aam nnlrivi.alkpvm and loppvm can be blank 
                  25.02.2002/aam credit invoices can be printed optionally
                  08.03.2002/aam move the files to a transfer directory
                  11.03.2002/aam only collect invoices and make a temp-table
                                 here, eplfile.p actually prints the file
                  12.02.2003/aam LetterClass
                  11.03.2003/aam MinAmt to wInvoice
                  30.04.2003/aam check Invoice.dType
                  19.05.2003/aam separate search loop when date is given
                  27.05.2004/aam input InvType, output ocError
                  01.11.2004/aam also credited invoices with ilCredit
                  30.09.2005/aam invgroup and invoice date to file name
                  07.10.2005/aam zipcode to wInvoice
                  10.05.2006/aam return-value from eplfile to ocError
                  21.06.2006/aam liFileType for eplfile.p
                  19.01.2007/mvi "Credit invoice" term -> "Credit note"
VERSIO .......: M15
---------------------------------------------------------------------------- */

{commali.i}
{cparam2.i}
{edefine.i NEW}

DEFINE INPUT  PARAMETER iiInvNum1      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER iiInvNum2      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER iiInvDate      AS DATE      NO-UNDO.
DEFINE INPUT  PARAMETER icInvGrp       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum      AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ilPrintService AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iiltila        AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER ilCredit       AS LOGICAL   NO-UNDO.
DEFINE INPUT  PARAMETER iiInvType      AS INT       NO-UNDO.
DEFINE INPUT  PARAMETER iLetterClass   AS INT       NO-UNDO. 
DEFINE INPUT  PARAMETER icFile         AS CHAR      NO-UNDO. 
DEFINE OUTPUT PARAMETER oInvCount      AS INT       NO-UNDO.
DEFINE OUTPUT PARAMETER ocError        AS CHAR      NO-UNDO. 

DEF VAR ldtNameDate AS DATE NO-UNDO. 
DEF VAR lcDate      AS CHAR NO-UNDO.

{invprdf.i}


FUNCTION fMakeTemp RETURNS LOGICAL.

    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN DO:
       ocError = "Printing denied".
       RETURN FALSE.
    END.

    ELSE IF NOT iiltila AND Invoice.PrintState > 0 THEN RETURN FALSE.
    
    ELSE IF NOT ilCredit AND Invoice.InvAmt < 0 THEN DO:
       ocError = "Invoice is a credit note".
       RETURN FALSE.
    END.
    
    ELSE IF NOT ilCredit AND Invoice.CrInvNum > 0 AND Invoice.InvAmt >= 0 
    THEN DO:
       ocError = "Invoice has been credited".
       RETURN FALSE.
    END. 
    
    /* deposit/adv.payment invoices are not sent to print service */
    ELSE IF ilPrintService AND 
         Invoice.InvType >= 3 AND Invoice.InvType <= 4
    THEN RETURN FALSE. 

    /* invoice type */
    ELSE IF (iiInvType <= 1 AND Invoice.InvType > 1) OR
            (iiInvType >= 3 AND Invoice.InvType < 3)
    THEN RETURN FALSE.
     
    ELSE DO:
    
       CREATE wInvoice.
       ASSIGN wInvoice.InvNum  = Invoice.InvNum
              oInvCount        = oInvCount + 1. 

       IF Customer.IDelName > "" 
       THEN wInvoice.ZipCode = Customer.IDelZipCode.
       ELSE wInvoice.ZipCode = Customer.ZipCode.

       IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.

       RETURN TRUE.
    END.
    
END FUNCTION.

IF iiInvNum1 = iiInvNum2 THEN 
FOR FIRST Invoice NO-LOCK WHERE
          Invoice.Brand  = gcBrand AND
          Invoice.InvNum = iiInvNum1,
    FIRST Customer OF Invoice NO-LOCK:
   
   icInvGrp = Customer.InvGroup.
   
   fMakeTemp().

END.

ELSE IF iiInvDate NE ? THEN 
FOR EACH Invoice NO-LOCK WHERE    
         Invoice.Brand    = gcBrand    AND
         Invoice.InvDate  = iiInvDate  AND
         /* delivery type is on paper */
         Invoice.DelType <= 1          AND
         Invoice.InvNum  >= iiInvNum1  AND      
         Invoice.InvNum  <= iiInvNum2  AND   
         (IF iiCustNum NE 0 
          THEN Invoice.CustNum = iiCustNum 
          ELSE Invoice.CustNum > 0),        
   FIRST Customer OF Invoice NO-LOCK WHERE 
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().
        
   IF oInvCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY oInvCount FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl.
   END.
   
END. 

ELSE 
FOR EACH Invoice NO-LOCK WHERE               
         Invoice.Brand  = gcBrand    AND
         Invoice.InvNum >= iiInvNum1  AND  
         Invoice.InvNum <= iiInvNum2  AND    
         /* delivery type is on paper */
         Invoice.DelType <= 1         AND
         (IF iiCustNum NE 0 
          THEN Invoice.CustNum = iiCustNum 
          ELSE Invoice.CustNum > 0),    
   FIRST Customer of Invoice NO-LOCK WHERE
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().

   IF oInvCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY oInvCount FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl2.
   END.
   
END. 

/* invgroup to file name */
IF icInvGrp > "" 
THEN icFile = REPLACE(icFile,"#IGRP",icInvGrp).
ELSE icFile = REPLACE(icFile,"#IGRP","ALL").
   
/* invoice date to file name */   
IF ldtNameDate NE ? THEN DO:
   
   lcDate = DYNAMIC-FUNCTION("fDateFmt" IN ghFunc1,
                             ldtNameDate,
                             "yyyymmdd").
   icFile = REPLACE(icFile,"#IDATE",lcDate).
END.
ELSE icFile = REPLACE(icFile,"#IDATE","").

/* print */
RUN eplfile (INPUT-OUTPUT TABLE wInvoice,  
             ?,           /* print date (= Invoice.InvDate) */
             (IF ilPrintService 
              THEN 2
              ELSE 0),     /* 0/2 = invoices, 
                               1   = reminders, 
                           */
             iLetterClass, 
             oInvCount,
             icFile,
             1,            /* FileType, 1=normal epl */
             OUTPUT oInvCount). 

ocError = RETURN-VALUE.
 
