/* ----------------------------------------------------------------------
  MODULE .......: printdoc1co.p
  TASK .........: Collect invoices to a doc1 file 
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.12.06
  CHANGED ......: 16.04.07/aam icBillRun
                  21.05.07/aam optionally calculate only quantity
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEFINE INPUT  PARAMETER icInvGrp       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum1     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiCustNum2     AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icInvID1       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER icInvID2       AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iiInvDate      AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER ilOnlyNew      AS LOG  NO-UNDO.
DEFINE INPUT  PARAMETER ilCredit       AS LOG  NO-UNDO.
DEFINE INPUT  PARAMETER iiInvType      AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER iiDelType      AS INT  NO-UNDO.
DEFINE INPUT  PARAMETER icFileType     AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER icFile         AS CHAR NO-UNDO. 
DEFINE INPUT  PARAMETER ilTarFile      AS LOG  NO-UNDO. 
DEFINE OUTPUT PARAMETER oiInvCount     AS INT  NO-UNDO.
DEFINE OUTPUT PARAMETER ocError        AS CHAR NO-UNDO. 

DEF VAR ldtNameDate AS DATE NO-UNDO. 
DEF VAR lcDate      AS CHAR NO-UNDO.
DEF VAR llSeparate  AS LOG  NO-UNDO.
DEF VAR lcBillRun   AS CHAR NO-UNDO. 

{Inv/printdoc1tt.i}


FUNCTION fMakeTemp RETURNS LOGICAL.
 
    IF Invoice.ExtInvID = "" THEN DO:
       ocError = "Invoice ID not set".
       RETURN FALSE.
    END.
 
    /* printing denied */
    IF Invoice.InvCfg[1] = TRUE THEN DO:
       ocError = "Printing denied".
       RETURN FALSE.
    END.

    IF ilOnlyNew AND Invoice.PrintState > 0 THEN RETURN FALSE.
    
    IF NOT ilCredit AND Invoice.InvAmt < 0 THEN DO:
       ocError = "Invoice is a credit invoice".
       RETURN FALSE.
    END.
    
    IF NOT ilCredit AND Invoice.CrInvNum > 0 AND Invoice.InvAmt >= 0 
    THEN DO:
       ocError = "Invoice has been credited".
       RETURN FALSE.
    END. 
    
    /* invoice type */
    IF iiInvType > 0 AND Invoice.InvType NE iiInvType 
    THEN RETURN FALSE.

    IF lcBillRun > "" AND Invoice.BillRun NE lcBillRun THEN RETURN FALSE.
    
    CREATE ttInvoice.
    ASSIGN ttInvoice.InvNum = Invoice.InvNum
           oiInvCount       = oiInvCount + 1. 

    IF Customer.IDelName > "" 
    THEN ttInvoice.ZipCode = Customer.IDelZipCode.
    ELSE ttInvoice.ZipCode = Customer.ZipCode.

    IF ldtNameDate = ? THEN ldtNameDate = Invoice.InvDate.

    RETURN TRUE.
    
END FUNCTION.

IF NUM-ENTRIES(icFileType,"|") > 1 THEN ASSIGN 
   lcBillRun  = ENTRY(2,icFileType,"|")
   icFileType = ENTRY(1,icFileType,"|").

IF icInvID1 = icInvID2 THEN 
FOR FIRST Invoice NO-LOCK WHERE
          Invoice.Brand    = gcBrand AND
          Invoice.ExtInvID = icInvID1 AND
          Invoice.DelType  = iiDelType,
    FIRST Customer OF Invoice NO-LOCK:
   
   icInvGrp = Customer.InvGroup.
   
   fMakeTemp().

   IF ocError > "" THEN RETURN "ERROR:" + ocError.
END.

ELSE IF iiInvDate NE ? THEN 
FOR EACH Invoice NO-LOCK WHERE    
         Invoice.Brand    = gcBrand    AND
         Invoice.InvDate  = iiInvDate  AND
         Invoice.ExtInvID >= icInvID1  AND      
         Invoice.ExtInvID <= icInvID2  AND   
         Invoice.CustNum >= iiCustNum1 AND
         Invoice.CustNum <= iiCustNum2 AND
         (IF iiDelType NE ? 
          THEN Invoice.DelType  = iiDelType
          ELSE TRUE),
   FIRST Customer OF Invoice NO-LOCK WHERE 
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().
        
   IF oiInvCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY oiInvCount FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl.
   END.
   
END. 

ELSE 
FOR EACH Invoice NO-LOCK WHERE               
         Invoice.Brand  = gcBrand      AND
         Invoice.ExtInvID >= icInvID1  AND      
         Invoice.ExtInvID <= icInvID2  AND   
         Invoice.CustNum >= iiCustNum1 AND
         Invoice.CustNum <= iiCustNum2 AND
         Invoice.DelType  = iiDelType,
   FIRST Customer of Invoice NO-LOCK WHERE
         (IF icInvGrp NE "" 
          THEN Customer.InvGroup = icInvGrp 
          ELSE TRUE):

   fMakeTemp().

   IF oiInvCount MOD 100 = 0 THEN DO:
      PAUSE 0.
      DISPLAY oiInvCount FORMAT ">>>>>>>9"
      WITH NO-LABELS OVERLAY ROW 10 CENTERED
           TITLE " Collecting " FRAME fColl2.
   END.
   
END. 

/* calculate only quantity */
IF icFile = "Qty" THEN RETURN.

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

IF icFileType BEGINS "XML" THEN DO:
   IF icFileType = "XMLSEP" THEN ASSIGN
      icFileType = "XML"
      llSeparate = TRUE.
   ELSE llSeparate = FALSE.
END.


CASE icFileType:

WHEN "Doc1" OR WHEN "" THEN 
   RUN printdoc1 (INPUT-OUTPUT TABLE ttInvoice,  
                  oiInvCount,
                  icFile,
                  "",
                  0,
                  0,
                  0,
                  OUTPUT oiInvCount). 

WHEN "XML" THEN 
   RUN Inv/invoice_xml.p (INPUT-OUTPUT TABLE ttInvoice,
                    ldtNameDate,
                    oiInvCount,
                    llSeparate,
                    icFile,
                    "QVANTEL",
                    INT(ilTarFile),
                    TRUE,
                    0,
                    0,
                    OUTPUT oiInvCount).

END CASE.

ocError = RETURN-VALUE.
 
