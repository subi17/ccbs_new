/* ---------------------------------------------------------------------------
  MODULE .......: PDFINVCL
  FUNCTION .....: Collect invoices to be printed as PDF files
  APPLICATION ..: TMS
  CREATED ......: 30.04.03/aam 
  MODIFIED .....: 08.10.03/aam SendMail as int 
  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Inv/pdfinvdf.i}

DEF INPUT  PARAMETER iiInvNum1     AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiInvNum2     AS INT  NO-UNDO.
DEF INPUT  PARAMETER idtInvDate1   AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtInvDate2   AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiCustNum1    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiCustNum2    AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiPrintState1 AS INT  NO-UNDO.
DEF INPUT  PARAMETER iiPrintState2 AS INT  NO-UNDO.
DEF INPUT  PARAMETER icInvGroup    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icExtGrp      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiDelType     AS INT  NO-UNDO. 
DEF INPUT  PARAMETER ilPrintRep    AS LOG  NO-UNDO.
DEF INPUT  PARAMETER ilFormPDF     AS LOG  NO-UNDO. 
DEF INPUT  PARAMETER iiSendMail    AS INT  NO-UNDO.
DEF OUTPUT PARAMETER oiInvQty      AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiMailQty     AS INT  NO-UNDO. 
DEF OUTPUT PARAMETER oiErrQty      AS INT  NO-UNDO. 

DEF VAR lcFile       AS CHAR  NO-UNDO. 
DEF VAR lcXLTDir     AS CHAR  NO-UNDO. 
DEF VAR lcXLTFile    AS CHAR  NO-UNDO. 
DEF VAR lcInvForm    AS CHAR  NO-UNDO. 
DEF VAR lcPDFFile    AS CHAR  NO-UNDO. 
DEF VAR lcFopMethod  AS CHAR  NO-UNDO. 
DEF VAR lcLogDir     AS CHAR  NO-UNDO.
DEF VAR lcErrFile    AS CHAR  NO-UNDO. 
DEF VAR lcConfDir    AS CHAR  NO-UNDO.

FUNCTION fMakeTemp RETURNS LOGICAL.

   IF Invoice.InvCfg[1] = TRUE THEN RETURN FALSE. 
   
   /* inv.group selected */ 
   IF icInvGroup ne "" AND Customer.InvGroup NE icInvGroup 
   THEN RETURN FALSE.

   /* ext cust group selected ? */
   IF icExtGrp NE "" AND 
      NOT CAN-FIND(CGMember WHERE 
                   CGMember.CustNum   = Customer.CustNum AND
                   CGMember.custgroup = icExtGrp) 
   THEN RETURN FALSE.
           
   CREATE ttPDFInv.
   ttPDFInv.InvNum = Invoice.InvNum.

   RETURN TRUE. 
   
END FUNCTION.

/* collect invoices */

/* if one invoice is selected, assume that other parameters
   are insignificant */
IF iiInvNum1 = iiInvNum2 THEN 
FOR FIRST Invoice NO-LOCK WHERE 
          Invoice.Brand  = gcBrand AND
          Invoice.InvNum = iiInvNum1,
    FIRST Customer OF Invoice NO-LOCK:
   
   IF Invoice.DelType NE 2 AND iiSendMail > 0 THEN NEXT. 
          
   fMakeTemp().       
END.          

/* one date is chosen */
ELSE IF idtInvDate1 = idtInvDate2 THEN
FOR EACH Invoice NO-LOCK WHERE          
         Invoice.Brand       = gcBrand       AND
         Invoice.InvDate     = idtInvDate1   AND
         Invoice.DelType     = iiDelType     AND
         Invoice.InvNum     >= iiInvNum1     AND
         Invoice.InvNum     <= iiInvNum2     AND
         Invoice.CustNum    >= iiCustNum1    AND
         Invoice.CustNum    <= iiCustNum2    AND
         Invoice.PrintState >= iiPrintState1 AND
         Invoice.PrintState <= iiPrintState2,
    FIRST Customer of Invoice NO-LOCK:
    
   fMakeTemp().
   
END.    
         
/* wider selection */
ELSE 
FOR EACH  Invoice NO-LOCK WHERE
          Invoice.Brand        = gcBrand       AND
          Invoice.InvDate     >= idtInvDate1   AND
          Invoice.InvDate     <= idtInvDate2   AND
          Invoice.InvNum      >= iiInvNum1     AND
          Invoice.InvNum      <= iiInvNum2     AND
          Invoice.CustNum     >= iiCustNum1    AND
          Invoice.CustNum     <= iiCustNum2    AND
          Invoice.PrintState  >= iiPrintState1 AND
          Invoice.PrintState  <= iiPrintState2 AND
          Invoice.DelType      = iiDelType,
    FIRST Customer of Invoice NO-LOCK:
    
   fMakeTemp(). 
  
END. 


IF NOT CAN-FIND(FIRST ttPDFInv) THEN DO:

   MESSAGE "No invoices matching given criteria were found."
   VIEW-AS ALERT-BOX
   ERROR.
   
   RETURN.
   
END.

RUN Inv/pdfinv.p(INPUT-OUTPUT TABLE ttPDFInv,
           INPUT  ilPrintRep,
           INPUT  ilFormPDF,
           INPUT  iiSendMail,
           INPUT  0,       /* printtype, 0=invoices */
           INPUT  ?,       /* reminding day */
           OUTPUT oiInvQty,
           OUTPUT oiMailQty,
           OUTPUT oiErrQty).
           

