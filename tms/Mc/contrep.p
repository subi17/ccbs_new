/* ------------------------------------------------------
  MODULE .......: CONTREP
  FUNCTION .....: List contract data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 06.10.03
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}
{utumaa.i}

DEF INPUT PARAMETER icInvGrp1    AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icInvGrp2    AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER iiCustNum1   AS INT   NO-UNDO. 
DEF INPUT PARAMETER iiCustNum2   AS INT   NO-UNDO. 
DEF INPUT PARAMETER icReseller1  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icReseller2  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSalesman1  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER icSalesman2  AS CHAR  NO-UNDO. 
DEF INPUT PARAMETER iiContrType1 AS INT   NO-UNDO. 
DEF INPUT PARAMETER iiContrType2 AS INT   NO-UNDO. 
DEF INPUT PARAMETER idtToDate1   AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtToDate2   AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtClDate1   AS DATE  NO-UNDO.
DEF INPUT PARAMETER idtClDate2   AS DATE  NO-UNDO.
DEF INPUT PARAMETER iiClosed     AS INT   NO-UNDO. 

def var viiva1 as char NO-UNDO format "x(112)".
def var viiva2 like viiva1.
def var viiva3 like viiva1.
def var viiva4 like viiva1.
def var jar    as char NO-UNDO format "x(24)".
def var order  as int  NO-UNDO.
def var sl     as int  NO-UNDO.
def var rl     as int  NO-UNDO.
def var rlx    as int  NO-UNDO.
def var lev    as int  NO-UNDO init 112.
def var otsi   as char NO-UNDO extent 39.

DEF VAR xDateHeader AS CHAR NO-UNDO.
DEF VAR lcContrType AS CHAR NO-UNDO. 
DEF VAR ldBilled    AS DEC  NO-UNDO. 
DEF VAR llVatIncl   AS LOG  NO-UNDO.
DEF VAR llVatFact   AS DEC  NO-UNDO. 

ASSIGN 
    viiva1   = fill("=",lev)
    viiva2   = fill("=",lev)
    viiva3   = fill("-",lev)
    viiva4   = fill("-",lev).


form header
   viiva1 AT 1 SKIP
   ynimi at 1 format "x(30)" 
      "CONTRACTS" at 40
      "Page" at 102  
      sl format "ZZZZ9" skip
   xDateHeader AT 40 FORMAT "X(30)"
      pvm format "99-99-9999" at 103 skip
   viiva2 at 1 skip
   "Contract"   AT 1 
   "Customer"   TO 17
   "Name"       AT 19
   "Salesman"   AT 32
   "Name"       AT 41
   "Contr.Type" AT 57
   "FeeModel"   AT 68
   "Agr.From"   AT 77
   "Agr.To"     AT 86
   "Closed"     AT 95
   "Fee Amt"    TO 112
   SKIP
   viiva3
   with width 112 no-label no-box frame sivuotsi.


FUNCTION fCheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    if rl + iAddLine >= skayt1 then do:
        {uprfeed.i rl}
        assign rlx = 0
               sl = sl + 1.
        view stream tul frame sivuotsi.  
        assign rl = 12.
    end.

    RETURN TRUE.
END.

FUNCTION fRemVat RETURNS DECIMAL
   (idAmt      AS DEC,
    icBillCode AS CHAR).

   IF llVatIncl THEN
   FOR FIRST BillItem NO-LOCK WHERE
             BillItem.Brand    = Contract.Brand AND
             BillItem.BillCode = icBillCode,
       FIRST VatCode OF BillItem NO-LOCK:

       idAmt = ROUND(idAmt / (1 + VatCode.VatPerc / 100),2).
   END.

   RETURN idAmt.

END FUNCTION.


assign sl = 1. 
VIEW STREAM tul FRAME sivuotsi.
ASSIGN rl = 12.

IF idtClDate1 = ? AND idtClDate2 NE ?
THEN idtClDate1 = DATE(01,01,2000).

FOR EACH Contract NO-LOCK WHERE
         Contract.Brand      = gcBrand      AND 
         Contract.CustNum   >= iiCustNum1   AND
         Contract.CustNum   <= iiCustNum2   AND
         Contract.Salesman  >= icSalesman1  AND
         Contract.Salesman  <= icSalesman2  AND
         Contract.ContrType >= iiContrType1 AND
         Contract.ContrType <= iiContrType2 AND
         Contract.ToDate    >= idtToDate1   AND
         Contract.ToDate    <= idtToDate2   AND
         (IF idtClDate1 = ?
          THEN TRUE
          ELSE Contract.CloseDate NE ? AND
               Contract.CloseDate <= idtClDate1 AND
               Contract.CloseDate >= idtClDate2)
BY Contract.ContrType
BY Contract.ToDate DESC:

   /* check closing state */
   IF iiClosed > 0 THEN 
   CASE iiClosed:
   WHEN 1 THEN DO:  /* open */
             IF Contract.ToDate < TODAY OR
                (Contract.CloseDate NE ? AND Contract.CloseDate <= TODAY)
             THEN NEXT.   
          END.
   WHEN 2 THEN DO:  /* closed */
             IF Contract.ToDate >= TODAY AND
                (Contract.CloseDate = ? OR Contract.CloseDate >= TODAY)
             THEN NEXT.  
          END.
   WHEN 3 THEN DO:  /* prematurely closed */
             IF Contract.CloseDate = ? OR            
                Contract.CloseDate >= Contract.ToDate 
             THEN NEXT. 
          END.
   END CASE.

   llVatIncl = FALSE.

   FIND Customer OF Contract NO-LOCK NO-ERROR.
   IF AVAILABLE Customer THEN DO:
      IF Customer.InvGroup < icInvGrp1 OR
         Customer.InvGroup > icInvGrp2 
      THEN NEXT.

      llVatIncl = Customer.VatIncl.
   END.

   FIND Salesman NO-LOCK WHERE
        Salesman.Brand    = Contract.Brand AND
        Salesman.Salesman = Contract.Salesman NO-ERROR.
   IF AVAILABLE Salesman THEN DO:
      IF Salesman.Reseller < icReseller1 OR
         Salesman.Reseller > icReseller2 
      THEN NEXT. 
   END.

   fCheckPage(0).

   ASSIGN lcContrType = DYNAMIC-FUNCTION ("fTMSCodeName" in ghfunc1,
                                          "Contract",
                                          "ContrType",
                                          STRING(Contract.ContrType))
          ldBilled    = 0.                                          

   /* penalty fees */
   IF Contract.CloseDate NE ? AND 
      Contract.CloseDate < Contract.ToDate
   THEN DO:
      FOR EACH FixedFee NO-LOCK WHERE
               FixedFee.CustNum  = Contract.CustNum  AND
               FixedFee.FeeModel = Contract.FeeModel AND
               FixedFee.Contract = Contract.Contract,
          EACH FFItem OF FixedFee NO-LOCK:

             ldBilled = ldBilled + fRemVat(FixedFee.Amt,
                                           FixedFee.BillCode).
      END.

      FOR EACH SingleFee NO-LOCK WHERE
               SingleFee.CustNum  = Contract.CustNum  AND
               SingleFee.FeeModel = Contract.FeeModel AND
               SingleFee.Contract = Contract.Contract:

             ldBilled = ldBilled + fRemVat(SingleFee.Amt,
                                           SingleFee.BillCode).
      END.
   END.

   PUT STREAM tul               
      Contract.Contract  AT 1   FORMAT "X(8)"  
      Contract.CustNum   TO 17  FORMAT ">>>>>>>9"
      (IF AVAILABLE Customer 
       THEN Customer.CustName   
       ELSE "")          AT 19  FORMAT "X(12)"
      Contract.Salesman  AT 32  FORMAT "X(8)"
      (IF AVAILABLE Salesman
       THEN Salesman.SMName
       ELSE "")          AT 41  FORMAT "X(15)"
      lcContrType        AT 57  FORMAT "X(10)"
      Contract.FeeModel  AT 68  FORMAT "X(8)" 
      Contract.FromDate  AT 77  FORMAT "99-99-99"
      Contract.ToDate    AT 86  FORMAT "99-99-99"
      Contract.CloseDate AT 95  FORMAT "99-99-99".

   IF ldBilled NE 0 THEN PUT STREAM tul
      ldBilled           TO 112 FORMAT "->>>>9.99".

   PUT STREAM tul SKIP.

   rl = rl + 1.   

END.

{uprfeed.i rl}

