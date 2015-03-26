/* inv_stat.p             
   billing statistics to a tab separated file

   changes:         13.12.2002/aam invtype added 
                    04.02.2003/aam account limit,
                                   new column amount without vat,
                                   new column interval
                    22.09.2003/aam brand 
                    07.01.2004/aam FixedFee.VatIncl             
                    29.06.2004/aam check lcTypeDenied
                                   
                                         
*/

{commali.i}
{excel.i}
{cparam2.i}

/* invoice dates */
def input parameter iDate1    as date no-undo.
def input parameter iDate2    as date no-undo.
/* invoice types */
def input parameter iInvType1 as int  no-undo.
def input parameter iInvType2 as int  no-undo.
/* invoice status, 0 = not printed */
def input parameter iState1   as int  no-undo.
def input parameter iState2   as int  no-undo.
/* accounts */
def input parameter iAccount1 as int  no-undo.
def input parameter iAccount2 as int  no-undo. 
/* credit invoices */
def input parameter iCredit   as log  no-undo. 
/* show interval */
def input parameter iInterval as log  no-undo. 
/* output file */
def input parameter iFile     as char no-undo.
/* qty of invoices */
def output parameter oCount   as int  no-undo. 

def temp-table ttProd no-undo
    field prod   as char
    field inter  as char
    field qty    as int
    field min    as dec 
    field amt    as dec
    field NoVat  AS DEC
    field Acct   as int
    index prod prod Acct Inter.

DEF TEMP-TABLE ttInter NO-UNDO
    FIELD Inter  AS CHAR
    FIELD IInter AS INT
    FIELD Amt    AS DEC
    FIELD Min    AS INT
    FIELD Kpl    AS INT.

def var i        as int  no-undo.
def var xprod    as char no-undo.
DEF VAR ldAmt    AS DEC  NO-UNDO.
DEF VAR llFound  AS LOG  NO-UNDO.

DEF VAR ldMinVel     AS DEC  NO-UNDO.
DEF VAR ldVatFactor  AS DEC  NO-UNDO.
def var lcTypeDenied as char  no-undo.

session:numeric-format = "European".

lcTypeDenied = fCParamC("InvTypeDenied").

output stream excel to value(iFile).

put stream excel unformatted
   "Account"        tab 
   "Product"        tab
   "Name"           tab.

IF iInterval THEN put stream excel unformatted
   "Interval"       tab.

put stream excel unformatted
   "Qty"            tab
   "Min"            tab
   "Amt VAT 0"      tab
   "Amt with VAT"   my-nl.

FOR FIRST CLIType NO-LOCK WHERE
          CLIType.MinimAmt > 0:
   ldMinVel = CLIType.MinimAmt.
END.
 
for each Invoice no-lock where
    Invoice.InvDate  >= iDate1    and
    Invoice.InvDate  <= iDate2    and
    Invoice.InvType  >= iInvType1 and
    Invoice.InvType  <= iInvType2 and 
    Invoice.PrintState >= iState1   and
    Invoice.PrintState <= iState2   and
    (if not iCredit
     then Invoice.CrInvNum = 0
     else true)                     and
    (IF iInvType1 NE iInvType2
     THEN lookup(string(Invoice.InvType),lcTypeDenied) = 0
     ELSE TRUE),
first Customer of Invoice no-lock,
FIRST InvGroup OF Customer NO-LOCK:

    assign oCount = oCount + 1.
    if oCount mod 100 = 0 then do:
        pause 0.
        display oCount label "Qty of invoices"
        with 1 down overlay row 10 centered
            title " Collecting data ".
    end.

    for each InvRow of Invoice no-lock where
             InvRow.SlsAcc >= iAccount1 AND
             InvRow.SlsAcc <= iAccount2:

         assign xprod = InvRow.BillCode
                ldVatFactor = 1 + InvRow.VatPerc / 100.

         EMPTY TEMP-TABLE ttInter.

         /* interval for fees */

         IF iInterval AND InvRow.RowType = 3 THEN DO:

            llFound = FALSE.

            FOR EACH FFItem NO-LOCK WHERE
                     FFItem.InvNum   = Invoice.InvNum     AND
                     FFItem.BillCode = InvRow.BillCode,
               FIRST FixedFee OF FFItem NO-LOCK:

               FIND FIRST ttInter WHERE
                          ttInter.Inter = STRING(FixedFee.Interval) NO-ERROR.
               IF NOT AVAILABLE ttInter THEN DO:
                  CREATE ttInter.
                  ASSIGN ttInter.Inter  = STRING(FixedFee.Interval)
                         ttInter.IInter = FixedFee.Interval.
               END.

               ASSIGN ttInter.Amt = ttInter.Amt + 
                                    FFItem.Amt *
                                        (IF InvRow.Amt < 0
                                         THEN -1 
                                         ELSE 1) *
                                    /* make sure that vat handling is          
                                       the same as for invoice row */
                                    (IF FixedFee.VatIncl NE Invoice.VatIncl
                                     THEN IF FixedFee.VatIncl
                                          THEN 1 / ldVatFactor
                                          ELSE ldVatFactor
                                     ELSE 1)
                      ttInter.kpl = ttInter.kpl + 1
                      llFound     = TRUE.
            END.

            /* FFItems may have been released when credited */
            IF NOT llFound THEN DO:
               CREATE ttInter.
               ASSIGN ttInter.Inter = ""
                      ttInter.Amt   = InvRow.Amt
                      ttInter.kpl   = InvRow.Qty
                      ttInter.min   = InvRow.Minutes.
            END.           

         END.

         ELSE IF iInterval AND InvRow.RowType = 4 THEN DO:
            CREATE ttInter.
            ASSIGN ttInter.Inter = "S"
                   ttInter.Amt   = InvRow.Amt
                   ttInter.kpl   = InvRow.Qty
                   ttInter.min   = InvRow.Minutes.
         END.

         ELSE DO:
            CREATE ttInter.
            ASSIGN ttInter.Inter = ""
                   ttInter.Amt   = InvRow.Amt
                   ttInter.kpl   = InvRow.Qty
                   ttInter.min   = InvRow.Minutes.
         END.

         /* minimum fees that are not "full" are displayed separately */
         if xprod = "MinimVel" and InvRow.Amt lt ldMinVel
         then xprod = "m" + xprod.

         FOR EACH ttInter
         BY ttInter.IInter
         BY ttInter.Inter:

            /* sum without vat */
            ldAmt = ROUND(ttInter.Amt /
                          (IF Invoice.vatincl THEN ldVatFactor ELSE 1),2).

            /* sum with vat */
            IF Invoice.vatincl = FALSE 
            THEN ttInter.Amt = ROUND(ttInter.Amt * ldVatFactor,2).

            /* show events by product by account */
            find first ttProd where
               ttProd.prod  = xprod           and
               ttProd.Acct  = InvRow.SlsAcc and
               ttProd.Inter = ttInter.Inter 
               no-error.

            if not available ttProd then do:
               create ttProd.
               assign ttProd.prod  = xprod
                      ttProd.Acct  = InvRow.SlsAcc
                      ttProd.Inter = ttInter.Inter.
            end.

            assign ttProd.Qty   = ttProd.Qty   + ttInter.kpl
                   ttProd.min   = ttProd.min   + ttInter.min
                   ttProd.Amt   = ttProd.Amt   + ttInter.Amt
                   ttProd.NoVat = ttProd.NoVat + ldAmt.

         END. 

    end.
end.

for each ttProd:

     if ttProd.prod = "mminimvel" then assign xProd = substr(ttProd.prod,2).
     else assign xProd = ttProd.prod.

     FIND BillItem no-lock where 
          BillItem.Brand    = gcBrand AND
          BillItem.BillCode = xProd no-error.

     put stream excel unformatted 
         ttProd.Acct    tab
         xProd          tab
         (if available BillItem 
          then BillItem.BIName
          else "") +
         (if ttProd.prod = "mminimvel"
          then " (osa)"
          else "")       tab.

     IF iInterval THEN put stream excel unformatted
         ttProd.Inter    tab.

     put stream excel unformatted    
         ttProd.Qty      tab
         ttProd.min      tab
         ttProd.NoVat    tab 
         ttProd.amt      my-nl.
end.



