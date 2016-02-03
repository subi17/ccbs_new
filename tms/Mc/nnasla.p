/* ----------------------------------------------------------------------------
  MODULE .......: NNASLA.P
  TASK ..........:Browse customer's invoices                
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 24.02.1997
  MODIFIED .....: 23.03.98 pt more speed on 1. find
                  01.10.98 kl sp-code[1]
                  08.10.98 pt print memo F2
                  13.10.98 pt notes on invoice f3
                  02.12.98 kl estimate Interest with f6
                  08.12.98 pt allow changes with ENTER
                  10.02.99 pt layout, column "DR"
                  10.04.99 kl output modi into nnlryp
                  20.05.99 pt DISP PP IF an invoice is involved in a PP Plan
                  02.09.99 pt change time stamp IF invoice is changed (->KHA)
                  06.09.99 pt DISP PG/BG 
                  14.09.99 pt sp-code[2] : deny reminder fees y/n
                  03.10.99 kl blan for customer's Balance
                  04.10.99 kl DISPlaying P/B fixed
                  08.10.99 kl F4 catch-up when editing
                  13.10.99 kl no-lock with blan
                  18.10.99 kl Invoice.AdvPaym DISPlaying
                  11.01.00 jp DISP interests
                  18.01.00 kl skip .00 in sums
                  19.01.00 kl DISPlay formats
                  29.03.00 pt run invbal
                  19.10.01 kl DISPlay formats
                  06.03.02 lp EPL PaymFile writing
                              must-print loop corrected
                  20.03.02 lp F1: show payment ref.no (umenu # 2042)
                              disp now Invoice.Currency instead of "kr"
                  25.03.02 lp change frame LIS like in the nnlayp.p  
                  27.03.02 lp show Customer.AdvPaym instead Max Limit
                  30.04.02 lp Checking: Dueday cann't be empty
                  30.04.02 tk Eventlogging added
                  17.05.02 tk F2: memo
                  28.05.02 aam use calcint.p FOR Interest calculation
                  07.06.02/aam use Invoice.OverPaym for overpayment
                  11.06.02 lp  corrected Dueday 
                  17.06.02/aam skip invoices defined in parameter InvTypeDenied,
                               use temp-table for invoices
                  20.06.02/jp  Add InvDate for TTInvoice (Browser)             
                  24.07.02/aam show deposit[1],
                               Deposit invoices optionally 
                  26.09.02/aam customer balances in table CustBal 
                  14.10.02/aam DirDisc removed, show always Currency
                  12.11.02/jr  "nnlasku" => "invoice" in memo
                  13.11.02/jr  Removed old memo
                  15.11.02 lp  use invdet.i for invoice details
                  04.12.02/aam message about epl test flag
                  21.01.03/aam CRTime added to index ttinvoice.invdate
                  12.02.03/aam LetterClass
                  03.03.03 tk  tokens
                  13.03.03/aam lcEPLFile for eletterinv
                  21.03.03/aam claiming history
                  11.06.03/aam due date cannot be changed if ddstate > 0       
                  12.09.03/aam brand,
                               don't go directly into update mode with "enter"
                  09.10.03/aam fEPLStart 
                  10.10.03/aam itsendlo
                  13.10.03/aam use si-recid2 to pass invoice to nnlaki,
                               crediting of invoice (nncimu)
                  02.02.04/jp  memo to customer  
                  09.02.04/aam CustNum to showpr
                  04.03.04/aam input orderid 
                  12.03.04/aam payment plan (other actions)
                  05.04.04/aam reduce deposit invoice's amount
                  22.04.04/aam show both types 3 and 4 for special invoices
                  30.04.04/aam fee from invoice copy
                  18.05.04/aam max. addition to due date from DueDateTrans
                  27.05.04/aam invoice type to eletterinv, error from it
                  17.06.04/aam paymplan.ppstatus must be 3
                  23.06.04/aam find with invoice nbr
                  14.07.04/tk memo to invoice, keylabel changed
                  04.11.04/aam don't use "invoice of customer"
                  12.01.05/aam CustNum to calcint.p
                  17.03.05/aam claiming and credit loss to "due"-column
                  07.10.05/aam update ClaimQty, create ClaimHist for state 5
                  13.12.05/aam interest instead of deposit
                  20.12.05/aam interest events (nnkoyp),
                               update letterclass before epl print
                  18.04.06/aam use payments.p instead of nnlasu.p
                  16.06.06/aam ClaimState instead of ClaimQty,
                               EndInvoice
                  29.11.06/aam ExtInvID             
                  12.01.07/aam new input parameter to fGetCustBal
                  06.02.07/aam get all invoices of an order
                  22.03.07 kl  new param for run payments
                  25.04.07/aam eventlog in detail view, 
                               use pInvoiceUpdate
                  22.11.07/aam filtering             
  Version ......: M15
  --------------------------------------------------------------------------- */

&GLOBAL-DEFINE TimeStampDef NO
&GLOBAL-DEFINE TMSCodeDef NO

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Syst/eventval.i}
{Func/fcustbal.i}
{Func/feplstart.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}
{Ar/invdet.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvoice AS HANDLE NO-UNDO.
   lhInvoice = BUFFER Invoice:HANDLE.
   RUN StarEventInitialize(lhInvoice).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhInvoice).
   END.

END.

DEF INPUT PARAMETER iCustNum  AS INT NO-UNDO.
DEF INPUT PARAMETER iiOrderId AS INT NO-UNDO.

def var cd-title     as c               no-undo.

def var memory       as recid           no-undo.
def var line         as int             no-undo.
def var must-print   as log             no-undo.
def var must-add     as log             no-undo.
def var ufkey        as log             no-undo.
def var fr-header    as char.
def var rtab         as recid extent 24 no-undo.
def var i            as int             no-undo.
def var xrecid       as recid.
def var era          as dec             no-undo format "ZZZZZ9.99-".
def var eiera        as dec             no-undo format "ZZZZZ9.99-".
def var plumii       as char            no-undo format "x" init "+".
def var due          as c               no-undo.
def var notes        as lo              no-undo format "M/".
def var Intr         as de              no-undo format "z,zzz,zz9.99-".
def var debt         as de              no-undo format "z,zzz,zz9.99-".
def var idate        as da              no-undo format "99-99-99".
def var inte         as lo              no-undo format "Y/".
def var invbal       as de              no-undo. 
def var blan         like Invoice.InvAmt.
def var NetBal       as de              no-undo format "z,zzz,zz9.99-".
DEF VAR xCount       AS INT NO-UNDO.
DEF VAR xCreditInvNum      AS LOG NO-UNDO INIT Yes.
def var keyp         as lo                  no-undo init yes format "Y/".
DEF VAR ok           AS LOG FORMAT "Yes/No" NO-UNDO.
def var defcurr      as c                   no-undo.
def var cu-text      as c                   no-undo.
DEF VAR oldday       LIKE Invoice.DueDate   NO-UNDO.
DEF VAR qdays        AS INT                 NO-UNDO.
DEF VAR qperc        AS DEC                 NO-UNDO. 

def var lcTypeDenied as char                no-undo.
def var lcTypeUsed   as CHAR                no-undo. 
DEF VAR IntCalcMet   AS INT                 NO-UNDO.

DEF VAR ldCustDP     AS DEC                 NO-UNDO.
DEF VAR ldCustAP     AS DEC                 NO-UNDO.
DEF VAR ldCustOP     AS DEC                 NO-UNDO. 
DEF VAR ldCustCL     AS DEC                 NO-UNDO. 
DEF VAR ldCustInt    AS DEC                 NO-UNDO. 
DEF VAR ldCustRef    AS DEC                 NO-UNDO. 
DEF VAR ldCustPBeh   AS DEC                 NO-UNDO. 
DEF VAR ldNetBal     AS DEC                 NO-UNDO. 
DEF VAR lcTestFlag   AS CHAR                NO-UNDO. 
DEF VAR lcEPLFile    AS CHAR                NO-UNDO.
DEF VAR liLetterClass AS INT                NO-UNDO. 
DEF VAR lcPaymPlan   AS CHAR                NO-UNDO.
DEF VAR ldDepoRed    AS DEC                 NO-UNDO. 
DEF VAR ldDepoTot    AS DEC                 NO-UNDO. 
DEF VAR llCreaFee    AS LOG                 NO-UNDO. 
DEF VAR liDueDate    AS INT                 NO-UNDO. 
DEF VAR ldtChkDate   AS DATE                NO-UNDO. 
DEF VAR lcError      AS CHAR                NO-UNDO.
DEF VAR liInvNum     AS INT                 NO-UNDO.  
DEF VAR ldOldClaim   AS DEC                 NO-UNDO. 
DEF VAR lcDPBal      AS CHAR                NO-UNDO.
DEF VAR lcIntLine    AS CHAR                NO-UNDO.
DEF VAR lcDueLine    AS CHAR                NO-UNDO.
DEF VAR lcNetLine    AS CHAR                NO-UNDO.
DEF VAR lcCustName   AS CHAR                NO-UNDO.
DEF VAR llCredited   AS LOG                 NO-UNDO.
DEF VAR liUseClass   AS INT                 NO-UNDO. 
DEF VAR lcCredited   AS CHAR                NO-UNDO.
DEF VAR menuc        AS CHAR EXTENT 2       NO-UNDO.
DEF VAR liClaimCancel AS INT                NO-UNDO.
DEF VAR lcExtInvId   AS CHARACTER           NO-UNDO FORMAT "X(12)".
DEF VAR lcSelHeader  AS CHAR                NO-UNDO.
DEF VAR lcFilter     AS CHAR                NO-UNDO.
DEF VAR liFilter     AS INT                 NO-UNDO.
DEF VAR lcSelected   AS CHAR                NO-UNDO EXTENT 5. 
DEF VAR liInvQty     AS INT                 NO-UNDO.

/* Interests */
DEF NEW SHARED VAR intpro  AS DE EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intdays AS I  EXTENT 10 NO-UNDO.
DEF NEW SHARED VAR intsumm AS DE EXTENT 10 NO-UNDO.


DEF TEMP-TABLE ttInvoice NO-UNDO
    FIELD InvDate  AS DATE
    FIELD InvNum   AS INT
    FIELD OpenBal  AS DEC
    FIELD CRTime   AS DEC
    FIELD ExtInvID AS CHAR
    /* invoices within one day are shown by descending creation order */
    index InvDate IS Primary InvDate DESC CRTime DESC
    INDEX InvNum  InvNum. 

DEF TEMP-TABLE ttAll NO-UNDO
   LIKE ttInvoice.

{Ar/invfilterkey.i}

assign defcurr       = fCParamC("DefCurrency")
       IntCalcMet    = fCParamI("IntCalcMet")
       lcTypeDenied  = fCParamC("InvTypeDenied")
       lcTestFlag    = fCParamC("EPLTest")
       lcEPLFile     = fCParamC("EPLFile")
       liLetterClass = fCParamI("EPLInvLClass")
       liDueDate     = fCParamI("DueDateTrans").

form
    ldCustOP     
       label "Overpayment Balance" 
       format "zzzz,zz9.99-" at 2
    ldCustREF
       LABEL "Refund Balance" 
       format "zzzz,zz9.99-" at 44
       skip   
    ldCustAP   
       LABEL "Adv.Payment Balance" AT 2 
       FORMAT "zzzz,zz9.99-"
    ldCustCL
       LABEL "Doubtful A/R ."
       format "zzzz,zz9.99-" at 44
       SKIP
    ldCustInt 
       LABEL "Unbilled Interest ." AT 2
       format "zzzz,zz9.99-" 
       SKIP
    blan 
       LABEL "Unpaid Balance ...." AT 2
       format "zzzz,zz9.99-" 
       SKIP
    ldNetBal
       LABEL "Total Balance ....." AT 2
       format "zzzz,zz9.99-" 
    lcPaymPlan FORMAT "X(15)" AT 44 NO-LABEL 
with
    overlay centered width 80 row 1 side-labels
    color value(cfc) title color value(ctc) cd-title
    frame cusdata.

form
    Invoice.ExtInvID   column-label "Invoice Nbr"
       help "Invoice nbr    (ENTER: DETAILS   '?': HELP FOR COLUMN CODES)"
    Invoice.InvDate    column-label "Date"  
    Invoice.DueDate    column-label "Due Date"  
    Invoice.InvAmt     column-label "Amount"       
       format "->>>>>>9.99"
    era                column-label "Unpaid"   
       format "->>>>>>9.99"
    due                column-label "DUE"        format "xxx"
    Invoice.Claimstatus column-label "CS"         format "x(5)"
    Invoice.ClaimPerm  column-label "CD"         format "/CD"
    Invoice.PrintState column-label "PS"         FORMAT ">9"
    Invoice.InvCfg[1]  column-label "PD"         FORMAT "PD/"
    notes              column-label "M"          
    Invoice.InvType    column-label "T"          FORMAT ">9"
with
    width 80 overlay centered scroll 1 8 down row 8
    color value(cfc) title color value(ctc) lcSelHeader frame sel.

form
   "Invoice ..........:" Invoice.InvNum    no-label skip
   "Date .............:" Invoice.InvDate    no-label skip
   "Dueday ...........:" Invoice.DueDate   no-label skip
   "Value ............:" Invoice.InvAmt no-label skip
   "Paid .............:" Invoice.PaidAmt     no-label skip
   "Date for Interest :" idate            no-label skip
   "Interest .........:" Intr             no-label
with centered overlay row 10 title " ESTIMATED Interest "
   frame frm.

FORM
   "Given amount reduces deposit invoice's open balance." AT 2 SKIP(1)
   Invoice.InvAmt AT 2 
      LABEL "Original Amount" 
      FORMAT "->>>>>>9.99"
      SKIP
   era AT 2
      LABEL "Paid .........."
      FORMAT "->>>>>>9.99"
      SKIP
   ldDepoRed AT 2 
      LABEL "Reduction ....."
      HELP "Amount that is reduced from invoice's amount"
      FORMAT "->>>>>>9.99"
      SKIP
   ldDepoTot AT 2
      LABEL "Payable amount "
      FORMAT "->>>>>>9.99"
      SKIP
   WITH ROW 10 CENTERED TITLE " Reduce Amount " SIDE-LABELS OVERLAY
        FRAME fDepoAmt.
      
form 
    "Invoice:" lcExtInvId
       help "Give Invoice Nbr"    
    with row 7 col 2 title color value(ctc) " FIND INVOICE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME F1.

FORM
   Invoice.InvNum     COLON 18 LABEL "Invoice" SKIP
   Invoice.PrintState COLON 18 LABEL "Printing Status" SKIP
   llCredited         COLON 18 LABEL "Credited" 
      FORMAT "Yes/No" SKIP(1)
   liUseClass         COLON 18 LABEL "Letter Class"
      HELP "Letter class"
      VALIDATE(INPUT liUseClass >= 1 AND INPUT liUseClass <= 4,
               "Valid values are 1-4")
   WITH ROW 8 CENTERED TITLE " EPL PRINT " SIDE-LABELS OVERLAY FRAME fEPL. 
      
FUNCTION fIsPaid RETURNS LOGIC
   (idOpenBal   AS DEC,
    iiPaymState AS INT):
    
   RETURN (idOpenBal = 0 AND iiPaymState NE 3).

END FUNCTION.

FUNCTION fCollInv RETURNS LOGICAL.

   IF CAN-FIND(FIRST ttAll WHERE ttAll.InvNum = Invoice.InvNum)
   THEN RETURN FALSE. 
   
   create ttAll.
   assign ttAll.InvDate  = Invoice.InvDate
          ttAll.CRTime   = Invoice.ChgStamp
          ttAll.InvNum   = Invoice.InvNum
          ttAll.ExtInvID = Invoice.ExtInvID.
          ttAll.OpenBal  = fInvBal(BUFFER Invoice,
                                   TODAY).
          blan           = blan + ttAll.OpenBal.
          liInvQty       = liInvQty + 1.

   /* filtering factors */ 
   FOR EACH SubInvoice OF Invoice NO-LOCK:
      DO i = 1 TO 2:
         CASE i:
         WHEN 1 THEN ASSIGN
           lcFilter = SubInvoice.CLI
           liFilter = 0.
         WHEN 2 THEN ASSIGN
           lcFilter = STRING(SubInvoice.MsSeq)
           liFilter = SubInvoice.MsSeq.
         END CASE.
      
         FIND FIRST ttFilter WHERE
                    ttFilter.FType    = i AND
                    ttFilter.FCharKey = lcFilter NO-ERROR.
         IF NOT AVAILABLE ttFilter THEN DO:
            CREATE ttFilter.
            ASSIGN 
               ttFilter.FType    = i 
               ttFilter.FCharKey = lcFilter
               ttFilter.FIntKey  = liFilter.
         END.
         ttFilter.FQty = ttFilter.FQty + 1.  
      END.   
   END.

   DO i = 3 TO 4:
      CASE i:
      WHEN 3 THEN ASSIGN
         lcFilter = STRING(Invoice.InvType)
         liFilter = Invoice.InvType.
      WHEN 4 THEN DO:
         IF fIsPaid(ttAll.OpenBal,Invoice.PaymState)
         THEN lcFilter = "Paid".
         ELSE lcFilter = "Unpaid".
         liFilter = 0.
      END.
      END CASE.
      
      FIND FIRST ttFilter WHERE
                 ttFilter.FType    = i AND
                 ttFilter.FCharKey = lcFilter NO-ERROR.
      IF NOT AVAILABLE ttFilter THEN DO:
         CREATE ttFilter.
         ASSIGN 
            ttFilter.FType    = i 
            ttFilter.FCharKey = lcFilter
            ttFilter.FIntKey  = liFilter.
      END.

      ttFilter.FQty = ttFilter.FQty + 1.  
   END.

END FUNCTION.

FUNCTION fInvoiceCopyFee RETURNS LOGICAL.

   DEF VAR lcInfo AS CHAR NO-UNDO.
   
   ok = TRUE.
   MESSAGE "Create a fee on customer for printing this invoice copy?"
   VIEW-AS ALERT-BOX
   QUESTION 
   BUTTONS YES-NO
   TITLE " INVOICE COPY "
   SET ok.
   
   IF ok THEN 
   RUN creasfee (iCustNum,
                 0,
                 TODAY,
                 "Prints",
                 "InvoiceCopy",
                  2,
                  ?,
                 "",
                 TRUE,
                 katun,
                 "",
                 0,
                 "",
                 "",
                 OUTPUT lcInfo).

END FUNCTION.

FUNCTION fSelHeader RETURNS LOGIC:

   lcSelHeader = "".
   
   IF lcSelected[5] = "ALL" THEN DO: 
      IF LOOKUP("3",lcTypeUsed) > 0 
      THEN lcSelHeader = "Deposit/Adv.Paym INVOICES".
      ELSE DO:
         IF iiOrderID > 0 
         THEN lcSelHeader = "CASH INVOICES".
         ELSE lcSelHeader = "ALL INVOICES".
      END.   
   END.   
   
   ELSE DO:
      DO i = 1 to 4:
         IF lcSelected[i] = "" THEN NEXT.
    
         IF lcSelHeader > "" THEN 
            lcSelHeader = lcSelHeader + ", ".
         CASE i:        
         WHEN 1  THEN lcSelHeader = lcSelHeader + "MSISDN ".
         WHEN 2  THEN lcSelHeader = lcSelHeader + "SUBSCR.ID".
         WHEN 3  THEN lcSelHeader = lcSelHeader + "INV.TYPE".
         WHEN 4  THEN lcSelHeader = lcSelHeader + 
                                    (IF lcSelected[i] = "Unpaid"
                                     THEN "UNPAID"
                                     ELSE "PAID").
         END CASE.

         IF i <= 3 THEN 
            lcSelHeader = lcSelHeader + " " + lcSelected[i].
      END.
      
      lcSelHeader = "FILTER: " + lcSelHeader.
   END.
   
   FRAME SEL:TITLE = " " + lcSelHeader + " ".
   
END FUNCTION.

FUNCTION fInvoiceFilter RETURNS LOGIC
   (iiType   AS INT,
    icFilter AS CHAR):

   DEF VAR llCopy AS LOG  NO-UNDO.
   
   EMPTY TEMP-TABLE ttInvoice.

   IF iiType > 5 THEN RETURN FALSE.
   
   IF INDEX(icFilter,"skip this") > 0 THEN icFilter = "".
   
   CASE iiType:
   WHEN 0 THEN lcSelected = "".
   OTHERWISE   lcSelected[iiType] = icFilter.
   END CASE. 

   lcSelected[5] = "All".
   DO i = 1 TO 4:
      IF lcSelected[i] > "" THEN DO:
         lcSelected[5] = "".
         LEAVE.
      END.
   END.
   
   FOR EACH ttAll,
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = ttAll.InvNum:
   
      llCopy = TRUE.
      
      DO i = 1 to 4:
         IF lcSelected[i] = "" THEN NEXT. 
         
         CASE i:
         /* msisdn */
         WHEN 1 THEN DO:
            IF NOT CAN-FIND(FIRST SubInvoice OF Invoice WHERE
                            SubInvoice.CLI = lcSelected[i]) THEN 
              llCopy = FALSE.
         END.     
         /* subscription id */
         WHEN 2 THEN DO:
            IF NOT CAN-FIND(FIRST SubInvoice OF Invoice WHERE
                         STRING(SubInvoice.MsSeq) = lcSelected[i]) THEN
               llCopy = FALSE.
         END.      
         /* invoice type */
         WHEN 3 THEN IF STRING(Invoice.InvType) NE lcSelected[i] THEN 
            llCopy = FALSE.
         /* payment state */
         WHEN 4 THEN DO:
            /* unpaid */
            IF lcSelected[i] = "Unpaid" AND
               fIsPaid(ttAll.OpenBal,Invoice.PaymState) 
            THEN llCopy = FALSE.
            /* paid */
            ELSE IF lcSelected[i] = "Paid" AND
               NOT fIsPaid(ttAll.OpenBal,Invoice.PaymState) 
            THEN llCopy = FALSE.
         END.         
         END CASE.
      END.
      
      IF llCopy THEN DO:
         CREATE ttInvoice.
         BUFFER-COPY ttAll TO ttInvoice.
      END.
      
   END.

   fSelHeader().
   
   FIND FIRST ttInvoice NO-ERROR.
   memory = RECID(ttInvoice).
   must-print = TRUE.

END.


/* Deposit bills */
IF iCustNum LT 0 THEN DO:
   ASSIGN lcTypeUsed = "3,4"
          iCustNum   = iCustNum * -1.
END.

IF iiOrderID = 0 THEN 
find Customer where Customer.CustNum = iCustNum no-lock.

message "Calculating Balance Due, wait ...".

IF AVAILABLE Customer AND 
   CAN-FIND(FIRST PaymPlan WHERE 
                  PaymPlan.Brand = gcBrand AND
                  PaymPlan.CustNum = Customer.CustNum AND
                  PaymPlan.PPStatus = 3)
THEN lcPaymPlan = "*Paym.plan*".

ASSIGN
   blan          = 0
   lcSelected[5] = "All".
   

/* if orderid given -> deposit invoice from invoice */
IF iiOrderID > 0 THEN DO:
   
   lcCredited = "".

   FOR EACH SingleFee NO-LOCK WHERE
            SingleFee.Brand     = gcBrand        AND
            SingleFee.HostTable = "Order"        AND
            SingleFee.KeyValue  = STRING(iiOrderID),
      FIRST Invoice NO-LOCK WHERE
            Invoice.InvNum = SingleFee.InvNum:
     
      fCollInv().
      
      iCustNum = Invoice.CustNum.
      
      IF LOOKUP(STRING(Invoice.CrInvNum),lcCredited) = 0 THEN 
         lcCredited = lcCredited + 
                      (IF lcCredited > "" THEN "," ELSE "") + 
                      STRING(Invoice.CrInvNum).
             
      FIND Customer OF Invoice NO-LOCK.

   END.

   IF lcCredited > "" THEN DO i = 1 TO NUM-ENTRIES(lcCredited):
      FOR FIRST Invoice NO-LOCK WHERE
                Invoice.InvNum = INTEGER(ENTRY(1,lcCredited)):
         fCollInv().       
      END.
   END.
    
END.
 
ELSE for each Invoice no-lock where
              Invoice.Brand   = Customer.Brand AND
              Invoice.CustNum = Customer.CustNum AND
    /* certain RepType wanted */
   (IF lcTypeUsed NE ""
    THEN LOOKUP(STRING(Invoice.InvType),lcTypeUsed) > 0
    /* printing not denied */
    ELSE lookup(string(Invoice.InvType),lcTypeDenied) = 0
   ):

   fCollInv().

end.

DO i = 1 TO 4:
   CREATE ttFilter.
   ASSIGN 
      ttFilter.FType    = i
      ttFilter.FCharKey = "* Skip this filtering key *"
      ttFilter.FQty     = liInvQty.
END.

fInvoiceFilter(0,"").

PAUSE 0.

/* Checks out IF there are any interests */
FUNCTION inter RETURNS LOGICAL
    (INPUT InvNum AS integer).

   RETURN CAN-FIND(FIRST CustIntEvent WHERE CustIntEvent.InvNum = InvNum).
END FUNCTION.

IF iiOrderID = 0 AND AVAILABLE Customer THEN ASSIGN
   /* get balances */
   ldCustDP   = fGetCustBal(Customer.CustNum,"Total","DP")
   ldCustOP   = fGetCustBal(Customer.CustNum,"Total","OP")
   ldCustAP   = fGetCustBal(Customer.CustNum,"Total","AP")
   ldCustInt  = fGetCustBal(Customer.CustNum,"Total","INT")
   ldCustRef  = fGetCustBal(Customer.CustNum,"Total","REF")
   ldCustPBeh = fGetCustBal(Customer.CustNum,"Total","PBEH"). 

IF ldCustPBeh < 0 THEN plumii = "-". else plumii = "+".

/* slightly different info for adv.payment/deposit invoices */
IF lcTypeUsed = "" THEN DO:

   NetBal = blan - ldCustOP - ldCustAP.

   IF ldCustDP NE 0 
   THEN lcDPBal = "(" + 
                  TRIM(STRING(blan - ldCustOP - ldCustAP - ldCustDP,
                       "->>>>>>9.99")) +
                  ")".
   ELSE lcDPBal = "".              

   ASSIGN 
      lcIntLine = "Interest ..: " + STRING(ldCustInt,"zzzz,zz9.99-")
      lcDueLine = "Balance Due: " + STRING(blan,"zzzz,zz9.99-")  
      lcNetLine = "Net Balance: " + STRING(NetBal,"zzzz,zz9.99-") + "  " +
                                    lcDPBal.
END.

ELSE ASSIGN 
      lcIntLine = "Deposit ...: " + STRING(ldCustDP,"zzzz,zz9.99-")
      lcDueLine = ""
      lcNetLine = "".

IF AVAILABLE Customer THEN DO:

   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                 BUFFER Customer).
   IF LENGTH(lcCustName) > 50 THEN 
      lcCustName = SUBSTRING(lcCustName,1,50).
      
END.

cd-title = " " + 
          (IF AVAILABLE Customer 
           THEN STRING(Customer.CustNum) + " " + lcCustName + " " 
           ELSE "") +
          (IF iiOrderID > 0 
           THEN ", Order " + STRING(iiOrderID)
           ELSE "") +
           " ".
    
ldNetBal = ldCustOP + ldCustAP - ldCustINT - blan.

cfc = "sel". run ufcolor. assign ccc = cfc.

PAUSE 0.
view frame sel.

IF iiOrderID = 0 THEN DO:

   PAUSE 0. 
   view frame cusdata.

   DISPLAY 
      ldCustOP
      ldCustAP
      ldCustINT
      ldCustREF
      ldCustCL
      ldNetBal
      blan
      lcPaymPlan
   with frame cusdata.
END.

find first ttInvoice no-error.
IF available ttInvoice THEN do:
   assign
   memory = recid(ttInvoice)
   must-print = true
   must-add    = false.
end.
else do:
   bell.
   message "This" 
           (IF iiOrderID > 0 
            THEN "order"
            ELSE "customer") +
           " has no" +
           (IF lcTypeUsed > ""
            THEN " deposit/adv.paym "
            ELSE " ") + 
           "invoices !"
   VIEW-AS ALERT-BOX.
   hide frame sel no-pause.
   hide frame cusdata no-pause.
   return.
end.
assign 
xrecid = ? 
ufkey = true.

LOOP:
repeat with frame sel on endkey undo LOOP, next LOOP:

print-line:
   do :
      IF must-print THEN do:
         UP FRAME-LINE - 1.  
         find ttInvoice where recid(ttInvoice) = memory
         no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory */

         repeat with frame sel:
            IF available ttInvoice THEN do:

               RUN local-DISP-row.

               rtab[frame-line] = recid(ttInvoice).
               find next ttInvoice no-error.


            end.
            else do:
               clear no-pause.
               rtab[frame-line] = ?.
            end.
            IF frame-line = frame-down THEN leave.
            down.
         end.
         UP FRAME-LINE - 1. 
         must-print = false.
         PAUSE 0 NO-MESSAGE.

         /* one page of data has been Printed and
         mAllA linellA choosea odotellen. */
      end. /* must-print = true */
   end. /* print-line */

   BROWSE:
   repeat with frame sel on endkey undo, retuRN:

      IF ufkey THEN do:
         assign
         ufk = 0 ufk[8]= 8 ufk[9]= 1 ehto = 3 ufkey = false.
         if keyp then do:
            assign
               ufk[1] = 92
               ufk[2] = 1635.
               
            IF iiOrderID > 0 THEN ufk[2] = 0.   
         end.   
         else assign
            ufk[1] = 639 
            ufk[3] = 1020
            ufk[4] = 1796 /* 1860 */.
         run ufkey.
      end.

      hide message no-pause.
      choose row Invoice.ExtInvID ;(uchoose.i;) no-error with frame sel.
      color DISPlay value(ccc) Invoice.ExtInvID with frame sel.

      assign nap = keylabel(lastkey).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"2,f2,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      /* previous line */
      IF lookup(nap,"cursor-up") > 0 THEN do with frame sel:
         IF frame-line = 1 THEN do:
            find ttInvoice where recid(ttInvoice) = rtab[1] no-lock.
            find prev ttInvoice no-error.
            IF not available ttInvoice THEN do:
               message "This is the first row !".
               bell.
               pause 1 no-message.
               next BROWSE.
            end.
            else do:
               /* a previous one was found */
               scroll down.
               RUN local-DISP-row.

               do i = frame-down to 2 by -1:
                  rtab[i] = rtab[i - 1].
               end.
               assign
               rtab[1] = recid(ttInvoice)
               memory = rtab[1].
            end.
         end.
         else up 1.
      end. /* previous line */

      /* next line */
      else IF lookup(nap,"cursor-down") > 0 THEN do
      with frame sel:

         IF frame-line = frame-down THEN do:
            find ttInvoice where recid(ttInvoice) = rtab[frame-down] no-lock .
            find next ttInvoice no-error.
            IF not available ttInvoice THEN do:
               message "This is the last row !".
               bell.
               pause 1 no-message.
               next BROWSE.
            end.
            else do:
               /* was found vielA seuraava tietue */
               scroll up.
               RUN local-DISP-row.

               do i = 1 to frame-down - 1:
                  rtab[i] = rtab[i + 1].
               end.
               rtab[frame-down] = recid(ttInvoice).
               /* ja lopuksi pannaan memoryin ylimmAn linen avain */
               memory = rtab[1].
            end.
         end.
         else down 1 .
      end. /* next line */

      /* previous page */
      else IF lookup(nap,"prev-page,page-up") > 0 THEN do:
         memory = rtab[1].
         find ttInvoice where recid(ttInvoice) = memory no-lock no-error.
         find prev ttInvoice no-error.

         IF available ttInvoice THEN do:
            memory = recid(ttInvoice).

            /* go back one page */
            do line = 1 to (frame-down - 1):
               find prev ttInvoice no-error.
               IF available ttInvoice THEN memory = recid(ttInvoice).
               else line = frame-down.
            end.
            must-print = true.
            next LOOP.
         end.
         else do:
            /* this is the first data page */
            message "This is the first page !".
            bell.
            pause 1 no-message.
         end.
     end. /* previous page */

     /* next page */
     else IF lookup(nap,"next-page,page-down") > 0 THEN do with frame sel:

        /* cursor to the downmost line */

        IF rtab[frame-down] = ? THEN do:
            message "This is the last page !".
            bell.
            pause 1 no-message.
        end.
        else do: /* the downmost line wasn't empty */
            memory = rtab[frame-down].
            find ttInvoice where recid(ttInvoice) = memory no-lock.
            must-print = true.
            next LOOP.
        end.
     end. /* next page */

     /* find */
     ELSE IF LOOKUP(nap,"1,F1") > 0 AND ufk[1] > 0 AND keyp THEN DO:  
        cfc = "puyr". RUN ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        
        lcExtInvId = "".      
        UPDATE lcExtInvId WITH FRAME F1.     
        HIDE FRAME F1 NO-PAUSE.

        FIND FIRST ttInvoice WHERE
                   ttInvoice.ExtInvID = lcExtInvID NO-ERROR.
                   
        IF NOT AVAILABLE ttInvoice THEN DO:
           MESSAGE "Not found!".
           PAUSE 1 NO-MESSAGE.
           NEXT BROWSE.
        END.

        ASSIGN Memory     = RECID(ttInvoice)
               must-print = TRUE. 
               
        NEXT LOOP.       
     END.
     
     /* filtering */
     ELSE IF LOOKUP(nap,"2,F2") > 0 AND ufk[2] > 0 AND keyp THEN DO:  
        
        ASSIGN 
           ufk      = 0
           ufk[1]   = 1636
           ufk[2]   = 1637
           ufk[3]   = 1638
           ufk[4]   = 1641
           ufk[7]   = 831
           ufk[8]   = 8
           ehto     = 0
           ufkey    = TRUE
           lcFilter = "".

        RUN ufkey.   

        liFilter = IF toimi = 7 THEN 0 ELSE toimi.
        
        IF toimi >= 1 AND toimi <= 4 THEN DO:
           RUN invfilterkey (INPUT TABLE ttFilter,
                             liFilter,
                             OUTPUT lcFilter).
        END.
        
        IF liFilter >= 0 AND liFilter <= 4 THEN DO:
           fInvoiceFilter(liFilter,lcFilter). 
        END.
        
        NEXT loop.
     END.

     else IF lookup(nap,"?") > 0 THEN DO:

        MESSAGE
        "A          Invoice contains Advance Payment" SKIP
        "DUE        Invoice is DUE                  " SKIP
        "RE         No. of reminders sent           " SKIP
        "N          Memo(s) exist                   " SKIP
        "S          Status Code of Invoice          " SKIP
        "DP         Printing is denied (barred)     " SKIP
        "DF         Reminder Fee is denied          " SKIP
        "DR         Reminders are denied            " SKIP
        "I          Overtime Interest due to late p." SKIP
        "Ty         Type of Invoice                 " SKIP
        "           1=Normal       2=Interest       " SKIP
        "           3=Deposit      4=Advance paym   " SKIP
        VIEW-AS ALERT-BOX
        TITLE " EXPLANATION OF COLUMN CODES ".
        NEXT.
     END.

     else IF lookup(nap,"1,f1") > 0 AND NOT keyp THEN do:
        find ttInvoice where recid(ttInvoice) = rtab[frame-line(sel)]
        no-lock no-error.
        find Invoice where Invoice.InvNum = ttInvoice.InvNum no-lock.
        
        assign memory    = recid(ttInvoice).
               si-recid2 = recid(Invoice).

        llCreaFee = (Invoice.PrintState > 0).
        
        run nnlaki.
        
        IF llCreaFee THEN fInvoiceCopyFee().

        assign
        must-print = true
        ufkey      = true
        si-recid2  = ?. 
        
        next LOOP.
     end.


     else IF lookup(nap,"3,f3") > 0 AND NOT keyp AND ufk[3] > 0
     THEN do:
        find ttInvoice where recid(ttInvoice) = rtab[frame-line(sel)]
        no-lock no-error.

        find Invoice where Invoice.InvNum = ttInvoice.InvNum no-lock.
        ASSIGN llCredited = (Invoice.CrInvNum > 0)
               liUseClass = liLetterClass.
        
        PAUSE 0.
        DISP Invoice.InvNum 
             Invoice.PrintState
             llCredited WITH FRAME fEPL.
        
        ASSIGN ehto  = 9
               ufkey = TRUE.
        RUN ufkey.
        REPEAT WITH FRAME fEPL ON ENDKEY UNDO, LEAVE:
           UPDATE liUseClass.
           LEAVE.
        END.
        
        HIDE FRAME fEPL NO-PAUSE.
        
        IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
        KEYLABEL(lastkey) = "F4" THEN NEXT.

        ehto = 5.
        RUN ufkey.
        
        IF fEPLStart(lcTestFlag) THEN DO:
        
           llCreaFee = (Invoice.PrintState > 0).
        
           RUN eletterinv(INPUT Invoice.InvNum,
                          INPUT Invoice.InvNum,
                          INPUT Invoice.InvDate,
                          INPUT "",
                          INPUT Invoice.CustNum,
                          INPUT FALSE,
                          INPUT TRUE,
                          INPUT TRUE,
                          INPUT Invoice.InvType,
                          INPUT liUseClass,
                          INPUT lcEPLFile,
                          OUTPUT xCount,
                          OUTPUT lcError).
           
           IF xCount > 0 THEN DO:
              MESSAGE "EPL file is done for invoice" Invoice.InvNum
              VIEW-AS ALERT-BOX.
      
              IF llCreaFee THEN fInvoiceCopyFee().
           END.   
              
           ELSE DO:
              MESSAGE "Printing failed;" SKIP
                      lcError
              VIEW-AS ALERT-BOX
              ERROR.
           END.   
        END.

        NEXT LOOP.
     END.

     else IF nap = "T" AND keyp THEN DO:
        ASSIGN ufk = 0 ehto = 3.
        RUN ufkey.
        find ttInvoice where recid(ttInvoice) = rtab[frame-line] NO-LOCK.
        find Invoice where Invoice.InvNum = ttInvoice.InvNum no-lock.
        MESSAGE 
        "Invoice Created/Changed" Invoice.ChgStamp "/" 
        "Exported" Invoice.ExpStamp.
        MESSAGE "Press ENTER !".
        PAUSE No-message.
        ufkey = TRUE.
        NEXT.

     END.

     else IF lookup(nap,"F7,7") > 0 AND ufk[7] > 0 AND keyp THEN DO:
        keyp = No.
        ufkey = true.
        next LOOP.
     end.        

     else IF lookup(nap,"enter,return") > 0 AND keyp THEN DO:

        FIND ttInvoice WHERE recid(ttInvoice) = rtab[frame-line(sel)].

        FIND Invoice WHERE Invoice.InvNum = ttInvoice.InvNum NO-LOCK.
        
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvoice).
        
        LP:
        REPEAT WITH FRAME fInvDet
        on endkey undo, LEAVE LP:

           /* show details */
           RUN pInvoiceDetails(ttInvoice.InvNum,
                               FALSE).

           RUN pInvoiceUpdate(ttInvoice.InvNum).
           
           IF RETURN-VALUE = "QUIT" THEN LEAVE.
        END.

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvoice).
        
        HIDE FRAME fInvDet NO-PAUSE.
        
        ASSIGN 
           xrecid = recid(ttInvoice)
           ufkey  = TRUE.
         
        DISPLAY Invoice.DueDate 
                Invoice.ClaimStatus
        WITH FRAME sel.
        
        NEXT LOOP.
     END.

     else IF lookup(nap,"home,h") > 0 THEN do:
        find first ttInvoice no-error.
        assign
        memory = recid(ttInvoice)
        must-print = true.
        next LOOP.
     end.

     else IF lookup(nap,"end,e") > 0 THEN do : /* last record */
        find last ttInvoice no-error.
        assign
        memory = recid(ttInvoice)
        must-print = true.
        next LOOP.
     end.

     else IF lookup(nap,"8,f8") > 0 AND keyp THEN leave LOOP.

     else IF lookup(nap,"8,f8") > 0 AND NOT keyp THEN DO:
        must-print = true.
        keyp = yes.
        ufkey = true.
        next LOOP.
     end.
  end.  /* BROWSE */
end.  /* LOOP */

hide frame sel no-pause.
hide frame cusdata no-pause.
si-recid = xrecid.

PROCEDURE local-DISP-row:

   find Invoice where Invoice.InvNum = ttInvoice.InvNum no-lock.
   era = fInvBal(BUFFER Invoice,
                 TODAY).

   assign notes = CAN-FIND(FIRST memo WHERE
                                 memo.Brand     = Invoice.Brand AND
                                 memo.Custnum   = Invoice.Custnum AND
                                 memo.HostTable = "Invoice" AND
                                 memo.KeyValue  = STRING(Invoice.InvNum)).

   /* status of claiming / credit loss / due balance */
   due = "".
   IF Invoice.ClaimState = 10 THEN DO:
      /* claiming cancelled */
      IF Invoice.ClaimCancel > 0 THEN DO:
         IF Invoice.PaymState = 3 THEN due = "CRL".
      END.
      /* still unpaid or credit loss */
      ELSE IF era NE 0 OR Invoice.PaymState = 3 
           THEN due = "AKF".
   END.
   
   ELSE DO:
      /* involved in an active payment plan */
      IF Invoice.PaymState = 4 THEN DO:
         due = "PPC".
         FOR EACH PPInv NO-LOCK WHERE
                  PPInv.InvNum = Invoice.InvNum,
            FIRST PaymPlan OF PPInv NO-LOCK WHERE
                  PaymPlan.PPStatus = 3:
                  
            due = "PPA".
            LEAVE.
         END.
      END.
      ELSE IF Invoice.PaymState = 3 THEN due = "CRL".
      ELSE IF ERA NE 0 AND Invoice.DueDate < pvm THEN due = "!!!".
   END. 

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY Invoice.ExtInvID 
           Invoice.InvDate 
           Invoice.DueDate 
           Invoice.ClaimStatus 
           Invoice.InvAmt
           due 
           era
           notes 
           Invoice.PrintState
           Invoice.InvCfg[1]
           Invoice.ClaimPerm 
           Invoice.InvType
           WITH FRAME sel.

END PROCEDURE.

PROCEDURE pCancellation.
   
   IF Invoice.ClaimState = 15 THEN DO:
      MESSAGE "Claim cancel already set!" VIEW-AS ALERT-BOX.
      LEAVE.
   END.
   
   /* Cancel form */
   DO WHILE TRUE:
               
      ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN ufkey.
                               
      DISPLAY   SKIP(1)
                " 1) Cancelled by operator         " @ menuc[1]
                SKIP
                " 2) Cancelled by collection agency" @ menuc[2]
                SKIP(1)
                WITH OVERLAY FRAME choices NO-LABELS.

      CHOOSE FIELD menuc AUTO-RETURN go-on (F8)
      WITH FRAME choices
      TITLE "CANCEL COLLECT" CENTERED WITH COL 2 ROW 10.

      HIDE FRAME choices.
                 
      IF LOOKUP(KEYLABEL(LASTKEY),"8,F8") > 0  THEN LEAVE.
                 
      IF FRAME-INDEX EQ 1      THEN liClaimCancel = 1.
      ELSE IF FRAME-INDEX EQ 2 THEN liClaimCancel = 2.   
      
      MESSAGE "Please make sure collection agency is aware" SKIP
              "of cancellation.                           " SKIP (1)
              "Manual cancellation of collection will not " SKIP
              "send any data to collection agency.        " SKIP (1)
              "Are you sure you want to continue ? "    
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
      TITLE "" UPDATE choice AS LOGICAL.
   
      CASE choice:
         
         WHEN TRUE THEN DO:
               
            FIND CURRENT Invoice EXCLUSIVE-LOCK.
            
            ASSIGN Invoice.ClaimCancel = liClaimCancel
                   Invoice.ClaimState  = 15.
                                     
            CREATE ClaimHist.
            ASSIGN ClaimHist.Brand      = Invoice.Brand
                   ClaimHist.InvNum     = Invoice.InvNum
                   ClaimHist.CustNum    = Invoice.CustNum
                   ClaimHist.ClaimState = Invoice.ClaimState
                   ClaimHist.Claim      = 10 * Invoice.ClaimState
                   ClaimHist.Memo       = "Claiming cancelled"
                   ClaimHist.ClaimDate  = TODAY
                   ClaimHist.ClaimAmt   = Invoice.InvAmt - Invoice.PaidAmt
                   ClaimHist.Handler    = katun.
               
            MESSAGE "Claiming cancelled!" VIEW-AS ALERT-BOX.
            liClaimCancel = 0.
            LEAVE.
         END.
         WHEN FALSE THEN DO:
            MESSAGE "Nothing done!" VIEW-AS ALERT-BOX.
            LEAVE.
         END.
      END CASE.
   END.
END.
                               
