/* ---------------------------------------------------------------------------
  MODULE .......: NNLAMU5.P
  FUNCTION .....: Laskujen muodostus Niin kiinteästä kuin fixedistä
  APPLICATION ..: NN
  AUTHOR .......: PT/TT
  CREATED ......: 06.11.96
  MODIFIED .....: 27.03.01
                  13.07.01 kl: bIncVAT (from PriceList.InclVAT)
                  20.07.01 jp: mobile AND miniminvoice VALUE
                  09.08.01 jp: browsing old mob Calls
                  27.03.02 ht: menu-selectable, without InvRunLog logic
                  02.04.02 lp - calculate invoices amount (llkm)
                              - same MESSAGE FOR monitoring program's work
                  08.04.02 aam lamt already includes Qty of invoices ->
                               llkm NOT needed 
                  09.04.02 lp INPUT PARAMETER CustNum1
                  15.04.02 aam: get also the total VALUE from pGetAmt AND
                                show billing statistics
                  26.04.02 kl:  pUpdInvGroup instead of pUpdInvSeq
                  04.09.02/aam  billrund.i
                  10.09.02/aam  check period 
                  09.10.02/jp   Banned Check for invgroup
                  22.10.02/aam  delete persistent procedure
                  11.03.03/tk   tokencheck
                  15.04.03/aam  nnlamu5; deposit invoice
                  11.09.03/aam  brand
                  19.01.04/aam  silent mode 
                  23.04.04/aam  iiInvtype passed to pDepositInvoice
                  09.06.04/aam  iiInvType can be -1 (first invoice),
                                icMSSeq (pass to pDepositInvoice)
                  23.06.04/aam  payment term instead of extra days             
                  23.08.04/aam  orderid to pDepositInvoice
                  21.09.05/aam  invoice nbr with fGetInvNum(),
                                don't update invoice nbr here
                  27.11.06/aam  cash invoice (iiInvType = 6)              
                  28.11.06/aam new logic for fGetInvNum()
                  16.08.07/aam tokenlib removed
   Version ......: M15
  -------------------------------------------------------------------------- */

/* don't initialize rerate etc. */
&GLOBAL-DEFINE InitPersistent NO
&GLOBAL-DEFINE GetAllParams NO

{Syst/commali.i}
{Func/cparam2.i}
{Func/faccper.i}

DEF INPUT  PARAMETER  CustNum1  LIKE Customer.CustNum NO-UNDO.
DEF INPUT  PARAMETER  iiOrderID AS INT                NO-UNDO. 
DEF INPUT  PARAMETER  icMSSeq   AS CHAR               NO-UNDO. 
DEF INPUT  PARAMETER  iiInvType AS INT                NO-UNDO.  
DEF INPUT  PARAMETER  ilSilent  AS LOG                NO-UNDO.  
DEF OUTPUT PARAMETER  olQty     AS INT                NO-UNDO. 

def var invDte     as Date format "99-99-99"     NO-UNDO init TODAY.
def var lcInvNum   as char format "x(12)"        NO-UNDO.
def var ok         as log  format "Yes/No"       NO-UNDO.
DEF VAR InvGroup   LIKE InvGroup.InvGroup        NO-UNDO.
DEF VAR b-acc      AS lo                         NO-UNDO.
DEF VAR ciperiod   AS i                          NO-UNDO.
def var i          as int  format "zzzzzzz9"     NO-UNDO.

def var atpvm1        as Date format "99-99-99"   NO-UNDO.
def var atpvm2        as Date format "99-99-99"   NO-UNDO.
def var CustNum2      as int  format "zzzzzzz9"   NO-UNDO.
DEF VAR mininv        LIKE InvGroup.MinInvAmt     NO-UNDO.
DEF VAR upmth         LIKE InvGroup.UnbilledLimit NO-UNDO.
DEF VAR kysy_rajat    AS LOG                      NO-UNDO.

DEF VAR unknown    AS i    NO-UNDO.
DEF VAR not-inv    AS DE   NO-UNDO.
def var mark       as lo   no-undo format "Yes/No" init TRUE.
def var lowvalue   as lo   no-undo format "Yes/No".
DEF VAR extratime  AS i    NO-UNDO.    
DEF VAR lVatAmt    AS DE   NO-UNDO. 
DEF VAR llInvType  AS LOG  NO-UNDO.
DEF VAR lcItem     AS CHAR NO-UNDO.
DEF VAR liPaymTerm AS INT  NO-UNDO. 
DEF VAR lcPrefix   AS CHAR NO-UNDO.
DEF VAR ldTotal    AS DEC  NO-UNDO.

{Func/tmsparam.i oh-tuasno  RETURN}. unknown = TMSParam.IntVal.

{lamupers.p NEW}

IF NOT ilSilent THEN DO:

form
   skip(17)
WITH
   OVERLAY TITLE COLOR value(ctc)
   " " + ynimi + " INVOICING " + string(pvm,"99-99-99") + " "
   COLOR value(cfc) width 80
   FRAME taka.

form
                                                skip(1)
   InvGroup label  " Invoice group .........."
           help "Invoice group's code"
   InvGroup.IGName no-label format  "x(16)"    SKIP
   CustNum1   label  " Customer number ........"
           help "Customers FROM number"
   "-"
   CustNum2   no-label help "Customers TO number"  SKIP

   llInvType 
           LABEL " Invoice Type ..........."
           HELP "(D)eposit invoice or (A)dvance payment invoice"
           FORMAT "Deposit/Adv.payment"
           SKIP
           
   invDte  label " Invoice Date ..........."
           help "Date of invoicing for all invoices"
                                                SKIP
   liPaymTerm label " Payment Term (days) ...." FORMAT "z9"
           help "Days from invoice date"
                                                SKIP
   mark    label " Approve automatically .."
           help "Approve all invoices automatically (Yes/No) ?" 
                                                SKIP(1)    
with title color value(ctc) " CRITERIA FOR CREATING INVOICES " side-labels
   COLOR value(cfc) ROW 3 centered OVERLAY FRAME rajat.

form
    SKIP(1)
    " Consecutive invoice number: " lcInvNum  NO-LABEL           
    SKIP(1)
WITH
   title color value (ctc) " INVOICE GROUP DATA " COLOR value(cfc)
   OVERLAY centered ROW 14 FRAME lCustNum.

   cfc = "sel". RUN Syst/ufcolor. ccc = cfc.
   view FRAME taka. PAUSE 0 no-message.

   cfc = "lis". RUN Syst/ufcolor.
   ehto = 9. RUN Syst/ufkey.
   
   liPaymTerm = 9. 
END.
ELSE liPaymTerm = 0.

ASSIGN
atpvm2 = TODAY
atpvm1 = TODAY
ciperiod = year(TODAY) * 100 + month(TODAY)
llInvType = (iiInvType = 3).

/* special case; first invoice to customer including monthly payments */
IF iiInvType = -1 THEN DO:
   
   lcItem = fCParamC("FirstInvItem").
   IF lcItem = ? OR lcItem = "" THEN RETURN. 
   
   ciperiod = 0.
   /* take first partial month and first full one */
   FOR EACH FixedFee NO-LOCK WHERE
            FixedFee.Brand   = gcBrand  AND
            FixedFee.CustNum = CustNum1 AND
            LOOKUP(FixedFee.BillCode,lcItem) > 0:
      
      /* if something has been billed -> cancel this invoice */
      IF CAN-FIND(FIRST FFItem OF FixedFee WHERE FFItem.Billed = TRUE) 
      THEN DO:
         ciperiod = 0.
         LEAVE.
      END.
      
      i = 0.
      FOR EACH FFItem OF FixedFee NO-LOCK
      BY FFItem.BillPeriod:
         ASSIGN i        = i + 1
                ciperiod = MAX(ciperiod,FFItem.BillPeriod).
         IF i = 2 THEN LEAVE.
      END.
   END.
          
   IF ciperiod = 0 THEN RETURN.         
   
   iiInvType = 1.
END.

ELSE IF iiInvType < 3 THEN 
ciperiod = IF MONTH(TODAY) = 1
           THEN (YEAR(TODAY) - 1) * 100 + 1
           ELSE year(TODAY) * 100 + month(TODAY) - 1.

IF CustNum1 = 0 THEN 
ASSIGN
   CustNum1  = unknown + 1
   CustNum2  = 99999999.

ELSE DO:
   FIND Customer WHERE Customer.CustNum = CustNum1 NO-LOCK NO-ERROR.
   FIND InvGroup OF Customer NO-LOCK NO-ERROR.
   IF NOT AVAIL InvGroup THEN DO:
      IF NOT ilSilent THEN 
      MESSAGE
      "Customer's invoice group code is not defined "
      VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE IF invgroup.banned = TRUE THEN DO:
      ok = FALSE.
      
      IF NOT ilSilent THEN 
      MESSAGE
      "This customer belongs to invoice group '" invgroup.invgroup "' as " 
                                                 InvGroup.igname   SKIP
      "and this invoice group is normally not invoiced at all (banned). "     
                                                                         SKIP(2)
      "Do you still want to create invoice for customer"                 SKIP
      Customer.CustNum Customer.CustName " ?"
      VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
      IF ok = FALSE THEN RETURN.
   END.

   /* consecutive invoice number */
   lcInvNum  = fGetInvNum(InvGroup.InvGroup,
                          iiInvType,
                          TODAY,
                          OUTPUT lcPrefix).

   ASSIGN 
   CustNum2 = CustNum1
   InvGroup = Customer.InvGroup
   InvDte   = TODAY
   /* shall we UPDATE accounts */
   b-acc    = InvGroup.UpdCustBal 
   /* MINIMUM invoicing Qty */
   mininv   = InvGroup.MinInvAmt
   /* unpaid MONTH Limit */
   upmth    = InvGroup.UnbilledLimit.

   IF ilSilent THEN DO:

      /* check period */
      IF fPeriodLocked(InvDte,FALSE) THEN RETURN.

      /* reject IF InvNum is ZERO */
      IF lcInvNum = "" THEN RETURN "Invnum is zero".
   END.
   
END.

IF NOT ilSilent THEN DO:
   view FRAME lCustNum.

   DISPLAY lcInvNum WITH FRAME lCustNum.
   PAUSE 0 no-message.

   kysy_rajat = TRUE.

   PAUSE 0.

   toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, RETURN:
      IF kysy_rajat THEN DO:
         /* We ask the limits */
         ehto = 9. RUN Syst/ufkey.
         UPDATE
            InvGroup
            CustNum1 CustNum2   validate(INPUT CustNum2  >= INPUT CustNum1,
                          "Impossible Customer number limitation!")
            llInvType              
         invDte liPaymTerm
         mark
         WITH FRAME rajat EDITING :
            READKEY. nap = keylabel(LASTKEY).
            IF lookup(nap,poisnap) > 0 THEN DO:
               HIDE MESSAGE no-pause.

               if frame-field = "InvGroup" THEN DO:
                  ASSIGN FRAME rajat InvGroup.
                  if InvGroup = "" THEN DO:
                     HIDE FRAME rajat no-pause.
                     HIDE FRAME lCustNum no-pause.
                     HIDE FRAME taka  no-pause.
                     RETURN.
                  END.

                  FIND InvGroup where 
                       InvGroup.Brand    = gcBrand AND
                       InvGroup.InvGroup = InvGroup
                  no-lock no-error.
                  IF NOT AVAIL InvGroup THEN DO:
                     bell.  message "Unknown invoice group !".
                     NEXT.
                  END.
                  DISP InvGroup.IGName WITH FRAME rajat.
                  PAUSE 0.
 
                  /* consecutive invoice number */
                  lcInvNum  = fGetInvNum(InvGroup.InvGroup,
                                         1,
                                         INPUT  INPUT InvDte,
                                         OUTPUT lcPrefix).
                  
                   ASSIGN
                     /* shall we UPDATE accounts */
                     b-acc  = InvGroup.UpdCustBal 
                     /* MINIMUM invoicing Qty */
                     mininv = InvGroup.MinInvAmt
                     /* unpaid MONTH Limit */
                     upmth  = InvGroup.UnbilledLimit.
                  IF lcInvNum = "" THEN DO:
                     message "NOTICE: Invoice number is ZERO !".
                     PAUSE no-message.
                     NEXT.
                  END.
                  ELSE
                     DISP lcInvNum  WITH FRAME lCustNum.
               END.

               ELSE IF FRAME-FIELD = "CustNum1" THEN ASSIGN CustNum1.

               ELSE IF FRAME-FIELD = "CustNum2" THEN DO:
                  IF INPUT CustNum2 = 0 THEN DO:
                     CustNum2 = INPUT CustNum1.
                     DISP CustNum1 @ CustNum2 WITH FRAME rajat.
                     ASSIGN 
                        CustNum1
                        CustNum2.
                  END.
                  IF CustNum2 < CustNum1 THEN DO:
                     MESSAGE 
                        "Impossible customer number limitation"
                     VIEW-AS ALERT-BOX error.
                     NEXT.
                  END.   
               END.

               ELSE IF FRAME-FIELD = "invDte" THEN DO:
                  IF INPUT invDte > TODAY THEN MESSAGE 
                     "You should NOT use an invoice date" SKIP
                     "other than today !"
                  VIEW-AS ALERT-BOX WARNING.
               END.

            END.
            APPLY LASTKEY.
         END.

         if input CustNum2  = "" THEN CustNum2  = 9999999.

         kysy_rajat = FALSE.
      END.

      ASSIGN ufk = 0 ufk[1] = 132 ufk[2] = 0 
                     ufk[4] = 0 ufk[5] = 795
                     ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN DO:
         kysy_rajat = TRUE.
         NEXT toimi.
      END.

      IF toimi = 5 THEN DO:

         /* check period */
         IF fPeriodLocked(InvDte,TRUE) THEN NEXT toimi.

         /* deposit or adv.payment */   
         IF lcItem = "" THEN DO:
            IF llInvType THEN iiInvType = 3.
            ELSE iiInvType = 4.
         END.
         
         /* reject IF InvNum is ZERO */
         IF lcInvNum = "" THEN DO:
            message "NOTICE: Invoice number is ZERO !".
            PAUSE no-message.
            NEXT toimi.
         END.

         LEAVE toimi.

      END.

      IF toimi = 8 THEN DO:
         HIDE MESSAGE no-pause.
         HIDE FRAME rajat no-pause.
         HIDE FRAME taka no-pause.
         RETURN.
      END.

   END. /* toimi */

   HIDE FRAME lCustNum no-pause.


   /* We make it THRU ALL the Calls, what we wanted TO handle */
   message "Sorting customers and Calls ...".
END.    
   
XCUST:
FOR EACH Customer   no-lock  where
         Customer.Brand     = gcBrand     AND
         Customer.CustNum  >= CustNum1    AND
         Customer.CustNum  <= CustNum2    AND
         Customer.InvGroup  = InvGroup,

   FIRST InvGroup OF Customer NO-LOCK:

   IF CustNum1 NE CustNum2 AND
      Customer.CustNum < unknown 
   THEN NEXT. 
    
   IF NOT ilSilent THEN 
   PUT SCREEN ROW 23 col 1 
      string(Customer.CustNum) + " " + Customer.CustName + fill(" ",20).

   CREATE ttInvCust.
   ASSIGN 
      ttInvCust.CustNr  = Customer.CustNum
      ttInvCust.MinInv  = InvGroup.MinInvAmt
      ttInvCust.CallQty = 0
      ttInvCust.LowVal  = TRUE.

END.


FIND FIRST ttInvCust NO-LOCK NO-ERROR.
IF NOT AVAIL ttInvCust THEN DO:
   IF NOT ilSilent THEN 
   MESSAGE 
      "No invoices can be created using given criteria !"
   VIEW-AS ALERT-BOX MESSAGE.
   RETURN.
END.

/* cash invoice */
IF iiInvType = 6 OR iiInvType = 7 THEN 
   RUN pCashInvoice           (iiInvType,
                               iiOrderID, 
                               icMSSeq,
                               invDte,
                               atpvm1,
                               atpvm2,
                               ciperiod,
                               extratime,
                               liPaymTerm,
                               ilSilent).

ELSE 
   RUN pDepositInvoice           (iiInvType,
                                  iiOrderID, 
                                  icMSSeq,
                                  invDte,
                                  atpvm1,
                                  atpvm2,
                                  ciperiod,
                                  extratime,
                                  liPaymTerm,
                                  ilSilent).

ok = TRUE.
IF NOT ilSilent THEN DO:

   /* IF automatic approving is NOT allowed */
   if not mark 
   then message "Do You want to approve invoicing (Y/N) ?" UPDATE ok.
END.

RUN pGetAmt   (OUTPUT olQty,
               OUTPUT ldTotal,
               OUTPUT lVatAmt).

/* approve invoice RUN */
IF ok AND olQty > 0 THEN DO: 
    RUN pUpdInvGroup.
    
    IF NOT ilSilent THEN 
    MESSAGE olQty 
            "invoices were created." SKIP
            "Total amount excluding VAT was" ldTotal SKIP
            "Total payable amount, including VAT and reductions was" lVatAmt
    VIEW-AS ALERT-BOX
    title " Billing statistics " .
END.

/* remove ALL created invoices */
ELSE IF olQty NE 0 THEN RUN pCancel.
/* no NEW invoices */
else IF NOT ilSilent THEN 
message "No invoice(s) were created !" VIEW-AS ALERT-BOX MESSAGE.

/* clear persistent procedures */
RUN pCleanMem .

IF NOT ilSilent THEN DO:

   HIDE MESSAGE no-pause.
   put screen row 22 col 60 "               ".
   HIDE FRAME rajat no-pause.
   HIDE FRAME LOG no-pause.
   HIDE FRAME taka no-pause.

END.


