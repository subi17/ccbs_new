/* -----------------------------------------------
  MODULE .......: NNTRDT
  FUNCTION .....: Transit call details
  APPLICATION ..: TN
  AUTHOR .......: KL
  CREATED ......: 02-10-01
  MODIFIED .....: 

  Version ......: M15
  SHARED .......: si-recid   TranCDR RECID
  ------------------------------------------------------ */

{commali.i}

def var kloa     as char format "x(8)"  NO-UNDO.
def var klop     as char format "x(8)"  NO-UNDO.
def var ckesto   as char format "x(8)"  NO-UNDO.
def var asnimi-s as char format "x(30)" NO-UNDO.
def var asnimi-l as char format "x(30)" NO-UNDO.
def var asnimi-r as char format "x(30)" NO-UNDO.
def var asnimi-a as char format "x(30)" NO-UNDO.
def var tunimi   as char format "x(30)" NO-UNDO.
def var btnro    as char format "x(25)" NO-UNDO.
def var btnimi   as char format "x(20)" NO-UNDO.
def var maanimi  as char format "x(10)" NO-UNDO.
DEF VAR atyyp    AS CHAR                NO-UNDO.
def var b-c-sub  as char format "x(30)" NO-UNDO.
DEF VAR punetto  LIKE TranCDR.GrossPrice.
DEF VAR vpv       AS c   NO-UNDO.
DEF VAR vp        AS c   NO-UNDO.
DEF VAR putrunkin AS c   NO-UNDO.
DEF VAR putrunk   AS c   NO-UNDO.
DEF VAR xtitle    AS c   NO-UNDO.
DEF VAR i         AS i   NO-UNDO.
DEF VAR l         AS i   NO-UNDO.
def var unit      as c   no-undo format "x(26)".
DEF VAR unit-cur  AS c   NO-UNDO.
DEF VAR unit-sec  AS c   NO-UNDO.
def var cucode1   as c   no-undo format "x(3)".
def var cucode2   as c   no-undo format "x(3)".

vpv = "sunday,monday,tuesday,wednesday,thursday,friday,saturday".
ASSIGN
   unit-cur = "Starting fee ..: "
   unit-sec = "Minimum seconds: ".

form
   TranCDR.Date   label "Date ........."  format "99-99-9999"
   vp format "x(9)" NO-LABEL
      kloa        label "Started ......." AT 51    SKIP
   TranCDR.Duration label "Length s ....."  format "zzz,zz9"
     ckesto       label "Length hh:mm:ss" AT 51 SKIP
   TranCDR.CustNum  label "A-subs. custnr" space(2) asnimi-s 
     no-label format "x(22)"
   klop           label "Ended ........." AT 51                        SKIP
   TranCDR.InvCust   label "Invoice custnr" space(2) asnimi-l 
     no-label  format "x(22)"
   putrunkin format "x(11)"
                   label "Exch./CGR IN   " AT 51                      SKIP
   TranCDR.RepCust   label "Report  custnr" space(2) asnimi-r 
     no-label  format "x(22)"
   putrunk  label "Exch./CGR OUT  " at 51 format "x(11)"                SKIP
   TranCDR.RateCust   label "Discount.   custnr" space(2) asnimi-a 
     no-label  format "x(24)"
     /*
   TranCDR.pu-npi    label "NPI ..........." at 51 "TON" TranCDR.pu-ton NO-LABEL
   */
   SKIP
                                           SKIP
   TranCDR.CLI   label "A-sub number.."  
   TranCDR.Prefix label "Prefix ........" format "x(5)" AT 51
   SKIP

   /*
   pu-tpi label "TPI" format "x(16)" AT 51
   SKIP
   */
   b-c-sub            label "B / C -sub nr." btnimi NO-LABEL AT 51       SKIP
   TranCDR.CCN       label "Rep.land ....." maanimi format "x(23)" NO-LABEL
   TranCDR.SpecialPrice[1] label "Normal Price   " format "zz,zz9.99" AT 51   SKIP
   TranCDR.BillCode      label "Inv. BillCode ."
   format "x(7)" tunimi no-label format "x(10)"
   unit       NO-LABEL AT 51   SKIP /* starting fee OR MINIMUM seconds */
"------------------------------------------------------------------------------"
  "Time Bands              1         2         3         4         5    W-end"
  SKIP
  "  -Length units" TranCDR.TBDuration format "z,zzz,zz9" NO-LABEL        SKIP
  "  -Price / unit" TranCDR.TBPrice                    NO-LABEL        SKIP
"------------------------------------------------------------------------------"

  "Gross pr:" TranCDR.GrossPrice no-label cucode1 no-label "Net:" punetto NO-LABEL
  "Type:"     TranCDR.TariffType  NO-LABEL
  format "General         (G)/Special         (S)"
  TranCDR.Priced   no-label format "Invoicable  /Not Invoicable" TO 78  SKIP
  "Discount: " TranCDR.DiscValue no-label cucode2 no-label "DirDiscPerc .:" TranCDR.Disc%      NO-LABEL
  "Type:" atyyp no-label format "x(15)"
  InvSeq.Billed   no-label format "Billed    /Not Billed  " TO 78 SKIP
   SKIP

WITH centered ROW 1 COLOR value(cfc) TITLE COLOR value(ctc) xtitle

   side-labels OVERLAY FRAME puh.

FIND TranCDR where recid(TranCDR) = si-recid no-lock.

FIND FIRST InvSeq where
           InvSeq.InvSeq = TranCDR.InvSeq
no-lock no-error.

cfc = "kline". RUN ufcolor.

ASSIGN
kloa    = string(TranCDR.TimeStart, "hh:mm:ss")
klop    = string(TranCDR.TimeEnd, "hh:mm:ss")
ckesto  = string(TranCDR.Duration,"hh:mm:ss")
punetto = GrossPrice - DiscValue.

/* soittajan asno */
FIND Customer where Customer.CustNum = TranCDR.CustNum no-lock no-error.
if avail Customer then asnimi-s = CustName. else asnimi-s = "!! BLANK !!".

/* laskutusasiakkaan asno */
FIND Customer where Customer.CustNum = TranCDR.InvCust no-lock no-error.
if avail Customer then asnimi-l = CustName. else asnimi-l = "!! BLANK !!".

/* raportin saajan asno */
FIND Customer where Customer.CustNum = TranCDR.RepCust no-lock no-error.
if avail Customer then asnimi-r = CustName. else asnimi-r = "!! BLANK !!".

/* alennukseen "omistajan" asno */
FIND Customer where Customer.CustNum = TranCDR.RateCust no-lock no-error.
if avail Customer then asnimi-a = CustName. else asnimi-a = "!! BLANK !!".

/* laskutettavan tuotteen nimi */
FIND BillItem where BillItem.BillCode = TranCDR.BillCode no-lock no-error.
if avail BillItem then tunimi = BIName. else tunimi = "!! BLANK !!".

/* Currency from Price list */
FIND PriceList where PriceList.PriceList = Customer.PriceList no-lock no-error.
if avail PriceList then cucode1 = PriceList.Currency. else cucode1 = "??".
cucode2 = cucode1.

/* b-tilaajan luokitelu nimi */
IF BDest NE ? THEN DO:
   FIND FIRST BDest where BDest.BDest = TranCDR.BDest no-lock no-error.
   IF AVAIL BDest THEN btnimi = BDest.BDName.
                  else btnimi = TranCDR.BDest + " ??".
END.
else btnimi = "!! BLANK !!".

/* raportoitavan maan nimi */
FIND CCN where CCN.CCN = BDest.CCN no-lock no-error.
if avail CCN then maanimi = CCNName. else maanimi = "!! BLANK !!".
ASSIGN
putrunkin = OperIn  + "/" + TranCDR.TrunkIn
putrunk   = OperOut + "/" + TranCDR.TrunkOut.

/* TITLE FOR FRAME */
xtitle = " CALL'S DETAILS, CDR FROM ExCode '" + ExCode + "' ".

if CSub = "FWD" then xtitle = xtitle + "(FWD) ".

IF Date >= 10/20/1998 THEN
   xtitle = xtitle + string(TranCDR.Connection,"Indirect/Direct") + " ".

IF Date >= 01/01/1999 THEN
   xtitle = xtitle + " (B-type " + string(TranCDR.BSubType) + ") ".

/* check IF MINIMUM seconds limitation is used */
l = 0.
DO i = 1 TO 6:
   l = l + TranCDR.TBDuration[i].
END.
IF StartFee = 0 THEN
   unit = unit-sec + string(TranCDR.Duration,"zzzzzzzz9").
ELSE
   unit = unit-cur + string(TranCDR.StartFee,"zz,zz9.99").

/* We combine B- AND C-sub nos. into a single FIELD */
b-c-sub = BSub.
if CSub ne "" AND index(TranCDR.BDest,TranCDR.CSub) = 0 THEN
   b-c-sub = b-c-sub + " > " + CSub.

DISPLAY
   Date  entry(weekday(TranCDR.Date),vpv) @ vp  kloa
   Duration klop ckesto
   TranCDR.CLI
   b-c-sub btnimi
   TranCDR.CCN  substr(maanimi,1,16) + " (" + BDest.BDest + ")" @ maanimi
   TranCDR.SpecialPrice[1] when TariffType = FALSE
   GrossPrice    when TariffType @ TranCDR.SpecialPrice[1]
   TranCDR.CustNum asnimi-s
   TranCDR.InvCust asnimi-l
   TranCDR.RepCust asnimi-r putrunk putrunkin
   TranCDR.RateCust asnimi-a
   /* pu-npi pu-ton */
   /* pu-tpi */ Prefix when substr(TranCDR.Prefix,1,1) <= "9"
   TranCDR.BillCode tunimi /* StartFee */ unit
   TranCDR.TBPrice
   TranCDR.TBDuration
   GrossPrice punetto TranCDR.Priced
   Disc% DiscValue cucode1 cucode2
   entry(TranCDR.DiscType + 1,"-,Fast % / land (1),Fast % / BillCode (2)," +
   "Vol.rab / BillCode. (3)") @ atyyp
   TariffType InvSeq.Billed
WITH FRAME puh.

IF InvSeq.InvNum NE 0 THEN PUT SCREEN ROW 23 col 2 
   "Billed with invoice number: " + string(InvSeq.InvNum).

action:
repeat WITH FRAME puh:
   ASSIGN ufk = 0 ufk[1] = 796 ufk[2] = 623 ufk[5] = 785  ufk[8] = 8 ehto = 0.
   RUN ufkey.
   IF toimi = 1 THEN DO:
      DO i = 1 TO 6:
         DISP
         decimal(TranCDR.TBPrice[i] * 0.6) format "z9.99999" 
         @ TranCDR.TBPrice[i].
      END.
      COLOR DISPLAY messages TranCDR.TBPrice[1 FOR 6].
      message "Press ENTER !".
      PAUSE no-message.
      DISP TranCDR.TBPrice[1 FOR 6].
      COLOR DISP normal TranCDR.TBPrice[1 FOR 6].
   END.

   IF toimi = 2 THEN RUN nnhitt(TranCDR.TariffId).

   /*if toimi = 5 THEN RUN nnatco.p.*/
   IF toimi = 5 THEN RUN nncopc(recid(TranCDR)).
   IF toimi = 8 THEN LEAVE Action.
END.

HIDE FRAME puh no-pause.

if InvSeq.InvNum NE 0 then put screen row 23 col 2 fill(" ",78).


