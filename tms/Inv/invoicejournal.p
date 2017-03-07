
/* ----------------------------------------------------------------------
  MODULE .......: invoicejournal.p
  TASK .........: Print invoice journal 
                  refactored logic part from nnlalu.p (laskuluettelo)
  APPLICATION ..: TMS
  AUTHOR .......: mvi 
  CREATED ......: 08.03.07
  CHANGED ......: see also change history in nnlalu.p for older modifications
                  
  Version ......: xfera
  TODO..........: process logging
----------------------------------------------------------------------- */



{Syst/commali.i}

{Syst/utumaa.i "new"}
{Func/tmsparam2.i}
{Func/fcurrency.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'invoice'}
{Func/logger.i}

/* INPUT PARAMETERS */
DEF INPUT PARAM  icInvGroup   LIKE InvGroup.InvGroup NO-UNDO.
DEF INPUT PARAM  ilExtCustGrp AS LOG NO-UNDO INIT FALSE FORMAT "Y/N".
DEF INPUT PARAM  iiCustNum    AS INT NO-UNDO EXTENT 2.
DEF INPUT PARAM  idaPvm1      AS DATE FORMAT "99-99-99" NO-UNDO.
DEF INPUT PARAM  idaPvm2      AS DATE FORMAT "99-99-99" NO-UNDO.
DEF INPUT PARAM  icExtInvID1  LIKE Invoice.ExtInvID NO-UNDO.
DEF INPUT PARAM  icExtInvID2  LIKE Invoice.ExtInvID NO-UNDO.
DEF INPUT PARAM  iiInvType1   AS INT NO-UNDO FORMAT ">9".
DEF INPUT PARAM  iiInvType2   LIKE iiInvType1.
DEF INPUT PARAM  iiPaymState1 AS INT NO-UNDO FORMAT "9".
DEF INPUT PARAM  iiPaymState2 LIKE iiPaymState1. 
DEF INPUT PARAM  ilConnType   LIKE Customer.ConnType NO-UNDO.
DEF INPUT PARAM  ilRem        AS LOG NO-UNDO FORMAT "All/Denied" INIT TRUE.
DEF INPUT PARAM  ilDeny       AS LOG NO-UNDO FORMAT "Denied/All".
DEF INPUT PARAM  ilUnpaid     AS LOG FORMAT "Unpaid/All" NO-UNDO.
DEF INPUT PARAM  ilVATAmt     AS LOG NO-UNDO FORMAT "All/No VAT" INIT TRUE.
DEF INPUT PARAM  ilLatil      AS LOG FORMAT "Yes/No" NO-UNDO.
DEF INPUT PARAM  ilTikoo      AS LOG FORMAT "Yes/No" NO-UNDO.
DEF INPUT PARAM  ilXOnlySum   AS LOG FORMAT "Yes/No"   NO-UNDO. 



/* INPUT PARAMETER VALIDATION */
FIND FIRST InvGroup NO-LOCK WHERE
           InvGroup.Brand    = gcBrand AND 
           InvGroup.InvGroup = icInvGroup
           NO-ERROR.
IF NOT AVAIL InvGroup THEN 
   gcProcessParamError = "Invalid invoice group". 

IF iiCustNum[2] < iiCustNum[1] THEN 
   gcProcessParamError = "Invalid customer range".

IF idaPvm2 < idaPvm1 THEN
   gcProcessParamError = "Invalid date range".

IF iiInvType2 < iiInvType1 THEN 
   gcProcessParamError = "Invalid invoice type range".

IF iiPaymState2 < iiPaymState2 THEN 
   gcProcessParamError = "Invalid payment state range".

/* TODO: process logging  */
IF gcProcessParamError NE "" THEN DO:
   RETURN.
END.

/* TEMP TABLES */
def temp-table latili NO-UNDO
   field latno as i format "zzzzzzz9"
   field latmk  as de format "->,>>>,>>9.99"
   INDEX latno latno.

def temp-table yhtili NO-UNDO
   field yhtno as i format "zzzzz9"
   field yhtmk  as de format "->,>>>,>>9.99"
   INDEX yhtno yhtno.

DEF TEMP-TABLE ttTotal NO-UNDO
   FIELD Currency AS CHAR
   FIELD InvAmt   AS DEC
   FIELD HomeAmt  AS DEC
   INDEX Currency Currency.

DEF TEMP-TABLE TCustGroup
   FIELD CustGroup LIKE CustGroup.CustGroup
   INDEX CustGroup CustGroup.

DEF TEMP-TABLE TCGMember
   FIELD custno LIKE customer.custnum
   INDEX custno custno.


/* LOCAL VARIABLES */
def var viiva1     as char format "x(114)" NO-UNDO.
def var viiva2     like viiva1.
def var viiva3     like viiva1.
def var jar        as char format "x(30)" NO-UNDO. /* not used */
def var order      as int NO-UNDO.
def var sl         as int NO-UNDO.
def var rl         as int NO-UNDO.
def var rlx        as int NO-UNDO.
def var lev        as int init 114 NO-UNDO.
def var ke         as log format "Yes/No" init false NO-UNDO. /* not used */
def var AccNum     as int extent 15 NO-UNDO.                        
def var Posting    as de  extent 15 NO-UNDO.
def var ysaldo     as dec format "->>>>>>>9.99" init 0 NO-UNDO. /* not used */
def var IGName    like InvGroup.IGName no-undo.
def var unpaid     as log format "Unpaid/All" NO-UNDO.
def var i          as int no-undo.
def var laskyht    as dec format "->>>>>>>9.99" NO-UNDO.
def var debyht     as dec format "->>>>>>>9.99" NO-UNDO.
def var kreyht     as dec format "->>>>>>>9.99" NO-UNDO.
def var edInvNum    like Invoice.ExtInvID NO-UNDO.
def var clapvm     as c no-undo.
def var claepvm    as c no-undo.
def var VATAmt     as lo no-undo format "All/No VAT" init true.
def var overpayacc as i  no-undo.
def var xVatAmt      as dec   no-undo.
def var xVatTot      as dec   no-undo.
def var lcTypeDenied as char  no-undo.
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR extname      AS CHAR  NO-UNDO.
DEF VAR dExtCustGrp  AS CHAR  NO-UNDO FORMAT "x(25)".
DEF VAR kpl          AS INT   NO-UNDO.
DEF VAR CgCustno1    AS INT   no-undo.
DEF VAR cgCustno2    AS INT   no-undo.
DEF VAR ldInvTot     AS DEC   NO-UNDO.
DEF VAR liQty        AS INT   NO-UNDO.
DEF VAR liMax        AS INT   NO-UNDO.
DEF VAR liChkInv1    AS INT   NO-UNDO.
DEF VAR liChkInv2    AS INT   NO-UNDO.
DEF VAR liInvNum     AS INT   NO-UNDO.
DEF VAR lcCustName   AS CHAR  NO-UNDO.




assign OverPayAcc   = fCParamI("OverPayAcc")
       lcTypeDenied = fCParamC("InvTypeDenied").
assign
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev).

/* FRAME SIVUOTS */
form header
   viiva1 at 2 skip
   ynimi at 2 "INVOICE JOURNAL FROM PERIOD" at 47
     "Page" at 106 sl format "ZZZZ9" skip
   "Inv.group" at 2 icInvGroup IGName format "x(14)" "ConnType:"
   ilConnType format "Dir/Indir"
     idaPvm1 at 47 format "99-99-9999" "-" idaPvm2 format "99-99-9999"
   ilDeny format "DENIED/" at 80  unpaid format "UNPAID/" at 90
   string(pvm,"99-99-99") at 108 skip
   viiva2 at 2 skip(1)

   "Invoice"     at 2
   "Customer"    to 22
   "Customer's"  at 24
   "Date Of"     at 44
   "Dueday"      at 53
   "Total Ex"    TO 74
   "VAT"         TO 88
   "Rounding"    TO 97
   "To Be Paid"  TO 111

   "Number"      at 2
   "Number"      to 22
   "Name"        at 24
   "Invoice"     at 44
   "VAT"         TO 74
   "Incl VAT"    TO 111
 
   viiva3 at 2 skip(1)
with
   width 116 no-label no-box frame sivuots.


/* FRAME TILYHT */
form header
   viiva1 at 2 skip
   ynimi at 2 "ACCOUNT SUMMARY / VOUCHER FOR GENERAL LEDGER" at 37
     "Page" at 106 sl format "ZZZZ9" skip
   "Inv.group" at 2 icInvGroup IGName format "x(20)"
     idaPvm1 at 43 format "99-99-9999" "-" idaPvm2 format "99-99-9999"
     string(pvm,"99-99-99") at 108 skip
   viiva2 at 2 skip(1)
   "Acct" at 2 "Amt Debit" at 22 "Amt Credit" at 38 skip
   viiva3 at 2 skip
with
   width 116 no-label no-box frame tilyht.


/* FUNCTIONS */
FUNCTION fChgPage RETURNS LOGICAL
   (iAddLine AS INT).

   if rl + iAddLine >= skayt1 then do:
      if sl > 0 then do:
         {Syst/uprfeed.i rl}
      end.
      assign rlx = 0 sl = sl + 1 rl = 9.
      view stream tul frame sivuots.
   end.

END FUNCTION.


FUNCTION fCollectAcc RETURNS LOGICAL
   (iAccNum AS INT,
    iAmount AS DEC). 

   /* totals (in home currency) */
   find first yhtili where yhtili.yhtno = iAccNum no-error.
   if not available yhtili then do:
      create yhtili.
      assign yhtili.yhtno = iAccNum.
   end.
   assign yhtili.yhtmk = yhtili.yhtmk + 
                         fToHomeCurr(iAmount,Invoice.ExchRate). 

   /* invoice Level (in original currency) */
   find first latili where latili.latno = iAccNum no-error.
   if not available latili then do:
      create latili.
      assign latili.latno = iAccNum.
   end.
   assign latili.latmk = latili.latmk + iAmount.

   ldInvTot = ldInvTot + iAmount.
   
   RETURN TRUE. 

END FUNCTION.

FUNCTION fStripInvID RETURNS INTEGER
   (icInvID AS CHAR):
   
   DEF VAR liPos   AS INT NO-UNDO.
   DEF VAR liStrip AS INT NO-UNDO.
   
   liStrip = 0.
   
   /* separate prefix */
   DO liPos = 1 TO LENGTH(icInvID):
      IF INDEX("0123456789",SUBSTRING(icInvID,liPos,1)) > 0 THEN DO:
         liStrip = INTEGER(SUBSTRING(icInvID,liPos)) NO-ERROR.
         LEAVE.
      END.
   END.
   
   RETURN liStrip.

END FUNCTION.




/* PRINT INVOICE JOURNAL LOGIC */
assign tila = true.
{Syst/utuloste.i "return"}

assign sl = 0 rl = skayt1.

EMPTY TEMP-TABLE ttTotal.

FOR EACH TCustGroup.
   FOR EACH cgmember WHERE
            cgMember.Brand     = gcBrand AND
            cgmember.custgroup = Tcustgroup.custgroup
   NO-lock.
      FIND FIRST tcgmember WHERE
                 Tcgmember.custno = cgmember.custnum
      NO-LOCK NO-ERROR.
      IF NOT AVAIL tcgmember THEN DO:
         CREATE Tcgmember.
         ASSIGN
            Tcgmember.custno = cgmember.custnum
            kpl = kpl  + 1
            cgcustno1 = min(cgcustno1,cgmember.custnum)
            cgcustno2 = max(cgcustno2,cgmember.custnum).
      END.
   END.
END. 

if ilExtcustgrp = FALSE THEN DO:
   ASSIGN
   cgcustno1 = 1
   cgcustno2 = 999999999.
END.

/* narrow down the selection */
IF iiCustNum[1] NE iiCustNum[2] THEN 
ASSIGN iiCustnum[1] = MAX(iiCustnum[1],cgCustno1)
       iiCustnum[2] = MIN(iiCustnum[2],cgCustno2).

runko:
for each Invoice no-lock USE-INDEX InvDate where             
      Invoice.InvDate   >= idaPvm1         and 
      Invoice.InvDate   <= idaPvm2         and 
      Invoice.Brand      = gcBrand      AND 
      Invoice.ExtInvID  >= icExtInvID1  AND
      Invoice.ExtInvID  <= icExtInvID2  AND
      Invoice.CustNum   >= iiCustnum[1] AND
      Invoice.CustNum   <= iiCustnum[2] AND
      Invoice.InvType   >= iiInvType1   AND
      Invoice.InvType   <= iiInvType2   AND
      Invoice.PaymState >= iiPaymState1 AND
      Invoice.PaymState <= iiPaymState2 AND 
      (if ilDeny        then Invoice.InvCfg[1] = true else true) and 
      (if VATAmt = FALSE then Invoice.VATAmt = 0 else true)         and
      (if ilRem = FALSE then NOT Invoice.ClaimPerm else true)         and
      /* printing not denied (not noted if only one type chosen) */
      (IF iiInvType1 NE iiInvType2
       THEN lookup(string(Invoice.InvType),lcTypeDenied) = 0
       ELSE TRUE),

first Customer no-lock where
      Customer.CustNum = Invoice.CustNum                                and
     (if InvGroup ne "" then Customer.InvGroup = InvGroup else true)    and
     (if ConnType    ne ?  then Customer.ConnType = ConnType else true) AND
     (If NOT ilExtCustGrp THEN TRUE
      ELSE CAN-FIND(FIRST tcgmember  where
                          tcgmember.custno = customer.custnum))
by Invoice.ExtInvID:

       /* if ONLY UNPAID ? */
       if unpaid and Invoice.PaidAmt = Invoice.InvAmt THEN NEXT.

       IF NOT ilXOnlySum THEN DO:
          fChgPage(5).
       END.

       /* YhteensA-summat */

       assign laskyht = laskyht + Invoice.InvAmt
              liQty   = liQty + 1.
              
       IF liQty MOD 1000 = 0 THEN DO:
          PAUSE 0.
          DISP liQty LABEL "Invoices" 
          WITH OVERLAY ROW 12 COL 50 SIDE-LABELS TITLE " COLLECT " FRAME fQty.
       END.

       /* EtsitAAn puuttuvat tositteet */
       if edInvNum > "" and ConnType = ? AND NOT ilDeny AND NOT ilXOnlySum
       then do:
          ASSIGN liChkInv1 = fStripInvID(edInvNum)
                 liChkInv2 = fStripInvID(Invoice.ExtInvID).
          if liChkInv1 + 1 NE liChkInv2 AND  
             Invoice.ExtInvID BEGINS SUBSTRING(edInvNum,1,4)
          then do:
             put stream tul "* * * INVOICE NUMBER(S) MISSING * * *" at 2 skip.
             assign rl = rl + 1.
          end.
       end.
       assign
          edInvNum   = Invoice.ExtInvID
          lcCustName = Invoice.CustName.
       
       /* temporary customer used */       
       IF Customer.CustNum < 1000 THEN DO:
      
          /* credit invoice */
          IF Invoice.InvType = 8 OR Invoice.InvType = 9 THEN
             liInvNum = Invoice.CrInvNum.
          ELSE liInvNum = Invoice.InvNum.
      
          FOR FIRST Order NO-LOCK WHERE
                    Order.InvNum = liInvNum,
              FIRST OrderCustomer OF Order NO-LOCK WHERE
                    OrderCustomer.RowType = Order.InvCustRole:
                
             lcCustName = DYNAMIC-FUNCTION("fPrintOrderName" IN ghFunc1,
                                           BUFFER OrderCustomer).
          END.                                     

       END.        
 
       IF NOT ilXOnlySum THEN 
       put stream tul
         Invoice.ExtInvID   at 2   format "x(12)"
         Invoice.CustNum    at 15  format "zzzzzzz9"
         lcCustName         at 24  format "x(19)"
         Invoice.InvDate    at 44  format "99.99.99"
         Invoice.DueDate    at 53  format "99.99.99"
         Invoice.AmtExclVAT to 74  format "->,>>>,>>9.99"
         Invoice.VATAmt     to 88  format "->,>>>,>>9.99"
         Invoice.Rounding   to 97  format "->>>9.99"
         Invoice.InvAmt     to 111 format "->,>>>,>>9.99"
         Invoice.Currency   at 113 format "X(3)" 
         skip.

       assign rl       = rl + 1
              AccNum   = 0
              Posting  = 0
              ldInvTot = 0.

       EMPTY TEMP-TABLE latili.
       
       /* TiliOinnin print-lineluuppi */
       if ilLatil or ilTikoo then do:
          /* Tulostellaan tiliOintitiedot */

          /* viedAAn laskun kiinteAt tiliOinnit paikalliseen tauluun */
          assign
          AccNum[1] = Invoice.ARAccNum    Posting[1] = Invoice.InvAmt
          AccNum[2] = Invoice.RoundAccNum Posting[2] = 0 - Invoice.Rounding
          AccNum[3] = Invoice.IntAccNum   Posting[3] = 0 - Invoice.InterestAmt
          AccNum[4] = Invoice.APAccNum    Posting[4] = 0 - Invoice.AdvPaym
          AccNum[5] = Invoice.OPAccNum    Posting[5] = 0 - Invoice.OverPaym.
          
          /* VAT can be divided into several accounts  */
          xVatTot = 0.
                 
          do i = 1 to 10:
             assign AccNum[i + 5]  = Invoice.VATAccount[i] 
                    Posting[i + 5] = -1 * Invoice.VATAmount[i]
                    xVatTot        = xVatTot + Invoice.VatAmount[i].
          end.

          /* kootaan tyOtiedostoihin */
          do i = 1 to 15:
             if Posting[i] = 0 then next.

             fCollectAcc(AccNum[i],
                         Posting[i]).
          end.

          /* invoice lines are posted without VAT */
          for each InvRow of Invoice no-lock 
          break by InvRow.VatPerc
                by InvRow.Amt:

             /* if VAT is included then remove it */
             assign xVatAmt = 0.

             if InvRow.VatPerc > 0 AND Invoice.VatIncl then do:

                if last(InvRow.Amt) then 
                    assign xVatAmt = xVatTot
                           xVatTot = 0.
                else assign xVatAmt = round(InvRow.Amt * InvRow.VATPerc
                                            / (100 + InvRow.VATPerc),3)
                            xVatTot = xVatTot - xVatAmt.
             end.

             /* update temp-tables */
             fCollectAcc(InvRow.SlsAccNum,
                         -1 * (InvRow.Amt - xVatAmt)). 

          end.
              
          /* if there is a difference caused by the use of 3 decimals in
             invoice row amounts then post it to rounding account */
          IF ldInvTot >= -0.005 AND ldInvTot <= 0.005 AND ldInvTot NE 0  
          THEN DO:
             fCollectAcc(Invoice.RoundAccNum,
                         -1 * ldInvTot).
          END.

          /* tulostetaan laskun tilit */
          for each latili by latili.latno.

             IF NOT ilXOnlySum AND ROUND(latili.latmk,2) NE 0 THEN DO:
                fChgPage(0).

                put stream tul
                   "Acct"       at 88
                   latili.latno at  94 format "zzzzzzz9"
                   latili.latmk at 103 format "->,>>>,>>9.99"
                skip.
                assign rl = rl + 1.
             END.
          end.

          IF NOT ilXOnlySum THEN DO:
             put stream tul skip(1).
             assign rl = rl + 1.
          END.

       end. /* if latil */

       else IF NOT ilXOnlySum THEN do:
          rlx = rlx + 1.
          if rlx = 5 then do:
             put stream tul skip(1).
             rl = rl + 1. rlx = 0.
          end.
       end.

       /* totals by currency */
       FIND FIRST ttTotal WHERE ttTotal.Currency = Invoice.Currency NO-ERROR.
       IF NOT AVAILABLE ttTotal THEN DO:
          CREATE ttTotal.
          ASSIGN ttTotal.Currency = Invoice.Currency.
       END.
       ASSIGN ttTotal.InvAmt  = ttTotal.InvAmt  + Invoice.InvAmt
              ttTotal.HomeAmt = ttTotal.HomeAmt + Invoice.CurrAmt.

end. /* print-line */



/* Loppusummien print-line */
IF NOT ilXOnlySum THEN DO:

   fChgPage(2).

   put stream tul
      viiva1 at 2
      skip.
   ASSIGN rl = rl + 1.

   /* totals by currency */
   IF CAN-FIND(FIRST ttTotal WHERE ttTotal.Currency NE lcHomeCurr) THEN DO:
      FOR EACH ttTotal:

         fChgPage(1). 

         IF ttTotal.Currency NE lcHomeCurr THEN 
         PUT STREAM tul
            ttTotal.Currency  FORMAT "X(3)" TO 70
            ttTotal.InvAmt    FORMAT "->>>,>>>,>>9.99" TO 86.

         PUT STREAM tul 
            lcHomeCurr         FORMAT "X(3)" TO 95
            ttTotal.HomeAmt   FORMAT "->>>,>>>,>>9.99" TO 111
            SKIP.

         rl = rl + 1.

         ACCUMULATE ttTotal.HomeAmt (TOTAL).
      END.

      laskyht = (ACCUM TOTAL ttTotal.HomeAmt). 

      fChgPage(2).
      PUT STREAM tul 
         viiva1 AT 2 SKIP.
      rl = rl + 1.
   END.

   PUT STREAM tul UNFORMATTED 
      "**  TOTAL:" at 2 
      lcHomeCurr TO 95  FORMAT "X(3)" 
      laskyht   to 111 format "->>>,>>>,>>9.99"
      "**" SKIP.

   assign rl = rl + 1.

END. 

/* Yhteenvetosivu */
if ilTikoo then do:

   IF NOT ilXOnlySum THEN DO:
      {Syst/uprfeed.i rl}
   END.

   assign sl = sl + 1 rl = 7.
   view stream tul frame tilyht.

   assign debyht = 0.

   for 
   each yhtili where 
        ROUND(yhtili.yhtmk,2) > 0
   by yhtili.yhtno:
         put stream tul 
            yhtno at 2  format ">>>>>>>9"
            yhtmk to 30 format "->>>>>>>>9.99" skip.
         assign
           rl     = rl     + 1
           debyht = debyht + yhtmk.
   end.

   assign kreyht = 0.

   for 
   each yhtili where 
        ROUND(yhtili.yhtmk,2) < 0
   by yhtili.yhtno:     

         put stream tul
            yhtno at 2  format ">>>>>>>9"
            (0 - yhtmk) format "->>>>>>>>9.99" to 47 skip.
         assign

         rl     = rl     + 1
         kreyht = kreyht - yhtmk.

   end.

   put stream tul viiva1 at 2 skip
                  "**  TOTAL " at 2
                  lcHomeCurr FORMAT "X(3)" 
                   debyht to 30 kreyht to 47 skip.
   assign rl = rl + 2.

end. /* if tikoo */


/* vielA viimeinen sivu kohdalleen */
{Syst/uprfeed.i rl}

assign tila = false.
{Syst/utuloste.i}

hide message no-pause.


