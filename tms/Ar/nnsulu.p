/* ------------------------------------------------------
  MODULE .......: NNSULU.P
  FUNCTION .....: Suoritusluettelon print-line
  SOVELLUTUS ...: NN
  CREATED ......: 13.08.1991
  changePVM ....: 21.01.1994 / lp - huomautus puuttuvista tositteista
                  22.03.94 /tt --> Muutettu teksti 'Laskun kirj.pvm' ->
                                   'Suorituksen kirj.pvm'
                  05.07.94 /tt --> Ulkoisen siirtotiedoston muodostus KP:hen
                  26.07.94 /tt --> Rajausten tarkistukset
                  01.08.94 /pt --> rajausten tarkistusten korjaus
                  16.06.95 /tt --> LisAtty siirrot PROGRESS-Tiliniekkaan
                  24.07.95 /tt --> Mahdollistettu 2 yritystA
                  28.05.96 /tt --> Jos EI kpvientiA, ei myOs tulosteta tul2
                  27.06.97 /pt --> pvm.formaatit 99.99.9999 headerissa
                  06.11.97 /pt --> change pvm-rajoihin FOR eachissa
                  23.11.97 /pt --> laskutusryhmArajaus
                  28.07.98 /pt --> rajaus PaymDate perusteella ei AccDate
                  15.05.99 /pt --> NEW RepType of payment (advP/Deposit)
                  07.09.99 /pt --> show Payment.Memo[1] (P/B) 
                  07.02.02 lp - IF OverPayment THEN Paid(NOT PPd) 
                  29.04.02 aam  use AccDate NOT PaymDate,
                                "of suoritus" was missing from FOR EACH 
                                loop of "asiakas" -> inv.group was NOT 
                                notified,
                                DISPLAY PaymSrc instead of memo
                  13.05.02 aam  invoice nbr range,
                                "if" statements removed from lakpv-selection
                                (FOR EACH -loop of Payment),
                                only summary optionally
                  13.06.02 lp   - added definition PaymSrc
                                - data TO summary page
                  14.06.02 aam  HELP FOR PaymSrc (paymsrc.i),
                                a list of payment sources can be given
                  09.10.02 aam  calculate payment sum using AccType,
                                column "discount" removed
                  14.10.02 aam  currency handling,
                                fChgPage(), uprfeed
                  01.11.02 aam  PaymType as criteria 
                  05.03.03 tk   tokens
                  15.09.03 aam  brand
                  01.04.04 aam  PaymFile as criteria ("batch")
                  04.05.04 aam  account with 6 digits
                  21.12.05 aam  check payment source with fTMSCodeChk 
                  05.01.06 aam  longer format for amounts
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{utumaa.i "new"}
{fcurrency.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'payment'}

assign tuni1 = "nnsulu"
       tuni2 = "".

DEF STREAM tul2.

DEF TEMP-TABLE wtili NO-UNDO 
   field ttilino as int format "zzzzz9"
   field ttilimk as dec format "zzzzzz9.99-"
   INDEX ttilino is unique ttilino. 

DEF TEMP-TABLE ttTotal NO-UNDO
   FIELD Currency AS CHAR
   FIELD InvAmt   AS DEC
   FIELD HomeAmt  AS DEC
   INDEX Currency Currency.

DEF BUFFER PrevPayment FOR Payment.

DEF VAR toslaji       AS INT NO-UNDO init 2.

def var viiva1        as char format "x(112)" NO-UNDO.
DEF VAR viiva2        LIKE viiva1.
DEF VAR viiva3        LIKE viiva1.
def var jar           as char format "x(30)" NO-UNDO.
DEF VAR order         AS INT NO-UNDO.
DEF VAR sl            AS INT NO-UNDO.
DEF VAR rl            AS INT NO-UNDO.
DEF VAR rlx           AS INT NO-UNDO.
DEF VAR lev           AS INT init 112 NO-UNDO.
def var ke            as log format "Yes/No" init FALSE NO-UNDO.

def var era           as dec  NO-UNDO format "ZZZZZZ9.99-" init 0.
def var ysaldo        as dec  NO-UNDO format "ZZZZZZZ9.99-" init 0.
def var yera          as dec  NO-UNDO format "ZZZZZZZ9.99-" init 0.
def var muist         as char NO-UNDO format "x(12)".
def var myoh          as int  NO-UNDO format "ZZZ9".
def var vapv          as int  NO-UNDO format "ZZ9".
def var kake          as int  NO-UNDO format "Z9".
def var ermk          as dec  NO-UNDO format "ZZZZZZ9.99-".

def var pvm1          as Date NO-UNDO format "99-99-99".
def var pvm2          as Date NO-UNDO format "99-99-99".
DEF VAR InvGroup      LIKE InvGroup.InvGroup NO-UNDO.
DEF VAR IGName        LIKE InvGroup.IGName NO-UNDO.
def var sutil         as log format "Yes/No" NO-UNDO.
def var sukoo         as log format "Yes/No" NO-UNDO.
DEF VAR i             AS INT NO-UNDO.
def var kust          as char format "x(16)" EXTENT 9 NO-UNDO.
DEF VAR selite        AS CHAR EXTENT 10 NO-UNDO.
def var tlaji         as int format "zzz9" EXTENT 10 NO-UNDO.

def var TotMoney      as dec NO-UNDO format "ZZZZZZZ9.99-".
def var TotDiscnt     as dec NO-UNDO format "ZZZZ9.99-".

DEF VAR InvBal        LIKE Payment.InvAmt NO-UNDO.
def var istat         as char NO-UNDO format "x(6)".
def var debyht        as dec  NO-UNDO format "ZZZZZZZ9.99-".
def var kreyht        as dec  NO-UNDO format "ZZZZZZZ9.99-".
def var tosi1         like Payment.Voucher format ">>>>>>>9" NO-UNDO.
def var tosi2         like Payment.Voucher format ">>>>>>>9" NO-UNDO.
DEF VAR PrVou         LIKE Payment.Voucher NO-UNDO.

def var valikko       as char NO-UNDO format "x(30)" EXTENT 2 init
["BY DATE","BY VOUCHER NO."].
def var valik         as char NO-UNDO format "x(30)".
def var tiedosto      as char NO-UNDO format "x(40)". 
def var InvNum1       as int  NO-UNDO format ">>>>>>>9".
def var InvNum2       as int  NO-UNDO format ">>>>>>>9".
def var xOnlySum      as log  NO-UNDO format "Yes/No". 

DEF VAR PaymSrc       LIKE Payment.PaymSrc NO-UNDO INIT "ALL".
DEF VAR ldPaidAmt     LIKE Payment.PaymAmt NO-UNDO. 
DEF VAR lcCurrency    AS   CHAR            NO-UNDO. 
DEF VAR ldRate        AS   DEC             NO-UNDO. 

DEF VAR liPaymType1  AS INT   NO-UNDO FORMAT "9".
DEF VAR liPaymType2  LIKE liPaymType1. 
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR lcFrameField AS CHAR  NO-UNDO. 
DEF VAR liBatch      AS INT   NO-UNDO EXTENT 2.

form
   valik NO-LABEL
   with overlay 2 down title color value(ctc) " CHOOSE SORT ORDER "
   COLOR value(cfc) ROW 6 centered FRAME rival.

ASSIGN
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev)

pvm2        = date(month(pvm),1,year(pvm)) - 1
pvm1        = date(month(pvm2),1,year(pvm2))
InvNum2     = 99999999
tosi2       = 99999999
liPaymType2 = 9.

form
   skip(1)
"  Note: This program prints out a journal of payments (payments for"     skip
"        invoices as well as deposits and advance payments) made into A/R."
         skip
"        Optionally also accounting data is being printed."      skip
skip(13)
   WITH ROW 1 side-labels width 80
        title color value(ctc) " " + ynimi + " PAYMENT JOURNAL " +
        string(pvm,"99-99-99") + " " COLOR value(cfc)
        FRAME MAIN.

form header
   viiva1 AT 1 SKIP
   ynimi at 1 "PAYMENT JOURNAL; PAYMENTS DURING" AT 42 pvm1
     format "99.99.9999" "-" pvm2 format "99.99.9999" "Page" AT 103
     sl format "ZZZZ9" SKIP
   "InvGroup" at 1 InvGroup IGName format "x(20)"
     Jar at 42 format "x(40)" string(pvm,"99-99-99") AT 105 SKIP
   viiva2 AT 1 skip(1)

   "Invoice"     TO  8
   "Cust."       TO 17
   "Customer's"  AT 19
   "Invoice"     AT 35
   "Invoice"     AT 44
   "Date of"     AT 53
   "Invoice"     TO 74
   "Paid"        TO 87
   "Balance"     TO 100
   "Cur"         AT 102
   "PS"          AT 106 
   "Inv."        AT 109 SKIP

   "Number"      TO  8
   "Number"      TO 17
   "Name"        AT 19
   "Date"        AT 35
   "DueDate"     AT 44
   "Payment"     AT 53
   "Amount"      TO 74
   "Now"         TO 100
   "Stat"        AT 109 SKIP

   viiva3 AT 1 skip(1)
   WITH width 116 NO-LABEL no-box FRAME sivuots.

form header
   viiva1 AT 1 SKIP
   ynimi at 1 "VOUCHER FOR BOOKKEEPING " AT 37 pvm1
   format "99.99.9999" "-" pvm2 format "99.99.9999"
   "Page" at 103 sl format "ZZZZ9" SKIP
   "InvGroup" at 1 InvGroup IGName format "x(24)"  AT 37
   string(pvm,"99-99-99") AT 105 SKIP
   viiva2 AT 1 skip(1)
   "Account" at 1 
   "Debit"   TO 32 
   "Credit"  TO 50 SKIP
   viiva3 AT 1 SKIP
   WITH width 116 NO-LABEL no-box FRAME tilyht.

form
   Payment.InvNum   AT 1  format ">>>>>>>9" 
   Payment.CustNum  AT 10 format ">>>>>>>9"
   Payment.CustName at 19 format "x(14)"
   Payment.InvDate  AT 35
   Payment.DueDate  AT 44
   Payment.AccDate  AT 53
   Payment.InvAmt   TO 74  FORMAT "->>>>>>>9.99"
   ldPaidAmt        TO 87  FORMAT "->>>>>>>9.99"
   InvBal           TO 100 FORMAT "->>>>>>>9.99"
   lcCurrency       AT 102 FORMAT "X(3)"
   Payment.PaymSrc  at 106 format "xx"
   istat            AT 109 FORMAT "X(4)" SKIP
WITH width 120 NO-LABELS no-box FRAME avline.

form
   InvGroup at 2 label "Customers in invoicing Group "
   IGName format "x(24)" NO-LABEL SKIP
   pvm1    at 2 label "Payments registered during  ." "-"  pvm2  NO-LABEL   SKIP
   tosi1   at 2 label "Voucher number Range ........"
   "-"  tosi2  NO-LABEL SKIP
   InvNum1   at 2 label "Invoice number range ........" "-" InvNum2
        NO-LABEL SKIP

   liPaymType1 
           at 2 label "Payment Type ................"
                help "Payment types from"
   "-"
   liPaymType2  no-label 
                  help "Payment types to" skip

   liBatch[1] AT 2 
       LABEL "Payment batch ..............." 
       FORMAT ">>>>>>>9"
       HELP "Payment batch for refund payments (0 = not sent to bank)"
       "-"
       liBatch[2] 
       FORMAT ">>>>>>>9"
       HELP "Payment batch for refund payments (0 = not sent to bank)"
       NO-LABEL
       VALIDATE(INPUT liBatch[2] >= INPUT liBatch[1],
                "Upper limit cannot be less than lower limit")
       SKIP
   PaymSrc AT 2 LABEL "Payments source ............." FORMAT "x(30)" 
       help "Payments source (a comma separated list), ALL = all sources"
       SKIP

   "Printout options:" AT 2  SKIP
   sutil     at 5 label "Acct nos. and amounts of each payment  " SKIP
   sukoo     at 5 label "A separate Acct Summary for bookkeeping" SKIP
   xOnlySum  at 5 label "Print only the Account Summary ........" SKIP
with title color value(ctc) " PRINTOUT PARAMETERS " side-labels
   COLOR value(cfc) ROW 7 centered OVERLAY FRAME options.

FUNCTION fChgPage RETURNS LOGICAL
   (iAddLine AS INT).

   IF rl + iAddLine >= skayt1 THEN DO:
      if sl > 0 then do:
         {uprfeed.i rl}
      end.
      ASSIGN rlx = 0
             sl = sl + 1
             rl = 9.
      view STREAM tul FRAME sivuots.
   END.

END FUNCTION.


cfc = "sel". RUN ufcolor.
view FRAME MAIN.
cfc = "puli". RUN ufcolor.
PAUSE 0 no-message.

ASSIGN vapv = 0 kake = 0 ermk = 0 PrVou = 0 sutil = TRUE sukoo = TRUE
       liBatch[1] = 0
       liBatch[2] = 99999999.

DISPLAY pvm1 pvm2 sutil sukoo xOnlySum
        liPaymType1 liPaymType2 
        "ALL" @ IGName PaymSrc WITH FRAME options.

toimi:
   repeat WITH FRAME MAIN ON ENDKEY UNDO toimi, NEXT toimi:
      ASSIGN ufk = 0 ufk[1] = 132 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN DO:
         ehto = 9. RUN ufkey.
         IF tosi2  = 9999999 THEN tosi2 = 0.
         repeat:
            UPDATE
                InvGroup
                pvm1
                pvm2 validate(input pvm2 >= input pvm1, "Invalid range !")
                tosi1
                tosi2 validate(input tosi2 >= input tosi1, "Invalid range !")
                InvNum1
                InvNum2 validate(input InvNum2 >= input InvNum1, 
                                 "Invalid range")
                liPaymType1
                liPaymType2
                   validate(input liPaymType2 >= input liPaymType1,
                            "Invalid order !")                
                liBatch
                PaymSrc
                sutil
                sukoo
                xOnlySum
            WITH FRAME options EDITING:
               READKEY. nap = keylabel(LASTKEY).

               IF nap = "F9" AND 
                  LOOKUP(FRAME-FIELD,"liPaymType1,liPaymType2") > 0
               THEN DO:

                  lcFrameField = FRAME-FIELD.

                  RUN h-tmscodes(INPUT "Payment",  /* TableName*/
                                       "PaymType", /* FieldName */
                                       "AccRec", /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? 
                  THEN DO WITH FRAME options:
                        IF lcFrameField  = "liPaymType1" 
                        THEN DISPLAY INTEGER(lcCode) ;& liPaymType1.
                        ELSE DISPLAY INTEGER(lcCode) ;& liPaymType2.
                  END.

                  ehto = 9.
                  RUN ufkey.
                  NEXT. 
               END.

               IF lookup(nap,poisnap) > 0 THEN DO:

                  HIDE MESSAGE.
                  if frame-field = "InvGroup" THEN DO:
                     ASSIGN FRAME options InvGroup.
                     if InvGroup = "" THEN DO:
                        disp "ALL" @ IGName WITH FRAME options.
                        IGName = "ALL".
                     END.
                     ELSE DO:
                        FIND InvGroup where 
                             InvGroup.Brand    = gcBrand AND
                             InvGroup.InvGroup = InvGroup
                        no-lock no-error.
                        IF NOT AVAIL InvGroup THEN DO:
                           bell.  message "Unknown Invoicing Group !".
                           NEXT.
                        END.
                        DISP InvGroup.IGName @ IGName WITH FRAME options.
                        IGName = InvGroup.IGName.
                     END.
                  END.

                  ELSE IF frame-field = "PaymSrc" THEN DO:
                     ASSIGN FRAME options PaymSrc.
                     IF PaymSrc NE "ALL" THEN DO:
                        DO i = 1 TO NUM-ENTRIES(PaymSrc):
                           IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                                                   "Payment",
                                                   "PaymSrc",
                                                   ENTRY(i,PaymSrc))
                           THEN DO:
                              MESSAGE "Unknown payment source: "
                                      ENTRY(i,PaymSrc)
                              VIEW-AS ALERT-BOX
                              ERROR.
                              i = -1.
                              LEAVE.
                           END.
                        END.
                        IF i LT 0 THEN DO:
                            NEXT-PROMPT PaymSrc.
                            NEXT.
                        END. 
                     END.   
                  END. 
               END.
               APPLY LASTKEY.
            END.

            LEAVE.
         END.

         IF xOnlySum THEN DO:
            sutil = FALSE.
            DISPLAY sutil WITH FRAME options. 
         END. 

         NEXT toimi.
      END.


      IF toimi = 5 THEN DO:

         assign cfc = "uusi". RUN ufcolor.   ccc = cfc.
         DO i = 1 TO 2 WITH FRAME rival:
            valik = valikko[i].
            DISPLAY valik.
            IF i < 2 THEN DOWN.
            ELSE up 1.
         END.

rival:      repeat ON ENDKEY UNDO rival, LEAVE rival WITH FRAME rival:
               MESSAGE
               "Choose sort order, press ENTER !".
               READKEY PAUSE 0.
               CHOOSE ROW valik ;(uchoose.i;) no-error.
               COLOR DISPLAY value(ccc) valik WITH FRAME rival.
               i = FRAME-LINE.
               HIDE MESSAGE no-pause.
               ASSIGN order = i
                      jar  = valikko[i].
               if lookup(keylabel(lastkey),"enter,return") > 0 THEN LEAVE rival.
            END.  /* rival */
            CLEAR FRAME rival ALL no-pause.
            HIDE FRAME rival no-pause.
            LEAVE toimi.
      END.
      IF toimi = 8 THEN RETURN.
   END. /* toimi */

ASSIGN tila = TRUE.
{utuloste.i "return"}

ASSIGN
sl = 0
rl = skayt1.

EMPTY TEMP-TABLE ttTotal.

message "Printing ...".                             

main : 
repeat:
FOR EACH  Payment no-lock USE-INDEX AccDate where  
          Payment.Brand     = gcBrand     AND
          Payment.AccDate  >= pvm1        AND 
          Payment.AccDate  <= pvm2        AND 
          Payment.InvNum   >= InvNum1     AND 
          Payment.InvNum   <= InvNum2     AND 
          Payment.PaymType >= liPaymType1 AND 
          Payment.PaymType <= liPaymType2 AND 
          Payment.Voucher  >= tosi1       AND 
          Payment.Voucher  <= tosi2       AND
          (IF PaymSrc NE "ALL" 
           THEN LOOKUP(Payment.PaymSrc,PaymSrc) > 0
           ELSE TRUE),
    first Customer of Payment no-lock where 
          (if InvGroup ne "" 
           THEN Customer.InvGroup = InvGroup 
           ELSE TRUE)
    BY (IF order = 1 
        THEN integer(Payment.AccDate)
        ELSE Voucher)
    BY Voucher:

       /* check batch for refund payments */
       IF Payment.PaymType = 6 THEN DO:
          IF INTEGER(Payment.PaymFile) < liBatch[1] OR
             INTEGER(Payment.PaymFile) > liBatch[2]
          THEN NEXT. 
       END.
       
       /* tarvitaanko uusi sivu */
       IF NOT xOnlySum THEN DO:
          fChgPage(5).
       END.

       /* YhteensA-summat */

       ASSIGN
         TotMoney = TotMoney + Payment.PaymAmt
         TotDiscnt = TotDiscnt + Payment.Discount.

       /* Katsotaan, lOytyykO aiempia suorituksia */
       InvBal = Payment.InvAmt.
       
       IF Payment.InvNum > 0 THEN
       FOR EACH PrevPayment NO-LOCK USE-INDEX InvNum where 
                PrevPayment.Brand    = Payment.Brand   AND
                PrevPayment.InvNum   = Payment.InvNum  AND
                PrevPayment.AccDate <= Payment.AccDate AND
                PrevPayment.Voucher NE Payment.Voucher:

         DO i = 1 TO 10:
            IF PrevPayment.AccType[i] = 1 THEN ASSIGN 
               InvBal = InvBal + PrevPayment.Posting[i].
         END.
       END.

       ldPaidAmt = 0.
       DO i = 1 to 10:
          IF Payment.InvNum NE 0 THEN DO:
             IF Payment.AccType[i] = 1 THEN ASSIGN
                InvBal    = InvBal + Payment.Posting[i]
                ldPaidAmt = ldPaidAmt - Payment.Posting[i].
          END.
          ELSE DO:
             /* deposit/overpayment/adv.payment */
             IF Payment.AccType[i] = 6 OR
                Payment.AccType[i] = 7 OR
                Payment.AccType[i] = 19
             THEN ASSIGN ldPaidAmt = ldPaidAmt - Payment.Posting[i].
          END. 
       END.

       IF Payment.InvNum NE 0 THEN DO:
          if InvBal <= 0 then istat = "Paid".
          else if InvBal = Payment.InvAmt then istat = "Unpd".
          else if InvBal > 0 then istat = "PPd".
       END.
       else istat = "----".


       /* EtsitAAn puuttuvat tositteet */
       IF PrVou > 0 AND order = 2 AND NOT xOnlySum THEN DO:
          IF NOT (PrVou + 1) = Payment.Voucher THEN DO:
             put stream tul "* * * VOUCHER MISSING * * *" AT 1 skip(1).
             ASSIGN rl = rl + 2.
          END.
       END.
       ASSIGN PrVou = Payment.Voucher.

       /* currency */
       IF Payment.InvNum > 0 THEN DO:
          FIND Invoice OF Payment NO-LOCK NO-ERROR.
          IF AVAILABLE Invoice THEN lcCurrency = Invoice.Currency.
          ELSE lcCurrency = lcHomeCurr. 
       END.
       ELSE ASSIGN lcCurrency = Customer.Currency.

       IF lcCurrency = "" THEN lcCurrency = lcHomeCurr. 

       /* rate; this is not the best way to do this, but so far the
          only way */
       ldRate = fCurrRate(lcCurrency,Payment.AccDate). 

       /* take rate from invoice if rates are not updated */
       IF ldRate = 0 AND Payment.InvNum > 0 AND AVAILABLE Invoice
       THEN ldRate = Invoice.ExchRate.

       IF NOT xOnlySum THEN DO:
          DISPLAY STREAM tul 
          Payment.InvNum     when Payment.InvNum NE 0
          " DEP/ADV"          when Payment.InvNum =  0 @ Payment.InvNum
          Payment.CustNum 
          Payment.CustName
          Payment.InvDate     when Payment.InvNum NE 0
          Payment.DueDate    when Payment.InvNum NE 0
          Payment.AccDate 
          Payment.InvAmt     when Payment.InvNum NE 0
          ldPaidAmt 
          InvBal             when Payment.InvNum NE 0
          lcCurrency
          Payment.PaymSrc
          istat

          WITH FRAME avline.
          DOWN STREAM tul WITH FRAME avline.

          ASSIGN rl = rl + 1.
          
          IF Payment.PaymType = 6 AND Payment.BankAcc > "" THEN DO:
             PUT STREAM tul UNFORMATTED
                "Bank: " AT 19
                Payment.BankAcc SKIP.
             rl = rl + 1.
          END.
       END. 

       /* TiliOinnin print-lineluuppi */
       IF sutil THEN DO:
          /* Tulostellaan tiliOintitiedot */
          PUT STREAM tul
             "Voucher No." AT 1 string(Payment.Voucher).
       END.


       DO i = 1 TO 10:
          IF Payment.AccNum[i] NE 0 OR 
             Payment.Posting[i] NE 0 THEN DO:

             IF sutil THEN DO:      

                fChgPage(1).

                PUT STREAM tul
                   "Acct" AT 57
                   Payment.AccNum[i]  AT 63 FORMAT ">>>>>9"
                   Payment.Posting[i] TO 82 FORMAT "->>>>>>>9.99" SKIP.
                ASSIGN rl = rl + 1.

             END.


             FIND FIRST wtili where 
                wtili.ttilino = Payment.AccNum[i]
                no-lock no-error.
             /* Pannaan tilinumero ja summa talteen yhteenvetoa varten */
             IF NOT AVAILABLE wtili THEN DO:
                CREATE wtili.
                ASSIGN wtili.ttilino = Payment.AccNum[i].
             END.

             /* summary amounts are in home currency */
             ASSIGN wtili.ttilimk = wtili.ttilimk + 
                                    fToHomeCurr(Payment.Posting[i],ldRate).

          END.

       END. /* 1 TO 10 */

       IF sutil THEN DO:
          PUT STREAM tul skip(1).
          ASSIGN rl = rl + 1.
       END.

       /* totals by currency */
       FIND FIRST ttTotal WHERE ttTotal.Currency = lcCurrency NO-ERROR.
       IF NOT AVAILABLE ttTotal THEN DO:
          CREATE ttTotal.
          ASSIGN ttTotal.Currency = lcCurrency.
       END.
       ASSIGN ttTotal.InvAmt  = ttTotal.InvAmt  + Payment.PaymAmt
              ttTotal.HomeAmt = ttTotal.HomeAmt + 
                                   fToHomeCurr(Payment.PaymAmt,ldRate).

END. /* print-line */

/* Loppusummien print-line */
IF NOT xOnlySum THEN DO:

   fChgPage(2).

   PUT STREAM tul viiva1 AT 1 SKIP.
   rl = rl + 1.

   /* totals by currency */
   IF CAN-FIND(FIRST ttTotal WHERE ttTotal.Currency NE lcHomeCurr) THEN DO:
      FOR EACH ttTotal:

         fChgPage(1). 

         IF ttTotal.Currency NE lcHomeCurr THEN 
         PUT STREAM tul
            ttTotal.Currency  FORMAT "X(3)" TO 46
            ttTotal.InvAmt    FORMAT "->,>>>,>>>,>>9.99" TO 64.

         PUT STREAM tul 
            lcHomeCurr        FORMAT "X(3)" TO 69
            ttTotal.HomeAmt   FORMAT "->,>>>,>>>,>>9.99" TO 87
            SKIP.

         rl = rl + 1.

         ACCUMULATE ttTotal.HomeAmt (TOTAL).
      END.

      TotMoney = (ACCUM TOTAL ttTotal.HomeAmt). 

      fChgPage(2).
      PUT STREAM tul 
         viiva1 AT 1 SKIP.
      rl = rl + 1.
   END.

   fChgPage(0). 
   PUT STREAM tul 
       "**  TOTAL:" AT 1 
       lcHomeCurr TO 69
       TotMoney   TO 87 FORMAT "->,>>>,>>>,>>9.99" SKIP.

   ASSIGN rl = rl + 1.
END.

/* Yhteenvetosivu */

IF sukoo THEN DO:

   IF NOT xOnlySum THEN DO:
       {uprfeed.i rl}
   END.

   ASSIGN
     sl = sl + 1
     rl = 7.
   view STREAM tul FRAME tilyht.

   ASSIGN debyht = 0.
   FOR EACH wtili where wtili.ttilimk >= 0:
      PUT STREAM tul 
         wtili.ttilino AT 1 
         wtili.ttilimk TO 32 FORMAT "->,>>>,>>>,>>9.99" SKIP.
      ASSIGN rl = rl + 1.
      debyht = debyht + ttilimk.
   END.

   ASSIGN kreyht = 0.
   FOR EACH wtili where wtili.ttilimk < 0:
      PUT STREAM tul
         wtili.ttilino AT 1
         (wtili.ttilimk * -1) FORMAT "->,>>>,>>>,>>9.99" TO 50 SKIP.
      ASSIGN rl = rl + 1.
      kreyht = kreyht + (ttilimk * -1).
   END.

   PUT STREAM tul viiva1 AT 1 SKIP
                  "**  TOTAL " AT 1 
                  lcHomeCurr FORMAT "X(3)" 
                  debyht TO 32 FORMAT "->,>>>,>>>,>>9.99"
                  kreyht TO 50 FORMAT "->,>>>,>>>,>>9.99" SKIP.
   ASSIGN rl = rl + 2.
END. /* IF sukoo */

LEAVE main.
END. /* main */

/* Adjust LAST page */
{uprfeed.i rl}
ASSIGN tila = FALSE.
{utuloste.i}

HIDE MESSAGE no-pause.
HIDE FRAME options no-pause.
HIDE FRAME MAIN no-pause.

