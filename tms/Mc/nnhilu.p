/* -----------------------------------------------
  MODULE .......: NNHILU.P
  TEHTAVA ......: HINTALUETTELON KIRJOITUS
  APPLICATION ..: NN
  TEKIJA .......: PT
  CREATED ......: 14.02.97
  changePVM ....: 04.03.97 pt korjattu toimi=8 -ehto
                  10.02.98 kl hi-perus => startFee[i]
                  20.01.99 kl into english, fname VARIABLE
                  15.09.03 aam brand
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{excel.i}
{function.i}

/* Maaritellaan print-linemuuttujat */
{utumaa.i "new"}

assign tuni1 = "nnhilu"
       tuni2 = "".

def new shared var menut as char format "x(16)" EXTENT 8.

DEF VAR ufkey   AS LOG NO-UNDO.
DEF VAR krmin   AS DE  NO-UNDO.
DEF VAR i       AS i   NO-UNDO.
DEF VAR sl      AS INT NO-UNDO.
DEF VAR rl      AS INT NO-UNDO.
DEF VAR lev     AS INT NO-UNDO init 170.
def var paper   as log no-undo format "Printer/File".
def var ok      as lo  no-undo format "Yes/No".
def var stch    as lo  no-undo format "Yes/No".
def var fname   as c   no-undo format "x(30)".

DEF BUFFER xhinta FOR Tariff.

DO FOR TMSUser:
   FIND FIRST TMSUser where
              TMSUser.UserCode = katun
   no-lock no-error.
   fname = fChkPath(TMSUser.RepDir) + "custprices.txt".
END.

form
   skip(3)
"   INSTRUCTION: This program prints out a list of customers prices"   skip
"                due to the criteria given below."                     skip(1)
"                You can print the list to a paper or to a file"       skip(1)

   paper   AT 17
      help "Print to (P)rinter or to a (F)ile ?"
      label "Printer or File "
   stch    AT 17
      help "Print starting fees (Y/N) ?" 
      label "Starting fees .." skip(1)
   fname   AT 17
      help "Name of the printout file"
      label "File Name ......" skip(3)
WITH
   width 80 ROW 1 COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " PRINT CUSTOMER Price LIST " + string(pvm,"99-99-99") + " "
   side-labels FRAME rajat.


form header
   fill ("=",lev) format "x(170)" SKIP
   ynimi "PRICELIST" at 64 "Page" at 161 sl format "ZZZZ9" TO 170
   SKIP
   string(pvm,"99-99-99") TO 170 SKIP
   fill ("=",lev) format "x(170)" skip(1)
   "BSUB"          AT 2
   "Service "      AT 19
   "Pr.code"       AT 40
   "Productname"   AT 47
   "CCN"           AT 68
   "Desc"          AT 72
   "Min.sec"       AT 117
   "--- 5 diff time/price zones --" AT 125
   "WEnd "         AT 155
   "Dsc% Fd Vd"    AT 161
   fill ("-",lev) format "x(170)" SKIP
WITH
   width 170 NO-LABEL no-box FRAME sivuots.


cfc = "sel". RUN ufcolor.
paper = TRUE.
DISPLAY paper stch WITH FRAME rajat.
PAUSE 0 no-message.

ufkey = TRUE.

toimi:
   repeat WITH FRAME rajat ON ENDKEY UNDO toimi, NEXT toimi:
      ASSIGN
      ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
      ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
      ehto = 0. RUN ufkey.p.

      IF toimi = 1 THEN DO:
         ehto = 9. RUN ufkey.p.
         disp "" @ fname WITH FRAME rajat.
         UPDATE 
            paper 
            stch 
         WITH FRAME rajat.
         IF NOT paper THEN UPDATE fname WITH FRAME rajat.
         NEXT toimi.
      END.
      ELSE  IF toimi = 5 THEN DO:
         LEAVE toimi.
      END.
      ELSE IF toimi = 8 THEN DO:
         RETURN.
      END.
   END. /* toimi */

/* Avataan striimi */
IF paper THEN DO:
   ASSIGN tila = TRUE.
   {tmsreport.i "return"}
END.
ELSE OUTPUT STREAM tul TO value(fname).

   message "Printing, break = END".

   /* kaikki b-numerot numerojArjestyksessA */
   IF paper = FALSE THEN DO:
      PUT STREAM tul
      "BSUB"          tab
      "Service"       tab
      "Pr.code"       tab
      "Productname"   tab
      "CCN"           tab
      "Desc"          tab
      "Min.sec"       tab
      "Z1"            tab
      "Z2"            tab
      "Z3"            tab
      "Z4"            tab
      "Z5"            tab
      "WEnd"          tab
      "Disc%"         tab
      "Fd"            tab
      "Vd"            .
      if opsys = "msdos" THEN PUT STREAM tul SKIP.
      ELSE                    PUT STREAM tul my-nl.
   END.

   rl = skayt1 + 1.

   print-line:
   FOR EACH  BDest   no-lock WHERE
             BDest.Brand   = gcBrand,
       EACH  RateCCN no-lock where
             RateCCN.Brand = gcBrand AND
             RateCCN.BDest = BDest.BDest AND
             RateCCN.DestType = BDest.DestType,
       FIRST CCN no-lock where
             CCN.Brand = gcBrand AND
             CCN.CCN   = RateCCN.CCN
   BREAK BY BDest.BDest:

      FOR EACH Tariff no-lock where
               Tariff.Brand   = gcBrand AND
               Tariff.CCN     = CCN.CCN,
      FIRST BillItem NO-LOCK WHERE
            BillItem.Brand    = gcBrand AND
            BillItem.BillCode = Tariff.BillCode:

         /* onko kjA pyytAnyt keskeytystA ? */
         READKEY PAUSE 0.
         nap = keylabel(LASTKEY).
         if nap = "END" THEN DO:
            message "Are You sure You want to cance printing (Y/N) ?"
            UPDATE ok.
            IF ok THEN DO:
               display stream tul "Printing cancelled !" WITH NO-LABEL no-box.
               rl = rl + 1.
               LEAVE print-line.
            END.
         END.


         IF paper AND rl > skayt1 THEN DO:
            IF sl > 0 THEN PUT STREAM tul skip(spit1 - rl).
            ASSIGN sl = sl + 1 rl = 7.
            view STREAM tul FRAME sivuots.
         END.

         IF paper THEN DO:

            PUT STREAM tul
            BDest.BDest     format "x(16)"   AT 2
            BDest.BDName    format "x(20)"   AT 19
            Tariff.BillCode     format "x(6)"    AT 40
            BillItem.BIName     format "x(20)"   AT 47
            CCN.CCN         format "zz9"     AT 68
            CCN.CCNName     format "x(30)"   AT 72
            /* hinnat */
            Tariff.MinSec format "zz9"  AT 118
            space(1).
         END.
         ELSE DO:
            PUT STREAM tul
            BDest.BDest     tab
            BDest.BDName    tab
            Tariff.BillCode     tab
            BillItem.BIName     format "x(24)"   tab
            CCN.CCN     tab
            CCN.CCNName     tab
            Tariff.MinSec tab.
         END.


         DO i = 1 TO 6.
            krmin = round(Tariff.Price[i] * 60 / 100,2).

            PUT STREAM tul
            krmin format "z9.99".
            IF paper THEN PUT STREAM tul space(1).
            ELSE          PUT STREAM tul tab.
         END.

         IF paper AND stch THEN DO:
            PUT STREAM tul SKIP.
            put stream tul "  " AT 120.
            rl = rl + 1.
         END.
         ELSE IF NOT paper AND stch THEN DO:
            DO i = 1 TO 9.
               PUT STREAM tul tab.
            END.
         END.
         /* starting fees */
         IF stch THEN DO i = 1 TO 6.
            put stream tul Tariff.StartCharge[i] format "z9.99".
            IF paper THEN PUT STREAM tul space(1).
            ELSE          PUT STREAM tul tab.
         END.
         if opsys = "msdos" THEN PUT STREAM tul SKIP.
         ELSE                    PUT STREAM tul my-nl.
      END.
   END.

/* vielA viimeinen sivu kohdalleen */
IF paper THEN PUT STREAM tul skip(spit1 - rl).

/* Suljetaan striimi */
ASSIGN tila = FALSE.
{tmsreport.i}

HIDE MESSAGE no-pause.
/* HIDE FRAME rajat no-pause. */
HIDE FRAME rajat no-pause.

