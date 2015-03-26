/* ------------------------------------------------------
  MODULE .......: NNHARA1.P
  TEHTAVA ......: HAlytysraportin print-line
  SOVELLUTUS ...: NN
  TEKIJA .......: TT
  CREATED ......: 18.09.96
  changePVM ....: 24.07.02/aam translation to English 
                  27.09.02/aam customer balances in table CustBal
                  07.03.03/aam customer.balance[2] -> CreditLimit
                  15.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{fcustbal.i}

/* print-linemuuttujat */
{utumaa.i}

DEF INPUT PARAMETER kund1   AS INT   NO-UNDO.
DEF INPUT PARAMETER kund2   AS INT   NO-UNDO.

def var kaytalv as dec format "z,zzz,zz9-" NO-UNDO.

def var viiva1 as char format "x(114)".
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
def var jar as char format "x(24)".
DEF VAR order AS INT.
DEF VAR sl AS INT.
DEF VAR rl AS INT.
DEF VAR rlx AS INT.
DEF VAR lev AS INT init 114.
def var ke as log format "Yes/No" init "No".

DEF VAR i AS INT.
DEF VAR eka AS LOG NO-UNDO.
def var erotus  as dec format "z,zzz,zz9-" NO-UNDO.
def var yerotus as dec format "z,zzz,zz9-" NO-UNDO.
def var ylimi1  as dec format "z,zzz,zz9-" NO-UNDO.
def var ylimi2  as dec format "z,zzz,zz9-" NO-UNDO.
def var ylimi3  as dec format "z,zzz,zz9-" NO-UNDO.
def var ylimi4  as dec format "z,zzz,zz9-" NO-UNDO.
def var ylimi5  as dec format "z,zzz,zz9-" NO-UNDO.
def var huutarit as char format "x(8)" NO-UNDO.

DEF VAR ldCustBal AS DEC NO-UNDO.
DEF VAR ldCustInt AS DEC NO-UNDO.
DEF VAR ldCustAP  AS DEC NO-UNDO. 
DEF VAR ldCustOP  AS DEC NO-UNDO. 

ASSIGN
viiva1 = fill("=",lev)
viiva2 = fill("=",lev)
viiva3 = fill("-",lev)
viiva4 = fill("-",lev).

form header
   viiva1 AT 2 SKIP
   ynimi at 2 "ALARMLIST" AT 55
   "page" at 106 sl format "ZZZZ9" SKIP
   string(pvm,"99-99-99") AT 108 SKIP
   viiva2 AT 2 skip(1)
   "Cust nr"    AT 2
   "Cust name"  AT 10
   "Adv.paym"   TO 49
   "Credit l."  TO 60
   "Unb. usage" TO 71
   "Balance"    TO 82
   "Interest"   TO 93
   "Difference" TO 105 SKIP
   viiva3 AT 2 SKIP
WITH width 116 NO-LABEL no-box FRAME sivuots.

form
   Customer.CustNum    AT 2
   Customer.CustName   AT 10
   ldCustAP            TO 50
   Customer.CreditLimit TO 61
   kaytalv             TO 72
   ldCustBal           TO 83 FORMAT "z,zzz,zz9-"
   ldCustInt           TO 94 FORMAT "z,zzz,zz9-"
   erotus              TO 106
   huutarit            TO 116 SKIP
WITH width 116 NO-LABELS no-box FRAME linet.

ASSIGN
rl = 0
sl = 0
eka = TRUE.

print-line:
repeat:
   FOR EACH Customer no-lock where
            Customer.Brand   = gcBrand AND
            Customer.CustNum >= kund1  AND
            (IF kund2 NE 0 THEN Customer.CustNum <= kund2 ELSE TRUE):

         /* onko kjA pyytAnyt keskeytystA ? */
         READKEY PAUSE 0.
         nap = keylabel(LASTKEY).
         if nap = "ESC" THEN DO:
            message "Abort printing (Y/N) ?"
            UPDATE ke.
            IF ke THEN DO:

               /* Tarvitaanko uusi sivu */
               IF rl >= skayt1 - 1 THEN DO:
                  DO i = rl TO spit1:
                     PUT STREAM tul skip(1).
                  END.
                  ASSIGN
                    sl = sl + 1
                    rl = 8
                    rlx = 0.
                  view STREAM tul FRAME sivuots.
               END.

               display stream tul "Printing aborted !"
               WITH NO-LABEL no-box.
               rl = rl + 1.
               LEAVE print-line.
            END.
         END.

         /* Tarvitaanko uusi sivu */
         IF rl >= skayt1 - 3 OR eka THEN DO:
            IF NOT eka THEN DO:
               DO i = rl TO spit1:
                  PUT STREAM tul skip(1).
               END.
            END.
            eka = FALSE.
            ASSIGN
              sl = sl + 1
              rl = 8
              rlx = 0.
            view STREAM tul FRAME sivuots.
         END.

         /* Lasketaan verollinen hinta Customer.Balance[3]:sen arvoille */
         FIND CustCount OF Customer NO-LOCK NO-ERROR.

         IF AVAILABLE CustCount THEN 
         ASSIGN
         kaytalv = CustCount.Unbilled.
         ELSE kaytalv = 0.                                                
         IF kaytalv = ? THEN kaytalv = 0.

         ASSIGN ldCustBal = fGetCustBal(Customer.CustNum,"TOTAL","ARBAL")
                ldCustInt = fGetCustBal(Customer.CustNum,"TOTAL","INT")
                ldCustAP  = fGetCustBal(Customer.CustNum,"TOTAL","AP")
                ldCustOP  = fGetCustBal(Customer.CustNum,"TOTAL","OP"). 

         ASSIGN erotus  = ldCustAP + ldCustOP + Customer.CreditLimit
                        - (kaytalv + ldCustBal + ldCustInt)
                ylimi1  = ylimi1 + ldCustAP + ldCustOP 
                ylimi2  = ylimi2 + Customer.CreditLimit
                ylimi3  = ylimi3 + kaytalv
                ylimi4  = ylimi4 + ldCustBal
                ylimi5  = ylimi5 + ldCustInt

                yerotus = yerotus + erotus.

         huutarit = "".
         IF erotus < 0 then huutarit = "!!!!!!!!".

         /* Tulostetaan line */
         DISPLAY STREAM tul
         Customer.CustNum Customer.CustName
         ldCustAP Customer.CreditLimit kaytalv
         ldCustBal ldCustInt erotus huutarit SKIP
         WITH FRAME linet.

         DOWN STREAM tul WITH FRAME linet.

         /* line- ja katkolaskurit */
         ASSIGN rl = rl + 1.
         rlx = rlx + 1.
         IF rlx = 5 THEN DO:
            rlx = 0.
            PUT STREAM tul skip(1).
            rl = rl + 1.
         END.
   END.

   PUT STREAM tul
      viiva3 AT 2 SKIP
      "** GRAND TOTAL " AT 2 ylimi1 TO 50 ylimi2 TO 61 ylimi3 TO 72
                             ylimi4 TO 83 ylimi5 TO 94 yerotus TO 106 SKIP
      viiva1 AT 2 SKIP.

   rl = rl + 3.

   /* VielA viimeinen sivu kohdalleen */
   DO i = rl TO spit1:
      PUT STREAM tul skip(1).
   END.

   LEAVE print-line.
END. /* print-line */

HIDE MESSAGE no-pause.

