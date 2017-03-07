/* -----------------------------------------------
  MODULE .......: NNOMRA.P
  TEHTAVA ......: Operaattori/maa raportin print-linerajojen kysyminen
  APPLICATION ..: NN
  TEKIJA .......: TT
  CREATED ......: 06.09.96
  changePVM ....:
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commpaa.i}

/* Maaritellaan print-linemuuttujat */
{Syst/utumaa.i "new"}

assign tuni1 = "nnomra"
       tuni2 = "".

def new shared var menut as char format "x(16)" EXTENT 8.

DEF VAR ufkey  AS LOG NO-UNDO.
def var oper1  as char format "x(12)" NO-UNDO.
def var oper2  as char format "x(12)" NO-UNDO.
def var btnro1 as char format "x(16)" NO-UNDO.
def var btnro2 as char format "x(16)" NO-UNDO.
def var kausi1 as int format  "9999"  NO-UNDO.
def var kausi2 as int format  "9999"  NO-UNDO.

form
   skip(1)
"   INSTRUKTION: Detta program utskriver pA printern 'OperatOr/land" skip
"                rapport' med valda kriterier. (Ej kriterier = ALLT)."
   skip(14)
   WITH ROW 1 side-labels width 80
   title " " + ynimi + " OPERATOR/LAND RAPPORT " +
   string(pvm,"99-99-99") + " "
   FRAME valinta.

form

   skip(1)
   "OperatOrkoder .....:"
   oper1 TO 37 NO-LABEL
   help "Minsta operatOrkoden"
   " - " oper2 no-label help "StOrsta operatOrkoden" SKIP

   "B-nummer (Land) ...:"
   btnro1 TO 37 NO-LABEL
   help "Minsta landsnumret"
   " - " btnro2 no-label help "StOrsta landnumret" SKIP

   "Period (AAMM) .....:" kausi1 NO-LABEL TO 37
   help "InstDuePeriod (Ar/mAnad) , vilken listas ut" skip(1)

with title " KRITERIER " side-labels ROW 8 centered OVERLAY FRAME rajat.

view FRAME valinta.
PAUSE 0 no-message.

/* Lasketaan oletuskaudet */
kausi1 = integer(string(year(pvm) - 2000,"99") + string(month(pvm),"99")).
kausi2 = kausi1.

DISPLAY kausi1 WITH FRAME rajat.

ufkey = TRUE.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      IF ufkey THEN DO:
    ASSIGN
    ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
    ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
    ehto = 3 ufkey = FALSE.
    RUN Syst/ufkey.p.
      END.

      READKEY.
      nap = keylabel(LASTKEY).

      if lookup(nap,"1,f1") > 0 THEN DO:
    ehto = 9. RUN Syst/ufkey.p.
    UPDATE oper1
      oper2
      validate(input oper2 = "" OR INPUT oper2 >= INPUT oper1,
      "Fel ordning !")

      btnro1
      btnro2
      validate (input btnro2 = "" OR INPUT btnro2 >= INPUT btnro1,
      "Fel ordning !")

      kausi1
    WITH FRAME rajat.
    kausi2 = kausi1.
    ufkey = TRUE.
    NEXT toimi.
      END.
      else if lookup(nap,"5,f5") > 0 THEN DO:
    LEAVE toimi.
      END.
      else if lookup(nap,"8,f8") > 0 THEN DO:
    RETURN.
      END.
   END. /* toimi */

/* Avataan striimi */
ASSIGN tila = TRUE.
{Syst/tmsreport.i "return"}

message "Utskrivning pAgAr, avbryt = ESC".

RUN nnomra1.p(
   INPUT oper1,
   INPUT oper2,
   INPUT btnro1,
   INPUT btnro2,
   INPUT kausi1,
   INPUT kausi2).

/* Suljetaan striimi */
ASSIGN tila = FALSE.
{Syst/tmsreport.i}

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

