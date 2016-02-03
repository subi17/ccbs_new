/* -----------------------------------------------
  MODULE .......: NNHARA.P
  TEHTAVA ......: HAlytysraportin print-linerajojen kysyminen
  APPLICATION ..: NN
  TEKIJA .......: TT
  CREATED ......: 18.09.96
  changePVM ....:
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

/* Maaritellaan print-linemuuttujat */
{Syst/utumaa.i "new"}

assign tuni1 = "nnhara"
       tuni2 = "".

def new shared var menut as char format "x(16)" EXTENT 8.

DEF VAR ufkey  AS LOG NO-UNDO.
def var cust-nr1  as int format "zzzzzzz9" NO-UNDO.
def var cust-nr2  as int format "zzzzzzz9" NO-UNDO.
def var btnro1 as char format "x(16)" NO-UNDO.
def var btnro2 as char format "x(16)" NO-UNDO.
def var kausi1 as int format  "9999"  NO-UNDO.
def var kausi2 as int format  "9999"  NO-UNDO.

form
   skip(1)
"   INFORMATION: This program will generate an ALARM LIST for"          skip
"                selected customers. The list will contain all "  skip
"                customers who have exceeded their credit."

   skip(13)
   WITH ROW 1 side-labels width 80
   title " " + ynimi + " ALARM LIST " +
   string(pvm,"99-99-99") + " "
   FRAME valinta.

form

   skip(1)
   "Customers .........:"
   cust-nr1 TO 37 NO-LABEL
   help "First customer number"
   " - " cust-nr2 no-label help "Last customer number" skip(1)

with title " CRITERIA " side-labels ROW 8 centered OVERLAY FRAME rajat.

view FRAME valinta.
PAUSE 0 no-message.
view FRAME rajat.

ASSIGN cust-nr1 = 0 cust-nr2 = 9999999 kausi1 = 0000 kausi2 = 9999
       btnro1 = "0" btnro2 = "9999999999999999".
DISPLAY cust-nr1 cust-nr2 WITH FRAME rajat.

ufkey = TRUE.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      IF ufkey THEN DO:
    ASSIGN
    ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
    ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
    ehto = 3 ufkey = FALSE.
    RUN ufkey.p.
      END.

      READKEY.
      nap = keylabel(LASTKEY).

      if lookup(nap,"1,f1") > 0 THEN DO:
    ehto = 9. RUN ufkey.p.
    UPDATE cust-nr1
      cust-nr2
      validate(input cust-nr2 = "" OR INPUT cust-nr2 >= INPUT cust-nr1,
      "Invalid order !")

    WITH FRAME rajat.
    IF cust-nr2 = 0 OR cust-nr2 = ? THEN cust-nr2 = 9999999.
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

message "Writing report, cancel = ESC".

RUN nnhara1(INPUT cust-nr1,input cust-nr2).

/* Suljetaan striimi */
ASSIGN tila = FALSE.
{Syst/tmsreport.i}

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

