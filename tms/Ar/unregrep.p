/*-----------------------------------------------------------------------------
  MODULE .......: UNREGREP.P
  FUNCTION .....: Prints out unregistered payment status for given day
  APPLICATION ..: 
  AUTHOR .......: tk
  CREATED ......: 09.08.02
  MODIFIED .....: 13.11.02.lp definition's view changed
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/utumaa.i "new"}

assign tuni1 = "unregrep"
       tuni2 = "".

DEF VAR ufkey  AS LO NO-UNDO.
DEF VAR lDate  AS DA NO-UNDO init today.
DEF VAR lDef   AS LO NO-UNDO FORMAT "Specification/Total" INIT TRUE.

form
   SKIP(1)
   "      This program will calculate total sum for unregistered " SKIP
   "      payments on a given Date."
   SKIP(2)
   "      Date :" lDate format "99-99-99" no-label SKIP
   "      Type :" lDef NO-LABEL 
   help "Total sum or Specification"
   SKIP
with title " " + ynimi + " UNREGISTERED PAYMENTS " +
           STRING(pvm,"99-99-99") + " "
   ROW 1 centered Size 80 by 19 FRAME valinta.

view frame valinta.
pause 0 no-message.

DISPLAY 
   lDate 
   lDef
WITH FRAME valinta. 
ASSIGN ufkey = TRUE
       nap   = "first". 

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 
         ufk[2]= 0  ufk[3]= 0 ufk[4]= 0 
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.
      END.

          READKEY.
          ASSIGN
          nap = keylabel(LASTKEY).
      if lookup(nap,"1,f1") > 0 THEN DO:
         ehto = 9. RUN ufkey.p.
         repeat WITH FRAME valinta ON ENDKEY UNDO, LEAVE:
            UPDATE 
                lDate
                lDef.
            LEAVE.               
         END.
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

ASSIGN tila = TRUE.
{Syst/utuloste.i "return"}

MESSAGE "Printing in progress...".

RUN unregrep2(INPUT lDate, INPUT lDef). 

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

HIDE FRAME valinta.

