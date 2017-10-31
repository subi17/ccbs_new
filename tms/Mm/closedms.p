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
with title " UNREGISTERED PAYMENTS " 
   ROW 1 centered Size 80 by 19 FRAME valinta.

view frame valinta.
pause 0 no-message.

DISPLAY 
   lDate 
   lDef
WITH FRAME valinta. 
ASSIGN ufkey = TRUE
       Syst.CUICommon:nap   = "first". 

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      IF ufkey THEN DO:
         ASSIGN
         Syst.CUICommon:ufk[1]= 132 
         Syst.CUICommon:ufk[2]= 0  Syst.CUICommon:ufk[3]= 0 Syst.CUICommon:ufk[4]= 0 
         Syst.CUICommon:ufk[5]= 63 Syst.CUICommon:ufk[6]= 0 Syst.CUICommon:ufk[7]= 0 Syst.CUICommon:ufk[8]= 8 
         Syst.CUICommon:ufk[9]= 1
         Syst.CUICommon:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

          READKEY.
          ASSIGN
          Syst.CUICommon:nap = keylabel(LASTKEY).
      if lookup(Syst.CUICommon:nap,"1,f1") > 0 THEN DO:
         Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
         repeat WITH FRAME valinta ON ENDKEY UNDO, LEAVE:
            UPDATE 
                lDate
                lDef.
            LEAVE.               
         END.
         ufkey = TRUE.
         NEXT toimi.
      END.

      else if lookup(Syst.CUICommon:nap,"5,f5") > 0 THEN DO:
         LEAVE toimi.
      END.
      else if lookup(Syst.CUICommon:nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
   END. /* Syst.CUICommon:toimi */

ASSIGN tila = TRUE.
{Syst/utuloste.i "return"}

MESSAGE "Printing in progress...".

RUN Ar/unregrep2.p(INPUT lDate, INPUT lDef). 

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

HIDE FRAME valinta.

