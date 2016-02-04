/* ---------------------------------------------------------------------------
  MODULE .......: arbalrel.p
  FUNCTION .....: ui FOR Balance report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 25.02.02/aam 
  MODIFIED .....: 18.09.02/jr fixed frame outlook
                  13.11.02 lp - definition's view changed(like in to JG)
                              - added Invoice group in the frame valinta
                              - arbalrep.p changed too(only two parameters)
                  12.09.03/aam brand                              
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/utumaa.i "new"}

assign tuni1 = "arbalrel"
       tuni2 = "".

DEF VAR ufkey      AS LOG                     NO-UNDO.
DEF VAR lcInvGroup AS CHAR  FORMAT "X(8)"     NO-UNDO EXTENT 2.

form
   SKIP(2)
   "     This program prints out a Balance report from" SKIP
   "     - deposits" SKIP
   "     - overpayments" SKIP
   "     - advance payments" SKIP
   "     - unregistered payments"
   SKIP(2)
   "     Note: Limiting invoicing groups doesn't apply to unregistered" SKIP
   "           payments."
   SKIP(1)
   lcInvGroup[1] COLON 22 LABEL "Invoicing groups"
      HELP "Invoicing group"
   "-"
   lcInvGroup[2] NO-LABEL
      HELP "Invoicing group"
      SPACE(2)
   SKIP(4)
   WITH ROW 1 SIDE-LABELS WIDTH 80
        TITLE " " + ynimi + " BALANCE REPORT " + STRING(pvm,"99-99-99") + " "
FRAME valinta.

VIEW FRAME valinta.
PAUSE 0 NO-MESSAGE.

FIND LAST InvGroup WHERE InvGroup.Brand = gcBrand NO-LOCK NO-ERROR.
IF AVAILABLE InvGroup THEN ASSIGN lcInvGroup[2] = InvGroup.InvGroup.

DISPLAY lcInvGroup WITH frame valinta.
ASSIGN
   ufkey = false
   nap   = "first".

toimi:
   REPEAT WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:
      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132  
         ufk[2]= 0  ufk[3]= 0 ufk[4]= 0
         ufk[5]= 63 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      IF nap NE "first" THEN DO:
         READKEY.
         ASSIGN
         nap = keylabel(lastkey).
      END.
      ELSE ASSIGN nap = "1".

      IF LOOKUP(nap,"1,f1") > 0 THEN DO:
         ehto = 9. 
         RUN Syst/ufkey.p.
         REPEAT WITH frame valinta ON ENDKEY UNDO, LEAVE:
            UPDATE 
               lcInvGroup[1]
               lcInvGroup[2]
               VALIDATE(INPUT lcInvGroup[2] >= INPUT lcInvGroup[1],
                        "Invalid choice !").
            LEAVE.
         END.
         ufkey = TRUE.
         NEXT toimi.
      END.

      ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:
         LEAVE toimi.
      END.

      ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         RETURN.
      END.
   END. /* toimi */

ASSIGN tila = TRUE.
{Syst/tmsreport.i "return"}

MESSAGE "Printing in process...".            

RUN Ar/arbalrep.p(lcInvGroup[1],
               lcInvGroup[2]).

ASSIGN tila = FALSE.
{Syst/tmsreport.i}        

MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.

HIDE MESSAGE       NO-PAUSE.
HIDE FRAME valinta NO-PAUSE.
