 /* ------------------------------------------------------
  MODULE .......: opbalrel.p
  FUNCTION .....: ui FOR overpayment balance report 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 06.03.03/aam 
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

{Syst/utumaa.i "new"}

assign tuni1 = "opbalrel"
       tuni2 = "".

DEF VAR ufkey      AS LOG                     NO-UNDO.
def var ok         as log   format "Yes/No"   NO-UNDO.
DEF VAR ldtDate    AS DATE  FORMAT "99-99-99" NO-UNDO.
DEF VAR oiCount    AS INT                     NO-UNDO. 

form
   skip(2)
   "This program prints out a balance report from"      AT 10 SKIP
   "customers' overpayments, that have been entered to" AT 10 SKIP
   "TMS before (and including) the below defined date." AT 10 SKIP(2)
   ldtdate AT 10
      LABEL "Payments made before"
      HELP "Latest posting date of overpayments to be reported" 
   skip(9)
   WITH ROW 1 side-labels width 80
        title " " + ynimi + " OVERPAYMENT BALANCE REPORT " +
        string(pvm,"99-99-99") + " "
        FRAME valinta.

view FRAME valinta.
PAUSE 0 no-message.

ASSIGN ldtDate = TODAY - 150
       ldtDate = DATE(MONTH(ldtDate),1,YEAR(ldtDate)) - 1.

DISPLAY ldtDate WITH FRAME valinta. 

ufkey = FALSE.

toimi:
   repeat WITH FRAME valinta ON ENDKEY UNDO toimi, NEXT toimi:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 132 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0 /* 847 */
         ufk[5]= 63  ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 
         ufk[9]= 1
         ehto = 3.
         RUN Syst/ufkey.p.

         READKEY.
         nap = keylabel(LASTKEY).
      END.

      ELSE nap = "1".

      if lookup(nap,"1,f1") > 0 THEN DO:

         ASSIGN ehto = 9 ufkey = TRUE.
         RUN Syst/ufkey.p.
         UPDATE ldtDate
                WITH FRAME valinta.
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
{Syst/utuloste.i "return"}

message "Printing in process...".         


RUN Ar/opbalrep  (ldtDate,
               OUTPUT oiCount).

ASSIGN tila = FALSE.
{Syst/utuloste.i}

MESSAGE oiCount "customers with overpayment balance were reported."
VIEW-AS ALERT-BOX
INFORMATION.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

