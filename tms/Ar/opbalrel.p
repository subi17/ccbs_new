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
        title " " + Syst.CUICommon:ynimi + " OVERPAYMENT BALANCE REPORT " +
        string(TODAY,"99-99-99") + " "
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
         Syst.CUICommon:ufk[1]= 132 Syst.CUICommon:ufk[2]= 0 Syst.CUICommon:ufk[3]= 0 Syst.CUICommon:ufk[4]= 0 /* 847 */
         Syst.CUICommon:ufk[5]= 63  Syst.CUICommon:ufk[6]= 0 Syst.CUICommon:ufk[7]= 0 Syst.CUICommon:ufk[8]= 8 
         Syst.CUICommon:ufk[9]= 1
         Syst.CUICommon:ehto = 3.
         RUN Syst/ufkey.p.

         READKEY.
         Syst.CUICommon:nap = keylabel(LASTKEY).
      END.

      ELSE Syst.CUICommon:nap = "1".

      if lookup(Syst.CUICommon:nap,"1,f1") > 0 THEN DO:

         ASSIGN Syst.CUICommon:ehto = 9 ufkey = TRUE.
         RUN Syst/ufkey.p.
         UPDATE ldtDate
                WITH FRAME valinta.
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

/* Avataan striimi */
ASSIGN tila = TRUE.
{Syst/utuloste.i "return"}

message "Printing in process...".         


RUN Ar/opbalrep.p  (ldtDate,
               OUTPUT oiCount).

ASSIGN tila = FALSE.
{Syst/utuloste.i}

MESSAGE oiCount "customers with overpayment balance were reported."
VIEW-AS ALERT-BOX
INFORMATION.

HIDE MESSAGE no-pause.
HIDE FRAME rajat no-pause.
HIDE FRAME valinta no-pause.

