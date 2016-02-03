/* ---------------------------------------------------------------------------
  MODULE .......: NNCDRSW.P
  FUNCTION .....: FOR starting the nncdr -module that reads in hotline CDRs
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 28.07.1998
  MODIFIED .....: 23.06.1999 kl DO NOT RETURN, QUIT
                  27.06.1999 kl smaller compiler blocks
                  02.11.1999 kl PARAMETER bDispErrors
                  01.02.2002 lp change {Syst/commpaa.i} --> {Syst/commali.i new}
                  07.03.2003 kl MASTER 1.0

  VERSION ......: M15
  -------------------------------------------------------------------------- */

{Syst/commpaa.i new}
{Func/cparam2.i}

def var pvmlog        as log no-undo format "Yes/No" init TRUE.
def var ok            as log no-undo format "Yes/No" init TRUE.
def var bDispErrors   as log no-undo format "Yes/No" init TRUE.
DEF VAR i             AS i   NO-UNDO.
DEF VAR psax          AS c   NO-UNDO.
def var iCust         as i   no-undo.

iCust = fCParamI("UnknownCustomer").

/* Unknown customer must be in database before reading in CDRs  */
IF NOT can-find(Customer where 
                Customer.CustNum = iCust) THEN DO:
   MESSAGE
      "'Unknown customer'" iCust
      " is not in customer file !"
   VIEW-AS ALERT-BOX.
   QUIT.
END.

/* check how many cdrsw scripts are started */
i = 0.
input through("ps -af | grep cdrsw.pf").
repeat:
   IMPORT UNFORMATTED psax.
   if index(psax,"grep") > 0 THEN NEXT.
   if index(psax,"cdrsw.pf") > 0 THEN i = i + 1.
END.

/* more that 1 is an error */
IF i > 1 THEN ok = FALSE.

form
   "------------------------  WARNING  -----------------------------"
   skip(1)
WITH no-box ROW 5 centered FRAME warn.
form
   "     CDR reading from has been already started. If that module"    SKIP
   "     is still running a new module will CAUSE ERRORS. IF You  "    SKIP
   "     are sure that there is no other module running then answer"   SKIP
   "     OK, else answer NO !"                                         skip(1)
   "     Start a new module (Yes/No) ?:" ok                            skip(1)
WITH OVERLAY centered NO-LABELS no-box width 70 FRAME frm.

form
   skip(1)
   "    Separated log file for each day   (Y/N)?"   pvmlog 
   help "Create a new log file for each day (Y/N) "
   skip(1)
   "    Display error / send error emails (Y/N)?"   bDispErrors
   help "Send error messages by email and display them on screen (Y/N)?"
   skip(1)
with centered overlay row 3 title " READ IN CALL RECORDS "
     NO-LABELS width 60 FRAME loki.

/* module already running */
IF NOT ok THEN DO:

   DISP WITH FRAME warn.
   UPDATE ok WITH FRAME frm.

   HIDE FRAME warn no-pause.
   HIDE FRAME frm  no-pause.

END.

IF ok THEN DO:
   ehto = 9. RUN ufkey.
   UPDATE pvmlog bDispErrors WITH FRAME loki.

   ufk = 0. ehto = 3. 
   RUN ufkey. PAUSE 0.

   message "Are You SURE You want to start reading CDRs into database ?"
   UPDATE ok.
END.

HIDE FRAME loki.

if ok then run fixonline1(pvmlog,"OnLine",bDispErrors).

