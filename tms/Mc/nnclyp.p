/*------------------------------------------------------
  MODULE .......: NNCLYP.P
  FUNCTION .....: Customer letter maintenance & printing
  APPLICATION ..: TELE1
  AUTHOR .......: KL
  CREATED ......: 18.03.1998
  changePVM ....: 04.11.2002 jr Eventlog
                  07.03.2003 tk tokens
                  14.11.2003/aam brand 
  Version ......: M15
  ------------------------------------------------------ */

DEF VAR ufkey       AS LOG NO-UNDO  init TRUE.
DEF VAR i           AS i   NO-UNDO.
DEF VAR new_custlet AS LOG NO-UNDO INIT FALSE.

{Syst/commali.i}
{Syst/eventval.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CustLetter'}

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustLetter AS HANDLE NO-UNDO.
   lhCustLetter = BUFFER CustLetter:HANDLE.
   RUN StarEventInitialize(lhCustLetter).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhCustLetter).
   END.
END.


form /* memo */
WITH
    OVERLAY ROW 1 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update letter "
    FRAME memo.

DO TRANS WITH FRAME memo ON ENDKEY UNDO, RETURN:

   assign ehto = 9 cfc = "lis" ufkey = TRUE.
   RUN Syst/ufkey. RUN Syst/ufcolor.
   FIND FIRST CustLetter WHERE CustLetter.Brand = gcBrand
      exclusive-lock no-error.
   IF NOT AVAIL CustLetter THEN 
   DO:
      IF lcRight NE "RW" THEN DO:
         MESSAGE "No custletter available !" VIEW-AS ALERT-BOX.
         RETURN.
      END.
      new_custlet = TRUE.
      CREATE CustLetter.
      CustLetter.Brand = gcBrand.
   END.

   IF NOT new_custlet AND
      llDoEvent THEN RUN StarEventSetOldBuffer(lhCustLetter).

   DISPLAY CustLetter.LtrText [1 FOR 17] WITH FRAME memo 1 col.   

   IF lcRight = "RW" THEN DO:   

      UPDATE text(CustLetter.LtrText [1 FOR 17]) WITH FRAME memo 1 col.
      /* UNDO WITH F4 -key */
      if keylabel(lastkey) = "F4" THEN DO:
         HIDE FRAME memo no-pause.
         UNDO, LEAVE.
      END.

      IF new_custlet AND
         llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustLetter).

      IF NOT new_custlet AND
         llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustLetter).
   END.
   ELSE PAUSE.      

   HIDE FRAME memo no-pause.
END.

DO i = 1 TO 17 TRANS:
   IF CustLetter.LtrText[i] entered THEN DO:
      CustLetter.ChgDate = pvm.
      LEAVE.
   END.
END.

