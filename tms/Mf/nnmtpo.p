/* -----------------------------------------------------------------
  MODULE .......: NNMTPO.P
  FUNCTION .....: DELETE old monthly Calls
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 07-01-98
  MODIFIED .....: 20-07-01 kl: bug fixed
                  11.03.03 tk: tokencheck
  Version ......: M15
  ----------------------------------------------------------------- */


{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MthCall'}

IF lcRight NE "RW" THEN DO:
   MESSAGE 
      " You do not have right " SKIP
      "  to delete counters ! "
   VIEW-AS ALERT-BOX.   
   RETURN.
END.


def var ok  as log format "Yes/No" NO-UNDO.
def var Month as i   format "999999" NO-UNDO.
DEF VAR cnt AS i                   NO-UNDO.

form
skip(1)
"  Instruction:  This program deletes all customer's monthly        " skip
"                counter records up to the month given below.       " skip(2)
"                Give month (YYYYMM) :" Month
                 HELP
                 " All monthly Calls up to this month will be deleted !"
                                                                      skip(11)

with row 1 title " DELETE MONTHLY CALL COUNTERS " width 80
   NO-LABELS FRAME frm.

IF month(pvm) NE 1 THEN ASSIGN
   Month = (year(pvm) * 100) + month(pvm) - 1.
ELSE ASSIGN
   Month = ((year(pvm) * 100) - 100) + 12.


LOOP:
repeat:
ehto = 9. RUN ufkey.

   UPDATE Month WITH FRAME frm.

do-it:
   repeat WITH FRAME frm:
      ASSIGN ufk = 0 ehto = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  LOOP.
      IF toimi = 8 THEN LEAVE LOOP.
      IF toimi = 5 THEN LEAVE do-it.
   END.

   IF Month NE 0 THEN DO:
      ok = FALSE.
      message " Are you sure you want to start deleting ?" UPDATE ok.
      IF ok THEN DO:
         FIND FIRST MthCall where MthCall.Month <= Month no-lock no-error.
         IF NOT AVAIL MthCall THEN DO:
            BELL.
            message " No earlier monthly Calls found - press ENTER !".
            PAUSE no-message.
            NEXT LOOP.
         END.
         message "Deleting in process ......".
         /* DELETE ALL older values from monthly Calls */
         FOR EACH MthCall where MthCall.Month <= Month TRANS:
            cnt = cnt + 1.
            PUT SCREEN ROW 23 col 60 string(cnt).
            DELETE MthCall.
         END.
      END. /* ok */
   END.
   ELSE LEAVE LOOP.
END. /* LOOP */

put screen row 23 col 60 "       ".

HIDE MESSAGE no-pause.
HIDE FRAME frm.

