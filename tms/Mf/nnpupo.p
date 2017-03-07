/* -----------------------------------------------------------------
  MODULE .......: NNPUPO.P
  FUNCTION .....: VANHIJEN PUHELUTIETUEIDEN removal ERAAJONA
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 18-03-96
  MODIFIED .....: 24-11-00 kl: PROCEDURE pDelCall + english
                  13.03.03 tk: tokens
  Version ......: M15
  ----------------------------------------------------------------- */


{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'FixCDR'}

IF lcRight NE "RW" THEN DO:
   MESSAGE 
      "You do not have rights" SKIP
      "to delete calls !"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

def var bOk   as log  format "Yes/No" NO-UNDO.
def var ToDate   as Date format "99-99-9999" NO-UNDO.

DEF VAR deletepwd           AS CHAR                 NO-UNDO.

form
skip(1)
"         This module REMOVES ALL Billed calls RECORDS" 
"         until the given Date !"  skip (2)
ToDate   label "        Give Date "
          HELP
          " All OLDER Billed calls including given Date will be removed"
          skip(11)

with row 1 title " REMOVING OLD Billed calls " width 80
   side-labels FRAME CallsTo.

form skip(1)
   "   !!!!!! calls TO BE REMOVED !!!!!!"                skip(1)
   " calls until" ToDate NO-LABEL skip(1)
   " Are You SURE (Y/N)" bOk
   help "Y=star removing, N=CANCEL"                      skip(1)
with overlay title " W A R N I N G !!! " ROW 6 centered
   NO-LABELS FRAME BeSure.

{Syst/pwd.i00}

PAUSE 0 no-message.
ehto = 9. RUN Syst/ufkey.p.
UPDATE ToDate WITH FRAME CallsTo.

IF ToDate NE ? THEN DO:

   bOk = FALSE.
   message " Start REMOVING (Y/N) ?" UPDATE bOk.
   IF bOk THEN DO:
      FIND FIRST InvSeq where 
                 InvSeq.ToDate   <= ToDate  AND
                 InvSeq.Billed = FALSE
      no-lock no-error.
      IF NOT AVAIL InvSeq THEN DO:
         BELL.
         message " No Billed calls withing criteria - press ENTER !".
         PAUSE no-message.
      END.
      ELSE DO:
         PAUSE 0 no-message.
         DO WITH FRAME BeSure:
            bOk = FALSE.
            DISPLAY ToDate.
            UPDATE bOk.
         END.
         IF NOT bOk THEN LEAVE.

      END. /* on poistettavia */

   END. /* IF bOk, 1. varmistus */

   IF bOk THEN RUN pDelCalls
     (INPUT FromDate, INPUT ToDate).

END. /* ToDate annettu */

HIDE MESSAGE no-pause.
HIDE FRAME BeSure.
HIDE FRAME CallsTo.

PROCEDURE pDelCalls.

   DEF INPUT PARAMETER FromDate AS DA NO-UNDO.
   DEF INPUT PARAMETER ToDate   AS DA NO-UNDO.

   DEF VAR Qty AS i NO-UNDO.

   DEF BUFFER delcall FOR FixCDR.

   FOR EACH InvSeq no-lock where
            InvSeq.ToDate   <= ToDate AND
            InvSeq.Billed = FALSE,

       EACH FixCDR no-lock USE-INDEX InvSeq where
            FixCDR.InvSeq = InvSeq.InvSeq.

      Qty = Qty + 1.
      IF time MOD 60 = 0 THEN DO:
         PUT SCREEN ROW 18 col 10 
            string(FixCDR.Date,"99-99-99")  + " " +
            string(FixCDR.TimeStart,"hh:mm:ss") + " " +
            string(Qty).
      END.

      FIND FIRST delcall where recid(delcall) = recid(FixCDR).
      DELETE delcall.
   END.         

   HIDE FRAME BeSure.
   MESSAGE 
   "All calls between" string(FromDate,"99-99-99") "-" 
   string(ToDate,"99-99-99") "are now removed !"
   VIEW-AS ALERT-BOX MESSAGE.   

END PROCEDURE.

