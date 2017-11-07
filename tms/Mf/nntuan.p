/* ----------------------------------------------------------------------------
  MODULE .......: NNTUAN.P
  FUNCTION .....: Browse of unknown A-subscribers
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 02.04.1997 pt
  MODIFIED .....: 22.04.02 lp - change TO english
                              - ON F8 ENDKEY.
  Version ......: M15
  --------------------------------------------------------------------------- */
{Syst/commali.i}

IF NOT CAN-FIND(FIRST FixCDR where FixCDR.InvCust = 1000) THEN DO:
   MESSAGE "There are no unknown A-subscribers !" VIEW-AS ALERT-BOX.
   RETURN.
END.


FOR
   EACH FixCDR no-lock where
        FixCDR.InvCust = 1000
BREAK
BY CLI.

   accumulate
   FixCDR.Duration(sub-count BY CLI)
   FixCDR.Duration(sub-total BY CLI).

   IF last-of(CLI) THEN DO:
      DISP
      FixCDR.CLI                               column-label "A-number"
      (accum sub-count by CLI Duration)       column-label "Call piece"
                                                  format "zzz,zz9"
      (accum sub-total by CLI Duration) / 60  column-label "Call min"
                                                  format "zzz,zz9"
      WITH
      centered overlay row 3 title " UNKNOWN A-NUMBER " 13 DOWN FRAME LOG.

      IF frame-line(LOG) < frame-down(LOG) THEN DOWN WITH FRAME LOG.
      ELSE DO:
         ON F8 ENDKEY.
         message "More A-numbers - press ENTER !".
         PAUSE no-message.
      END.

   END.
END.
MESSAGE "PRESS ENTER". PAUSE NO-MESSAGE.
