/* ------------------------------------------------------
  MODULE .......: COPAYREM
  FUNCTION .....: Mark commission events as paid 
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.02.03
  MODIFIED .....: 
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}

DEF TEMP-TABLE ttMark NO-UNDO
    FIELD EventID AS INT.

DEF INPUT  PARAMETER TABLE FOR ttMark.
DEF INPUT  PARAMETER idtPaymDate AS DATE  NO-UNDO. 
DEF OUTPUT PARAMETER oiCount     AS INT   NO-UNDO.


FOR EACH ttMark,
FIRST CoEvent EXCLUSIVE-LOCK WHERE
      CoEvent.CoEventId = ttMark.EventId:

   ASSIGN CoEvent.PaymDate = idtPaymDate
          oiCount          = oiCount + 1.

END.           

