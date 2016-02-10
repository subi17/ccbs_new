/* -----------------------------------------------
  MODULE .......: tmcdelete.p
  FUNCTION .....: Delete old TM counters
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.05.08
  MODIFIED .....: 
------------------------------------------------------ */

{Syst/commali.i}
{Syst/eventlog.i}
{Func/timestamp.i}

DEF INPUT PARAMETER idtBreakDate AS DATE NO-UNDO.

DEF VAR liQty   AS INT  NO-UNDO.
DEF VAR llRetry AS LOG  NO-UNDO.
DEF VAR liLoop  AS INT  NO-UNDO.
DEF VAR ldtFrom AS DATE NO-UNDO.
DEF VAR ldtTo   AS DATE NO-UNDO.

DEF BUFFER bCounter FOR TMCounter.

DEF TEMP-TABLE ttRule NO-UNDO
   FIELD TMRuleSeq AS INT
   INDEX TMRuleSeq TMRuleSeq.


fELog("MONTHLY","TMCounterDeleteStarted").

IF idtBreakDate = ? THEN idtBreakDate = TODAY.

/* delete only counters with correct type */
FOR EACH TMRule NO-LOCK WHERE
         TMRule.Brand = gcBrand AND
         TMRule.CounterType = 1:
   CREATE ttRule.
   ttRule.TMRuleSeq = TMRule.TMRuleSeq.
END.

llRetry = FALSE.

REPEAT
liLoop = 1 TO 50:

   COUNTER_LOOP:
   FOR EACH TMCounter NO-LOCK WHERE
            TMCounter.ToDate < idtBreakDate,
      FIRST ttRule WHERE
            ttRule.TMRuleSeq = TMCounter.TMRuleSeq:
   
      DO TRANS:
         FIND bCounter WHERE RECID(bCounter) = RECID(TMCounter) 
            EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
         IF LOCKED(bCounter) THEN DO:
            llRetry = TRUE.
            NEXT COUNTER_LOOP.
         END.
         
         IF ldtFrom = ? THEN ASSIGN
            ldtFrom = TMCounter.FromDate
            ldtTo   = TMCounter.ToDate.
         
         ELSE ASSIGN
            ldtFrom = MIN(ldtFrom,TMCounter.FromDate)
            ldtTo   = MAX(ldtTo,TMCounter.ToDate).
         
         DELETE bCounter.
         
         liQty = liQty + 1.
      END. /* TRANS */
   END.
   
   IF NOT llRetry THEN LEAVE.
END.

DO TRANS:
   CREATE ActionLog.
   ASSIGN 
      ActionLog.Brand        = gcBrand   
      ActionLog.TableName    = "TMCounter"  
      ActionLog.KeyValue     = STRING(YEAR(TODAY),"9999") + 
                               STRING(MONTH(TODAY),"99")  +
                               STRING(DAY(TODAY),"99")
      ActionLog.ActionID     = "TMCDelete"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 + 
                               MONTH(TODAY)
      ActionLog.ActionDec    = liQty
      ActionLog.ActionChar   = STRING(liQty) + 
                               " counters dated before " + 
                               STRING(idtBreakDate,"99.99.9999") +
                               " were deleted."
      ActionLog.ActionStatus = 3
      ActionLog.UserCode     = "Cron"
      ActionLog.FromDate     = ldtFrom
      ActionLog.ToDate       = ldtTo.
      ActionLog.ActionTS     = fMakeTS().
END.

fELog("MONTHLY","TMCounterDeleteStopped:" + STRING(liQty)).

