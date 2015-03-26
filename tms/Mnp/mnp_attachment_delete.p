/* ----------------------------------------------------------------------
MODULE .......: mnp_attachment_delete.p
TASK .........: Deletes MNP cancellation attachments. YDR-233 
APPLICATION ..: TMS
AUTHOR .......: anttis
CREATED ......: 04.03.11
Version ......: yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{tmsconst.i}
{date.i}

DEF VAR liEnd AS INT NO-UNDO.
DEF VAR i AS INT NO-UNDO.
DEF VAR lcStatuses AS CHAR NO-UNDO.
DEF VAR liStatusCode AS INT NO-UNDO.
DEF VAR ldaLimit AS DATE NO-UNDO. 
DEF VAR liDeleted AS INT NO-UNDO. 

lcStatuses = "0,1,2,3,4".

ldaLimit = TODAY - 180.

liEnd = INT(STRING(YEAR(ldaLimit)) +
            STRING(MONTH(ldaLimit),"99") +
            STRING(DAY(ldaLimit),"99")).

DO i = 1 TO NUM-ENTRIES(lcStatuses):

   liStatusCode = INT(ENTRY(i,lcStatuses)).

   FOR EACH MNPCancelProposal WHERE
            MNPCancelProposal.StatusCode = liStatusCode AND
            MNPCancelProposal.CreatedTs < liEnd EXCLUSIVE-LOCK:
      DELETE MNPCancelProposal. 
      liDeleted = liDeleted + 1.
   END.

END.

IF liDeleted > 0 THEN DO TRANS:
   CREATE ActionLog.
   ASSIGN
      ActionLog.Brand        = gcBrand
      ActionLog.TableName    = "Cron"
      ActionLog.KeyValue     = STRING(YEAR(TODAY) * 100 +
                                      MONTH(TODAY) * 100 +
                                      DAY(TODAY))
      ActionLog.ActionID     = "MNPDELETE"
      ActionLog.ActionPeriod = YEAR(TODAY) * 100 +
                               MONTH(TODAY)
      ActionLog.ActionDec    = liDeleted
      ActionLog.ActionChar   = "Number of deleted MNP attachments: " + 
                               STRING(liDeleted)
      ActionLog.ActionStatus = 3
      ActionLog.ActionTS     = fMakeTS().
END.
