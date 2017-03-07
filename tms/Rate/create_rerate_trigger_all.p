/* create_rerate_trigger_all.p   14.02.12/aam
*/

{Syst/commpaa.i}
ASSIGN 
   gcBrand = "1"
   katun = "cron".
{Syst/eventlog.i}
{Func/cparam2.i}
{Func/date.i}

DEF VAR llCreated  AS LOG  NO-UNDO.
DEF VAR liRateDay  AS INT  NO-UNDO.
DEF VAR lcLastDays AS CHAR NO-UNDO.
DEF VAR liLoop     AS INT  NO-UNDO.
DEF VAR ldaLastDay AS DATE NO-UNDO.

fELog("Trigger_rerate_all","Started").

ASSIGN 
   lcLastDays = fCParamC("RerateALLDays")
   ldaLastDay = fLastDayOfMonth(TODAY)
   llCreated  = FALSE.
IF lcLastDays = ? THEN lcLastDays = "". 
   
/* do only on defined number of last days of the month */
DO liLoop = 1 TO NUM-ENTRIES(lcLastDays):

   liRateDay = INT(ENTRY(liLoop,lcLastDays)) NO-ERROR.
   IF ERROR-STATUS:ERROR OR liRateDay = 0 THEN NEXT.
   
   IF ldaLastDay - TODAY + 1 = liRateDay THEN 
   FOR FIRST TriggerConf NO-LOCK WHERE
             TriggerConf.TriggerConfID = "ALL"  AND
             TriggerConf.EventRule     > 0      AND
             TriggerConf.ValidTo       >= TODAY AND
             TriggerConf.ValidFrom     <= TODAY:

      CREATE TriggerEvent.
      ASSIGN
         TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
         TriggerEvent.TriggerConfID  = TriggerConf.TriggerConfID
         TriggerEvent.EventSource    = "CRON"
         TriggerEvent.Created        = NOW
         TriggerEvent.TableID        = 0
         TriggerEvent.TableName      = TriggerConf.TriggerConfID
         TriggerEvent.Keyvalue       = ""
         TriggerEvent.ChangedFields  = "Batch"
         TriggerEvent.ChangedValues  = ""
         llCreated = TRUE.

      RELEASE TriggerEvent.
   END.
   
   IF llCreated THEN LEAVE. 
END.

fELog("Trigger_rerate_all","Stopped:" + 
                           STRING(llCreated,"Created/Not created")).

QUIT.


