/*===========================================================================
 MODULE ........: msopenbatch.p
 APPLICATION ...: create open callalarm
 TASK ..........:
 CREATED .......: 29.01.03 jp
 CHANGED .......:
                  13.12.04/aam SSDate for SubSer
                  31.12.04/aam CreditType from SubSer,
                               use buffer for MobSub update
                  20.04.05 jp  open status 37 also
                  29.12.05/aam logic to msopenbatch.i
                  30.05.06 jl  log file changed
                  03.07.06/aam msstatus 47
 VERSION .......:
============================================================================*/

{commali.i}
{timestamp.i}
{fsubser.i}
{msopenbatch.i}

DEF STREAM AlarmBatch.
DEF STREAM log.
DEF STREAM email.


DEF VAR AlarmBatchDir   AS C   NO-UNDO.
DEF VAR SbFile          AS C   NO-UNDO.
DEF VAR LogFile         AS C   NO-UNDO.
DEF VAR emailfile       AS C   NO-UNDO.
DEF VAR i               AS I   NO-UNDO.
DEF VAR llDone          AS LOG NO-UNDO.
DEF VAR liCurrPeriod    AS INT NO-UNDO.
DEF VAR liCreditType    AS INT NO-UNDO.

{cparam.i AlarmBatchDir RETURN}. AlarmBatchDir = tmsparam.charval. 

ASSIGN
   emailfile      = AlarmBatchDir + "tmsmail.log"
   sbfile         = AlarmBatchDir + "alarm"
   logfile        = sbfile + ".log"
   liCurrPeriod   = YEAR(TODAY) * 100 + MONTH(TODAY).

OUTPUT STREAM Alarmbatch TO value(alarmbatchdir + "subscriptions_open.txt") APPEND.

PUT STREAM alarmbatch UNFORMATTED
   TODAY "|" string(time,"hh:mm:ss") "|Start" SKIP.
 
FOR EACH Mobsub NO-LOCK WHERE
         MobSub.Brand    = gcBrand AND
         LOOKUP(STRING(Mobsub.msstatus),"7,37,47") > 0:
   
   liCreditType = fCreditTypeValue(MobSub.MsSeq,
                                   OUTPUT i).
   IF liCreditType NE 3 THEN NEXT.

   llDone = fOpenSaldoBarring().

   IF llDone THEN DO:
      /* set counter limit to zero */
      FOR EACH SaldoCounter EXCLUSIVE-LOCK WHERE
               SaldoCounter.MsSeq  = MobSub.MsSeq AND
               SaldoCounter.Period = liCurrPeriod:
         SaldoCounter.MobLimit = 0.
      END. 
   END.
     
   PUT STREAM alarmbatch UNFORMATTED
      TODAY                   "|" 
      string(time,"hh:mm:ss") "|" 
      mobsub.custnum          "|" 
      mobsub.cli              "|"
      STRING(llDone,"DONE/ERROR")
      SKIP.
               
END.

OUTPUT STREAM Alarmbatch CLOSE.

