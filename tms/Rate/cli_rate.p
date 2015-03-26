/* ===========================================================================
 MODULE ........: CLI_rate.p
 APPLICATION ...: Ticket Master
 TASK ..........: Analyse Mobile CDRs
 CREATED .......: 27.12.04/aam (from man_rate)
 CHANGED .......: 11.01.07/aam fInvSeq to mobcdr_rate.i
                  30.01.08 jp fBCopy

 VERSION .......: M15
 ============================================================================*/

&GLOBAL-DEFINE CounterHandling TempTable
         
{commali.i}      
{rerate_define.i}
{premiumnumber.i}

DEF INPUT PARAMETER icCLI    AS CHAR NO-UNDO.
DEF INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
DEF INPUT PARAMETER idtTo    AS DATE NO-UNDO. 
DEF INPUT PARAMETER ilSilent AS LOG  NO-UNDO.


/* Default values */
ASSIGN 
  lcRerateSource = "CLI"
  bbatch   = SESSION:BATCH
  cdate1   = idtFrom
  cdate2   = idtTo
  CLI      = icCLI.

IF NOT bbatch THEN bbatch = ilSilent.

ldestamp = YEAR(cdate1)  * 10000 +
           MONTH(cdate1) * 100   +
           DAY(cdate1) .
      
fEmptyRerateTempTables().

FOR EACH MsOwner NO-LOCK WHERE
         MsOwner.CLI      = icCLI AND
         MsOwner.TSEnd   >= ldestamp AND
         MsOwner.PayType = FALSE
BREAK BY MsOwner.MsSeq
      BY MsOwner.CustNum:
         
   IF NOT FIRST-OF(MsOwner.CustNum) THEN NEXT.
   
   /* counters to temp-table (with null amounts) */
   fServiceLCounter2Temp(MsOwner.MsSeq,
                         MsOwner.Custnum,
                         cDate1,
                         cDate2).

   fDCCounter2Temp(MsOwner.MsSeq,
                   cDate1,
                   cDate2).

   fSaldoCounter2Temp(MsOwner.MsSeq,
                      cDate1,
                      cDate2).

END.

fInitializeRerateLog(0,
                     0,
                     icCLI,
                     lcRerateSource,
                     cdate1,
                     cdate2).
  
fFillTT().

liRerateSeq = fRerateLogStart (
   katun,
   idtFrom,
   idtTo,
   icCLI,
   0, /* custnum from */
   0, /* custnum to */
   "", /* clitype */
   0, /* ErrorCode */
   "", /* InvGroup */
   ""). /* icInvRunCode*/

ETIME(YES).
Main:
REPEAT:

   DO:
   
      MobCDR: 
      FOR EACH MobCDR  NO-LOCK WHERE   
               MobCDR.CLI      = icCLI  AND         
               MobCDR.datest  >= cdate1 AND
               MobCDR.datest  <= cdate2    
      TRANSACTION WITH FRAME MobCDR: 
       
         {man_rate2.i}
       
fRerateLogFinish(liRerateSeq).

