/* ===========================================================================
 MODULE ........: CLI_ratep.p
 APPLICATION ...: Ticket Master
 TASK ..........: Analyse Mobile CDRs, persistent routines
 CREATED .......: 02.08.05/aam (from cli_rate)
 CHANGED .......: 11.01.07/aam fInvSeq to mobcdr_rate.i
                  30.01.08 jp fBCopy

 VERSION .......: M15
 ============================================================================*/

&GLOBAL-DEFINE PersistentRun YES
&GLOBAL-DEFINE CounterHandling TempTable

DEFINE VARIABLE objDynQueryMServiceLimit AS CLASS Syst.DynQuery NO-UNDO.
objDynQueryMServiceLimit = NEW Syst.DynQuery().
objDynQueryMServiceLimit:mAddBuffer(BUFFER mServiceLimit:HANDLE).

{Syst/commali.i}
{Rate/rerate_define.i}
{Rate/premiumnumber.i}

PROCEDURE pInitializeRerate:

   ASSIGN
      bbatch   = SESSION:BATCH
      lcRerateSource = "CLIP". 

   fFillTT().

END PROCEDURE.

PROCEDURE pInitializeRerateReport:

   DEF INPUT PARAMETER icUserCode AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idtFrom AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtTo AS DATE NO-UNDO.
   DEF INPUT PARAMETER icInvRunCode AS CHAR NO-UNDO.

   liRerateSeq = fRerateLogStart (
      icUserCode,
      idtFrom,
      idtTo,
      "", /* cli */
      0, /* custnum from */
      0, /* custnum to*/
      "", /* clitype */
      0, /* ErrorCode */
      "", /* InvGroup */
      icInvRunCode).

END PROCEDURE.

PROCEDURE pFinalizeRerateReport:
   
   IF liRerateSeq > 0 THEN fRerateLogFinish(liRerateSeq).

END PROCEDURE.


PROCEDURE pRunRerate:

   DEF INPUT PARAMETER icCLI    AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
   DEF INPUT PARAMETER idtTo    AS DATE NO-UNDO. 
   DEF INPUT PARAMETER ilSilent AS LOG  NO-UNDO.

   ASSIGN 
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
 
   ETIME(YES).
   Main:
   REPEAT:

      DO:
   
         MobCDR: 
         FOR EACH MobCDR NO-LOCK  WHERE   
                  MobCDR.CLI      = icCLI  AND         
                  MobCDR.datest  >= cdate1 AND
                  MobCDR.datest  <= cdate2    
         TRANSACTION WITH FRAME MobCDR: 
       
            {Rate/man_rate2.i}

FINALLY:
   IF VALID-OBJECT(objDynQueryMServiceLimit)
   THEN DELETE OBJECT objDynQueryMServiceLimit.
END FINALLY.

