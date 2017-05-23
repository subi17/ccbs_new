/* ===========================================================================
 MODULE ........: cust_rate.p
 APPLICATION ...: Ticket Master
 TASK ..........: Analyse Mobile CDRs
 CREATED .......: 28.04.05/jp (from cli_rate)
 CHANGED .......: 11.01.07/aam fInvSeq to mobcdr_rate.i
                  30.01.08 jp fBCopy

 VERSION .......: M15
 ============================================================================*/

&GLOBAL-DEFINE CounterHandling TempTable
         
{Syst/commali.i}      
{Rate/rerate_define.i}
{Rate/premiumnumber.i}

DEF INPUT PARAMETER iiCustNum  AS INT NO-UNDO.
DEF INPUT PARAMETER idtFrom    AS DATE NO-UNDO.
DEF INPUT PARAMETER idtTo      AS DATE NO-UNDO. 
DEF INPUT PARAMETER ilSilent   AS LOG  NO-UNDO.

DEFINE VARIABLE objDynQueryMServiceLimit AS CLASS Syst.DynQuery NO-UNDO.
objDynQueryMServiceLimit = NEW Syst.DynQuery().
objDynQueryMServiceLimit:mAddBuffer(BUFFER mServiceLimit:HANDLE).

DEF VAR ldeBegStamp AS DEC  NO-UNDO.

/* Default values */
ASSIGN 
  lcRerateSource = "CUST"
  bbatch   = SESSION:BATCH
  cdate1   = idtFrom
  cdate2   = idtTo.

IF NOT bbatch THEN bbatch = ilSilent.


ldestamp = YEAR(cdate1)  * 10000 +
           MONTH(cdate1) * 100   +
           DAY(cdate1) .

ldebegstamp = YEAR(cdate2)  * 10000 +
              MONTH(cdate2) * 100   +
              DAY(cdate2) .

fEmptyRerateTempTables().

FOR EACH MsOwner NO-LOCK WHERE
         MsOwner.CustNum      = iiCustNum AND
         MsOwner.TSEnd   >= ldestamp      AND 
         Msowner.TSBegin <= ldebegstamp AND
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

fInitializeRerateLog(iiCustNum,
                     0,
                     "",
                     lcRerateSource,
                     cdate1,
                     cdate2).
 
fFillTT().

ETIME(YES).

/* man_rate2.i needs main-loop and outer-loop (do) for mobcdr */
Main:
REPEAT:

   DO:
   
      MobCDR: 
      FOR EACH MobCDR  NO-LOCK WHERE   
               MobCDR.CustNum  = iiCustnum  AND         
               MobCDR.datest  >= cdate1 AND
               MobCDR.datest  <= cdate2    
      TRANSACTION WITH FRAME MobCDR: 
       
         {Rate/man_rate2.i}

FINALLY:
   IF VALID-OBJECT(objDynQueryMServiceLimit)
   THEN DELETE OBJECT objDynQueryMServiceLimit.
END FINALLY.
