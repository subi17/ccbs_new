/* ----------------------------------------------------------------------
  MODULE .......: tarj7_renewal_alignment.p
  TASK .........: Aligns TARJ7 renewal date in AIR. YPR-1080
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 16.04.14
  Version ......: Yoigo
----------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}
{Syst/dumpfile_run.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR ldaFromdate           AS DATE NO-UNDO.
DEF VAR liTime                AS INT  NO-UNDO.
DEF VAR lcError               AS CHAR NO-UNDO.
DEF VAR ldaExpDate            AS DATE NO-UNDO.
DEF VAR ldaLastDay            AS DATE NO-UNDO.
DEF VAR liCurrentServiceClass AS INT  NO-UNDO.
DEF VAR liErrors              AS INT  NO-UNDO. 
DEF VAR liRetry               AS INT  NO-UNDO. 
DEF VAR liCount               AS INT  NO-UNDO.
DEF VAR lcGroupCodes          AS CHAR NO-UNDO.
DEF VAR liTempSC              AS INT NO-UNDO. 
DEF VAR liNormalSC            AS INT NO-UNDO. 

DEF STREAM sout.

ASSIGN ldaLastDay   = Func.Common:mLastDayOfMonth(today)
       lcGroupCodes = "TARJ7,TARJ9,TARJ10,TARJ11,TARJ12".

OUTPUT STREAM sout TO VALUE(icFile) APPEND.

PUT STREAM sout UNFORMATTED
   "CUSTNUM;MSSEQ;MSISDN;ORIG.DATE;NEW_RENEWAL_DATE;CURRENT_SC;ERROR" SKIP.

DO liCount = 1 TO NUM-ENTRIES(lcGroupCodes):
   FOR FIRST ServiceLimit NO-LOCK WHERE
             ServiceLimit.GroupCode = ENTRY(liCount,lcGroupCodes,","),
        EACH MServiceLimit NO-LOCK WHERE
             MServiceLimit.SlSeq    = ServiceLimit.SlSeq AND
             MServiceLimit.DialType = ServiceLimit.DialType AND
             MServiceLimit.EndTS    = 99999999.99999:

      Func.Common:mSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).

      IF ldaFromdate >= TODAY THEN NEXT.

      IF DAY(ldaFromDate) <= 28 THEN NEXT.

      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.MsSeq = MServiceLimit.MsSeq NO-ERROR.
      IF NOT AVAIL MobSub THEN NEXT.
      
      CASE Mobsub.CLIType:
         WHEN "TARJ7" THEN liTempSC = 303.
         WHEN "TARJ9" THEN liTempSC = 309.
         WHEN "TARJ10" THEN liTempSC = 310.
         WHEN "TARJ11" THEN liTempSC = 311.
         WHEN "TARJ12" THEN liTempSC = 312.
         OTHERWISE NEXT.
      END.

      IF DAY(ldaFromDate) < DAY(ldaLastDay)
      THEN ldaExpDate = DATE(MONTH(ldaLastDay),DAY(ldaFromDate),YEAR(ldaLastDay)).
      ELSE ldaExpDate = ldaLastDay.

      ASSIGN
         ldaExpDate = ldaExpDate - 1
         oiEvents = oiEvents + 1
         liNormalSC = liTempSC - 300.

      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents LABEL "Renewal Alignments" 
         WITH OVERLAY ROW 10 CENTERED SIDE-LABELS
            TITLE " Collecting " FRAME fQty.
      END.

      DO liRetry = 1 TO 3.
      
         RUN Gwy/air_get_account_details.p(Mobsub.CLI,
                                       OUTPUT liCurrentServiceClass,
                                       OUTPUT lcError).

         IF INDEX(lcError,"ERR:Unable to Connect") = 0 AND
            INDEX(lcError,"ERR:Took too long to Connect") = 0 AND
            INDEX(lcError,"ERR:No response") = 0 AND
            INDEX(lcError,"ERROR:AIR GetAccountDetails responseCode: 100") = 0 AND
            INDEX(lcError,"ERROR:AIR GetAccountDetails response:") = 0 THEN LEAVE.
         ELSE IF liRetry < 3 THEN PAUSE 5 NO-MESSAGE.
      END.

      if lcError > "" THEN DO:
         PUT STREAM sout UNFORMATTED
            MobSub.custnum ";"
            MobSub.MsSeq ";"
            Mobsub.cli ";"
            Mobsub.CliType ";"
            ldaFromDate ";"
            ldaExpDate ";"
            liCurrentServiceClass ";"
            lcError skip.
         liErrors = liErrors + 1.
         NEXT.
      END.

      IF liCurrentServiceClass EQ liTempSC THEN
         lcError = SUBST("SKIPPED:Current SC in AIR is already &1",liTempSC).
      ELSE IF liCurrentServiceClass NE liNormalSC THEN ASSIGN
         lcError = SUBST("SKIPPED:Current SC &1 in AIR is not &2",
                          liCurrentServiceClass,
                          liNormalSC).

      IF lcError > "" THEN DO:

         PUT STREAM sout UNFORMATTED
            MobSub.custnum ";"
            MobSub.MsSeq ";"
            Mobsub.cli ";"
            Mobsub.CliType ";"
            ldaFromDate ";"
            ldaExpDate ";"
            liCurrentServiceClass ";"
            lcError
            SKIP.
         NEXT.
      END.

      DO liRetry = 1 TO 3.
         RUN Gwy/air_set_temp_sc.p(MobSub.CLI,
                               liTempSC,
                               ldaExpDate,
                               OUTPUT lcerror).
         IF INDEX(lcError,"ERR:Unable to Connect") = 0 AND
            INDEX(lcError,"ERR:Took too long to Connect") = 0 AND
            INDEX(lcError,"ERR:No response") = 0 AND
            INDEX(lcError,"ERROR:AIR UpdateServiceClass responseCode: 100") = 0 AND
            INDEX(lcError,"ERROR:AIR UpdateServiceClass response:") = 0 THEN LEAVE.
         ELSE IF liRetry < 3 THEN PAUSE 5 NO-MESSAGE.
      END.

      PUT STREAM sout UNFORMATTED
         MobSub.custnum ";"
         MobSub.MsSeq ";"
         Mobsub.cli ";"
         Mobsub.CliType ";"
         ldaFromDate ";"
         ldaExpDate  ";"
         liCurrentServiceClass ";"
         lcError SKIP.
         
      IF lcError > "" AND
         lcError NE "ERROR:AIR UpdateServiceClass responseCode: 104"
         THEN liErrors = liErrors + 1.

   END.
END. /* DO liCount = 1 TO NUM-ENTRIES(lcGroupCodes): */

OUTPUT STREAM sout CLOSE.

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
         
/* This is to raise automatic monitoring alarm */
IF liErrors > 0 THEN ASSIGN
   oiEvents = {&DUMPLOG_ERROR_NOTIFICATION}
   olInterrupted = TRUE.
