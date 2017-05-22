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
{Func/date.i}
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

DEF STREAM sout.

ASSIGN ldaLastDay   = fLastdayofMonth(today)
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

      fSplitTS(MServiceLimit.FromTS,OUTPUT ldaFromdate,OUTPUT liTime).

      IF ldaFromdate >= TODAY THEN NEXT.

      IF DAY(ldaFromDate) <= 28 THEN NEXT.

      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.MsSeq = MServiceLimit.MsSeq NO-ERROR.
      IF NOT AVAIL MobSub THEN NEXT.

      IF DAY(ldaFromDate) < DAY(ldaLastDay)
      THEN ldaExpDate = DATE(MONTH(ldaLastDay),DAY(ldaFromDate),YEAR(ldaLastDay)).
      ELSE ldaExpDate = ldaLastDay.

      ASSIGN
         ldaExpDate = ldaExpDate - 1
         oiEvents = oiEvents + 1.

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
            INDEX(lcError,"ERR:No response") = 0 THEN LEAVE.
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

      IF (Mobsub.CliType = "TARJ7"  AND liCurrentServiceClass EQ 303) OR
         (Mobsub.CliType = "TARJ9"  AND liCurrentServiceClass EQ 309) OR
         (Mobsub.CliType = "TARJ10" AND liCurrentServiceClass EQ 310) OR
         (Mobsub.CliType = "TARJ11" AND liCurrentServiceClass EQ 311) OR
         (Mobsub.CliType = "TARJ12" AND liCurrentServiceClass EQ 312) THEN DO:
         PUT STREAM sout UNFORMATTED
            MobSub.custnum ";"
            MobSub.MsSeq ";"
            Mobsub.cli ";"
            Mobsub.CliType ";"
            ldaFromDate ";"
            ldaExpDate ";"
            liCurrentServiceClass ";"
            IF Mobsub.CliType = "TARJ7"
               THEN "SKIPPED:Current SC in AIR is already 303"
               ELSE "SKIPPED:Current SC in AIR is already 309"
            SKIP.
         NEXT.
      END.

      IF (Mobsub.CliType = "TARJ7"  AND liCurrentServiceClass NE 3)  OR
         (Mobsub.CliType = "TARJ9"  AND liCurrentServiceClass NE 9)  OR
         (Mobsub.CliType = "TARJ10" AND liCurrentServiceClass NE 10) OR
         (Mobsub.CliType = "TARJ11" AND liCurrentServiceClass NE 11) OR
         (Mobsub.CliType = "TARJ12" AND liCurrentServiceClass NE 12) THEN DO:
         PUT STREAM sout UNFORMATTED
            MobSub.custnum ";"
            MobSub.MsSeq ";"
            Mobsub.cli ";"
            Mobsub.CliType ";"
            ldaFromDate ";"
            ldaExpDate ";"
            liCurrentServiceClass ";"
            IF Mobsub.CliType = "TARJ7" 
               THEN "ERROR:Current SC in AIR is not 3"
               ELSE "ERROR:Current SC in AIR is not 9"
            SKIP.
         liErrors = liErrors + 1.
         NEXT.
      END.

      DO liRetry = 1 TO 3.
         RUN Gwy/air_set_temp_sc.p(MobSub.CLI,
                               IF Mobsub.CliType = "TARJ7" THEN 303
                                                           ELSE 309, /* SC temp */
                               ldaExpDate,
                               OUTPUT lcerror).
         IF INDEX(lcError,"ERR:Unable to Connect") = 0 AND
            INDEX(lcError,"ERR:Took too long to Connect") = 0 AND
            INDEX(lcError,"ERR:No response") = 0 THEN LEAVE.
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
