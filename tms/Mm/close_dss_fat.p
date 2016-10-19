/* ----------------------------------------------------------------------
  MODULE .......: close_dss_fat.p
  TASK .........: CLOSE DSS promotion if criteria does not match
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 14.05.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
katun   = "CRON".
{cparam2.i}
{timestamp.i}
{fcpfat.i}
{Func/date.i}
{Mm/active_bundle.i}

DEF VAR lcPromotionPath          AS CHAR NO-UNDO.
DEF VAR ldaPromoFromDate         AS DATE NO-UNDO.
DEF VAR ldaPromoToDate           AS DATE NO-UNDO.
DEF VAR ldPromoPeriodFrom        AS DEC  NO-UNDO.
DEF VAR ldPromoPeriodTo          AS DEC  NO-UNDO.
DEF VAR ldaFirstDayOfLastMonth   AS DATE NO-UNDO.
DEF VAR ldaLastDayOfLastMonth    AS DATE NO-UNDO.
DEF VAR ldPeriodFrom             AS DEC  NO-UNDO.
DEF VAR ldPeriodTo               AS DEC  NO-UNDO.
DEF VAR ldeStamp                 AS DEC  NO-UNDO.
DEF VAR liLastMonthPeriod        AS INT  NO-UNDO.
DEF VAR liCurrentMonthPeriod     AS INT  NO-UNDO.
DEF VAR ldeNextMonth             AS DEC  NO-UNDO.
DEF VAR liMonth                  AS INT  NO-UNDO.
DEF VAR lcLogFile                AS CHAR NO-UNDO.
DEF VAR lcDelim                  AS CHAR NO-UNDO INIT "|".
DEF VAR lcError                  AS CHAR NO-UNDO.
DEF VAR llKeepFAT                AS LOG  NO-UNDO.
DEF VAR ldeCurrentMonthStamp     AS DEC  NO-UNDO.

DEF BUFFER bMServiceLimit        FOR MServiceLimit.
DEF BUFFER bServiceLimit         FOR ServiceLimit.
DEF BUFFER bDayCampaign          FOR DayCampaign.

DEF STREAM sout.

DEF TEMP-TABLE ttDSSFat NO-UNDO
   FIELD DSSMsSeq         AS INT
   FIELD CustNum          AS INT
   FIELD CLI              AS CHAR
   FIELD Remark1          AS CHAR
   INDEX CustNum CustNum.

liMonth = MONTH(TODAY).
IF liMonth = 1 THEN
   ldaFirstDayOfLastMonth = DATE(12,1,YEAR(TODAY) - 1).
ELSE
   ldaFirstDayOfLastMonth = DATE((liMonth - 1),1,YEAR(TODAY)).

ASSIGN ldeStamp          = fMakeTS()
       ldeCurrentMonthStamp = fMake2Dt(DATE(MONTH(TODAY),1,YEAR(TODAY)),0)
       ldaPromoFromDate  = fCParamDa("DSSPromoFromDate")
       ldaPromoToDate    = fCParamDa("DSSPromoEndDate")
       lcPromotionPath   = fCParamC("DSSPromoFilePath")
       ldPromoPeriodFrom = fMake2Dt(ldaPromoFromDate,0)
       ldPromoPeriodTo   = fMake2Dt(ldaPromoToDate,86399)
       ldaLastDayOfLastMonth = fLastDayOfMonth(ldaFirstDayOfLastMonth)
       liCurrentMonthPeriod = YEAR(TODAY) * 100 + MONTH(TODAY)
       ldeNextMonth = fMake2Dt(fLastDayOfMonth(TODAY) + 1,0)
       liLastMonthPeriod = YEAR(ldaFirstDayOfLastMonth) * 100 +
                           MONTH(ldaFirstDayOfLastMonth)
       ldPeriodFrom = fMake2Dt(ldaFirstDayOfLastMonth,0)
       ldPeriodTo   = fMake2Dt(ldaLastDayOfLastMonth,86399)
       lcLogFile        = lcPromotionPath + "/close_dss_fat_" +
       STRING(liLastMonthPeriod) + "_" + STRING(ldeStamp) + ".log".

DEF BUFFER lbMobSub FOR MobSub.

FAT_LOOP:
FOR EACH FATime WHERE
         FATime.Brand  = gcBrand AND
         FATime.FTGrp  = "DSSCPFREE" AND
         FATime.InvNum = 0 AND
         FATime.LastPeriod > liLastMonthPeriod NO-LOCK,
   FIRST Customer WHERE
         Customer.CustNum = FATime.CustNum NO-LOCK:

   /* Check wheather subs. has active DSS service or not */
   IF fGetActiveSpecificBundle(FATime.MsSeq,ldeCurrentMonthStamp,"DSS") = ""
   THEN DO:
      Create ttDSSFat.
      ASSIGN ttDSSFat.DSSMsSeq = FATime.MsSeq
             ttDSSFat.CustNum  = FATime.CustNum
             ttDSSFat.CLI      = FATime.CLI
             ttDSSFat.Remark1  = "DSS is not active".
      NEXT.
   END. /* IF fGetActiveSpecificBundle(FATime.MsSeq,fMakeTS() */

   /* Check if any  multisim subscription pair is active */
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Custnum = Customer.Custnum AND
            MobSub.MultiSimID > 0,
      FIRST lbMobSub NO-LOCK WHERE
            lbMobSub.Brand = gcBrand AND
            lbMobSub.MultiSImID = Mobsub.MultiSImID AND
            lbMobSub.MultiSimType NE Mobsub.MultiSIMType:
      IF Mobsub.Custnum NE lbMobSub.Custnum THEN LEAVE.
      NEXT FAT_LOOP.
   END.
  
   llKeepFAT = FALSE.

   EACH_MsOwner:
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.InvCust  = Customer.CustNum AND
            MsOwner.TsEnd   >= ldPeriodFrom
   BREAK BY MsOwner.MsSeq:
      IF FIRST-OF(MsOwner.MsSeq) THEN DO:

         STATUS DEFAULT MsOwner.CLI.

         DATA_BUNDLE_LOOP:
         FOR EACH bMServiceLimit WHERE
                  bMServiceLimit.MsSeq    = MsOwner.MsSeq  AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS  >= ldPromoPeriodFrom AND
                  bMServiceLimit.FromTS  <= ldPromoPeriodTo   AND
                  bMServiceLimit.EndTS    > ldPeriodTo NO-LOCK,
             FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
                   bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
             FIRST bDayCampaign NO-LOCK WHERE
                   bDayCampaign.Brand = gcBrand AND
                   bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
                   LOOKUP(bDayCampaign.DCType,
                          {&PERCONTRACT_RATING_PACKAGE}) > 0:

             IF LOOKUP(bDayCampaign.DCEvent,
                "MDUB2,MDUB3,MDUB4,CONTDATA,CONTD2,CONTD4") > 0 THEN DO:
                llKeepFAT = TRUE.
                LEAVE EACH_MsOwner.
             END.
         END. /* FOR EACH bMServiceLimit WHERE */
      END. /* IF FIRST-OF(MsOwner.MsSeq) THEN DO: */
   END. /* FOR EACH MsOwner NO-LOCK WHERE */

   IF NOT llKeepFAT THEN DO:
      Create ttDSSFat.
      ASSIGN ttDSSFat.DSSMsSeq  = FATime.MsSeq
             ttDSSFat.CustNum   = FATime.CustNum
             ttDSSFat.CLI       = FATime.CLI
             ttDSSFat.Remark1   = "No active promotion bundle".
   END. /* IF NOT llKeepFAT THEN DO: */
END. /* FOR EACH FATime WHERE */

OUTPUT STREAM sout TO VALUE(lcLogFile).

PUT STREAM sout UNFORMATTED
    "CustNum"      lcDelim
    "Subs Id"      lcDelim
    "CLI"          lcDelim
    "Remark1"      lcDelim
    "Remark2"      SKIP.

/* Creating DSS FAT */
FOR EACH ttDSSFat NO-LOCK:

   fCloseFat("DSSCPFREE",
             ttDSSFat.DSSMsSeq,
             liLastMonthPeriod).

   PUT STREAM sout UNFORMATTED
       STRING(ttDSSFat.CustNum)  lcDelim
       STRING(ttDSSFat.DSSMsSeq) lcDelim
       ttDSSFat.CLI              lcDelim
       ttDSSFat.Remark1          lcDelim
       "Successfully closed." SKIP.

END. /* FOR EACH ttDSSFat NO-LOCK: */

OUTPUT STREAM sout CLOSE.
