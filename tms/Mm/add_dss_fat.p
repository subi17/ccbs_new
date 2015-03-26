/* ----------------------------------------------------------------------
  MODULE .......: add_dss_fat.p
  TASK .........: Create DSS promotion if customer has higher price of bundle
                  during promotion period
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 20.04.12
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commpaa.i}
gcBrand = "1".
katun   = "CRON".
{cparam2.i}
{timestamp.i}
{fdss.i}
{fcpfat.i}

DEF VAR lcPromotionPath          AS CHAR NO-UNDO.
DEF VAR ldaPromoFromDate         AS DATE NO-UNDO.
DEF VAR ldaPromoToDate           AS DATE NO-UNDO.
DEF VAR ldaFromDate              AS DATE NO-UNDO.
DEF VAR ldaToDate                AS DATE NO-UNDO.
DEF VAR ldPeriodFrom             AS DEC  NO-UNDO.
DEF VAR ldPeriodTo               AS DEC  NO-UNDO.
DEF VAR ldNextMonthStamp         AS DEC  NO-UNDO.
DEF VAR ldeStamp                 AS DEC  NO-UNDO.
DEF VAR liPeriod                 AS INT  NO-UNDO.
DEF VAR lcLogFile                AS CHAR NO-UNDO.
DEF VAR lcDelim                  AS CHAR NO-UNDO INIT "|".
DEF VAR lcError                  AS CHAR NO-UNDO.

DEF STREAM sout.

DEF TEMP-TABLE ttDSSFat NO-UNDO
   FIELD DSSMsSeq         AS INT
   FIELD CustNum          AS INT
   FIELD BundleId         AS CHAR
   FIELD BundleMsSeq      AS INT
   FIELD BundleCLI        AS CHAR
   FIELD BundleFromTS     AS DEC
   FIELD BundleEndTS      AS DEC
   INDEX CustNum CustNum.

/******** Main start ********/

ASSIGN
   ldaPromoFromDate = fCParamDa("DSSPromoFromDate")
   ldaPromoToDate   = fCParamDa("DSSPromoEndDate")
   lcPromotionPath  = fCParamC("DSSPromoFilePath")
   ldaFromDate      = ldaPromoFromDate.

IF DAY(TODAY) = 1 THEN
   ldaToDate = fLastDayOfMOnth(TODAY - 1).
ELSE
   ldaToDate = fLastDayOfMOnth(TODAY).

ASSIGN
   ldeStamp         = fMakeTS()
   ldPeriodFrom     = fMake2Dt(ldaFromDate,0)
   ldPeriodTo       = fMake2Dt(ldaToDate,86399)
   ldNextMonthStamp = fMake2Dt((ldaToDate + 1),0)
   liPeriod         = YEAR(ldaToDate) * 100 + MONTH(ldaToDate)
   lcLogFile        = lcPromotionPath + "/add_dss_fat_" + STRING(liPeriod) +
                      "_" + STRING(ldeStamp) + ".log".

OUTPUT STREAM sout TO VALUE(lcLogFile).

IF ldaToDate > ldaPromoToDate THEN DO:
   PUT STREAM sout UNFORMATTED "ERROR:Promotion period has been expired. " +
                               "FAT will not be created." SKIP.
   OUTPUT STREAM sout CLOSE.
   RETURN.
END. /* IF ldaToDate > ldaPromoToDate THEN DO: */

RUN pGetAllCustomersSubscriptions.

PUT STREAM sout UNFORMATTED
    "CustNum"         lcDelim
    "DSS Subs Id"     lcDelim
    "Bundle Id"       lcDelim 
    "Bundle Subs Id"  lcDelim
    "Bundle CLI"      lcDelim
    "Bundle ActStamp" lcDelim
    "Bundle EndStamp" lcDelim
    "Remark" SKIP.

/* Creating DSS FAT */
FOR EACH ttDSSFat NO-LOCK:

   lcError = "".

   IF fFatExists("DSSCPFREE",
                 ttDSSFat.DSSMsSeq,
                 ttDSSFat.Custnum,
                 liPeriod) THEN DO:
      PUT STREAM sout UNFORMATTED
          STRING(ttDSSFat.CustNum) lcDelim
          STRING(ttDSSFat.DSSMsSeq) lcDelim
          STRING(ttDSSFat.BundleId) lcDelim
          STRING(ttDSSFat.BundleMsSeq) lcDelim
          STRING(ttDSSFat.BundleCLI) lcDelim
          STRING(ttDSSFat.BundleFromTS) lcDelim
          STRING(ttDSSFat.BundleEndTS) lcDelim
          "FAT already exists." SKIP.
      NEXT.
   END. /* IF fFatExists */

   /* Create FAT */
   RUN creafat.p(ttDSSFat.CustNum,
                 ttDSSFat.DSSMsSeq,
                 "DSSCPFREE",
                 ?, /* amount */
                 0, /* percent */
                 ?, /* vat incl. */
                 liPeriod,
                 999999,
                 OUTPUT lcError).
   IF lcError > "" THEN lcError = "ERROR: " + lcError.
   ELSE lcError = "Successfully added.".

   PUT STREAM sout UNFORMATTED
       STRING(ttDSSFat.CustNum) lcDelim
       STRING(ttDSSFat.DSSMsSeq) lcDelim
       STRING(ttDSSFat.BundleId) lcDelim
       STRING(ttDSSFat.BundleMsSeq) lcDelim
       STRING(ttDSSFat.BundleCLI) lcDelim
       STRING(ttDSSFat.BundleFromTS) lcDelim
       STRING(ttDSSFat.BundleEndTS) lcDelim
       lcError SKIP.

END. /* FOR EACH ttDSSFat NO-LOCK: */

OUTPUT STREAM sout CLOSE.

/******** Main end ********/

PROCEDURE pGetAllCustomersSubscriptions:

   FOR EACH Customer NO-LOCK:

      RUN pGetCustomerSubscriptions(INPUT Customer.Custnum).

   END. /* FOR EACH Customer NO-LOCK: */

END PROCEDURE.


PROCEDURE pGetCustomerSubscriptions:

   DEF INPUT PARAMETER iiInvCust    AS INT  NO-UNDO.

   DEF VAR ldeDSSLimit              AS DEC  NO-UNDO.
   DEF VAR llFatAllowed             AS LOG  NO-UNDO.
   DEF VAR lcBundleId               AS CHAR NO-UNDO.
   DEF VAR liDSSMsSeq               AS INT  NO-UNDO.

   DEF BUFFER bMServiceLimit        FOR MServiceLimit.
   DEF BUFFER bServiceLimit         FOR ServiceLimit.
   DEF BUFFER bDayCampaign          FOR DayCampaign.
   DEF BUFFER bMSRequest            FOR MsRequest.

   /* Check wheather customer is linked with DSS service or not */
   IF NOT fGetDSSMsSeqLimit(INPUT  iiInvCust,
                            INPUT  ldPeriodTo,
                            OUTPUT liDSSMsSeq,
                            OUTPUT ldeDSSLimit,
                            OUTPUT lcBundleId) OR
      lcBundleId = "DSS2" THEN RETURN.

   EACH_MsOwner:
   FOR EACH MsOwner NO-LOCK WHERE
            MsOwner.InvCust  = iiInvCust  AND
            MsOwner.TsEnd   >= ldPeriodFrom
   BREAK BY MsOwner.MsSeq:
      IF FIRST-OF(MsOwner.MsSeq) THEN DO:

         ASSIGN llFatAllowed = False
                lcBundleId   = "".

         STATUS DEFAULT MsOwner.CLI.

         DATA_BUNDLE_LOOP:
         FOR EACH bMServiceLimit WHERE
                  bMServiceLimit.MsSeq    = MsOwner.MsSeq  AND
                  bMServiceLimit.DialType = {&DIAL_TYPE_GPRS} AND
                  bMServiceLimit.FromTS  >= ldPeriodFrom     AND
                  bMServiceLimit.FromTS  <= ldPeriodTo       AND
                  bMServiceLimit.EndTS   >= ldPeriodTo NO-LOCK,
             FIRST bServiceLimit NO-LOCK USE-INDEX SlSeq WHERE
                   bServiceLimit.SLSeq = bMServiceLimit.SLSeq,
             FIRST bDayCampaign NO-LOCK WHERE
                   bDayCampaign.Brand = gcBrand AND
                   bDayCampaign.DCEvent = bServiceLimit.GroupCode AND
                   LOOKUP(bDayCampaign.DCType,
                          {&PERCONTRACT_RATING_PACKAGE}) > 0:

             IF LOOKUP(bDayCampaign.DCEvent,
                "MDUB2,MDUB3,MDUB4,CONTDATA,CONTD2,CONTD4") = 0 THEN
                NEXT DATA_BUNDLE_LOOP.

             Create ttDSSFat.
             ASSIGN ttDSSFat.DSSMsSeq      = liDSSMsSeq
                    ttDSSFat.CustNum       = MsOwner.CustNum
                    ttDSSFat.BundleId      = bServiceLimit.GroupCode
                    ttDSSFat.BundleMsSeq   = bMServiceLimit.MsSeq
                    ttDSSFat.BundleCLI     = MsOwner.CLI
                    ttDSSFat.BundleFromTS  = bMServiceLimit.FromTS
                    ttDSSFat.BundleEndTS   = bMServiceLimit.EndTS.

             LEAVE EACH_MsOwner.
         END. /* FOR EACH bMServiceLimit WHERE */

         /* Check STC Request with higher price bundle during promotion period */
         FIND FIRST bMSRequest NO-LOCK WHERE
                    bMSRequest.MsSeq   = MsOwner.MsSeq AND
                    bMSRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                    LOOKUP(STRING(bMSRequest.ReqStat),"2,4,9,99,3") = 0 AND
                    bMSRequest.ActStamp = ldNextMonthStamp USE-INDEX MsSeq NO-ERROR.
         IF AVAIL bMSRequest AND
            LOOKUP(bMSRequest.ReqCparam5,
                   "MDUB2,MDUB3,MDUB4,CONTDATA,CONTD2,CONTD4") > 0
         THEN ASSIGN llFatAllowed = True
                     lcBundleId   = bMSRequest.ReqCparam5.

         /* Check BTC Request with higher price bundle during promotion period */
         IF NOT llFatAllowed THEN DO:
            FIND FIRST bMSRequest NO-LOCK WHERE
                       bMSRequest.MsSeq   = MsOwner.MsSeq AND
                       bMSRequest.ReqType = {&REQTYPE_BUNDLE_CHANGE} AND
                       LOOKUP(STRING(bMSRequest.ReqStat),"2,4,9,99,3") = 0 AND
                       bMSRequest.ActStamp = ldNextMonthStamp
                 USE-INDEX MsSeq NO-ERROR.
            IF AVAIL bMSRequest AND
               LOOKUP(bMsRequest.ReqCparam2,
                      "MDUB2,MDUB3,MDUB4,CONTDATA,CONTD2,CONTD4") > 0
            THEN ASSIGN llFatAllowed = True
                        lcBundleId   = bMsRequest.ReqCparam2.
         END. /* IF NOT llFatAllowed THEN DO: */

         IF llFatAllowed THEN DO:
            Create ttDSSFat.
            ASSIGN ttDSSFat.DSSMsSeq      = liDSSMsSeq
                   ttDSSFat.CustNum       = MsOwner.CustNum
                   ttDSSFat.BundleId      = lcBundleId
                   ttDSSFat.BundleMsSeq   = bMsRequest.MsSeq
                   ttDSSFat.BundleCLI     = MsOwner.CLI
                   ttDSSFat.BundleFromTS  = bMSRequest.ActStamp
                   ttDSSFat.BundleEndTS   = 99999999.99999.
            LEAVE EACH_MsOwner.
         END. /* IF llFatAllowed THEN DO: */

      END. /* IF FIRST-OF(MsOwner.MsSeq) THEN DO: */
   END. /* FOR EACH MsOwner NO-LOCK WHERE */

END PROCEDURE.

