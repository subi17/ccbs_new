/*--------------------------------------------------------------------
  MODULE .......: march2017_Azul_update.p
  TASK .........: This is temporary program that is run hourly 
                  1.3.2017 - 15.4.2017 for Activating 25MB upsell for
                  Azul subscriptions done in March. (March 2017 Promo)
                  25MB Upsell == Flecx Upsell.
  APPLICATION ..: tms
  AUTHOR .......: Ilkka Savolainen
  CREATED ......: 27.2.2017
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/tmsconst.i}
{Syst/commpaa.i}
{Func/timestamp.i}
{Func/upsellbundle.i}

/*Logic:*
   1. Make a lock that prevents multiple instances of the program. 
   2. Collect all orders that have been activated during the coll.period. with
      criteria:
      -Order Date is in March
      -Clitype is Azul
      -No FLEX_UPSELL activated
      
   3. Activate upsell for the mentioned subscription & write log.
   4. Free lock taken in step 1.
*/

gcBrand = "1".

DEF VAR lcTableName           AS CHAR NO-UNDO.
DEF VAR lcActionId            AS CHAR NO-UNDO.
DEF VAR ldCampaignStartApril  AS DEC  NO-UNDO.
DEF VAR ldCampaignStart       AS DEC  NO-UNDO.
DEF VAR ldCampaignEnd         AS DEC  NO-UNDO.
DEF VAR ldCurrentTimeTS       AS DEC  NO-UNDO.
DEF VAR ldCollPeriodStartTS   AS DEC  NO-UNDO.
DEF VAR ldCollPeriodEndTS     AS DEC  NO-UNDO. /*now - 1 minute*/
DEF VAR lcResult              AS CHAR NO-UNDO.
DEF VAR lcUpsell              AS CHAR NO-UNDO.
DEF VAR lcLogDir              AS CHAR NO-UNDO.
DEF VAR lcLogFile             AS CHAR NO-UNDO.
DEF VAR ldaReadDate           AS DATE NO-UNDO.
DEF VAR llgSimulate           AS LOG  NO-UNDO.
DEF VAR lcOutROw              AS CHAR NO-UNDO.

DEF STREAM sLogFile.

ASSIGN
   llgSimulate       = FALSE              /*TRUE-> only log writing, FALSE->make real updates*/
   lcTableName       = "March2017Promo"   /*For execution lock*/
   lcActionId        = "UpsellForAzul"    /*For execution lock*/
   ldCampaignStartApril   = fCParamDe("March2017AprilFromDate")
   ldCampaignStart   = fCParamDe("March2017PromoFromDate") /*Dates when order must be done */
   ldCampaignEnd     = fCParamDe("March2017PromoToDate")   /*Dates when order must be done */
   lcUpsell          = "FLEX_UPSELL"      /*Upsell that will be aded in the promo*/
   ldaReadDate       = TODAY
   ldCurrentTimeTS   = fMakeTS()
   lcLogDir          = fCParam("March2017Promo","March2017LogDir").

IF lcLogDir EQ "" OR lcLogDir EQ ? THEN lcLogDir = "/tmp/".

lcLogFile = lcLogDir + "March2017Promo_hourly_" +
            STRING(YEAR(ldaReadDate)) +
            STRING(MONTH(ldaReadDate),"99") +
            STRING(DAY(ldaReadDate),"99") +
            REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".

OUTPUT STREAM sLogFile TO VALUE(lcLogFile) APPEND.

PUT STREAM sLogFile UNFORMATTED "Azul Upsell Activation starts " +
                                 fTS2HMS(ldCurrentTimeTS) SKIP.
IF llgSimulate EQ TRUE THEN
   PUT STREAM sLogFile UNFORMATTED "Simulation mode" SKIP.

/*Temp table for orders of activated mobsubs during the collection period*/
DEF TEMP-TABLE ttOrderList NO-UNDO
   FIELD OrderID   AS INT
   FIELD MsSeq     AS INT.

FUNCTION fIsAzul RETURNS LOG
   (icCliType AS CHAR):
   IF icCliType EQ "CONTDSL59" OR
      icCliType EQ "CONTFH59_50" OR
      icCliType EQ "CONTFH69_300" OR
      icCliType EQ "CONT25" THEN
      RETURN TRUE.
 
   RETURN FALSE.
END.   

FUNCTION fCollect RETURNS CHAR
   (idStartTS AS DEC,
    idEndTS AS DEC):

   DEF BUFFER Order FOR Order.
   DEF BUFFER Mobsub FOR Mobsub.
   DEF BUFFER OrderTimestamp FOR OrderTimestamp.

   DEF VAR lcErr AS CHAR NO-UNDO.
   
   FOR EACH OrderTimestamp NO-LOCK WHERE
            OrderTimestamp.Brand EQ gcBrand AND
            OrderTimestamp.RowType EQ {&ORDERTIMESTAMP_DELIVERY} AND
            OrderTimestamp.TimeStamp < idEndTS AND
            OrderTimestamp.TimeStamp >= idStartTS:

      lcErr = "".

      FIND FIRST ttOrderList WHERE
                 ttOrderList.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
      IF AVAIL ttOrderList THEN NEXT. /*Skip duplicates*/
      
      FIND FIRST Order NO-LOCK WHERE
                 Order.Brand EQ gcBrand AND
                 Order.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
      IF NOT AVAIL Order THEN NEXT. /*This should not happen*/

      /*Order must be for Azul or CONT25 (april promo) */
      IF NOT fIsAzul(Order.CliType) THEN NEXT.

      /* April promotion */
      IF Order.CliType = "CONT25" AND
         Order.OrderType = {&ORDER_TYPE_MNP} THEN DO:
         IF NOT (Order.CrStamp >= ldCampaignStartApril AND /* 20170403 */
                 Order.CrStamp < ldCampaignEnd) THEN NEXT.
      END.
      ELSE DO:
         /*Order must be created in March*/
         IF NOT(Order.CrStamp GE ldCampaignStart AND
                Order.CrStamp LT ldCampaignEnd) THEN NEXT.
      END.
      /*No Flex upsell is allowed*/
         /*This is checked in activation phase in fUpsellForAzul*/

      IF Order.OrderType NE {&ORDER_TYPE_NEW} AND
         Order.OrderType NE {&ORDER_TYPE_MNP} AND
         Order.OrderType NE {&ORDER_TYPE_STC} THEN NEXT.

      FIND FIRST Mobsub NO-LOCK WHERE
                 Mobsub.MsSeq EQ Order.MsSeq.
      IF NOT AVAIL Mobsub THEN DO:
         lcErr = "No active mobsub " + STRING(Order.MsSeq).
         PUT STREAM sLogFile UNFORMATTED lcErr SKIP.
         NEXT.
       END.  
      IF Mobsub.CliType NE Order.Clitype THEN DO:
         lcErr = "Clitypes do not match " + 
                 STRING(Order.Orderid) + "|" +
                 STRING(Order.clitype) + "|" +
                 STRING(Mobsub.clitype).
         PUT STREAM sLogFile UNFORMATTED lcErr SKIP.
         NEXT.
      END.

      /*After all validations: this order needs upsell*/
      CREATE ttOrderList.
      ASSIGN
         ttOrderList.OrderID = Order.Orderid
         ttOrderList.MsSeq = Order.MsSeq.

   END.
END.

/*Function activates upsell for subscription that is related to given order*/
FUNCTION fUpsellForAzul RETURNS CHAR
   (iiMsSeq AS INT):
   DEF VAR lcError AS CHAR NO-UNDO.
   DEF VAR liRequest AS INT NO-UNDO.
   DEF BUFFER MsRequest FOR MsRequest.

   FIND FIRST MsRequest NO-LOCK where
              MsRequest.MsSeq EQ iiMsSeq AND
              MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND
              MsRequest.ReqCparam3 EQ lcUpsell NO-ERROR.
   IF AVAIL MsRequest THEN RETURN "Upsell already activated".
   
   IF llgSimulate EQ FALSE THEN DO:
      fCreateUpsellBundle(iiMsSeq,
                           lcUpsell,
                           {&REQUEST_SOURCE_YOIGO_TOOL},
                           fMakeTS(),
                           OUTPUT liRequest,
                           OUTPUT lcError).
      IF lcError NE "" THEN RETURN lcError.                    
   END.
   RETURN "OK".
END.
/*Set execution lock on.*/
DO TRANS:

   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      QUIT.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCampaignStart.
      RELEASE ActionLog.
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      ASSIGN
         /*store previous starting time before setting new value to db*/
         ldCollPeriodStartTS = ActionLog.ActionTS

         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun
         ActionLog.ActionTS     = ldCurrentTimeTS.

      RELEASE Actionlog.
   END.
END.

/*Actual execution:*/
ldCollPeriodEndTS = fSecOffSet(ldCurrentTimeTS, -60). /*Now - 1 minute*/
fCollect(ldCollPeriodStartTS, ldCollPeriodEndTS). /*Select orders that need activation*/

/*Activate upsells*/
FOR EACH ttOrderList:
   lcOutRow = "".
   lcResult = fUpsellForAzul(ttOrderList.MsSeq).
   lcOutRow = STRING(ttOrderList.OrderID) + "|" +
              STRING(ttOrderList.MsSeq) + "|" +
              lcResult.
   PUT STREAM sLogFile UNFORMATTED lcoutRow SKIP.
END.


/*Release execution lock*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.

   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionTS = ldCollPeriodEndTS. /*Start next collection from here*/
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.
   RELEASE ActionLog.
END.

PUT STREAM sLogFile UNFORMATTED "Azul Upsell Activation done " +
                                 fTS2HMS(fMakeTS()) SKIP.
OUTPUT STREAM sLogFile CLOSE.

