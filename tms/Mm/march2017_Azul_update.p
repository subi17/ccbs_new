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

{tmsconst.i}
{commpaa.i}
{timestamp.i}
{upsellbundle.i}

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

/*gcBrand = "1".*/

DEF VAR lcTableName AS CHAR NO-UNDO.
DEF VAR lcActionId AS CHAR NO-UNDO.
DEF VAR ldCampaignStart AS DEC NO-UNDO.
DEF VAR ldCampaignEnd AS DEC NO-UNDO.
DEF VAR ldCurrentTimeTS AS DEC  NO-UNDO.
DEF VAR ldCollPeriodStartTS AS DEC NO-UNDO.
DEF VAR ldCollPeriodEndTS AS DEC NO-UNDO. /*now - 1 minute*/
DEF VAR lcResult AS CHAR NO-UNDO.
DEF VAR lcUpsell AS CHAR NO-UNDO.
DEF VAR lcLogDir AS CHAR NO-UNDO.

DEF STREAM sLogFile.

lcLogDir     = fCParam("March2017Promo","March2017LogDir").
lcTableName = "March2017Promo". /*For execution lock*/
lcActionId = "UpsellForAzul". /*For execution lock*/
ldCampaignStart = 20170301. /*Dates when order must be done */
ldCampaignEnd = 20170401. /*Dates when order must be done */
lcUpsell = "FLEX_UPSELL". /*Upsell that will be aded in the promo*/
ldCurrentTimeTS = fMakeTS().

PUT STREAM sLogFile UNFORMATTED "Azul Upsell Activation starts " +  
                                 fTS2HMS(ldCurrentTimeTS) SKIP.

/*Temp table for orders of activated mobsubs during the collection period*/
DEF TEMP-TABLE ttOrderList NO-UNDO
   FIELD OrderID   AS INT
   FIELD MsSeq     AS INT.

FUNCTION fIsAzul RETURNS LOG
   (icCliType AS CHAR):
   IF icCliType EQ "CONTDSL59" OR
      icCliType EQ "CONTFH59_50" OR
      icCliType EQ "CONTFH69_300" THEN
      RETURN TRUE.
 
   RETURN FALSE.
END.   

FUNCTION fCollect RETURNS CHAR
   (idStartTS AS DEC,
    idEndTS AS DEC):
   DEF BUFFER Order FOR Order.
   DEF BUFFER ORdertimestamp FOR Ordertimestamp.
   FOR EACH Ordertimestamp NO-LOCK WHERE
            Ordertimestamp.Brand EQ gcBrand AND
            OrderTimestamp.RowType EQ {&ORDERTIMESTAMP_DELIVERY} AND
            Ordertimestamp.TimeStamp < idEndTS AND
            Ordertimestamp.TimeStamp >= idStartTS:
      
      FIND FIRST ttOrderList WHERE
                 ttOrderList.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
      IF AVAIL ttOrderList THEN NEXT. /*Skip duplicates*/
      
      FIND FIRST Order WHERE
                 Order.Brand EQ gcBrand AND
                 Order.OrderID EQ OrderTimestamp.OrderId NO-ERROR.
      IF NOT AVAIL Order THEN NEXT. /*This should not happen*/

      /*Order must be for Azul*/
      IF NOT fIsAzul(Order.CliType) THEN NEXT.

      /*Order must be created in March*/
      IF Order.Brand EQ gcBrand AND 
         NOT(Order.CrStamp GE ldCampaignStart AND
             Order.CrStamp LT ldCampaignEnd) THEN NEXT.

      /*No Flex upsell is allowed*/
         /*This is checked in activation phase*/

      /*Do not make the activation for STC*/
      IF Order.OrderType EQ {&ORDER_TYPE_STC} THEN NEXT.

      /*Do not make the activation for reactivation orders. TODO Prio2*/


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

   fCreateUpsellBundle(iiMsSeq,
                        lcUpsell,
                        {&REQUEST_SOURCE_YOIGO_TOOL},
                        fMakeTS(),
                        OUTPUT liRequest,
                        OUTPUT lcError).
   IF lcError NE "" THEN RETURN lcError.                    

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
         ActionLog.ActionTS     = 20170301.
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
   lcResult = fUpsellForAzul(ttOrderList.MsSeq).
   PUT STREAM sLogFile STRING(ttOrderList.OrderID) + ";" 
                       STRING(ttOrderList.MsSeq) + ";" +
                       lcResult SKIP.
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

