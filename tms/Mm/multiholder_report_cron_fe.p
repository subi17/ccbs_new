/* ----------------------------------------------------------------------
  MODULE .......: multiholder_report_cron.p
  TASK .........: Temporary report for multiholder data
  APPLICATION ..: tms
  AUTHOR .......: ilkkasav
  CREATED ......: 25.07.17
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commpaa.i}
gcBrand = "1".
Katun = "Cron".
{Syst/tmsconst.i}
{Func/timestamp.i}
{Func/cparam2.i}
{Func/ftransdir.i}
{Func/dms.i}
{Func/email.i}

DEF STREAM sOutFile.


DEF VAR ldCollPeriodStartTS   AS DEC  NO-UNDO.
DEF VAR ldCollPeriodEndTS AS DEC  NO-UNDO.
DEF VAR ldCurrentTimeTS   AS DEC  NO-UNDO.
DEF VAR lcActionID        AS CHAR NO-UNDO.
DEF VAR lcTableName       AS CHAR NO-UNDO.
DEF VAR lcFile            AS CHAR NO-UNDO.
DEF VAR lcSpoolDir        AS CHAR NO-UNDO.
DEF VAR lcOutDir          AS CHAR NO-UNDO.
DEF VAR lcConfDir          AS CHAR NO-UNDO. /*Email conf*/
DEF VAR ldaReadDate       AS DATE NO-UNDO.
DEF VAR lcLogDir          AS CHAR NO-UNDO.
DEF VAR lcLogFile1        AS CHAR NO-UNDO.
DEF VAR lcDelim           AS CHAR NO-UNDO INIT ";".

lcTableName = "Multiholder".
lcActionID = "Multiholder_collector_cron".
ldCurrentTimeTS = fMakeTS().



FUNCTION fReport RETURNS CHAR
   (INPUT idStartTS AS DECIMAL,
    INPUT idEndTS AS DECIMAL):
   DEF BUFFER Order FOR Order. 
   DEF BUFFER OrderCustomer FOR OrderCustomer.
   DEF BUFFER OrderCustomer1 FOR OrderCustomer.
   DEF BUFFER OrderFusion FOR OrderFusion.
   DEF VAR lcRow AS CHAR NO-UNDO.
   OUTPUT STREAM sOutFile to VALUE(lcFile) APPEND.
   FOR EACH Order NO-LOCK WHERE
            Order.Brand EQ gcBrand AND
            Order.CRStamp < idEndTS AND
            Order.CRStamp >= idStartTS:
      FIND FIRST OrderCustomer1 NO-LOCK WHERE
                 OrderCustomer1.Brand EQ Order.Brand AND
                 OrderCustomer1.Orderid EQ Order.OrderID AND
                 OrderCustomer1.RowType EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}
                 NO-ERROR.
      IF NOT AVAIL OrderCustomer1 THEN NEXT.
      FIND FIRST OrderFusion NO-LOCK WHERE 
                 OrderFusion.Brand EQ Order.Brand AND
                 OrderFusion.OrderId EQ Order.OrderID.
      IF NOT AVAIL OrderFusion THEN NEXT.           

      FOR EACH OrderCustomer NO-LOCK WHERE
               OrderCustomer.Brand EQ Order.Brand AND
               OrderCustomer.Orderid EQ Order.OrderID AND
               OrderCustomer.RowType EQ 10 : /*ORDERCUSTOMER_ROWTYPE_FIXED_POUSER */ 
               
         lcRow = "".
         /*WorkOrderID;PhoneNumber; */
         lcRow = "Y" + STRING(Order.OrderID) + lcDelim +
              OrderFusion.FixedNumber + lcDelim +
         /*firstNameHolder;middleNameHolder;lastNameHolder;
         documentTypeHolder;documentNumberHolder; */
              OrderCustomer1.FirstName + lcDelim +
              OrderCustomer1.SurName1 + lcDelim +
              OrderCustomer1.SurName2 + lcDelim +
              OrderCustomer1.CustIDType + lcDelim +
              OrderCustomer1.CustID + lcDelim + 
         /*firstNameMultiHolder;middleNameMultiHolder;lastNameMultiHolder;
         documentTypeMultiHolder */
              OrderCustomer.FirstName + lcDelim +
              OrderCustomer.SurName1 + lcDelim +
              OrderCustomer.SurName2 + lcDelim +
              OrderCustomer.CustIDType + lcDelim +
              OrderCustomer.CustID + lcDelim. 
         
         PUT STREAM sOutFile UNFORMATTED lcRow SKIP.
      END.
   END.

RETURN "".    
END.

/*Dir and file definition*/
ASSIGN
   ldaReadDate  = TODAY        
   lcSpoolDir = "/tmp/"

       lcFile   = lcSpoolDir + "multiholder_data_" +
                      STRING(YEAR(ldaReadDate)) +
                      STRING(MONTH(ldaReadDate),"99") +
                      STRING(DAY(ldaReadDate),"99") +
                      REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".txt".


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
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      RETURN. /*No reporting in first time.*/
   END.
   ELSE DO:
      /*store previous starting time before setting new value to db*/
      ldCollPeriodStartTS = ActionLog.ActionTS.
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = katun.
      
      RELEASE Actionlog.
   END.
END.

/*Execute read operation and assign new period end time to actionlog.*/
ldCollPeriodEndTS = fSecOffSet(ldCurrentTimeTS, -60).

/*Actual collection*/
fReport(ldCollPeriodStartTS, ldCollPeriodEndTS).

/*send email*/
lcConfDir = fCParamC("RepConfDir"). /* /apps/yoigo/tms/Mailconf/ */
GetRecipients(lcConfDir + "multiholder.email").
IF xMailAddr > "" THEN DO:
   SendMail(lcFile, "").
END.

/* Move the file to Transfer directory */
/*fMove2TransDir(lcFile, ".txt", lcOutDir). Not needed because of the temporary natyure of this feature */

/*Update cunrent collection period end time to actionlog*/
DO TRANS:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  gcBrand        AND
              ActionLog.ActionID  EQ  lcActionID     AND
              ActionLog.TableName EQ  lcTableName    AND
              ActionLog.ActionStatus NE {&ACTIONLOG_STATUS_SUCCESS}
   EXCLUSIVE-LOCK NO-ERROR.
   IF AVAIL ActionLog THEN DO:
      ActionLog.ActionTS = ldCollPeriodEndTS.
      ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
   END.   
   RELEASE ActionLog.
END.





