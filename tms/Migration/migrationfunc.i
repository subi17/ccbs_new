/* ----------------------------------------------------------------------
  Module .......: migration.i.i
  Task .........: Functions that are used in migration related needs
  Application ..: TMS
  Author .......: ilsavola
  Created ......: 10.2.2017
  Version ......: Yoigo
---------------------------------------------------------------------- */
&IF "{&migrationi}" NE "YES"
&THEN
&GLOBAL-DEFINE migrationi YES

/*Temp table for sending NC response info to WEB.*/
DEF TEMP-TABLE NCResponse NO-UNDO
   FIELD Order AS CHAR
   FIELD MSISDN AS CHAR
   FIELD StatusCode AS CHAR
   FIELD Comment AS CHAR.


{commali.i}
{Syst/tmsconst.i}
{Func/freacmobsub.i}
/*Function checks that customer given:
   -does not have active subscrition
   -does not have subscription that can be reactivated
   RETURN:
   "" OK
   Error text in failure cases
*/
FUNCTION fMigrationCheckCustomer RETURNS CHAR
   (INPUT icBrand AS CHAR,
    INPUT icCustID AS CHAR):
   DEF BUFFER Customer for Customer.
   DEF BUFFER MobSub for MobSub.
   DEF BUFFER TermMobSub for TermMobSub.
   DEF VAR lcResult AS CHAR NO-UNDO.
  
   FOR EACH Customer NO-LOCK WHERE
            Customer.Brand EQ icBrand AND
            Customer.OrgID EQ icCustID:
      FIND FIRST MobSub NO-LOCK WHERE
                 MobSub.Brand EQ icBrand AND
                 MobSub.CustNum EQ Customer.CustNum.
      IF AVAIL MobSub THEN RETURN "ERROR: Mobsub for the customer exists".

      FOR EACH TermMobSub NO-LOCK WHERE
               TermMobSub.Brand EQ icBrand AND
               TermMobSub.CustNum EQ Customer.Custnum:
         lcResult = freacprecheck(TermMobSub.MsSeq,
                                  "username",
                                  FALSE). 
         if lcResult NE "" THEN RETURN lcResult.
      END.

   END.

RETURN "".
END.


FUNCTION fIsNumberInMigration RETURNS LOGICAL
   (icCLI AS CHAR):
   DEF BUFFER Order FOR Order.
   FIND FIRST Order NO-LOCK WHERE
              Order.CLI EQ icCLI AND
              (Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR /*60*/
               Order.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}).  /*61*/
   IF AVAIL Order THEN RETURN TRUE.

   RETURN FALSE.

END.   

/*Function creates Json entry containing Nodo Central response related data*/
FUNCTION fGenerateNCResponseInfo RETURNS CHAR
   (iiOrderID AS INT,
    icMSISDN AS CHAR,
    icStatusCode AS CHAR,
    icComment AS CHAR):
   DEF VAR lcTargetType AS CHAR NO-UNDO.
   DEF VAR llcMessage  AS LONGCHAR NO-UNDO.
   DEF VAR llgOK AS LOGICAL NO-UNDO.


   CREATE NCResponse.
   ASSIGN
      NCResponse.Order = STRING(iiOrderID)
      NCResponse.MSISDN = icMSISDN
      NCResponse.Statuscode = icStatusCode
      NCResponse.Comment = icComment.
   llgOK = TEMP-TABLE NCResponse:WRITE-JSON("LONGCHAR", /*writing type*/
                                             llcMessage, /*target*/
                                             TRUE). /*formatted to readabale*/

   EMPTY TEMP-TABLE NCResponse.
   IF llgOK EQ TRUE THEN RETURN STRING( llcMessage  ).


RETURN "".
END.


/*Function creates Json entry containing Order status information*/
FUNCTION fGenerateOrderInfo RETURNS CHAR
   (iiOrderID AS INT):

RETURN "".
END.   

/*Function creates Json entry containing Operational data  status information*/
FUNCTION fGenerateOPDataInfo RETURNS CHAR
   (iiOrderID AS INT):

RETURN "".
END.   


&ENDIF

