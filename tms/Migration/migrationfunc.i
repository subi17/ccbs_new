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


&ENDIF

