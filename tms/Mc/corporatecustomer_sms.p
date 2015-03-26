/* ----------------------------------------------------------------------
  MODULE .......: corporatecustomer_sms.p
  TASK .........: Sends welcome sms-messages to new Corporate customers (YSC-2)
  APPLICATION ..: TMS
  AUTHOR .......: timok/anttis
  CREATED ......:
  CHANGED ......:
  Version ......: xfera
----------------------------------------------------------------------- */
{commpaa.i}
katun = "cron".
gcBrand = "1".
{fgettxt.i}
{timestamp.i}
{fmakesms.i}
{transname.i}
{fbundle.i}

DEF VAR lcBundleCLITypes  AS CHAR NO-UNDO.
DEF VAR lcReplacedTxt     AS CHAR NO-UNDO.
DEF VAR ldeSMSStamp       AS DEC  NO-UNDO.

lcBundleCLITypes = fCParamC("BUNDLE_BASED_CLITYPES").

FUNCTION createWelcomeSMS RETURNS LOGICAL(
   INPUT pcCLI AS CHAR,
   INPUT piCustNum AS INT,
   INPUT piLang AS INT,
   INPUT pcSMSText AS CHAR):


IF pcSMSText > "" THEN
   fMakeSchedSMS2(
      piCustNum,
      pcCLI,
      40,
      pcSMSText,
      ldeSMSStamp,
      "622622622",
      "").
   
END FUNCTION.

/*
   Check new subscriptions
*/
FUNCTION checkNewSubscriptions RETURNS LOGICAL:

   DEFINE VARIABLE ldaDate  AS DATE NO-UNDO.
   DEFINE VARIABLE liTime   AS INT  NO-UNDO.
   DEFINE VARIABLE ldeTs    AS DECIMAL NO-UNDO.
   DEFINE VARIABLE liStatus AS INT  NO-UNDO.

   fSplitTS(fMakeTs(), OUTPUT ldaDate, OUTPUT liTime).
   ldaDate = ldaDate - 1.
   ldeTs = fMake2Dt(ldaDate, liTime).
   
   DO liStatus = 1 TO 99:

      FOR EACH Mobsub NO-LOCK WHERE 
               MobSub.Brand  = gcBrand AND
               MobSub.MsStatus = liStatus AND
               MobSub.ActivationDate >= ldaDate AND
               MobSub.ActivationTS > ldeTs:

         FIND FIRST Customer WHERE 
                    Customer.custnum = Mobsub.custnum AND
                   (Customer.category = "20" OR
                    Customer.category = "40" OR
                    Customer.category = "41") NO-LOCK NO-ERROR.
         IF NOT AVAIL Customer THEN NEXT.

         FIND FIRST CallAlarm WHERE
                    CallAlarm.Brand = gcBrand AND
                    CallAlarm.CLI   = MobSub.CLI AND
                    CallAlarm.CreditType = 40 NO-LOCK NO-ERROR.
         IF AVAIL CallAlarm THEN NEXT. /* SMS is already created */

         IF LOOKUP(MobSub.CLIType,lcBundleCLITypes) > 0 THEN DO:
            IF MobSub.TariffBundle = "" THEN NEXT.

            lcReplacedTxt = fConvBundleToBillItem(MobSub.TariffBundle).
            lcReplacedTxt = fGetItemName(gcBrand,
                                   "BillItem",
                                   lcReplacedTxt,
                                   Customer.Language,
                                   TODAY).
         END.
         ELSE
            lcReplacedTxt = fGetItemName(gcBrand,
                                  "CLIType",
                                  MobSub.CLIType,
                                  Customer.Language,
                                  TODAY).

         lcSMSText = fGetSMSTxt("WelcomeSubs",
                                TODAY,
                                Customer.Language,
                                OUTPUT ldeSMSStamp).
   
         ASSIGN lcSMSText = REPLACE(lcSMSText,"#CLITYPE",lcReplacedTxt)
                lcSMSText = REPLACE(lcSMSText,"#CLI",MobSub.CLI).

         createWelcomeSMS(Mobsub.cli,Customer.custnum,Customer.language,lcSMSText).
      END.
   END.

   RETURN TRUE.

END FUNCTION.

/*
   Main
*/

checkNewSubscriptions().
IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
