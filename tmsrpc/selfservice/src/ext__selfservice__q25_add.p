/**
external_selfservice__q25_add.p
* Create Quota 25 extension request in TMS

* @input    msisdn;string;mandatory

* @output   boolean;true
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
DEFINE SHARED VARIABLE ghAuthLog AS HANDLE NO-UNDO.
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/fsendsms.i}
{Func/fexternalapi.i}
{Func/q25functions.i}

/* top_struct */
DEF VAR top_struct        AS CHARACTER NO-UNDO.
DEF VAR top_struct_fields AS CHARACTER NO-UNDO.

DEF VAR pcCLI             AS CHARACTER   NO-UNDO. /* MSISDN */
DEF VAR pcTransId         AS CHAR NO-UNDO. 

DEF VAR liCreated        AS INTEGER   NO-UNDO.
DEF VAR lcResult         AS CHARACTER NO-UNDO.

DEF VAR ldaMonth22Date    AS DATE NO-UNDO.
DEF VAR ldaMonth24Date    AS DATE NO-UNDO.
DEF VAR ldaQ25PeriodStartDate  AS DATE NO-UNDO.
DEF VAR ldaQ25PeriodEndDate    AS DATE NO-UNDO.
DEF VAR ldeFeeAmount      AS DEC NO-UNDO. 

/* Contract activation timestamp */
DEF VAR ldContractActivTS AS DECIMAL NO-UNDO.
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
DEF VAR lcSMSTxt AS CHAR NO-UNDO. 
DEF VAR lcApplicationId  AS CHAR NO-UNDO.
DEF VAR lcAppEndUserId   AS CHAR NO-UNDO.
/* common validation */
IF validate_request(param_toplevel_id, "string,string") EQ ? THEN RETURN.

ASSIGN pcTransId  = get_string(param_toplevel_id,"0")
       pcCLI      = get_string(param_toplevel_id,"1").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/findtenant.i NO ordercanal MobSub Cli pcCLI}

ASSIGN lcApplicationId = SUBSTRING(pcTransId,1,3)
       lcAppEndUserId  = ghAuthLog::EndUserId.

katun = lcApplicationId + "_" + ghAuthLog::EndUserId.  /* YTS-8221 fixed back */

FIND FIRST Customer NO-LOCK WHERE
           Customer.Custnum = MobSub.Custnum NO-ERROR.
IF NOT AVAILABLE Customer THEN
   RETURN appl_err("Customer not found").

ASSIGN
   /* Possible Q25 period validto value should be between fisrt day of current
      month and last day of current month + 2 */
   ldaQ25PeriodStartDate    = DATE(MONTH(TODAY),1, YEAR(TODAY)) - 1
   ldaQ25PeriodEndDate    = ADD-INTERVAL(TODAY, 2, 'months':U)
   ldaQ25PeriodEndDate    = fLastDayOfMonth(ldaQ25PeriodEndDate).

/* Find original installment contract */   
FIND DCCLI NO-LOCK WHERE
     DCCLI.Brand   = gcBrand AND
     DCCLI.DCEvent BEGINS "PAYTERM" AND
     DCCLI.MsSeq   = MobSub.MsSeq AND 
     DCCLI.ValidTo >= ldaQ25PeriodStartDate AND
     DCCLI.ValidTo <= ldaQ25PeriodEndDate NO-ERROR. 

IF NOT AVAIL DCCLI THEN
   RETURN appl_err("Installment contract not found").

IF DCCLI.TermDate NE ? THEN
   RETURN appl_err("Installment contract terminated").
   
FIND SingleFee USE-INDEX Custnum WHERE
     SingleFee.Brand       = gcBrand AND
     SingleFee.Custnum     = MobSub.CustNum AND
     SingleFee.HostTable   = "Mobsub" AND
     SingleFee.KeyValue    = STRING(Mobsub.MsSeq) AND
     SingleFee.SourceTable = "DCCLI" AND
     SingleFee.SourceKey   = STRING(DCCLI.PerContractId) AND
     SingleFee.CalcObj     = "RVTERM" NO-LOCK NO-ERROR.

IF NOT AVAIL SingleFee THEN
   RETURN appl_err("Residual fee not found").

IF SingleFee.Billed AND 
   NOT CAN-FIND(FIRST Invoice NO-LOCK WHERE
                      Invoice.Invnum = SingleFee.InvNum aND
                      Invoice.InvType = 99) THEN 
   RETURN appl_err("Residual fee billed").

ASSIGN   
   ldaMonth22Date    = ADD-INTERVAL(DCCLI.ValidFrom, 22, 'months':U)
   ldaMonth22Date    = DATE(MONTH(ldaMonth22Date),1,YEAR(ldaMonth22Date))
   ldaMonth24Date    = ADD-INTERVAL(DCCLI.ValidFrom, 24, 'months':U)
   ldaMonth24Date    = DATE(MONTH(ldaMonth24Date),21,YEAR(ldaMonth24Date)).

/* If the Quota 25 prorate request is created between 1st day of month 22
   until 20th day of month 24 then
   it should be handled on 21st day of month 24 at 00:00.
   But if the request is created after 20th day of month 24 then
   those will be handled immediately
   */
IF TODAY < ldaMonth22Date THEN
   RETURN appl_err("Q25 extension not allowed before 22th month").
ELSE IF TODAY >= ldaMonth22Date AND
   TODAY < ldaMonth24Date THEN
   /* handle it on 21st day of month 24 at 00:00 */
   ldContractActivTS = fMake2Dt(ldaMonth24Date,0).
ELSE ASSIGN
   ldaMonth24Date = TODAY
   ldContractActivTS = fSecOffSet(fMakeTS(),5). /* Handle it immediately */

IF CAN-FIND(FIRST DCCLI NO-LOCK WHERE
                  DCCLI.Brand   EQ gcBrand AND
                  DCCLI.DCEvent EQ "RVTERM12" AND
                  DCCLI.MsSeq   EQ MobSub.MsSeq AND
                  DCCLI.ValidTo >= TODAY) THEN
   RETURN appl_err("Q25 extension already active").

IF SingleFee.OrderId > 0 THEN DO:

   FIND FIRST TermReturn NO-LOCK WHERE
              TermReturn.OrderId = SingleFee.OrderId NO-ERROR.

   IF AVAIL TermReturn AND 
          ((TermReturn.DeviceScreen = TRUE AND TermReturn.DeviceStart  = TRUE) OR 
           (TermReturn.DeviceScreen = ?    AND TermReturn.DeviceStart  = ?)) THEN
      RETURN appl_err("Already returned terminal").
END.

liCreated = fPCActionRequest(
   MobSub.MsSeq,
   "RVTERM12",
   "act",
   ldContractActivTS,
   TRUE, /* create fees */
   {&REQUEST_SOURCE_EXTERNAL_API},
   "",
   0,
   FALSE,
   "",
   0, /* payterm residual fee */
   DCCLI.PerContractId, /* Periodical Contract-ID */
   OUTPUT lcResult).   

IF liCreated = 0 THEN
   RETURN appl_err(SUBST("Q25 extension request failed: &1",
                         lcResult)).

/*YPR-3268:ac3*/
FIND FIRST MSRequest WHERE
           MSRequest.MSrequest EQ liCreated EXCLUSIVE-LOCK NO-ERROR.
IF AVAIL MsRequest THEN DO:
   /*YBU-5247: For this reqtype this is not reserved for ContractID*/
   MsRequest.ReqCparam6 = fBankByBillCode(SingleFee.BillCode).
END.
RELEASE MsRequest.

CASE SingleFee.BillCode:
   WHEN "RVTERM1EF" THEN
      lcSMSTxt = fGetSMSTxt("Q25ExtensionUNOE",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp).
   /* YPR-3565 */
/*    WHEN "RVTERMBSF" THEN
      lcSMSTxt = fGetSMSTxt("Q25ExtensionSabadell",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp). */
    WHEN "RVTERMBCF" THEN
      lcSMSTxt = fGetSMSTxt("Q25ExtensionCetelem",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp). 

   OTHERWISE 
      lcSMSTxt = fGetSMSTxt("Q25ExtensionYoigo",
                            TODAY,
                            Customer.Language,
                            OUTPUT ldeSMSStamp).
END CASE.

IF lcSMSTxt > "" THEN DO:
   
   ldeFeeAmount = SingleFee.Amt.
   
   FOR EACH DiscountPlan NO-LOCK WHERE
            DiscountPlan.Brand = gcBrand AND
           (DiscountPlan.DPRuleID = "RVTERMDT1DISC" OR
            DiscountPlan.DPRuleID = "RVTERMDT4DISC"),
       EACH DPMember NO-LOCK WHERE
            DPMember.DpID       = DiscountPlan.DpId AND
            DPMember.HostTable  = "MobSub" AND
            DPMember.KeyValue   = STRING(MobSub.MsSeq) AND
            DPMember.ValidFrom  = fPer2Date(SingleFee.BillPeriod,0) AND
            DPMember.ValidTo   >= DPMember.ValidFrom:
      ldeFeeAmount = ldeFeeAmount - DPMember.DiscValue.
   END.

   IF ldeFeeAmount > 0 THEN DO:

      ASSIGN
         lcSMSTxt = REPLACE(lcSMSTxt,"#MONTHNAME",
                             lower(entry(month(ADD-INTERVAL(ldaMonth24Date, 1,
                             'months':U)),{&MONTHS_ES})))
         lcSMSTxt = REPLACE(lcSMSTxt,"#YEAR", STRING(YEAR(ldaMonth24Date)))
         lcSMSTxt = REPLACE(lcSMSTxt,"#AMOUNT",
               STRING(TRUNC(ldeFeeAmount / 12, 2))).

      fMakeSchedSMS2(MobSub.CustNum,
                     MobSub.CLI,
                     {&SMSTYPE_CONTRACT_ACTIVATION},
                     lcSMSTxt,
                     ldeSMSStamp,
                     "Yoigo info",
                     "").
   END.
END.

/* Adding the details into Main struct */
top_struct = add_struct(response_toplevel_id, "").
add_string(top_struct, "transaction_id", pcTransId).
add_boolean(top_struct, "result", True).

FINALLY:
   ghAuthLog::TransactionId = pcTransId.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
