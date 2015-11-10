/**
external_selfservice__q25_add.p
* Create Quota 25 extension request in TMS

* @input    msisdn;string;mandatory

* @output         boolean;true
*/

{xmlrpc/xmlrpc_access.i}
{commpaa.i}
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
{fmakemsreq.i}
{fsendsms.i}

/* top_struct */
DEF VAR top_struct        AS CHARACTER NO-UNDO.
DEF VAR top_struct_fields AS CHARACTER NO-UNDO.

/* q25_struct */
DEF VAR lcusername    AS CHARACTER NO-UNDO. /* Quota 25 person who requests the change */
DEF VAR limsseq       AS INTEGER   NO-UNDO. /* Quota 25 subscription id */
DEF VAR liper_contract_id AS INTEGER   NO-UNDO. /* Quota 25 installment contract id */
DEF VAR pcCLI             AS CHARACTER   NO-UNDO. /* MSISDN */
DEF VAR pcQ25Struct       AS CHARACTER NO-UNDO. /* Quota 25 input struct */
DEF VAR lcQ25Struct       AS CHARACTER NO-UNDO.

/* memo_struct */
DEF VAR lcmemo_title       AS CHARACTER NO-UNDO. /* Memo Title */
DEF VAR lcmemo_content     AS CHARACTER NO-UNDO. /* Memo Content */
DEF VAR pcmemoStruct       AS CHARACTER NO-UNDO. /* Memo input struct */
DEF VAR lcmemoStruct       AS CHARACTER NO-UNDO.

DEF VAR liCreated        AS INTEGER   NO-UNDO.
DEF VAR lcResult         AS CHARACTER NO-UNDO.

DEF VAR ldaMonth22Date    AS DATE NO-UNDO.
DEF VAR ldaMonth24Date    AS DATE NO-UNDO.
DEF VAR ldaQ25PeriodStartDate  AS DATE NO-UNDO.
DEF VAR ldaQ25PeriodEndDate    AS DATE NO-UNDO.

/* Contract activation timestamp */
DEF VAR ldContractActivTS AS DECIMAL NO-UNDO.
DEF VAR ldeSMSStamp AS DEC NO-UNDO. 
DEF VAR lcSMSTxt AS CHAR NO-UNDO. 

/* common validation */
IF validate_request(param_toplevel_id, "string") EQ ? THEN RETURN.

ASSIGN pcCLI = get_string(param_toplevel_id,"0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

katun = "Adapter".

FIND FIRST MobSub NO-LOCK WHERE
           Mobsub.brand = gcBrand AND
           MobSub.CLI = pcCLI NO-ERROR.
           
IF NOT AVAILABLE MobSub THEN
   RETURN appl_err("Subscription not found").

FIND FIRST Customer NO-LOCK WHERE
           Customer.Custnum = MobSub.Custnum NO-ERROR.
IF NOT AVAILABLE Customer THEN
   RETURN appl_err("Customer not found").

ASSIGN
   ldaQ25PeriodStartDate    = ADD-INTERVAL(TODAY, -22, 'months':U)
   ldaQ25PeriodStartDate    = DATE(MONTH(ldaQ25PeriodStartDate),
                                   DAY(fLastDayOfMonth(ldaQ25PeriodStartDate)),
                                   YEAR(ldaQ25PeriodStartDate))
   ldaQ25PeriodEndDate    = ADD-INTERVAL(TODAY, -24, 'months':U)
   ldaQ25PeriodEndDate    = DATE(MONTH(ldaQ25PeriodEndDate),1,
                                       YEAR(ldaQ25PeriodEndDate)).

/* Find original installment contract */   
FIND FIRST DCCLI NO-LOCK WHERE
           DCCLI.Brand   = gcBrand AND
           DCCLI.DCEvent BEGINS "PAYTERM" AND
           DCCLI.MsSeq   = MobSub.MsSeq AND 
           DCCLI.FromDate < ldaQ25PeriodStartDate AND
           DCCLI.FromDate > ldaQ25PeriodEndDate NO-ERROR. 

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

liCreated = fPCActionRequest(
   MobSub.MsSeq,
   "RVTERM12",
   "act",
   ldContractActivTS,
   TRUE, /* create fees */
   {&REQUEST_SOURCE_NEWTON},
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


lcSMSTxt = fGetSMSTxt("Q25ExtensionYoigo",
                      TODAY,
                      Customer.Language,
                      OUTPUT ldeSMSStamp).

IF lcSMSTxt > "" THEN DO:

   ASSIGN
      lcSMSTxt = REPLACE(lcSMSTxt,"#MONTHNAME",
                          lower(entry(month(ldaMonth24Date),{&MONTHS_ES})))
      lcSMSTxt = REPLACE(lcSMSTxt,"#YEAR", STRING(YEAR(ldaMonth24Date)))
      lcSMSTxt = REPLACE(lcSMSTxt,"#AMOUNT",
            STRING(ROUND(SingleFee.Amt / 12, 2))).

   fMakeSchedSMS2(MobSub.CustNum,
                  MobSub.CLI,
                  {&SMSTYPE_CONTRACT_ACTIVATION},
                  lcSMSTxt,
                  ldeSMSStamp,
                  "Yoigo info",
                  "").
END.
/*
IF lcmemo_title > "" THEN DO:

   CREATE Memo.
   ASSIGN
       Memo.CreStamp  = {&nowTS}
       Memo.Brand     = gcBrand
       Memo.HostTable = "MobSub"
       Memo.KeyValue  = STRING(MobSub.MsSeq)
       Memo.MemoSeq   = NEXT-VALUE(MemoSeq)
       Memo.CreUser   = katun
       Memo.MemoTitle = lcmemo_title
       Memo.MemoText  = lcmemo_content
       Memo.CustNum   = MobSub.CustNum.
END. /* IF lcmemo_title > "" AND lcmemo_content > "" THEN DO: */
*/
add_boolean(response_toplevel_id, "", TRUE).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
