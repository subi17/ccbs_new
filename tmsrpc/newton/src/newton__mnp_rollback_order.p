/**
 * Create a MNP rollback order
 *
 * @input   orderdata;struct;order data

 * @orderdata  brand:string;mandatory;Tenant
               msseq;int;mandatory;subscription id
               id_type;string;mandatory;id type
               person_id;string;mandatory;person id
               contract_id;string;mandatory;contract id
               salesman;string;optional;id of the seller
               channel;string;optional;controller name
               orderer_ip;string;mandatory;IP that submits the order
               old_operator;string;optional;for mnp only
               retention_order_id;int;optional;Retention Order Id

 * @output     boolean;true
 */

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
Syst.Var:katun = "NewtonRPC".
{Func/orderchk.i}
{Func/barrfunc.i}
{Func/fixedlinefunc.i}
{Syst/tmsconst.i}
{Func/create_eventlog.i}
{Mm/fbundle.i}

DEF VAR pcArray              AS CHAR  NO-UNDO.
DEF VAR pcStruct             AS CHAR  NO-UNDO.
DEF VAR pcSubsStruct         AS CHAR  NO-UNDO.
DEF VAR lcStruct             AS CHAR  NO-UNDO.
DEF VAR pcTenant             AS CHAR  NO-UNDO.
DEF VAR piMsSeq              AS INT   NO-UNDO.
DEF VAR pcIdType             AS CHAR  NO-UNDO.
DEF VAR pcPersonId           AS CHAR  NO-UNDO.
DEF VAR pcContractId         AS CHAR  NO-UNDO.
DEF VAR pcOldOperator        AS CHAR  NO-UNDO.
DEF VAR pcSalesman           AS CHAR  NO-UNDO.
DEF VAR pcChannel            AS CHAR  NO-UNDO.
DEF VAR pcOrderIP            AS CHAR  NO-UNDO.

DEF VAR piRetentionOrderId   AS INT   NO-UNDO.
DEF VAR pcOldOperatorPayType AS CHAR  NO-UNDO.
DEF VAR pcOldOperatorICC     AS CHAR  NO-UNDO.

DEF VAR liOrderId            AS INT   NO-UNDO.
DEF VAR liCounter            AS INT   NO-UNDO.

DEF TEMP-TABLE ttMNPRollback NO-UNDO
   FIELD MsSeq            AS INT
   FIELD CLI              AS CHAR
   FIELD CLIType          AS CHAR
   FIELD ContractId       AS CHAR
   FIELD ICC              AS CHAR
   FIELD OldICC           AS CHAR
   FIELD PayType          AS LOG
   FIELD OldPayType       AS LOG
   FIELD RetentionOrderId AS INT
   INDEX MsSeq IS PRIMARY UNIQUE MsSeq.

FUNCTION fUpdateSIM RETURNS LOGICAL:

   IF AVAIL SIM THEN SIM.simstat = 4.

   RETURN TRUE.
END.

FUNCTION fHandleCorporateCustomer RETURNS LOGICAL:

   DEF BUFFER bMobSub   FOR MobSub.
   /*
   The Cases where CIF order should go to automated order management

   CASE 1: If the customer has any active subs. then order should go to 21
   CASE 2: If the customer has not any active subs. then order should go to 20
   */

   IF AVAIL Customer THEN DO:
      FIND FIRST bMobSub WHERE
                 bMobSub.Brand   = Syst.Var:gcBrand AND
                 bMobSub.AgrCust = Customer.CustNum
           NO-LOCK NO-ERROR.
      IF NOT AVAIL bMobSub THEN Order.StatusCode = "20".
      ELSE Order.StatusCode = "21".
   END. /* IF AVAIL Customer THEN DO: */
   ELSE Order.StatusCode = "20".

END. /* FUNCTION fHandleCorporateCustomer RETURNS LOGICAL: */

FUNCTION fCreateOrder RETURNS LOGICAL:

   CREATE Order.
   ASSIGN
      Order.Brand           = Syst.Var:gcBrand 
      Order.OrderId         = liOrderId
      Order.Salesman        = pcSalesman
      Order.Source          = "newton"
      Order.OrderChannel    = pcChannel 
      Order.OrdererIP       = pcOrderIP
      Order.CrStamp         = Func.Common:mMakeTS() 
      Order.InvCustRole     = 1
      Order.UserRole        = 1
      Order.StatusCode      = "3"
      Order.OrderType       = 3
      Order.CLI             = ttMNPRollback.CLI
      Order.CLIType         = ttMNPRollback.CLIType
      Order.mnpstatus       = 1
      Order.paytype         = ttMNPRollback.PayType
      Order.OldPayType      = ttMNPRollback.OldPayType
      Order.ICC             = ttMNPRollback.ICC
      Order.OldIcc          = ttMNPRollback.OldICC
      Order.CurrOper        = pcOldOperator
      Order.ContractId      = ttMNPRollback.ContractId
      Order.MsSeq           = ttMNPRollback.MsSeq
      Order.CustNum         = Customer.CustNum.

   IF Customer.CustIdType = "CIF"
   THEN fHandleCorporateCustomer().

END. /* FUNCTION fCreateOrder RETURNS LOGICAL: */

FUNCTION fCreateOrderCustomer RETURNS LOGICAL:

   CREATE OrderCustomer.
   ASSIGN
      OrderCustomer.OrderId     = Order.OrderId
      OrderCustomer.RowType     = 1
      OrderCustomer.CustId      = Customer.OrgId
      OrderCustomer.DataChecked = TRUE
      OrderCustomer.Language    = STRING(Customer.Language)
      OrderCustomer.CustTitle   = Customer.HonTitle
      OrderCustomer.Surname1    = Customer.CustName
      OrderCustomer.Company     = Customer.CompanyName
      OrderCustomer.BankCode    = Customer.BankAcc
      OrderCustomer.FixedNumber = Customer.Phone
      OrderCustomer.MobileNumber = Customer.SMSNumber
      OrderCustomer.Street      = Customer.Address.

   BUFFER-COPY Customer EXCEPT Language TO OrderCustomer.
   
   FIND FIRST CustCat WHERE CustCat.Brand = Syst.Var:gcBrand AND CustCat.Category = Customer.Category NO-LOCK NO-ERROR.
   IF AVAIL CustCat THEN
       ASSIGN 
           OrderCustomer.Pro          = CustCat.Pro
           OrderCustomer.SelfEmployed = CustCat.SelfEmployed.

   RETURN TRUE.
END.

/* Main block */
IF validate_request(param_toplevel_id,"struct,array") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
pcArray = get_array(param_toplevel_id, "1").

lcStruct = validate_request(pcStruct,"brand,id_type,person_id,old_operator,salesman,channel,orderer_ip").
IF gi_xmlrpc_error NE 0 THEN RETURN.

ASSIGN
   pcTenant      = get_string(pcStruct,"brand")
   pcIdType      = get_string(pcStruct,"id_type")
   pcPersonId    = get_string(pcStruct,"person_id")
   pcOldOperator = get_string(pcStruct,"old_operator").

ASSIGN pcSalesman = get_string(pcStruct,"salesman")
                    WHEN LOOKUP("salesman",lcStruct) > 0
       pcChannel  = get_string(pcStruct,"channel")
                    WHEN LOOKUP("channel",lcStruct) > 0
       pcOrderIP  = get_string(pcStruct,"orderer_ip")
                    WHEN LOOKUP("orderer_ip",lcStruct) > 0.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pcIdType = "" OR pcIdType = ? THEN
   RETURN appl_err("Id Type is blank or unknown").

IF pcPersonId = "" OR pcPersonId = ? THEN
   RETURN appl_err("Person Id is blank or unknown").

IF pcOldOperator = "" OR pcOldOperator = ? THEN
   RETURN appl_err("Old operator is blank or unknown").

{newton/src/settenant.i pcTenant}

FIND FIRST Customer WHERE
           Customer.Brand      = Syst.Var:gcBrand    AND
           Customer.CustIDType = pcIdType   AND
           Customer.OrgId      = pcPersonId AND
           Customer.Roles     <> "inactive" NO-LOCK NO-ERROR.
IF NOT AVAIL Customer THEN
   RETURN appl_err("Customer not found").

IF fExistBarredSubForCustomer(Customer.CustNum) THEN
   RETURN appl_err("barring").

DO liCounter = 0 TO get_paramcount(pcArray) - 1:
   pcSubsStruct = get_struct(pcArray, STRING(liCounter)).

   lcStruct = validate_request(pcSubsStruct,"msseq,contract_id,old_operator_paytype,old_operator_icc,retention_order_id").
   IF gi_xmlrpc_error NE 0 THEN RETURN.

   ASSIGN
      piMsSeq       = get_int(pcSubsStruct,"msseq")
      pcContractId  = get_string(pcSubsStruct,"contract_id")
      pcOldOperatorPayType = get_string(pcSubsStruct,"old_operator_paytype").

   ASSIGN piRetentionOrderId = get_int(pcSubsStruct,"retention_order_id")
                       WHEN LOOKUP("retention_order_id",lcStruct) > 0
          pcOldOperatorICC = get_string(pcSubsStruct,"old_operator_icc")
                       WHEN LOOKUP("old_operator_icc",lcStruct) > 0.

   IF gi_xmlrpc_error NE 0 THEN RETURN.

   FIND FIRST TermMobSub WHERE
              TermMobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL TermMobSub THEN
      RETURN appl_err("Invalid Subscription Id").

   IF fHasConvergenceTariff(piMsSeq) AND
      TermMobsub.fixednumber NE "" THEN
      RETURN appl_err("Not allowed for fixed line tariffs").
   ELSE IF fHasConvergenceTariff(piMsSeq) AND
        NOT CAN-FIND(FIRST MobSub WHERE
                           MobSub.MsSeq = piMsSeq AND
                           Mobsub.cli EQ Mobsub.fixednumber) THEN
      RETURN "Not allowed when fixed line terminated".   

   IF pcOldOperatorPayType = "" OR pcOldOperatorPayType = ? THEN
      RETURN appl_err("Old operator paytype is blank or unknown").

   IF pcContractId = "" OR pcContractId = ? THEN
      RETURN appl_err("Contract Id is blank or unknown").

   IF NOT fHasConvergenceTariff(piMsSeq) AND
      CAN-FIND(FIRST MobSub WHERE
                     MobSub.MsSeq = piMsSeq NO-LOCK) THEN
      RETURN appl_err("Subscription is alreay active").

   IF CAN-FIND(FIRST MobSub WHERE
                     MobSub.Brand = Syst.Var:gcBrand AND
                     MobSub.CLI   = TermMobSub.CLI NO-LOCK) OR
      CAN-FIND(FIRST Order WHERE
                     Order.Brand = Syst.Var:gcBrand AND
                     Order.CLI   = TermMobSub.CLI AND
                     Order.OrderType NE {&ORDER_TYPE_RENEWAL} AND
                     Order.OrderType NE {&ORDER_TYPE_STC} AND
                     Order.OrderType NE {&ORDER_TYPE_ACC} AND
                     LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0 NO-LOCK) THEN
      RETURN appl_err("MSISDN is already in use").

   FIND FIRST IMSI WHERE
              IMSI.IMSI = TermMobSub.IMSI NO-LOCK NO-ERROR.
   IF NOT AVAIL IMSI THEN
      RETURN appl_err("IMSI not found").

   FIND FIRST SIM WHERE
              SIM.ICC = TermMobSub.ICC NO-LOCK NO-ERROR.
   IF NOT AVAIL SIM THEN
      RETURN appl_err("SIM not found").
   ELSE IF SIM.simstat = 4 THEN
      RETURN appl_err("SIM is already in use").

   FIND CLIType WHERE
        CLIType.Brand = Syst.Var:gcBrand AND
        CLIType.CliType = TermMobSub.CLIType NO-LOCK NO-ERROR.
   IF NOT AVAIL CLIType THEN 
      RETURN appl_err("Invalid CLIType").

   FIND FIRST ttMNPRollback WHERE
              ttMNPRollback.MsSeq = TermMobSub.MsSeq NO-LOCK NO-ERROR.
   IF AVAIL ttMNPRollback THEN NEXT.

   CREATE ttMNPRollback.
   ASSIGN ttMNPRollback.MsSeq      = TermMobSub.MsSeq
          ttMNPRollback.CLI        = TermMobSub.CLI
          ttMNPRollback.CLIType    = TermMobSub.CLIType
          ttMNPRollback.ICC        = TermMobSub.ICC
          ttMNPRollback.PayType    = (IF CLIType.PayType = 2 THEN TRUE ELSE FALSE)
          ttMNPRollback.OldPayType = (pcOldOperatorPayType EQ "PrePaid")
          ttMNPRollback.OldICC     = pcOldOperatorICC
          ttMNPRollback.ContractId = pcContractId
          ttMNPRollback.RetentionOrderId = piRetentionOrderId.
END.

FOR EACH ttMNPRollback:

   FIND FIRST SIM WHERE
              SIM.ICC = ttMNPRollback.ICC EXCLUSIVE-LOCK NO-ERROR.

   fUpdateSIM().
   liOrderId = NEXT-VALUE(OrderId).

   fCreateOrder().

   fCreateOrderCustomer().

   IF ttMNPRollback.RetentionOrderId > 0 THEN
      fCreateOrderAction(Order.OrderId,"OrderId",
                         STRING(ttMNPRollback.RetentionOrderId),"").

   /* YTS-2890 */
   fMakeCreateEvent((BUFFER Order:HANDLE),"",Syst.Var:katun,"").
END.

add_boolean(response_toplevel_id, "", True).

FINALLY:
   END.
