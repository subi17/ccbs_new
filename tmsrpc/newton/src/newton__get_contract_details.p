/* ----------------------------------------------------------------------
  MODULE .......: newton__get_contract_details.p
  TASK .........: RPC for requesting Q25 extension AND terminal return contracts
  APPLICATION ..: 
  AUTHOR .......: subhash sanjeevi
  CREATED ......: 22-06-2016
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */
/*
@INPUT msisdn;string;optional
       cust_id_type;string;optional
       cust_id;string;optional
       salesman_id;string;mandatory
       date_from;date;optional

@OUTPUT 
       @subscription_struct:cli;string
                            ownerid_type;string
                            ownerid_num;int
                            owner_name;string
             @contracts_struct: contract_type;string
                                q25_contract_id;string
                                contract_date;date
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}

{Syst/commpaa.i}
Syst.Var:katun = "NewtonRPC".
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}

DEF VAR pcTenant     AS CHAR NO-UNDO. 
DEF VAR pcMSISDN     AS CHAR NO-UNDO. 
DEF VAR pcCustIDType AS CHAR NO-UNDO. 
DEF VAR pcCustID     AS CHAR NO-UNDO. 
DEF VAR pcSalesID    AS CHAR NO-UNDO. 
DEF VAR pcDate       AS DATE NO-UNDO.

DEF VAR lcTopArray           AS CHAR NO-UNDO.  
DEF VAR lcContractsArray     AS CHAR NO-UNDO. 
DEF VAR lpcDateStamp         AS DEC  NO-UNDO.
DEF VAR ldtTodayStamp        AS DEC  NO-UNDO.
DEF VAR lcCustName           AS CHAR NO-UNDO. 
DEF VAR ldtContractDate      AS DATE NO-UNDO.
DEF VAR llgContractDate      AS LOG  NO-UNDO.
DEF VAR gcSubscriptionStruct AS CHAR NO-UNDO. 
DEF VAR gcContractsStruct    AS CHAR NO-UNDO. 
DEF VAR lcUserCode           AS CHAR NO-UNDO. 

DEFINE TEMP-TABLE ttContractDetails NO-UNDO
   FIELD CLI           AS CHAR
   FIELD ContractType  AS CHAR
   FIELD ContractId    AS CHAR
   FIELD ContractDate  AS DATE
   FIELD CustomerType  AS CHAR
   INDEX CLI CLI.

DEFINE BUFFER bttContractDetails FOR ttContractDetails.

IF validate_request(param_toplevel_id,"string,string,string,string,string,datetime") EQ ? THEN
   RETURN.

ASSIGN 
   pcTenant     = get_string(param_toplevel_id,"0")
   pcMSISDN     = get_string(param_toplevel_id,"1")
   pcCustIDType = get_string(param_toplevel_id,"2")
   pcCustID     = get_string(param_toplevel_id,"3")   
   pcSalesID    = get_string(param_toplevel_id,"4")
   pcDate       = get_date(param_toplevel_id,"5").

IF gi_xmlrpc_error NE 0 THEN RETURN.

{newton/src/settenant.i pcTenant}

IF TRIM(pcSalesID) EQ "" THEN  
   RETURN appl_err("Sales ID is empty").

FIND SalesMan NO-LOCK WHERE 
     SalesMan.Brand    = Syst.Var:gcBrand   AND 
     SalesMan.Salesman = pcSalesID NO-ERROR.

IF NOT AVAIL SalesMan THEN 
   RETURN appl_err("Salesman is not available"). 

IF pcMSISDN NE "" AND 
   pcCustID NE "" THEN 
   RETURN appl_err("Both Subscription AND Customer ID are not allowed").

IF pcMSISDN NE "" THEN DO:
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.Brand = Syst.Var:gcBrand  AND 
              MobSub.CLI   = pcMSISDN NO-ERROR.

   IF NOT AVAIL MobSub THEN 
      RETURN appl_err("Subscription not available").
END.

IF (pcCustID     NE ""  AND 
    pcCustIDType EQ "") OR
   (pcCustID     EQ ""  AND 
    pcCustIDType NE "") THEN 
   RETURN appl_err("Customer ID AND Customer ID type has to be provided together").    

IF pcCustID NE "" THEN DO:
   FIND FIRST Customer NO-LOCK WHERE 
              Customer.Brand      = Syst.Var:gcBrand      AND 
              Customer.OrgID      = pcCustID     AND 
              Customer.CustIDType = pcCustIDType NO-ERROR.

   IF NOT AVAIL Customer THEN 
      RETURN appl_err("Customer is not available").
END.

IF pcDate EQ ? THEN pcDate = TODAY - 30.

ASSIGN 
   lpcDateStamp  = Func.Common:mHMS2TS(pcDate,"00:00:00")
   ldtTodayStamp = Func.Common:mHMS2TS(TODAY,"23:59:59").

/* Creating temp-table data WITH Terminal return contracts */
FOR EACH TermReturn NO-LOCK WHERE 
         TermReturn.ReturnTs >= lpcDateStamp      AND 
         TermReturn.ReturnTs <= ldtTodayStamp     AND 
         TermReturn.Salesman  = SalesMan.SalesMan AND
        (IF pcMSISDN NE "" THEN 
         TermReturn.MSISDN = MobSub.CLI 
         ELSE TRUE):

   ASSIGN 
      ldtContractDate = ?
      llgContractDate = Func.Common:mTS2Date(TermReturn.ReturnTS,
                                 ldtContractDate).
   CREATE ttContractDetails.
   ASSIGN 
      ttContractDetails.CLI          = TermReturn.MSISDN
      ttContractDetails.ContractType = "terminal_return"
      ttContractDetails.ContractId   = TermReturn.ContractId
      ttContractDetails.ContractDate = ldtContractDate.
END.

/* Creating temp-table data WITH Q25 extension contracts */

lcUserCode = "POS_" + pcSalesID.

RUN pQ25ExtensionContracts(lcUserCode).

lcUserCode = "RENEWAL_POS_" + pcSalesID.

RUN pQ25ExtensionContracts(lcUserCode).

FOR EACH TMSCodes NO-LOCK WHERE 
         TMSCodes.TableName = "Order"      AND 
         TMSCodes.FieldNAme = "StatusCode" AND 
         TMSCodes.CodeGroup = "Orders"     AND    
         TMSCodes.InUse     = 1:

   IF LOOKUP(TMSCodes.CodeValue,"6,7,8,9") > 0 THEN NEXT. 
         
   FOR EACH Order NO-LOCK USE-INDEX Salesman WHERE 
            Order.Brand      = Syst.Var:gcBrand            AND
            Order.Salesman   = Salesman.Salesman  AND 
            Order.Statuscode = TMSCodes.CodeValue AND 
            Order.OrderType  = 2                  AND 
            Order.CrStamp   >= lpcDateStamp       AND 
            Order.CrStamp   <= ldtTodayStamp      AND 
           (IF pcMSISDN NE "" THEN
            Order.MsSeq      = MobSub.MsSeq       
            ELSE TRUE)                            AND 
           (IF pcCustID NE "" THEN 
            Order.CustNum    = Customer.CustNum  
            ELSE TRUE):

      FIND FIRST OrderAction NO-LOCK WHERE 
                 OrderAction.Brand    = Syst.Var:gcBrand        AND 
                 OrderAction.OrderID  = Order.OrderID  AND 
                 OrderAction.ItemType = "Q25Extension" NO-ERROR.

      IF NOT AVAIL OrderAction THEN NEXT.           

      ASSIGN 
         ldtContractDate = ?
         llgContractDate = Func.Common:mTS2Date(Order.CrStamp,
                                    ldtContractDate).
      CREATE ttContractDetails.
      ASSIGN 
         ttContractDetails.CLI          = Order.CLI
         ttContractDetails.ContractType = "extension"
         ttContractDetails.ContractId   = OrderAction.ItemKey
         ttContractDetails.ContractDate = ldtContractDate.

   END. /* FOR EACH Order */        

END. /* FOR EACH TMSCodes */

lcTopArray = add_array(response_toplevel_id,"").

FOR EACH ttContractDetails EXCLUSIVE-LOCK 
    BREAK BY ttContractDetails.CLI:
   
   IF LAST-OF(ttContractDetails.CLI) THEN DO:
      
      IF pcMSISDN              NE ""         AND 
         ttContractDetails.CLI NE MobSub.CLI THEN NEXT.
      ELSE DO:
         FIND FIRST MobSub NO-LOCK WHERE 
                    MobSub.Brand = Syst.Var:gcBrand               AND 
                    MobSub.CLI   = ttContractDetails.CLI NO-ERROR. 
         
         IF NOT AVAIL MobSub THEN NEXT.
      END.

      IF pcCustID       NE ""               AND 
         MobSub.CustNum NE Customer.CustNum THEN NEXT.
      ELSE DO:
         FIND FIRST Customer NO-LOCK WHERE 
                    Customer.Brand   = Syst.Var:gcBrand        AND 
                    Customer.CustNum = MobSub.CustNum NO-ERROR.

         IF NOT AVAIL Customer THEN NEXT.           
      END. 
      
      ASSIGN 
         lcCustName = ""  
         lcCustName = Func.Common:mPrintCustName(BUFFER Customer).

      gcSubscriptionStruct = add_struct(lcTopArray,"").
      add_string(gcSubscriptionStruct,"cli",ttContractDetails.CLI).
      add_string(gcSubscriptionStruct,"ownerid_type",Customer.CustIdType).
      add_int(gcSubscriptionStruct,"ownerid_num",Customer.CustNum).
      add_string(gcSubscriptionStruct,"ownerid_name",lcCustName).

      lcContractsArray = add_array(gcSubscriptionStruct,"contracts_array").

      FOR EACH bttContractDetails NO-LOCK WHERE 
               bttContractDetails.CLI = ttContractDetails.CLI:
         
         gcContractsStruct = add_struct(lcContractsArray,"").
         add_string(gcContractsStruct,"contract_type",bttContractDetails.ContractType).
         add_string(gcContractsStruct,"q25_contract_id",bttContractDetails.ContractID).
         add_datetime(gcContractsStruct,"contract_date",bttContractDetails.ContractDate).

      END.          

   END.

END.

PROCEDURE pQ25ExtensionContracts:
DEFINE INPUT PARAMETER icUserCode AS CHAR NO-UNDO. 
   
   FOR EACH MsRequest NO-LOCK USE-INDEX UserCode WHERE 
            MsRequest.Brand      = Syst.Var:gcBrand          AND 
            MsRequest.UserCode   = icUserCode       AND 
            MsRequest.ActStamp  >= lpcDateStamp     AND 
            MsRequest.CreStamp  >= lpcDateStamp     AND
           (IF pcMSISDN NE "" THEN 
            MsRequest.MsSeq      = MobSub.MsSeq 
            ELSE TRUE)                              AND 
           (IF pcCustID NE "" THEN 
            MsRequest.CustNum    = Customer.CustNum 
            ELSE TRUE)                              AND 
            MsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION} AND 
           (MsRequest.ReqStatus  = 2                OR 
            MsRequest.ReqStatus  = 0)               AND 
            MsRequest.ReqCParam3 = "RVTERM12",
      FIRST SingleFee NO-LOCK USE-INDEX HostTable WHERE 
            SingleFee.Brand       = Syst.Var:gcBrand                      AND 
            SingleFee.HostTable   = "MobSub"                     AND 
            SingleFee.KeyValue    = STRING(MsRequest.MsSeq)      AND 
            SingleFee.SourceTable = "DCCLI"                      AND 
            SingleFee.SourceKey   = STRING(MsRequest.ReqIParam3) AND 
            SingleFee.CalcObj     = "RVTERM":

      ASSIGN 
         ldtContractDate = ?
         llgContractDate = Func.Common:mTS2Date(MsRequest.ActStamp,
                                    ldtContractDate).
      CREATE ttContractDetails.
      ASSIGN 
         ttContractDetails.CLI          = MsRequest.CLI
         ttContractDetails.ContractType = "extension"
         ttContractDetails.ContractId   = MsRequest.ReqCparam4
         ttContractDetails.ContractDate = ldtContractDate.
      
   END.         
END.

FINALLY:
   END.

