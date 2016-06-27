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
       cust_id_num;int;optional
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

{xmlrpc/xmlrpc_access.i}

{commpaa.i}
katun = "NewtonRPC".
gcBrand = "1".
{timestamp.i}
{tmsconst.i}
 
DEF VAR pcMSISDN     AS CHAR NO-UNDO. 
DEF VAR pcCustIDType AS CHAR NO-UNDO. 
DEF VAR pcCustIDNum  AS INT  NO-UNDO. 
DEF VAR pcSalesID    AS CHAR NO-UNDO. 
DEF VAR pcDate       AS DATE NO-UNDO.

DEF VAR top_struct           AS CHAR NO-UNDO.  
DEF VAR lpcDateStamp         AS DEC  NO-UNDO.
DEF VAR ldtTodayStamp        AS DEC  NO-UNDO.
DEF VAR lcContractsDetails   AS CHAR NO-UNDO. 
DEF VAR lcSubsDetails        AS CHAR NO-UNDO. 
DEF VAR lcCustName           AS CHAR NO-UNDO. 
DEF VAR ldtTermReturnDate    AS DATE NO-UNDO.
DEF VAR llgTermReturnDate    AS LOG  NO-UNDO.
DEF VAR gcSubscriptionStruct AS CHAR NO-UNDO. 
DEF VAR gcContractsStruct    AS CHAR NO-UNDO. 

DEFINE BUFFER bMsRequest FOR MsRequest.

IF validate_request(param_toplevel_id,"string,string,int,string,date") EQ ? THEN
   RETURN.

ASSIGN 
   pcMSISDN     = get_string(param_toplevel_id,"0")
   pcCustIDType = get_string(param_toplevel_id,"1")
   pcCustIDNum  = get_int(param_toplevel_id,"2")   
   pcSalesID    = get_string(param_toplevel_id,"3")
   pcDate       = get_date(param_toplevel_id,"4").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF TRIM(pcSalesID) EQ "" THEN  
   RETURN appl_err("Sales ID is empty").

FIND SalesMan NO-LOCK WHERE 
     SalesMan.Brand    = gcBrand   AND 
     SalesMan.Salesman = pcSalesID NO-ERROR.

IF NOT AVAIL SalesMan THEN 
   RETURN appl_err("Salesman is not available"). 

IF pcMSISDN    NE "" AND 
   pcCustIDNum NE 0  THEN 
   RETURN appl_err("Both MSISDN AND Customer ID are not allowed").

IF pcMSISDN NE "" THEN DO:
   FIND FIRST MobSub NO-LOCK WHERE 
              MobSub.Brand = gcBrand  AND 
              MobSub.CLI   = pcMSISDN NO-ERROR.

   IF NOT AVAIL MobSub THEN 
      RETURN appl_err("MSISDN not available").
END.

IF pcCustIDNum NE 0 THEN DO:
   FIND FIRST Customer NO-LOCK WHERE 
              Customer.CustNum = pcCustIDNum NO-ERROR.

   IF NOT AVAIL Customer THEN 
      RETURN appl_err("Customer ID is not available").

END.

IF pcCustIDType NE "" THEN DO:
   FIND FIRST Customer NO-LOCK WHERE 
              Customer.Brand      = gcBrand      AND 
              Customer.CustIdType = pcCustIDType NO-ERROR.
    
   IF NOT AVAIL Customer THEN 
      RETURN appl_err("Customer ID Type not available").
END.

IF pcDate EQ ? THEN pcDate = TODAY - 30.

ASSIGN 
   lpcDateStamp  = fHMS2TS(pcDate,"00:00:00")
   ldtTodayStamp = fHMS2TS(TODAY,"23:59:59").

top_struct = add_struct(response_toplevel_id,"").

FOR EACH Order NO-LOCK WHERE 
         Order.Brand    = gcBrand      AND
         Order.Salesman = pcSalesID    AND 
         Order.CrStamp >= lpcDateStamp AND 
         Order.CrStamp <= ldtTodayStamp:
  
   IF pcMSISDN NE "" AND Order.CLI NE pcMSISDN THEN NEXT.

   IF pcCustIDNum NE 0 AND Order.CustNum NE pcCustIDNum THEN NEXT.
   
   FIND FIRST Customer NO-LOCK WHERE 
              Customer.Brand   = gcBrand       AND 
              Customer.CustNum = Order.CustNum NO-ERROR.

   IF pcCustIDType NE "" AND 
      pcCustIDType NE Customer.CustIdType THEN NEXT.           

   lcSubsDetails = add_array(top_struct,"subscription_struct").

   lcCustName = DYNAMIC-FUNCTION("fPrintCustName" IN ghFunc1,
                                  BUFFER Customer).

   gcSubscriptionStruct = add_struct(lcSubsDetails,"").
   add_string(gcSubscriptionStruct,"cli",Order.CLI).
   add_string(gcSubscriptionStruct,"ownerid_type",Customer.CustIdType).
   add_int(gcSubscriptionStruct,"ownerid_num",Order.CustNum).
   add_string(gcSubscriptionStruct,"ownerid_name",lcCustName).

   lcContractsDetails = add_array(top_struct,"contracts_struct").

   FOR EACH TermReturn NO-LOCK WHERE
            TermReturn.OrderId = Order.OrderId AND
          ((TermReturn.DeviceScreen = TRUE AND TermReturn.DeviceStart  = TRUE) OR
           (TermReturn.DeviceScreen = ?    AND TermReturn.DeviceStart  = ?)):
     
      ASSIGN 
         ldtTermReturnDate = ?
         llgTermReturnDate = fTS2Date(TermReturn.ReturnTS,
                                      ldtTermReturnDate).

      gcContractsStruct = add_struct(lcContractsDetails,"").
      add_string(gcContractsStruct,"contract_type","terminal_return").
      add_string(gcContractsStruct,"q25_contract_id",TermReturn.ContractId).
      add_datetime(gcContractsStruct,"contract_date",ldtTermReturnDate).
      
   END.
   
   FOR EACH DCCLI NO-LOCK WHERE
            DCCLI.Brand    = gcBrand     AND
            DCCLI.DCEvent  = "RVTERM12"  AND
            DCCLI.MsSeq    = Order.MsSeq AND
            DCCLI.ValidTo >= TODAY,
      EACH bMsRequest NO-LOCK USE-INDEX MsActStamp WHERE
           bMsRequest.MsSeq      = DCCLI.MsSeq                     AND
           bMsRequest.ReqType    = {&REQTYPE_CONTRACT_ACTIVATION}  AND
           bMsRequest.ReqStat    = 2                               AND
           bMsRequest.ActStamp  >= fMake2Dt(DCCLI.ValidFrom,0)     AND
           bMsRequest.ActStamp  <= fMake2Dt(DCCLI.ValidFrom,86399) AND
           bMsRequest.Reqcparam3 = "RVTERM12",
      FIRST SingleFee NO-LOCK USE-INDEX Custnum WHERE
            SingleFee.Brand       = gcBrand                       AND
            SingleFee.Custnum     = Order.CustNum                 AND
            SingleFee.HostTable   = "Mobsub"                      AND
            SingleFee.KeyValue    = STRING(Order.MsSeq)           AND
            SingleFee.SourceTable = "DCCLI"                       AND
            SingleFee.SourceKey   = STRING(bMsRequest.ReqIParam3) AND
            SingleFee.CalcObj     = "RVTERM"                      AND
            SingleFee.OrderId     = Order.OrderId:
  
      gcContractsStruct = add_struct(lcContractsDetails,"").
      add_string(gcContractsStruct,"contract_type","extension").
      add_string(gcContractsStruct,"q25_contract_id",bMsRequest.ReqCparam4).
      add_datetime(gcContractsStruct,"contract_date",DCCLI.ValidFrom).
        
   END.

END.        

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.

