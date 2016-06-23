/**
 * Add charge events
 *
 * @input         charge_event;struct;mandatory; charge event data

 * @charge_event  username;string;mandatory;
                  id;string;mandatory; charge event id
                  name;string;optional; charge event name
                  amount;decimal;mandatory; 
                  billing_item_id;mandatory; billing item id
                  paytype;string;mandatory; prepaid or postpaid 
                  valid_from;date;mandatory;
                  valid_to;date;optional;
 * @output        empty;
  
*/


{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEFINE VARIABLE pcStruct AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStruct AS CHARACTER NO-UNDO.  
DEFINE VARIABLE ocError AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcId AS CHAR NO-UNDO. 
DEFINE VARIABLE pdAmt AS DECIMAL NO-UNDO.
DEFINE VARIABLE pcBItem AS CHAR NO-UNDO.
DEFINE VARIABLE pcPayType AS CHAR NO-UNDO.
DEFINE VARIABLE pdFromDate AS DATE NO-UNDO.
DEFINE VARIABLE pdToDate AS DATE NO-UNDO INITIAL 12/31/2049. 
DEFINE VARIABLE lcBrand  AS CHAR NO-UNDO INITIAL "1".

DEFINE TEMP-TABLE ttFeeModel NO-UNDO LIKE FeeModel.
DEFINE TEMP-TABLE ttFMItem NO-UNDO LIKE FMItem.

IF validate_request(param_toplevel_id, "struct") = ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").
IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_request(pcStruct, 
   "username!,id!,name,amount!,billing_item_id!,paytype!,valid_from!,valid_to").
 
IF lcStruct = ? THEN RETURN.

IF gi_xmlrpc_error NE 0 THEN RETURN.

pcId       = get_string(pcStruct,"id").
pdAmt      = get_double(pcStruct,"amount").
pcBItem    = get_string(pcStruct,"billing_item_id").
pcPayType  = get_string(pcStruct,"paytype").
pdFromDate = get_date(pcStruct,"valid_from").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF pdAmt = 0 THEN 
RETURN appl_err("Amount can not be zero").


IF LOOKUP("valid_to",lcStruct) > 0 THEN DO:

   pdToDate   = get_date(pcStruct,"valid_to").
   /* allow empty string and convert it to default value */
   IF gi_xmlrpc_error = {&INVALID_METHOD_PARAMS} THEN DO:
      gi_xmlrpc_error = 0.
      gc_xmlrpc_error = "".
     IF get_string(pcStruct,"valid_to") EQ "" THEN DO:
        pdToDate = 12/31/2049. 
     END.
     ELSE RETURN
        appl_err("Incorrect valid_to value: " + get_string(pcStruct,"valid_to")).
   END.

   IF pdFromDate > pdToDate THEN 
   RETURN appl_err("Valid To must be later than Valid From ").

END.

/* check FeeModel */
IF CAN-FIND (FIRST FeeModel WHERE
                   FeeModel.Brand = lcBrand AND
                   FeeModel.FeeModel = pcId ) THEN 
   RETURN appl_err("Billing Event " + pcId + " already exist !").

CREATE ttFeeModel.
ASSIGN 
      ttFeeModel.Brand = lcBrand
      ttFeeModel.FeeModel = pcId
      ttFeeModel.FMGroup  = 1.

IF LOOKUP("name",lcStruct) > 0 THEN 
      ttFeeModel.FeeName  = get_string(pcStruct,"name"). 


/* check invalid paytype */
DEFINE VARIABLE lcPriceList AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBIGroup AS CHARACTER NO-UNDO.

FIND TMSParam WHERE TMSParam.Brand = lcBrand AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "FMItemPriceList" + pcPayType NO-LOCK NO-ERROR.  
IF AVAIL TMSParam THEN lcPriceList = TMSParam.CharVal.
ELSE RETURN appl_err("Invalid paytype !").

/* pick up BIGroup */
FIND TMSParam WHERE TMSParam.Brand = lcBrand AND
                    TMSParam.ParamGroup = "CCAdminTool" AND
                    TMSParam.ParamCode = "BIGroup" NO-LOCK NO-ERROR.
IF AVAIL TMSParam THEN lcBIGroup = TMSParam.CharVal.

/* check billing item */
FIND BillItem WHERE BillItem.Brand = lcBrand AND
                    BillItem.BillCode = pcBItem AND
                    LOOKUP(BillItem.BIGroup,lcBIGroup) > 0 NO-LOCK NO-ERROR.
IF NOT AVAIL BillItem THEN 
RETURN appl_err("Can not find billing item with id " + pcBItem + " in charge/comp billing item groups").

/* avoid postpaid compensations */
IF BillItem.BIGroup = {&BITEM_GRP_COMPENSATION} AND 
   pcPayType = "postpaid" THEN 
RETURN appl_err("Forbiden to create compensation events for postpaid").


CREATE ttFMItem. 
ASSIGN 
      ttFMItem.Brand     = ttFeeModel.Brand
      ttFMItem.FeeModel  = ttFeeModel.FeeModel
      ttFMItem.PriceList = lcPriceList
      ttFMItem.BillCode  = BillItem.BillCode
      ttFMItem.Amount    = pdAmt
      ttFMItem.FromDate  = pdFromDate
      ttFMItem.ToDate    = pdToDate 
      ttFMItem.BillMethod = TRUE 
      ttFMItem.BillType  = "CC".

{Syst/commpaa.i}
gcBrand = lcBrand.
{Syst/eventval.i}
katun = "VISTA_" + get_string(pcStruct, "username").

IF TRIM(katun) EQ "VISTA_" THEN DO:
   RETURN appl_err("username is empty").
END.

/* create FeeModel */
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun 
   {Func/lib/eventlog.i}
   DEF VAR lhFeeModel AS HANDLE NO-UNDO.
   lhFeeModel = BUFFER FeeModel:HANDLE.
   RUN StarEventInitialize(lhFeeModel).
END.

CREATE FeeModel.
BUFFER-COPY ttFeeModel TO FeeModel.

IF llDoEvent THEN DO:
   RUN StarEventMakeCreateEvent (lhFeeModel).
   fCleanEventObjects().
END.

/* create FMItem */
IF llDoEvent THEN DO:
   DEF VAR lhFMItem AS HANDLE NO-UNDO.
   lhFMItem = BUFFER FMItem:HANDLE.
   RUN StarEventInitialize(lhFMItem).
END.

CREATE FMItem.
BUFFER-COPY ttFMItem TO FMItem.

IF llDoEvent THEN DO:
   RUN StarEventMakeCreateEvent (lhFMItem).
   fCleanEventObjects().
END.

add_struct(response_toplevel_id, "").

FINALLY:
   EMPTY TEMP-TABLE ttFeeModel.
   EMPTY TEMP-TABLE ttFMItem.
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
