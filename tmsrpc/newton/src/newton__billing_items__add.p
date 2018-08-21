/**
 * Add billing item
 *
 * @input         billing_item;struct;mandatory; billing item data

 * @billing_item  brand;string;mandatory;brand to add billitem
                  username;string;mandatory;
                  id;string;mandatory;billing item  id
                  name;string;mandatory;billing item name
                  billing_group;mandatory;billing item group
                  title_es;string;optional; 
                  title_ca;string;optional;
                  title_eu;string;optional;
                  title_ga;string;optional;
                  title_en;string;optional;

 * @output        empty;struct
  
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/tmsconst.i}

DEFINE VARIABLE pcId          AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcStruct      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcTenant      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStruct      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE pcBIGroup     AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcName        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcBrand       AS CHARACTER NO-UNDO INITIAL "1".
DEFINE VARIABLE liccAcount    AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCCSAPRId    AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcBIGroupCC   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lctext        AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTaxClass    AS CHARACTER NO-UNDO. 

DEFINE VARIABLE licount      AS INTEGER NO-UNDO. 
DEFINE VARIABLE lctitle   AS CHARACTER NO-UNDO EXTENT 5 
       INITIAL ["title_es","title_ca","title_eu","title_ga","title_en"] . 

IF validate_request(param_toplevel_id, "struct") EQ ? THEN RETURN.

pcStruct = get_struct(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

lcStruct = validate_struct(pcStruct, "username!,id!,name!,billing_group!," + 
                                     "title_es,title_ca,title_eu,title_ga,title_en").
IF gi_xmlrpc_error NE 0 THEN RETURN.

pcId = get_string(pcStruct,"id").
pcBIGroup = get_string(pcStruct,"billing_group").
pcName = get_string(pcStruct,"name"). 

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF NUM-ENTRIES(pcID,"|") > 1 THEN
   ASSIGN
       pcTenant = ENTRY(2, pcId, "|")
       pcId     = ENTRY(1, pcId, "|").
ELSE
   RETURN appl_err("Invalid tenant information").

{newton/src/settenant.i pcTenant}

/*check id */
IF CAN-FIND(BillItem WHERE  BillItem.Brand = lcBrand AND
                            BillItem.BillCode = pcId) THEN
   RETURN appl_err("Billing Item with Id: " + pcId + " already exist !").

IF pcBIGroup = {&BITEM_GRP_TERMINAL} THEN DO: /* terminals */
   ASSIGN
      liccAcount = {&BITEM_GRP_TERMINAL_ACCOUNT}
      lcCCSAPRId = {&BITEM_GRP_TERMINAL_SAPRID}
      lcTaxClass = {&BITEM_GRP_TERMINAL_TAXCLASS}.
END.
ELSE DO:
   /* pick up fixed defined values in CCAdminTool */
   FIND TMSParam WHERE TMSParam.Brand = lcBrand AND
                       TMSParam.ParamGroup = "CCAdminTool" AND
                       TMSParam.ParamCode = "BillItemAccount" NO-LOCK NO-ERROR.
   IF AVAIL TMSParam THEN liccAcount = TMSParam.IntVal.
   ELSE RETURN appl_err("Billing Item Acount number not found in database !").

   FIND TMSParam WHERE TMSParam.Brand = lcBrand AND
                       TMSParam.ParamGroup = "CCAdminTool" AND
                       TMSParam.ParamCode = "BillItemSAPRId" NO-LOCK NO-ERROR.
   IF AVAIL TMSParam THEN lcCCSAPRId = TMSParam.CharVal.
   ELSE RETURN appl_err("Billing Item SAP code not found in database !").

   /* validate billing_group */
   FIND TMSParam WHERE TMSParam.Brand = lcBrand AND
                       TMSParam.ParamGroup = "CCAdminTool" AND
                       TMSParam.ParamCode = "BIGroup" NO-LOCK NO-ERROR.
   IF AVAIL TMSParam THEN lcBIGroupCC = TMSParam.CharVal.
   IF LOOKUP(pcBIGroup,lcBIGroupCC) = 0 THEN
      RETURN appl_err("Invalid billing group ! allowed values for charge/comp are: " + lcBIGroupCC ).
   lcTaxClass = "1".
END.

IF LENGTH(pcId) > 16
   THEN RETURN appl_err("Billing Item code max size exceeded").

{Syst/commpaa.i}
Syst.Var:gcBrand = lcBrand.
{Syst/eventval.i}
Syst.Var:katun = "VISTA_" + get_string(pcStruct, "username").

IF TRIM(Syst.Var:katun) EQ "VISTA_" THEN DO:
   RETURN appl_err("username is empty").
END.

/* create BillItem */
IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun 
   {Func/lib/eventlog.i}
   DEF VAR lhBillItem AS HANDLE NO-UNDO.
   lhBillItem = BUFFER BillItem:HANDLE.
   RUN StarEventInitialize(lhBillItem).
END.
 
CREATE BillItem.       
ASSIGN
    BillItem.Brand    = lcBrand
    BillItem.BillCode = pcId
    BillItem.DispMPM  = FALSE
    BillItem.BIGroup  = pcBIGroup
    BillItem.BIName   = pcName 
    BillItem.TaxClass = lcTaxClass
    NO-ERROR. 

CREATE CCRule.
ASSIGN 
    CCRule.Brand       = BillItem.Brand
    CCRule.CCRuleID    = NEXT-VALUE(CCRuleSeq)
    CCRule.Category    = "*"
    CCRule.BillCode    = BillItem.BillCode
    CCRule.ValidFrom   = TODAY
    CCRule.ValidTo     = DATE(12,31,2049)   
    CCRule.ReportingID = BillItem.SAPRid
    CCRule.AccNum      = liccAcount
    CCRule.EUAccNum    = liccAcount
    CCRule.EUConAccNum = liccAcount
    CCRule.FSAccNum    = liccAcount
    CCRule.ReportingID = lcCCSAPRId NO-ERROR. 
        
IF llDoEvent THEN DO:
   RUN StarEventMakeCreateEvent (lhBillItem).
   fCleanEventObjects().
END.

/* add translation names */
IF llDoEvent THEN DO:
   DEF VAR lhRepText AS HANDLE NO-UNDO.
   lhRepText = BUFFER RepText:HANDLE.
   RUN StarEventInitialize(lhRepText).
END.

DO licount = 1 TO 5 :

   IF LOOKUP(lctitle[licount],lcStruct) = 0 THEN NEXT.
    
   lctext = get_string(pcStruct,lctitle[licount]).
   IF lctext = "" THEN NEXT.

   CREATE RepText.
   ASSIGN RepText.Brand     = lcBrand 
          RepText.TextType  = 1  
          RepText.LinkCode  = pcId
          RepText.Language  = licount 
          RepText.ToDate    = 12/31/2049
          RepText.FromDate  = TODAY
          RepText.RepText   =  lctext.

   IF llDoEvent THEN DO:
      RUN StarEventMakeCreateEvent (lhRepText).
   END.

END.

add_struct(response_toplevel_id, "").

FINALLY:
   IF llDoEvent THEN fCleanEventObjects().
   END.
