/**
   Set charge event
   @ MsSeq;mandatory;string;Main line 
     Offer;mandatory;string;Offer 
     User;mandatory;string;User activated
     charge_event;mandatory;struct
   @ charge_event    username;mandatory;
                     name;optional;
                     valid_to;date;optional; 
                     amount;double;optional; 
*/

{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Func/orderfunc.i}

DEFINE VARIABLE piMsSeq       AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcBundle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcOfferId     AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserCode    AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcBundleType  AS CHARACTER NO-UNDO.

IF validate_request(param_toplevel_id, "int,string,string") EQ ? THEN RETURN.

piMsSeq    = get_int   (param_toplevel_id, "0").
pcBundle   = get_string(param_toplevel_id, "1").
pcOfferId  = get_string(param_toplevel_id, "2").
pcUserCode = get_string(param_toplevel_id, "3").

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF piMsSeq    = 0  OR
   pcBundle   = "" OR 
   pcOfferId  = "" OR 
   pcUserCode = "" THEN 
   RETURN appl_err("Invalid parameters").

FIND FIRST DayCampaign WHERE DayCampaign.Brand = gcBrand AND DayCampaign.DCEvent = pcBundle NO-LOCK NO-ERROR.
IF NOT AVAILABLE DayCampaign THEN 
    RETURN appl_err("Invalid bundle").

ASSIGN lcBundleType = (IF DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} THEN "Television" ELSE "").

fCreateTPService(piMsSeq, pcBundle, lcBundleType, pcOfferId, pcUserCode).

fInitiate_ThirdParty_BB_Service_STB_Logistics(piMsSeq).

add_struct(response_toplevel_id, "", True).

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
