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

DEFINE VARIABLE piMsSeq       AS INTEGER   NO-UNDO.
DEFINE VARIABLE pcId          AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcBundle      AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcOfferId     AS CHARACTER NO-UNDO.
DEFINE VARIABLE pcUserCode    AS CHARACTER NO-UNDO.
DEFINE VARIABLE piActOrDeAct  AS INTEGER   NO-UNDO.

DEFINE VARIABLE lcBundleType  AS CHARACTER NO-UNDO.
IF validate_request(param_toplevel_id, "string,string,string,int") EQ ? THEN 
    RETURN.

pcId         = get_string(param_toplevel_id, "0").
pcOfferId    = get_string(param_toplevel_id, "1").
pcUserCode   = get_string(param_toplevel_id, "2").
piActOrDeAct = get_int   (param_toplevel_id, "3").

pcBundle   =     ENTRY(1,pcId,"|").
piMsSeq    = INT(ENTRY(2,pcId,"|")) NO-ERROR.

IF gi_xmlrpc_error NE 0 THEN RETURN.

IF piMsSeq    = 0  OR
   pcBundle   = "" OR  
   pcUserCode = "" THEN 
   RETURN appl_err("Invalid parameters").
ELSE IF piActOrDeAct = 1 AND pcOfferId  = "" THEN
   RETURN appl_err("Invalid parameters"). 

FIND FIRST DayCampaign WHERE DayCampaign.Brand = gcBrand AND DayCampaign.DCEvent = pcBundle NO-LOCK NO-ERROR.
IF NOT AVAILABLE DayCampaign OR DayCampaign.BundleTarget <> {&TELEVISION_BUNDLE} THEN 
    RETURN appl_err("Invalid tv bundle").

ASSIGN lcBundleType = (IF DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} THEN "Television" ELSE "").

CASE piActOrDeAct:
    WHEN 0 THEN 
        RUN pDeActivateTVService.
    WHEN 1 THEN
        RUN pActivateTVService.
END CASE.

IF RETURN-VALUE = "" THEN 
    add_boolean(response_toplevel_id, "", True).
ELSE
    RETURN appl_err(RETURN-VALUE).

PROCEDURE pDeActivateTVService:
    DEFINE VARIABLE liRequest   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ldeActStamp AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lcResult    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liServSeq   AS INTEGER   NO-UNDO.

    ASSIGN ldeActStamp = fMakeTS().

    FIND FIRST TPService WHERE TPService.MsSeq = piMsSeq AND TPService.Operation = {&TYPE_ACTIVATION} AND TPService.ServType = lcBundleType NO-LOCK NO-ERROR.
    IF AVAIL TPService THEN
    DO:
        IF LOOKUP(TPService.ServStatus,"HANDLED") > 0 THEN 
        DO:
            ASSIGN liServSeq = fCreateNewTPService(piMsSeq, 
                                                   pcBundle, 
                                                   "Huawei", 
                                                   lcBundleType, 
                                                   {&TYPE_DEACTIVATION}, 
                                                   {&STATUS_NEW}, 
                                                   pcOfferId, 
                                                   pcUserCode).

            IF liServSeq > 0 THEN 
            DO:
                fCreateTPServiceMessage(piMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).

                fCreateTPServiceMessage(piMsSeq, liServSeq , {&SOURCE_TMS}, {&WAITING_FOR_STB_DEACTIVATION}).
            END.    
        END.
        ELSE
        DO:
            IF TPService.ServStatus = {&STATUS_LOGISTICS_INITIATED}              OR
               TPService.ServStatus = {&WAITING_FOR_STB_ACTIVATION}              OR
               TPService.ServStatus = {&WAITING_FOR_STB_ACTIVATION_CONFIRMATION} THEN 
                RETURN "Setup box logistics/activation process is already initiated. Cancellation not allowed now.". 
        
            ASSIGN liServSeq = fCreateNewTPService(piMsSeq, 
                                                   pcBundle, 
                                                   "Huawei", 
                                                   lcBundleType, 
                                                   {&TYPE_DEACTIVATION}, 
                                                   {&STATUS_NEW}, 
                                                   pcOfferId, 
                                                   pcUserCode).

            IF liServSeq > 0 THEN 
            DO:    
                fCreateTPServiceMessage(piMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).

                fCreateTPServiceMessage(piMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_CANCELED}).
            END.                                  
        END.
    END.
    ELSE 
        RETURN appl_err("No tv service active for cancellation").

    RETURN "".

END PROCEDURE.

PROCEDURE pActivateTVService:
    DEF VAR liServSeq AS INT NO-UNDO.

    FIND FIRST TPService WHERE TPService.MsSeq = piMsSeq AND TPService.ServType = lcBundleType AND TPService.ServStatus <> "HANDLED" NO-LOCK NO-ERROR.
    IF AVAIL TPService THEN 
        RETURN "There exists an ongoing tv service request.".

    ASSIGN liServSeq = fCreateNewTPService(piMsSeq, 
                                           pcBundle, 
                                           "Huawei", 
                                           lcBundleType, 
                                           {&TYPE_ACTIVATION}, 
                                           {&STATUS_NEW}, 
                                           pcOfferId, 
                                           pcUserCode).
    
    IF liServSeq > 0 THEN 
        fCreateTPServiceMessage(piMsSeq, liServSeq , {&SOURCE_TMS}, {&STATUS_NEW}).

    RETURN "".

END PROCEDURE.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR. 
END.
