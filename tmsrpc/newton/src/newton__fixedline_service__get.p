/**
 * List of SVAs and their status for a subscription
 *
 * @Input msseq;int;mandatory;the subscription
 * @output services;array;a list of structs
 * @service service_id;string;newton alias for a service
            value;string/int;Status of the service (on/off)
            params;struct;Optionally additional information
 */
{fcgi_agent/xmlrpc/xmlrpc_access.i}
{Syst/commpaa.i}
katun    = "NewtonAd".
gcBrand  = "1".
{Syst/tmsconst.i}
{Func/vasfunc.i}
/* Input parameters */
DEF VAR piMsSeq AS INT NO-UNDO.

/* Local variables */
DEF VAR lii             AS INT NO-UNDO.

/* Output parameters */
DEF VAR top_array       AS CHAR NO-UNDO.
DEF VAR top_struct      AS CHAR NO-UNDO.
DEF VAR params_struct   AS CHAR NO-UNDO.

DEF VAR llgSVA        AS LOGICAL NO-UNDO.
DEF VAR liParams      AS INT     NO-UNDO.
DEF VAR ldPrice       AS DECIMAL NO-UNDO.
DEF VAR liServStatus  AS INT     NO-UNDO.

DEFINE BUFFER bf_TPService_Deactivation FOR TPService.

FUNCTION fGetSVAStatus RETURNS INTEGER
  (iiMsSeq   AS INT,
   icDCEvent AS CHAR):

  DEF VAR liStatus AS INT     NO-UNDO.

  FIND FIRST MsRequest NO-LOCK WHERE
             MsRequest.MsSeq      EQ piMsSeq                           AND
             (MsRequest.ReqType   EQ {&REQTYPE_CONTRACT_ACTIVATION} OR
             MsRequest.ReqType    EQ {&REQTYPE_CONTRACT_TERMINATION} ) AND
             MsRequest.ReqCparam3 EQ icDCEvent                         AND
             MsRequest.ReqStatus  NE {&REQUEST_STATUS_CANCELLED}       AND
             MsRequest.ReqStatus  NE {&REQUEST_STATUS_HANDLED}         USE-INDEX MsActStamp NO-ERROR.
  IF AVAIL MsRequest THEN 
  DO:
     IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} AND MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} THEN
        liStatus = 1. /* activation done*/
     ELSE IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} AND MsRequest.ReqStatus EQ {&REQUEST_STATUS_DONE} THEN
        liStatus = 0. /* Incative */ 
     /* Show pending when operation is ongoing or pending status */
     ELSE IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_ACTIVATION} THEN
        liStatus = 2. /*Pending activation*/
     ELSE IF MsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION} THEN
        liStatus = 3. /*Pending deactivation*/
     ELSE 
        liStatus = 0. /*Inactive*/
  END.
  ELSE 
      liStatus = 0. /*Inactive, never requested activation*/

  RETURN liStatus.
      
END FUNCTION.  

IF validate_request(param_toplevel_id, "int") EQ ? THEN 
    RETURN.

piMsSeq = get_pos_int(param_toplevel_id, "0").

IF gi_xmlrpc_error NE 0 THEN RETURN.

FIND FIRST MobSub WHERE MobSub.MsSeq = piMsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN
    RETURN appl_err(SUBST("MobSub entry &1 not found", piMsSeq)).

top_array = add_array(response_toplevel_id, "").

FOR EACH daycampaign NO-LOCK:
   liParams = 0.
   llgSVA = fIsSVA(daycampaign.dcevent, liParams).

   IF llgSVA THEN 
   DO:
      FIND FIRST FMItem WHERE FMItem.Brand     EQ gcBrand              AND 
                              FMItem.FeeModel  EQ DayCampaign.FeeModel AND 
                              FMItem.BillCode  <> ""                   AND 
                              FMItem.PriceList <> ""                   AND
                              FMItem.FromDate  <= TODAY                AND 
                              FMItem.ToDate    >= TODAY                NO-LOCK NO-ERROR.
      IF AVAIL FMItem THEN 
          ldPrice = FMItem.Amount.
      ELSE 
          ldPrice = 0.

      top_struct = add_struct(top_array, "").
      add_string(top_struct, "service_id", daycampaign.dcevent).    
      add_double(top_struct, "price"   , ldPrice).
      add_string(top_struct, "status"  , STRING(fGetSVAStatus(piMsSeq, daycampaign.dcevent))).
      add_string(top_struct, "category", "pro").
   END.
   ELSE IF DayCampaign.BundleTarget = {&TELEVISION_BUNDLE} THEN 
   DO:
       FIND FIRST TPService WHERE TPService.MsSeq      = piMsSeq             AND 
                                  TPService.Operation  = {&TYPE_ACTIVATION}  AND 
                                  TPService.ServType   = "Television"        AND 
                                  TPService.ServStatus > ""                  AND 
                                  TPService.Product    = DayCampaign.DCEvent NO-LOCK
                                  USE-INDEX MsSeqTypeStatus NO-ERROR.
       IF NOT AVAIL TPService THEN 
           ASSIGN liServStatus = 0. /* Inactive */
       ELSE 
       DO:    
           IF TPService.ServStatus = {&STATUS_HANDLED} THEN 
               ASSIGN liServStatus = 1.  /* Activation Done*/ 
           ELSE IF LOOKUP(TPService.ServStatus, {&STATUS_CANCELED} + "," + {&STATUS_ERROR}) > 0 THEN
              ASSIGN liServStatus = 0. /* Inactive */         
           ELSE     
               ASSIGN liServStatus = 2.  /*Pending activation*/

           FIND FIRST bf_TPService_Deactivation WHERE bf_TPService_Deactivation.MsSeq       = piMsSeq              AND 
                                                      bf_TPService_Deactivation.Operation   = {&TYPE_DEACTIVATION} AND 
                                                      bf_TPService_Deactivation.ServType    = "Television"         AND 
                                                      bf_TPService_Deactivation.ServStatus  > ""                   AND 
                                                      bf_TPService_Deactivation.CreatedTS   > TPService.CreatedTS  AND 
                                                      bf_TPService_Deactivation.Product     = DayCampaign.DCEvent  NO-LOCK
                                                      USE-INDEX MsSeqTypeStatus NO-ERROR.
           IF AVAIL bf_TPService_Deactivation THEN 
           DO:
               IF bf_TPService_Deactivation.ServStatus = {&STATUS_HANDLED} THEN 
                   ASSIGN liServStatus = 0. /* 'InActive' */
               ELSE IF bf_TPService_Deactivation.ServStatus = {&STATUS_ERROR} THEN
                   ASSIGN liServStatus = 1. /* Deactivation cancelled, so service is still 'Active' */
               ELSE 
                   ASSIGN liServStatus = 3. /*Pending deactivation*/
           END.
       END.

       FIND FIRST FMItem WHERE FMItem.Brand     EQ gcBrand              AND 
                               FMItem.FeeModel  EQ DayCampaign.FeeModel AND 
                               FMItem.BillCode  <> ""                   AND 
                               FMItem.PriceList <> ""                   AND
                               FMItem.FromDate  <= TODAY                AND 
                               FMItem.ToDate    >= TODAY                NO-LOCK NO-ERROR.
       IF AVAIL FMItem THEN 
           ldPrice = FMItem.Amount.
       ELSE 
           ldPrice = 0.

       top_struct = add_struct(top_array, "").
       add_string(top_struct, "service_id", Daycampaign.DCEvent).    
       add_double(top_struct, "price"     , ldPrice).
       add_string(top_struct, "status"    , STRING(liServStatus)).
       add_string(top_struct, "category"  , "tv").
   END.
END.

FINALLY:
   IF VALID-HANDLE(ghFunc1) THEN 
      DELETE OBJECT ghFunc1 NO-ERROR.
END.
          
