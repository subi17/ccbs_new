ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.DayCampaignModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR loRequestMaker  AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
DEF VAR loRepTextMaker  AS Gwy.RepTextMaker NO-UNDO.
DEF VAR lcError         AS CHAR NO-UNDO. 
    
DEF TEMP-TABLE ttDayCampaign NO-UNDO SERIALIZE-NAME "DayCampaign"
   FIELD external_id AS CHAR 
   FIELD name AS CHAR 
   FIELD valid_from AS CHAR
   FIELD valid_to AS CHAR 
   FIELD target AS CHAR INIT ?
   FIELD package_type AS CHAR INIT ?
   FIELD instance_limit AS INT INIT ?
   FIELD limit AS DEC INIT ?
   FIELD limit_unit AS CHAR INIT ?
   FIELD limit_rule_id AS CHAR INIT ? 
   FIELD first_limit AS CHAR INIT ?
   FIELD billing_item AS CHAR INIT ?
   FIELD call_case AS INT INIT ?
   FIELD calc_method AS CHAR INIT ?
   FIELD effective AS CHAR INIT ?
   FIELD weekday AS CHAR INIT ?
   FIELD duration_type AS CHAR INIT ?
   FIELD duration AS INT INIT ?
   FIELD duration_unit AS CHAR INIT ?
   FIELD fee_id AS CHAR INIT ?
   FIELD dc_validfrom AS DATE
   FIELD dc_validto AS DATE
   FIELD dc_target AS CHAR INIT ?
   FIELD dc_packagetype AS CHAR INIT ?
   FIELD dc_limitunit AS INT INIT ?
   FIELD dc_limitruleid AS INT INIT ? 
   FIELD dc_firstlimit AS INT INIT ?
   FIELD dc_calcmethod AS INT INIT ?
   FIELD dc_effective AS INT INIT ?
   FIELD dc_durationtype AS INT INIT ?
   FIELD dc_durationunit AS INT INIT ?.

{Gwy/RepText_definition.i}

DEFINE DATASET DayCampaignDataSet FOR ttDayCampaign, ttRepText
   PARENT-ID-RELATION idparent FOR ttDayCampaign, ttRepText
      PARENT-ID-FIELD parent_id.


CONSTRUCTOR PUBLIC DayCampaignModel ( iJson    AS LONGCHAR,
                                  iModelid AS CHAR, 
                                  iSource  AS CHAR,
                                  iCorrelationID AS CHAR,
                                  iReplyTo AS CHAR):
   ASSIGN
      gcBrand        = "1"
      lJson          = iJson
      lModelid       = iModelid
      lCorrelationID = iCorrelationID
      loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
      loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
      loRepTextMaker  = NEW Gwy.RepTextMaker(iSource).
   
END CONSTRUCTOR.

METHOD PUBLIC LOG create():
 
   lJson = '~{"DayCampaignDataSet":~{"DayCampaign":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET DayCampaignDataset BIND, lJson) THEN
       RETURN FALSE.
   
   FIND ttDayCampaign NO-ERROR.
   IF NOT AVAILABLE ttDayCampaign THEN
       RETURN loRequestMaker:logError("Empty package set").

   IF ttDayCampaign.external_id = "" THEN 
      RETURN loRequestMaker:logError("Invalid package ID").
        
   lcError = validate_daycampaign(ttDayCampaign.external_id,
                                  TRUE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                             14,
                                             ttDayCampaign.external_id,
                                             RECID(ttDayCampaign)).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
      
   CREATE DayCampaign.
   ASSIGN 
      DayCampaign.Brand          = gcBrand
      DayCampaign.DCEvent        = ttDayCampaign.external_id.
      
   update_daycampaign().
   
   loEventLogMaker:make_eventlog("create",BUFFER DayCampaign:HANDLE).

   lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                           14,
                                           DayCampaign.DCEvent,
                                           RECID(ttDayCampaign)). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
     
   RELEASE DayCampaign.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("DayCampaign",
                                  ttDayCampaign.external_id,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("DayCampaign create: Id=&1 Ext.Request=&2", 
                 ttDayCampaign.external_id,
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():

   DEF VAR liWait AS INT  NO-UNDO.
   
   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('Package ID not given').

   lJson = '~{"DayCampaignDataSet":~{"DayCampaign":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET DayCampaignDataset BIND, lJson) THEN
       RETURN FALSE.
   
   IF lModelID = "" THEN 
      RETURN loRequestMaker:logError("Invalid package ID").
      
   FIND ttDayCampaign NO-ERROR.
   IF NOT AVAILABLE ttDayCampaign THEN
       RETURN loRequestMaker:logError("Empty package set").

   lcError = validate_daycampaign(lModelID,
                                  FALSE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   DO WHILE TRUE:
      FIND FIRST DayCampaign WHERE 
                 DayCampaign.Brand = gcBrand AND
                 DayCampaign.DCEvent = lModelID EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED(DayCampaign) THEN DO:
         liWait = liWait + 1.
         IF liWait > {&WAIT_CONFIG_LOCK} THEN 
            RETURN loRequestMaker:logError(
                      "Package was not available for update").
         PAUSE 1 NO-MESSAGE. 
         NEXT. 
      END.
      LEAVE.
   END.

   IF NOT AVAILABLE DayCampaign THEN 
      RETURN loRequestMaker:logError("Unknown package").

   lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                             14,
                                             DayCampaign.DCEvent,
                                             RECID(ttDayCampaign)).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
       
   loEventLogMaker:make_eventlog("oldbuffer",BUFFER DayCampaign:HANDLE).

   update_daycampaign().
    
   loEventLogMaker:make_eventlog("modify",BUFFER DayCampaign:HANDLE).

   lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                           14,
                                           DayCampaign.DCEvent,
                                           RECID(ttDayCampaign)). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   RELEASE DayCampaign.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("DayCampaign",
                                  lModelId,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("DayCampaign update: Id=&1 Ext.Request=&2", 
                 lModelId,
                 lCorrelationID), 
      "DEBUG").
       
END METHOD.

METHOD PUBLIC LOG makeOtherAction(icAction AS CHAR):
   CASE icAction:
      WHEN "update_other" THEN process_other_update().
      OTHERWISE 
         RETURN loRequestMaker:logError(SUBST("Unknown action &1",icAction)).
   END CASE.
END METHOD.

METHOD PUBLIC LOG process_other_update():

END METHOD.

METHOD PUBLIC CHAR validate_daycampaign
   (icDayCampaign AS CHAR,
    ilCreate AS LOG):
    
   DEF VAR liValue AS INT  NO-UNDO. 
   DEF VAR lcValue AS CHAR NO-UNDO. 

   IF LENGTH(icDayCampaign) > {&CONFIG_ID_MAX} THEN 
      RETURN "Max length for ID is {&CONFIG_ID_MAX} characters".
 
   IF CAN-FIND(FIRST DayCampaign WHERE 
                     DayCampaign.Brand = gcBrand AND
                     DayCampaign.DCEvent = icDayCampaign) THEN DO:
      IF ilCreate THEN RETURN "Package already exists".
   END.
   ELSE IF NOT ilCreate THEN RETURN "Unknown package".

   IF ilCreate AND ttDayCampaign.name = "" THEN RETURN "Name is missing".
   
   IF ttDayCampaign.valid_from > "" THEN 
      ttDayCampaign.dc_validfrom = loRequestMaker:make_date(
                              loRequestMaker:make_timestamp(ttDayCampaign.valid_from))
      NO-ERROR.                           
   IF ttDayCampaign.valid_to > "" THEN                              
      ttDayCampaign.dc_validto  = loRequestMaker:make_date(
                              loRequestMaker:make_timestamp(ttDayCampaign.valid_to))
      NO-ERROR.
   IF ilCreate AND (ttDayCampaign.dc_validfrom = ? OR ttDayCampaign.dc_validto = ?) THEN 
      RETURN "Invalid package period". 

   IF (ilCreate OR ttDayCampaign.package_type > "") THEN DO:
      ttDayCampaign.dc_packagetype = loRequestMaker:get_tmscodes_bssapi_internalid(
                                          "DayCampaign",
                                          "DCType",
                                          ttDayCampaign.package_type).
      IF ttDayCampaign.dc_packagetype = "" THEN RETURN "Invalid package type".
   END.
   
   IF (ilCreate OR ttDayCampaign.instance_limit NE ?) THEN DO:
      IF ttDayCampaign.instance_limit = 0 OR 
         ttDayCampaign.instance_limit > 99 THEN 
            RETURN "Invalid instance limit".
   END.

   IF ttDayCampaign.limit_rule_id NE ? THEN DO:
      ttDayCampaign.dc_limitruleid = INT(ttDayCampaign.limit_rule_id) NO-ERROR.
      IF ttDayCampaign.dc_limitruleid = 0 OR 
         NOT CAN-FIND(FIRST TMRule WHERE 
                         TMRule.TMRuleSeq = ttDayCampaign.dc_limitruleid) THEN 
            RETURN "Invalid limit rule id".
   END.
   
   IF (ilCreate OR ttDayCampaign.target > "") THEN DO:
      ttDayCampaign.dc_target = loRequestMaker:get_tmscodes_bssapi_internalid(
                                          "DayCampaign",
                                          "DCTarget",
                                          ttDayCampaign.target).
      IF ttDayCampaign.dc_target = "" THEN RETURN "Invalid package target".
   END.
   
   IF ttDayCampaign.limit_unit > "" THEN DO:
      ttDayCampaign.dc_limitunit = INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                                          "DayCampaign",
                                          "InclUnit",
                                          ttDayCampaign.limit_unit)) NO-ERROR.
      IF ttDayCampaign.dc_limitunit = 0 THEN RETURN "Invalid limit unit".
   END.

   IF ttDayCampaign.first_limit > "" THEN DO: 
      ttDayCampaign.dc_firstlimit = LOOKUP(ttDayCampaign.first_limit,"full,relative").
      IF ttDayCampaign.dc_firstlimit = 0 THEN 
         RETURN "Invalid first limit definition".
      ttDayCampaign.dc_firstlimit = ttDayCampaign.dc_firstlimit - 1.     
   END.
      
   IF ttDayCampaign.billing_item > "" AND
      NOT CAN-FIND(FIRST BillItem WHERE
                         BillItem.Brand = gcBrand AND
                         BillItem.BillCode = ttDayCampaign.billing_item) THEN
      RETURN "Invalid billing item".

   IF ttDayCampaign.call_case > 0 AND 
      NOT CAN-FIND(FIRST CCN WHERE
                         CCN.Brand = gcBrand AND
                         CCN.CCN = ttDayCampaign.call_case) THEN
      RETURN "Invalid call case".
      
   IF ttDayCampaign.calc_method > "" THEN DO:
      lcValue = loRequestMaker:get_tmscodes_bssapi_internalid(
                                         "DayCampaign",
                                         "CalcMethod",
                                         ttDayCampaign.calc_method).
      IF lcValue = "" THEN RETURN "Invalid calculation method".
      ttDayCampaign.dc_calcmethod = INT(lcValue) NO-ERROR.
      IF ERROR-STATUS:ERROR OR 
         ttDayCampaign.dc_calcmethod = ? THEN RETURN "Invalid calculation method".
   END.

   IF ttDayCampaign.effective > "" THEN DO:
      ttDayCampaign.dc_effective = INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                                          "DayCampaign",
                                          "Effective",
                                          ttDayCampaign.effective)) NO-ERROR.
      IF ttDayCampaign.dc_effective = 0 THEN RETURN "Invalid effective definition".
   END.

   IF ttDayCampaign.weekday > "" THEN DO:
      liValue = INT(ttDayCampaign.weekday) NO-ERROR.
      IF liValue = 0 OR
         INDEX(ttDayCampaign.weekday,"8") > 0 OR
         INDEX(ttDayCampaign.weekday,"9") > 0 OR
         INDEX(ttDayCampaign.weekday,"0") > 0 THEN 
            RETURN "Invalid weekday definition".
   END.         

   IF ttDayCampaign.duration_type > "" THEN DO:
      ttDayCampaign.dc_durationtype = 
         INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                                      "DayCampaign",
                                      "DurType",
                                      ttDayCampaign.duration_type)) NO-ERROR.
      IF ttDayCampaign.dc_durationtype = 0 THEN 
         RETURN "Invalid duration type".
   END.

   IF ttDayCampaign.duration_unit > "" THEN DO:
      ttDayCampaign.dc_durationunit = 
         INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                                       "DayCampaign",
                                       "DurUnit",
                                       ttDayCampaign.duration_unit)) NO-ERROR.
      IF ttDayCampaign.dc_durationunit = 0 THEN 
         RETURN "Invalid duration unit".
   END.

   IF ttDayCampaign.fee_id > "" AND 
      NOT CAN-FIND(FIRST FeeModel WHERE
                         FeeModel.Brand = gcBrand AND
                         FeeModel.FeeModel = ttDayCampaign.fee_id) THEN 
      RETURN "Invalid fee model".
                          
END METHOD.

METHOD PUBLIC CHAR update_daycampaign():
 
   ASSIGN 
      DayCampaign.DCName         = ttDayCampaign.name WHEN ttDayCampaign.name > ""
      DayCampaign.ValidFrom      = ttDayCampaign.dc_validfrom 
                                      WHEN ttDayCampaign.dc_validfrom NE ?
      DayCampaign.ValidTo        = ttDayCampaign.dc_validto
                                      WHEN ttDayCampaign.dc_validto NE ?
      DayCampaign.DCTarget       = ttDayCampaign.dc_target 
                                      WHEN ttDayCampaign.dc_target NE ?
      DayCampaign.DCType         = ttDayCampaign.dc_packagetype 
                                      WHEN ttDayCampaign.dc_packagetype NE ?
      DayCampaign.InstanceLimit  = ttDayCampaign.instance_limit
                                      WHEN ttDayCampaign.instance_limit NE ?
      DayCampaign.MaxChargeIncl  = ttDayCampaign.limit 
                                       WHEN ttDayCampaign.limit NE ?
      DayCampaign.InclUnit       = ttDayCampaign.dc_limitunit 
                                      WHEN ttDayCampaign.dc_limitunit NE ?
      DayCampaign.TMRuleSeq      = ttDayCampaign.dc_limitruleid
                                      WHEN ttDayCampaign.dc_limitruleid NE ?
      DayCampaign.FirstMonthCalc = ttDayCampaign.dc_firstlimit
                                      WHEN ttDayCampaign.dc_firstlimit NE ?
      DayCampaign.BillCode       = ttDayCampaign.billing_item
                                      WHEN ttDayCampaign.billing_item NE ?
      DayCampaign.CCN            = ttDayCampaign.call_case 
                                      WHEN ttDayCampaign.call_case NE ?
      DayCampaign.CalcMethod     = ttDayCampaign.dc_calcmethod 
                                      WHEN ttDayCampaign.dc_calcmethod NE ?
      DayCampaign.Effective      = ttDayCampaign.dc_effective 
                                      WHEN ttDayCampaign.dc_effective NE ?
      DayCampaign.Weekday        = ttDayCampaign.weekday 
                                      WHEN ttDayCampaign.weekday NE ?
      DayCampaign.DurType        = ttDayCampaign.dc_durationtype
                                      WHEN ttDayCampaign.dc_durationtype NE ?
      DayCampaign.DurMonths      = ttDayCampaign.duration 
                                      WHEN ttDayCampaign.duration NE ?
      DayCampaign.DurUnit        = ttDayCampaign.dc_durationunit   
                                      WHEN ttDayCampaign.dc_durationunit NE ?
      DayCampaign.FeeModel       = ttDayCampaign.fee_id 
                                      WHEN ttDayCampaign.fee_id NE ?.
 
END METHOD.

DESTRUCTOR PUBLIC DayCampaignModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
END DESTRUCTOR.

END CLASS.

