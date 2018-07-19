ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.DayCampaignModel IMPLEMENTS Gwy.ModelInterface:

   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelid       AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR INIT "1"     NO-UNDO.
   DEF VAR lcError         AS CHAR              NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
   DEF VAR loRepTextMaker  AS Gwy.RepTextMaker  NO-UNDO.
    
   DEF TEMP-TABLE ttDayCampaign NO-UNDO SERIALIZE-NAME "DayCampaign"
      FIELD external_id                AS CHAR 
      FIELD name                       AS CHAR 
      FIELD valid_from                 AS CHAR
      FIELD valid_to                   AS CHAR 
      FIELD target                     AS CHAR INIT ?
      FIELD package_type               AS CHAR INIT ?
      FIELD category                   AS CHAR   
      FIELD pay_type                   AS CHAR
      FIELD instance_limit             AS INT  INIT ?
      FIELD limit                      AS DEC  INIT ?
      FIELD limit_unit                 AS CHAR INIT ?
      FIELD first_limit                AS CHAR INIT ?
      FIELD billing_item               AS CHAR INIT ?
      FIELD call_case                  AS INT  INIT ?
      FIELD calc_method                AS CHAR INIT ?
      FIELD effective                  AS CHAR INIT ?
      FIELD weekday                    AS CHAR INIT ?
      FIELD duration_type              AS CHAR INIT ?
      FIELD duration                   AS INT  INIT ?
      FIELD duration_unit              AS CHAR INIT ?
      FIELD fee_id                     AS CHAR INIT ?
      FIELD penalty_fee_calc_method    AS CHAR 
      FIELD dc_validfrom               AS DATE
      FIELD dc_validto                 AS DATE
      FIELD dc_pay_type                AS INT
      FIELD dc_category                AS INT 
      FIELD dc_paytype                 AS INT
      FIELD dc_limitunit               AS INT  INIT ?
      FIELD dc_firstlimit              AS INT  INIT ?
      FIELD dc_calcmethod              AS INT  INIT ?
      FIELD dc_effective               AS INT  INIT ?
      FIELD dc_durationtype            AS INT  INIT ?
      FIELD dc_durationunit            AS INT  INIT ?
      FIELD dc_penalty_fee_calc_method AS INT.
      
   {Gwy/RepText_definition.i}

   DEFINE DATASET DayCampaignDataSet FOR ttDayCampaign, ttRepText
      PARENT-ID-RELATION idparent FOR ttDayCampaign, ttRepText
      PARENT-ID-FIELD parent_id.

   CONSTRUCTOR PUBLIC DayCampaignModel(iJson AS LONGCHAR,
                                       iModelid AS CHAR, 
                                       iSource AS CHAR,
                                       iCorrelationID AS CHAR,
                                       iReplyTo AS CHAR):
      ASSIGN
         lcJson          = iJson
         lcModelid       = iModelid
         lcCorrelationID = iCorrelationID
         loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
         loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
         loRepTextMaker  = NEW Gwy.RepTextMaker(iSource).
   
   END CONSTRUCTOR.

   METHOD PUBLIC LOG create():
 
      lcJson = '~{"DayCampaignDataSet":~{"DayCampaign":~[' + lcJson + '~]~}~}'.
      IF loRequestMaker:parser_error(DATASET DayCampaignDataset BIND, lcJson) THEN
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
         DayCampaign.Brand   = gcBrand
         DayCampaign.DCEvent = ttDayCampaign.external_id.
          
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
      IF lcCorrelationID > "" THEN    
      loRequestMaker:create_response("DayCampaign",
                                     ttDayCampaign.external_id,
                                     "OK",
                                     ""). 
      &IF {&MQ_BASED} 
      &THEN 
      loRequestMaker:create_log(
         SUBSTITUTE("DayCampaign create: Id=&1 Ext.Request=&2", 
                    ttDayCampaign.external_id,
                    lcCorrelationID), 
                    "DEBUG").
      &ENDIF
     
   END METHOD.

   METHOD PUBLIC LOG update():
    
      DEF VAR liWait AS INT  NO-UNDO.
       
      IF lcModelid = ? THEN 
         RETURN loRequestMaker:logError('Package ID not given').
    
      lcJson = '~{"DayCampaignDataSet":~{"DayCampaign":~[' + lcJson + '~]~}~}'.
      IF loRequestMaker:parser_error(DATASET DayCampaignDataset BIND, lcJson) THEN
         RETURN FALSE.
       
      IF lcModelid = "" THEN 
         RETURN loRequestMaker:logError("Invalid package ID").
          
      FIND ttDayCampaign NO-ERROR.
      IF NOT AVAILABLE ttDayCampaign THEN
         RETURN loRequestMaker:logError("Empty package set").
    
      lcError = validate_daycampaign(lcModelID,
                                     FALSE).
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
    
      DO WHILE TRUE:
         FIND FIRST DayCampaign EXCLUSIVE-LOCK WHERE 
                    DayCampaign.Brand = gcBrand AND
                    DayCampaign.DCEvent = lcModelID  
                    NO-ERROR NO-WAIT.
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
      IF lcCorrelationID > "" then   
      loRequestMaker:create_response("DayCampaign",
                                     lcModelId,
                                     "OK",
                                     ""). 
      &IF {&MQ_BASED} 
      &THEN    
      loRequestMaker:create_log(
         SUBSTITUTE("DayCampaign update: Id=&1 Ext.Request=&2", 
                    lcModelId,
                    lcCorrelationID), 
                    "DEBUG").
      &ENDIF
          
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

   METHOD PUBLIC CHAR validate_daycampaign(icDayCampaign AS CHAR,
                                           ilCreate AS LOG):
        
      DEF VAR liValue        AS INT  NO-UNDO. 
      DEF VAR lcCalcMethod   AS CHAR NO-UNDO.
      DEF VAR lcDurUnit      AS CHAR NO-UNDO. 
      DEF VAR lcPayType      AS CHAR NO-UNDO.
      DEF VAR lcEffective    AS CHAR NO-UNDO.
      DEF VAR lcFeeCalMethod AS CHAR NO-UNDO.
    
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
    
      IF (ilCreate OR ttDayCampaign.category > "") THEN DO:
         ttDayCampaign.dc_category = LOOKUP(ttDayCampaign.category,{&DC_BUNDLETARGET_MOBILE}).  
         IF ttDayCampaign.dc_category = 0 THEN RETURN "Invalid category". 
      END.                                                 
       
      IF (ilCreate OR ttDayCampaign.pay_type > "") THEN DO:
         lcPayType = (Func.Common:mTMSCodeValue("DayCampaign",
                                                "PayType",
                                                ttDayCampaign.pay_type)).
         IF lcPayType EQ "" THEN 
            RETURN "Invalid Pay_Type".
         ELSE 
            ttDayCampaign.dc_pay_type = INT(lcPayType).    
      END.     
       
      IF (ilCreate OR ttDayCampaign.instance_limit NE ?) THEN DO:
          IF ttDayCampaign.instance_limit = 0 OR 
             ttDayCampaign.instance_limit > 99 THEN 
             RETURN "Invalid instance limit".
      END.
      
      IF (ttDayCampaign.limit NE ?) THEN DO:
         RETURN "Invalid Limit".
      END.
    /*
      IF (ilCreate OR ttDayCampaign.target > "") THEN DO:
         ttDayCampaign.target = loRequestMaker:get_tmscodes_bssapi_internalid(
                                      "DayCampaign",
                                      "DCTarget",
                                      ttDayCampaign.target).
         IF ttDayCampaign.target = "" THEN RETURN "Invalid package target".
      END.*/
       
      IF ttDayCampaign.limit_unit > "" THEN DO:
         ttDayCampaign.dc_limitunit = 
            INT(Func.Common:mTMSCodeValue("DayCampaign",
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
         lcCalcMethod = Func.Common:mTMSCodeValue("DayCampaign",
                                                  "CalcMethod",
                                                  ttDayCampaign.calc_method).
         IF lcCalcMethod = "" THEN RETURN "Invalid calculation method".
         ttDayCampaign.dc_calcmethod = INT(lcCalcMethod) NO-ERROR.
         IF ERROR-STATUS:ERROR OR 
            ttDayCampaign.dc_calcmethod = ? THEN RETURN "Invalid calculation method".
      END.
    
      IF (ilCreate OR ttDayCampaign.effective NE "") THEN DO:
         lcEffective = Func.Common:mTMSCodeValue("DayCampaign",
                                                 "Effective",
                                                 ttDayCampaign.effective).
         IF lcEffective > "" THEN 
            ttDayCampaign.dc_effective = INT(lcEffective).
         ELSE 
            RETURN "Invalid effective definition".
      END.
    
      IF ttDayCampaign.weekday > "" THEN DO:
         liValue = INT(ttDayCampaign.weekday) NO-ERROR.
         IF liValue = 0 OR
            INDEX(ttDayCampaign.weekday,"8") > 0 OR
            INDEX(ttDayCampaign.weekday,"9") > 0 OR
            INDEX(ttDayCampaign.weekday,"0") > 0 THEN 
               RETURN "Invalid weekday definition".
      END.         
    
      IF (ilCreate OR ttDayCampaign.duration_type NE "") THEN DO:
         ttDayCampaign.dc_durationtype = 
            INT(Func.Common:mTMSCodeValue("DayCampaign",
                                          "DurType",
                                          ttDayCampaign.duration_type)) NO-ERROR.
         IF ttDayCampaign.dc_durationtype = 0 THEN 
            RETURN "Invalid duration type".
      END.
      
      IF ttDayCampaign.duration EQ ? THEN DO: 
         RETURN "Invalid Duration Type". 
      END.
    
      IF ttDayCampaign.duration_unit > "" THEN DO:
         lcDurUnit = Func.Common:mTMSCodeValue("DayCampaign",
                                            "DurUnit",
                                            ttDayCampaign.duration_unit)
                                            NO-ERROR.
         IF lcDurUnit = "" THEN RETURN "Invalid Duration Unit".
            ttDayCampaign.dc_durationunit = INT(lcDurUnit) NO-ERROR.
         IF ERROR-STATUS:ERROR OR 
            ttDayCampaign.dc_durationunit = ? THEN RETURN "Invalid Duration Unit".
      END.
    
      IF ttDayCampaign.fee_id > "" AND 
         NOT CAN-FIND(FIRST FeeModel WHERE
                            FeeModel.Brand = gcBrand AND
                            FeeModel.FeeModel = ttDayCampaign.fee_id) THEN 
         RETURN "Invalid fee model".
          
      IF ttDayCampaign.penalty_fee_calc_method > "" THEN DO:
         lcFeeCalMethod = Func.Common:mTMSCodeValue("DayCampaign",
                                                 "TermFeeCalc",
                                                 ttDayCampaign.penalty_fee_calc_method)
                                                 NO-ERROR.
         IF lcFeeCalMethod = "" THEN RETURN "Invalid Fee Calc Method".
            ttDayCampaign.dc_penalty_fee_calc_method = INT(lcFeeCalMethod) NO-ERROR.
         IF ERROR-STATUS:ERROR OR 
            ttDayCampaign.dc_penalty_fee_calc_method = ? THEN 
            RETURN "Invalid Fee Calc Method".
      END.
                              
   END METHOD.

   METHOD PUBLIC CHAR update_daycampaign():
     
      ASSIGN 
         DayCampaign.DCName         = ttDayCampaign.name 
                                         WHEN ttDayCampaign.name > ""
         DayCampaign.ValidFrom      = ttDayCampaign.dc_validfrom 
                                         WHEN ttDayCampaign.dc_validfrom NE ?
         DayCampaign.ValidTo        = ttDayCampaign.dc_validto
                                         WHEN ttDayCampaign.dc_validto NE ?
         DayCampaign.DCTarget       = ttDayCampaign.target 
                                         WHEN ttDayCampaign.target NE ?
         DayCampaign.DCType         = ttDayCampaign.package_type 
                                         WHEN ttDayCampaign.package_type NE ?
         DayCampaign.BundleTarget   = ttDayCampaign.dc_category
                                         WHEN ttDayCampaign.dc_calcmethod NE ?
         DayCampaign.PayType        = ttDayCampaign.dc_paytype
                                         WHEN ttDayCampaign.dc_pay_type NE ?
         DayCampaign.InstanceLimit  = ttDayCampaign.instance_limit
                                         WHEN ttDayCampaign.instance_limit NE ?
         DayCampaign.MaxChargeIncl  = ttDayCampaign.limit 
                                          WHEN ttDayCampaign.limit NE ?
         DayCampaign.InclUnit       = ttDayCampaign.dc_limitunit 
                                         WHEN ttDayCampaign.dc_limitunit NE ?
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
                                         WHEN ttDayCampaign.duration NE ?.
         IF ttDayCampaign.dc_durationunit NE ? THEN 
            DayCampaign.DurUnit = ttDayCampaign.dc_durationunit.
         IF ttDayCampaign.fee_id NE ? THEN 
            DayCampaign.FeeModel = ttDayCampaign.fee_id.
         IF ttDayCampaign.dc_penalty_fee_calc_method NE ? THEN 
            DayCampaign.TermFeeCalc = ttDayCampaign.dc_penalty_fee_calc_method.
           
   END METHOD.

   DESTRUCTOR PUBLIC DayCampaignModel ():
      IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
      IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
      IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
   END DESTRUCTOR.

END CLASS.

