
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

USING Gwy.ModelInterface.

CLASS Gwy.RatingBucketsModel IMPLEMENTS Gwy.ModelInterface: 
    
    DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
    DEF VAR lcBrand         AS CHAR              NO-UNDO INIT "1".
    DEF VAR lcModelid       AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
    DEF VAR lcElemType AS CHAR NO-UNDO.

    DEF TEMP-TABLE ttSLTemplate NO-UNDO SERIALIZE-NAME "SLTemplate"
        FIELD external_id          AS CHAR
        FIELD usage_type           AS CHAR
        FIELD traffic_type         AS CHAR
        FIELD destination_type     AS CHAR
        FIELD inside_call_case     AS CHAR
        FIELD unlimited_usage      AS LOG 
        FIELD usage_amount         AS INT
        FIELD priority             AS INT
        FIELD shared_data_usage    AS LOG
        FIELD provisioning_profile AS CHAR
        FIELD first_month_limit    AS CHAR
        FIELD last_month_limit     AS CHAR 
        FIELD bdestination_limit   AS INT.
        
    DEF TEMP-TABLE ttoriginal_call_case NO-UNDO SERIALIZE-NAME "original_call_case"
        FIELD parent_id AS RECID 
        FIELD billing_call_case AS CHAR.
   
    DEF DATASET SLTemplateDataset FOR ttSLTemplate,ttoriginal_call_case
        PARENT-ID-RELATION idparent FOR ttSLTemplate,ttoriginal_call_case
        PARENT-ID-FIELD parent_id.

    CONSTRUCTOR PUBLIC RatingBucketsModel ( iJson          AS LONGCHAR,
                                            iModelid       AS CHAR, 
                                            iSource        AS CHAR,
                                            iCorrelationID AS CHAR,
                                            iReplyTo       AS CHAR):
                                   
        ASSIGN
            lcJson          = iJson
            lcModelId       = iModelid
            lcCorrelationId = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
    END CONSTRUCTOR.

    METHOD PUBLIC LOG create():
 
        lcJson = '~{"SLTemplateDataset":~{"SLTemplate":' + lcJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET SLTemplateDataset BIND, lcJson) THEN
            RETURN FALSE.
       
        FIND ttSLTemplate NO-ERROR.
        IF NOT AVAILABLE ttSLTemplate THEN
            RETURN loRequestMaker:logError("Empty SLTemplate set").
   
        IF ttSLTemplate.external_id = "" THEN
            RETURN loRequestMaker:logError("Invalid SLTemplate Code").
      
        IF LENGTH(ttSLTemplate.external_id) > {&CONFIG_ID_MAX} THEN 
            RETURN loRequestMaker:logError(
                "Max length for code is {&CONFIG_ID_MAX} characters").

        IF CAN-FIND(FIRST SLTemplate WHERE
                          SLTemplate.SLCode = ttSLTemplate.external_id) THEN
            RETURN loRequestMaker:logError("SLTemplate already exists").
             
        lcError = validate_sltemplate(TRUE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
        
        CREATE SLTemplate.
        ASSIGN
            SLTemplate.SLCode            = ttSLTemplate.external_id
            SLTemplate.elementtype       = INT(lcElemType)
            SLTemplate.trafficType       = ttSLTemplate.traffic_type
            SLTemplate.BCC               = INT(ttSLTemplate.inside_call_case)
            SLTemplate.unlimitedUsage    = ttSLTemplate.unlimited_usage
            SLTemplate.InclAmt           = ttSLTemplate.usage_amount 
                                               WHEN ttSLTemplate.usage_amount > 0
            SLTemplate.Prior             = ttSLTemplate.priority
            SLTemplate.FirstMonthLimit   = ttSLTemplate.first_month_limit
            SLTemplate.LastMonthLimit    = ttSLTemplate.last_month_limit
            SLTemplate.BDestLimit        = ttSLTemplate.bdestination_limit.
            
        FOR EACH ttoriginal_call_case WHERE 
                 ttoriginal_call_case.parent_id = RECID(ttSLTemplate):
            ASSIGN 
                SLTemplate.bcc_list = SLTemplate.bcc_list + "," +  ttoriginal_call_case.billing_call_case.
        END.   
        
        SLTemplate.bcc_list  = TRIM(SLTemplate.bcc_list , ",") .  
       
        loEventLogMaker:make_eventlog("create",BUFFER SLTemplate:HANDLE).
    
        RELEASE SLTemplate.
    
        /* error cases get response via logError, make response for 
          the successful one */
        loRequestMaker:create_response("SLTemplate",
                                       ttSLTemplate.external_id,
                                       "OK",
                                       "").
        loRequestMaker:create_log(
                                  SUBSTITUTE("SLTemplate create: Id=&1 Ext.Request=&2", 
                                  ttSLTemplate.external_id,
                                  lcCorrelationID), 
                                  "DEBUG").
    
    END METHOD.
    
    METHOD PUBLIC LOG update():

        DEF VAR liWait AS INT NO-UNDO.
    
        IF lcModelId = ? THEN 
            RETURN loRequestMaker:logError('SLTemplate Code not given').

        lcJson = '~{"SLTemplateDataset":~{"SLTemplate":' + lcJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET SLTemplateDataset BIND, lcJson) THEN
            RETURN FALSE.
   
        IF lcModelId = "" THEN 
            RETURN loRequestMaker:logError("Invalid SLTemplate Code").
      
        FIND ttSLTemplate NO-ERROR.
        IF NOT AVAILABLE ttSLTemplate THEN
            RETURN loRequestMaker:logError("Empty SLTemplate set").
       
        DO WHILE TRUE:
            FIND FIRST SLTemplate WHERE
                SLTemplate.SLCode = lcModelId EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(SLTemplate) THEN 
            DO:
                liWait = liWait + 1.
                IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                    RETURN loRequestMaker:logError("SLTemplate was not available for update").
                PAUSE 1 NO-MESSAGE. 
                NEXT. 
            END.
            LEAVE.
        END.
   
        IF NOT AVAILABLE SLTemplate THEN
            RETURN loRequestMaker:logError("Unknown SLTemplate").
      
        lcError = validate_sltemplate(FALSE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
    
        loEventLogMaker:make_eventlog("oldbuffer",BUFFER SLTemplate:HANDLE).
    
        ASSIGN 
            SLTemplate.elementtype       = INT(lcElemType)
                                               WHEN ttSLTemplate.usage_type > "" 
            SLTemplate.trafficType       = ttSLTemplate.traffic_type
                                               WHEN ttSLTemplate.traffic_type > ""
            SLTemplate.BCC               = INT(ttSLTemplate.inside_call_case)
                                               WHEN ttSLTemplate.inside_call_case > ""
            SLTemplate.unlimitedUsage    = ttSLTemplate.unlimited_usage
                                               WHEN ttSLTemplate.unlimited_usage 
            SLTemplate.InclAmt           = ttSLTemplate.usage_amount
            SLTemplate.Prior             = ttSLTemplate.priority 
                                               WHEN ttSLTemplate.priority > 0
            SLTemplate.FirstMonthLimit   = ttSLTemplate.first_month_limit
                                               WHEN ttSLTemplate.first_month_limit > ""
            SLTemplate.LastMonthLimit    = ttSLTemplate.last_month_limit
                                               WHEN ttSLTemplate.last_month_limit > ""
            SLTemplate.BDestLimit        = ttSLTemplate.bdestination_limit
                                               WHEN ttSLTemplate.bdestination_limit > 0.                             
        
        FOR EACH ttoriginal_call_case WHERE 
                 ttoriginal_call_case.parent_id = RECID(ttSLTemplate):
            ASSIGN 
                SLTemplate.bcc_list = SLTemplate.bcc_list + "," +  ttoriginal_call_case.billing_call_case.
        END.   
        
        SLTemplate.bcc_list  = TRIM(SLTemplate.bcc_list , ",") .   
                                            
        loEventLogMaker:make_eventlog("modify",BUFFER SLTemplate:HANDLE).
   
        RELEASE SLTemplate.  
   
        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("SLTemplate",
                                        lcModelId,
                                        "OK",
                                        ""). 

        loRequestMaker:create_log(
                                  SUBSTITUTE("SLTemplate Update: Id=&1 Ext.Request=&2", 
                                  lcModelId,
                                  lcCorrelationID), 
                                  "DEBUG").
      
    END METHOD.

    METHOD PUBLIC CHAR validate_sltemplate(ilCreate AS LOG):
   
        IF(ilCreate OR ttSLTemplate.usage_type > "") AND
            LOOKUP(ttSLTemplate.usage_type,"voice,data,sms") = 0 THEN
            RETURN "Invalid Usage TYPE".
        ELSE lcElemType = Func.Common:mTMSCodeValue("SLTemplate",
                                                    "ElementType",
                                                    ttSLTemplate.usage_type).
   
        IF(ilCreate OR ttSLTemplate.traffic_type > "") AND
            LOOKUP(ttSLTemplate.traffic_type,"National,International,Roaming") = 0 THEN
            RETURN "Invalid Tarffic TYPE".
        
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
    
    DESTRUCTOR PUBLIC RatingBucketsModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
    END DESTRUCTOR.

END CLASS.
