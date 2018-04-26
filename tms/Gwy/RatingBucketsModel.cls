
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.RatingBucketsModel IMPLEMENTS Gwy.ModelInterface: 
    
    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lcBrand         AS CHAR              NO-UNDO.
    DEF VAR lcModelid       AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    DEF VAR locommon        AS Func.Common       NO-UNDO.
    
    DEF VAR liElemType AS CHAR NO-UNDO.

    DEF TEMP-TABLE ttSLTemplate NO-UNDO SERIALIZE-NAME "SLTemplate"
        FIELD external_id          AS CHAR
        FIELD usage_type           AS CHAR
        FIELD traffic_type         AS CHAR
        FIELD inside_call_case     AS CHAR
        FIELD unlimited_usage      AS LOG 
        FIELD usage_amount         AS INT
        FIELD priority             AS INT
        FIELD shared_data_usage    AS LOG
        FIELD provisioning_profile AS CHAR.
        
    DEF TEMP-TABLE ttoutside_call_case NO-UNDO SERIALIZE-NAME "outside_call_case"
        FIELD parent_id      AS RECID 
        FIELD call_case      AS CHAR
        FIELD call_case_name AS CHAR.
   
    DEF DATASET SLTemplateDataset FOR ttSLTemplate,ttoutside_call_case
        PARENT-ID-RELATION idparent FOR ttSLTemplate,ttoutside_call_case
        PARENT-ID-FIELD parent_id.

    CONSTRUCTOR PUBLIC RatingBucketsModel ( iJson          AS LONGCHAR,
                                            iBrand         AS CHAR,
                                            iModelid       AS CHAR, 
                                            iSource        AS CHAR,
                                            iCorrelationID AS CHAR,
                                            iReplyTo       AS CHAR):
                                   
        ASSIGN
            lJson           = iJson
            lcModelId       = iModelid
            lcCorrelationId = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
            loCommon        = NEW Func.Common().
   
    END CONSTRUCTOR.

    METHOD PUBLIC LOG create():
 
        lJson = '~{"SLTemplateDataset":~{"SLTemplate":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET SLTemplateDataset BIND, lJson) THEN
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
            SLTemplate.SLCode         = ttSLTemplate.external_id
            SLTemplate.elementtype    = liElemType
            SLTemplate.trafficType    = ttSLTemplate.traffic_type
            SLTemplate.BCC            = INT(ttSLTemplate.inside_call_case)
            SLTemplate.unlimitedUsage = ttSLTemplate.unlimited_usage
            SLTemplate.InclAmt        = ttSLTemplate.usage_amount 
                                            WHEN ttSLTemplate.usage_amount > 0
            SLTemplate.Prior          = ttSLTemplate.priority.
            
        FOR EACH ttoutside_call_case WHERE 
                 ttoutside_call_case.parent_id = RECID(ttSLTemplate):
            ASSIGN 
                SLTemplate.bcc_list = SLTemplate.bcc_list + "," +  ttoutside_call_case.call_case.
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

        lJson = '~{"SLTemplateDataset":~{"SLTemplate":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET SLTemplateDataset BIND, lJson) THEN
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
            SLTemplate.elementtype    = liElemType
                                            WHEN ttSLTemplate.usage_type > ""
            SLTemplate.trafficType    = ttSLTemplate.traffic_type
                                            WHEN ttSLTemplate.traffic_type > ""
            SLTemplate.BCC            = ttSLTemplate.inside_call_case
                                            WHEN ttSLTemplate.inside_call_case > ""
            SLTemplate.unlimitedUsage = ttSLTemplate.unlimited_usage
                                            WHEN ttSLTemplate.unlimited_usage > ""
            SLTemplate.InclAmt        = ttSLTemplate.usage_amount
                                            WHEN ttSLTemplate.usage_amount > 0
            SLTemplate.Prior          = ttSLTemplate.priority 
                                            WHEN ttSLTemplate.priority > 0.
        FOR EACH ttoutside_call_case WHERE 
                 ttoutside_call_case.parent_id = RECID(ttSLTemplate):
            ASSIGN 
                SLTemplate.bcc_list = SLTemplate.bcc_list + "," +  ttoutside_call_case.call_case.
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
        ELSE liElemType = loCommon:mTMSCodeValue("SLTemplate",
                                                 "UsageType",
                                                 ttSLTemplate.usage_type).
   
        IF(ilCreate OR ttSLTemplate.tarffic_type > "") AND
            LOOKUP(ttSLTemplate.tarffic_type,"National,International,Roaming") = 0 THEN
            RETURN "Invalid Tarffic TYPE".
      
        IF(ilCreate OR ttSLTemplate.unlimited_usage > "") AND 
          (ttSLTemplate.unlimited_usage NE TRUE OR ttSLTemplate.unlimited_usage NE FALSE) THEN
            RETURN "Invalid Usage VALUE".  
    
    END METHOD.
    
    DESTRUCTOR PUBLIC RatingBucketsModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
    END DESTRUCTOR.

END CLASS.
