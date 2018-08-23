
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

    DEF TEMP-TABLE ttSLTemplate NO-UNDO SERIALIZE-NAME "SLTemplate"
        FIELD external_id          AS CHAR
        FIELD dial_type            AS CHAR
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
        
    DEF TEMP-TABLE ttdestination_call_case NO-UNDO SERIALIZE-NAME "destination_call_case"
        FIELD parent_id AS RECID 
        FIELD billing_call_case AS CHAR.
   
    DEF DATASET SLTemplateDataset FOR ttSLTemplate,ttdestination_call_case
        PARENT-ID-RELATION idparent FOR ttSLTemplate,ttdestination_call_case
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
             
        lcError = validate_sltemplate().
        IF lcError > "" THEN 
           RETURN loRequestMaker:logError(lcError).
        
        CREATE SLTemplate.
        ASSIGN
            SLTemplate.SLCode          = ttSLTemplate.external_id
            SLTemplate.Dialtype        = INT(ttSLTemplate.dial_type)
            SLTemplate.DestinationType = ttSLTemplate.destination_type
            SLTemplate.BCC             = INT(ttSLTemplate.inside_call_case)
            SLTemplate.unlimitedUsage  = ttSLTemplate.unlimited_usage
            SLTemplate.InclAmt         = ttSLTemplate.usage_amount
            SLTemplate.Prior           = ttSLTemplate.priority
            SLTemplate.FirstMonthLimit = ttSLTemplate.first_month_limit
            SLTemplate.LastMonthLimit  = ttSLTemplate.last_month_limit
            SLTemplate.BDestLimit      = ttSLTemplate.bdestination_limit.
            
        FOR EACH ttdestination_call_case WHERE 
                 ttdestination_call_case.parent_id = RECID(ttSLTemplate):
            ASSIGN SLTemplate.bcc_list = SLTemplate.bcc_list                             + 
                                         (IF SLTemplate.bcc_list <> "" THEN "," ELSE "") + 
                                         ttdestination_call_case.billing_call_case.
        END.
       
        loEventLogMaker:make_eventlog("create",BUFFER SLTemplate:HANDLE).
    
        /* error cases get response via logError, make response for the successful one */
        loRequestMaker:create_response("SLTemplate",
                                       ttSLTemplate.external_id,
                                       "OK",
                                       "").

        loRequestMaker:create_log(SUBSTITUTE("SLTemplate create: Id=&1 Ext.Request=&2", 
                                  ttSLTemplate.external_id,
                                  lcCorrelationID), 
                                  "DEBUG").
        RELEASE SLTemplate.

        RETURN TRUE.

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
            SLTemplate.Dialtype        = INT(ttSLTemplate.dial_type)        WHEN ttSLTemplate.dial_type > "" 
            SLTemplate.DestinationType = ttSLTemplate.destination_type      WHEN ttSLTemplate.destination_type > ""
            SLTemplate.BCC             = INT(ttSLTemplate.inside_call_case) WHEN ttSLTemplate.inside_call_case > ""
            SLTemplate.unlimitedUsage  = ttSLTemplate.unlimited_usage       WHEN ttSLTemplate.unlimited_usage 
            SLTemplate.InclAmt         = ttSLTemplate.usage_amount          WHEN ttSLTemplate.usage_amount > 0
            SLTemplate.Prior           = ttSLTemplate.priority              WHEN ttSLTemplate.priority > 0
            SLTemplate.FirstMonthLimit = ttSLTemplate.first_month_limit     WHEN ttSLTemplate.first_month_limit > ""
            SLTemplate.LastMonthLimit  = ttSLTemplate.last_month_limit      WHEN ttSLTemplate.last_month_limit > ""
            SLTemplate.BDestLimit      = ttSLTemplate.bdestination_limit    WHEN ttSLTemplate.bdestination_limit > 0.                             
        
        FOR EACH ttdestination_call_case WHERE 
                 ttdestination_call_case.parent_id = RECID(ttSLTemplate):
            ASSIGN SLTemplate.bcc_list = SLTemplate.bcc_list + 
                                         (IF SLTemplate.bcc_list <> "" THEN "," ELSE "") + 
                                         ttdestination_call_case.billing_call_case.
        END.   
                                            
        loEventLogMaker:make_eventlog("modify",BUFFER SLTemplate:HANDLE).
   
        /* error cases get response via logError, make response for the succesful one */
        loRequestMaker:create_response("SLTemplate",
                                        lcModelId,
                                        "OK",
                                        ""). 
        loRequestMaker:create_log(
                                  SUBSTITUTE("SLTemplate Update: Id=&1 Ext.Request=&2", 
                                  lcModelId,
                                  lcCorrelationID), 
                                  "DEBUG").
        RELEASE SLTemplate.  

        RETURN TRUE.

    END METHOD.

    METHOD PUBLIC CHAR validate_sltemplate():
   
        IF ttSLTemplate.dial_type > "" THEN
        DO:
            FIND FIRST DialType WHERE DialType.DialType = INT(ttSLTemplate.dial_type) NO-LOCK NO-ERROR.
            IF NOT AVAIL DialType THEN       
                RETURN "Invalid Usage type".
        END.
        ELSE 
            RETURN "Invalid Usage type".

        IF ttSLTemplate.destination_type > "" THEN 
        DO:
            IF LOOKUP(ttSLTemplate.destination_type,"National,International,Roaming") = 0 THEN
               RETURN "Invalid destination type". 
        END.
        ELSE    
           RETURN "Invalid destination type".
        
        RETURN "".

    END METHOD.
    
    METHOD PUBLIC LOG makeOtherAction(icAction AS CHAR):
        CASE icAction:
            WHEN "update_other" THEN 
                process_other_update().
            OTHERWISE 
                RETURN loRequestMaker:logError(SUBST("Unknown action &1",icAction)).
        END CASE.
    END METHOD.

    METHOD PUBLIC LOG process_other_update():

    END METHOD.
    
    DESTRUCTOR PUBLIC RatingBucketsModel ():
        IF VALID-OBJECT(loRequestMaker) THEN 
            DELETE OBJECT loRequestMaker.

        IF VALID-OBJECT(loEventLogMaker) THEN 
            DELETE OBJECT loEventLogMaker.
    END DESTRUCTOR.

END CLASS.
