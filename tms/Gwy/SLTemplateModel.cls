
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.SLTemplateModel IMPLEMENTS Gwy.ModelInterface: 
    
    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lModelid        AS CHAR              NO-UNDO.
    DEF VAR lCorrelationID  AS CHAR              NO-UNDO.
    DEF VAR gcBrand         AS CHAR              NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.

    DEFINE TEMP-TABLE ttSLTemplate NO-UNDO SERIALIZE-NAME "SLTemplate"
        FIELD external_id            AS CHAR
        FIELD usage_type             AS CHAR
        FIELD traffic_type           AS CHAR
        FIELD billing_call_case      AS CHAR
        FIELD billing_call_case_list AS CHARACTER 
        FIELD unlimited_usage        AS LOGICAL
        FIELD usage_amount           AS INT
        FIELD priority               AS INT
        FIELD shared_data_usage      AS LOGICAL
        FIELD provisioning_profile   AS CHARACTER.
   
    DEFINE DATASET SLTemplateDataset FOR ttSLTemplate.

    CONSTRUCTOR PUBLIC SLTemplateModel ( iJson    AS LONGCHAR,
                                        iModelid AS CHAR, 
                                        iSource  AS CHAR,
                                        iCorrelationID AS CHAR,
                                        iReplyTo AS CHAR):
                                   
        ASSIGN
            gcBrand         = "1"
            lJson           = iJson
            lModelid        = iModelid
            lCorrelationID  = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
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
            SLTemplate.code = ttSLTemplate.external_id) THEN
            RETURN loRequestMaker:logError("SLTemplate already exists").
             
        lcError = validate_sltemplate(TRUE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
        
        CREATE SLTemplate.
        ASSIGN
            SLTemplate.SLCode         = ttSLTemplate.external_id
            SLTemplate.elementtype    = ttSLTemplate.usage_type
            SLTemplate.trafficType    = ttSLTemplate.traffic_type
            SLTemplate.BCC            = ttSLTemplate.billing_call_case
            SLTemplate.bcc_list       = ttSLTemplate.billing_call_case_list
            SLTemplate.unlimitedUsage = ttSLTemplate.unlimited_usage
            SLTemplate.InclAmt        = ttSLTemplate.usage_amount
            SLTemplate.Prior          = ttSLTemplate.priority.
       
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
                                  lCorrelationID), 
                                  "DEBUG").
    
    END METHOD.
    
    METHOD PUBLIC LOG update():

        DEF VAR liWait AS INT NO-UNDO.
    
        IF lModelid = ? THEN 
            RETURN loRequestMaker:logError('SLTemplate Code not given').

        lJson = '~{"SLTemplateDataset":~{"SLTemplate":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET SLTemplateDataset BIND, lJson) THEN
            RETURN FALSE.
   
        IF lModelID = "" THEN 
            RETURN loRequestMaker:logError("Invalid SLTemplate Code").
      
        FIND ttSLTemplate NO-ERROR.
        IF NOT AVAILABLE ttSLTemplate THEN
            RETURN loRequestMaker:logError("Empty SLTemplate set").
       
        DO WHILE TRUE:
            FIND FIRST SLTemplate WHERE
                       SLTemplate.SLCode = lModelid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(SLTemplate) THEN DO:
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
        SLTemplate.elementtype    = ttSLTemplate.usage_type
        SLTemplate.trafficType    = ttSLTemplate.traffic_type
        SLTemplate.BCC            = ttSLTemplate.billing_call_case
        SLTemplate.bcc_list       = ttSLTemplate.billing_call_case_list
        SLTemplate.unlimitedUsage = ttSLTemplate.unlimited_usage
        SLTemplate.InclAmt        = ttSLTemplate.usage_amount
        SLTemplate.Prior          = ttSLTemplate.priority.
      
    loEventLogMaker:make_eventlog("modify",BUFFER SLTemplate:HANDLE).
   
    RELEASE SLTemplate.  
   
    /* error cases get response via logError, make response for 
       the succesful one */
    loRequestMaker:create_response("SLTemplate",
                                   lModelID,
                                   "OK",
                                   ""). 

    loRequestMaker:create_log(
                              SUBSTITUTE("SLTemplate Update: Id=&1 Ext.Request=&2", 
                              lModelID,
                              lCorrelationID), 
                              "DEBUG").
      
END METHOD.

DESTRUCTOR PUBLIC SLTemplateModel ():
    IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
    IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

METHOD PUBLIC CHAR validate_sltemplate(ilCreate AS LOG):
   
   IF(ilCreate OR ttSLTemplate.usage_type > "") AND
      LOOKUP(ttSLTemplate.usage_type,"voice,data,sms") = 0 THEN
      RETURN "Invalid Usage TYPE".
   
   IF(ilCreate OR ttSLTemplate.tarffic_type > "") AND
      LOOKUP(ttSLTemplate.tarffic_type,"National,International,Roaming") = 0 THEN
      RETURN "Invalid Tarffic TYPE".
      
   IF(ilCreate OR ttSLTemplate.unlimited_usage > "") AND
      LOOKUP(ttSLTemplate.unlimited_usage,{&BOOLEAN_TRUE}) = 0 AND
      LOOKUP(ttSLTemplate.unlimited_usage,{&BOOLEAN_FALSE}) = 0 THEN
      RETURN "Invalid Usage VALUE".  
    
END METHOD.

END CLASS.
