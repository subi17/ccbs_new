ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.AccountTypeModel IMPLEMENTS Gwy.ModelInterface: 
    
   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelId       AS CHAR              NO-UNDO.
   DEF VAR lcSource        AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR INIT "1"     NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
   
   DEF TEMP-TABLE ttAccountType NO-UNDO SERIALIZE-NAME "TMSCodes"
      FIELD external_id  AS CHAR 
      FIELD name         AS CHAR.

   DEF DATASET AccountTypeDataset FOR ttAccountType.
    
   CONSTRUCTOR PUBLIC AccountTypeModel(iJson AS LONGCHAR,
                                       iModelid AS CHAR, 
                                       iSource AS CHAR,
                                       iCorrelationID AS CHAR,
                                       iReplyTo AS CHAR):
                                   
      ASSIGN
         lcJson          = iJson
         lcModelId       = iModelid
         lcSource        = iSource
         lcCorrelationID = iCorrelationID
         loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
         loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
   END CONSTRUCTOR.
    
   METHOD PUBLIC LOG create():
      
      lcJson = '~{"AccountTypeDataset":~{"AccountType":~[' + lcJson + '~]~}~}'.
      IF loRequestMaker:parser_error(DATASET AccountTypeDataset BIND, lcJson) THEN
         RETURN FALSE.
       
      FIND ttAccountType NO-ERROR.
      IF NOT AVAILABLE ttAccountType THEN
         RETURN loRequestMaker:logError("Empty Account Type").
   
      IF ttAccountType.external_id EQ "" THEN
         RETURN loRequestMaker:logError("Invalid Account Type").
      
      IF CAN-FIND(FIRST TMSCodes WHERE
                        TMSCodes.TableName = "Account" AND
                        TMSCodes.FieldName = "AccType" AND
                        TMSCodes.CodeGroup = "Accounts" AND
                        TMSCodes.CodeValue = ttAccountType.external_id) THEN
         RETURN loRequestMaker:logError("AccountType was already exists").
            
      CREATE TMSCodes.
      ASSIGN 
         TMSCodes.TableName = "Account" 
         TMSCodes.FieldName = "AccType" 
         TMSCodes.CodeGroup = "Accounts"
         TMSCodes.CodeValue = ttAccountType.external_id
         TMSCodes.CodeName = ttAccountType.name.
                   
      loEventLogMaker:make_eventlog("create",BUFFER TMSCodes:HANDLE).
    
      RELEASE TMSCodes.
    
      /* error cases get response via logError, make response for 
         the successful one */
      IF lcCorrelationID > "" THEN
      loRequestMaker:create_response("TMSCodes",
                                     ttAccountType.external_id,
                                     "OK",
                                     "").
      &IF {&MQ_BASED} 
      &THEN                               
      loRequestMaker:create_log(
         SUBSTITUTE("TMSCodes create: Id=&1 Ext.Request=&2", 
         ttAccountType.external_id,
         lcCorrelationID), 
         "DEBUG").
      &ENDIF    
    
   END METHOD.
    
   METHOD PUBLIC LOG update():
       
      DEF VAR liWait AS INT NO-UNDO.
    
      IF lcModelId = ? THEN 
         RETURN loRequestMaker:logError('AccountType is not given').

      lcJson = '~{"AccountTypeDataset":~{"AccountType":~[' + lcJson + '~]~}~}'.
      
      IF loRequestMaker:parser_error(DATASET AccountTypeDataset BIND, lcJson) THEN
         RETURN FALSE.
   
      IF lcModelId = "" THEN 
         RETURN loRequestMaker:logError("Invalid AccountType id").
            
      FIND ttAccountType NO-ERROR.
      IF NOT AVAILABLE ttAccountType THEN
         RETURN loRequestMaker:logError("Empty AccountType set").
      DO WHILE TRUE:
         FIND FIRST TMSCodes EXCLUSIVE-LOCK WHERE
                    TMSCodes.TableName = "Account" AND
                    TMSCodes.FieldName = "AccType" AND
                    TMSCodes.CodeGroup = "Accounts" AND
                    TMSCodes.CodeValue = ttAccountType.external_id
                    NO-ERROR NO-WAIT.
         IF LOCKED(TMSCodes) THEN DO:
            liWait = liWait + 1.
            IF liWait > {&WAIT_CONFIG_LOCK} THEN 
               RETURN loRequestMaker:logError("TMSCodes was not available for update").
            PAUSE 1 NO-MESSAGE. 
            NEXT. 
         END.
         LEAVE.
      END.
   
      IF NOT AVAILABLE TMSCodes THEN
         RETURN loRequestMaker:logError("Unknown TMSCodes").
      
      loEventLogMaker:make_eventlog("oldbuffer",BUFFER TMSCodes:HANDLE).
        
      ASSIGN 
         TMSCodes.CodeName = ttAccountType.name 
                              WHEN ttAccountType.name > "".
        
      loEventLogMaker:make_eventlog("modify",BUFFER TMSCodes:HANDLE).
   
      RELEASE TMSCodes.  
   
      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN
      loRequestMaker:create_response("TMSCodes",
                                     lcModelId,
                                     "OK",
                                     ""). 

      &IF {&MQ_BASED} 
      &THEN
      loRequestMaker:create_log(
         SUBSTITUTE("TMSCodes Update: Id=&1 Ext.Request=&2", 
         lcModelId,
         lcCorrelationID), 
         "DEBUG").
      &ENDIF
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
    
   DESTRUCTOR PUBLIC AccountTypeModel ():
      IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
      IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   END DESTRUCTOR.

END CLASS.
