ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

{Syst/tmsconst.i}

USING Gwy.ModelInterface.

CLASS Gwy.AccountModel IMPLEMENTS Gwy.ModelInterface: 
    
   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelId       AS CHAR              NO-UNDO.
   DEF VAR lcSource        AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR lcError         AS CHAR              NO-UNDO. 
   DEF VAR lcCodeName      AS CHAR              NO-UNDO.
   DEF VAR liCodeValue     AS INT               NO-UNDO.
   DEF VAR liAccount       AS INT               NO-UNDO.
   DEF VAR lcAccType       AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR INIT "1"     NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
   
   DEF TEMP-TABLE ttAccount NO-UNDO SERIALIZE-NAME "Account"
      FIELD external_id  AS CHAR 
      FIELD name         AS CHAR
      FIELD account_type AS CHAR.
       
   DEF DATASET AccountDataset FOR ttAccount.
    
   CONSTRUCTOR PUBLIC AccountModel(iJson AS LONGCHAR,
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
      
      lcJson = '~{"AccountDataset":~{"Account":' + lcJson + '~}~}'.
      IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lcJson) THEN
         RETURN FALSE.
       
      FIND ttAccount NO-ERROR.
      IF NOT AVAILABLE ttAccount THEN
         RETURN loRequestMaker:logError("Empty Account Set").
   
      liAccount = INT(ttAccount.external_id) NO-ERROR.
      
      IF liAccount = 0 THEN 
         RETURN loRequestMaker:logError("Invalid account ID").
      
      IF CAN-FIND(FIRST Account WHERE
                        Account.Brand = gcBrand AND 
                        Account.AccNum = liAccount) THEN
         RETURN loRequestMaker:logError("Account already exists").
            
      IF ttAccount.account_type > "" THEN 
         ASSIGN lcAccType = Func.Common:mTMSCodeValue("Account",
                                                      "AccType",
                                                      ttAccount.account_type).               
      CREATE Account.
      ASSIGN 
         Account.Brand   = gcBrand
         Account.AccNum  = liAccount
         Account.AccName = ttAccount.name WHEN ttAccount.name > ""
         Account.AccType = INT(lcAccType).       
          
      loEventLogMaker:make_eventlog("create",BUFFER Account:HANDLE).
    
      RELEASE Account.
    
      /* error cases get response via logError, make response for 
         the successful one */
      IF lcCorrelationID > "" THEN
         loRequestMaker:create_response("Account",
                                        ttAccount.external_id,
                                        "OK",
                                        "").
      &IF {&MQ_BASED} &THEN                               
         loRequestMaker:create_log(SUBSTITUTE("Account created: Id=&1", ttAccount.external_id), "DEBUG").
      &ENDIF    
    
   END METHOD.
    
   METHOD PUBLIC LOG update():
       
      DEF VAR liWait AS INT NO-UNDO.
    
      IF lcModelId = ? OR lcModelId = "" THEN 
         RETURN loRequestMaker:logError('Account id is missing.').

      lcJson = '~{"AccountDataset":~{"Account":' + lcJson + '~}~}'.
      
      IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lcJson) THEN
         RETURN FALSE.
            
      liAccount = INT(lcModelId) NO-ERROR.
      IF liAccount = 0 THEN 
         RETURN loRequestMaker:logError("Invalid Account ID").
      
      FIND ttAccount NO-ERROR.
      IF NOT AVAILABLE ttAccount THEN
         RETURN loRequestMaker:logError("Empty Account set").

      DO WHILE TRUE:
         FIND FIRST Account WHERE
                    Account.Brand  = gcBrand AND
                    Account.AccNum = INT(lcModelId) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF LOCKED(Account) THEN 
         DO:
            liWait = liWait + 1.
            IF liWait > {&WAIT_CONFIG_LOCK} THEN 
               RETURN loRequestMaker:logError("Account was not available for update").
            PAUSE 1 NO-MESSAGE. 
            NEXT. 
         END.
         LEAVE.
      END.
   
      IF NOT AVAILABLE Account THEN
         RETURN loRequestMaker:logError("Unknown Account").
      
      loEventLogMaker:make_eventlog("oldbuffer",BUFFER Account:HANDLE).
        
      ASSIGN 
         Account.AccName = ttAccount.name WHEN ttAccount.name > ""
         Account.AccType = INT(lcAccType).       
        
      loEventLogMaker:make_eventlog("modify",BUFFER Account:HANDLE).
   
      RELEASE Account.  
   
      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN
         loRequestMaker:create_response("Account",
                                        lcModelId,
                                        "OK",
                                        ""). 
      &IF {&MQ_BASED} &THEN
         loRequestMaker:create_log(SUBSTITUTE("Account Update: Id=&1", lcModelId), "DEBUG").
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
    
   DESTRUCTOR PUBLIC AccountModel ():
      IF VALID-OBJECT(loRequestMaker) THEN 
         DELETE OBJECT loRequestMaker.
         
      IF VALID-OBJECT(loEventLogMaker) THEN 
         DELETE OBJECT loEventLogMaker.
   END DESTRUCTOR.

END CLASS.
