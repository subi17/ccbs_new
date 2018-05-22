 
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.
//USING Gwy.AccountModel FROM PROPATH.

&GLOBAL-DEFINE WAIT_CONFIG_LOCK 20.

{Syst/tmsconst.i}

CLASS Gwy.AccountModel IMPLEMENTS Gwy.ModelInterface: 
    
    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lcModelId       AS CHAR              NO-UNDO.
    DEF VAR lcSource        AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR lcCodeName      AS CHAR              NO-UNDO.
    DEF VAR liCodeValue     AS INT               NO-UNDO.
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    DEF VAR loCommon        AS Func.Common       NO-UNDO.
    
    DEF VAR liAccount       AS INT               NO-UNDO.
    DEF VAR lcAccType       AS CHAR              NO-UNDO.
    DEF VAR gcBrand         AS CHAR              NO-UNDO.
    
    DEF TEMP-TABLE ttAccount NO-UNDO SERIALIZE-NAME "Account"
        FIELD external_id  AS INT
        FIELD name         AS CHAR
        FIELD account_type AS CHAR.
        
    DEF DATASET AccountDataset FOR ttAccount.
    
    CONSTRUCTOR PUBLIC AccountModel ( iJson          AS LONGCHAR,
                                      iBrand         AS CHAR,
                                      iModelid       AS CHAR, 
                                      iSource        AS CHAR,
                                      iCorrelationID AS CHAR,
                                      iReplyTo       AS CHAR):
                                   
        ASSIGN
            lJson           = iJson
            gcBrand         = iBrand
            lcModelId       = iModelid
            lcSource        = iSource
            lcCorrelationID = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
            loCommon        = NEW Func.Common().
   
    END CONSTRUCTOR.
    
    METHOD PUBLIC LOG create():
 
        lJson = '~{"AccountDataset":~{"Account":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lJson) THEN
            RETURN FALSE.
       
        FIND ttAccount NO-ERROR.
        IF NOT AVAILABLE ttAccount THEN
            RETURN loRequestMaker:logError("Empty Account Set").
   
        IF ttAccount.external_id = "" THEN
            RETURN loRequestMaker:logError("Invalid Account Id").
            
        liAccount = INT(ttAccount.external_id) NO-ERROR.
        
        IF liAccount = 0 THEN 
            RETURN loRequestMaker:logError("Invalid account ID").
      
        IF CAN-FIND(FIRST Account WHERE
                          Account.AccNum = liAccount) THEN
            RETURN loRequestMaker:logError("Account was already exists").
            
        IF ttAccount.account_type > "" THEN 
            ASSIGN liAcctype = loCommon:mTMSCodeValue("Account",
                                                      "AccType",
                                                      ttAccount.account_type).               
        CREATE Account.
        ASSIGN 
            Account.AccNum  = liAccount
            Account.AccName = ttAccount.name
                                  WHEN ttAccount.name > ""
            Account.AccType = liAccType.       
          
        loEventLogMaker:make_eventlog("create",BUFFER Account:HANDLE).
    
        RELEASE Account.
    
        /* error cases get response via logError, make response for 
          the successful one */
        loRequestMaker:create_response("Account",
                                       ttAccount.external_id,
                                       "OK",
                                       "").
        loRequestMaker:create_log(
                                  SUBSTITUTE("Account create: Id=&1 Ext.Request=&2", 
                                  ttAccount.external_id,
                                  lcCorrelationID), 
                                  "DEBUG").
    
    END METHOD.
    
    METHOD PUBLIC LOG update():

        DEF VAR liWait AS INT NO-UNDO.
    
        IF lcModelId = ? THEN 
            RETURN loRequestMaker:logError('Account id is not given').

        lJson = '~{"AccountDataset":~{"Account":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lJson) THEN
            RETURN FALSE.
   
        IF lcModelId = "" THEN 
            RETURN loRequestMaker:logError("Invalid Account id").
            
        liAccount = INT(lcModelId) NO-ERROR.
        IF liAccount = 0 THEN 
            RETURN loRequestMaker:logError("Invalid account ID").
      
        FIND ttAccount NO-ERROR.
        IF NOT AVAILABLE ttAccount THEN
            RETURN loRequestMaker:logError("Empty Account set").
       
        IF ttAccount.account_type > "" THEN 
            ASSIGN liAcctype = loCommon:mTMSCodeValue("Account",
                                                      "AccType",
                                                      ttAccount.account_type).
                                                                        
        DO WHILE TRUE:
            FIND FIRST Account WHERE
                Account.AccNum = lcModelId EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(Account) THEN 
            DO:
                liWait = liWait + 1.
                IF liWait > "{&WAIT_CONFIG_LOCK}" THEN 
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
            Account.AccName = ttAccount.name 
                                  WHEN ttAccount.name > ""
            Account.AccType = liAccType.       
        
        loEventLogMaker:make_eventlog("modify",BUFFER Account:HANDLE).
   
        RELEASE Account.  
   
        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("Account",
                                       lcModelId,
                                       "OK",
                                       ""). 

        loRequestMaker:create_log(
                                  SUBSTITUTE("Account Update: Id=&1 Ext.Request=&2", 
                                  lcModelId,
                                  lcCorrelationID), 
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
    
    DESTRUCTOR PUBLIC AccountModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
    END DESTRUCTOR.

END CLASS.