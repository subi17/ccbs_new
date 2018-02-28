ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.AccountModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR liAccount      AS INT  NO-UNDO.
DEF VAR liAccType      AS INT  NO-UNDO.
DEF VAR loRequestMaker AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
DEFINE TEMP-TABLE ttAccount NO-UNDO SERIALIZE-NAME "Account"
   FIELD external_id AS CHAR 
   FIELD name AS CHAR 
   FIELD account_type AS CHAR.
   
DEFINE DATASET AccountDataSet FOR ttAccount.    

CONSTRUCTOR PUBLIC AccountModel ( iJson    AS LONGCHAR,
                                  iModelid AS CHAR, 
                                  iSource  AS CHAR,
                                  iCorrelationID AS CHAR,
                                  iReplyTo AS CHAR):

   ASSIGN
      gcBrand        = "1"
      lJson          = iJson
      lModelid       = iModelid
      lCorrelationID = iCorrelationID
      loRequestMaker = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
      loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
END CONSTRUCTOR.

METHOD PUBLIC LOG create():
 
   lJson = '~{"AccountDataSet":~{"Account":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lJson) THEN
       RETURN FALSE.
   
   FIND ttAccount NO-ERROR.
   IF NOT AVAILABLE ttAccount THEN
       RETURN loRequestMaker:logError("Empty Account set").

   liAccount = INT(ttAccount.external_id) NO-ERROR.
   IF liAccount = 0 THEN 
      RETURN loRequestMaker:logError("Invalid account ID").
        
   IF CAN-FIND(FIRST Account WHERE 
                     Account.Brand = gcBrand AND
                     Account.AccNum = liAccount) THEN 
      RETURN loRequestMaker:logError("Account already exists").
      
   IF ttAccount.account_type > "" THEN 
      liAccType = INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                         "Account",
                         "AccType",
                         ttAccount.account_type)) NO-ERROR.
   IF liAccType = 0 THEN 
      RETURN loRequestMaker:logError("Invalid account type").
   
   CREATE Account.
   ASSIGN 
      Account.Brand   = gcBrand
      Account.AccNum  = liAccount
      Account.AccName = ttAccount.name
      Account.AccType = liAccType.

   loEventLogMaker:make_eventlog("create",BUFFER Account:HANDLE).
      
   RELEASE Account.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("Account",
                                  STRING(liAccount),
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("Account create: Id=&1 Ext.Request=&2", 
                 liAccount,
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():

   DEF VAR liWait AS INT  NO-UNDO.
   
   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('Account ID not given').

   lJson = '~{"AccountDataSet":~{"Account":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lJson) THEN
       RETURN FALSE.
   
   liAccount = INT(lModelID) NO-ERROR.
   IF liAccount = 0 THEN 
      RETURN loRequestMaker:logError("Invalid account ID").
      
   FIND ttAccount NO-ERROR.
   IF NOT AVAILABLE ttAccount THEN
       RETURN loRequestMaker:logError("Empty Account set").

   DO WHILE TRUE:
      FIND FIRST Account WHERE 
                 Account.Brand = gcBrand AND
                 Account.AccNum = liAccount EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED(Account) THEN DO:
         liWait = liWait + 1.
         IF liWait > {&WAIT_CONFIG_LOCK} THEN 
            RETURN loRequestMaker:logError("Account was not available for update").
         PAUSE 1 NO-MESSAGE. 
         NEXT. 
      END.
      LEAVE.
   END.
   
   IF NOT AVAILABLE Account THEN 
      RETURN loRequestMaker:logError("Unknown account").
      
   IF ttAccount.account_type > "" THEN DO:
      liAccType = INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                         "Account",
                         "AccType",
                         ttAccount.account_type)) NO-ERROR.
      IF liAccType = 0 THEN 
         RETURN loRequestMaker:logError("Invalid account type").
   END.

   loEventLogMaker:make_eventlog("oldbuffer",BUFFER Account:HANDLE).

   IF ttAccount.name > "" THEN Account.AccName = ttAccount.name.
   IF liAccType > 0 THEN Account.AccType = liAccType.
   
   loEventLogMaker:make_eventlog("modify",BUFFER Account:HANDLE).
   
   RELEASE Account.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("Account",
                                  STRING(liAccount),
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("Account update: Id=&1 Ext.Request=&2", 
                 liAccount,
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

DESTRUCTOR PUBLIC AccountModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

END CLASS.

