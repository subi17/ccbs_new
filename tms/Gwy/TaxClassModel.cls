ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.

{Syst/tmsconst.i}

USING Gwy.ModelInterface.

CLASS Gwy.TaxClassModel IMPLEMENTS Gwy.ModelInterface: 
    
   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelId       AS CHAR              NO-UNDO.
   DEF VAR lcSource        AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR INIT "1"     NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
   
   DEF TEMP-TABLE ttTaxClass NO-UNDO SERIALIZE-NAME "TaxClass"
      FIELD external_id  AS CHAR 
      FIELD name         AS CHAR
      FIELD active       AS LOGICAL
      FIELD type         AS CHAR
      FIELD amount       AS DEC.
       
   DEF DATASET TaxClassDataset FOR ttTaxClass.
    
   CONSTRUCTOR PUBLIC TaxClassModel(iJson AS LONGCHAR,
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
      
      lcJson = '~{"TaxClassDataset":~{"TaxClass":~[' + lcJson + '~]~}~}'.
      IF loRequestMaker:parser_error(DATASET TaxClassDataset BIND, lcJson) THEN
         RETURN FALSE.
       
      FIND ttTaxClass NO-ERROR.
      IF NOT AVAILABLE ttTaxClass THEN
         RETURN loRequestMaker:logError("Empty TaxClass Set").
   
      IF ttTaxClass.external_id EQ "" THEN
         RETURN loRequestMaker:logError("Invalid TaxClass ID").
      
      IF CAN-FIND(FIRST TaxClass WHERE
                        TaxClass.TaxClass = ttTaxClass.external_id) THEN
         RETURN loRequestMaker:logError("TaxClass was already exists").
            
      CREATE TaxClass.
      ASSIGN 
         TaxClass.TaxClass  = ttTaxClass.external_id
         TaxClass.TCName = ttTaxClass.name
         TaxClass.isActive = ttTaxClass.active
         TaxClass.TCType = ttTaxClass.type
         TaxClass.amount = ttTaxClass.amount.
                   
      loEventLogMaker:make_eventlog("create",BUFFER TaxClass:HANDLE).
    
      RELEASE TaxClass.
    
      /* error cases get response via logError, make response for 
         the successful one */
      IF lcCorrelationID > "" THEN
      loRequestMaker:create_response("TaxClass",
                                     ttTaxClass.external_id,
                                     "OK",
                                     "").
      &IF {&MQ_BASED} 
      &THEN                               
      loRequestMaker:create_log(
         SUBSTITUTE("TaxClass create: Id=&1 Ext.Request=&2", 
         ttTaxClass.external_id,
         lcCorrelationID), 
         "DEBUG").
      &ENDIF    
    
   END METHOD.
    
   METHOD PUBLIC LOG update():
       
      DEF VAR liWait AS INT NO-UNDO.
    
      IF lcModelId = ? THEN 
         RETURN loRequestMaker:logError('TaxClass is not given').

      lcJson = '~{"TaxClassDataset":~{"TaxClass":~[' + lcJson + '~]~}~}'.
      
      IF loRequestMaker:parser_error(DATASET TaxClassDataset BIND, lcJson) THEN
         RETURN FALSE.
   
      IF lcModelId = "" THEN 
         RETURN loRequestMaker:logError("Invalid TaxClass id").
            
      FIND ttTaxClass NO-ERROR.
      IF NOT AVAILABLE ttTaxClass THEN
         RETURN loRequestMaker:logError("Empty TaxClass set").
      DO WHILE TRUE:
         FIND FIRST TaxClass EXCLUSIVE-LOCK WHERE
                    TaxClass.TaxClass = lcModelId  
                    NO-ERROR NO-WAIT.
         IF LOCKED(TaxClass) THEN DO:
            liWait = liWait + 1.
            IF liWait > {&WAIT_CONFIG_LOCK} THEN 
               RETURN loRequestMaker:logError("TaxClass was not available for update").
            PAUSE 1 NO-MESSAGE. 
            NEXT. 
         END.
         LEAVE.
      END.
   
      IF NOT AVAILABLE TaxClass THEN
         RETURN loRequestMaker:logError("Unknown TaxClass").
      
      loEventLogMaker:make_eventlog("oldbuffer",BUFFER TaxClass:HANDLE).
        
      ASSIGN 
         TaxClass.TCName = ttTaxClass.name 
                              WHEN ttTaxClass.name > ""
         TaxClass.isActive = ttTaxClass.active
                              WHEN ttTaxClass.active
         TaxClass.TCType = ttTaxClass.type
                            WHEN ttTaxClass.type > ""
         TaxClass.amount = ttTaxClass.amount 
                              WHEN ttTaxClass.amount >= 0.
        
      loEventLogMaker:make_eventlog("modify",BUFFER TaxClass:HANDLE).
   
      RELEASE TaxClass.  
   
      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN
      loRequestMaker:create_response("TaxClass",
                                     lcModelId,
                                     "OK",
                                     ""). 

      &IF {&MQ_BASED} 
      &THEN
      loRequestMaker:create_log(
         SUBSTITUTE("TaxClass Update: Id=&1 Ext.Request=&2", 
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
    
   DESTRUCTOR PUBLIC TaxClassModel ():
      IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
      IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   END DESTRUCTOR.

END CLASS.
