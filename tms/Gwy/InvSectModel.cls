USING Gwy.ModelInterface.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS Gwy.InvSectModel IMPLEMENTS Gwy.ModelInterface:

   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelid       AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR INIT "1"     NO-UNDO.
   DEF VAR lcError         AS CHAR              NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
   DEF VAR loRepTextMaker  AS Gwy.RepTextMaker  NO-UNDO.
    
    
   DEF TEMP-TABLE ttInvSect NO-UNDO SERIALIZE-NAME "InvSect"
      FIELD external_id AS CHAR 
      FIELD name        AS CHAR 
      FIELD is_printed  AS LOG.

   {Gwy/RepText_definition.i}    

   DEFINE DATASET InvSectDataSet FOR ttInvSect,ttRepText 
      PARENT-ID-RELATION idparent FOR ttInvSect,ttRepText
      PARENT-ID-FIELD parent_id. 

   CONSTRUCTOR PUBLIC InvSectModel(iJson    AS LONGCHAR,
                                   iModelid AS CHAR, 
                                   iSource  AS CHAR,
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
      lcJson = '~{"InvSectDataSet":~{"InvSect":' + lcJson + '~}~}'.
      
      IF loRequestMaker:parser_error(DATASET InvSectDataset BIND, lcJson) THEN
         RETURN FALSE.

      FIND ttInvSect NO-ERROR.
      IF NOT AVAILABLE ttInvSect THEN
         RETURN loRequestMaker:logError("Empty InvSect set").

      IF ttInvSect.external_id = "" THEN 
         RETURN loRequestMaker:logError("Invalid InvSect ID").

      IF (LENGTH(ttInvSect.external_id) > {&CONFIG_ID_MAX}) THEN 
         RETURN loRequestMaker:logError("Max length for ID is {&CONFIG_ID_MAX} characters").
         
      IF CAN-FIND(FIRST InvSect WHERE 
                        InvSect.Brand = gcBrand AND
                        InvSect.InvSect = ttInvSect.external_id) THEN 
         RETURN loRequestMaker:logError("InvSect already exists").
      
      lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                8,
                                                ttInvSect.external_id,
                                                RECID(ttInvSect)).
                                            
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
        
      CREATE InvSect.
      ASSIGN 
         InvSect.Brand     = gcBrand 
         InvSect.InvSect   = ttInvSect.external_id
         InvSect.ISName    = ttInvSect.name
         InvSect.PrintHead = ttInvSect.is_printed.
           
      loEventLogMaker:make_eventlog("create",BUFFER InvSect:HANDLE).
      
      lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                              8,
                                              InvSect.InvSect,
                                              RECID(ttInvSect)). 
      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
     
      RELEASE InvSect.

      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN 
      loRequestMaker:create_response("InvSect",
                                     ttInvSect.external_id,
                                     "OK",
                                     "").
      &IF {&MQ_BASED} 
      &THEN                                
      loRequestMaker:create_log(SUBSTITUTE("InvSect create: Id=&1 Ext.Request=&2", 
                                           ttInvSect.external_id,
                                           lcCorrelationID), 
                                           "DEBUG").
      &ENDIF
   END METHOD.

   METHOD PUBLIC LOG update():
      DEF VAR liWait AS INT NO-UNDO.
   
      IF lcModelid = ? THEN 
         RETURN loRequestMaker:logError('InvSect ID not given').

      lcJson = '~{"InvSectDataSet":~{"InvSect":' + lcJson + '~}~}'.
      IF loRequestMaker:parser_error(DATASET InvSectDataset BIND, lcJson) THEN
         RETURN FALSE.
   
      IF lcModelid = "" THEN 
         RETURN loRequestMaker:logError("Invalid InvSect ID").
      
      FIND ttInvSect NO-ERROR.
      IF NOT AVAILABLE ttInvSect THEN
         RETURN loRequestMaker:logError("Empty InvSect set").
      
      DO WHILE TRUE:
         FIND FIRST InvSect EXCLUSIVE-LOCK WHERE 
                    InvSect.Brand = gcBrand AND
                    InvSect.InvSect = lcModelid NO-ERROR NO-WAIT.
         IF LOCKED(InvSect) THEN 
         DO:
            liWait = liWait + 1.
            IF liWait > {&WAIT_CONFIG_LOCK} THEN 
               RETURN loRequestMaker:logError(
                  "Invoice section was not available for update").
            PAUSE 1 NO-MESSAGE. 
            NEXT. 
         END.
         LEAVE.
      END.
 
      IF NOT AVAILABLE InvSect THEN 
         RETURN loRequestMaker:logError("Unknown InvSect").
      
      lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                8,
                                                InvSect.InvSect,
                                                RECID(ttInvSect)).
      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
       
      loEventLogMaker:make_eventlog("oldbuffer",BUFFER InvSect:HANDLE).

      IF ttInvSect.name > "" THEN InvSect.ISName = ttInvSect.name.
   
      loEventLogMaker:make_eventlog("modify",BUFFER InvSect:HANDLE).

      lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                              8,
                                              InvSect.InvSect,
                                              RECID(ttInvSect)). 
      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

      RELEASE InvSect.

       /* error cases get response via logError, make response for 
          the succesful one */
      IF lcCorrelationID > "" THEN 
      loRequestMaker:create_response("InvSect",
                                     lcModelid,
                                     "OK",
                                     ""). 
      &IF {&MQ_BASED} 
      &THEN  
      loRequestMaker:create_log(
         SUBSTITUTE("InvSect update: Id=&1 Ext.Request=&2", 
                    lcModelid,
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

   DESTRUCTOR PUBLIC InvSectModel ():
      IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
      IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
      IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
   END DESTRUCTOR.

END CLASS.

