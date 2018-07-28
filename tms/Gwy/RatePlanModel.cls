ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.RatePlanModel IMPLEMENTS Gwy.ModelInterface:

   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelid       AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR              NO-UNDO.
   DEF VAR lcSource        AS CHAR              NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
   DEFINE TEMP-TABLE ttRatePlan NO-UNDO SERIALIZE-NAME "RatePlan"
      FIELD external_id AS CHAR
      FIELD name        AS CHAR.
    
   DEFINE TEMP-TABLE ttPListConf NO-UNDO SERIALIZE-NAME "pricelist"
      FIELD parent_id    AS RECID 
      FIELD pricelist    AS CHAR  
      FIELD valid_from   AS CHAR
      FIELD valid_to     AS CHAR
      FIELD priority     AS INT   INIT ?
      FIELD pl_rowid     AS INT 
      FIELD pl_validfrom AS DATE
      FIELD pl_validto   AS DATE.
   
   DEFINE DATASET RatePlanDataSet FOR ttRatePlan, ttPListConf
      PARENT-ID-RELATION idparent FOR ttRatePlan, ttPListConf
      PARENT-ID-FIELD parent_id.
 
   CONSTRUCTOR PUBLIC RatePlanModel(iJson    AS LONGCHAR,
                                    iModelid AS CHAR, 
                                    iSource  AS CHAR,
                                    iCorrelationID AS CHAR,
                                    iReplyTo AS CHAR):
   
      ASSIGN
         lcJson          = iJson
         lcModelid       = iModelid
         lcCorrelationID = iCorrelationID
         lcSource        = iSource
         loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
         loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
       
   END CONSTRUCTOR.

   METHOD PUBLIC LOG create():
   
      DEF VAR liText  AS INT  NO-UNDO. 
      DEF VAR lcError AS CHAR NO-UNDO.
     
      lcJson = '~{"RatePlanDataSet":~{"RatePlan":' + lcJson + '~}~}'.
      IF loRequestMaker:parser_error(DATASET RatePlanDataset BIND, lcJson) THEN
         RETURN FALSE.
      
      FIND ttRatePlan NO-ERROR.
      IF NOT AVAILABLE ttRatePlan THEN
         RETURN loRequestMaker:logError("Empty RatePlan set").
    
      IF ttRatePlan.external_id = "" THEN 
         RETURN loRequestMaker:logError("Invalid RatePlan ID").
    
      lcError = validate_pricelist(RECID(ttRatePlan),
                                   ttRatePlan.external_id,
                                   TRUE).
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
    
      CREATE RatePlan.
      ASSIGN 
         RatePlan.Brand    = gcBrand
         RatePlan.RatePlan = ttRatePlan.external_id
         RatePlan.RPName   = ttRatePlan.name.
    
      loEventLogMaker:make_eventlog("create",BUFFER RatePlan:HANDLE).
       
      RELEASE RatePlan.
          
      FOR EACH ttPListConf WHERE
               ttPListConf.parent_id = RECID(ttRatePlan):
         CREATE PListConf.
         ASSIGN 
            PListConf.Brand       = gcBrand
            PListConf.RatePlan    = ttRatePlan.external_id
            PListConf.PriceList   = ttPListConf.pricelist
            PListConf.dFrom       = ttPListConf.pl_validfrom
            PListConf.dTo         = ttPListConf.pl_validto
            PListConf.Prior       = ttPListConf.priority WHEN ttPListConf.priority NE ?.
    
         loEventLogMaker:make_eventlog("create",BUFFER PListConf:HANDLE).
          
         RELEASE PListConf.
      END.
       
      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN    
      loRequestMaker:create_response("RatePlan",
                                     ttRatePlan.external_id,
                                     "OK",
                                     ""). 
    
      &IF {&MQ_BASED} 
      &THEN
      loRequestMaker:create_log(
         SUBSTITUTE("RatePlan create: Id=&1 Ext.Request=&2", 
                    ttRatePlan.external_id,
                    lcCorrelationID), 
                    "DEBUG").
      &ENDIF
   
   END METHOD.

   METHOD PUBLIC LOG update():
     
      DEF VAR liText  AS INT   NO-UNDO. 
      DEF VAR lcError AS CHAR  NO-UNDO.
      DEF VAR llDone  AS LOG   NO-UNDO.
      DEF VAR liWait  AS INT   NO-UNDO.
      DEF VAR llOld   AS LOG   NO-UNDO.
      DEF VAR lrConf  AS RECID NO-UNDO.
       
      IF lcModelid = ? THEN 
         RETURN loRequestMaker:logError('RatePlan ID not given').
    
      lcJson = '~{"RatePlanDataSet":~{"RatePlan":' + lcJson + '~}~}'.
      IF loRequestMaker:parser_error(DATASET RatePlanDataset BIND, lcJson) THEN
         RETURN FALSE.
      
      IF lcModelid = "" THEN 
         RETURN loRequestMaker:logError("Invalid RatePlan ID").
         
      FIND ttRatePlan NO-ERROR.
      IF NOT AVAILABLE ttRatePlan THEN
         RETURN loRequestMaker:logError("Empty RatePlan set").
    
      lcError = validate_pricelist(RECID(ttRatePlan),
                                   lcModelid,
                                   FALSE).
      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
    
      llDone = FALSE.
    
      IF ttRatePlan.name > "" THEN DO:
         DO WHILE TRUE:
            FIND FIRST RatePlan EXCLUSIVE-LOCK WHERE 
                       RatePlan.Brand = gcBrand AND
                       RatePlan.RatePlan = lcModelid
                       NO-ERROR NO-WAIT.
            IF LOCKED(RatePlan) THEN DO:
               liWait = liWait + 1.
               IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                  RETURN loRequestMaker:logError("Rateplan was not available for update").
               PAUSE 1 NO-MESSAGE. 
               NEXT. 
            END.
            LEAVE.
         END.
     
         loEventLogMaker:make_eventlog("oldbuffer",BUFFER RatePlan:HANDLE).
         ASSIGN 
            RatePlan.RPName = ttRatePlan.name
            llDone = TRUE.
         loEventLogMaker:make_eventlog("modify",BUFFER RatePlan:HANDLE).
         RELEASE RatePlan.
      END.
     
      FOR EACH ttPListConf WHERE
               ttPListConf.parent_id = RECID(ttRatePlan):
           
      /* either existing item is updated or a new one is created */
          
         FIND FIRST PListConf WHERE 
                    PListConf.Brand = gcBrand AND
                    PListConf.RatePlan = lcModelid AND
                    PListConf.dFrom = ttPListConf.pl_validfrom AND
                    PListConf.PriceList = ttPListConf.pricelist
                    NO-LOCK NO-ERROR.
    
         IF AVAILABLE PListConf THEN DO:
            ASSIGN 
               liWait = 0
               lrConf = RECID(PListConf).
                
            DO WHILE TRUE:
               FIND FIRST PListConf EXCLUSIVE-LOCK WHERE 
                          RECID(PListConf) = lrConf
                          NO-WAIT NO-ERROR.
               IF LOCKED(PListConf) THEN DO:
                  liWait = liWait + 1.
                  /* rateplan may be partially already updated, but the entire update 
                     can be resent */
                  IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                      RETURN loRequestMaker:logError(
                                "Pricelist row was not available for update").
                   PAUSE 1 NO-MESSAGE. 
                   NEXT. 
                END.
                LEAVE.
             END.
          END.
          
          IF AVAILABLE PListConf THEN DO: 
             llOld = TRUE.
             loEventLogMaker:make_eventlog("oldbuffer",BUFFER PListConf:HANDLE).
          END.
          ELSE DO:
             CREATE PListConf.
             ASSIGN 
                PListConf.Brand    = gcBrand
                PListConf.RatePlan = lcModelid
                PListConf.dFrom    = ttPListConf.pl_validfrom
                PListConf.PriceList = ttPListConf.pricelist.
          END.
    
          ASSIGN 
             PListConf.dTo   = ttPListConf.pl_validto WHEN ttPListConf.pl_validto NE ?
             PListConf.Prior = ttPListConf.priority WHEN ttPListConf.priority NE ?.
    
          IF llOld THEN DO: 
             loEventLogMaker:make_eventlog("modify",BUFFER PListConf:HANDLE).
          END.
          ELSE DO:
             loEventLogMaker:make_eventlog("create",BUFFER PListConf:HANDLE).
          END.
          
          RELEASE PListConf.
          llDone = TRUE.
       END.
       
       /* error cases get response via logError, make response for 
          the succesful one */
       IF llDone THEN 
          loRequestMaker:create_response("RatePlan",
                                         lcModelid,
                                         "OK",
                                         ""). 
       ELSE RETURN loRequestMaker:logError("Nothing updated").
       
       loRequestMaker:create_log(
          SUBSTITUTE("RatePlan update: Id=&1 Ext.Request=&2", 
                     lcModelid,
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

METHOD PUBLIC CHAR validate_pricelist
   (irParentID AS RECID,
    icRatePlan  AS CHAR,
    ilCreate AS LOG):

   DEF VAR lcValue     AS CHAR NO-UNDO.
   DEF VAR lcPriceList AS CHAR NO-UNDO.
   DEF VAR llRowExists AS LOG  NO-UNDO.
   
   DEF BUFFER bttConf FOR ttPListConf.
   DEF BUFFER bPListConf FOR PListConf.

   IF LENGTH(icRatePlan) > {&CONFIG_ID_MAX} THEN 
      RETURN "Max length for ID is {&CONFIG_ID_MAX} characters".
 
   IF CAN-FIND(FIRST RatePlan WHERE 
                     RatePlan.Brand = gcBrand AND
                     RatePlan.RatePlan = icRatePlan) THEN DO:
      IF ilCreate THEN RETURN "Rateplan already exists".
   END.   
   ELSE IF NOT ilCreate THEN RETURN "Unknown rateplan".
 
   FOR EACH ttPListConf WHERE
            ttPListConf.parent_id = irParentID:

      IF CAN-FIND(FIRST bttConf WHERE 
                        bttConf.parent_id = irParentID AND
                        bttConf.pricelist = ttPListConf.pricelist AND
                        bttConf.valid_from = ttPListConf.valid_from AND
                        RECID(bttConf) NE RECID(ttPListConf)) THEN 
         RETURN "Two items have the same key values".
      
      ASSIGN 
         ttPListConf.pl_validfrom = loRequestMaker:make_date(
                                     loRequestMaker:make_timestamp(ttPListConf.valid_from))
         ttPListConf.pl_validto  = loRequestMaker:make_date(
                                    loRequestMaker:make_timestamp(ttPListConf.valid_to))
         NO-ERROR.
      IF ttPListConf.pl_validfrom = ? THEN 
         RETURN "Invalid pricelist row period". 

      IF ttPListConf.pricelist = "" OR
         NOT CAN-FIND(FIRST PriceList WHERE 
                            PriceList.Brand = gcBrand AND
                            PriceList.PriceList = ttPListConf.pricelist) THEN 
         RETURN "Invalid price list".

      llRowExists = CAN-FIND(FIRST PListConf WHERE 
                                PListConf.Brand = gcBrand AND
                                PListConf.RatePlan = icRatePlan AND
                                PListConf.dFrom = ttPListConf.pl_validfrom AND
                                PListConf.PriceList = ttPListConf.pricelist).
      IF llRowExists THEN DO: 
         IF ilCreate THEN RETURN "Pricelist row already exists".
      END.
      /* a new pricelist row can be created also with update message for rateplan */
   
      IF NOT llRowExists AND ttPListConf.pl_validto = ? THEN 
         RETURN "Invalid pricelist row period". 
          
      IF ttPListConf.pl_validto NE ? THEN DO:
         IF CAN-FIND(FIRST bPListConf USE-INDEX RatePlan WHERE
                           bPListConf.Brand = gcBrand AND
                           bPListConf.RatePlan = icRatePlan AND
                           bPListConf.PriceList = ttPListConf.pricelist AND
                           bPListConf.dFrom <= ttPListConf.pl_validto AND
                           bPListConf.dTo >= ttPListConf.pl_validfrom AND
                           bPListConf.dFrom NE ttPListConf.pl_validfrom) THEN 
           RETURN "Pricelist row with overlapping period exists".
      END.
   END.
   
   RETURN "".
 
END METHOD.

METHOD PUBLIC LOG process_other_update():

END METHOD.

DESTRUCTOR PUBLIC RatePlanModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

END CLASS.

