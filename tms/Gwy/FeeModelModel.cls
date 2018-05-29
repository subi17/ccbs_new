ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

&GLOBAL-DEFINE CONFIG_ID_MAX 32
&GLOBAL-DEFINE WAIT_CONFIG_LOCK 20
&GLOBAL-DEFINE FEE_TYPE "SF,MF,MC,PF"
&GLOBAL-DEFINE BILLING_METHOD "before,during,after"
&GLOBAL-DEFINE BOOLEAN_TRUE true 

{Syst/tmsconst.i}

CLASS Gwy.FeeModelModel IMPLEMENTS Gwy.ModelInterface:

   DEF VAR lcJson          AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelid       AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR              INIT "1" NO-UNDO.
   DEF VAR lcSource        AS CHAR              NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
   
   DEFINE TEMP-TABLE ttFeeModel NO-UNDO SERIALIZE-NAME "FeeModel"
      FIELD external_id AS CHAR
      FIELD name        AS CHAR.
    
   DEFINE TEMP-TABLE ttFMItem NO-UNDO SERIALIZE-NAME "item"
      FIELD parent_id          AS RECID 
      FIELD pricelist          AS CHAR  
      FIELD valid_from         AS CHAR
      FIELD valid_to           AS CHAR
      FIELD billing_item       AS CHAR
      FIELD fee_type           AS CHAR  INIT ?
      FIELD price              AS DEC   INIT ?
      FIELD first_period_rule  AS CHAR
      FIELD last_period_rule   AS CHAR 
      FIELD billing_method     AS CHAR  INIT ?
      FIELD interval           AS INT   INIT ?
      FIELD recurrence_limit   AS INT   INIT ?
      FIELD fi_validfrom       AS DATE
      FIELD fi_validto         AS DATE
      FIELD fi_firstperiodrule AS INT   INIT ?
      FIELD fi_lastperiodrule  AS INT   INIT ?.
   
   DEFINE DATASET FeeModelDataSet FOR ttFeeModel, ttFMItem
      PARENT-ID-RELATION idparent FOR ttFeeModel, ttFMItem
      PARENT-ID-FIELD parent_id.
 
   CONSTRUCTOR PUBLIC FeeModelModel(iJson          AS LONGCHAR,
      iModelid       AS CHAR, 
      iSource        AS CHAR,
      iCorrelationID AS CHAR,
      iReplyTo       AS CHAR):

      ASSIGN
         lcJson          = iJson
         lcModelid       = iModelid
         lcCorrelationID = iCorrelationID
         lcSource        = iSource
         loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
         loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
              
   END CONSTRUCTOR.

   METHOD PUBLIC LOG create():

      DEF VAR lcError AS CHAR NO-UNDO.
 
      lcJson = '~{"FeeModelDataSet":~{"FeeModel":~[' + lcJson + '~]~}~}'.
      
      IF loRequestMaker:parser_error(DATASET FeeModelDataset BIND, lcJson) THEN
         RETURN FALSE.

      FIND ttFeeModel NO-ERROR.
      IF NOT AVAILABLE ttFeeModel THEN
         RETURN loRequestMaker:logError("Empty FeeModel set").
      IF ttFeeModel.external_id = "" THEN 
         RETURN loRequestMaker:logError("Invalid FeeModel ID").

      IF LENGTH(ttFeeModel.external_id) > {&CONFIG_ID_MAX} THEN 
         RETURN loRequestMaker:logError(
            "Max length for ID is {&CONFIG_ID_MAX} characters").
 
      lcError = validate_items(RECID(ttFeeModel),
         ttFeeModel.external_id,
         TRUE).
      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

      CREATE FeeModel.
      ASSIGN 
         FeeModel.Brand    = gcBrand
         FeeModel.FeeModel = ttFeeModel.external_id
         FeeModel.FeeName  = ttFeeModel.name.
         
      loEventLogMaker:make_eventlog("create",BUFFER FeeModel:HANDLE).
   
      RELEASE FeeModel.

      lcError = update_items(RECID(ttFeeModel),ttFeeModel.external_id).
      
      IF lcError > "" AND lcError NE "Done" THEN 
         RETURN loRequestMaker:logError(lcError).

      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN 
         loRequestMaker:create_response("FeeModel",
                                        ttFeeModel.external_id,
                                        "OK",
                                        ""). 
      &IF {&MQ_BASED} 
      &THEN
      loRequestMaker:create_log(
            SUBSTITUTE("FeeModel create: Id=&1 Ext.Request=&2", 
            ttFeeModel.external_id,
            lcCorrelationID), 
            "DEBUG").  
      &ENDIF     

   END METHOD.

   METHOD PUBLIC LOG update():
  
      DEF VAR lcError  AS CHAR NO-UNDO.
      DEF VAR lcResult AS CHAR NO-UNDO.
      DEF VAR llDone   AS LOG  NO-UNDO.
      DEF VAR liWait   AS INT  NO-UNDO.
  
      IF lcModelid = ? THEN 
         RETURN loRequestMaker:logError('FeeModel ID not given').

      lcJson = '~{"FeeModelDataSet":~{"FeeModel":~[' + lcJson + '~]~}~}'.
      
      IF loRequestMaker:parser_error(DATASET FeeModelDataset BIND, lcJson) THEN
         RETURN FALSE.
   
      IF lcModelid = "" THEN 
         RETURN loRequestMaker:logError("Invalid FeeModel ID").
      
      FIND ttFeeModel NO-ERROR.
      IF NOT AVAILABLE ttFeeModel THEN
         RETURN loRequestMaker:logError("Empty FeeModel set").

      lcError = validate_items(RECID(ttFeeModel),
                               lcModelid,
                               FALSE).
      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

      llDone = FALSE.

      IF ttFeeModel.name > "" THEN DO:
         DO WHILE TRUE:
            FIND FIRST FeeModel EXCLUSIVE-LOCK WHERE 
                       FeeModel.Brand = gcBrand AND
                       FeeModel.FeeModel = lcModelid
                       NO-ERROR NO-WAIT.
            IF LOCKED(FeeModel) THEN DO:
               liWait = liWait + 1.
               IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                  RETURN loRequestMaker:logError("Fee model was 
                  not available for update").
               PAUSE 1 NO-MESSAGE. 
               NEXT. 
            END.
            LEAVE.
         END.
 
         loEventLogMaker:make_eventlog("oldbuffer",BUFFER FeeModel:HANDLE).
         ASSIGN 
            FeeModel.FeeName = ttFeeModel.name
            llDone           = TRUE.
         loEventLogMaker:make_eventlog("modify",BUFFER FeeModel:HANDLE).
         RELEASE FeeModel.
      END.

      lcResult = update_items(RECID(ttFeeModel),lcModelid).
      IF lcResult = "Done" THEN llDone = TRUE.
      ELSE IF lcResult > "" THEN RETURN loRequestMaker:logError(lcResult).

      /* error cases get response via logError, make response for 
         the succesful one */
      IF llDone THEN 
         loRequestMaker:create_response("FeeModel",
                                        lcModelid,
                                        "OK",
                                        ""). 
      ELSE RETURN loRequestMaker:logError("Nothing updated").
   
      &IF {&MQ_BASED} 
      &THEN
      loRequestMaker:create_log(
            SUBSTITUTE("FeeModel update: Id=&1 Ext.Request=&2", 
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

   METHOD PUBLIC CHAR validate_items(irParentID AS RECID,
                                     icFeeModel  AS CHAR,
                                     ilCreate AS LOG):

      DEF VAR lcValue     AS CHAR NO-UNDO.
      DEF VAR lcPriceList AS CHAR NO-UNDO.
      DEF VAR lcBillCode  AS CHAR NO-UNDO.
      DEF VAR llItemExist AS LOG  NO-UNDO.
   
      DEF BUFFER bttItem FOR ttFMItem.
      DEF BUFFER bFMItem FOR FMItem.

      IF CAN-FIND(FIRST FeeModel WHERE 
                        FeeModel.Brand = gcBrand AND
                        FeeModel.FeeModel = icFeeModel) THEN DO:
         IF ilCreate THEN RETURN "Fee model already exists".
      END.   
      ELSE IF NOT ilCreate THEN RETURN "Unknown fee model".
 
      FOR EACH ttFMItem WHERE
               ttFMItem.parent_id = irParentID:

         /* a new item can be created also with update message for fee model */
      
         ASSIGN 
            ttFMItem.fi_validfrom = loRequestMaker:make_date(
               loRequestMaker:make_timestamp(ttFMItem.valid_from))
            ttFMItem.fi_validto   = loRequestMaker:make_date(
               loRequestMaker:make_timestamp(ttFMItem.valid_to))NO-ERROR.
     
         IF ttFMItem.fi_validfrom = ? THEN 
            RETURN "Invalid item rule period". 
    
         IF ilCreate THEN DO: 
            IF CAN-FIND(FIRST bttItem WHERE 
                              bttItem.parent_id = irParentID AND
                              bttItem.pricelist = ttFMItem.pricelist AND
                              bttItem.billing_item = ttFMItem.billing_item AND
            /* do not check period */
                             RECID(bttItem) NE RECID(ttFMItem)) THEN 
               RETURN "Two items have the same key values".
         END.
          
         IF ttFMItem.pricelist = "" OR 
            NOT CAN-FIND(FIRST PriceList WHERE 
                               PriceList.Brand = gcBrand AND
                               PriceList.PriceList = ttFMItem.pricelist) THEN 
            RETURN "Invalid price list".
    
         IF ttFMItem.billing_item = "" OR 
            NOT CAN-FIND(FIRST BillItem WHERE
                               BillItem.Brand = gcBrand AND
                               BillItem.BillCode = ttFMItem.billing_item) THEN
            RETURN "Invalid billing item".
     
         llItemExist = CAN-FIND(FIRST FMItem WHERE
                                      FMItem.Brand = gcBrand AND
                                      FMItem.FeeModel = icFeeModel AND
                                      FMItem.PriceList = ttFMItem.pricelist AND
                                      FMItem.FromDate = ttFMItem.fi_validfrom).
            
         IF NOT llItemExist AND ttFMItem.fi_validto = ? THEN 
            RETURN "Invalid item rule period".                          
    
         IF ttFMItem.fi_validto NE ? THEN DO: 
            IF CAN-FIND(FIRST FMItem WHERE
                              FMItem.Brand = gcBrand AND
                              FMItem.FeeModel = icFeeModel AND
                              FMItem.PriceList = ttFMItem.pricelist AND
                              FMItem.BillCode = ttFMItem.billing_item AND
                              FMItem.ToDate >= ttFMItem.fi_validfrom AND
                              FMItem.FromDate <= ttFMItem.fi_validto AND
                              FMItem.FromDate NE ttFMItem.fi_validfrom) THEN
               RETURN "Fee item with overlapping period exists".
    
            IF CAN-FIND(FIRST FMItem WHERE
                              FMItem.Brand = gcBrand AND
                              FMItem.FeeModel = icFeeModel AND
                              FMItem.PriceList = ttFMItem.pricelist AND
                              FMItem.BillCode = ttFMItem.billing_item AND
                              FMItem.ToDate = ttFMItem.fi_validto AND
                              FMItem.FromDate NE ttFMItem.fi_validfrom) THEN
               RETURN "Fee item period end date is already in use".
         END.
              
         IF (NOT llItemExist OR ttFMItem.fee_type > "") AND
            (LOOKUP(ttFMItem.fee_type,{&FEE_TYPE}) = 0) THEN
            RETURN "Invalid fee type".
            
         IF (NOT llItemExist OR ttFMItem.billing_method > "") AND
            (LOOKUP(ttFMItem.billing_method,{&BILLING_METHOD}) = 0) THEN 
            RETURN "Invalid billing method".
          
         IF ttFMItem.first_period_rule > "" THEN DO:
            lcValue = Func.Common:mTMSCodeValue("FMItem",
                                                "FirstMonthBR",
                                                ttFMItem.first_period_rule).
            IF lcValue = "" THEN                                  
               RETURN "Invalid first period rule".
            ttFMItem.fi_firstperiodrule = INT(lcValue) NO-ERROR.
         END.
       
         IF ttFMItem.last_period_rule > "" THEN DO:
            lcValue = Func.Common:mTMSCodeValue("FMItem",
                                                "BrokenRental",
                                                ttFMItem.last_period_rule).
            IF lcValue = "" THEN                                  
               RETURN "Invalid last period rule".
            ttFMItem.fi_lastperiodrule = INT(lcValue) NO-ERROR.
         END. 
      END.
   
   RETURN "".

   END METHOD.

   METHOD PUBLIC CHAR update_items(irParentID AS RECID,icFeeModel  AS CHAR):

      DEF VAR liWait    AS INT   NO-UNDO.
      DEF VAR lrItem    AS RECID NO-UNDO.
      DEF VAR llNewItem AS LOG   NO-UNDO.
      DEF VAR liUpdated AS INT   NO-UNDO.

      FOR EACH ttFMItem WHERE
               ttFMItem.parent_id = RECID(ttFeeModel):

         FIND FIRST FMItem WHERE
                    FMItem.Brand = gcBrand AND
                    FMItem.FeeModel = icFeeModel AND
                    FMItem.PriceList = ttFMItem.pricelist AND
                    FMItem.BillCode = ttFMItem.billing_item AND
                    FMItem.FromDate = ttFMItem.fi_validfrom NO-LOCK NO-ERROR.
            /* either existing item is updated or a new one is created */
            IF AVAILABLE FMItem THEN DO: 
               ASSIGN 
                  liWait    = 0
                  lrItem    = RECID(FMItem)
                  llNewItem = FALSE.
            
               DO WHILE TRUE:
                  FIND FIRST FMItem EXCLUSIVE-LOCK WHERE 
                             RECID(FMItem) = lrItem
                             NO-ERROR NO-WAIT.
                  IF LOCKED(FMItem) THEN DO:
                     liWait = liWait + 1.
                     /* fee model may be partially already updated, 
                     but the entire update can be resent */
                     IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                        RETURN "Fee item was not available for update".
                     PAUSE 1 NO-MESSAGE. 
                     NEXT. 
                  END.
                  LEAVE.
               END.
 
               loEventLogMaker:make_eventlog("oldbuffer",BUFFER FMItem:HANDLE).
            END.
            ELSE DO:
               CREATE FMItem.
               ASSIGN 
                  FMItem.Brand     = gcBrand
                  FMItem.FeeModel  = icFeeModel
                  FMItem.PriceList = ttFMItem.pricelist
                  FMItem.BillCode  = ttFMItem.billing_item
                  FMItem.FromDate  = ttFMItem.fi_validfrom
                  llNewItem        = TRUE.
            END.

            IF ttFMItem.fi_validto NE ? THEN 
                FMItem.ToDate       = ttFMItem.fi_validto.
            IF ttFMItem.fee_type NE ? AND ttFMItem.fee_type = ENTRY(1,{&FEE_TYPE}) THEN 
                FMItem.BillMethod   = YES.
            IF ttFMItem.interval NE ? THEN 
                FMItem.Interval     = ttFMItem.interval.
            IF ttFMItem.recurrence_limit NE ? THEN 
                FMItem.FFItemQty    = ttFMItem.recurrence_limit.
            IF ttFMItem.billing_method NE ? THEN 
                FMItem.BillCycle    = LOOKUP(ttFMItem.billing_method,{&BILLING_METHOD}).
            IF ttFMItem.fi_firstperiodrule NE ? THEN 
                FMItem.FirstMonthBR = ttFMItem.fi_firstperiodrule.
            IF ttFMItem.fi_lastperiodrule NE ? THEN 
                FMItem.BrokenRental = ttFMItem.fi_lastperiodrule.  
            IF ttFMItem.price NE ? THEN 
                FMItem.Amount       = ttFMItem.price.
                FMItem.BillType     = IF FMItem.BillMethod THEN "OTC"
                                      ELSE "RC".      

            IF FMItem.BillMethod THEN ASSIGN
                    FMItem.interval  = 0
                    FMItem.FFItemQty = 0.
            ELSE IF FMItem.interval = 0 THEN
                    FMItem.Interval = 1.

            IF llNewItem THEN 
            DO: 
                loEventLogMaker:make_eventlog("create",BUFFER FMItem:HANDLE).
            END.
            ELSE 
            DO:
                loEventLogMaker:make_eventlog("modify",BUFFER FMItem:HANDLE).
            END.
      
            liUpdated = liUpdated + 1.
      
            RELEASE FMItem.
      END.

      IF liUpdated > 0 THEN RETURN "Done".
      ELSE RETURN "".
 
   END METHOD.

   METHOD PUBLIC LOG process_other_update():
   
   END METHOD.

   DESTRUCTOR PUBLIC FeeModelModel ():
      IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
      IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   END DESTRUCTOR.

END CLASS.

