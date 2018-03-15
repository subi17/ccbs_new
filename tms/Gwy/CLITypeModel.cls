&GLOBAL-DEFINE COPY_SERVICE_PACKAGE "ADDITIONAL"

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.CLITypeModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR loRequestMaker  AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
DEF VAR loRepTextMaker  AS Gwy.RepTextMaker NO-UNDO.
DEF VAR lcError         AS CHAR NO-UNDO. 
    
DEF TEMP-TABLE ttCLIType NO-UNDO SERIALIZE-NAME "CLIType"
   FIELD external_id AS CHAR 
   FIELD name AS CHAR 
   FIELD payment_type AS CHAR
   FIELD rating_target AS INT 
   FIELD rateplan AS CHAR 
   FIELD roaming_group AS CHAR INIT ?
   FIELD ct_paymenttype AS INT.

{Gwy/RepText_definition.i}

DEFINE DATASET CLITypeDataSet FOR ttCLIType, ttRepText
   PARENT-ID-RELATION idparent FOR ttCLIType, ttRepText
      PARENT-ID-FIELD parent_id.


CONSTRUCTOR PUBLIC CLITypeModel ( iJson    AS LONGCHAR,
                                  iModelid AS CHAR, 
                                  iSource  AS CHAR,
                                  iCorrelationID AS CHAR,
                                  iReplyTo AS CHAR):
   ASSIGN
      gcBrand        = "1"
      lJson          = iJson
      lModelid       = iModelid
      lCorrelationID = iCorrelationID
      loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
      loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
      loRepTextMaker  = NEW Gwy.RepTextMaker(iSource).
   
END CONSTRUCTOR.

METHOD PUBLIC LOG create():
 
   lJson = '~{"CLITypeDataSet":~{"CLIType":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET CLITypeDataset BIND, lJson) THEN
       RETURN FALSE.
   
   FIND ttCLIType NO-ERROR.
   IF NOT AVAILABLE ttCLIType THEN
       RETURN loRequestMaker:logError("Empty subscription type set").

   IF ttCLIType.external_id = "" THEN 
      RETURN loRequestMaker:logError("Invalid subscription type ID").
        
   lcError = validate_clitype(ttCLIType.external_id,
                              TRUE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                             9,
                                             ttCLIType.external_id,
                                             RECID(ttCLIType)).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
      
   CREATE CLIType.
   ASSIGN 
      CLIType.Brand = gcBrand
      CLIType.CLIType = ttCLIType.external_id
      CLIType.CLIName = ttCLIType.name
      CLIType.PayType = ttCLIType.ct_paymenttype
      CLIType.BillTarget = ttCLIType.rating_target
      CLIType.PricePlan = ttCLIType.rateplan
      CLIType.RoamGroup = ttCLIType.roaming_group.
      
   loEventLogMaker:make_eventlog("create",BUFFER CLIType:HANDLE).

   /* copy default service packages */
   &IF {&COPY_SERVICE_PACKAGE} > "" 
   &THEN 
   copy_service_package(CLIType.CLIType,
                        TODAY).
   &ENDIF
   
   lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                           9,
                                           CLIType.CLIType,
                                           RECID(ttCLIType)). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
     
   RELEASE CLIType.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("CLIType",
                                  ttCLIType.external_id,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("CLIType create: Id=&1 Ext.Request=&2", 
                 ttCLIType.external_id,
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():

   DEF VAR liWait AS INT  NO-UNDO.
   
   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('CLIType ID not given').

   lJson = '~{"CLITypeDataSet":~{"CLIType":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET CLITypeDataset BIND, lJson) THEN
       RETURN FALSE.
   
   IF lModelID = "" THEN 
      RETURN loRequestMaker:logError("Invalid subscription type ID").
      
   FIND ttCLIType NO-ERROR.
   IF NOT AVAILABLE ttCLIType THEN
       RETURN loRequestMaker:logError("Empty subscription type set").

   lcError = validate_clitype(lModelID,
                              FALSE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   DO WHILE TRUE:
      FIND FIRST CLIType WHERE 
                 CLIType.Brand = gcBrand AND
                 CLIType.CLIType = lModelID EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED(CLIType) THEN DO:
         liWait = liWait + 1.
         IF liWait > {&WAIT_CONFIG_LOCK} THEN 
            RETURN loRequestMaker:logError(
                      "Subscription type was not available for update").
         PAUSE 1 NO-MESSAGE. 
         NEXT. 
      END.
      LEAVE.
   END.

   IF NOT AVAILABLE CLIType THEN 
      RETURN loRequestMaker:logError("Unknown subscription type").

   lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                             9,
                                             CLIType.CLIType,
                                             RECID(ttCLIType)).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
       
   loEventLogMaker:make_eventlog("oldbuffer",BUFFER CLIType:HANDLE).

   ASSIGN 
      CLIType.CLIName = ttCLIType.name WHEN ttCLIType.name > ""
      CLIType.PayType = ttCLIType.ct_paymenttype 
                           WHEN ttCLIType.ct_paymenttype > 0
      CLIType.BillTarget = ttCLIType.rating_target 
                              WHEN ttCLIType.rating_target > 0
      CLIType.PricePlan = ttCLIType.rateplan WHEN ttCLIType.rateplan > ""
      CLIType.RoamGroup = ttCLIType.roaming_group 
                             WHEN ttCLIType.roaming_group > "".
 
   loEventLogMaker:make_eventlog("modify",BUFFER CLIType:HANDLE).

   /* copy default service packages */
   &IF {&COPY_SERVICE_PACKAGE} > "" 
   &THEN 
   copy_service_package(CLIType.CLIType,
                        TODAY).
   &ENDIF
    
   lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                           9,
                                           CLIType.CLIType,
                                           RECID(ttCLIType)). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   RELEASE CLIType.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("CLIType",
                                  lModelId,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("CLIType update: Id=&1 Ext.Request=&2", 
                 lModelId,
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

METHOD PUBLIC CHAR validate_clitype
   (icCLIType AS CHAR,
    ilCreate AS LOG):

   IF LENGTH(icCLIType) > {&CONFIG_ID_MAX} THEN 
      RETURN "Max length for ID is {&CONFIG_ID_MAX} characters".
 
   IF CAN-FIND(FIRST CLIType WHERE 
                     CLIType.Brand = gcBrand AND
                     CLIType.CLIType = icCLIType) THEN DO:
      IF ilCreate THEN RETURN "Subscription type already exists".
   END.
   ELSE IF NOT ilCreate THEN RETURN "Unknown subscription type".

   IF ilCreate AND ttCLIType.name = "" THEN RETURN "Name is missing".
   
   IF (ilCreate OR ttCLIType.payment_type > "") THEN DO:
      ttCLIType.ct_paymenttype = INT(loRequestMaker:get_tmscodes_bssapi_internalid(
                                          "CLIType",
                                          "PayType",
                                          ttCLIType.payment_type)) NO-ERROR.
      IF ttCLIType.ct_paymenttype = 0 THEN RETURN "Invalid payment type".
   END.

   IF (ilCreate OR ttCLIType.rateplan > "") AND
      NOT CAN-FIND(FIRST RatePlan WHERE
                         RatePlan.Brand = gcBrand AND
                         RatePlan.RatePlan = ttCLIType.rateplan) THEN
      RETURN "Invalid rateplan".
   
   IF (ilCreate OR ttCLIType.roaming_group > "") AND
      NOT CAN-FIND(FIRST TMSCodes WHERE
                         TMSCodes.TableName = "RZItem" and
                         TMSCodes.FieldName = "RZIType" AND
                         TMSCodes.CodeValue = ttCLIType.roaming_group AND
                         TMSCodes.InUse > 0) THEN 
         RETURN "Invalid roaming group".
         
   RETURN "".
 
END METHOD.

METHOD PUBLIC LOG copy_service_package 
   (icCLIType AS CHAR,
    idaValidFrom AS DATE):
 
   DEF VAR liPack AS INT  NO-UNDO.
   
   DO liPack = 1 TO NUM-ENTRIES({&COPY_SERVICE_PACKAGE}):
   
      IF NOT CAN-FIND(FIRST ServPac WHERE 
                            ServPac.Brand = gcBrand AND
                            ServPac.ServPac = ENTRY(liPack,{&COPY_SERVICE_PACKAGE}))
      THEN NEXT. 
      
      FIND FIRST CTServPac WHERE 
                 CTServPac.Brand    = gcBrand AND
                 CTServPac.CLIType  = icCLIType AND
                 CTServPac.ServPac  = ENTRY(liPack,{&COPY_SERVICE_PACKAGE})
         NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CTServPac THEN DO:
         CREATE CTServPac.
         ASSIGN
            CTServPac.Brand    = gcBrand
            CTServPac.CLIType  = icCLIType
            CTServPac.ServPac  = ENTRY(liPack,{&COPY_SERVICE_PACKAGE})
            CTServPac.FromDate = idaValidFrom
            CTServPac.ToDate   = 12/31/2049
            CTServPac.ServType = 3. 
         loEventLogMaker:make_eventlog("create",BUFFER CTServPac:HANDLE).
      END.
      
      FOR EACH ServEl NO-LOCK WHERE
               ServEl.Brand   = gcBrand AND
               ServEl.ServPac = CTServPac.ServPac,
         FIRST ServCom NO-LOCK WHERE
               ServCom.Brand   = gcBrand AND
               ServCom.ServCom = ServEl.ServCom:
                      
         FIND FIRST CTServEl WHERE
              CTServEl.Brand    = gcBrand AND
              CTServEl.CLIType  = icCLIType AND
              CTServEl.ServPac  = CTServPac.ServPac AND
              CTServEl.ServCom  = ServEl.ServCom NO-LOCK NO-ERROR.
         IF NOT AVAILABLE CTServEl THEN DO:
            CREATE CTServEl.
            ASSIGN 
               CTServEl.Brand      = gcBrand
               CTServEl.CTServEl   = NEXT-VALUE(CTServEl)
               CTServEl.CLIType    = icCLIType
               CTServEl.ServPac    = CTServPac.ServPac
               CTServEl.ServCom    = ServEl.ServCom
               CTServEl.FromDate   = idaValidFrom
               CTServEl.ChgAllowed = ServCom.SCChgable
               CTServEl.ServType   = ServCom.ServType
               CTServEl.DefValue   = ServEl.SeValue.
         END.

         FOR EACH ServAttr NO-LOCK WHERE
                  ServAttr.Brand   = gcBrand AND
                  ServAttr.ServCom = ServEl.ServCom:
                         
            IF CAN-FIND(FIRST CTServAttr WHERE
                        CTServAttr.CTServEl = CTServEl.CTServEl AND
                        CTServAttr.ServAttr = ServAttr.ServAttr)
            THEN NEXT.
                   
            CREATE CTServAttr.
            ASSIGN 
               CTServAttr.CTServEl   = CTServEl.CTServEl
               CTServAttr.ServAttr   = ServAttr.ServAttr
               CTServAttr.FromDate   = idaValidFrom
               CTServAttr.DefValue   = ServAttr.DefValue
               CTServAttr.ChgAllowed = ServAttr.ScChgable.
            RELEASE CTServAttr.   
         END.
      
         RELEASE CTServEl.
      END.

      RELEASE CTServPac.
   END.
     
END METHOD.

DESTRUCTOR PUBLIC CLITypeModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
END DESTRUCTOR.

END CLASS.
