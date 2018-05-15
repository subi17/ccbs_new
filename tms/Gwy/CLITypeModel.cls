&GLOBAL-DEFINE COPY_SERVICE_PACKAGE "ADDITIONAL"

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.CLITypeModel IMPLEMENTS Gwy.ModelInterface:

    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lcModelid       AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR gcBrand         AS CHAR              NO-UNDO.
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    DEF VAR loRepTextMaker  AS Gwy.RepTextMaker  NO-UNDO.
    DEF VAR loCommon        AS Func.Common       NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    
    DEF TEMP-TABLE ttCLIType NO-UNDO SERIALIZE-NAME "CLIType"
        FIELD external_id                  AS CHAR 
        FIELD name                         AS CHAR 
        FIELD payment_type                 AS CHAR
        FIELD rating_target                AS INT 
        FIELD rateplan                     AS CHAR
        FIELD category                     AS CHAR 
        FIELD stc_status                   AS CHAR 
        FIELD web_status                   AS CHAR 
        FIELD service_class                AS CHAR 
        FIELD download_speed               AS CHAR 
        FIELD upload_speed                 AS CHAR 
        FIELD tariff_type                  AS CHAR 
        FIELD line_type                    AS CHAR 
        FIELD fixed_line_type              AS CHAR 
        FIELD min_consumption_billing_item AS CHAR
        FIELD ct_payment_type              AS INT 
        FIELD ct_category                  AS INT 
        FIELD ct_stc_status                AS INT 
        FIELD ct_web_status                AS INT 
        FIELD ct_tariff_type               AS INT
        FIELD ct_line_type                 AS INT 
        FIELD ct_fixed_line_type           AS INT.
        
    DEF TEMP-TABLE ttMatrix NO-UNDO 
        FIELD parent_id AS RECID 
        FIELD bundle_id AS char
        FIELD valid_from AS DATE 
        FIELD valid_to AS DATE.
         
   {Gwy/RepText_definition.i}

    DEFINE DATASET CLITypeDataSet FOR ttCLIType, ttMatrix, ttRepText
        PARENT-ID-RELATION idparent FOR ttCLIType, ttMatrix
            PARENT-ID-FIELD parent_id
        PARENT-ID-RELATION idparent FOR ttCLIType, ttRepText
            PARENT-ID-FIELD parent_id.

    CONSTRUCTOR PUBLIC CLITypeModel ( iJson    AS LONGCHAR,
                                      iBrand AS CHAR,
                                      iModelid AS CHAR, 
                                      iSource  AS CHAR,
                                      iCorrelationID AS CHAR,
                                      iReplyTo AS CHAR):
        ASSIGN
            gcBrand         = iBrand
            lJson           = iJson
            lcModelid       = iModelid
            lcCorrelationID = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
            loRepTextMaker  = NEW Gwy.RepTextMaker(iSource)
            loCommon        = NEW Func.Common().
   
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
        
        lcError = validate_clitype(ttCLIType.external_id,TRUE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                  9,
                                                  ttCLIType.external_id,
                                                  RECID(ttCLIType)).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
      
        CREATE CLIType.
        ASSIGN 
            CLIType.Brand             = gcBrand
            CLIType.CLIType           = ttCLIType.external_id
            CLIType.CLIName           = ttCLIType.name
            CLIType.PayType           = ttCLIType.ct_payment_type
            CLIType.BillTarget        = ttCLIType.rating_target
            CLIType.PricePlan         = ttCLIType.rateplan
            CLIType.UsageType         = ttCLIType.ct_category            
            CLIType.StatusCode        = ttCLIType.ct_stc_status
            CLIType.WebStatusCode     = ttCLIType.ct_web_status
            CLIType.ServiceClass      = ttCLIType.service_class
                                            WHEN ttCLIType.service_class > ""
            CLIType.FixedLineDownload = ttCLIType.download_speed
                                            WHEN ttCLIType.download_speed > ""
            CLIType.FixedLineUpload   = ttCLIType.upload_speed
                                            WHEN ttCLIType.upload_speed > ""
            CLIType.TariffType        = ttCLIType.ct_tariff_type
            CLIType.LineType          = ttCLIType.ct_line_type
            CLIType.FixedLineType     = ttCLIType.ct_fixed_line_type
                                            WHEN ttCLIType.fixed_line_type GE 0
            CLIType.BillCode          = ttCLIType.min_consumption_billing_item
                                            WHEN ttCLIType.min_consumption_billing_item > "".
      
        loEventLogMaker:make_eventlog("create",BUFFER CLIType:HANDLE).

   /* copy default service packages */
   &IF {&COPY_SERVICE_PACKAGE} > "" 
   &THEN 
        copy_service_package(CLIType.CLIType,TODAY).
   &ENDIF
        pMatrix(ttCliType.CliType,ttCliType.BaseBundle,"PERCONTR").
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
                        lcCorrelationID), 
                        "DEBUG").
 
    END METHOD.

    METHOD PUBLIC LOG update():

        DEF VAR liWait AS INT NO-UNDO.
   
        IF lcModelid = ? THEN 
            RETURN loRequestMaker:logError('CLIType ID not given').

        lJson = '~{"CLITypeDataSet":~{"CLIType":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET CLITypeDataset BIND, lJson) THEN
            RETURN FALSE.
   
        IF lcModelid = "" THEN 
            RETURN loRequestMaker:logError("Invalid subscription type ID").
      
        FIND ttCLIType NO-ERROR.
        IF NOT AVAILABLE ttCLIType THEN
            RETURN loRequestMaker:logError("Empty subscription type set").

        lcError = validate_clitype(lcModelid,FALSE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        DO WHILE TRUE:
            FIND FIRST CLIType WHERE 
                       CLIType.Brand = gcBrand AND
                       CLIType.CLIType = lcModelid 
                       EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(CLIType) THEN 
            DO:
                liWait = liWait + 1.
                IF liWait > "{&WAIT_CONFIG_LOCK}" THEN 
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
            CLIType.CLIName           = ttCLIType.name
            CLIType.PayType           = ttCLIType.ct_payment_type
            CLIType.BillTarget        = ttCLIType.rating_target
            CLIType.PricePlan         = ttCLIType.rateplan
            CLIType.UsageType         = ttCLIType.ct_category            
            CLIType.StatusCode        = ttCLIType.ct_stc_status
            CLIType.WebStatusCode     = ttCLIType.ct_web_status
            CLIType.ServiceClass      = ttCLIType.service_class
                                            WHEN ttCLIType.service_class > ""
            CLIType.FixedLineDownload = ttCLIType.download_speed
                                            WHEN ttCLIType.download_speed > ""
            CLIType.FixedLineUpload   = ttCLIType.upload_speed
                                            WHEN ttCLIType.upload_speed > ""
            CLIType.TariffType        = ttCLIType.ct_tariff_type
            CLIType.LineType          = ttCLIType.ct_line_type
            CLIType.FixedLineType     = ttCLIType.ct_fixed_line_type
                                            WHEN ttCLIType.fixed_line_type GE 0
            CLIType.BillCode          = ttCLIType.min_consumption_billing_item
                                            WHEN ttCLIType.min_consumption_billing_item > "".
   
        loEventLogMaker:make_eventlog("modify",BUFFER CLIType:HANDLE).

   /* copy default service packages */
   &IF {&COPY_SERVICE_PACKAGE} > "" 
   &THEN 
        copy_service_package(CLIType.CLIType,TODAY).
   &ENDIF
        pMatrix(ttCliType.CliType,ttCliType.BaseBundle,"PERCONTR").
            
        lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                                9,
                                                CLIType.CLIType,
                                                RECID(ttCLIType)). 
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        RELEASE CLIType.

        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("CLIType",
                                        lcModelid,
                                        "OK",
                                        ""). 

        loRequestMaker:create_log(
            SUBSTITUTE("CLIType update: Id=&1 Ext.Request=&2", 
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

    METHOD PUBLIC LOG process_other_update():

    END METHOD.

    METHOD PUBLIC CHAR validate_clitype
        (icCLIType AS CHAR,
        ilCreate AS LOG):
            
        DEF VAR lcPaymentType   AS CHAR NO-UNDO.
        DEF VAR lcStatusCode    AS CHAR NO-UNDO.
        DEF VAR lcCategory      AS CHAR NO-UNDO.
        DEF VAR lcWebStatusCode AS CHAR NO-UNDO.
        DEF VAR lcTariffType    AS CHAR NO-UNDO.
        DEF VAR lcLineType      AS CHAR NO-UNDO.
        DEF VAR lcFixLineType   AS CHAR NO-UNDO.
        
        IF LENGTH(icCLIType) > "{&CONFIG_ID_MAX}" THEN 
            RETURN "Max length for ID is {&CONFIG_ID_MAX} characters".
 
        IF CAN-FIND(FIRST CLIType WHERE 
                          CLIType.Brand = gcBrand AND
                          CLIType.CLIType = icCLIType) THEN 
        DO:
            IF ilCreate THEN RETURN "Subscription type already exists".
        END.
        ELSE IF NOT ilCreate THEN RETURN "Unknown subscription type".

        IF ilCreate AND ttCLIType.name = "" THEN RETURN "Name is missing".
   
        IF (ilCreate OR ttCLIType.payment_type > "") THEN 
        DO:
            lcPaymentType = loCommon:mTMSCodeValue("CLIType",
                                                   "PayType",
                                                   ttCLIType.payment_type) NO-ERROR.
            IF lcPaymentType = "" THEN RETURN "Invalid Payment Type".
            ttCLIType.ct_payment_type = INT(lcPaymentType) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_payment_type EQ ? THEN 
                RETURN "Invalid Payment Type".
            
        END.

        IF (ilCreate OR ttCLIType.rateplan > "") AND
            NOT CAN-FIND(FIRST RatePlan WHERE
                               RatePlan.Brand = gcBrand AND
                               RatePlan.RatePlan = ttCLIType.rateplan) THEN
            RETURN "Invalid rateplan".
   
        IF (ilCreate OR ttCLIType.category > "") THEN
        DO:
            lcCategory = loCommon:mTMSCodeValue("CLIType",
                                                "UsageType",
                                                ttCLIType.category) NO-ERROR.
            IF lcCategory = "" THEN RETURN "Invalid Category".
            ttCLIType.ct_category = INT(lcCategory) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_category EQ ? THEN 
                RETURN "Invalid Category".                                  
        END.
        
        IF (ilCreate OR ttCLIType.stc_status > "") THEN
        DO:
            lcStatusCode = loCommon:mTMSCodeValue("CLIType",
                                                  "StatusCode",
                                                  ttCLIType.stc_status) NO-ERROR.
            IF lcStatusCode = "" THEN RETURN "Invalid Status Code".
            ttCLIType.ct_stc_status = INT(lcStatusCode) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_stc_status EQ ? THEN 
                RETURN "Invalid Status Code".
        END.
        
        IF (ilCreate OR ttCLIType.web_status > "") THEN
        DO:
            lcWebStatusCode = loCommon:mTMSCodeValue("CLIType",
                                                     "WebStatusCode",
                                                     ttCLIType.web_status) NO-ERROR.
            IF lcWebStatusCode = "" THEN RETURN "Invalid WebStatusCode".
            ttCLIType.ct_web_status = INT(lcWebStatusCode) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_web_status EQ ? then
                RETURN "Invalid WebStatus Code".
        END.
        
        IF (ilCreate OR ttCLIType.tariff_type > "") THEN
        DO:
            lcTariffType = loCommon:mTMSCodeValue("CLIType",
                                                  "TariffType",
                                                  ttCLIType.tariff_type) NO-ERROR.
            IF lcTariffType = "" THEN RETURN "Invalid Tariff Type".
            ttCLIType.ct_tariff_type = INT(lcTariffType) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_tariff_type EQ ? THEN 
                RETURN "Invalid Tariff Type".
        END.
        
        IF (ilCreate OR ttCLIType.line_type > "") THEN
        DO:
            lcLineType = loCommon:mTMSCodeValue("CLIType",
                                                "LineType",
                                                ttCLIType.line_type) NO-ERROR.
            IF lcLineType = "" THEN RETURN "Invalid LineType".
            ttCLIType.ct_line_type = INT(lcLineType) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_line_type EQ ? THEN 
                RETURN "Invalid LineType".
        END.
        
        IF (ilCreate OR ttCLIType.fixed_line_type > "") THEN
        DO:
            lcFixLineType = loCommon:mTMSCodeValue("CLIType",
                                                   "FixedLineType",
                                                   ttCLIType.fixed_line_type) NO-ERROR.
            IF lcFixLineType = "" THEN RETURN "Invalid FixedLineType".
            ttCLIType.ct_Fixed_line_type = INT(lcFixLineType) NO-ERROR.
            IF ERROR-STATUS:ERROR OR ttCLIType.ct_Fixed_line_type EQ ? THEN 
                RETURN "Invalid FixedLineType".
        END.
        
        RETURN "".
 
    END METHOD.

    METHOD PUBLIC LOG copy_service_package 
        (icCLIType AS CHAR,
        idaValidFrom AS DATE):
 
        DEF VAR liPack AS INT NO-UNDO.
   
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
            IF NOT AVAILABLE CTServPac THEN 
            DO:
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
                IF NOT AVAILABLE CTServEl THEN 
                DO:
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
    
    METHOD PUBLIC CHAR pMatrix(icCLIType AS CHAR,
                               icAllowedBundles AS CHAR,
                               icType AS CHAR):

        DEF VAR liPrior AS INT NO-UNDO.
        DEF VAR liMXSeq AS INT NO-UNDO.

        DEFINE VARIABLE liCount AS INTEGER NO-UNDO.
        
        DEFINE BUFFER bf_Matrix FOR Matrix.

        FIND LAST bf_Matrix WHERE bf_Matrix.mxkey = icKey NO-LOCK NO-ERROR.
        IF AVAILABLE bf_Matrix THEN 
            liPrior = (bf_Matrix.Prior + 1).
        ELSE 
            liPrior = 1.
            
        FOR EACH bf_Matrix NO-LOCK BY bf_Matrix.MXSeq DESCENDING:
            liMXSeq =  bf_Matrix.MXSeq + 1.
        END.

        FIND FIRST Matrix WHERE 
                   Matrix.Brand = gcBrand AND 
                   Matrix.MXKey = icType AND 
                   Matrix.MxName = icCLIType NO-LOCK NO-ERROR.
        IF NOT AVAIL Matrix THEN 
        DO:
            CREATE Matrix.
            ASSIGN
                Matrix.Brand  = Syst.Var:gcBrand
                Matrix.MXSeq  = liMXSeq
                Matrix.mxkey  = icType
                Matrix.mxname = icCLIType
                Matrix.prior  = liPrior
                Matrix.mxres  = 1.
       
            CREATE MXItem.
            ASSIGN
                MXItem.MxSeq   = Matrix.MXSeq
                MXItem.MxValue = icCLIType
                MXItem.MxName  = "SubsTypeTo".   
        END.
      
        DO liCount = 1 TO NUM-ENTRIES(icAllowedBundles)
            ON ERROR UNDO, THROW:
      
            IF icType = "PERCONTR" AND LOOKUP(ENTRY(liCount,icAllowedBundles), "CONTDSL") > 0 THEN 
                NEXT.
      
            FIND FIRST MxItem WHERE 
                MxItem.MxSeq   = Matrix.MXSeq AND 
                MxItem.MxName  = (IF icType = "PERCONTR" THEN "PerContract" ELSE "SubsTypeFrom") AND 
                MxItem.MxValue = ENTRY(liCount,icAllowedBundles) NO-LOCK NO-ERROR.
            IF NOT AVAIL MxItem THEN 
            DO:      
                CREATE MXItem.
                ASSIGN
                    MXItem.MxSeq   = Matrix.MXSeq
                    MXItem.MxValue = ENTRY(liCount,icAllowedBundles)
                    MXItem.MxName  = (IF icType = "PERCONTR" THEN "PerContract" ELSE "SubsTypeFrom").   
            END.   

        END.

        RETURN "".

    END METHOD.
    
    DESTRUCTOR PUBLIC CLITypeModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
        IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
    END DESTRUCTOR.

END CLASS.

