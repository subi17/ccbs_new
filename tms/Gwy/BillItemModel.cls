ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.
USING Gwy.BillItemModel FROM PROPATH.

{Syst/tmsconst.i}

CLASS Gwy.BillItemModel IMPLEMENTS Gwy.ModelInterface:

    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lcModelid       AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR gcBrand         AS CHAR              NO-UNDO.
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    DEF VAR loRepTextMaker  AS Gwy.RepTextMaker  NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR liWait          AS INT               NO-UNDO.
     
    DEF TEMP-TABLE ttBillItem NO-UNDO SERIALIZE-NAME "BillItem"
        FIELD external_id        AS CHAR 
        FIELD name               AS CHAR 
        FIELD invoice_section    AS CHAR
        FIELD display_order      AS INT
        FIELD billing_item_group AS CHAR
        FIELD tax_class          AS CHAR
        FIELD debt_type          AS CHAR
        FIELD profit_type        AS CHAR.

    {Gwy/RepText_definition.i}
  
    DEFINE DATASET BillItemDataSet FOR ttBillItem, ttRepText
        PARENT-ID-RELATION idparent FOR ttBillItem, ttRepText
        PARENT-ID-FIELD parent_id.

    CONSTRUCTOR PUBLIC BillItemModel ( iJson          AS LONGCHAR,
                                       iBrand         AS CHAR,
                                       iModelid       AS CHAR, 
                                       iSource        AS CHAR,
                                       iCorrelationID AS CHAR,
                                       iReplyTo       AS CHAR):
        ASSIGN
            gcBrand         = iBrand
            lJson           = iJson
            lcModelid       = iModelid
            lcCorrelationID = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource)
            loRepTextMaker  = NEW Gwy.RepTextMaker(iSource).
   
    END CONSTRUCTOR.

    METHOD PUBLIC LOG create():
 
        lJson = '~{"BillItemDataSet":~{"BillItem":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET BillItemDataset BIND, lJson) THEN
            RETURN FALSE.
   
        FIND ttBillItem NO-ERROR.
        IF NOT AVAILABLE ttBillItem THEN
            RETURN loRequestMaker:logError("Empty BillItem set").

        IF ttBillItem.external_id = "" THEN 
            RETURN loRequestMaker:logError("Invalid BillItem ID").

        IF LENGTH(ttBillItem.external_id) > {&CONFIG_ID_MAX} THEN 
            RETURN loRequestMaker:logError(
                "Max length for ID is {&CONFIG_ID_MAX} characters").
         
        IF CAN-FIND(FIRST BillItem WHERE 
                          BillItem.Brand = gcBrand AND
                          BillItem.BillCode = ttBillItem.external_id) THEN 
            RETURN loRequestMaker:logError("BillItem already exists").

        lcError = validate_billitem(TRUE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
   
        lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                  1,
                                                  ttBillItem.external_id,
                                                  RECID(ttBillItem)).  
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
   
        CREATE BillItem.
        ASSIGN 
            BillItem.Brand    = gcBrand
            BillItem.BillCode = ttBillItem.external_id
            BillItem.BIName   = ttBillItem.name
            BillItem.InvSect  = ttBillItem.invoice_section
            BillItem.BIGroup  = ttBillItem.billing_item_group
            BillItem.TaxClass = ttBillItem.tax_class
            BillItem.DebtType = ttBillItem.debt_type
            BillItem.BillType = ttBillItem.profit_type.

        loEventLogMaker:make_eventlog("create",BUFFER BillItem:HANDLE).

        lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                                1,
                                                BillItem.BillCode,
                                                RECID(ttBillItem)). 
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        RELEASE BillItem.

        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("BillItem",
                                        ttBillItem.external_id,
                                        "OK",
                                        ""). 

        loRequestMaker:create_log(
                                    SUBSTITUTE("BillItem create: Id=&1 Ext.Request=&2", 
                                    ttBillItem.external_id,
                                    lcCorrelationID), 
                                    "DEBUG").
 
    END METHOD.

    METHOD PUBLIC LOG update():

        IF lcModelid = ? THEN 
            RETURN loRequestMaker:logError('BillItem ID not given').

        lJson = '~{"BillItemDataSet":~{"BillItem":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET BillItemDataset BIND, lJson) THEN
            RETURN FALSE.
   
        IF lcModelid = "" THEN 
            RETURN loRequestMaker:logError("Invalid BillItem ID").
      
        FIND ttBillItem NO-ERROR.
        IF NOT AVAILABLE ttBillItem THEN
            RETURN loRequestMaker:logError("Empty BillItem set").
       
        DO WHILE TRUE:
            FIND FIRST BillItem WHERE 
                       BillItem.Brand = gcBrand AND
                       BillItem.BillCode = lcModelid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(BillItem) THEN 
            DO:
                liWait = liWait + 1.
                IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                    RETURN loRequestMaker:logError("Billing item was not available for update").
                PAUSE 1 NO-MESSAGE. 
                NEXT. 
            END.
            LEAVE.
        END.
 
        IF NOT AVAILABLE BillItem THEN 
            RETURN loRequestMaker:logError("Unknown BillItem").

        lcError = validate_billitem(FALSE).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
 
        lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                  1,
                                                  BillItem.BillCode,
                                                  RECID(ttBillItem)).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        loEventLogMaker:make_eventlog("oldbuffer",BUFFER BillItem:HANDLE).

        IF ttBillItem.name > "" THEN BillItem.BIName = ttBillItem.name.
        IF ttBillItem.invoice_section > "" THEN 
            BillItem.InvSect = ttBillItem.invoice_section.
        IF ttBillItem.billing_item_group > "" THEN    
            BillItem.BIGroup = ttBillItem.billing_item_group.
        IF ttBillItem.tax_class > "" THEN    
            BillItem.TaxClass = ttBillItem.tax_class.
        IF ttBillItem.debt_type > "" THEN    
            BillItem.DebtType = ttBillItem.debt_type.
        IF ttBillItem.profit_type > "" THEN 
            BillItem.BillType = ttBillItem.profit_type.
   
        loEventLogMaker:make_eventlog("modify",BUFFER BillItem:HANDLE).

        lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                                1,
                                                BillItem.BillCode,
                                                RECID(ttBillItem)). 
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        RELEASE BillItem.

        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("BillItem",
                                        lcModelid,
                                        "OK",
                                        ""). 

        loRequestMaker:create_log(
            SUBSTITUTE("BillItem update: Id=&1 Ext.Request=&2", 
                        lcModelid,
                        lcCorrelationID), 
                        "DEBUG").
       
    END METHOD.

    METHOD PUBLIC CHAR validate_BillItem (ilCreate AS LOG):

        IF (ilCreate OR ttBillItem.invoice_section > "") AND 
            NOT CAN-FIND(FIRST InvSect WHERE 
                               InvSect.Brand = gcBrand AND
                               InvSect.InvSect = ttBillItem.invoice_section) THEN 
            RETURN "Invalid invoice section". 
        IF (ilCreate OR ttBillItem.billing_item_group > "") AND
            NOT CAN-FIND(FIRST BItemGroup WHERE
                               BItemGroup.Brand = gcBrand AND
                               BItemGroup.BIGroup = ttBillItem.billing_item_group) THEN
            RETURN "Invalid billing item group".
        IF (ilCreate OR ttBillItem.tax_class > "") AND
            NOT CAN-FIND(FIRST TaxClass WHERE 
                               TaxClass.TaxClass = ttBillItem.tax_class) THEN
            RETURN "Invalid tax class".
        IF ttBillItem.debt_type > "" AND 
            NOT CAN-FIND(FIRST TMSCodes WHERE
                               TMSCodes.TableName = "BillItem" AND
                               TMSCodes.FieldName = "DebtType" AND
                               TMSCodes.CodeValue = ttBillItem.debt_type AND
                               TMSCodes.InUse > 0) THEN 
            RETURN "Invalid debt type". 
      
        RETURN "".    
    
    END METHOD.

   DESTRUCTOR PUBLIC BillItemModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
        IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
    END DESTRUCTOR.

END CLASS.

