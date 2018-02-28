ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.BillItemModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR loRequestMaker  AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
DEF VAR loRepTextMaker  AS Gwy.RepTextMaker NO-UNDO.
DEF VAR lcError         AS CHAR NO-UNDO. 
DEF VAR liWait          AS INT  NO-UNDO.
     
DEF TEMP-TABLE ttBillItem NO-UNDO SERIALIZE-NAME "BillItem"
   FIELD external_id AS CHAR 
   FIELD name AS CHAR 
   FIELD invoice_section AS CHAR
   FIELD display_order AS INT
   FIELD billing_item_group AS CHAR
   FIELD tax_class AS CHAR
   FIELD debt_type AS CHAR.

DEF TEMP-TABLE ttBillItemRule NO-UNDO SERIALIZE-NAME "item_rule"
   FIELD parent_id AS RECID
   FIELD rule_type AS CHAR
   FIELD valid_from AS CHAR
   FIELD valid_to AS CHAR 
   FIELD substitute_billing_item AS CHAR INIT ?
   FIELD billing_text AS CHAR INIT ?
   FIELD billing_text_rule AS CHAR INIT ?
   FIELD bi_validfrom AS DATE
   FIELD bi_validto AS DATE
   FIELD bi_ruletype AS CHAR.
   
{Gwy/RepText_definition.i}
  
DEFINE DATASET BillItemDataSet FOR ttBillItem, ttRepText, ttBillItemRule
   PARENT-ID-RELATION idparent FOR ttBillItem, ttRepText
      PARENT-ID-FIELD parent_id
   PARENT-ID-RELATION idparent FOR ttBillItem, ttBillItemRule
      PARENT-ID-FIELD parent_id.


CONSTRUCTOR PUBLIC BillItemModel ( iJson    AS LONGCHAR,
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
   
   lcError = validate_billitemrule(RECID(ttBillItem),
                                   ttBillItem.external_id).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
      
   CREATE BillItem.
   ASSIGN 
      BillItem.Brand = gcBrand
      BillItem.BillCode = ttBillItem.external_id
      BillItem.BIName = ttBillItem.name
      BillItem.InvSect = ttBillItem.invoice_section
      BillItem.ISOrder = ttBillItem.display_order
      BillItem.BIGroup = ttBillItem.billing_item_group
      BillItem.TaxClass = ttBillItem.tax_class
      BillItem.DebtType = ttBillItem.debt_type.

   loEventLogMaker:make_eventlog("create",BUFFER BillItem:HANDLE).

   lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                           1,
                                           BillItem.BillCode,
                                           RECID(ttBillItem)). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   lcError = update_billitemrule(RECID(ttBillItem),
                                 BillItem.BillCode). 
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
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():

   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('BillItem ID not given').

   lJson = '~{"BillItemDataSet":~{"BillItem":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET BillItemDataset BIND, lJson) THEN
       RETURN FALSE.
   
   IF lModelID = "" THEN 
      RETURN loRequestMaker:logError("Invalid BillItem ID").
      
   FIND ttBillItem NO-ERROR.
   IF NOT AVAILABLE ttBillItem THEN
       RETURN loRequestMaker:logError("Empty BillItem set").
       
   DO WHILE TRUE:
      FIND FIRST BillItem WHERE 
                 BillItem.Brand = gcBrand AND
                 BillItem.BillCode = lModelID EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED(BillItem) THEN DO:
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

   lcError = validate_billitemrule(RECID(ttBillItem),
                                   BillItem.BillCode).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
        
   loEventLogMaker:make_eventlog("oldbuffer",BUFFER BillItem:HANDLE).

   IF ttBillItem.name > "" THEN BillItem.BIName = ttBillItem.name.
   IF ttBillItem.invoice_section > "" THEN 
      BillItem.InvSect = ttBillItem.invoice_section.
   IF ttBillItem.display_order > 0 THEN 
      BillItem.ISOrder = ttBillItem.display_order.
   IF ttBillItem.billing_item_group > "" THEN    
      BillItem.BIGroup = ttBillItem.billing_item_group.
   IF ttBillItem.tax_class > "" THEN    
      BillItem.TaxClass = ttBillItem.tax_class.
   IF ttBillItem.debt_type > "" THEN    
      BillItem.DebtType = ttBillItem.debt_type.
   
   loEventLogMaker:make_eventlog("modify",BUFFER BillItem:HANDLE).

   lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                           1,
                                           BillItem.BillCode,
                                           RECID(ttBillItem)). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   lcError = update_billitemrule(RECID(ttBillItem),
                                 BillItem.BillCode). 
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   RELEASE BillItem.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("BillItem",
                                  lModelId,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("BillItem update: Id=&1 Ext.Request=&2", 
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

METHOD PUBLIC CHAR validate_BillItemRule (irParentID AS RECID,
                                          icBillCode AS CHAR):

   DEF BUFFER bSubstItem FOR BillItem.
   DEF BUFFER bItemRule FOR BillItemRule.
   
   DEF VAR liText AS INT  NO-UNDO.
   
   FOR EACH ttBillItemRule WHERE
        ttBillItemRule.parent_id = irParentID:
        
      ASSIGN 
         ttBillItemRule.bi_validfrom = loRequestMaker:make_date(
                                     loRequestMaker:make_timestamp(ttBillItemRule.valid_from))
         ttBillItemRule.bi_validto  = loRequestMaker:make_date(
                                    loRequestMaker:make_timestamp(ttBillItemRule.valid_to))
         NO-ERROR.
      IF ttBillItemRule.bi_validfrom = ? OR ttBillItemRule.bi_validto = ? THEN 
         RETURN "Invalid item rule period". 
         
      IF ttBillItemRule.rule_type > "" THEN 
         ttBillItemRule.bi_ruletype = loRequestMaker:get_tmscodes_bssapi_internalid(
                                         "BillItemRule",
                                         "RuleType",
                                         ttBillItemRule.rule_type).
      IF ttBillItemRule.bi_ruletype = "" THEN 
         RETURN "Invalid rule type".

      IF CAN-FIND(FIRST BillItemRule WHERE
                        BillItemRule.BillCode = icBillCode AND
                        BillItemRule.RuleType = ttBillItemRule.bi_ruletype AND
                        BillItemRule.ValidTo >= ttBillItemRule.bi_validfrom AND
                        BillItemRule.ValidFrom <= ttBillItemRule.bi_validto AND
                        BillItemRule.ValidFrom NE ttBillItemRule.bi_validfrom) THEN 
         RETURN "Billing item rule with overlapping period exists".

      IF ttBillItemRule.substitute_billing_item > "" AND 
         NOT CAN-FIND(FIRST bSubstItem WHERE
                            bSubstItem.Brand = gcBrand AND
                            bSubstItem.BillCode = ttBillItemRule.substitute_billing_item) THEN
         RETURN "Invalid substitute item".
         
      IF ttBillItemRule.billing_text > "" THEN DO: 
      
         liText = INT(ttBillItemRule.billing_text) NO-ERROR.
         IF liText = 0 OR 
            NOT CAN-FIND(FIRST HdrText WHERE 
                               HdrText.Brand = gcBrand AND
                               HdrText.te-nro = liText) THEN 
         RETURN "Invalid billing text".                      
      END.
         
      IF LOOKUP(ttBillItemRule.billing_text_rule,",substitute,additional") = 0 THEN
         RETURN "Invalid billing text rule".
         
      IF CAN-FIND(FIRST bItemRule WHERE 
                        bItemRule.BillCode  = icBillCode AND
                        bItemRule.RuleType  = ttBillItemRule.bi_ruletype AND
                        bItemRule.ValidTo   = ttBillItemRule.bi_validto AND
                        bItemRule.ValidFrom NE ttBillItemRule.bi_validfrom) THEN
         RETURN "Item rule period end date is already in use".
   END.

   RETURN "". 
 
END METHOD.

METHOD PUBLIC CHAR update_BillItemRule (irParentID AS RECID,
                                       icBillCode AS CHAR):
   DEF VAR lrRule AS RECID NO-UNDO.

   FOR EACH ttBillItemRule WHERE ttBillItemRule.parent_id = irParentID:
   
      FIND FIRST BillItemRule WHERE
                 BillItemRule.BillCode = icBillCode AND
                 BillItemRule.RuleType = ttBillItemRule.bi_ruletype AND
                 BillItemRule.ValidFrom = ttBillItemRule.bi_validfrom 
         NO-LOCK NO-ERROR.

      /* if exists with given attributes then just update */
      IF AVAILABLE BillItemRule THEN DO: 
         ASSIGN 
            liWait = 0
            lrRule = RECID(BillItemRule).
         DO WHILE TRUE:
            /* 'find current' doesn't work with this, it doesn't get the record 
                when it's released, so use recid find  */
            FIND FIRST BillItemRule WHERE RECID(BillItemRule) = lrRule
                 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(BillItemRule) THEN DO:
               liWait = liWait + 1.
                /* billing item may have already been partially updated but the entire 
                   update can be resent */
               IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                  RETURN "Item rule was not available for update".
               PAUSE 1 NO-MESSAGE. 
               NEXT. 
            END.
            LEAVE.
         END.

         loEventLogMaker:make_eventlog("oldbuffer",BUFFER BillItemRule:HANDLE).
         BillItemRule.ValidTo = ttBillItemRule.bi_validto.
         IF ttBillItemRule.substitute_billing_item NE ? THEN 
            BillItemRule.SubstBillCode = ttBillItemRule.substitute_billing_item.
         IF ttBillItemRule.billing_text NE ? THEN 
            BillItemRule.RuleText = INT(ttBillItemRule.billing_text).
         IF ttBillItemRule.billing_text_rule NE ? THEN 
            BillItemRule.UseText = INT(ttBillItemRule.billing_text_rule = "substitute").
         loEventLogMaker:make_eventlog("modify",BUFFER BillItemRule:HANDLE).   
         RELEASE BillItemRule.
      END.
      
      ELSE DO:
         /* new entry */
         CREATE BillItemRule.
         ASSIGN
            BillItemRule.BillCode  = icBillCode 
            BillItemRule.RuleType  = ttBillItemRule.bi_ruletype 
            BillItemRule.ValidFrom = ttBillItemRule.bi_validfrom
            BillItemRule.ValidTo   = ttBillItemRule.bi_validto.
         IF ttBillItemRule.substitute_billing_item NE ? THEN 
            BillItemRule.SubstBillCode = ttBillItemRule.substitute_billing_item.
         IF ttBillItemRule.billing_text NE ? THEN 
            BillItemRule.RuleText = INT(ttBillItemRule.billing_text).
         IF ttBillItemRule.billing_text_rule NE ? THEN 
            BillItemRule.UseText = INT(ttBillItemRule.billing_text_rule = "replace").
   
         loEventLogMaker:make_eventlog("create",BUFFER BillItemRule:HANDLE).

         RELEASE BillItemRule.
      END.   
   END.   
   
   RETURN "". 

END METHOD. 
 
DESTRUCTOR PUBLIC BillItemModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
END DESTRUCTOR.

END CLASS.

