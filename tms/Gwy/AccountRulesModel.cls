ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.AccountRulesModel IMPLEMENTS Gwy.ModelInterface:

   DEF VAR lJson           AS LONGCHAR          NO-UNDO.
   DEF VAR lcModelid       AS CHAR              NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
   DEF VAR lcSource        AS CHAR              NO-UNDO.
   DEF VAR gcBrand         AS CHAR              NO-UNDO.
   DEF VAR lcCCRule        AS CHAR              NO-UNDO.
   DEF VAR lcError         AS CHAR              NO-UNDO.
   DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
   DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
   DEFINE TEMP-TABLE ttCCRule NO-UNDO SERIALIZE-NAME "CCRule"
      FIELD external_id      AS INT  
      FIELD category         AS CHAR 
      FIELD billing_item     AS CHAR
      FIELD valid_from       AS CHAR
      FIELD valid_to         AS CHAR
      FIELD domestic_account AS INT
      FIELD eu_comp_account  AS INT
      FIELD eu_cons_account  AS INT
      FIELD foreign_account  AS INT
      FIELD cost_center      AS CHAR INIT ?
      FIELD reporting_id     AS CHAR INIT ?
      FIELD cc_validfrom     AS DATE 
      FIELD cc_validto       AS DATE.
   
   DEFINE DATASET CCRuleDataSet FOR ttCCRule.    


   CONSTRUCTOR PUBLIC AccountRulesModel(iJson    AS LONGCHAR,
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
     
      lcJson = '~{"CCRuleDataSet":~{"CCRule":~[' + lcJson + '~]~}~}'.
      IF loRequestMaker:parser_error(DATASET CCRuleDataset BIND, lcJson) THEN
         RETURN FALSE.
       
      FIND ttCCRule NO-ERROR.
      IF NOT AVAILABLE ttCCRule THEN
         RETURN loRequestMaker:logError("Empty CCRule set").
    
      lcError = validate_rule(ttCCRule.external_id,TRUE).      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
    
      CREATE CCRule.
      ASSIGN 
         CCRule.Brand        = gcBrand
         CCRule.CCRuleID     = STRING(NEXT-VALUE(CCRuleID))
         CCRule.BillCode     = ttCCRule.billing_item
         CCRule.Category     = ttCCRule.category
         CCRule.ValidFrom    = ttCCRule.cc_validfrom
         CCRule.ValidTo      = ttCCRule.cc_validto
         CCRule.AccNum       = ttCCRule.domestic_account
         CCRule.EUAccNum     = ttCCRule.eu_comp_account
         CCRule.EUConAccNum  = ttCCRule.eu_cons_account
         CCRule.FSAccNum     = ttCCRule.foreign_account
         CCRule.CostCentre   = ttCCRule.cost_center 
                                  WHEN ttCCRule.cost_center NE ?
         CCRule.ReportingID  = ttCCRule.reporting_id 
                                  WHEN ttCCRule.reporting_id NE ?.
          
      loEventLogMaker:make_eventlog("create",BUFFER CCRule:HANDLE).
          
      RELEASE CCRule.
    
      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN
      loRequestMaker:create_response("CCRule",
                                     lcCCRule,
                                     "OK",
                                     ""). 
      &IF {&MQ_BASED} 
      &THEN
      loRequestMaker:create_log(
         SUBSTITUTE("CCRule create: Id=&1,&2 Ext.Request=&3", 
         lcCCRule,
         ttCCRule.external_id,
         lcCorrelationID), 
         "DEBUG").
      &ENDIF  
     
   END METHOD.

   METHOD PUBLIC LOG update():
      
      DEF VAR liWait AS INT  NO-UNDO.
       
      IF lcModelid = ? THEN 
         RETURN loRequestMaker:logError('CCRule ID not given').
    
      lcJson = '~{"CCRuleDataSet":~{"CCRule":~[' + lcJson + '~]~}~}'.
      IF loRequestMaker:parser_error(DATASET CCRuleDataset BIND, lJson) THEN
         RETURN FALSE.
    
      FIND ttCCRule NO-ERROR.
      IF NOT AVAILABLE ttCCRule THEN
         RETURN loRequestMaker:logError("Empty CCRule set").
     
      lcError = validate_rule(lcModelid,
                              FALSE).      
      IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
    
      DO WHILE TRUE:
         FIND FIRST CCRule WHERE 
                    CCRule.CCRuleID = lcCCRule EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
         IF LOCKED(CCRule) THEN DO:
            liWait = liWait + 1.
            IF liWait > "{&WAIT_CONFIG_LOCK}" THEN 
               RETURN loRequestMaker:logError("Rule was not available for update").
            PAUSE 1 NO-MESSAGE. 
            NEXT. 
         END.
         LEAVE.
      END.
     
      IF NOT AVAILABLE CCRule THEN 
         RETURN loRequestMaker:logError("Unknown CCRule").
          
      loEventLogMaker:make_eventlog("oldbuffer",BUFFER CCRule:HANDLE).
    
      IF ttCCRule.cc_validfrom NE ? THEN CCRule.ValidFrom = ttCCRule.cc_validfrom.
      IF ttCCRule.cc_validto NE ? THEN CCRule.ValidTo = ttCCRule.cc_validto.
      IF ttCCRule.domestic_account > 0 THEN CCRule.AccNum = ttCCRule.domestic_account.
      IF ttCCRule.eu_comp_account > 0 THEN CCRule.EUAccNum = ttCCRule.eu_comp_account.
      IF ttCCRule.eu_cons_account > 0 THEN CCRule.EUConAccNum = ttCCRule.eu_cons_account.
      IF ttCCRule.foreign_account > 0 THEN CCRule.FSAccNum = ttCCRule.foreign_account.
      IF ttCCRule.cost_center NE ? THEN CCRule.CostCentre = ttCCRule.cost_center.
      IF ttCCRule.reporting_id NE ? THEN CCRule.ReportingId = ttCCRule.reporting_id.
       
      loEventLogMaker:make_eventlog("modify",BUFFER CCRule:HANDLE).
       
      RELEASE CCRule.
    
      /* error cases get response via logError, make response for 
         the succesful one */
      IF lcCorrelationID > "" THEN
      loRequestMaker:create_response("CCRule",
                                      lcCCRule,
                                      "OK",
                                      ""). 
      &IF {&MQ_BASED} 
      &THEN 
      loRequestMaker:create_log(
         SUBSTITUTE("CCRule update: Id=&1/&2 Ext.Request=&3", 
         lcCCRule,
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

   METHOD PUBLIC CHAR validate_rule(icExternalID AS CHAR,
                                     ilCreate AS LOG):
    
      DEF BUFFER bRule FOR CCRule.
    
      IF icExternalID = "" THEN 
         RETURN "Invalid rule ID". 
          
      lcCCRule = icExternalID.
      IF lcCCRule > "" THEN 
      
      IF CAN-FIND(FIRST CCRule WHERE 
                        CCRule.Brand = gcBrand
                        CCRule.CCRuleId = icExternalID) THEN DO:
         IF ilCreate THEN RETURN "Rule already exists".
      END.
      ELSE IF NOT ilCreate THEN RETURN "Unknown rule".
       
      IF ttCCRule.valid_from > "" THEN 
         ttCCRule.cc_validfrom = loRequestMaker:make_date(
                                    loRequestMaker:make_timestamp(ttCCRule.valid_from))
         NO-ERROR.                           
      
      IF ttCCRule.valid_to > "" THEN                              
         ttCCRule.cc_validto = loRequestMaker:make_date(
                                  loRequestMaker:make_timestamp(ttCCRule.valid_to))
         NO-ERROR.
      
      IF ilCreate AND (ttCCRule.cc_validfrom = ? OR ttCCRule.cc_validto = ?) THEN 
         RETURN "Invalid rule period". 
    
      IF ttCCRule.billing_item > "" THEN DO:
         IF NOT CAN-FIND(FIRST BillItem WHERE 
                               BillItem.Brand = gcBrand AND
                               BillItem.BillCode = ttCCRule.billing_item) THEN 
         RETURN "Invalid billing item".
      END.
      ELSE IF ilCreate THEN     
         RETURN "Invalid billing item".
    
      IF ttCCRule.category > "" THEN DO:
         IF NOT CAN-FIND(FIRST CustCat WHERE 
                               CustCat.Brand = gcBrand AND
                               CustCat.Category = ttCCRule.category) THEN 
            RETURN "Invalid category".
      END.
      ELSE IF ilCreate THEN 
         RETURN "Invalid category".
    
      /* check unique index with to date */
      IF ttCCRule.cc_validto NE ? THEN DO:
         IF ilCreate AND 
            CAN-FIND(FIRST bRule WHERE
                           bRule.Brand = gcBrand AND
                           bRule.Category = ttCCRule.category AND
                           bRule.BillCode = ttCCRule.billing_item AND
                           bRule.ValidTo = ttCCRule.cc_validto) THEN 
            RETURN "Rule with given key values already exists".
         ELSE IF NOT ilCreate THEN DO: 
            FIND FIRST CCRule WHERE CCRule.CCRuleID = lcCCRule NO-LOCK NO-ERROR.
            IF AVAILABLE CCRule AND 
               CAN-FIND(FIRST bRule USE-INDEX Category WHERE
                              bRule.Brand = gcBrand AND
                              bRule.Category = CCRule.Category AND
                              bRule.BillCode = CCRule.BillCode AND
                              bRule.ValidTo = ttCCRule.cc_validto AND
                              bRule.CCRuleID NE lcCCRule) THEN 
               RETURN "Rule with given end date already exists".
         END.     
      END.
        
      IF ttCCRule.domestic_account > 0 AND 
         NOT CAN-FIND(FIRST Account WHERE 
                            Account.Brand = gcBrand AND
                            Account.AccNum = ttCCRule.domestic_account) THEN
         RETURN "Invalid domestic account".
      
      IF ttCCRule.eu_comp_account > 0 AND 
         NOT CAN-FIND(FIRST Account WHERE 
                            Account.Brand = gcBrand AND
                            Account.AccNum = ttCCRule.eu_comp_account) THEN
         RETURN "Invalid EU company account".
      
      IF ttCCRule.eu_cons_account > 0 AND 
         NOT CAN-FIND(FIRST Account WHERE 
                            Account.Brand = gcBrand AND
                            Account.AccNum = ttCCRule.eu_cons_account) THEN
         RETURN "Invalid EU consumer account".
      
      IF ttCCRule.foreign_account > 0 AND 
         NOT CAN-FIND(FIRST Account WHERE 
                            Account.Brand = gcBrand AND
                            Account.AccNum = ttCCRule.foreign_account) THEN
         RETURN "Invalid foreign account".
      
      IF ttCCRule.cost_center NE ? AND 
         NOT CAN-FIND(FIRST CostCentre WHERE 
                            CostCentre.Brand = gcBrand AND
                            CostCentre.CostCentre = ttCCRule.cost_center) THEN
         RETURN "Invalid cost center".
    
      RETURN "".    
        
   END METHOD.

   DESTRUCTOR PUBLIC AccountRulesModel ():
      IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
      IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
   END DESTRUCTOR.

END CLASS.

