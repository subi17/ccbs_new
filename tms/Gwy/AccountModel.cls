 
USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.
USING Gwy.AccountModel FROM PROPATH.

{Syst/tmsconst.i}

CLASS Gwy.AccountModel IMPLEMENTS Gwy.ModelInterface: 
    
    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lcModelid       AS CHAR              NO-UNDO.
    DEF VAR lcSource        AS CHAR              NO-UNDO.
    DEF VAR gcBrand         AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    DEF VAR lcCodeName      AS CHAR              NO-UNDO.
    DEF VAR liCodeValue     AS INT               NO-UNDO.
    
    DEF VAR lcAccType       AS CHAR              NO-UNDO.
    DEF VAR lcAcctypeCode   AS CHAR              NO-UNDO.
    DEF VAR liCount         AS CHAR              NO-UNDO.
    DEF VAR lcLkupRes       AS INT               NO-UNDO.
    
      
    DEF TEMP-TABLE ttAccount NO-UNDO SERIALIZE-NAME "Account"
        FIELD external_id  AS INT
        FIELD name         AS CHAR
        FIELD account_type AS CHAR.
        
    DEF DATASET AccountDataset FOR ttAccount.
    
    CONSTRUCTOR PUBLIC AccountModel ( iJson AS LONGCHAR,
        iModelid AS CHAR, 
        iSource  AS CHAR,
        iBrand   AS CHAR,
        iCorrelationID AS CHAR,
        iReplyTo AS CHAR):
                                   
        ASSIGN
            lJson           = iJson
            lcModelid       = iModelid
            lcSource        = iSource
            gcBrand         = iBrand
            lcCorrelationID = iCorrelationID
            loRequestMaker  = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
            loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
    END CONSTRUCTOR.
    
    METHOD PUBLIC LOG create():
 
        lJson = '~{"AccountDataset":~{"Account":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lJson) THEN
            RETURN FALSE.
       
        FIND ttAccount NO-ERROR.
        IF NOT AVAILABLE ttAccount THEN
            RETURN loRequestMaker:logError("Empty Account Data").
   
        IF ttAccount.external_id = "" THEN
            RETURN loRequestMaker:logError("Invalid Account Id").
      
        IF CAN-FIND(FIRST Account WHERE
            Account.AccNum = ttAccount.external_id) THEN
            RETURN loRequestMaker:logError("Account was already exists").
            
        create_TmsCodes(ttaccount.account_type).
             
        CREATE Account.
        ASSIGN 
            Account.AccNum  = ttAccount.external_id
            Account.AccName = ttAccount.name
            Account.AccType = lcEntryRes.       
          
        loEventLogMaker:make_eventlog("create",BUFFER Account:HANDLE).
    
        RELEASE Account.
    
        /* error cases get response via logError, make response for 
          the successful one */
        loRequestMaker:create_response("Account",
            ttAccount.external_id,
            "OK",
            "").
        loRequestMaker:create_log(
            SUBSTITUTE("Account create: Id=&1 Ext.Request=&2", 
            ttAccount.external_id,
            lcCorrelationID), 
            "DEBUG").
    
    END METHOD.
    
    METHOD PUBLIC LOG update():

        DEF VAR liWait AS INT NO-UNDO.
    
        IF lcModelid = ? THEN 
            RETURN loRequestMaker:logError('Account Number is not given').

        lJson = '~{"AccountDataset":~{"Account":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET AccountDataset BIND, lJson) THEN
            RETURN FALSE.
   
        IF lModelID = "" THEN 
            RETURN loRequestMaker:logError("Invalid Account Number").
      
        FIND ttAccount NO-ERROR.
        IF NOT AVAILABLE ttAccount THEN
            RETURN loRequestMaker:logError("Empty Account set").
       
        create_TmsCodes(ttaccount.account_type).
                
        DO WHILE TRUE:
            FIND FIRST Account WHERE
                Account.AccNum = lcModelid EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(Account) THEN 
            DO:
                liWait = liWait + 1.
                IF liWait > {&WAIT_CONFIG_LOCK} THEN 
                    RETURN loRequestMaker:logError("Account was not available for update").
                PAUSE 1 NO-MESSAGE. 
                NEXT. 
            END.
            LEAVE.
        END.
   
        IF NOT AVAILABLE Account THEN
            RETURN loRequestMaker:logError("Unknown Account").
      
        loEventLogMaker:make_eventlog("oldbuffer",BUFFER Account:HANDLE).
    
        ASSIGN 
            Account.AccName = ttAccount.name
            Account.AccType = lcEntryRes.       
        
        loEventLogMaker:make_eventlog("modify",BUFFER Account:HANDLE).
   
        RELEASE Account.  
   
        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("Account",
            lModelID,
            "OK",
            ""). 

        loRequestMaker:create_log(
            SUBSTITUTE("Account Update: Id=&1 Ext.Request=&2", 
            lModelID,
            lcCorrelationID), 
            "DEBUG").
      
    END METHOD.
    
    METHOD PUBLIC CHAR create_TmsCodes (iacc_type AS CHAR):
        
        ASSIGN 
            lcAcctype     = "receivable,payment,tax,revenue,overpayment,deposit,
                             advance_payment,rounding,discount,interest,
                             credit_loss,own_usage,payable,sales_expense,
                             reminder_fee,collection_fee,clearing,
                             direct_cost_account,report_account,doubtful_ar,
                             investigation_account,topup_balance,compensation,
                             refund_balance,relationship_costs,repair_income,
                             invoice_rejection_income,unregisterered_account_receivable,
                             discount_extension_over_q25,
                             google_purchase,q25_discount_1,q25_discount_2,
                             q25_charge_any_discount"
            lcAccTypeCode = "1,4,5,12,6,7,19,8,9,14,,24,,13,10,11,15,16,17,18,2,
                             20,21,22,23,25,26,2,43,55,60,70,79"                    
            lcLkupRes     = LOOKUP(iacc_type,lcAcctype,",")
            lcEntryRes    = ENTRY(lcLkupRes,lcAccTypeCode).  
            
        IF NOT Func.Common:mTMSCodechk("Account",
            "AccType",
            lcEntryRes) THEN 
            CREATE TMSCodes.
            ASSIGN 
                TMSCodes.TableName = "Account"
                TMSCodes.FieldName = "AccType"
                TMSCodes.codeGroup = "Accounts"
                TMSCodes.codeValue = lcEntryRes
                TMSCodes.CodeName  = ttAccount.account_type.
                        
        RETURN lcEntryRes.
                    
    END METHOD. 

    DESTRUCTOR PUBLIC AccountModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
    END DESTRUCTOR.

END CLASS.
