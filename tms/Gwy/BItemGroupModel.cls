
USING Progress.Lang.*.
&GLOBAL-DEFINE CONFIG_ID_MAX 32.
ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.BItemGroupModel IMPLEMENTS Gwy.ModelInterface: 
    
    DEF VAR lJson           AS LONGCHAR          NO-UNDO.
    DEF VAR lcModelid       AS CHAR              NO-UNDO.
    DEF VAR lcCorrelationID AS CHAR              NO-UNDO.
    DEF VAR gcBrand         AS CHAR              NO-UNDO.
    DEF VAR loRequestMaker  AS Gwy.RequestMaker  NO-UNDO.
    DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    DEF VAR loRepTextMaker  AS Gwy.RepTextMaker  NO-UNDO.
    DEF VAR loCommon        AS Func.Common       NO-UNDO.
    DEF VAR lcError         AS CHAR              NO-UNDO. 
    DEF VAR lcBiGroup       AS CHAR              NO-UNDO.
    
    DEF TEMP-TABLE ttBItemGroup NO-UNDO SERIALIZE-NAME "BItemGroup"
        FIELD external_id   AS CHAR 
        FIELD name          AS CHAR 
        FIELD display_order AS INT
        FIELD group_type    AS INT.

    {Gwy/RepText_definition.i}

    DEFINE DATASET BItemGroupDataSet FOR ttBItemGroup, ttRepText
        PARENT-ID-RELATION idparent FOR ttBItemGroup, ttRepText
        PARENT-ID-FIELD parent_id.


    CONSTRUCTOR PUBLIC BItemGroupModel ( iJson          AS LONGCHAR,
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
            loCommon        = NEW Func.Common().
   
    END CONSTRUCTOR.

    METHOD PUBLIC LOG create():
 
        lJson = '~{"BItemGroupDataSet":~{"BItemGroup":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET BItemGroupDataset BIND, lJson) THEN
            RETURN FALSE.
   
        FIND ttBItemGroup NO-ERROR.
        IF NOT AVAILABLE ttBItemGroup THEN
            RETURN loRequestMaker:logError("Empty BItemGroup set").

        IF ttBItemGroup.external_id = "" THEN 
            RETURN loRequestMaker:logError("Invalid BItemGroup ID").
        
        IF CAN-FIND(FIRST BItemGroup WHERE 
                          BItemGroup.Brand = gcBrand AND
                          BItemGroup.BIGroup = loCommon:mTMSCodeValue("BItemGroup",
                                                                      "BillGroup",
                                                                      ttBItemGroup.external_id)) THEN 
            RETURN loRequestMaker:logError("BItemGroup already exists").

        IF LENGTH(ttBItemGroup.external_id) > "{&CONFIG_ID_MAX}" THEN 
            RETURN loRequestMaker:logError("Max length for ID is {&CONFIG_ID_MAX} characters").
      
        lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                  6,
                                                  ttBItemGroup.external_id,
                                                  RECID(ttBItemGroup)).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
        
        CREATE BItemGroup.
        ASSIGN 
            BItemGroup.Brand        = gcBrand
            BItemGroup.BIGroup      = loCommon:mTMSCodeValue("BItemGroup",
                                                             "BillGroup",
                                                             ttBItemGroup.external_id)
            BItemGroup.BIGName      = ttBItemGroup.name
            BItemGroup.InvoiceOrder = ttBItemGroup.display_order
                                         WHEN ttBItemGroup.display_order > ""
            BItemGroup.GroupType    = loCommon:mTMSCodeValue("BItemGroup",
                                                             "BillGroup",
                                                             ttBItemGroup.group_type).

        loEventLogMaker:make_eventlog("create",BUFFER BItemGroup:HANDLE).

        lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                                6,
                                                BItemGroup.BIGroup,
                                                RECID(ttBItemGroup)). 
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
     
        RELEASE BItemGroup.

        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("BItemGroup",
                                       ttBItemGroup.external_id,
                                       "OK",
                                       ""). 

        loRequestMaker:create_log(
                                   SUBSTITUTE("BItemGroup create: Id=&1 Ext.Request=&2", 
                                   ttBItemGroup.external_id,
                                   lcCorrelationID), 
                                   "DEBUG").
 
    END METHOD.

    METHOD PUBLIC LOG update():

        DEF VAR liWait AS INT NO-UNDO.
   
        IF lcModelid = ? THEN 
            RETURN loRequestMaker:logError('BItemGroup ID not given').

        lJson = '~{"BItemGroupDataSet":~{"BItemGroup":' + lJson + '~}~}'.
        IF loRequestMaker:parser_error(DATASET BItemGroupDataset BIND, lJson) THEN
            RETURN FALSE.
   
        IF lcModelid = "" THEN 
            RETURN loRequestMaker:logError("Invalid BItemGroup ID").
      
        FIND ttBItemGroup NO-ERROR.
        IF NOT AVAILABLE ttBItemGroup THEN
            RETURN loRequestMaker:logError("Empty BItemGroup set").
            
        ASSIGN 
            lcBiGroup = loCommon:mTMSCodeValue("BItemGroup",
                                               "BillGroup",
                                               ttBItemGroup.external_id).

        DO WHILE TRUE:
            FIND FIRST BItemGroup WHERE 
                       BItemGroup.Brand   = gcBrand AND
                       BItemGroup.BIGroup = lcBiGroup 
                       EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED(BItemGroup) THEN 
            DO:
                liWait = liWait + 1.
                IF liWait > "{&WAIT_CONFIG_LOCK}" THEN 
                    RETURN loRequestMaker:logError("Billing item group was not available for update").
                PAUSE 1 NO-MESSAGE. 
                NEXT. 
            END.
            LEAVE.
        END.

        IF NOT AVAILABLE BItemGroup THEN 
            RETURN loRequestMaker:logError("Unknown BItemGroup").

        lcError = loRepTextMaker:validate_reptext(TABLE ttRepText BIND,
                                                  6,
                                                  BItemGroup.BIGroup,
                                                  RECID(ttBItemGroup)).
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).
       
        loEventLogMaker:make_eventlog("oldbuffer",BUFFER BItemGroup:HANDLE).

        BItemGroup.BIGName = ttBItemGroup.name
                                 WHEN ttBItemGroup.name > "".
        BItemGroup.InvoiceOrder = ttBItemGroup.display_order
                                      WHEN ttBItemGroup.display_order > 0.
        IF LOOKUP(ttBItemGroup.group_type,"mobile,fixedline,broadband") > 0 THEN 
            BItemGroup.GroupType = ttBItemGroup.group_type. 

        loEventLogMaker:make_eventlog("modify",BUFFER BItemGroup:HANDLE).
   
        lcError = loRepTextMaker:update_reptext(TABLE ttRepText BIND,
                                                6,
                                                BItemGroup.BIGroup,
                                                RECID(ttBItemGroup)). 
        IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

        RELEASE BItemGroup.

        /* error cases get response via logError, make response for 
           the succesful one */
        loRequestMaker:create_response("BItemGroup",
                                        lcModelid,
                                        "OK",
                                        ""). 

        loRequestMaker:create_log(
                                  SUBSTITUTE("BItemGroup update: Id=&1 Ext.Request=&2", 
                                  lcModelid,
                                  lcCorrelationID), 
                                  "DEBUG").
       
    END METHOD.
    /*
        METHOD PUBLIC LOG makeOtherAction(icAction AS CHAR):
            CASE icAction:
                WHEN "update_other" THEN process_other_update().
                OTHERWISE 
                RETURN loRequestMaker:logError(SUBST("Unknown action &1",icAction)).
            END CASE.
        END METHOD.
    
        METHOD PUBLIC LOG process_other_update():
    
        END METHOD.*/

    DESTRUCTOR PUBLIC BItemGroupModel ():
        IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
        IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
        IF VALID-OBJECT(loRepTextMaker) THEN DELETE OBJECT loRepTextMaker.
    END DESTRUCTOR.

END CLASS.