ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Gwy.ModelInterface.

{Syst/tmsconst.i}

CLASS Gwy.PriceListModel IMPLEMENTS Gwy.ModelInterface:

DEF VAR lJson          AS LONGCHAR NO-UNDO.
DEF VAR lModelid       AS CHAR NO-UNDO.
DEF VAR lCorrelationID AS CHAR NO-UNDO.
DEF VAR gcBrand        AS CHAR NO-UNDO.
DEF VAR lcError        AS CHAR NO-UNDO. 
DEF VAR loRequestMaker AS Gwy.RequestMaker NO-UNDO.
DEF VAR loEventLogMaker AS Gwy.EventLogMaker NO-UNDO.
    
DEFINE TEMP-TABLE ttPriceList NO-UNDO SERIALIZE-NAME "PriceList"
   FIELD external_id AS CHAR 
   FIELD name AS CHAR
   FIELD currency AS CHAR INIT ?
   FIELD currency_unit AS CHAR
   FIELD tax_included AS CHAR INIT ?
   FIELD rounding AS INT INIT ?.
   
DEFINE DATASET PriceListDataSet FOR ttPriceList.    

CONSTRUCTOR PUBLIC PriceListModel ( iJson    AS LONGCHAR,
                                  iModelid AS CHAR, 
                                  iSource  AS CHAR,
                                  iCorrelationID AS CHAR,
                                  iReplyTo AS CHAR):

   ASSIGN
      gcBrand        = "1"
      lJson          = iJson
      lModelid       = iModelid
      lCorrelationID = iCorrelationID
      loRequestMaker = NEW Gwy.RequestMaker(iCorrelationID, iReplyTo, iSource)
      loEventLogMaker = NEW Gwy.EventLogMaker(iSource).
   
END CONSTRUCTOR.

METHOD PUBLIC LOG create():
 
   lJson = '~{"PriceListDataSet":~{"PriceList":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET PriceListDataset BIND, lJson) THEN
       RETURN FALSE.
   
   FIND ttPriceList NO-ERROR.
   IF NOT AVAILABLE ttPriceList THEN
       RETURN loRequestMaker:logError("Empty PriceList set").

   IF ttPriceList.external_id = "" THEN 
      RETURN loRequestMaker:logError("Invalid PriceList ID").

   IF LENGTH(ttPriceList.external_id) > {&CONFIG_ID_MAX} THEN 
      RETURN loRequestMaker:logError(
                "Max length for ID is {&CONFIG_ID_MAX} characters").
         
   IF CAN-FIND(FIRST PriceList WHERE 
                     PriceList.Brand = gcBrand AND
                     PriceList.PriceList = ttPriceList.external_id) THEN 
      RETURN loRequestMaker:logError("PriceList already exists").
      
   lcError = validate_pricelist(TRUE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   CREATE PriceList.
   ASSIGN 
      PriceList.Brand  = gcBrand
      PriceList.PriceList = ttPriceList.external_id
      PriceList.CustNum = 0
      PriceList.PLName = ttPriceList.name
      PriceList.Currency = ttPriceList.currency
      PriceList.CurrUnit = (ttPriceList.currency_unit = "full") 
                              WHEN ttPriceList.currency_unit > "" 
      PriceList.InclVat  = (LOOKUP(ttPriceList.tax_included,{&BOOLEAN_TRUE}) > 0)
      PriceList.Rounding = ttPriceList.rounding.

   loEventLogMaker:make_eventlog("create",BUFFER PriceList:HANDLE).
      
   RELEASE PriceList.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("PriceList",
                                  ttPriceList.external_id,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("PriceList create: Id=&1 Ext.Request=&2", 
                 ttPriceList.external_id,
                 lCorrelationID), 
      "DEBUG").
 
END METHOD.

METHOD PUBLIC LOG update():
  
   DEF VAR liWait AS INT  NO-UNDO.
   
   IF lModelid = ? THEN 
      RETURN loRequestMaker:logError('PriceList ID not given').

   lJson = '~{"PriceListDataSet":~{"PriceList":' + lJson + '~}~}'.
   IF loRequestMaker:parser_error(DATASET PriceListDataset BIND, lJson) THEN
       RETURN FALSE.
   
   IF lModelID = "" THEN 
      RETURN loRequestMaker:logError("Invalid PriceList ID").
      
   FIND ttPriceList NO-ERROR.
   IF NOT AVAILABLE ttPriceList THEN
       RETURN loRequestMaker:logError("Empty PriceList set").
       
   DO WHILE TRUE:
      FIND FIRST PriceList WHERE 
                 PriceList.Brand = gcBrand AND
                 PriceList.PriceList = lModelID AND
                 PriceList.CustNum = 0 EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED(PriceList) THEN DO:
         liWait = liWait + 1.
         IF liWait > {&WAIT_CONFIG_LOCK} THEN 
            RETURN loRequestMaker:logError("Pricelist was not available for update").
         PAUSE 1 NO-MESSAGE. 
         NEXT. 
      END.
      LEAVE.
   END.
   
   IF NOT AVAILABLE PriceList THEN 
      RETURN loRequestMaker:logError("Unknown PriceList").

   lcError = validate_pricelist(FALSE).
   IF lcError > "" THEN RETURN loRequestMaker:logError(lcError).

   loEventLogMaker:make_eventlog("oldbuffer",BUFFER PriceList:HANDLE).

   ASSIGN 
      PriceList.PLName = ttPriceList.name WHEN ttPriceList.name > ""
      PriceList.Currency = ttPriceList.currency WHEN ttPriceList.currency NE ?
      PriceList.CurrUnit = (ttPriceList.currency_unit = "full") 
                              WHEN ttPriceList.currency_unit > "" 
      PriceList.InclVat  = (LOOKUP(ttPriceList.tax_included,{&BOOLEAN_TRUE}) > 0) 
                              WHEN ttPriceList.tax_included NE ?
      PriceList.Rounding = ttPriceList.rounding WHEN ttPriceList.rounding NE ?.

   loEventLogMaker:make_eventlog("modify",BUFFER PriceList:HANDLE).
   
   RELEASE PriceList.

   /* error cases get response via logError, make response for 
      the succesful one */
   loRequestMaker:create_response("PriceList",
                                  lModelID,
                                  "OK",
                                  ""). 

   loRequestMaker:create_log(
      SUBSTITUTE("PriceList update: Id=&1 Ext.Request=&2", 
                 lModelID,
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

METHOD PUBLIC CHAR validate_pricelist(ilCreate AS LOG):
 
   IF (ilCreate OR ttPriceList.Currency > "") AND 
      NOT CAN-FIND(FIRST Currency WHERE Currency.Currency = ttPriceList.Currency) THEN
         RETURN "Invalid currency".
       
   IF LOOKUP(ttPriceList.currency_unit,",full,sub") = 0 THEN 
      RETURN "Invalid currency unit".
      
   IF (ilCreate OR ttPriceList.tax_included > "") AND
      LOOKUP(ttPriceList.tax_included,{&BOOLEAN_TRUE}) = 0 AND
      LOOKUP(ttPriceList.tax_included,{&BOOLEAN_FALSE}) = 0 THEN 
         RETURN "Invalid tax included value".
         
   IF ttPriceList.rounding > 6 THEN 
      RETURN "Invalid rounding value".
 
END METHOD.

DESTRUCTOR PUBLIC PriceListModel ():
   IF VALID-OBJECT(loRequestMaker) THEN DELETE OBJECT loRequestMaker.
   IF VALID-OBJECT(loEventLogMaker) THEN DELETE OBJECT loEventLogMaker.
END DESTRUCTOR.

END CLASS.
