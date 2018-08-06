
/*------------------------------------------------------------------------
    File        : SAPC_Change_API.i
    Purpose     : Building the Json message for SAPC Change API Interface 
                  
    Syntax      :

    Description : 
    
    Note        : This include needs the below in the program where it is 
                  inserted: 
                    USING Progress.Json.ObjectModel.JsonObject.
                    USING Progress.Json.ObjectModel.JsonArray .
                    {Func/cparam2.i}
                    
                  These statements are not included here because several .i 
                  files could use them and, including them in all the .i files, 
                  will mean the statements are duplicated, triplicated, ... if 
                  two or more of the .i files are included in the same program 

    Author(s)   : Diego Pastrana
    Created     : Wed Jul 18 18:06:26 CEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEF TEMP-TABLE ttOrder SERIALIZE-NAME "Order"
   FIELD orderId     AS INT
   FIELD orderType   AS CHAR
   FIELD sellChannel AS CHAR 
   FIELD sellDate    AS DATETIME-TZ 
   FIELD seller      AS CHAR 
   FIELD createdBy   AS CHAR 
   FIELD createdDate AS DATETIME-TZ.   

DEF TEMP-TABLE ttOutService SERIALIZE-NAME "Services"
   FIELD orderid     AS INT  
   FIELD type        AS CHAR 
   FIELD serviceId   AS CHAR 
   FIELD quantity    AS CHAR
   FIELD action      AS CHAR. 

DEF TEMP-TABLE ttInService SERIALIZE-NAME "Services"
   FIELD orderId       AS INT
   FIELD outtype       AS CHAR 
   FIELD outserviceId  AS CHAR 
   FIELD type          AS CHAR 
   FIELD serviceName   AS CHAR 
   FIELD quantity      AS CHAR
   FIELD action        AS CHAR. 
   
DEF TEMP-TABLE ttCharacteristic SERIALIZE-NAME "Characteristics"
   FIELD orderid      AS INT 
   FIELD type         AS CHAR 
   FIELD serviceName  AS CHAR 
   FIELD name         AS CHAR 
   FIELD valueamt     AS CHAR.
   
DEF TEMP-TABLE ttLine SERIALIZE-NAME "Line"
   FIELD orderid     AS INT
   FIELD type        AS CHAR 
   FIELD serviceId   AS CHAR 
   FIELD phonenumber AS CHAR.

/* Developer note: 
   - So far there is only one record in each of these temp-tables
   - If this changes, here it goes the realtion between tables

   DATA-RELATION r1 FOR ttOrder, ttOutService 
      RELATION-FIELDS (orderid, orderid) 
   DATA-RELATION r2 FOR ttOutService, ttInService  
      RELATION-FIELDS (orderid, orderid, type, outtype, serviceId, outserviceId) 
   DATA-RELATION r3 FOR ttInService, ttCharacteristic 
      RELATION-FIELDS (orderid, orderid, type, type, servicename, servicename)
   DATA-RELATION r4 FOR ttOutService, ttline 
      RELATION-FIELDS (orderid, orderid, type, type, serviceId, serviceId).
*/   


/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fChange_API_NB_URL RETURNS CHARACTER 
	(  ) FORWARD.

FUNCTION fDSS_NB_URL RETURNS CHARACTER 
	(  ) FORWARD.

FUNCTION fCreateJSON_for_API_Interface RETURNS JsonObject 
	( ) FORWARD.

/* ***************************  Main Block  *************************** */

/* ************************  Function Implementations ***************** */


FUNCTION fChange_API_NB_URL RETURNS CHARACTER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose: Return Change API interface Northbound URL for SAPC 
 Notes:
------------------------------------------------------------------------------*/	

   DEF VAR lcNB_Order_URL AS CHAR NO-UNDO. /* Procommand target */
   DEF VAR lcNB_Host_URL  AS CHAR NO-UNDO. /* Northbound host   */
   DEF VAR lcNB_Port_URL  AS CHAR NO-UNDO. /* Northbound port   */
   
   ASSIGN 
      lcNB_Order_URL = fCParam("SAPC","NB_ORDERS_URL")
      lcNB_Host_URL  = fCParam("SAPC","Host")
      lcNB_Port_URL  = fCParam("SAPC","Port").

   ASSIGN
      lcNB_Order_URL = REPLACE(lcNB_Order_URL,"~{host}",lcNB_Host_URL)
      lcNB_Order_URL = REPLACE(lcNB_Order_URL,"~{port}",lcNB_Port_URL).

   RETURN lcNB_Order_URL.
		
END FUNCTION.

FUNCTION fDSS_NB_URL RETURNS CHARACTER 
	(  ):
/*------------------------------------------------------------------------------
 Purpose: Return DSS Northbound URL for SAPC 
 Notes:
------------------------------------------------------------------------------*/ 

   DEF VAR lcNB_DSS_URL   AS CHAR NO-UNDO. /* Procommand target */
   DEF VAR lcNB_Host_URL  AS CHAR NO-UNDO. /* Northbound host   */
   DEF VAR lcNB_Port_URL  AS CHAR NO-UNDO. /* Northbound port   */
   
   ASSIGN 
      lcNB_DSS_URL  = "http://~{host}:~{port}/groups"
      lcNB_Host_URL = fCParam("SAPC", "Host")
      lcNB_Port_URL = fCParam("SAPC", "Port").

   ASSIGN
      lcNB_DSS_URL = REPLACE(lcNB_DSS_URL,"~{host}",lcNB_Host_URL)
      lcNB_DSS_URL = REPLACE(lcNB_DSS_URL,"~{port}",lcNB_Port_URL).

   RETURN lcNB_DSS_URL.
      
END FUNCTION.

FUNCTION fCreateJSON_for_API_Interface RETURNS JsonObject 
	():
/*------------------------------------------------------------------------------
 Purpose: Return a logical indicating whether the execution was succesful 
          Return JSON message for the Change API Interface
          Message is built with information from the Temp-tables received 
 Notes:
------------------------------------------------------------------------------*/	
   DEFINE VARIABLE oJson_line     AS JsonObject NO-UNDO.
   
   DEFINE VARIABLE loJsonObject   AS JsonObject NO-UNDO.
   DEFINE VARIABLE loJsonArray    AS JsonArray  NO-UNDO.

   DO ON ERROR UNDO, THROW:
         
      loJsonObject = NEW JsonObject().
      oJson_Line   = NEW JsonObject().
      loJsonArray  = NEW JsonArray().

      /* Reading data */
      FIND FIRST ttOrder          NO-LOCK.
      FIND FIRST ttOutService     NO-LOCK.
      FIND FIRST ttLine           NO-LOCK.
      FIND FIRST ttInService      NO-LOCK.
      FIND FIRST ttCharacteristic NO-LOCK.

      /* Characteristics objects */
      loJsonObject:add("value",ttCharacteristic.valueamt).
      loJsonObject:add("name",ttCharacteristic.name).
      
      /* Adding characteristic object to characteristics array */
      loJsonArray:add(loJsonObject).

      loJsonObject = NEW JsonObject().
      
      /* Inner services object */
      loJsonObject:add("type",ttInService.type).
      loJsonObject:add("serviceName",ttInService.serviceName).
      loJsonObject:add("quantity",ttInService.quantity).
      loJsonObject:add("action",ttInService.action).

      /* Adding characteristic array to Inner Services object */
      loJsonObject:add("Characteristics",loJsonArray).

      loJsonArray = NEW JsonArray().
      
      /* Adding Inner Services object to Inner Services array */
      loJsonArray:add(loJsonObject).

      /* Line Object */ 
      oJson_Line:add("phoneNumber", ttLine.phonenumber).

      loJsonObject = NEW JsonObject().

      /* Outer services object */
      loJsonObject:add("type",ttOutService.type).
      loJsonObject:add("serviceId",ttOutService.serviceId).
      loJsonObject:add("quantity",ttOutService.quantity).
      loJsonObject:add("action",ttOutService.action).
      
      /* Adding Line Object to outer Services object */
      loJsonObject:add("Line",oJson_Line).

      /* Adding Inner Services array to outer Services object */
      loJsonObject:add("Services",loJsonArray).

      loJsonArray = NEW JsonArray().

      /* Adding Outer Services object to outer Services array */
      loJsonArray:add(loJsonObject).

      loJsonObject = NEW JsonObject().

      /* Order (root) object */
      loJsonObject:add("orderId", ttOrder.orderid).
      loJsonObject:add("orderType", ttOrder.ordertype).
      loJsonObject:add("sellChannel", ttOrder.sellChannel).
      loJsonObject:add("sellDate", ttOrder.sellDate).
      loJsonObject:add("seller", ttOrder.seller).
      loJsonObject:add("createdBy", ttOrder.createdBy).
      loJsonObject:add("createdDate", ttOrder.createdDate).

      /* Adding outer Service array to Order (root) object */
      loJsonObject:Add("Services", loJsonArray).
       
   END.
   
   RETURN loJsonObject.
		
END FUNCTION.

