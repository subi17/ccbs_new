
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

FUNCTION fCreateJSON_for_API_Interface RETURNS LOGICAL 
	(OUTPUT cJsonMsg AS LONGCHAR) FORWARD.

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

FUNCTION fCreateJSON_for_API_Interface RETURNS LOGICAL 
	(OUTPUT cJsonMsg AS LONGCHAR):
/*------------------------------------------------------------------------------
 Purpose: Return a logical indicating whether the execution was succesful 
          Return JSON message for the Change API Interface
          Message is built with information from the Temp-tables received 
 Notes:
------------------------------------------------------------------------------*/	
   DEFINE VARIABLE lResult        AS LOGICAL    NO-UNDO INITIAL FALSE.
   DEFINE VARIABLE oJson_Order    AS JsonObject NO-UNDO.
   DEFINE VARIABLE oJson_line     AS JsonObject NO-UNDO.
   DEFINE VARIABLE oJson_OutServ  AS JsonObject NO-UNDO.
   DEFINE VARIABLE oJson_InServ   AS JsonObject NO-UNDO.
   DEFINE VARIABLE oJson_Charact  AS JsonObject NO-UNDO.

   DEFINE VARIABLE aJson_OutServ  AS JsonArray  NO-UNDO.
   DEFINE VARIABLE aJson_InServ   AS JsonArray  NO-UNDO.
   DEFINE VARIABLE aJson_Charact  AS JsonArray  NO-UNDO.

   blk:
   DO ON ERROR UNDO blk, LEAVE blk
      ON STOP UNDO blk, LEAVE blk:
         
      /* Create JsonObjects */
      oJson_Order   = NEW JsonObject().  /* root object */
      oJson_Line    = NEW JsonObject().
      oJson_Inserv  = NEW JsonObject().
      oJson_Outserv = NEW JsonObject().
      oJson_Charact = NEW JsonObject().

      /* Create JsonArrays  */
      aJson_OutServ = NEW JsonArray().
      aJson_InServ  = NEW JsonArray().
      aJson_Charact = NEW JsonArray().
      
      /* Reading data */
      FIND FIRST ttOrder          NO-LOCK.
      FIND FIRST ttOutService     NO-LOCK.
      FIND FIRST ttLine           NO-LOCK.
      FIND FIRST ttInService      NO-LOCK.
      FIND FIRST ttCharacteristic NO-LOCK.

      /* Characteristics objects */
      oJson_Charact:add("value",ttCharacteristic.valueamt).
      oJson_Charact:add("name",ttCharacteristic.name).
      
      /* Adding characteristic object to characteristics array */
      aJson_Charact:add(oJson_Charact).
      
      /* Inner services object */
      oJson_InServ:add("type",ttInService.type).
      oJson_InServ:add("serviceName",ttInService.serviceName).
      oJson_InServ:add("quantity",ttInService.quantity).
      oJson_Inserv:add("action",ttInService.action).

      /* Adding characteristic array to Inner Services object */
      oJson_InServ:add("Characteristics",aJson_Charact).
      
      /* Adding Inner Services object to Inner Services array */
      aJson_Inserv:add(oJson_Inserv).

      /* Line Object */ 
      oJson_Line:add("phoneNumber", ttLine.phonenumber).

      /* Outer services object */
      oJson_OutServ:add("type",ttOutService.type).
      oJson_OutServ:add("serviceId",ttOutService.serviceId).
      oJson_OutServ:add("quantity",ttOutService.quantity).
      oJson_Outserv:add("action",ttOutService.action).
      
      /* Adding Line Object to outer Services object */
      oJson_Outserv:add("Line",oJson_Line).

      /* Adding Inner Services array to outer Services object */
      oJson_Outserv:add("Services",aJson_InServ).

      /* Adding Outer Services object to outer Services array */
      aJson_OutServ:add(oJson_OutServ).

      /* Order (root) object */
      oJson_Order:add("orderId", ttOrder.orderid).
      oJson_Order:add("orderType", ttOrder.ordertype).
      oJson_Order:add("sellChannel", ttOrder.sellChannel).
      oJson_Order:add("sellDate", ttOrder.sellDate).
      oJson_Order:add("seller", ttOrder.seller).
      oJson_Order:add("createdBy", ttOrder.createdBy).
      oJson_Order:add("createdDate", ttOrder.createdDate).

      /* Adding outer Service array to Order (root) object */
      oJson_Order:Add("Services", aJson_OutServ).

      /* Write Json message. Default encoding: UTF-8. */
      lResult = oJson_Order:WRITE(cJsonMsg,TRUE).
       
   END.
   
   RETURN lResult.
		
END FUNCTION.

