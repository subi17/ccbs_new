 
 /*------------------------------------------------------------------------
    File        : ProCommandDSS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.ProCommandDSS INHERITS Gwy.ProCommand: 

   CONSTRUCTOR ProCommandDSS
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE VARIABLE lcmsisdns AS CHARACTER NO-UNDO.
      DEFINE VARIABLE loJson    AS CLASS JsonObject NO-UNDO.

      IF MsRequest.ReqType NE {&REQTYPE_DSS}
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandDSS can only " +
                                               "handle ReqType &1",
                                               {&REQTYPE_DSS})).
      
      acProCommandTarget = "NB_AS".

      CASE MsRequest.ReqCparam1:
         WHEN "CREATE"
         THEN DO:
            ASSIGN
               loJson                = NEW JsonObject()
               acProCommandType      = "CREATE_DSS_GROUP"
               acProCommandVerb      = "PUT"
               acProCommandtargetURL = SUBSTITUTE("/groups/&1", MsRequest.CustNum)
               lcmsisdns   = SUBSTRING(MSRequest.ReqCParam2,INDEX(MSRequest.ReqCParam2,"MSISDNS="))
               lcmsisdns   = REPLACE(lcmsisdns,"MSISDNS=","")
               lcmsisdns   = REPLACE(lcmsisdns,";","|")
               lcDataLimit = SUBSTRING(MSRequest.ReqCParam2,INDEX(MSRequest.ReqCParam2,"LIMIT_UNSHAPED="))
               lcDataLimit = REPLACE(lcDataLimit,"LIMIT_UNSHAPED=","").

            loJson:Add("type", "POST"). /* Type of user: POST */
            loJson:Add("priority", "1000"). /* For Base tariff = 1000 */
            loJson:Add("msisdns", lcmsisdns).
         END.
         WHEN "ADD" OR WHEN "REMOVE"
         THEN DO:
            ASSIGN
               loJson                = NEW JsonObject()
               acProCommandType      = (IF MSrequest.ReqCparam1 = "ADD"
                                        THEN "ADD_TO_DSS_GROUP"
                                        ELSE "REMOVE_FROM_DSS_GROUP") 
               acProCommandVerb      = "POST"
               acProCommandtargetURL = SUBSTITUTE("/groups/&1/manage-subscription",
                                                  MsRequest.CustNum)
               lcmsisdns = SUBSTRING(MSRequest.ReqCParam2,INDEX(MSRequest.ReqCParam2,"MSISDN="))
               lcmsisdns = REPLACE(lcmsisdns,"MSISDN=","")
               lcmsisdns = SUBSTRING(lcmsisdns,1,INDEX(lcmsisdns,",") - 1).

            loJson:Add("action", (IF MSrequest.ReqCparam1 = "ADD"
                                  THEN "add"  /* Hard-coded when adding to DSS */
                                  ELSE "remove") /* Hard-coded when removing from DSS */
            loJson:Add("msisdn", lcmsisdns).
         END.
         WHEN "DELETE" THEN
            ASSIGN
               loProCommand:acProCommandType      = "TERMINATE_DSS_GROUP"
               loProCommand:acProCommandVerb      = "DELETE"
               loProCommand:acProCommandtargetURL = SUBSTITUTE("/groups/&1",
                                                               MsRequest.CustNum).
         /* Currently there is no request for this...
         WHEN "UPDATEDATALIMIT"
         THEN DO:
            ASSIGN
               loJson                = NEW JsonObject()
               acProCommandType      = "UPDATE_DSS_GROUP_DATALIMIT"
               acProCommandVerb      = "POST"
               acProCommandtargetURL = SUBSTITUTE("/groups/&1/set-accumulated-volume",
                                                  MsRequest.CustNum).

            loJson:Add("offeringName", MsRequest.ReqCParam3). /* DSS/DSS2/DSS4 */
            loJson:Add("roamingLikeAtHome", lcDataLimit).
            loJson:Add("tariff", lcDataLimit).
         END.
         */
      END CASE.

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END CONSTRUCTOR.

END CLASS.