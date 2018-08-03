 
/*------------------------------------------------------------------------
   File        : DefaultWebHandler
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : tlamminmaki
   Created     : Wed Jun 20 10:51:56 EEST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Net.HTTP.StatusCodeEnum.
USING profcgi.REST.Request.
USING profcgi.REST.Response.
USING profcgi.lib.Logger.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonDataType.

{Syst/tmsconst.i}

&GLOBAL-DEFINE USE_PROCOMMAND_LOB 1
&GLOBAL-DEFINE USE_NOTIFY_BODY 2
&GLOBAL-DEFINE LEAVE_EMPTY 3

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS web.cls.ProCommandNotify INHERITS profcgi.REST.DefaultWebHandler: 
   /*------------------------------------------------------------------------------
    Purpose:
    Notes:
   ------------------------------------------------------------------------------*/

   DEFINE PRIVATE VARIABLE aiProCommandId AS INTEGER     NO-UNDO.
   DEFINE PRIVATE VARIABLE acStatus       AS CHARACTER   NO-UNDO.
   DEFINE PRIVATE VARIABLE adtResponseTS  AS DATETIME-TZ NO-UNDO.
   DEFINE PRIVATE VARIABLE ampRequestBody AS MEMPTR      NO-UNDO.
   
   DEFINE PRIVATE BUFFER ProCommand FOR ProCommand.
   DEFINE PRIVATE BUFFER MsRequest  FOR MsRequest.

   DEFINE PRIVATE PROPERTY ValidStatuses     AS CHARACTER NO-UNDO
      GET:
         RETURN "INPROGRESS,FAILED,COMPLETED".
      END GET.

   DESTRUCTOR ProCommandNotify():
      SET-SIZE(ampRequestBody) = 0.
   END DESTRUCTOR.

   METHOD PRIVATE VOID mParseValidateReqEntity
      (ioRequestBody AS CLASS Object):

      /*
         The request body should have one of following type:

         {
         "notificationID": "7547",
         "notificationType": "O",
         "orderID": "Y70044965",
         "notificationTime": "2016-10-26T17:43:42.000+02:00",
         "status": "INPROGRESS",
         "code": "",
         "description": "Order accepted"
         }
         
         {
         "notificationID": "7547",
         "notificationType": "O",
         "orderID": "Y70044965",
         "notificationTime": "2016-10-26T17:43:42.000+02:00",
         "status": "FAILED",
         "code": "F400",
         "description": "Invalid order format"
         }
         
         {
         "notificationID": "7547",
         "notificationType": "O",
         "orderID": "Y70044965",
         "notificationTime": "2016-10-26T17:43:42.000+02:00",
         "status": "COMPLETED",
         "code": "",
         "description": "Order completed"
         }      
      */

      DEFINE VARIABLE loJsonRequest  AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE llOK           AS LOGICAL          NO-UNDO.

      IF NOT TYPE-OF(ioRequestBody, JsonObject)
      THEN UNDO, THROW NEW AppError("Request body is not json object",
                                    INTEGER(StatusCodeEnum:BadRequest)).

      loJsonRequest = CAST(ioRequestBody, JsonObject).

      IF NOT loJsonRequest:Has("status")
      THEN UNDO, THROW NEW AppError("status property is missing",
                                    INTEGER(StatusCodeEnum:BadRequest)).

      IF loJsonRequest:GetType("status") NE JsonDataType:STRING
      THEN UNDO, THROW NEW AppError("status property is not string",
                                    INTEGER(StatusCodeEnum:BadRequest)).

      IF LOOKUP(loJsonRequest:GetCharacter("status"), ValidStatuses) EQ 0
      THEN UNDO, THROW NEW AppError
                              (SUBSTITUTE("Unknown status information " +
                                          "&1. Supported ones are &2",
                                          loJsonRequest:GetCharacter("status"),
                                          ValidStatuses),
                               INTEGER(StatusCodeEnum:BadRequest)).

      IF NOT loJsonRequest:Has("orderID")
      THEN UNDO, THROW NEW AppError("orderID property is missing",
                                    INTEGER(StatusCodeEnum:BadRequest)).

      IF loJsonRequest:GetType("orderID") NE JsonDataType:STRING
      THEN UNDO, THROW NEW AppError("orderID property is not string",
                                    INTEGER(StatusCodeEnum:BadRequest)).

      ASSIGN
         aiProCommandId = INTEGER(loJsonRequest:GetCharacter("orderID"))
         acStatus       = loJsonRequest:GetCharacter("status")
         NO-ERROR.

      IF ERROR-STATUS:ERROR
      THEN UNDO, THROW NEW AppError
               (SUBSTITUTE("orderID property value '&1' is not possible to " +
                           "convert to integer",
                           loJsonRequest:GetCharacter("orderID")),
                INTEGER(StatusCodeEnum:BadRequest)).

      llOK = loJsonRequest:Write(ampRequestBody, YES).

      IF NOT llOK
      THEN UNDO, THROW NEW AppError
               ("Internal error. Cannot write request body to memory",
                INTEGER(StatusCodeEnum:InternalServerError)).

      FIND ProCommand NO-LOCK WHERE
           ProCommand.ProCommandId = aiProCommandId NO-ERROR.
      IF NOT AVAILABLE ProCommand
      THEN UNDO, THROW NEW AppError
               (SUBSTITUTE("Unknown orderID '&1'",
                           loJsonRequest:GetCharacter("orderID")),
                INTEGER(StatusCodeEnum:BadRequest)).

   END METHOD.

   METHOD PRIVATE VOID mCreateProCommandResp
      ( iiStatus AS INTEGER,
        idtResponseTS AS DATETIME-TZ,
        iiResponsePlace AS INTEGER ):

      CREATE ProCommandResp.
      ASSIGN
         ProCommandResp.ProCommandId     = ProCommand.ProCommandId
         ProCommandResp.ProCommandStatus = iiStatus
         ProCommandResp.ResponseTS       = idtResponseTS.

      CASE iiResponsePlace:
         WHEN {&USE_PROCOMMAND_LOB}
         THEN COPY-LOB FROM OBJECT ProCommand.Response TO OBJECT ProCommandResp.Response NO-CONVERT.
         WHEN {&USE_NOTIFY_BODY}
         THEN COPY-LOB FROM OBJECT ampRequestBody TO OBJECT ProCommandResp.Response NO-CONVERT.
      END CASE.

   END METHOD.

   METHOD PRIVATE CHARACTER mProcessProCommand
      ( Resp AS CLASS Response ):

      DEFINE VARIABLE liReTry AS INTEGER  NO-UNDO.

      DO TRANSACTION ON ERROR UNDO, THROW:

         /* If ProCommand is locked then lets try to get the lock
            5 times waiting 1 second in between. If it is still
            locked then lets send HTTP status 503 Service unavailable */
         DO WHILE TRUE:
            FIND CURRENT ProCommand EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED ProCommand
            THEN DO:
               PAUSE 1 NO-MESSAGE.
               liReTry = liReTry + 1.
               IF liReTry >= 5
               THEN DO:
                  Resp:SetHeader("Retry-After", "5").
                  Resp:StatusCode = INTEGER(StatusCodeEnum:ServiceUnavailable).
                  RETURN "LOCKED".
               END.
   
               NEXT.
            END.  
   
            LEAVE.
         END.

         IF acStatus NE "INPROGRESS"
         THEN DO:
            FIND MsRequest NO-LOCK WHERE
                 MsRequest.MsRequest = ProCommand.MsRequest NO-ERROR.
                 
            IF NOT AVAILABLE MsRequest
            THEN DO:
               ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_FAULTY_RECORD}.
               RETURN SUBSTITUTE("Data corruption. Cannot find msrequest '&1'",
                                 ProCommand.MsRequest).
            END.
         END.

         liReTry = 1.

         IF AVAILABLE MsRequest
         THEN DO WHILE TRUE:
            /* If MsRequest is locked then lets try to get the lock
               5 times waiting 1 second in between. If it is still
               locked then lets send HTTP status 503 Service unavailable */
            FIND CURRENT MsRequest EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF LOCKED MsRequest
            THEN DO:
               PAUSE 1 NO-MESSAGE.
               liReTry = liReTry + 1.
               IF liReTry >= 5
               THEN DO:
                  Resp:SetHeader("Retry-After", "5").
                  Resp:StatusCode = INTEGER(StatusCodeEnum:ServiceUnavailable).
                  RETURN "LOCKED".
               END.
   
               NEXT.
            END.  
   
            LEAVE.
         END.

         /* Lets store the old values to ProCommandResp table */
         mCreateProCommandResp(ProCommand.ProCommandStatus,
                               ProCommand.ResponseTS,
                               {&USE_PROCOMMAND_LOB}).

         ProCommand.ResponseTS = adtResponseTS.
         COPY-LOB FROM OBJECT ampRequestBody TO OBJECT ProCommand.Response NO-CONVERT.

         /* ProCommand record is status 10 (Waiting accept) and gets completed
            notify from BPM in listener
            => We need to create another ProCommandResp indicating
               there were no accept notify
         */
         IF ProCommand.ProCommandStatus EQ {&PROCOMMANDSTATUS_WAITING_ACCEPT_NOTIFY} AND
            acStatus EQ "COMPLETED"
         THEN mCreateProCommandResp({&PROCOMMANDSTATUS_NO_ACCEPT_NOFITY_ARRIVED},
                                    adtResponseTS,
                                    {&LEAVE_EMPTY}).

         CASE acStatus:
            WHEN "INPROGRESS"
            THEN ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_ACCEPTED_NOTIFY}.
            WHEN "FAILED"
            THEN ASSIGN
                    ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_REJECTED_NOTIFY}
                    MsRequest.ReqStatus         = {&REQUEST_STATUS_REJECTED}.
            WHEN "COMPLETED"
            THEN ASSIGN
                    ProCommand.ProCommandStatus = {&PROCOMMANDSTATUS_DONE}
                    MsRequest.ReqStatus         = {&REQUEST_STATUS_HLR_DONE}.
         END.
      END.

      RETURN "".

   END METHOD.


   /*
      When the request is ok the reponse status is 200 and
      the response body is:
   
      {
      "result": "OK"
      }
      
      When the request is not ok the response status is 400 and
      the response body is:
   
      {
      "result": "KO",
      "errorDescription": "Description of the problem"
      }
   */
   METHOD PROTECTED OVERRIDE VOID HandlePost
      ( Req  AS CLASS Request,
        Resp AS CLASS Response ):

      DEFINE VARIABLE loJsonResponse AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE lcError        AS CHARACTER        NO-UNDO.

      ASSIGN
         adtResponseTS  = NOW
         loJsonResponse = NEW JsonObject().

      mParseValidateReqEntity(Req:Entity).

      IF ProCommand.ProCommandStatus EQ {&PROCOMMANDSTATUS_DONE}
      THEN UNDO, THROW NEW AppError("The notify was ignored as the order was already completed",
                                    INTEGER(StatusCodeEnum:OK)).

      IF ProCommand.ProCommandStatus > {&PROCOMMANDSTATUS_DONE}
      THEN UNDO, THROW NEW AppError("The notify was ignored due to previous errors",
                                    INTEGER(StatusCodeEnum:OK)).

      lcError = mProcessProCommand(Resp).
      
      IF lcError EQ "LOCKED"
      THEN RETURN.
      
      IF lcError > ""
      THEN UNDO, THROW NEW AppError(lcError,
                                    INTEGER(StatusCodeEnum:InternalServerError)).
      
      loJsonResponse:Add("result", "OK").
      Resp:Entity = loJsonResponse.
      Resp:StatusCode = INTEGER(StatusCodeEnum:OK).
      
      CATCH loAppError AS AppError :
         /* The notify was ignored. Lets mark this to the ProCommandResp table */
         IF loAppError:GetMessageNum(1) EQ INTEGER(StatusCodeEnum:OK)
         THEN mCreateProCommandResp({&PROCOMMANDSTATUS_IGNORED_NOTIFY},
                                    adtResponseTS,
                                    {&USE_NOTIFY_BODY}).
         
         loJsonResponse:Add("result", "KO").
         loJsonResponse:Add("errorDescription", loAppError:GetMessage(1)).
         Resp:Entity = loJsonResponse.
         Resp:StatusCode = loAppError:GetMessageNum(1).
      END CATCH.      

   END METHOD.

END CLASS.
