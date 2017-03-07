    
{Syst/commpaa.i}
{Func/timestamp.i}
{Func/xmlfunction.i}
{Func/heartbeat.i}

ASSIGN
   gcBrand = "1"
   katun   = "Request".

DEFINE STREAM outfile.

DEFINE VARIABLE lhServer  AS HANDLE    NO-UNDO.
DEFINE VARIABLE llRC      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liLoop    AS INTEGER   NO-UNDO FORMAT "zzzz9".
DEFINE VARIABLE lcTime1   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTime2   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liTimeOut AS INTEGER   NO-UNDO INIT 60.
DEFINE VARIABLE liNagios  AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNagios  AS CHARACTER NO-UNDO.
DEFINE VARIABLE llTimeOut AS LOGICAL   NO-UNDO.

PAUSE 0 BEFORE-HIDE.

FORM
   SKIP(1)
   "Started"      AT 2
   "Messages"     TO 30
   "Last message" TO 50 SKIP
   lcTime1 AT 2   FORMAT "X(19)" 
   liLoop  TO 30  FORMAT "zzzz9"
   lcTime2 TO 50  FORMAT "X(19)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " IVR MESSAGE LISTENER " WIDTH 54 ROW 8
FRAME frmMain .

ASSIGN
   liLoop   = 0
   lcTime1  = STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS")
   lcTime2  = lcTime1
   lcNagios = "ivri:IVR Interface".

DISPLAY
   lcTime1
   liLoop
   lcTime2
WITH FRAME frmMain.

/* Nagios keep alive signal */
liNagios = fKeepAlive(lcNagios).

LOOP:
REPEAT ON STOP UNDO, LEAVE ON QUIT UNDO, LEAVE:
   
   PUT SCREEN ROW 23 "Idle...          ".

   IF NOT llRC                   OR 
      NOT VALID-HANDLE(lhServer) THEN RUN pOpenConnection.

   IF llRC AND VALID-HANDLE(lhServer) THEN DO:
      WAIT-FOR CONNECT OF lhServer PAUSE liTimeOut.
      llTimeOut = (LAST-EVENT:CODE = -1). 
   END.
   ELSE DO.
      PAUSE 10 MESSAGE "Connection error!".
      HIDE MESSAGE NO-PAUSE.
   END.

   IF llTimeOut THEN DO:

      PUT SCREEN ROW 23 "Hit F8 to QUIT ..".

      READKEY PAUSE 2.

      PUT SCREEN ROW 23 "                 ".

      IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE LOOP.

   END.
   
   /* Nagios keep alive signal */
   liNagios = fKeepAlive(lcNagios).

END.

lhServer:DISABLE-CONNECTIONS().

DELETE OBJECT lhServer.

QUIT.

PROCEDURE pOpenConnection:

   lhServer:DISABLE-CONNECTIONS() NO-ERROR.
   
   DELETE OBJECT lhServer NO-ERROR.

   CREATE SERVER-SOCKET lhServer.

   llRC = lhServer:SET-CONNECT-PROCEDURE('ProcessClientConnect') NO-ERROR.
                     
   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      DISPLAY 'Unable To Establish Connect Procedure'.
      RETURN.
   END.

   llRC = lhServer:ENABLE-CONNECTIONS('-H localhost -S 8081') NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      MESSAGE
         'Unable To Establish Listener' ERROR-STATUS:GET-MESSAGE(1)
      VIEW-AS ALERT-BOX.
      RETURN.
   END.

END.

PROCEDURE ProcessClientConnect:
   
   DEFINE INPUT PARAMETER hSocket AS HANDLE NO-UNDO.

   llRC = hSocket:SET-READ-RESPONSE-PROCEDURE('SocketIO') NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      DISPLAY 'Unable To Establish Read Response Procedure'.
      RETURN.
   END.

END PROCEDURE.

PROCEDURE SocketIO:
   
   DEFINE VARIABLE lcResponse    AS CHARACTER NO-UNDO INIT "OK".
   DEFINE VARIABLE lcRequest     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcXML         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lmData        AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE lcLength      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCLI         AS CHARACTER NO-UNDO.

   DEFINE BUFFER bufPP FOR PrePaidRequest.

   IF SELF:CONNECTED() = FALSE THEN RETURN.

   PUT SCREEN ROW 23 "Processing...    ".

   SET-SIZE(lmData)       = 0.
   SET-SIZE(lmData)       = SELF:GET-BYTES-AVAILABLE().
   SET-BYTE-ORDER(lmData) = BIG-ENDIAN.

   /* read ONLY message */
   llRC = SELF:READ(lmData,1,SELF:GET-BYTES-AVAILABLE(),1) NO-ERROR.

   IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
      lcResponse = 'Unable To Read Detail Bytes'.
      RETURN.
   END.

   lcRequest = GET-STRING(lmData,1).

   OUTPUT TO /scratch/nagios/tms/ivr/ivr_request.xml APPEND.
   PUT CONTROL "NEW MESSAGE: " fTS2HMS(fMakeTS()) CHR(10) lcRequest CHR(10).
   OUTPUT CLOSE.

   /* take away newline characters */
   lcXML = REPLACE(lcRequest,CHR(13) + CHR(10),"").

   IF INDEX(lcXML,"<") > 0 THEN DO:

      /* take away http header */
      lcXML = SUBSTR(lcXML,INDEX(lcXML,"<")).

      IF lcXML NE "" THEN DO:

         ASSIGN
            liLoop  = liLoop + 1
            lcTime2 = fTS2HMS(fMakeTS())
            lcCLI   = fGetRPCNodeValue(lcXML,"Subscriber").
         
         DISP liLoop lcTime2 WITH FRAME frmMain.
         PAUSE 0.
         
         FIND FIRST PrePaidRequest WHERE
                    PrePaidRequest.CLI      = lcCLI   AND
                    PrePaidRequest.Brand    = gcBrand AND
                    PrePaidRequest.Source   = "IVR"   AND
                   (PrePaidRequest.PPStatus = 0 OR
                    PrePaidRequest.PPStatus = 9)
         NO-LOCK NO-ERROR.

         IF NOT AVAIL PrePaidRequest THEN DO TRANSACTION:
      
            CREATE PrePaidRequest.
            PrePaidRequest.TSRequest = fMakeTS().

            ASSIGN
               PrePaidRequest.CLI       = lcCLI
               PrePaidRequest.Brand     = gcBrand
               PrePaidRequest.Source    = "IVR"
               PrePaidRequest.PPRequest = NEXT-VALUE(PrePaidReq)
               PrePaidRequest.CommLine  = lcXML
               PrePaidRequest.PPStatus  = 0.
         
            FIND FIRST MobSub WHERE
                       MobSub.Brand = gcBrand AND
                       MobSub.CLI   = PrePaidRequest.CLI
            NO-LOCK NO-ERROR.
            
            IF NOT AVAIL MobSub THEN ASSIGN
               PrePaidRequest.RespCode = 200
               PrePaidRequest.PPStatus = 2.
            ELSE IF NOT MobSub.PayType THEN ASSIGN
               PrePaidRequest.RespCode = 201
               PrePaidRequest.PPStatus = 2.
            ELSE IF NOT MobSub.MsStatus = 1 THEN ASSIGN
               PrePaidRequest.RespCode = 202
               PrePaidRequest.PPStatus = 2.

            IF PrePaidRequest.RespCode = 0 THEN DO:

               CREATE MsRequest.
               ASSIGN
                  MsRequest.CreStamp   = PrePaidRequest.TSRequest
                  MsRequest.MsRequest  = NEXT-VALUE(MsRequest)
                  MsRequest.Brand      = gcBrand
                  MsRequest.MSSeq      = MobSub.MSSeq
                  MsRequest.CLI        = MobSub.CLI
                  MsRequest.CustNum    = MobSub.CustNum
                  MsRequest.UserCode   = katun
                  MsRequest.ActStamp   = MsRequest.CreStamp
                  MsRequest.ReqType    = 1
                  MsRequest.ReqCParam1 = "PROFILE"
                  MsRequest.ReqCParam3 = "2,PAYTYPE=->PREPAID".
            
            END.
            
            RELEASE PrePaidRequest.
         
         END.

         RUN pResponse(-1).

         lcXML = RETURN-VALUE + CHR(13) + CHR(10).      

      END.

      OUTPUT STREAM outfile TO /scratch/nagios/tms/ivr/ivr_httppost.log APPEND.
      PUT STREAM outfile CONTROL lcXML CHR(10).
      OUTPUT STREAM outfile CLOSE.

      ASSIGN
         lcLength   = STRING(LENGTH(lcXML))
         lcResponse = 'HTTP/1.1 200 OK~r~n'         + 
                      'Server: 145.247.16.66~r~n'   +
                      'Content-Length: ' + lcLength + '~r~n' +
                      'Connection: close~r~n'       +
                      'Content-type: text/html~r~n~r~n'
         lcResponse = lcResponse + lcXML.

      SET-SIZE(lmData) = 0.
      SET-SIZE(lmData) = LENGTH(lcResponse).

      PUT-STRING(lmData,1,LENGTH(lcResponse)) = lcResponse.

      /* write complete data: header + message */
      llRC = SELF:WRITE(lmData,1,LENGTH(lcResponse)) NO-ERROR.
   
      SET-SIZE(lmData) = 0.
   
   END.

   PUT SCREEN ROW 23 "Idle...          ".

END PROCEDURE.

PROCEDURE pResponse:

   DEFINE INPUT PARAMETER piResponse AS INTEGER NO-UNDO.

   DEFINE VARIABLE lhSAXWriter  AS HANDLE    NO-UNDO.
   DEFINE VARIABLE llOK         AS LOGICAL   NO-UNDO. 
   DEFINE VARIABLE lmXML        AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE lcXML        AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liResponse   AS INTEGER   NO-UNDO.

   IF piResponse = 0 THEN liResponse =  1.
   ELSE                   liResponse = -1.

   CREATE SAX-WRITER lhSAXWriter. 

   ASSIGN
      lhSAXWriter:FORMATTED = FALSE
      lhSAXWriter:ENCODING = "UTF-8". 

   llOK = lhSAXWriter:SET-OUTPUT-DESTINATION("memptr",lmXML).
   
   llOK = lhSAXWriter:START-DOCUMENT().

   fRPCStruct("Start","methodResponse,params,param,value",lhSAXWriter).
   
   llOK = lhSAXWriter:WRITE-DATA-ELEMENT("int",STRING(liResponse)).

   fRPCStruct("End","methodResponse,params,param,value",lhSAXWriter).

   llOK = lhSAXWriter:END-DOCUMENT().

   DELETE OBJECT lhSAXWriter.

   lcXML = GET-STRING(lmXML,1).

   SET-SIZE(lmXML) = 0.

   OUTPUT TO /scratch/nagios/tms/ivr/ivr_response.xml APPEND.
   PUT CONTROL "NEW RESPONSE: " + fTS2HMS(fMakeTS()) CHR(10) lcXML CHR(10).
   OUTPUT CLOSE.

   RETURN lcXML.

END.

