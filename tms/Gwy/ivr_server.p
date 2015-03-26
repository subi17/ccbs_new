    
{commpaa.i}
{timestamp.i}
{xmlfunction.i}
{heartbeat.i}
{fgettxt.i}

gcBrand = "1".

DEFINE STREAM outfile.

FUNCTION fCallAlarm RETURNS LOGICAL
  (INPUT pcAction AS CHARACTER,
   INPUT pcCLI    AS CHARACTER,
   INPUT pdeAmt   AS INTEGER):
   
   DEFINE VARIABLE ldeActStamp  AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcAlarmMess  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLang       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcDate       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTime       AS CHARACTER NO-UNDO.
   DEFINE VARIABLE ldaDate      AS DATE      NO-UNDO.
   DEFINE VARIABLE liTime       AS INTEGER   NO-UNDO.

   FIND FIRST MobSub WHERE
              MobSub.CLI = pcCLI
   NO-LOCK NO-ERROR.

   ASSIGN
      /* others dont exist yet ! */
      liLang      = 1
      lcAlarmMess = fGetTxt(INPUT "SMS", pcAction, TODAY, liLang)
      lcAlarmMess = REPLACE(lcAlarmMess,"#TOPUP", TRIM(STRING(pdeAmt / 100,">>>99.99")))
      ldeActStamp = fMakeTS().
   
   CREATE CallAlarm.
   ASSIGN
      CallAlarm.ActStamp   = ldeActStamp
      CallAlarm.CLSeq      = 0
      CallAlarm.CASeq      = NEXT-VALUE(CallAlarm)
      CallAlarm.CustNo     = MobSub.CustNum
      CallAlarm.CLI        = MobSub.CLI
      CallAlarm.DeliStat   = 1
      CallAlarm.Delitype   = 1
      CallAlarm.DeliPara   = "1"
      CallAlarm.DeliMsg    = lcAlarmMess
      CallAlarm.Limit      = 0
      CallAlarm.CreditType = 22
      CallAlarm.Orig       = "800622800"
      CallAlarm.Brand      = gcBrand.
      
   RELEASE CallAlarm.

END FUNCTION.

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
   lcNagios = "ivr:IVR Interface".

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
   DEFINE VARIABLE liSoLog       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcCommLine    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liPPRequest   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liMyRequest   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liResponse    AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcCLI         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcSogResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llSMS         AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE ldeTopUp      AS DECIMAL   NO-UNDO.
   
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

      PUT SCREEN ROW 1 COL 1 "1: " + FILL(" ",13).
      PUT SCREEN ROW 2 COL 1 "2: " + FILL(" ",13).
      PUT SCREEN ROW 3 COL 1 "3: " + FILL(" ",13).
      PUT SCREEN ROW 4 COL 1 "4: " + FILL(" ",78).
      PUT SCREEN ROW 5 COL 1 "5: " + FILL(" ",13).
      PUT SCREEN ROW 6 COL 1 "6: " + FILL(" ",13).
      PUT SCREEN ROW 7 COL 1 "7: " + FILL(" ",78).
      PUT SCREEN ROW 8 COL 1 "8: " + FILL(" ",13).
      PUT SCREEN ROW 9 COL 1 "9: " + FILL(" ",13).

      ETIME(YES).

      PUT SCREEN ROW 1 COL 1 "1: " + STRING(ETIME,"zzzzz9").
      
      /* take away http header */
      lcXML = SUBSTR(lcXML,INDEX(lcXML,"<")).

      IF lcXML NE "" THEN DO:

         ASSIGN
            llSMS    = FALSE
            ldeTopUp = 0.0
            liLoop   = liLoop + 1
            lcTime2  = STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS").
         
         DISP liLoop lcTime2 WITH FRAME frmMain.
         PAUSE 0.
         
         DO TRANSACTION:
      
            CREATE PrePaidRequest.
            PrePaidRequest.TSRequest = fMakeTS().

            ASSIGN
               PrePaidRequest.CLI       = fGetRPCNodeValue(lcXML,"Subscriber")
               PrePaidRequest.Brand     = gcBrand
               PrePaidRequest.Source    = "IVR"
               PrePaidRequest.PPRequest = NEXT-VALUE(PrePaidReq)
               PrePaidRequest.CommLine  = lcXML
               PrePaidRequest.PPStatus  = 1
               liSoLog                  = NEXT-VALUE(SoLog)
               liMyRequest              = PrePaidRequest.PPRequest
               liResponse               = 0
               lcCLI                    = PrePaidRequest.CLI
               liResponse               = 0.
         
            RELEASE PrePaidRequest.
         
         END.

         FIND FIRST MobSub WHERE
                    MobSub.Brand = gcBrand AND
                    MobSub.CLI   = lcCLI
         NO-LOCK NO-ERROR.

         PUT SCREEN ROW 2 COL 1 "2: " + STRING(ETIME,"zzzzz9").
   
         IF AVAIL MobSub AND MobSub.MsStat = 1 THEN DO:
         
            lcCommLine = STRING(liSoLog) + " MODIFY,OPERATOR=YOIGO,MSISDN=34" +
                         lcCLI + ",PROFILE=2,PAYTYPE=->PREPAID".

            PUT SCREEN ROW 3 COL 1 "3: " + STRING(ETIME,"zzzzz9").
            RUN ivrsog(liSoLog,lcCommLine).
            PUT SCREEN ROW 4 COL 1 "4: " + STRING(ETIME,"zzzzz9") + " " +
                                           RETURN-VALUE          + 
                                           FILL(" ", 40 - LENGTH(RETURN-VALUE)).

            lcSogResponse = RETURN-VALUE.

            IF ENTRY(3,lcSogResponse," ") NE "OK" THEN DO TRANSACTION:

               FIND FIRST PrePaidRequest WHERE
                          PrePaidRequest.Brand     = gcBrand AND
                          PrePaidRequest.PPRequest = liMyRequest
               EXCLUSIVE-LOCK NO-ERROR.
               ASSIGN
                  PrePaidRequest.RespCode = 909
                  liResponse              = 909.

               RELEASE PrePaidRequest.
               
            END.
            ELSE IF AVAIL MobSub THEN DO:

               DO TRANSACTION:
      
                  FIND CURRENT MobSub EXCLUSIVE-LOCK.
                  MobSub.MsStatus = 4.
             
                  FIND FIRST SubSer WHERE
                             SubSer.MsSeq   = MobSub.MsSeq AND
                             SubSer.ServCom = "VMS"        AND
                             SubSer.SSDate  = TODAY
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE SubSer THEN DO:
                     CREATE SubSer.
                     ASSIGN
                        SubSer.MsSeq   = MobSub.MsSeq
                        SubSer.ServCom = "VMS"
                        SubSer.SSDate  = TODAY.
                  END.
                  SubSer.SSStat = 1.

               END.

               PUT SCREEN ROW 5 COL 1 "5: " + STRING(ETIME,"zzzzz9").

               RUN topupcamp(MobSub.MsSeq, OUTPUT liPPRequest).

               IF liPPRequest NE 0 THEN DO:

                  PUT SCREEN ROW 6 COL 1 "6: " + STRING(ETIME,"zzzzz9").
                  RUN pp_platform(gcBrand,liPPRequest).

                  FIND FIRST bufPP WHERE
                             bufPP.Brand     = gcBrand AND
                             bufPP.PPRequest = liPPRequest
                  NO-LOCK NO-ERROR.

                  IF AVAIL bufPP AND bufPP.RespCode = 0 THEN ASSIGN
                     llSMS    = TRUE
                     ldeTopUp = bufPP.TopUpAmt
                     lcCLI    = bufPP.CLI.

               END.

            END.
         
         END.
         ELSE lcSogResponse = "Duplicate IVR request".

         IF llSMS THEN DO:

            PUT SCREEN ROW 7 COL 1 "7: " + STRING(ETIME,"zzzzz9").
            
            fCallAlarm("TopUpOrder", lcCLI, INT(ldeTopUp)).

         END.
         
         PUT SCREEN ROW 8 COL 1 "8: " + STRING(ETIME,"zzzzz9").
         RUN pResponse(liResponse).

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
   
      IF llRC = FALSE OR ERROR-STATUS:GET-MESSAGE(1) <> '' THEN DO:
         lcResponse = 'Unable To Write Response Bytes'.
         OUTPUT TO /scratch/nagios/tms/ivr/ivr_response_errors.txt APPEND.
         PUT UNFORMATTED lcResponse  ": " fTS2HMS(fMakeTS()) " " lcCLI CHR(10).
         OUTPUT CLOSE.
      END.
   
      DO TRANSACTION:
      
         FIND FIRST PrePaidRequest WHERE
                    PrePaidRequest.Brand     = gcBrand AND
                    PrePaidRequest.PPRequest = liMyRequest
         EXCLUSIVE-LOCK NO-ERROR.

         CREATE SoLog.
         ASSIGN
            SoLog.CreatedTS         = fMakeTS()
            SoLog.CLI               = PrePaidRequest.CLI
            SoLog.ActivationTS      = SoLog.CreatedTS
            SoLog.CompletedTS       = SoLog.CreatedTS
            SoLog.TimeSlotTMS       = SoLog.CreatedTS
            SoLog.SoLog             = liSoLog
            SoLog.CommLine          = lcCommLine
            SoLog.MsSeq             = MobSub.MsSeq WHEN AVAIL MobSub
            SoLog.Stat              = 2 WHEN PrePaidRequest.RespCode =  0
            SoLog.Stat              = 1 WHEN PrePaidRequest.RespCode NE 0
            SoLog.Brand             = gcBrand
            SoLog.Response          = lcSogResponse
            SoLog.Users             = "IVR First call"
            PrePaidRequest.PPStatus = 2.

         RELEASE PrePaidRequest.
         RELEASE SoLog.
      
      END.

      PUT SCREEN ROW 9 COL 1 "9: " + STRING(ETIME,"zzzzz9").

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

