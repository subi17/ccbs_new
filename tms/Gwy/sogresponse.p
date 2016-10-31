/* ----------------------------------------------------------------------------
  MODULE .......: sogresponse.p
  FUNCTION .....: 
  APPLICATION ..: TMS
  CREATED ......: 
  MODIFIED .....: 02.12.2006 kl PING
                  08.02.2007 kl automatic resend
                  16.02.2007 kl gcBrand = "1", ivr_process.i
                  14.03.2007 kl solog => bufSoLog
                  03.10.2007 kl procedure pUpdateQueue
                  18.03.2010 mk Fix pUpdateQueue handling

  Version ......: 
  --------------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/tmsparam4.i}
{Func/msreqfunc.i}

gcBrand = "1".

DEFINE VARIABLE lhServer    AS HANDLE    NO-UNDO.
DEFINE VARIABLE ldaDate     AS DATE      NO-UNDO FORMAT "99.99.9999".
DEFINE VARIABLE lcTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResponse  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogIn     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop      AS INTEGER   NO-UNDO.
DEFINE VARIABLE llRC        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lhSocket    AS HANDLE    NO-UNDO.
DEFINE VARIABLE lcAck       AS CHARACTER NO-UNDO.
DEFINE VARIABLE liSoLog     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcStatus    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liReSocket  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPing      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liTimeOut   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNagios    AS CHARACTER NO-UNDO.
DEFINE VARIABLE llTimeOut   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcLogFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNagios    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcURL       AS CHARACTER NO-UNDO.
DEFINE VARIABLE clsNagios   AS CLASS Class.nagios    NO-UNDO.

ASSIGN
   lcURL     = fCParamC4(gcBrand,"SOG","URL_Read")
   lcLogin   = "LOGIN yoigo toro"
   lcStatus  = "YOIGO SOG READER"
   liTimeOut = 30
   lcNagios  = "sres:Activ. Response Sender"
   gcBrand   = "1".

FORM 
   ldaDate
   lctime
   liReSocket
WITH
NO-LABEL  TITLE "YOIGO SOG RESPONSE READER "  CENTERED ROW 4 FRAME frm.
   
PAUSE 0.

DISP
   today @ ldadate
   string(time,"hh:mm:ss") @ lctime 
   liReSocket
WITH FRAME frm.

FORM
   ldaDate
   lcTime
   lcResponse
WITH
   NO-LABEL 10 DOWN TITLE lcStatus  CENTERED ROW 8
FRAME frmMain.

clsNagios = NEW Class.nagios().

RUN pUpdateQueue.

REPEAT:

   RUN pOpenConnection.

   IF RETURN-VALUE = "ERROR" THEN DO:

      MESSAGE "Could NOT LogIn".

      PAUSE 5 NO-MESSAGE.
      
      HIDE MESSAGE NO-PAUSE.

      NEXT.

   END.
   ELSE LEAVE.

END.

/* Nagios keep alive signal for response reader */
liNagios = clsNagios:KeepAlive(lcNagios).

PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                    STRING(time,"hh:mm:ss").

LOOP:
REPEAT ON STOP UNDO, LEAVE ON QUIT UNDO, LEAVE:

   PUT SCREEN ROW 23 "WAIT FOR-RESPONSE".
   
   IF lhSocket:CONNECTED() THEN DO:

      lcResponse = "".

      WAIT-FOR READ-RESPONSE OF lhSocket PAUSE liTimeOut.

      llTimeOut = (LAST-EVENT:CODE = -1). 
      IF NOT llTimeOut THEN ASSIGN
         lcResponse = RETURN-VALUE
         liReSocket = liReSocket + 1.

   END.
   
   IF NOT lhSocket:CONNECTED() THEN DO:

      PAUSE 10 NO-MESSAGE.

      RUN pOpenConnection.

      NEXT LOOP.

   END.

   IF liReSocket MOD 50 = 0 THEN RUN pUpdateQueue.

   /* Nagios keep alive signal for response reader */
   liNagios = clsNagios:KeepAlive(lcNagios).
      
   PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                       STRING(time,"hh:mm:ss").
   PAUSE 0.
   DISP
      today @ ldadate
      string(time,"hh:mm:ss") @ lctime
      liReSocket
   WITH FRAME frm.

   IF llTimeOut THEN DO:

      liPing = liPing + 1.
      
      RUN pUpdateQueue.

      PUT SCREEN ROW 23 "Hit F8 to QUIT ..".

      READKEY PAUSE 5.
      HIDE MESSAGE NO-PAUSE.

      IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE LOOP.

   END.
   ELSE DO:
   
      PUT SCREEN ROW 23 "RUNNING ..........".
   
      ASSIGN
         liSoLog = INTEGER(ENTRY(1,lcResponse," "))
         lcAck   = "ACK " + STRING(liSoLog)
         lcTime  = STRING(TIME,"HH:MM:SS")
         ldaDate = TODAY
         liPing  = 0
      NO-ERROR.

      IF NOT ERROR-STATUS:ERROR THEN DO:

         DISP
           ldaDate 
           lcTime
           lcResponse format "X(37)" 
         WITH FRAME frmMain.
         DOWN WITH FRAME frmMain.

         PUT SCREEN ROW 23 "WORKING..........".

         RUN pPostRequest(lcAck + CHR(10),lhSocket).

         RUN pSoLog(liSoLog,lcResponse).

      END.

   END.
   
   /* 20 is appr. 10 minutes since timeout is 30 seconds */
   IF liPing = 20 THEN DO:

      liPing = 0.

      RUN pPostRequest("PING" + CHR(10),lhSocket).

      WAIT-FOR READ-RESPONSE OF lhSocket PAUSE liTimeOut.

      lcResponse = RETURN-VALUE.

      IF RETURN-VALUE NE "PONG" + CHR(10) THEN run pOpenConnection.

      RUN pUpdateQueue.

   END.
   
   IF liSoLog MOD 10 = 0 AND liSoLog > 0 THEN DO: 
      
      liNagios = clsNagios:KeepAlive(lcNagios).
      
      PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                          STRING(time,"hh:mm:ss").
   END.

END.

ASSIGN
   lcTime  = STRING(TIME,"HH:MM:SS")
   ldaDate = TODAY.

PAUSE 0 BEFORE-HIDE.

lhSocket:DISCONNECT() NO-ERROR.

DELETE OBJECT(lhSocket).

QUIT.

PROCEDURE pOpenConnection:
   
   DEFINE VARIABLE llOK AS LOGICAL NO-UNDO INIT TRUE.

   lhSocket:DISCONNECT() NO-ERROR.

   IF VALID-HANDLE(lhSocket) THEN DELETE OBJECT lhSocket.

   CREATE SOCKET lhSocket.

   llOK = lhSocket:CONNECT(lcURL) NO-ERROR.

   llOK = lhSocket:SET-READ-RESPONSE-PROCEDURE('pGetServerResponce').

   /* 1: login */
   IF llOK THEN DO:

      RUN pPostRequest(lcLogIn + CHR(10),lhSocket).

      llOK = (RETURN-VALUE = "OK").

   END.
   
   IF llOK THEN DO:

      WAIT-FOR READ-RESPONSE OF lhSocket PAUSE 1.

      lcResponse = RETURN-VALUE.

      IF lcResponse NE "OK LOGIN" + CHR(10) THEN DO:

         lhSocket:DISCONNECT() NO-ERROR.

         DELETE OBJECT(lhSocket).

         llOK = FALSE.

      END.

   END.

   RETURN STRING(llOK,"OK/ERROR").

END PROCEDURE.

PROCEDURE pUpdateQueue:

   DEFINE BUFFER bufQ FOR UpdateQueue.

   FOR EACH UpdateQueue NO-LOCK WHERE
            UpdateQueue.State = 0:
   
      FIND FIRST bufQ WHERE
           RECID(bufQ) = RECID(UpdateQueue)
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
      IF LOCKED bufQ THEN NEXT.
      
      RUN pSoLog(UpdateQueue.Seq1,UpdateQueue.Value1).

      IF RETURN-VALUE = "OK" THEN DO:
         
         ASSIGN
            bufQ.TSUpdate = fMakeTS()
            bufQ.State    = 1.
      
         RELEASE bufQ.
   
      END.

   END.
   
END PROCEDURE.

PROCEDURE pSoLog:
   
   DEFINE INPUT PARAMETER iiSolog    AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER pcResponse AS CHARACTER NO-UNDO.

   DEFINE VARIABLE liSoLog     AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcCommand   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcStatus    AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcInfo      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcAlarmMess AS CHARACTER NO-UNDO.
   DEFINE VARIABLE liLang      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE ldeSMSTime  AS DEC       NO-UNDO.
   DEFINE VARIABLE liTime      AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liNewTS     AS DECIMAL   NO-UNDO.
   DEFINE VARIABLE lcReturn    AS CHARACTER NO-UNDO INIT "OK".
   DEF VAR lcErrorCode AS CHAR NO-UNDO. 

   DEFINE BUFFER bufSoLog FOR SoLog.
   DEF BUFFER bOrigRequest FOR MsRequest.

   ASSIGN
      liSoLog   = INT(ENTRY(1,pcResponse," "))
      lcCommand =     ENTRY(2,pcResponse," ")     
      lcStatus  =     ENTRY(3,pcResponse," ")
      lcInfo    = SUBSTRING(pcResponse,INDEX(pcResponse,
                      ENTRY(4,pcResponse," ")))
   NO-ERROR.

   IF liSoLog > 0 THEN DO:

      /* YTS-8530, EYOIGO-43 */
      IF liSoLog <= 1000000 THEN RETURN "SKIPPED".
      
      FIND FIRST SoLog WHERE
                 SoLog.SoLog = liSoLog
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF NOT LOCKED(SoLog) AND NOT ERROR-STATUS:ERROR THEN DO:

         ASSIGN
            Solog.CompletedTS = fMakeTS()
            Solog.stat        = 2.
         
         IF lcStatus = "OK" THEN DO:
            SoLog.Response = solog.response + lcStatus.
            IF lcCommand EQ "DISPLAY" THEN
               solog.response = solog.response + " " + lcInfo.
         END.
         ELSE SoLog.Response = solog.response + lcStatus + " " + lcInfo.
         
         IF SoLog.MsRequest > 0 THEN
            FIND FIRST MSRequest WHERE 
                       MSRequest.MSrequest  = Solog.MSRequest
            EXCLUSIVE-LOCK NO-ERROR.
         ELSE RELEASE MSRequest.
      
         /* automatic resend */
         IF lcStatus NE "OK" AND
            NUM-ENTRIES(SoLog.Response," ") >= 5 THEN DO:

            lcErrorCode = ENTRY(5,SoLog.Response," ").

            IF LOOKUP(lcErrorCode,"6110,6310") > 0 THEN DO:

               fSplitTS(SoLog.TimeSlotTMS,ldaDate,liTime).
               
               liTime = liTime - 600.
               IF liTime < 0 THEN ASSIGN
                  liTime  = 86400 + liTime
                  ldaDate = ldaDate - 1.

               liNewTS = fMake2Dt(ldaDate,liTime).

               FIND FIRST bufSoLog WHERE
                          bufSoLog.MsSeq       = SoLog.MsSeq    AND
                          bufSoLog.CompletedTS = liNewTS        AND
                          bufSoLog.CommLine    = SoLog.CommLine AND
                    RECID(bufSoLog) NE RECID(SoLog)
               NO-LOCK NO-ERROR.
               
               IF NOT AVAIL bufSoLog THEN DO:

                  fSplitTS(SoLog.CompletedTS,ldaDate,liTime).
               
                  liTime = liTime + 600.
                  IF liTime > 86400 THEN ASSIGN
                     liTime  = 86400 - liTime
                     ldaDate = ldaDate + 1.

                  liNewTS = fMake2Dt(ldaDate,liTime).

                  CREATE bufSoLog.
                  ASSIGN
                     bufSoLog.SoLog       = NEXT-VALUE(SoLog)
                     bufSoLog.TimeSlotTMS = liNewTS.

                  BUFFER-COPY SoLog EXCEPT
                     SoLog.SoLog
                     SoLog.Stat
                     SoLog.Response
                     SoLog.TimeSlotTMS
                     SoLog.CompletedTS
                  TO bufSoLog.

               END.
            END.
            ELSE IF AVAIL(MSRequest) AND
               MsRequest.ReqCParam1 EQ "SHAPER" AND
               MsRequest.ReqCParam2 EQ "HSPA_ROAM_EU" THEN DO:
               
               FIND FIRST Customer WHERE
                          Customer.CustNum = MsRequest.CustNum
               NO-LOCK NO-ERROR.
            
               IF AVAIL Customer THEN liLang = INT(Customer.Language).
               ELSE                   liLang = 1.
               
               lcAlarmMess = fGetSMSTxt(
                  (IF INDEX(lcInfo,"ERROR: 90003") > 0
                   THEN "HSPARoamEUErrorUsage"
                   ELSE "HSPARoamEUErrorGeneral"), 
                  TODAY, liLang, OUTPUT ldeSMSTime).

               IF lcAlarmMess > "" THEN DO:
                  fMakeSchedSMS2(MSRequest.CustNum,
                                 MsRequest.CLI,
                                 {&SMSTYPE_CONTRACT_ACTIVATION},
                                 lcAlarmMess,
                                 ldeSMSTime,
                                 "0034633800800",
                                 "").
                  RELEASE callalarm.
               END.
            END.
         END.
           
         IF AVAIL MSREquest THEN DO:
            /* already handled */
            IF MsRequest.ReqStatus = 2 AND lcStatus = "OK" THEN ASSIGN
               MsRequest.Memo = MsRequest.Memo + 
                                (IF MsRequest.Memo > ""
                                 THEN ", " 
                                 ELSE "") +
                                " 2. response: " + 
                                REPLACE(lcResponse,CHR(10),"").
            ELSE DO:
               /* YOT-1966 */
               IF MsRequest.ReqStatus EQ 2 AND Solog.response BEGINS "OK" THEN
                  MsRequest.Memo = MsRequest.Memo + 
                                   (IF MsRequest.Memo > ""
                                    THEN ", " 
                                    ELSE "") +
                            " Request already handled, response discarded: " + 
                                   REPLACE(lcResponse,CHR(10),"").
               ELSE DO:
                   /* YPT-135 */
                  IF lcStatus NE "OK" AND
                     INDEX(lcInfo,"ERROR: 90003") > 0 AND
                     MsRequest.ReqCParam1 EQ "SHAPER" AND
                     MsRequest.ReqCParam2 EQ "HSPA_ROAM_EU" AND
                     MsRequest.OrigRequest > 0 AND
                     MsRequest.Mandatory EQ 1 THEN DO:

                     FIND bOrigRequest NO-LOCK WHERE
                          bOrigRequest.MsRequest = MsRequest.OrigRequest
                     NO-ERROR.
                     IF AVAIL bOrigRequest AND
                              bOrigRequest.ReqType = 8 AND
                LOOKUP(STRING(bOrigRequest.ReqStatus),"3,7") > 0 THEN DO:
                        fReqStatus(9, REPLACE(lcResponse,CHR(10),"") ).
                        FIND Msrequest NO-LOCK WHERE
                             Msrequest.Msrequest = bOrigRequest.MsRequest.
                        fReqStatus(9, "SubRequest 1 failed").
                     END.
                     ELSE fReqStatus(3, REPLACE(lcResponse,CHR(10),"") ).
                  END.
                  ELSE fReqStatus(IF lcStatus = "OK" THEN 6 ELSE 3,
                                  REPLACE(lcResponse,CHR(10),"") ).
               END.
            END.              
         END.        
      END.
      ELSE DO:

         IF NOT CAN-FIND(FIRST UpdateQueue WHERE
                               UpdateQueue.Seq1 = iiSoLog) THEN DO:

            CREATE UpdateQueue.
            ASSIGN
               UpdateQueue.TSCreate = fMakeTS()
               UpdateQueue.Seq1     = iiSoLog
               UpdateQueue.Value1   = pcResponse.

            RELEASE UpdateQueue.         
         
         END.
      
         lcReturn = "LOCKED".

      END.

      RELEASE SOLOG.
      RELEASE MSREQUEST.
   
   END.

   RETURN lcReturn.

END PROCEDURE.

/* make a request and post the xml document */
PROCEDURE pPostRequest:

   DEFINE INPUT PARAMETER pcRequest AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lhSocket  AS HANDLE    NO-UNDO.
   
   DEFINE VARIABLE liRequest   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE liMsg       AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lmRequest   AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE llOK        AS LOGICAL   NO-UNDO.
   DEFINE VARIABLE lcReturn    AS CHARACTER NO-UNDO INIT "OK".

   liRequest = LENGTH(pcRequest).

   SET-SIZE(lmRequest) = 0.
   SET-SIZE(lmRequest) = liRequest + 1.
   SET-BYTE-ORDER(lmRequest) = BIG-ENDIAN.

   PUT-STRING(lmRequest,1) = pcRequest.

   llOK = lhSocket:WRITE(lmRequest,1,liRequest) NO-ERROR.

   IF NOT llOK THEN lcReturn = "ERROR: Write failure.".

   SET-SIZE(lmRequest) = 0.

   RETURN lcReturn.

END PROCEDURE.

PROCEDURE pGetServerResponce:

   DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lmResponse AS MEMPTR    NO-UNDO.
   DEFINE VARIABLE llRC       AS LOGICAL   NO-UNDO.

   IF SELF:CONNECTED() = FALSE THEN DO:
      MESSAGE "ERROR: Connection failure.".
      RETURN "".
   END.
   
   SET-SIZE(lmResponse) = 0.
   SET-SIZE(lmResponse) = lhSocket:GET-BYTES-AVAILABLE() + 1.
   SET-BYTE-ORDER(lmResponse) = BIG-ENDIAN.
   llRC = lhSocket:READ(lmResponse,1,lhSocket:GET-BYTES-AVAILABLE() + 1,1).

   lcResponse = GET-STRING(lmResponse,1).
   
   SET-SIZE(lmResponse) = 0.

   RETURN lcResponse.

END PROCEDURE.
