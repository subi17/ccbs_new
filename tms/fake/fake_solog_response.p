/* Set Solog to status Ok for a defined list of ServCom */

{commpaa.i}
katun = "fakeSolog".
gcBrand = "1".
{timestamp.i}
{msreqfunc.i}
{fgettxt.i}
{fmakesms.i}

DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldToday    AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTime2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liAmount   AS INTEGER   NO-UNDO.
DEFINE VARIABLE liRequests   AS INTEGER   NO-UNDO.

DEFINE VARIABLE lcListServCom AS CHARACTER NO-UNDO.
lcListServCom = "VMS,CF".

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "Request Qty:" liRequests FORMAT ">>>>>>>9"
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " Solog Response Fake "
FRAME frmLog.


FakeSolog:
DO WHILE TRUE :

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP  liLoop
         Today @ ldToday
         liRequests
         string(time,"hh:mm:ss") @ lctime
   WITH FRAME frmLog.
   PAUSE 0.

   RUN pFakeSolog(lcListServCom,
                   OUTPUT liAmount).

   liRequests = liRequests + liAmount.

   PUT SCREEN ROW 22 COL 1
   "F8 TO QUIT, OTHER KEYS START SOLOG RESPONSE FAKE IMMEDIATELLY".

   READKEY PAUSE 15.

   IF KEYLABEL(LASTKEY) = "F8" THEN DO:
       LEAVE FakeSolog.
   END.

END.

IF VALID-HANDLE(ghFunc1) THEN DELETE OBJECT ghFunc1 NO-ERROR.

QUIT.


PROCEDURE pFakeSolog:

DEFINE INPUT PARAMETER pcListServCom AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER poNumReq AS INTEGER NO-UNDO.

poNumReq = 0 .

DEF VAR liCli AS INT NO-UNDO.
DEFINE VARIABLE lcAlarmMess AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLang      AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldeSMSTime  AS DEC       NO-UNDO.

FOR EACH MsRequest NO-LOCK WHERE
         MsRequest.Brand = gcBrand AND
         LOOKUP(STRING(MsRequest.ReqType),"0,1,13,15,18,19,82,83") > 0 AND
         MsRequest.ReqStatus = 5 :
        /*   AND
         LOOKUP(MsRequest.ReqCParam1,pcListServCom) > 0:
        */

      /* special test range, used with production EMA */
     liCli = INT(MsRequest.cli) NO-ERROR.
     IF ERROR-STATUS:ERROR THEN NEXT.
     IF ((licli >= 633993700 AND licli <= 633993750) OR
         (licli >= 633993500 AND licli <= 633993620)) THEN NEXT.

      FIND SoLog WHERE
           Solog.Brand = gcBrand AND
           SoLog.MsSeq = MsRequest.MsSeq AND
           SoLog.Stat = 0 AND
           SoLog.MsRequest = MsRequest.MsRequest
           EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL SoLog THEN NEXT.

      ASSIGN
         Solog.CompletedTS = fMakeTS()
         Solog.stat        = 2
         SoLog.Response    = "OK (FAKE)".

      /* OK create for MNP => callalarm */
      IF INDEX(ENTRY(1,SoLog.CommLine),"CREATE") > 0 AND
         INDEX(SoLog.CommLine,"MNP=1") > 0           AND
         AVAIL MsRequest THEN DO:
         FIND FIRST Customer WHERE
                    Customer.CustNum = MsRequest.CustNum NO-LOCK NO-ERROR.
         IF AVAIL Customer THEN liLang = INT(Customer.Language).
         ELSE                   liLang = 1.

         lcAlarmMess = fGetSMSTxt("MNPDone",
                                  TODAY, liLang, OUTPUT ldeSMSTime).
         lcAlarmMess = REPLACE(lcAlarmMess,"#CLI",SoLog.CLI).

         IF lcAlarmMess > "" THEN DO:
            fMakeSchedSMS2(MsRequest.CustNum,
                           MsRequest.CLI,
                           12,
                           lcAlarmMess,
                           ldeSMSTime,
                           "622",
                           "").
            RELEASE callalarm.
         END.
      END. /* IF INDEX(ENTRY(1,SoLog.CommLine),"CREATE") > 0 AND */

      RELEASE Solog.

      fReqStatus(6 ,"OK" ).
      poNumReq = poNumReq + 1 .
END.

END PROCEDURE.



/*
PROCEDURE pSoLog:

   DEFINE INPUT PARAMETER iiSolog    AS INTEGER   NO-UNDO.
   DEFINE INPUT PARAMETER lcResponse AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lcStatus   AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lcInfo     AS CHARACTER NO-UNDO.

   DEFINE BUFFER bufSoLog FOR SoLog.

   FIND FIRST bufSoLog WHERE
              bufSoLog.SoLog = iiSoLog EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

   IF NOT LOCKED(bufSoLog) AND
      NOT ERROR-STATUS:ERROR AND
      bufSolog.Stat = 0 THEN DO:

       ASSIGN
            bufSolog.CompletedTS = fMakeTS()
            bufSolog.stat        = 2.

       IF lcStatus = "OK" THEN bufSoLog.Response = bufSoLog.response + lcStatus.
       ELSE bufSoLog.Response = bufSoLog.response + lcStatus + " " + lcInfo.


       FIND FIRST MsRequest WHERE
                  MsRequest.MSrequest  = bufSolog.MSRequest NO-LOCK NO-ERROR.

       IF AVAIL MsRequest THEN DO:
                fReqStatus(IF lcStatus = "OK" THEN 6 ELSE 3,
                          REPLACE(lcResponse,CHR(10),"") ).
                 DISPLAY MsRequest.ReqStatus.
       END.
   END.

   RELEASE bufSolog.

END PROCEDURE.

*/

