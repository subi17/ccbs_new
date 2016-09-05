    
{Syst/commpaa.i}
{Func/timestamp.i}
{Func/xmlfunction.i}
{Func/heartbeat.i}
{Func/fgettxt.i}

gcBrand = "1".

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
      /* others don't exist yet ! */
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

DEFINE VARIABLE lcTime1    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTime2    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop     AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNagios   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNagios   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcResponse AS CHARACTER NO-UNDO.

DEFINE BUFFER bufPP  FOR PrePaidRequest.
DEFINE BUFFER bufMob FOR MobSub.

FORM
   SKIP(1)
   "Started"      AT 2
   "Messages"     TO 30
   "Last process" TO 50 SKIP
   lcTime1 AT 2   FORMAT "X(19)" 
   liLoop  TO 30  FORMAT "zzzz9"
   lcTime2 TO 50  FORMAT "X(19)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " IVR PROCESSOR " WIDTH 54 ROW 8
FRAME frmMain .

ASSIGN
   liLoop   = 0
   lcTime1  = fTS2HMS(fMakeTS())
   lcTime2  = lcTime1
   lcNagios = "ivrh:IVR Handler".

/* Nagios keep alive signal */
liNagios = fKeepAlive(lcNagios).

LOOP:
REPEAT ON STOP UNDO, LEAVE ON QUIT UNDO, LEAVE:
   
   lcTime2  = fTS2HMS(fMakeTS()).
  
   DISPLAY
      lcTime1
      liLoop
      lcTime2
   WITH FRAME frmMain.

   PUT SCREEN ROW 23 "Idle...          ".

   /* Nagios keep alive signal */
   liNagios = fKeepAlive(lcNagios).

   PUT SCREEN ROW 23 "Processing...    ".

   FOR EACH PrePaidRequest NO-LOCK WHERE
            PrePaidRequest.Brand    = gcBrand AND
            PrePaidRequest.Source   = "IVR"   AND
           (PrePaidRequest.PPStatus = 0 OR
            PrePaidRequest.PPStatus = 9),
      FIRST MobSub NO-LOCK WHERE
            MobSub.CLI    = PrePaidRequest.CLI,
      FIRST MsRequest   NO-LOCK WHERE
            MsRequest.MsSeq      = MobSub.MsSeq AND
            MsRequest.ReqType    = 1            AND
            MsRequest.ReqStatus  > 0            AND
            MsRequest.ReqCParam1 = "PROFILE"    AND
            MsRequest.ActStamp   = PrePaidRequest.TSRequest:

      IF MobSub.MsStat = 1 AND MsRequest.ReqStatus = 2 THEN DO:
            
         FIND FIRST bufMob WHERE
              RECID(bufMob) = RECID(MobSub)
         EXCLUSIVE-LOCK.
         
         bufMob.MsStatus = 4.
             
         FIND FIRST SubSer WHERE
                    SubSer.MsSeq   = bufMob.MsSeq AND
                    SubSer.ServCom = "VMS"        AND
                    SubSer.SSDate  = TODAY
         EXCLUSIVE-LOCK NO-ERROR.
      
         IF NOT AVAILABLE SubSer THEN DO:
            CREATE SubSer.
            ASSIGN
               SubSer.MsSeq   = bufMob.MsSeq
               SubSer.ServCom = "VMS"
               SubSer.SSDate  = TODAY.
         END.
         SubSer.SSStat = 1.

      END.

      IF MsRequest.ReqStatus = 2 THEN DO:
         
         RUN pIvrProcess(MobSub.MsSeq).
         
         lcResponse = RETURN-VALUE.
         
         FIND FIRST bufPP WHERE
              RECID(bufPP) = RECID(PrePaidRequest)
         EXCLUSIVE-LOCK NO-ERROR.

         CASE lcResponse:
            WHEN "OK"    THEN ASSIGN
               bufPP.PPStatus = 2
               liLoop         = liLoop + 1.
            WHEN "ERROR" THEN bufPP.PPStatus = 9.
         END.
         
      END.
      /* change status to handled but error */
      ELSE IF PrePaidRequest.PPStatus = 0 THEN DO:

         FIND FIRST bufPP WHERE
              RECID(bufPP) = RECID(PrePaidRequest)
         EXCLUSIVE-LOCK NO-ERROR.
         
         bufPP.PPStatus = 9.

      END.

   END.

   PUT SCREEN ROW 23 "Hit F8 to QUIT ..".

   READKEY PAUSE 15.

   PUT SCREEN ROW 23 "                 ".
   
   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN LEAVE LOOP.

END.

QUIT.

PROCEDURE pIvrProcess:

   DEFINE INPUT PARAMETER piMsSeq AS INTEGER NO-UNDO.

   DEFINE VARIABLE liPPRequest AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcResponse  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcRespCode  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcReturn    AS CHARACTER NO-UNDO INIT "ERROR".

   DEFINE BUFFER bufPP FOR PrePaidRequest.

   RUN Mm/topupcamp.p(piMsSeq, OUTPUT liPPRequest).

   IF liPPRequest NE 0 THEN DO:

      RUN Gwy/pp_platform.p(gcBrand,liPPRequest).

      lcResponse = RETURN-VALUE.
      
      lcRespCode = fGetRPCNodeValue(lcResponse,"responseCode").
      
      FIND FIRST bufPP WHERE
                 bufPP.Brand     = gcBrand AND
                 bufPP.PPRequest = liPPRequest
      EXCLUSIVE-LOCK NO-ERROR.
      
      /* web order request */
      bufPP.PPStatus = 2.

      /* response code when integer */
      bufPP.RespCode = INT(lcRespCode) NO-ERROR.
      
      IF AVAIL bufPP AND lcRespCode = "0" THEN DO:

         fCallAlarm("TopUpOrder",
                    bufPP.CLI,
                    INT(bufPP.TopUpAmt)).

         lcReturn = "OK".

      END.
      
      RELEASE bufPP.
      
   END.

   RETURN lcReturn.

END PROCEDURE.
