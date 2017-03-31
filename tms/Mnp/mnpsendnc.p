/* ----------------------------------------------------------------------
  MODULE .......: mnpsendnc.p
  TASK .........: Send MNP Messages
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 12/2009
  Version ......: xfera
  ---------------------------------------------------------------------- */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/commpaa.i}
{Func/timestamp.i}
{Func/log.i}
{Func/cparam2.i}
{Mnp/mnp.i}
{Syst/tmsconst.i}

katun = "MNP".
gcBrand = "1".
{Func/heartbeat.i}
{Func/multitenantfunc.i}

DEFINE VARIABLE liLoop      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTime      AS CHARACTER NO-UNDO.
DEFINE VARIABLE liPause     AS INTEGER   NO-UNDO.
DEFINE VARIABLE ldRetry     AS DECIMAL   NO-UNDO INIT 30.
DEFINE VARIABLE llNagBeat   AS LOG       NO-UNDO INIT TRUE.
DEFINE VARIABLE llNCTime    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liSent      AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lierrors    AS INTEGER   NO-UNDO. 
DEFINE VARIABLE lcLogDir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcURL       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcHost      AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcAddress   AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liTenant    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcTenant    AS CHARACTER NO-UNDO.

FORM
   SKIP(1)
   " Total Sent: " liSent FORMAT ">>>>>>9" SKIP
   " Errors ...: " liErrors FORMAT ">>>>>>9" SKIP
   " Loops ....: " liLoop FORMAT ">>>>>>9" SKIP
   " Last Loop.: " lcTime FORMAT "X(20)"
   SKIP(1)
WITH
   CENTERED NO-LABELS TITLE " MNP XML-RPC MESSAGE SENDER " WIDTH 40 ROW 8
FRAME frmMain .

DEFINE VARIABLE liLoops AS INTEGER NO-UNDO.

DO WHILE TRUE
    ON ERROR UNDO, THROW:

   liLoop = liLoop + 1.

   DISP
      liLoop
      liSent
      liErrors
      STRING(TODAY,"99.99.9999") + " " + STRING(TIME,"HH:MM:SS") @ lcTime
   WITH FRAME frmMain.
   PAUSE 0.

   llNCTime = fIsNCSendTime().

   IF llNCTime THEN 
   DO liTenant = 0 TO 1
      ON ERROR UNDO, THROW:

      ASSIGN lcTenant = (IF liTenant = 0 THEN {&TENANT_YOIGO} ELSE IF liTenant = 1 THEN {&TENANT_MASMOVIL} ELSE "").

      IF lcTenant = "" OR (NOT fsetEffectiveTenantForAllDB(lcTenant)) THEN
          UNDO, THROW NEW Progress.Lang.AppError("Unable to change tenant. Abort!",1). 

      lcUrl     = fCParam("MNP","MNPSendHost").
      lcAddress = fCParam("MNP","MNPSendURL").
      lcHost    = ENTRY(2,lcUrl," ") + ":" + ENTRY(4, lcUrl, " ").

      IF NOT (lcUrl > "" AND lcAddress > "") THEN DO:
         MESSAGE "Missing MNPSendHost or MNPSendURL TMSParam on tenant '" + lcTenant + "'" VIEW-AS ALERT-BOX.
         UNDO, THROW NEW Progress.Lang.AppError("Missing MNPSendHost or MNPSendURL TMSParam on tenant '" + lcTenant + "'",1).
      END.

      /* new requests */
      FOR EACH MNPOperation NO-LOCK WHERE MNPOperation.Sender = 1 AND MNPOperation.StatusCode = {&MNP_MSG_WAITING_SEND} 
         liLoops = 1 TO 60 ON ERROR UNDO, THROW:

         RUN pSendXML(RECID(MNPOperation)).
         
         IF llNagBeat = FALSE THEN 
             LEAVE.
      END.
   END.

   IF llNCTime THEN DO:
      liPause = 5.
      PUT SCREEN ROW 22 COL 1
         "F8 TO QUIT, OTHER KEYS TO PROCESS IMMEDIATELLY".
   END.
   ELSE DO:
      liPause = 60.
      PUT SCREEN ROW 22 COL 1
         "NOT MNP NC TIME, SENDING ON HOLD, F8 TO QUIT".
   END.

   IF llNagBeat THEN fKeepAlive("mnpsendnc:MNP Send"). 

   READKEY PAUSE liPause.

   IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO: 
      fCloseLog().
      QUIT.
   END. 

   PUT SCREEN ROW 22 COL 1
      "SENDING MESSAGES ............................".

END.

PROCEDURE pSendXML:

   DEFINE INPUT PARAMETER piRecId AS INTEGER NO-UNDO.

   DEFINE VARIABLE lcResponse      AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE lcResponseData  AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE lcXMLRequest    AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE lcError         AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcResponseBody  AS LONGCHAR  NO-UNDO.
   DEFINE VARIABLE lcRespCode      AS CHAR      NO-UNDO.
   DEFINE VARIABLE liContentLength AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcHttpHead      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE llcPdfData      AS LONGCHAR.
   DEFINE VARIABLE lmpContents     AS MEMPTR    NO-UNDO. 

   FIND MNPOperation WHERE RECID(MNPOperation) = piRecId EXCLUSIVE-LOCK NO-ERROR.
   
   FIND MNPProcess WHERE MNPProcess.MNPSeq = MNPOperation.MNPSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MNPProcess THEN 
       NEXT.

   PUT SCREEN ROW 23 COL 4 "Running " + MNPProcess.FormRequest + "                                    ".

   COPY-LOB MNPOperation.XMLRequest TO lcXMLRequest.

   /* special handling for pdf-files */
   IF MNPOperation.MessageType = "crearSolicitudCancelacionPortabilidadMovilIniciadaPorDonante" THEN 
   DO:
      /* in normal situation only one file should exist */
      FIND FIRST MNPCancelProposal WHERE MNPCancelProposal.MNPSeq = MNPProcess.MNPSeq NO-LOCK NO-ERROR.

      IF NOT AVAIL MNPCancelProposal OR LENGTH(MNPCancelProposal.PDF) EQ ? THEN 
      DO:
         ASSIGN
            MNPOperation.ErrorDesc = "PDF file not found"
            MNPOperation.ErrorHandled = {&MNP_ERRORHANDLED_NO}
            MNPOperation.ErrorCode = {&MNP_ERRORCODE_HANDLE}
            MNPOperation.StatusCode = {&MNP_MSG_HANDLING}.

          liErrors = liErrors + 1.
          LEAVE.
      END.
      
      COPY-LOB FROM MNPCancelProposal.PDF TO lmpContents.
      llcPdfData = BASE64-ENCODE(lmpContents).
      SET-SIZE(lmpContents) = 0. 
      lcXMLRequest = REPLACE(lcXMLRequest,"#PDF",llcPdfData).
   END.
   
   liContentLength = LENGTH(lcXMLRequest).

   lcHttpHead = SUBST("POST &1 HTTP/1.0~r~n" +
                    "User-Agent: TMSRPC/&2~r~n" +
                    "Host: &3~r~n" +
                    "Connection: &4~r~n" +
                    "Content-Type: text/xml~r~n" +
                    "Content-length: &5~r~n~r~n",
                    lcAddress,
                    "0.1",
                    lcHost,
                    STRING(false, "keep-alive/close"),
                    liContentLength).

   etime(true).
   RUN Gwy/tcpgwy_large.p(lcHttpHead + lcXMLRequest,lcURL,60,1,"", output lcResponseData).
   fLogBasic("HANDLING " + string(etime) + " " + MNPProcess.formrequest).

   lcResponse = ENTRY(1, lcResponseData ,CHR(10)).

   lcRespCode = ENTRY(2,lcResponse," ") NO-ERROR.
   
   IF ERROR-STATUS:ERROR OR NOT lcRespCode BEGINS "20" THEN 
   DO:

      lcError = STRING(lcResponse) NO-ERROR.

      PUT SCREEN ROW 23 COL 4  "Running " + MNPProcess.FormRequest + " " + lcError.
      llNagBeat = FALSE.

      fLogError(MNPProcess.FormRequest + " : " + lcError).
      liErrors = liErrors + 1.
      LEAVE.
   END.
   ELSE llNagBeat = TRUE.

   FIND MNPOperation WHERE RECID(MNPOperation) = piRecId EXCLUSIVE-LOCK.

   lcResponseBody = SUBSTRING(lcResponseData, INDEX(lcResponseData,"<?xml")) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN
      COPY-LOB lcResponseData TO MNPOperation.XMLResponse.
   ELSE 
      COPY-LOB lcResponseBody TO MNPOperation.XMLResponse.
   
   ASSIGN
      MNPOperation.SentTS = fMakeTS()
      MNPOperation.StatusCode = {&MNP_MSG_WAITING_RESPONSE_HANDLE}. /* Waiting for response handling */
   
   liSent = liSent + 1.
END.

