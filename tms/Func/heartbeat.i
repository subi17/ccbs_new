{Syst/commali.i}
{Func/timestamp.i}
{Func/date.i}
{Func/cparam2.i}

DEFINE VARIABLE lcOperator  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcServer    AS CHARACTER NO-UNDO.
DEFINE VARIABLE liOffSet    AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcNagiosUrl AS CHARACTER NO-UNDO.

/* SER-8026 change */
DEF VAR lcMonitorMethod AS CHAR NO-UNDO.
DEF VAR lcMonitorDir    AS CHAR NO-UNDO.
DEF STREAM sNagios.
ASSIGN
   lcOperator = fCparam("NAGIOS","Operator")
   liOffSet   = INT(fCParam("NAGIOS","TimeOffSet"))
   lcMonitorMethod = fCParam("NAGIOS","MonitorMethod").
IF lcMonitorMethod = ? THEN lcMonitorMethod = "File".

IF lcMonitorMethod = "File" THEN DO:
   lcMonitorDir = fCParam("NAGIOS","MonitorDir").
   IF lcMonitorDir = ? THEN lcMonitorDir = "/tmp".
END.   

input through value("hostname").
import unformatted lcServer.
input close.

DEFINE TEMP-TABLE ttNagios NO-UNDO
   FIELD tcCommand AS CHARACTER
   FIELD tdeTS1    AS DECIMAL
   FIELD tdeTS2    AS DECIMAL.

FUNCTION fSocWrite RETURNS LOGICAL
  (INPUT pcMessage AS CHARACTER,
   INPUT pcServerAndPort  AS CHARACTER):
   
   DEFINE VARIABLE lhSocket AS HANDLE  NO-UNDO.
   DEFINE VARIABLE lmMemPtr AS MEMPTR  NO-UNDO.
   DEFINE VARIABLE llReturn AS LOGICAL NO-UNDO.
   
   CREATE SOCKET lhSocket.
   lhSocket:SET-SOCKET-OPTION("SO-RCVTIMEO","10").

   lhSocket:CONNECT(pcServerAndPort) NO-ERROR.

 
   IF NOT lhSocket:CONNECTED() THEN llReturn = FALSE.
   ELSE DO:
   
      SET-SIZE(lmMemPtr) = LENGTH(pcMessage) + 1.

      PUT-STRING(lmMemPtr,1,LENGTH(pcMessage)) = pcMessage.

      lhSocket:WRITE(lmMemPtr,1,GET-SIZE(lmMemPtr)) NO-ERROR.
   
      SET-SIZE(lmMemPtr) = 0.
   
   END.
   
   lhSocket:DISCONNECT() NO-ERROR.

   DELETE OBJECT lhSocket.

   RETURN llReturn.

END FUNCTION.

/* SER-8026 change */
FUNCTION fFileWrite RETURNS LOGIC
   (icMessage AS CHAR,
    icFile AS CHAR):
    
   OUTPUT STREAM sNagios TO VALUE(icFile) APPEND.
   PUT STREAM sNagios UNFORMATTED icMessage SKIP.
   OUTPUT STREAM sNagios CLOSE.
       
END FUNCTION.

FUNCTION fNagios RETURNS LOGICAL
  (INPUT pcCommand AS CHARACTER):

   DEFINE VARIABLE lcNagios      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTimeStamp   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcCommand     AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcDescription AS CHARACTER NO-UNDO.
      
   /* nagios URL */
   FIND FIRST TMSParam WHERE
              TMSParam.Brand      = gcBrand  AND
              TMSParam.ParamGroup = "NAGIOS" AND
              TMSParam.ParamCode  = "URL"
   NO-LOCK NO-ERROR.
   IF AVAIL TMSParam AND TRIM(TMSParam.CharVal) NE "" 
      THEN lcNagiosURL = TMSParam.CharVal.
   ELSE RETURN FALSE.

   ASSIGN
      lcCommand     = ENTRY(1,pcCommand,":")
      lcDescription = ENTRY(2,pcCommand,":") WHEN
                      NUM-ENTRIES(pcCommand,":") > 1
      lcTimeStamp   = fTS2HMS(fOffSetTS(liOffSet))
      lcTimeStamp   = SUBSTR(lcTimeStamp,7,4)  +
                      SUBSTR(lcTimeStamp,4,2)  +
                      SUBSTR(lcTimeStamp,1,2)  +
                      SUBSTR(lcTimeStamp,12,2) +
                      SUBSTR(lcTimeStamp,15,2) +
                      SUBSTR(lcTimeStamp,18,2)
      lcNagios      = "[" + lcTimeStamp + "]".
      IF lcMonitorMethod = "File" THEN 
         lcNagios = lcNagios + ";" + lcServer + ";" +
                    lcOperator + ";" + lcCommand.
      ELSE lcNagios = lcNagios +
                      "PROCESS_SERVICE_CHECK_RESULT;" +
                      lcServer + ";" +
                      "Screen_" + lcOperator + "_" + 
                      UPPER(lcCommand) +
                      ";0;Screen Ok - " + lcOperator + " ".
   
   lcNagios = lcNagios + lcDescription.

   /* SER-8026 change */
   IF lcMonitorMethod = "File" THEN DO:
      fFileWrite(lcNagios,lcMonitorDir + "/" + UPPER(lcCommand) + ".txt").
   END.   
   ELSE DO:
      IF lcNagiosURL = ? OR lcNagiosURL = "" THEN RETURN FALSE.
      fSocWrite(lcNagios + CHR(10),lcNagiosUrl).
   END.
      
   RETURN TRUE.
END.

FUNCTION fKeepAlive RETURNS INTEGER
  (INPUT pcCommand AS CHARACTER):

   DEFINE VARIABLE ldeAlarmLimit AS DECIMAL   NO-UNDO INITIAL 0.00600.
   DEFINE VARIABLE lcCommand     AS CHARACTER NO-UNDO.

   lcCommand = ENTRY(1,pcCommand,":").

   FIND FIRST ttNagios WHERE
              ttNagios.tcCommand = lcCommand 
   EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL ttNagios THEN DO:

      CREATE ttNagios.
      ASSIGN
         ttNagios.tcCommand = lcCommand
         ttNagios.tdeTS1    = fOffSetTS(0).

      fNagios(pcCommand).
   END.
   
   ttNagios.tdeTS2 = fOffSetTS(liOffSet).

   IF ttNagios.tdeTS2 - ttNagios.tdeTS1 > ldeAlarmLimit THEN DO:
   
      fNagios(pcCommand).

      ASSIGN
         ttNagios.tdeTS1 = fOffSetTS(0)
         ttNagios.tdeTS2 = ttNagios.tdeTS1.

   END.

   RETURN
      INTEGER((ldeAlarmLimit - (ttNagios.tdeTS2 - ttNagios.tdeTS1)) * 100000).

END.

FUNCTION fGetRequestNagiosToken RETURNS CHAR
  (INPUT piReqType AS INT):

   DEF BUFFER RequestType FOR RequestType.
   DEF BUFFER RequestQueue FOR RequestQueue.

   FIND RequestType NO-LOCK WHERE
        RequestType.Brand = gcBrand AND
        RequestType.ReqType = piReqType NO-ERROR.
   IF NOT AVAIL RequestType THEN RETURN "".

   FIND RequestQueue NO-LOCK WHERE
        RequestQueue.Brand = gcBrand AND
        RequestQueue.Queue = RequestType.Queue NO-ERROR.
   IF NOT AVAIL RequestQueue THEN RETURN "".

   IF RequestQueue.MonitorOn AND
      RequestQueue.Monitor > "" THEN
      RETURN RequestQueue.Monitor.

   RETURN "".
END.
