
CLASS Nagios:

   DEFINE VARIABLE lcOperator AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE lcServer   AS CHARACTER     NO-UNDO.
   DEFINE VARIABLE liOffSet   AS INTEGER       NO-UNDO.
   DEFINE VARIABLE classTD    AS timedate      NO-UNDO.
   DEFINE VARIABLE lcURL      AS CHARACTER     NO-UNDO.
   /* SER-8026 change */
   DEF VAR lcMonitorMethod AS CHAR NO-UNDO.
   DEF VAR lcMonitorDir    AS CHAR NO-UNDO.
   DEF STREAM sNagios.

   CONSTRUCTOR PUBLIC Nagios():

      /* nagios URL */
      FIND FIRST TMSParam WHERE
                 TMSParam.ParamGroup = "NAGIOS" AND
                 TMSParam.ParamCode  = "URL"
      NO-LOCK NO-ERROR.
      IF AVAIL TMSParam THEN lcURL = TMSParam.CharVal.


      /* server (localhost) */
      FIND FIRST TMSParam WHERE
                 TMSParam.ParamGroup = "NAGIOS" AND
                 TMSParam.ParamCode  = "Server"
      NO-LOCK NO-ERROR.
      IF AVAIL TMSParam THEN lcServer = TMSParam.CharVal.

      /* operator prefix */
      FIND FIRST TMSParam WHERE
                 TMSParam.ParamGroup = "NAGIOS" AND
                 TMSParam.ParamCode  = "Operator"
      NO-LOCK NO-ERROR.
      IF AVAIL TMSParam THEN lcOperator = TMSParam.CharVal.

      /* possible time offset */
      FIND FIRST TMSParam WHERE
                 TMSParam.ParamGroup = "NAGIOS" AND
                 TMSParam.ParamCode  = "TimeOffSet"
      NO-LOCK NO-ERROR.
      IF AVAIL TMSParam THEN liOffSet = TMSParam.IntVal.
      
      /* SER-8026 change */
      /* possible monitor method */
      FIND FIRST TMSParam WHERE
                 TMSParam.ParamGroup = "NAGIOS" AND
                 TMSParam.ParamCode  = "MonitorMethod"
      NO-LOCK NO-ERROR.
      IF AVAIL TMSParam THEN lcMonitorMethod = TMSParam.CharVal.

      /* SER-8026 change */
      /* possible monitor dir */
      FIND FIRST TMSParam WHERE
                 TMSParam.ParamGroup = "NAGIOS" AND
                 TMSParam.ParamCode  = "MonitorDir"
      NO-LOCK NO-ERROR.
      IF AVAIL TMSParam THEN DO:
         lcMonitorDir = TMSParam.CharVal.
         IF lcMonitorDir = ? THEN lcMonitorDir = "/tmp".
      END.

      classTD = NEW timedate().

   END CONSTRUCTOR.

   DESTRUCTOR PUBLIC Nagios():

      IF VALID-OBJECT(classTD) THEN DELETE OBJECT(classTD).

   END.

   DEFINE TEMP-TABLE ttNagios NO-UNDO
      FIELD tcCommand AS CHARACTER
      FIELD tdeTS1    AS DECIMAL
      FIELD tdeTS2    AS DECIMAL.

   METHOD PUBLIC LOGICAL SocWrite
     (INPUT pcURL AS CHARACTER,
      INPUT pcMessage AS CHARACTER):
   
      DEFINE VARIABLE lhSocket AS HANDLE  NO-UNDO.
      DEFINE VARIABLE lmMemPtr AS MEMPTR  NO-UNDO.
      DEFINE VARIABLE llReturn AS LOGICAL NO-UNDO.
   
      CREATE SOCKET lhSocket.

      lhSocket:SET-SOCKET-OPTION("SO-RCVTIMEO","10").
      
      lhSocket:CONNECT(pcURL) NO-ERROR.

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

   END METHOD.

   METHOD PUBLIC LOGICAL WriteNagios
     (INPUT pcCommand AS CHARACTER):

      DEFINE VARIABLE lcNagios      AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcTimeStamp   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcCommand     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcDescription AS CHARACTER NO-UNDO.

      ASSIGN
         lcCommand     = ENTRY(1,pcCommand,":")
         lcDescription = ENTRY(2,pcCommand,":") WHEN
                         NUM-ENTRIES(pcCommand,":") > 1
         lcTimeStamp   = classTD:TS2HMS(classTD:OffSetTS(liOffSet))
         lcTimeStamp   = SUBSTR(lcTimeStamp,7,4)  +
                         SUBSTR(lcTimeStamp,4,2)  +
                         SUBSTR(lcTimeStamp,1,2)  +
                         SUBSTR(lcTimeStamp,12,2) +
                         SUBSTR(lcTimeStamp,15,2) +
                         SUBSTR(lcTimeStamp,18,2)
         lcNagios      = "[" + lcTimeStamp + "] ".
         IF lcMonitorMethod = "File" THEN 
              lcNagios = lcNagios + ";" + lcServer + ";" +
                         lcOperator + ";" + lcCommand.
         ELSE lcNagios = lcNagios +
                         "PROCESS_SERVICE_CHECK_RESULT;" +
                         lcServer + ";" +
                         "Screen_" + lcOperator + "_" + 
                         UPPER(lcCommand) +
                         ";0;Screen Ok - " + lcOperator + " ".
   
      /* SER-8026 change */
      IF lcMonitorMethod = "File" THEN DO:
         FileWrite(lcNagios,lcMonitorDir + "/" + lcCommand + ".txt").
      END.   
      ELSE DO:
         IF lcURL NE ? AND lcURL NE "" THEN
         SocWrite(lcUrl, lcNagios + CHR(10)).
      END.
 

   END METHOD.

   METHOD PUBLIC INTEGER KeepAlive
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
            ttNagios.tdeTS1    = classTD:OffSetTS(1).

         WriteNagios(pcCommand).

      END.
   
      ttNagios.tdeTS2 = classTD:OffSetTS(liOffSet).

      IF ttNagios.tdeTS2 - ttNagios.tdeTS1 > ldeAlarmLimit THEN DO:
   
         WriteNagios(pcCommand).

         ASSIGN
            ttNagios.tdeTS1 = classTD:OffSetTS(1)
            ttNagios.tdeTS2 = ttNagios.tdeTS1.
       
      END.

      RETURN
         INTEGER((ldeAlarmLimit - (ttNagios.tdeTS2 - ttNagios.tdeTS1)) * 100000).

   END METHOD.

   /* SER-8026 change */
   METHOD PUBLIC LOGICAL FileWrite
     (INPUT pcMessage AS CHARACTER,
      INPUT pcFile    AS CHARACTER):
    
      OUTPUT STREAM sNagios TO VALUE(pcFile) APPEND.
      PUT STREAM sNagios UNFORMATTED pcMessage SKIP.
      OUTPUT STREAM sNagios CLOSE.
       
   END METHOD.

END CLASS.
