/* ----------------------------------------------------------------------------
  MODULE .......: SOGREQUEST.P
  FUNCTION .....: Loop for SOG requests
  APPLICATION ..: TMS
  CREATED ......: 08.02.2005 kl
  CHANGED ......: 28.05.2006 kl  updatequeue.i renamed to tmsmisc.i
                  02.01.2007 tk  release HistMSISDN
                  02.01.2007 kl  llTime, pSogRequest
                  19.02.2007 mvi exclude sologs with CLI in XLIST during test
                                 of First call change. They will be handled
                                 by the secondary sogrequest.
                  20.03.2007 kl  Send KillMs.UserCode to fSubscriptionRequest
                  29.01.2009 mk  llTime time restriction removed 
                                 due to YCM-1245

  Version ......: TMS / Production version
  --------------------------------------------------------------------------- */

{Syst/commpaa.i} katun = "SOG". gcbrand = "1".
{Func/msreqfunc.i}
{Func/tmsparam4.i}

gcBrand = "1".

DEFINE VARIABLE ldToday       AS DATE      NO-UNDO.
DEFINE VARIABLE lcTime        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogPath     AS CHARACTER NO-UNDO.
DEFINE VARIABLE liLoop        AS INTEGER   NO-UNDO.
DEFINE VARIABLE liPause       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcReturn      AS CHARACTER NO-UNDO INIT "ERROR".
DEFINE VARIABLE liNagios      AS INTEGER   NO-UNDO.
DEFINE VARIABLE liMSSeq       AS INTEGER   NO-UNDO.
DEFINE VARIABLE liParams      AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcParams      AS CHARACTER NO-UNDO EXTENT 4.
DEFINE VARIABLE liAmount      AS INT       NO-UNDO.
DEFINE VARIABLE lcTime2       AS CHAR      NO-UNDO FORMAT "X(20)" .
DEFINE VARIABLE lcCommandLine AS CHAR      NO-UNDO FORMAT "X(37)" .
DEFINE VARIABLE lcNagios      AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocResult      AS CHAR      NO-UNDO.
DEFINE VARIABLE ocError       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcURL         AS CHARACTER NO-UNDO.
DEFINE VARIABLE clsNagios     AS CLASS Class.nagios    NO-UNDO.
DEFINE VARIABLE clsTimeDate   AS CLASS Class.timedate  NO-UNDO.
DEFINE VARIABLE liTime        AS INTEGER   NO-UNDO.
DEFINE VARIABLE llTime        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lhSocket      AS HANDLE NO-UNDO.
DEFINE VARIABLE lcLogFile     AS CHAR NO-UNDO. 
DEFINE VARIABLE llLogFile     AS LOG NO-UNDO. 

DEF VAR lcHostName AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,"hebe,pallas") = 0 AND
   LOOKUP(lcHostName,{&HOSTNAME_STAGING}) = 0 THEN DO:
   MESSAGE "Unknown host" lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.

DEFINE BUFFER xxSolog  FOR Solog.

DEFINE STREAM sLog.
DEFINE STREAM sLogFile.

clsNagios   = NEW Class.nagios().
clsTimeDate = NEW Class.timedate().

ASSIGN
   lcLogPath   = fCParamC4(gcBrand,"SOG","LogDirectory")
   lcURL       = fCParamC4(gcBrand,"SOG","URL_Post")
   lcLogFile   = fCParamC4(gcBrand,"SOG","LogFile").

IF lcLogPath > "" AND lcLogFile > "" THEN DO:
   llLogFile = True.
   OUTPUT STREAM sLogFile TO
      VALUE(lcLogPath + lcLogFile) APPEND.
END.

FORM
   SKIP
   "ROUND:" liLoop   FORMAT ">>>>>>9"
   ldToday  FORMAT "99.99.9999"
   lcTime
   "TotAmount:" liAmount 
WITH
   ROW 6 WIDTH 60 CENTERED OVERLAY NO-LABEL
   TITLE " Yoigo SOG Request  "
FRAME frmLog.


FORM
   SKIP
   lcTime2 
   ocError FORMAT "X(14)"
   lcCommandline FORMAT "X(37)" 
   WITH
   ROW 9 WIDTH 78 11 DOWN  CENTERED OVERLAY NO-LABEL
   TITLE " LATEST SOLOGs " 
  FRAME frmDetail.

FUNCTION fErrorLog RETURNS LOGICAL
  (INPUT pcOwner AS CHARACTER):

   DEFINE VARIABLE liError AS INTEGER   NO-UNDO.
   
   OUTPUT STREAM sLog TO VALUE(lcLogPath + "connection_error.txt") APPEND.
   
   DO liError = 1 TO ERROR-STATUS:NUM-MESSAGES:

      PUT STREAM sLog UNFORMATTED
         TODAY FORMAT "99.99.9999" CHR(9)
         STRING(TIME,"hh:mm:ss")   CHR(9)
         pcOwner                   CHR(9)
         ERROR-STATUS:GET-NUMBER(liError)  CHR(9)
         ERROR-STATUS:GET-MESSAGE(liError) CHR(10).

   END.

   OUTPUT STREAM sLog CLOSE.

END.

ASSIGN
   lcNagios = "sreq:Activ. Request Reader"
   liNagios = clsNagios:KeepAlive(lcNagios).

PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                    STRING(time,"hh:mm:ss").
                                         
Yoigo:
DO WHILE TRUE :

   liLoop = liLoop + 1.
   PAUSE 0.
   DISP
      liLoop  
      Today @ ldToday 
      liAmount 
      string(time,"hh:mm:ss") @ lctime
   WITH FRAME frmLog.
   PAUSE 0.

   ASSIGN
      liTime = TIME
      llTime = TRUE.
      
   IF llTime THEN llTime = (fCParamC4(gcBrand,"ServiceBreak","Activation") = "0").

   IF llTime THEN RUN pSogRequest.

   PUT SCREEN ROW 23 "                                  ".   
   
   PUT SCREEN ROW 22 COL 1
      "F8 TO QUIT, OTHER KEYS START SOG REQUEST IMMEDIATELLY".
   
   READKEY PAUSE 5.

   liNagios = clsNagios:KeepAlive(lcNagios).
   
   PUT SCREEN ROW 1 COL 1 "NAGIOS: " + STRING(today,"99-99-9999") + " " +
                                      STRING(time,"hh:mm:ss").
   
   IF KEYLABEL(LASTKEY) = "F8" THEN LEAVE Yoigo.

END.

IF VALID-HANDLE(lhSocket) THEN DO:
   lhSocket:DISCONNECT() NO-ERROR.
   DELETE OBJECT lhSocket NO-ERROR.
END.

IF llLogFile THEN
   OUTPUT STREAM sLogFile CLOSE.

QUIT.

FUNCTION fCheckStagingMSISDN RETURNS LOGICAL
   (icCLI AS CHAR,
    icCommline AS CHAR):

   DEF VAR lcTemp AS CHAR NO-UNDO. 
   DEF VAR liCLi AS INT NO-UNDO. 
   DEF VAR lcCLI AS CHAR NO-UNDO.
   DEF VAR i AS INT NO-UNDO.
         
   liCLi = INT(icCLI) NO-ERROR.
   IF ERROR-STATUS:ERROR THEN NEXT.
   IF NOT ((liCLi >= 633993700 AND liCLi <= 633993750) OR
           (liCLi >= 633993500 AND liCLi <= 633993620)) THEN RETURN FALSE.
   
   /* Check DSS command MSISDNS list */
   IF index(iccommline,"DSS-ACCOUNT") > 0 and
      index(iccommline,"MSISDNS=") > 0 then do:
      lcTemp = substring(iccommline, index(iccommline,"MSISDNS=")).
      lcTemp = entry(1,lcTemp,",").
      lcTemp = entry(2,lcTemp,"=").
      DO i = 1 TO NUM-ENTRIES(lcTemp,";"):
         lcCli = TRIM(ENTRY(i,lcTemp,";")).
         IF lcCli BEGINS "34" THEN lcCLi = SUBSTRING(lcCLi,3).
         liCLi = INT(lcCLi) NO-ERROR.
         IF ERROR-STATUS:ERROR THEN RETURN FALSE.
         IF NOT ((liCLi >= 633993700 AND liCLi <= 633993750) OR
                 (liCLi >= 633993500 AND liCLi <= 633993620))
         THEN RETURN FALSE.
      END.
   END.

   RETURN TRUE.

END FUNCTION. 

PROCEDURE pSogRequest:

   FOR EACH Solog NO-LOCK WHERE 
            Solog.Brand        = gcBrand    AND 
            Solog.Stat         = 0          AND 
            Solog.TimeSlotTMS <= clsTimeDate:MakeTS()
   BREAK BY Solog.TimeSlotTMS     .

      /* Staging TMS (merak) can access production EMA.
         Block all numbers outside staging test number ranges */
      IF LOOKUP(lcHostName,{&HOSTNAME_STAGING}) > 0 THEN DO:
         IF fCheckStagingMSISDN(solog.cli, solog.commline) EQ FALSE THEN NEXT.
      END.

      IF solog.Commline = ? OR 
         Solog.commline = "" THEN NEXT.

      PAUSE 0 NO-MESSAGE.

      PUT SCREEN ROW 23 "Running Solog " + string(solog.solog).            

      RUN Gwy/sogpost.p(Solog.CommLine, "LOGIN yoigo toro", lcURL, 
                    INPUT-OUTPUT lhSocket, OUTPUT ocError).

      lcCommandline = ENTRY(1,(ENTRY(2,Solog.CommLine," ")),",") + " " +
                      ENTRY(3,Solog.Commline,",")                + " " .

      DO liLoop = 7 TO NUM-ENTRIES(Solog.Commline,","):
                          
        lcCommandline = lcCommandline + entry(liLoop,solog.commline,",") + " ".

      ENd.   
                       
      lcCommandline = replace(lcCommandline,"MSISDN=","").                   
      
      PAUSE 0.
      
      disp 
         clsTimeDate:TS2HMS(solog.timeslottms) @ lctime2 "  " 
         ocError
         lcCommandLine
      WITH  FRAME   frmDetail.
      DOWN WITH FRAME frmDetail. PAUSE 0.

      PUT SCREEN ROW 23 "Solog" + string(solog.solog) + ":" + ocerror.      
         
      IF ocError BEGINS "OK" THEN DO:
      
         IF llLogFile THEN PUT STREAM
            sLogFile UNFORMATTED solog.CommLine SKIP. 

         liAmount = liAmount + 1.
         
         FIND FIRST xxSolog WHERE 
                    xxSolog.Solog = Solog.Solog
         EXCLUSIVE-LOCK NO-ERROR.

         xxSolog.Stat = 5.
         
         RELEASE xxSolog.

      END.   
      ELSE IF ocError NE "ERROR: Connection failure." AND
              ocError NE "Failed!" THEN DO:

         FIND FIRST xxSolog WHERE
                    xxSolog.Solog = Solog.Solog
         EXCLUSIVE-LOCK NO-ERROR.

         ASSIGN 
            xxSolog.Stat = 1
            xxSolog.Response = ocError.                       

         FIND FIRST MSrequest WHERE 
                    MSRequest.MSRequest = Solog.MSrequest NO-LOCK NO-ERROR.
                    
         IF AVAIL MSrequest then fReqStatus(3,"").           
      END.

   END.

   RELEASE xxSOLOG.
   RELEASE  MSISDN.

END PROCEDURE.
