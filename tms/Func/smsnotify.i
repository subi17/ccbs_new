{fmakesms.i}

/* get the SMS recipients */
FUNCTION GetSMSRecipients RETURNS CHARACTER
    (iConfigFile AS CHAR):
    
    DEF VAR lcSMSRecip     AS CHAR NO-UNDO.
    DEF VAR lcSMSAddr      AS CHAR NO-UNDO.
    DEF VAR lcSMSError     AS CHAR NO-UNDO.
    DEF VAR lcMailComm     AS CHAR NO-UNDO.
    
    ASSIGN lcSMSRecip = ""
           lcSMSAddr  = "".
    
    IF SEARCH(iConfigFile) = ?
        THEN ASSIGN lcSMSError = "Config File " + iConfigFile +
                                 " missing. ".
    ELSE DO:                   
        INPUT FROM VALUE(iConfigFile) NO-ECHO.
        REPEAT:
            IMPORT UNFORMATTED lcSMSRecip.
            IF lcSMSRecip > "" THEN
            ASSIGN lcSMSAddr = lcSMSAddr +
                               (IF lcSMSAddr NE ""
                                THEN ","
                                ELSE "") +
                                lcSMSRecip.
        END.
        INPUT CLOSE.
    END.
    
    IF lcSMSError > "" THEN DO:               
        OUTPUT TO /tmp/tmssms.log.
        PUT UNFORMATTED 
            THIS-PROCEDURE:FILE-NAME SKIP
            lcSMSError SKIP.
        OUTPUT CLOSE.
        ASSIGN 
          /* parameters FOR sending the Error EMail */
          lcMailComm = "/opt/local/bin/sendatt" +
                       " -s TMS_SMS_error" +      /* subject   */
                       " -c /tmp/tmssms.log" +    /* content   */
                       " -f starnet@starnet.fi" + /* sender    */
                       " -m " +
                       " "    + "ari@starnet.fi" +      
                       " /tmp/tmssms.log".        /* recipient */
        UNIX SILENT VALUE(lcMailComm + " >>/tmp/sendmail.log").
        RETURN "".
    END.
    ELSE
        RETURN lcSMSAddr.
    
END FUNCTION.

FUNCTION fSMSNotify RETURN CHARACTER
   (icType            AS CHAR,
    icSMSReplacedText AS CHAR,
    icAddrConfDir     AS CHAR,
    iIniSeconds       AS INT,
    iEndSeconds       AS INT):
   
   DEF VAR i                   AS INT  NO-UNDO.
   DEF VAR lcSMSAddr           AS CHAR NO-UNDO.
   DEF VAR lcAddrConfDirNotify AS CHAR NO-UNDO.
   
   ASSIGN lcAddrConfDirNotify = icAddrConfDir + "smsinvoice.sms".
   
   lcSMSAddr = GetSMSRecipients(lcAddrConfDirNotify).
   
   IF lcSMSAddr > "" THEN
   DO i = 1 TO NUM-ENTRIES(lcSMSAddr,","):
         fMakeSchedSMS2(0,
                        ENTRY(i,lcSMSAddr,","),
                        44,
                        icType + " Invoice " + icSMSReplacedText,
                        fMakeTS(),
                        "Fact. Yoigo",
                        STRING(iIniSeconds) + "-" + STRING(iEndSeconds)).

         IF AVAIL CallAlarm THEN RELEASE CallAlarm.
   END.
END FUNCTION.
