BLOCK-LEVEL ON ERROR UNDO, THROW.

USING Progress.Lang.*.
USING Syst.Configuration.

CLASS Gwy.AckMessageSender INHERITS gearbox.daemons.Daemon: 
    
    DEFINE VARIABLE viKnt        AS INTEGER                NO-UNDO.
    DEFINE VARIABLE vcHostName   AS CHARACTER              NO-UNDO.
    DEFINE VARIABLE vcPortNumber AS CHARACTER              NO-UNDO.
    DEFINE VARIABLE vcQueueName  AS CHARACTER              NO-UNDO.
    DEFINE VARIABLE vcUserName   AS CHARACTER              NO-UNDO.
    DEFINE VARIABLE vcPasscode   AS CHARACTER              NO-UNDO.
    DEFINE VARIABLE vcStatusList AS CHARACTER              NO-UNDO.
    DEFINE VARIABLE StompClient  AS dotr.Stomp.StompClient NO-UNDO.
    DEFINE VARIABLE StompConfig  AS dotr.Stomp.StompConfig NO-UNDO.
    
    DEFINE TEMP-TABLE ttResponse NO-UNDO SERIALIZE-NAME "acknowledgement"
       FIELD AckTarget         AS INTEGER     SERIALIZE-NAME "task_id"
       FIELD AckResultCode     AS INTEGER     SERIALIZE-NAME "status_code"
       FIELD AckResult         AS CHARACTER   SERIALIZE-NAME "status"
       FIELD ResultDescription AS CHARACTER   SERIALIZE-NAME "message".
    
    DEFINE TEMP-TABLE ttAddInfo NO-UNDO SERIALIZE-NAME "info"
        FIELD AttrName  AS CHAR SERIALIZE-NAME "code"
        FIELD AttrValue AS CHAR SERIALIZE-NAME "value".   
    
    DEFINE DATASET dsAckResponse FOR ttResponse, ttAddInfo.
    
    DEFINE TEMP-TABLE ttTMSCodes NO-UNDO
       FIELD CodeName  LIKE TMSCodes.CodeName
       FIELD CodeValue LIKE TMSCodes.CodeValue
       INDEX CodeName CodeName.

    CONSTRUCTOR PUBLIC AckMessageSender (  ):
   
        ASSIGN
            vcHostName   = Configuration:getc("PC.Queue.ip")
            vcPortNumber = STRING(Configuration:geti("PC.Queue.port"))
            vcUserName   = Configuration:getc("PC.User.Name")
            vcPasscode   = Configuration:getc("PC.Passcode").

        LOG-MANAGER:WRITE-MESSAGE((SUBSTITUTE("hostname=&1" ,vcHostName  )  +  
                                   SUBSTITUTE("port=&1"     ,vcPortNumber)),"INIT").
        
        IF NOT VALID-OBJECT(StompConfig) THEN 
        DO:
            StompConfig                     = dotr.Stomp.StompConfig:DEFAULT. 
            StompConfig:StompServer         = vcHostName.
            StompConfig:StompPort           = vcPortNumber.
            StompConfig:StompHost           = "/".
            StompConfig:LargeMessageSupport = NO.
        END.
        
        IF NOT VALID-OBJECT(StompClient) THEN 
            StompClient = NEW dotr.Stomp.StompClient("", vcUserName, vcPasscode).
            
        FOR EACH TMSCodes WHERE 
                 TMSCodes.TableName = "AckMessage" AND
                 TMSCodes.FieldName = "ErrorCode"  
                 NO-LOCK:
            FIND FIRST ttTMSCodes WHERE ttTMSCodes.CodeName = TMSCodes.CodeName NO-ERROR.
            IF NOT AVAILABLE ttTMSCodes THEN DO:       
               CREATE ttTMSCodes.
               BUFFER-COPY TMSCodes TO ttTMSCodes.
            END.
        END.
        
        
    END CONSTRUCTOR.

    METHOD PUBLIC OVERRIDE LOG processOne ():
        
        DEFINE VARIABLE liCnt         AS INTEGER     NO-UNDO.
        DEFINE VARIABLE liAckNum      AS INTEGER     NO-UNDO.
        
        DEFINE VARIABLE lShutdowStamp AS DATETIME-TZ NO-UNDO.
        DEFINE VARIABLE ldSleepDelay   AS DEC      NO-UNDO.
        DEFINE VARIABLE lMessages     AS LOGICAL     NO-UNDO.
        DEFINE VARIABLE lcJSON        AS LONGCHAR    NO-UNDO.
        DEFINE VARIABLE bResponse     AS HANDLE      NO-UNDO.
        DEFINE VARIABLE llSuccess     AS LOGICAL     NO-UNDO.   
        DEFINE VARIABLE lcQueueName    AS CHARACTER NO-UNDO. 
        DEFINE VARIABLE lcDefaultQueue AS CHARACTER NO-UNDO. 
        DEFINE VARIABLE liRetries      AS INTEGER NO-UNDO. 
        DEFINE VARIABLE liRetryCount   AS INTEGER NO-UNDO.  
 
        ASSIGN ldSleepDelay   = Configuration:getf("PC.Ack.Queue.Sleep","PC.Interface").
         
        IF NOT VALID-OBJECT(StompClient) THEN  
           RETURN ERROR NEW Progress.Lang.AppError("Unable to connect to server",1). 
        
        lcDefaultQueue = Configuration:getc("PC.Ack.QueueName","PC.Interface").
            
        FOR EACH AckMessage WHERE AckMessage.AckStatus = 0 EXCLUSIVE-LOCK:
            
            llSuccess = FALSE.
            
            EMPTY TEMP-TABLE ttResponse.
            EMPTY TEMP-TABLE ttAddInfo.
            
            CREATE ttResponse.
            BUFFER-COPY AckMessage EXCEPT AckMessage.AckTarget TO ttResponse.
            ASSIGN ttResponse.AckTarget = INT(AckMessage.AckTarget) NO-ERROR. 
            
            IF ERROR-STATUS:ERROR THEN 
            DO:
               ASSIGN AckMessage.AckStatus = 3.
               LOG-MANAGER:WRITE-MESSAGE("task_id is incorrect for task " + AckMessage.AckTarget,"ERROR").
               NEXT.
            END.
            
            /* As of now errorcode is only required for WinCash;Below code should be Re-factored if ErrorcCode is to be used for all Ack(s)*/
            IF AckMessage.AckResult <> "OK" THEN 
            DO:
               FIND FIRST ttTMSCodes WHERE ttTMSCodes.CodeName = ttResponse.ResultDescription NO-ERROR.
               IF NOT AVAILABLE ttTMSCodes THEN 
               DO:
                  FIND FIRST ttTMSCodes WHERE ttResponse.ResultDescription MATCHES ttTMSCodes.CodeName NO-ERROR.
                  IF AVAILABLE ttTMSCodes THEN
                     ASSIGN ttResponse.AckResultCode = INTEGER(ttTMSCodes.CodeValue).
               END.
               ELSE
                   ASSIGN ttResponse.AckResultCode = INTEGER(ttTMSCodes.CodeValue).
            END.
            
            IF AckMessage.AddInfo > "" THEN 
            DO liCnt = 1 TO NUM-ENTRIES(AckMessage.AddInfo,"|"):
                CREATE ttAddInfo.
                ASSIGN 
                    ttAddInfo.AttrName  = ENTRY(1, ENTRY(liCnt,AckMessage.AddInfo,"|") ,"=")
                    ttAddInfo.AttrValue = ENTRY(2, ENTRY(liCnt,AckMessage.AddInfo,"|") ,"=").
            END.
            
            DATASET dsAckResponse:WRITE-JSON("LONGCHAR",lcJSON,?,?,?,YES).
                            
            IF AckMessage.QueueName = "" THEN 
                lcQueueName = lcDefaultQueue.
            ELSE 
                lcQueueName = AckMessage.QueueName. 
            
            IF NOT VALID-OBJECT(StompClient) THEN  
               UNDO, THROW NEW Progress.Lang.AppError("Connection lost to server",1). 
            ELSE
               llSuccess = SendMessage(lcJSON,lcQueueName).  
            
            IF llSuccess THEN 
            DO:
               ASSIGN AckMessage.AckStatus = 2. 
               LOG-MANAGER:WRITE-MESSAGE("Message: " + STRING(lcJSON),"INFO").
            END. 
            ELSE 
               LOG-MANAGER:WRITE-MESSAGE("Message: " + STRING(AckMessage.AckTarget) + " was not send", "ERROR").
        END.
        
        PAUSE ldSleepDelay.
    END METHOD.
    
    METHOD PRIVATE LOG SendMessage(INPUT lcJSON AS LONGCHAR,
                                   INPUT lcQueueName AS CHAR):
       StompClient:SendPersistentQueue(lcQueueName, lcJSON).
       RETURN TRUE.
    
    END METHOD.
 
    DESTRUCTOR PUBLIC AckMessageSender ( ):
    
        IF VALID-OBJECT(StompClient) THEN 
            DELETE OBJECT StompClient.
        
        IF VALID-OBJECT(StompConfig) THEN 
           DELETE OBJECT StompConfig.

        IF VALID-OBJECT(dotr.Stomp.StompConfig:DEFAULT) THEN     
            DELETE OBJECT dotr.Stomp.StompConfig:DEFAULT.
            
    END DESTRUCTOR.

END CLASS.
