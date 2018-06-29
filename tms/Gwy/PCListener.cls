/*
 * Polls the ESB using the STOMProtocol.
 */
ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

&GLOBAL-DEFINE IMPORTDIR ../var/import/pc

/* lang, stomp adpter, logic  libs*/ 
USING Progress.Lang.*.
USING dotr.Stomp.*.
USING dotr.Stomp.Interface.*.
USING Syst.Configuration.

&GLOBAL-DEFINE MQ_BASED NO                

CLASS Gwy.PCListener INHERITS gearbox.daemons.Daemon 
                     IMPLEMENTS dotr.Stomp.Interface.IStompHandler:
                         
    &IF NOT {&MQ_BASED}
    &THEN 
    DEFINE STREAM s-dir.
    DEFINE STREAM s-in.
    DEFINE STREAM s-ok.
    DEFINE STREAM s-error.
    DEFINE STREAM s-missing.
    &ENDIF                       
   
   DEF VAR lcBrand                AS CHAR                     NO-UNDO INIT "1".
   /*stomp protocol  settings*/ 
   DEF VAR lcHostName             AS CHAR                     NO-UNDO.
   DEF VAR lcPortNumber           AS CHAR                     NO-UNDO.
   DEF VAR lcQueueName            AS CHAR                     NO-UNDO.
   /*stomp protocol handler objects*/
   DEF VAR loStompConfig          AS dotr.Stomp.StompConfig   NO-UNDO.
   DEF VAR loStompClient          AS dotr.Stomp.StompClient   NO-UNDO.
   DEF VAR loReaderHandler        AS Gwy.PCListener           NO-UNDO.
   /*waiting for */
   DEF VAR hpWaitForProcedure     AS HANDLE                   NO-UNDO.
   /*encodings settings*/  
   DEF VAR lcUTF8_Scandics_List   AS CHAR                     NO-UNDO.
   DEF VAR lcAllUTF8              AS CHAR                     NO-UNDO.
   DEF VAR lcAllScandics          AS CHAR                     NO-UNDO.
   /*counters*/ 
   DEF VAR liConsecutiveErrors    AS INT                      NO-UNDO.
   DEF VAR liMaxConsecutiveErrors AS INT                      NO-UNDO INIT 5.
   DEF VAR llStopCondition        AS LOG                      NO-UNDO INIT NO.
   DEF VAR lcStopReason           AS CHAR                     NO-UNDO.
   /* message header fields */
   DEF VAR lcSource AS CHAR NO-UNDO. 
   DEF VAR lcReplyTo AS CHAR NO-UNDO.
   DEF VAR lcCorrelationID AS CHAR NO-UNDO. 
   
   DEF VAR lcHeader     AS CHAR NO-UNDO.
   DEF VAR lcBody       AS CHAR NO-UNDO.  
   DEF VAR lLogFileName AS CHAR NO-UNDO.

   /* File based loader variables*/
    DEF VAR lLogDir            AS CHAR        NO-UNDO INIT "../var/log/pc".
    DEF VAR lLogFileStartStamp AS DATETIME-TZ NO-UNDO.
   
   DEF VAR lhTenantInfo  AS HANDLE NO-UNDO.
   
   DEF EVENT leTooManyErrors SIGNATURE VOID (  ).
   
   CONSTRUCTOR PUBLIC PCListener ():
      ASSIGN
         lcUTF8_Scandics_List = Configuration:getc("UTF8_to_Scandics")
         lcAllUTF8            = ENTRY(1, lcUTF8_Scandics_List, "|")
         lcAllScandics        = ENTRY(2, lcUTF8_Scandics_List, "|")
         lcHostName           = Configuration:getc("PC.Queue.ip")
         lcPortNumber         = STRING(Configuration:geti("PC.Queue.port"))
         lcQueueName          = Configuration:getc("PC.Queue.Name").

      ASSIGN 
         lLogFileName = STRING(YEAR(TODAY),"9999")
                      + STRING(MONTH(TODAY),"99")
                      + STRING(DAY(TODAY),"99")
                      + REPLACE(STRING(TIME,"HH:MM:SS"),":","").           

      &IF {&MQ_BASED} &THEN   
      logMsg( STRING( SUBSTITUTE("SETTINGS: hostname=&1," ,lcHostName) )   +
              STRING( SUBSTITUTE("port=&1," ,lcPortNumber) )     +
              STRING( SUBSTITUTE("queueName=&1" ,lcQueueName) ), "INIT").
      &ELSE
      OS-CREATE-DIR VALUE("../var/import").
      OS-CREATE-DIR VALUE("{&IMPORTDIR}").
      OS-CREATE-DIR VALUE("{&IMPORTDIR}/input").
      OS-CREATE-DIR VALUE("{&IMPORTDIR}/done").
      &ENDIF
      
   END CONSTRUCTOR.
   
   METHOD PRIVATE VOID connect ():
      loStompClient     = NEW dotr.Stomp.StompClient(loStompConfig).
      loReaderHandler   = THIS-OBJECT.
      loStompClient:SUBSCRIBE(loReaderHandler).
      /*ATTENTION*/
      loStompClient:subscribeToQueueACK(lcQueueName,"client-individual").
      loStompClient:subscribeToQueueACK(lcQueueName).
      leTooManyErrors:SUBSCRIBE(tooManyErrors). 
   END METHOD.
   
   METHOD PRIVATE VOID config ():
      loStompConfig                     = dotr.Stomp.StompConfig:DEFAULT. 
      loStompConfig:StompServer         = lcHostName.
      loStompConfig:StompPort           = lcPortNumber.
      loStompConfig:LargeMessageSupport = NO.
   END METHOD.
   
   METHOD PUBLIC OVERRIDE LOG processOne ():
       
      &IF NOT {&MQ_BASED} 
      &THEN  
      DO:
      processOneFileImport().
      END.
      &ELSE    
      IF llStopCondition = TRUE
         THEN UNDO, THROW NEW Progress.Lang.AppError (lcStopReason, 550).
      
      IF NOT VALID-OBJECT(loStompConfig) THEN 
      DO:
         THIS-OBJECT:config().
      END.
      
      IF NOT VALID-OBJECT(loStompClient) THEN 
      DO:
         THIS-OBJECT:connect().
      END.
      /* IMPORTANT: This will allow events to be triggered, otherwise 
         READ-RESPONSE wont be triggered and no processing. */
      IF NOT VALID-HANDLE(hpWaitForProcedure) THEN 
          RUN Gwy/waitfor.p PERSISTENT SET hpWaitForProcedure.
      RUN runWaitForStatement IN hpWaitForProcedure.    
      &ENDIF
      
    END METHOD.

    METHOD PUBLIC VOID newStompMessage(INPUT ioStompMessage AS 
        dotr.Stomp.StompMessage):
         
        DEF VAR lcHeader AS CHAR NO-UNDO.
        DEF VAR liCnt    AS INT  NO-UNDO.
      
        ASSIGN
            lcSource        = ""
            lcCorrelationID = ""
            lcReplyTo       = "".
      
        IF ioStompMessage:Body BEGINS "SocketDisconnected" THEN 
        DO:
            APPLY "U1" TO hpWaitForProcedure.
            RETURN .
        END.
      
        lcHeader = REPLACE(ioStompMessage:HEADER,"~n"," ").
      
        IF ENTRY(1,lcHeader," ") = "MESSAGE" THEN 
        DO:
            lcHeader = SUBSTRING(lcHeader,9). /* Remove text MESSAGE + space */
             
            newJsonMessage(lcHeader , ioStompMessage:Body). /* make proper logic module */     
        END. 
        ELSE 
        DO:
            /*header logging*/
            logMsg(lcHeader, "INFO").
        END.        
      
        liConsecutiveErrors = 0.
              
        CATCH e AS Progress.Lang.Error:
            DO liCnt = 1 TO e:NumMessages:
                logMsg(e:GetMessage(liCnt), "ERROR").    

                /* for RequestMaker:logError exceptions */
                IF e:GetMessageNum(liCnt) EQ 80000 AND
                    lcCorrelationID > "" AND
                    lcCorrelationID NE "0" AND
                    lcReplyTo > "" THEN 
                DO:
                    &IF {&MQ_BASED}
                    &THEN
                    CREATE AckMessage.
                    ASSIGN 
                        AckMessage.HostTable         = "MsRequest"
                        AckMessage.KeyValue          = ""
                        AckMessage.AckTarget         = lcCorrelationId
                        AckMessage.QueueName         = lcReplyTo
                        AckMessage.Created           = NOW
                        AckMessage.AckStatus         = 0
                        AckMessage.AckResult         = "Error"
                        AckMessage.ResultDescription = e:GetMessage(liCnt).
                    RELEASE AckMessage.
                    &ENDIF
                END. 

            END.

            IF e:getclass():TypeName NE "Progress.Lang.AppError" THEN
                liConsecutiveErrors = liConsecutiveErrors + 1.

            IF liConsecutiveErrors > liMaxConsecutiveErrors THEN 
                leTooManyErrors:publish().
        END CATCH.
      
        /*is it neccesary*/
        FINALLY:
            /*ack only in case of properly dequeued message*/
            THIS-OBJECT:loStompClient:ACK(ioStompMessage).
            IF VALID-OBJECT(ioStompMessage) THEN DELETE OBJECT ioStompMessage.                        
        END FINALLY.
         
   END METHOD.
    
   METHOD PUBLIC VOID newJsonMessage (icHeader      AS LONGCHAR, 
                                      icMessageBody AS LONGCHAR):
      /* dotr Header formt */
      /*Cm integration params*/ 
      DEF VAR lcModel         AS CHAR                     NO-UNDO.
      DEF VAR lcModelId       AS CHAR                     NO-UNDO.
      DEF VAR lcAction        AS CHAR                     NO-UNDO.
      DEF VAR lcFeeId         AS CHAR                     NO-UNDO.
      /*json parsing varaibles*/
      DEF VAR lcFieldSet      AS CHAR                     NO-UNDO.
      DEF VAR lcField         AS CHAR                     NO-UNDO.
      DEF VAR lcValue         AS CHAR                     NO-UNDO.
      /*indexes*/
      DEF VAR lii             AS INT                      NO-UNDO.
      /*json handler*/
      DEF VAR loLogicModule     AS CLASS Gwy.ModelInterface NO-UNDO.
      DEF VAR lcInternalId      AS CHAR NO-UNDO. 

      DEF VAR liReplacePosition AS INT NO-UNDO.
      ASSIGN
         lcSource = ""
         lcCorrelationID = ""
         lcReplyTo = "".

      IF icHeader EQ "" THEN DO : 
          logMsg("Empty Header","WARNING").
          RETURN.
      END.
      IF icMessageBody EQ "" THEN DO:
          logMsg("Empty Body","WARNING").
          RETURN.
      END.
      DO lii = 1 TO NUM-ENTRIES(icHeader," ") :
         lcFieldSet = ENTRY(lii,icHeader," ").
         IF INDEX(lcFieldSet, ":") = 0 THEN NEXT.
         ELSE ASSIGN
            lcField    = TRIM(TRIM(ENTRY(1,lcFieldSet,":")),'"')
            lcValue    = TRIM(TRIM(ENTRY(2,lcFieldSet,":")),'"')
            lcValue    = REPLACE(lcValue,CHR(92) + "c",":").
          
         CASE lcField:
            WHEN "model" THEN
               lcModel = lcValue.
            WHEN "source" THEN
               lcSource = lcValue.
            WHEN "brand" THEN
               lcBrand = lcValue.
            WHEN "action" THEN
               lcAction = lcValue.
            WHEN "id" THEN
               lcModelId = lcValue.
            WHEN "JMSCorrelationID" OR
            WHEN "correlation-id" THEN
                lcCorrelationID = lcValue.
            WHEN "JMSReplyTo" OR 
            WHEN "Reply-To" THEN DO: 
                lcvalue = REPLACE(lcvalue,"/queue/","") .
                lcReplyTo = lcvalue.            
            END.
            /*what do we need to do in case of crappy field name*/
            /*currently not logging it*/
            OTHERWISE DO :
               logMsg("Unknown lcField " + lcField,"WARNING").
            END.
         END CASE.
      END.
        
      /*check lAllUTF8*/
      /*PROGRESS BUG:Replace latin-1 supplement chars if found in JSON*/
      REPEAT liReplacePosition = 1 TO NUM-ENTRIES(lcAllUTF8):
         icMessageBody = REPLACE(icMessageBody,
                                 ENTRY(liReplacePosition, lcAllUTF8), 
                                 ENTRY(liReplacePosition, lcAllScandics)).
      END.

      IF LOG-MANAGER:LOGGING-LEVEL GE 3 THEN DO:
         logMsg(SUBST("JSON head: `Model=&1 ModelId=&2 Action=&3 " +
                      "Feeid=&4 Source=&5 correlation-id=&6 Reply-To=&7`",
                      lcModel, lcModelid, lcAction, lcFeeid, lcSource,
                      lcCorrelationID, lcReplyTo),
                "DEBUG").
         /*Note: lModelid is blank for new contract or customer */
         logMsg(SUBST("JSON body: `&1`", icMessageBody), "DEBUG").
      END.
      
      /* something to the eventlog */
      IF lcSource = "" THEN lcSource = "PC".
      IF lcBrand EQ "" THEN DO:
          logMsg("Empty Brand","ERROR").
          RETURN.
      END.
           
      CASE lcModel:
         WHEN "account" THEN DO:
            loLogicModule = NEW Gwy.AccountModel(icMessageBody, lcModelId, lcSource,
                                                 lcCorrelationID, lcReplyTo).
         END.
         WHEN "account_rule" THEN DO:
            loLogicModule = NEW Gwy.AccountRulesModel(icMessageBody, lcModelId, lcSource,
                                                lcCorrelationID, lcReplyTo).
         END.
         WHEN "billing_item" THEN DO:
            loLogicModule = NEW Gwy.BillItemModel(icMessageBody, lcModelId, lcSource,
                                                  lcCorrelationID, lcReplyTo).
         END. 
         WHEN "billing_item_group" THEN DO:
            loLogicModule = NEW Gwy.BItemGroupModel(icMessageBody, lcModelId, lcSource,
                                                    lcCorrelationID, lcReplyTo).
         END. /*
         WHEN "compensation" THEN DO:
            loLogicModule = NEW Gwy.CompensationModel(icMessageBody, lcModelId, lcSource,
                                                      lcCorrelationID, lcReplyTo).
         END.  */
         WHEN "fee_model" THEN DO:
            loLogicModule = NEW Gwy.FeeModelModel(icMessageBody, lcModelId, lcSource,
                                                  lcCorrelationID, lcReplyTo).
         END.
         WHEN "invoice_section" THEN DO:
            loLogicModule = NEW Gwy.InvSectModel(icMessageBody, lcModelId, lcSource,
                                                 lcCorrelationID, lcReplyTo).
         END.  
         WHEN "package" THEN DO:
            loLogicModule = NEW Gwy.DayCampaignModel(icMessageBody, lcModelId, lcSource,
                                                     lcCorrelationID, lcReplyTo).
         END.  
         WHEN "pricelist" THEN DO:
            loLogicModule = NEW Gwy.PriceListModel(icMessageBody, lcModelId, lcSource,
                                                   lcCorrelationID, lcReplyTo).
         END.
         WHEN "rateplan" THEN DO:
            loLogicModule = NEW Gwy.RatePlanModel(icMessageBody, lcModelId, lcSource,
                                                  lcCorrelationID, lcReplyTo).
         END.
         WHEN "subscription_type" THEN DO:
            loLogicModule = NEW Gwy.CLITypeModel(icMessageBody, lcModelId, lcSource,
                                                 lcCorrelationID, lcReplyTo).
         END.
         WHEN "rating_bucket" THEN DO:
            loLogicModule = NEW Gwy.RatingBucketsModel(icMessageBody, lcModelId, lcSource,
                                                 lcCorrelationID, lcReplyTo).
         END. 
         OTHERWISE DO:
            logMsg("Unknown Model " + lcModel,"ERROR").
            RETURN.
         END.
      END.
      
      IF lcAction = "create" THEN 
          loLogicModule:create().
      ELSE IF lcAction = "update" THEN 
          loLogicModule:update().
      ELSE 
          loLogicModule:MakeOtherAction(lcAction).
      
      logMsg("Message processed", "INFO").
   
      FINALLY:
         IF VALID-OBJECT(loLogicModule) THEN 
             DELETE OBJECT loLogicModule.
      END FINALLY.     
        
   END METHOD. 

   METHOD PRIVATE VOID closeSession():
       loStompClient:UnSubscribeFromQueue(lcQueueName) NO-ERROR.
       IF VALID-OBJECT(loStompClient) THEN 
           DELETE OBJECT loStompClient.
       IF VALID-OBJECT(loStompConfig) THEN 
           DELETE OBJECT loStompConfig.
       IF VALID-OBJECT(dotr.Stomp.StompConfig:DEFAULT) THEN 
           DELETE OBJECT dotr.Stomp.StompConfig:DEFAULT.
   END METHOD.

   METHOD PUBLIC VOID tooManyErrors (  ):
      llStopCondition = TRUE.
      lcStopReason    = "Too many consecutive errors".
      APPLY "U1" TO hpWaitForProcedure.
   END METHOD.  

   METHOD PROTECTED VOID logMsg (icMsg AS CHAR, icMsgType AS CHAR):
      
      &IF {&MQ_BASED} &THEN
      LOG-MANAGER:WRITE-MESSAGE(icMsg, icMsgType).
      &ELSE
      ASSIGN lLogFileStartStamp = NOW.
      CASE icMsgType:
          WHEN "INFO"  OR 
          WHEN "DEBUG" THEN
          DO:
              OUTPUT STREAM s-ok TO VALUE(lLogDir + "/ok_" + lLogFileName + ".log") UNBUFFERED.
              PUT STREAM s-ok UNFORMATTED STRING(lLogFileStartStamp) + " " + icMsgType + ": " + icMsg SKIP.  
              OUTPUT STREAM s-ok CLOSE.
          END.
          WHEN "ERROR" THEN
          DO:
              OUTPUT STREAM s-error TO VALUE(lLogDir + "/error_" + lLogFileName + ".log") UNBUFFERED.
              PUT STREAM s-error UNFORMATTED STRING(lLogFileStartStamp) + " " + icMsgType + ": " + icMsg SKIP.  
              OUTPUT STREAM s-error CLOSE.
          END.
          WHEN "MISSING" THEN 
          DO:
            OUTPUT STREAM s-missing TO VALUE(lLogDir + "/missing_" + lLogFileName + ".log") UNBUFFERED.
            PUT STREAM s-missing UNFORMATTED STRING(lLogFileStartStamp) + " " + icMsgType + ": " + icMsg SKIP.  
            OUTPUT STREAM s-missing CLOSE.
          END.
      END CASE.
      &ENDIF
    END METHOD.

    &IF NOT {&MQ_BASED} &THEN 
    METHOD PUBLIC LOG processOneFileImport
        () :        

        &SCOPED-DEFINE SHUTDOWN_CHECK_TIME 30

        DEFINE VARIABLE lFileCount AS INTEGER   NO-UNDO.
        DEFINE VARIABLE lFile      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lFilePath  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lLiveTime  AS INTEGER   NO-UNDO.
    
        lLiveTime = TIME.
    
        lFileCount = 0.
            
        INPUT STREAM s-dir THROUGH ls -1tr "{&IMPORTDIR}/input" NO-ECHO.
        ERROR-STATUS:ERROR = FALSE. 
            
        LOOP:
        REPEAT ON ERROR UNDO, LEAVE :
            IMPORT STREAM s-dir lFile.
            FILE-INFO:FILE-NAME = "{&IMPORTDIR}/input/" + lFile.
            
            IF INDEX(FILE-INFO:FILE-TYPE,"F") = 0 THEN NEXT.
            ASSIGN lFileCount = lFileCount + 1.

            logMsg(lFile, "INFO").
            lFilePath = FILE-INFO:FULL-PATHNAME.  

            fileInput(lFilePath). 
            
                OS-COMMAND SILENT 
                    mv
                    VALUE(lFilePath)
                    VALUE
                    (SUBSTITUTE
                    ("{&IMPORTDIR}/done/&1&2&3&4_&5",
                    YEAR(TODAY),
                    STRING(MONTH(TODAY),"99"),
                    STRING(DAY(TODAY),"99"),
                    REPLACE(STRING(TIME,"HH:MM:SS"),":",""),
                    lFile                  
                    )
                    ).
                
            IF TIME - lLiveTime > {&SHUTDOWN_CHECK_TIME} OR 
               TIME - lLiveTime < 0 THEN 
               LEAVE LOOP. /* time to check shutdown */
        END.

        IF ERROR-STATUS:ERROR THEN 
            logMsg(ERROR-STATUS:GET-MESSAGE(1), "ERROR").
        
        INPUT STREAM s-dir CLOSE.
        
    END METHOD.
    
    METHOD PUBLIC LOG fileInput (iFile AS CHARACTER) :
        DEFINE VARIABLE lRecord    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lHeader    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lJsonBody  AS CHAR NO-UNDO.
        
        INPUT STREAM s-in FROM VALUE(iFile) NO-MAP NO-CONVERT.
        
        REPEAT:
            IMPORT STREAM s-in UNFORMATTED lRecord.
            
            ASSIGN 
                lHeader   = ENTRY(1,lRecord,"^")
                lHeader   = TRIM(lHeader,'~{')
                lHeader   = TRIM(lHeader,'~}')
                lJsonBody = TRIM(ENTRY(2,lRecord,"^")).
                
            newJsonMessage(lHeader,lJsonBody).    
        END.     
        
        INPUT STREAM s-in CLOSE.

    END METHOD.
    &ENDIF 

    DESTRUCTOR PUBLIC PCListener():
    closeSession().
    END DESTRUCTOR.

END CLASS.

