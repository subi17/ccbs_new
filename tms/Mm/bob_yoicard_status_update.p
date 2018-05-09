/*------------------------------------------------------------------------
    File        : Mm/bob_yoicard_status_update.p
    Purpose     : Bob Tool to update/create yoicard status  
    Author(s)   : Ashok
    Notes       : Input file Line should have Order and Status atleast
                  Data has fixed length
                     1-10 (order)  10 chars 
                     11-11 ( Status) 1 char
                  creates one generic log file per day - Log dir
                  creates one log file for each input file - Outgoing dir  
  ----------------------------------------------------------------------*/

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/ftransdir.i} 
{Syst/eventval.i}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt_file
    FIELD file_name     AS CHARACTER 
    FIELD base_filename AS CHARACTER .
DEFINE VARIABLE lcLogsDirectory      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncomingDirectory  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutGoingDirectory  AS CHARACTER NO-UNDO.    
DEFINE VARIABLE lcProcessedDirectory AS CHARACTER NO-UNDO.

DEFINE STREAM sFilesInDir.  
DEFINE STREAM sCurrentFile. 
DEFINE STREAM sCurrentLog.  
DEFINE STREAM sOutgoingLog.
DEFINE STREAM inFile.

DEFINE VARIABLE lcActionID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTableName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcToday         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE llUpdateAL      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ldCurrentTimeTS AS DECIMAL   NO-UNDO.

DEFINE VARIABLE lcOutLogFile    AS CHARACTER NO-UNDO.

/* ***************************  Main Block  *************************** */

/* Getting directories from CParams */
ASSIGN
   lcIncomingDirectory  = fCParamC("yoicardIncomingDir")
   lcProcessedDirectory = fCParamC("yoicardProcessedDir")
   lcLogsDirectory      = fCParamC("yoicardLogsDir")  
   lcOutGoingDirectory  = fCParamC("yoicardOutGoingDir") NO-ERROR.
   
FILE-INFO:FILE-NAME = lcIncomingDirectory . IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
FILE-INFO:FILE-NAME = lcProcessedDirectory. IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
FILE-INFO:FILE-NAME = lcLogsDirectory     . IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
FILE-INFO:FILE-NAME = lcOutGoingDirectory . IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.

ASSIGN 
    llUpdateAL        = TRUE 
    lcToday         = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
    lcLogFile       = lcLogsDirectory     + "/yoicard_status_log_" + lcToday  + ".log" 
    lcTableName     = "yoicard_status"
    lcActionID      = "yoicard_status_processor" 
    ldCurrentTimeTS = Func.Common:mMakeTS().

OUTPUT STREAM sCurrentLog  TO VALUE(lcLogFile) APPEND.
PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";yoicard_status_update_processing_started" SKIP.
    
DO TRANSACTION:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand AND
              ActionLog.ActionID  EQ  lcActionID       AND
              ActionLog.TableName EQ  lcTableName      NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      llUpdateAL = FALSE.
      PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";another_yoicard_status_update_processor_running" SKIP.
      RETURN.
   END.

   IF NOT AVAIL ActionLog THEN DO:
      /*First execution stamp*/
      CREATE ActionLog.
      ASSIGN
         ActionLog.Brand        = Syst.Var:gcBrand
         ActionLog.TableName    = lcTableName
         ActionLog.ActionID     = lcActionID
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";yoicard_status_update_processor_first_run" SKIP.
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END. /* ActionLog */

RUN pReadDirectory.

FINALLY:
    IF llUpdateAL THEN 
        DO TRANSACTION :
           FIND FIRST ActionLog WHERE
                      ActionLog.Brand        EQ  Syst.Var:gcBrand  AND
                      ActionLog.ActionID     EQ  lcActionID        AND
                      ActionLog.TableName    EQ  lcTableName       AND
                      ActionLog.ActionStatus NE  {&ACTIONLOG_STATUS_SUCCESS}
                      EXCLUSIVE-LOCK NO-ERROR.
           IF AVAIL ActionLog THEN 
              ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}.
           RELEASE ActionLog.
        END.

    PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";yoicard_status_update_processing_finished" SKIP.
    PUT STREAM sCurrentLog UNFORMATTED "-------------------------------" SKIP.
    OUTPUT STREAM sCurrentLog CLOSE.
END FINALLY.

PROCEDURE pReadDirectory:
    DEFINE VARIABLE lcFile      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFileName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFileFlags AS CHARACTER NO-UNDO.
    /* Collect files from incoming directory */
    INPUT STREAM sFilesInDir FROM OS-DIR(lcInComingDirectory).
    REPEAT:
        IMPORT STREAM sFilesInDir lcFile lcFileName lcFileFlags.
        IF NOT CAN-DO(lcFileFlags,"F") THEN NEXT.
        IF NOT (lcFile BEGINS "yoicard_") THEN NEXT.
        CREATE tt_file. ASSIGN tt_file.file_name = lcFileName tt_file.base_filename = lcFile. 
    END. /* repeat */
    INPUT STREAM sFilesInDir CLOSE.
    
    IF CAN-FIND(FIRST tt_file) THEN DO:
        FOR EACH tt_file:
            /* logfile for each input file */
            lcOutLogFile    = lcLogsDirectory + "/" +  SUBSTRING(tt_file.base_filename,1, R-INDEX(tt_file.base_filename,".") - 1 ) +  "_" + lcToday  +  ".log".
            OUTPUT STREAM sOutgoingLog TO VALUE(lcOutLogFile) APPEND.
            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";yoicard_status_update started."  SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";yoicard_status_update started."  SKIP.
            
            RUN pReadFile(tt_file.file_name) NO-ERROR.
            
            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";yoicard_status_update finished."  SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";yoicard_status_update finished."  SKIP.
            OUTPUT STREAM sOutgoingLog CLOSE.
            fMove2TransDir(lcOutLogFile      , "", lcOutGoingDirectory).
            fMove2TransDir(tt_file.file_name , "", lcProcessedDirectory).            
        END.
    END.
END PROCEDURE.


PROCEDURE pReadFile:
    DEFINE INPUT  PARAMETER icFileName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcLine    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liStatus  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE liOrderId AS INTEGER   NO-UNDO.
    INPUT STREAM inFile FROM VALUE(icFileName).
        REPEAT:
            IMPORT STREAM inFile UNFORMATTED lcLine.
            liOrderId = INTEGER(SUBSTRING(lcLine,1,10)).
            liStatus  = INTEGER(SUBSTRING(lcLine,11,1)).

            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + "Received Order=" + STRING(liOrderId) + ";Status=" + STRING(liStatus)  SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(icFileName) + ";Received Order=" + STRING(liOrderId) + ";Status=" + STRING(liStatus)  SKIP.
            
            RUN pCreateUpdateStatus(liOrderId,liStatus).
        END.
    INPUT STREAM inFile CLOSE.
END PROCEDURE.


PROCEDURE pCreateUpdateStatus :
    DEFINE INPUT  PARAMETER liOrderId AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER liStatus  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lAvailCard AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lcError  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE liRequest  AS INTEGER NO-UNDO.
    
    FIND Order WHERE
         Order.Brand = Syst.Var:gcBrand  AND 
         ORder.OrderId = liOrderId NO-LOCK NO-ERROR.
    IF NOT AVAILABLE order THEN DO:
        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Order Not found" SKIP.
        PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Order Not found" SKIP.
        RETURN.
    END.
    FIND FIRST Customer WHERE
           Customer.brand   EQ Syst.Var:gcBrand  AND 
           Customer.custnum EQ order.Custnum NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Customer THEN DO:
        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Customer Not found" SKIP.
        PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Customer Not found" SKIP.
        RETURN.
    END.
    FIND FIRST DCCli WHERE 
               DCCLi.brand = Customer.Brand AND 
               DCCLi.msseq = Order.msseq   AND 
               DCCli.Dcevent = "YOICARD" EXCLUSIVE-LOCK NO-ERROR.
    IF LOCKED DCCli THEN DO:
        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Contract record locked" SKIP.
        PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Contract record locked" SKIP.
        RETURN.
    END. 
    ELSE IF NOT AVAILABLE DCCLi THEN DO:
       liRequest = fPCActionRequest(Order.msseq,
                                    "YOICARD" ,
                                    "act"     ,
                                    Func.Common:mMakeTS() ,
                                    FALSE     , /* fees */
                                    {&REQUEST_SOURCE_YOIGO_TOOL},
                                    "",
                                    0,
                                    FALSE,
                                    "",
                                    0,
                                    0,
                                    '',
                                    OUTPUT lcError).    
        IF liRequest > 0 THEN DO:
            FIND bCreaReq WHERE bCreaReq.MsRequest = liRequest EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE bCreareq THEN DO:
                bCreaReq.ReqIParam1 = liStatus.
            END.
            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Request created" SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Request created" SKIP.
            RELEASE bCreaReq. 
        END.
        ELSE DO:
            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Request creation Error;" + lcError SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Error=Request creation Error;" + lcError SKIP.
            RETURN.
        END.
    END.
    ELSE DO: 
        DCCLi.ServiceStatus = liStatus.
        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Status updated" SKIP.
        PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";"  + "Order=" + STRING(liOrderId) + ";Status updated" SKIP.
    END.
END PROCEDURE.