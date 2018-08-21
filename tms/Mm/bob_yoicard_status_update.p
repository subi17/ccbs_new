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
Syst.Var:katun   EQ "Cron".
Syst.Var:gcBrand EQ "1".

{Syst/tmsconst.i}
{Func/fmakemsreq.i}
{Func/ftransdir.i} 
{Syst/eventval.i}
{Syst/eventlog.i}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun
   {Func/lib/eventlog.i}
END.

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE tt_file
    FIELD file_name     AS CHARACTER 
    FIELD base_filename AS CHARACTER .

DEFINE VARIABLE lcLine   AS CHARACTER NO-UNDO.
DEFINE VARIABLE liNumOK  AS INTEGER   NO-UNDO.
DEFINE VARIABLE liNumErr AS INTEGER   NO-UNDO.

/* files and dirs */
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIncDir        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessDir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcInputFile     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcDir       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessedFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProcessFile   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSpoolDir      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcReportFileOut AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutDir        AS CHARACTER NO-UNDO.

/* field variables */
DEFINE VARIABLE liOrderID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lhDCCLI        AS HANDLE     NO-UNDO.

/* streams */
DEFINE STREAM sFile.
DEFINE STREAM sLog.
DEFINE STREAM sFilesInDir.  
DEFINE STREAM sCurrentFile. 
DEFINE STREAM inFile.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

ASSIGN
   lcIncDir     = fCParam("YOICard","IncDirCC")
   lcProcessDir = fCParam("YOICard","IncProcessDir")
   lcProcDir    = fCParam("YOICard","IncProcDir")
   lcSpoolDir   = fCParam("YOICard","OutSpoolDir")
   lcOutDir     = fCParam("YOICard","OutDir").

FUNCTION fLogLine RETURNS LOGIC
   (icMessage AS CHAR):

   PUT STREAM sLog UNFORMATTED
      lcLine  "|"
      icMessage SKIP.

END FUNCTION.

FUNCTION fError RETURNS LOGIC
   (icMessage AS CHAR):

   fLogLine("ERROR:" + icMessage).

   liNumErr = liNumErr + 1.

END FUNCTION.


/********************** Execution starts here ***************************/
RUN pReadDirectory.

PROCEDURE pReadDirectory:
    DEFINE VARIABLE lcFile      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFileName  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcFileFlags AS CHARACTER NO-UNDO.
    /* Collect files from incoming directory */
    INPUT STREAM sFilesInDir FROM OS-DIR(lcIncDir).
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
           ASSIGN
              liNumOk  = 0
              liNumErr = 0.            
            lcProcessFile = fMove2TransDir(tt_file.file_name, "", lcProcessDir).
            lcLogFile = lcSpoolDir + tt_file.base_filename  + ".log".

            IF lcProcessFile EQ "" OR 
               SEARCH(lcProcessFile) EQ ? THEN NEXT.
            
            fBatchLog("START", lcProcessFile).
            
            OUTPUT STREAM sLog TO VALUE(lcLogFile) APPEND.
            PUT STREAM sLog UNFORMATTED
                       tt_file.base_filename    " "
                       STRING(TODAY,"99.99.99") " "
                       STRING(TIME ,"hh:mm:ss") SKIP.
                       
            RUN pReadFile(lcProcessFile) NO-ERROR.
            
            PUT STREAM sLog UNFORMATTED
                  "input: " STRING(liNumOK + liNumErr) ", "
              "processed: " STRING(liNumOK) ", "
                 "errors: " STRING(liNumErr) SKIP.            
                
            OUTPUT STREAM sLog CLOSE.
            ASSIGN 
                lcReportFileOut = fMove2TransDir(lcLogFile, "", lcOutDir)
                lcProcessedFile = fMove2TransDir(lcProcessFile, "", lcProcDir).
    
            IF lcProcessedFile NE "" THEN fBatchLog("FINISH", lcProcessedFile).
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
            IF TRIM(lcLine) EQ "" THEN NEXT.
            ASSIGN 
                liOrderId = INTEGER(SUBSTRING(lcLine,1,10))
                liStatus  = INTEGER(SUBSTRING(lcLine,11,1)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN DO:
                fError(lcLine + ";Invalid Data").
                NEXT.
            END.
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
        fError("Order=" + STRING(liOrderId) + ";Order Not found").
        RETURN.
    END.
    FIND FIRST Customer WHERE
           Customer.brand   EQ Syst.Var:gcBrand  AND 
           Customer.custnum EQ order.Custnum NO-LOCK NO-ERROR.
    IF NOT AVAILABLE Customer THEN DO:
        fError("Order=" + STRING(liOrderId) + ";Customer Not found").
        RETURN.
    END.
    FIND FIRST DCCli WHERE 
               DCCLi.brand = Customer.Brand AND 
               DCCLi.msseq = Order.msseq   AND 
               DCCli.Dcevent = {&YOICARD_DCEvent} EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
    IF LOCKED DCCli THEN DO:
        fError("Order=" + STRING(liOrderId) + ";Contract record locked").
        RETURN.
    END. 
    ELSE IF NOT AVAILABLE DCCLi THEN DO:
       liRequest = fPCActionRequest(Order.msseq,
                                    {&YOICARD_DCEvent} ,
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
            fLogLine("Order=" + STRING(liOrderId) + ";Request created").
            RELEASE bCreaReq. 
        END.
        ELSE DO:
            fError("Order=" + STRING(liOrderId) + ";Request creation Error;" + lcError ).
            RETURN.
        END.
    END.
    ELSE DO: 
        lhDCCLI = BUFFER DCCli:HANDLE.
        RUN StarEventSetOldBuffer(lhDCCLI).
        DCCLi.ServiceStatus = liStatus.
        RUN StarEventMakeModifyEvent(lhDCCLI).
        fLogLine("Order=" + STRING(liOrderId) + ";Status updated").
    END.
    liNumOk = liNumOk + 1 .
END PROCEDURE.
  