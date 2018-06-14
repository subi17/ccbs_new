/*------------------------------------------------------------------------
    File        : Mc/installaddr_speed_chg_bob.p
    Purpose     : Bob Tool to change Intallation address and/or speed change 
    Author(s)   : Ashok
    Created     : Thu Jan 18 10:03:09 IST 2018
    Notes       : Input file Line should have 25 entries 
                  if second entry is not "", then STC change too.
                  creates one generic log file per day - Log dir
                  creates one log file for each input file - Outgoing dir  
  ----------------------------------------------------------------------*/

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/fbankdata.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/ftransdir.i} 
{Func/lib/eventlog.i}
{Func/create_eventlog.i}  

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

DEFINE BUFFER OrderCustomer  FOR OrderCustomer.
DEFINE BUFFER MSRequest      FOR MSRequest. 
DEFINE BUFFER CliType1       FOR CLiType.
DEFINE BUFFER CliType2       FOR CliType.

DEFINE VARIABLE lhOrderCustomer AS HANDLE    NO-UNDO.
DEFINE VARIABLE llCreation      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lcLine          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNewCliType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocResult        AS CHARACTER NO-UNDO.
DEFINE VARIABLE liOrderID       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcLogFile       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcOutLogFile    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFileName      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFile          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcActionID      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTableName     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDelimiter     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldCurrentTimeTS AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ldActivationTS  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE llCreateSTC     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE liRequest       AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcFileFlags     AS CHARACTER NO-UNDO.
DEFINE VARIABLE ocInfo          AS CHARACTER NO-UNDO.
DEFINE VARIABLE llUpdateAL        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llIsGettingLogged AS LOGICAL   NO-UNDO.
DEFINE VARIABLE llReqExist4Month  AS LOGICAL   NO-UNDO.

/* Address fields */
DEFINE VARIABLE lcFirstName        AS CHARACTER NO-UNDO.  
DEFINE VARIABLE lcSurname1         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcSurname2         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcPhoneNumber      AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStreet           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCity             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcZipCode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStreetNumber     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcRegion           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcEmail            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcGescal           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFloor            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStreetType       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBisDuplicate     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBlock            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcDoor             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLetter           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcStair            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcHand             AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcKm               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcTerritoryOwner   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCoverageToken    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcAddressID        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcToday            AS CHARACTER NO-UNDO.
DEFINE VARIABLE liMonth            AS INTEGER   NO-UNDO.
DEFINE VARIABLE liYear             AS INTEGER   NO-UNDO.
DEFINE VARIABLE lcPermanencyRule   AS CHARACTER NO-UNDO.


/* ***************************  Main Block  *************************** */

/* Getting directories from CParams */
ASSIGN
   lcIncomingDirectory  = fCParamC("InstallAddrChgIncomingDir")
   lcProcessedDirectory = fCParamC("InstallAddrChgProcessedDir")
   lcLogsDirectory      = fCParamC("InstallAddrChgLogsDir")  
   lcOutGoingDirectory  = fCParamC("InstallAddrChgOutGoingDir") NO-ERROR.
   
FILE-INFO:FILE-NAME = lcIncomingDirectory . IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
FILE-INFO:FILE-NAME = lcProcessedDirectory. IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
FILE-INFO:FILE-NAME = lcLogsDirectory     . IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.
FILE-INFO:FILE-NAME = lcOutGoingDirectory . IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN.

ASSIGN 
    llUpdateAL        = TRUE 
    llIsGettingLogged = TRUE
    lcToday         = STRING(YEAR(TODAY),"9999") + STRING(MONTH(TODAY),"99") + STRING(DAY(TODAY),"99")
    lcDelimiter     = "|"
    lcLogFile       = lcLogsDirectory     + "/addrchg_log_" + lcToday  + ".log" 
    
    lcTableName     = "install_address_change"
    lcActionID      = "install_address_change_processor" 
    ldCurrentTimeTS = Func.Common:mMakeTS(). 
    

OUTPUT STREAM sCurrentLog  TO VALUE(lcLogFile) APPEND.
PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";install_address_change_processing_started" SKIP.

DO TRANSACTION:
   FIND FIRST ActionLog WHERE
              ActionLog.Brand     EQ  Syst.Var:gcBrand AND
              ActionLog.ActionID  EQ  lcActionID       AND
              ActionLog.TableName EQ  lcTableName      NO-ERROR.

   IF AVAIL ActionLog AND
      ActionLog.ActionStatus EQ {&ACTIONLOG_STATUS_PROCESSING} THEN DO:
      llUpdateAL = FALSE.
      PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";install_another_address_change_processor_running" SKIP.
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
      PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";install_address_change_processor_first_run" SKIP.
   END.
   ELSE DO:
      ASSIGN
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_PROCESSING}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE Actionlog.
   END.
END.

DO ON ERROR UNDO , LEAVE:
    /* Collect files from incoming directory */
    INPUT STREAM sFilesInDir FROM OS-DIR(lcInComingDirectory).
    REPEAT:
        IMPORT STREAM sFilesInDir lcFile lcFileName lcFileFlags.
        IF NOT CAN-DO(lcFileFlags,"F") THEN NEXT.
        IF NOT (lcFile BEGINS "addr_") THEN NEXT.
        CREATE tt_file. ASSIGN tt_file.file_name = lcFileName tt_file.base_filename = lcFile. 
    END. /* repeat */
    INPUT STREAM sFilesInDir CLOSE.
    
    IF CAN-FIND(FIRST tt_file) THEN DO:
        /* Process file by file */
        FOR EACH tt_file:
            /* logfile for each input file */
            lcOutLogFile    = lcLogsDirectory + "/" +  SUBSTRING(tt_file.base_filename,1, R-INDEX(tt_file.base_filename,".") - 1 ) +  "_" + lcToday  +  ".log".
            OUTPUT STREAM sOutgoingLog TO VALUE(lcOutLogFile) APPEND.
            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";install_address_change started."  SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";install_address_change started."  SKIP.
            /* Read line by line  */
            INPUT STREAM sCurrentFile FROM VALUE(tt_file.file_name).
            REPEAT TRANSACTION:
                IMPORT STREAM sCurrentFile UNFORMATTED lcLine.
                IF TRIM (lcLine) = "" THEN NEXT.
                
                IF NUM-ENTRIES(lcLine,lcDelimiter) < 26 THEN DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";install_address_change_request_failed;" + "Incorrect number of entries"  SKIP.
                    PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";install_address_change_request_failed;" + "Incorrect number of entries"  SKIP.
                    NEXT.
                END.
                
                ASSIGN
                    liOrderID        = INTEGER(TRIM(ENTRY(1,lcLine,lcDelimiter))) 
                    lcNewCliType     = TRIM(ENTRY(2,lcLine,lcDelimiter))
                    llCreateSTC      = (lcNewCliType NE "" )
                    lcFirstName      =  ENTRY(3,lcLine,lcDelimiter)  
                    lcSurname1       =  ENTRY(4,lcLine,lcDelimiter)  
                    lcSurname2       =  ENTRY(5,lcLine,lcDelimiter)  
                    lcPhoneNumber    =  ENTRY(6,lcLine,lcDelimiter)  
                    lcStreet         =  ENTRY(7,lcLine,lcDelimiter)  
                    lcCity           =  ENTRY(8,lcLine,lcDelimiter)  
                    lcZipCode        =  ENTRY(9,lcLine,lcDelimiter)  
                    lcStreetNumber   =  ENTRY(10,lcLine,lcDelimiter)  
                    lcRegion         =  ENTRY(11,lcLine,lcDelimiter)  
                    lcEmail          =  ENTRY(12,lcLine,lcDelimiter)  
                    lcGescal         =  ENTRY(13,lcLine,lcDelimiter)  
                    lcFloor          =  ENTRY(14,lcLine,lcDelimiter)  
                    lcStreetType     =  ENTRY(15,lcLine,lcDelimiter)  
                    lcBisDuplicate   =  ENTRY(16,lcLine,lcDelimiter)  
                    lcBlock          =  ENTRY(17,lcLine,lcDelimiter)  
                    lcDoor           =  ENTRY(18,lcLine,lcDelimiter)  
                    lcLetter         =  ENTRY(19,lcLine,lcDelimiter)  
                    lcStair          =  ENTRY(20,lcLine,lcDelimiter)  
                    lcHand           =  ENTRY(21,lcLine,lcDelimiter)  
                    lcKm             =  ENTRY(22,lcLine,lcDelimiter)  
                    lcTerritoryOwner =  ENTRY(23,lcLine,lcDelimiter)  
                    lcCoverageToken  =  ENTRY(24,lcLine,lcDelimiter)  
                    lcAddressID      =  ENTRY(25,lcLine,lcDelimiter)
                    lcPermanencyRule =  ENTRY(26,lcLine,lcDelimiter) NO-ERROR.
                
                FIND Order WHERE Order.Brand = Syst.Var:gcBrand AND Order.OrderId = liOrderID NO-LOCK NO-ERROR.
                IF NOT AVAILABLE Order THEN DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" +  STRING(liOrderID) + ";install_address_change_request_failed;" + "Order not found."  SKIP.
                    PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) ";install_address_change_request_failed;" + "Order not found."  SKIP.
                    NEXT.
                END.
                FIND CliType WHERE CliType.Brand = Order.Brand AND CLIType.Clitype = Order.CliType NO-LOCK NO-ERROR.
                IF CLIType.FixedLineType NE {&CLITYPE_TARIFFTYPE_CONVERGENT} AND CLIType.FixedLineType NE {&CLITYPE_TARIFFTYPE_FIXEDONLY} THEN DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" +  STRING(liOrderID) + ";install_address_change_request_failed;" + "Order is not a Convergent Order."  SKIP.
                    PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) ";install_address_change_request_failed;" + "Order is not a Convergent Order."  SKIP.
                    NEXT.
                END.
                IF lcPermanencyRule NE "FTERM=3" AND lcPermanencyRule NE "" THEN DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" +  STRING(liOrderID) + ";install_address_change_request_failed;" + "Permanency rule not allowed."  SKIP.
                    PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) ";install_address_change_request_failed;" + "Permanency rule not allowed."  SKIP.
                    NEXT.
                END.
                IF NOT llCreateSTC THEN
                DO:
                    FIND FIRST OrderCustomer WHERE 
                               OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
                               OrderCustomer.OrderId EQ liOrderID        AND
                               OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
                               EXCLUSIVE-LOCK NO-ERROR.      
                    IF LOCKED OrderCustomer THEN 
                    DO:
                        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";install_address_change_request_failed;" + "OrderCustomer record is locked"  SKIP.
                        PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";install_address_change_request_failed;" + "OrderCustomer record is locked"  SKIP.
                        NEXT.
                    END.
                    ASSIGN 
                        llCreation      = FALSE
                        lhOrderCustomer = BUFFER OrderCustomer:HANDLE.
                    
                    IF NOT AVAILABLE  OrderCustomer THEN DO:
                        llCreation = TRUE .
                        CREATE OrderCustomer. 
                        RUN StarEventInitialize(lhOrderCustomer).
                    END.
                    ELSE 
                        RUN StarEventSetOldBuffer(lhOrderCustomer).
                        
                    ASSIGN
                        OrderCustomer.Brand          = Syst.Var:gcBrand 
                        OrderCustomer.Order          = liOrderID
                        OrderCustomer.RowType        = {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
                        OrderCustomer.FirstName      = lcFirstName
                        OrderCustomer.Surname1       = lcSurname1
                        OrderCustomer.Surname2       = lcSurname2
                        OrderCustomer.MobileNumber   = lcPhoneNumber
                        OrderCustomer.Street         = lcStreet
                        OrderCustomer.PostOffice     = lcCity
                        OrderCustomer.ZipCode        = lcZipCode
                        OrderCustomer.BuildingNum    = lcStreetNumber
                        OrderCustomer.Region         = lcRegion
                        OrderCustomer.Email          = lcEmail
                        OrderCustomer.Gescal         = lcGescal
                        OrderCustomer.Floor          = lcFloor
                        OrderCustomer.StreetType     = lcStreetType
                        OrderCustomer.BisDuplicate   = lcBisDuplicate
                        OrderCustomer.Block          = lcBlock
                        OrderCustomer.Door           = lcDoor
                        OrderCustomer.Letter         = lcLetter
                        OrderCustomer.Stair          = lcStair
                        OrderCustomer.Hand           = lcHand
                        OrderCustomer.Km             = lcKm
                        OrderCustomer.TerritoryOwner = lcTerritoryOwner
                        OrderCustomer.CoverageToken  = lcCoverageToken
                        OrderCustomer.AddressId      = lcAddressID
                        OrderCustomer.Address        = OrderCustomer.Street .
                    IF OrderCustomer.BuildingNum NE "" THEN 
                       OrderCustomer.Address = OrderCustomer.Address + " " +
                                                OrderCustomer.BuildingNum.         
                    IF llCreation THEN
                        RUN StarEventMakeCreateEvent(lhOrderCustomer ). 
                    ELSE 
                        RUN StarEventMakeModifyEvent(lhOrderCustomer).
                    RELEASE OrderCustomer.

                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";install_address_changed;" SKIP.
                    PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";install_address_changed;" SKIP.
                END.
                ELSE 
                DO:
                    ASSIGN 
                        liMonth          = 0
                        liYear           = 0
                        llReqExist4Month = FALSE
                        ldActivationTS   = Func.Common:mMake2DT(TODAY + 1, 0).
                        
                    FOR EACH MsRequest 
                       WHERE MsRequest.Msseq     = Order.Msseq
                         AND MsRequest.ReqType   = 0 
                         AND MsRequest.ReqStatus = 2 NO-LOCK BY MsRequest.CreStamp DESC:
                             /* Not in current month */
                        ASSIGN 
                            liMonth = MONTH(Func.Common:mTSToDate(MsRequest.CreStamp))
                            liYear  =  YEAR(Func.Common:mTSToDate(MsRequest.CreStamp)).
                             
                        IF liMonth NE MONTH(TODAY) OR (liMonth EQ MONTH(TODAY) AND liYear <> YEAR(TODAY)) THEN 
                            LEAVE.

                        llReqExist4Month = TRUE.

                        LEAVE.
                    END.

                    IF llReqExist4Month THEN 
                    DO:
                        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";stc_request_creation;" + "There exist STC already for this month, so request is created for next month." SKIP.
                        PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";stc_request_creation;" + "There exist STC already for this month, so request is created for next month." SKIP.
                        ASSIGN ldActivationTS = Func.Common:mMake2DT((Func.Common:mLastDayOfMonth(TODAY + 1) + 1), 0).
                    END.

                    /* same tech then NEXT  */
                    FIND FIRST MobSub WHERE MobSub.MsSeq = Order.MsSeq NO-LOCK NO-ERROR.
                    IF AVAILABLE MobSub THEN
                    DO:
                        FIND CliType1 WHERE CliType1.Brand = Order.Brand AND CliType1.CliType = MobSub.CliType NO-LOCK NO-ERROR.
                        FIND CliType2 WHERE CliType2.Brand = Order.Brand AND CliType2.CliType = lcNewCliType   NO-LOCK NO-ERROR.
                        IF NOT AVAILABLE CliType1 OR  NOT AVAILABLE CLiType2 THEN NEXT.
                        
                        IF NOT (LOOKUP(STRING(CliType1.TariffType),"{&CLITYPE_TARIFFTYPE_CONVERGENT},{&CLITYPE_TARIFFTYPE_FIXEDONLY}") > 0 AND 
                                LOOKUP(STRING(CliType2.TariffType),"{&CLITYPE_TARIFFTYPE_CONVERGENT},{&CLITYPE_TARIFFTYPE_FIXEDONLY}") > 0) THEN
                        DO:
                            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";stc_request_fail;" + "STC request between non-convergent nor non-fixedonly." SKIP.
                            PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";stc_request_fail;" + "STC request between non-convergent nor non-fixedonly." SKIP.
                            NEXT.
                        END.
                        ELSE IF CliType1.fixedlinetype EQ CliType2.fixedlinetype THEN
                        DO:
                            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";stc_request_fail;" + "STC request between same technology." SKIP.
                            PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";stc_request_fail;" + "STC request between same technology." SKIP.
                            NEXT.
                        END.
                    END.
                                                        
                    liRequest = fCTChangeRequest ( Order.msseq,
                                    lcNewCliType,
                                    '' , /* DataBundle */
                                    '' , /* Bank Acc */ 
                                    ldActivationTS,
                                    0  ,    /* Credit Check OK   */
                                    (IF lcPermanencyRule EQ "FTERM=3" 
                                       THEN INT(SUBSTRING(lcPermanencyRule,7,1)) ELSE 0),                                    
                                    "" ,    /* SalesMan          */
                                    FALSE , /* Create Fees       */
                                    FALSE , /* Send SMS          */
                                    "" ,    /* Creator           */
                                    0  ,    /* Charge            */
                                    {&REQUEST_SOURCE_YOIGO_TOOL}, 
                                    0  , 
                                    0  ,    /* Parent Request */
                                    '' ,    /* Conract ID */
                                    OUTPUT ocInfo).
                    IF ocInfo > "" THEN DO:
                        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";stc_request_creation_failed;" + ocInfo SKIP.
                        PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";stc_request_creation_failed;" + ocInfo  SKIP.
                    END.
                    ELSE DO:
                        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";stc_request_created;Ok;" + Order.CliType + "->" + lcNewCliType SKIP.
                        PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";stc_request_created;Ok;" + Order.CliType + "->" + lcNewCliType SKIP.
                    END.   
                END.
                CATCH err AS Progress.Lang.Error :
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) +  ";" + STRING(liOrderID)  ";stc_request_creation_failed;" + err:GetMessage(1) SKIP.
                    PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";stc_request_creation_failed;" + err:GetMessage(1) SKIP.
                END CATCH.
            END. /* repeat  */            
            INPUT STREAM sCurrentFile CLOSE.
            PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";install_address_change finished."  SKIP.
            PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(tt_file.file_name) + ";install_address_change finished."  SKIP.
            OUTPUT STREAM sOutgoingLog CLOSE.
            fMove2TransDir(lcOutLogFile      , "", lcOutGoingDirectory).
            fMove2TransDir(tt_file.file_name , "", lcProcessedDirectory).
        END. /* for each tt_file */
    END.
END. /* DO ON ERROR */

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
    
    IF llIsGettingLogged THEN DO:   
        PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";install_address_change_processing_finished" SKIP.
        PUT STREAM sCurrentLog UNFORMATTED "-------------------------------" SKIP.
        OUTPUT STREAM sCurrentLog CLOSE.
    END.
END FINALLY.
