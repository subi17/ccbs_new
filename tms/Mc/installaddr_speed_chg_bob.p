/*------------------------------------------------------------------------
    File        : Mc/installaddr_speed_chg_bob.p
    Purpose     : Bob Tool to change Intallation address and/or speed change 
    Author(s)   : Ashok
    Created     : Thu Jan 18 10:03:09 IST 2018
    Notes       : Input file Line should have entries > 26
                  if second and third entries are "" then only address change
                  otherwise STC change too. 
  ----------------------------------------------------------------------*/

{Syst/commpaa.i}
Syst.Var:gcBrand = "1".
{Syst/tmsconst.i}
{Func/fbankdata.i}
{Func/fctchange.i}
{Func/fmakemsreq.i}
{Func/ftransdir.i}   

/* ***************************  Definitions  ************************** */

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
    lcDelimiter     = "|"
    lcLogFile       = lcLogsDirectory + "/addrchg_log.log" 
    lcOutLogFile    = lcLogsDirectory + "/addrchg_out.txt"
    lcTableName     = "install_address_change"
    lcActionID      = "install_address_change_processor" 
    ldCurrentTimeTS = Func.Common:mMakeTS(). 
    

OUTPUT STREAM sCurrentLog  TO VALUE(lcLogFile) APPEND.
PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";address_change_processing_started" SKIP.

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
         ActionLog.ActionStatus = {&ACTIONLOG_STATUS_SUCCESS}
         ActionLog.UserCode     = Syst.Var:katun
         ActionLog.ActionTS     = ldCurrentTimeTS.
      RELEASE ActionLog.
      llUpdateAL = FALSE.
      PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") + ";install_address_change_processor_first_run" SKIP.
      RETURN.
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
    /* Processing files in incoming directory */
    INPUT STREAM sFilesInDir FROM OS-DIR(lcInComingDirectory).
    OUTPUT STREAM sOutgoingLog TO VALUE(lcOutLogFile) APPEND.
    REPEAT:
        IMPORT STREAM sFilesInDir lcFile lcFileName lcFileFlags.
        IF NOT CAN-DO(lcFileFlags,"F") THEN NEXT.
        IF NOT (lcFile BEGINS "addr_") THEN NEXT.
        INPUT STREAM sCurrentFile FROM VALUE(lcFileName).
        REPEAT:
            IMPORT STREAM sCurrentFile UNFORMATTED lcLine.
            
            IF NUM-ENTRIES(lcLine,lcDelimiter) < 25 THEN DO:
                PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) + ";install_address_change_request_failed;" + "Incorrect number of entries"  SKIP.
                PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) + ";install_address_change_request_failed;" + "Incorrect number of entries"  SKIP.
                NEXT.
            END.
                
            ASSIGN
                liOrderID        = INTEGER(ENTRY(1,lcLine,lcDelimiter)) 
                lcNewCliType     = ENTRY(2,lcLine,lcDelimiter)
                llCreateSTC        = (lcNewCliType NE "" )
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
                lcAddressID      =  ENTRY(25,lcLine,lcDelimiter) NO-ERROR.
            FIND Order WHERE Order.OrderId = liOrderID NO-LOCK NO-ERROR.
            IF NOT AVAILABLE Order THEN DO:
                PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) +  ";" +  STRING(liOrderID) + ";install_address_change_request_failed;" + "Order not found"  SKIP.
                PUT STREAM sCurrentLog  UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) ";install_address_change_request_failed;" + "Order not found"  SKIP.
                NEXT.
            END.
            
            FIND FIRST OrderCustomer WHERE 
                       OrderCustomer.Brand   EQ Syst.Var:gcBrand AND
                       OrderCustomer.OrderId EQ liOrderID        AND
                       OrderCustomer.RowType EQ {&ORDERCUSTOMER_ROWTYPE_FIXED_INSTALL}
                       EXCLUSIVE-LOCK NO-ERROR.      
            IF LOCKED OrderCustomer THEN 
            DO:
                PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) +  ";" + STRING(liOrderID)  ";install_address_change_request_failed;" + "OrderCustomer record is locked"  SKIP.
                PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";install_address_change_request_failed;" + "OrderCustomer record is locked"  SKIP.
                NEXT.
            END.
            IF NOT AVAILABLE  OrderCustomer THEN
                CREATE OrderCustomer. 
    
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
            RELEASE OrderCustomer.

            IF llCreateSTC  THEN DO:
                ldActivationTS = Func.Common:mMake2DT( TODAY + 1 , 0 ).
                llReqExist4Month = FALSE.
                FOR EACH MsRequest 
                   WHERE MsRequest.Msseq   = Order.Msseq
                     AND Msrequest.reqtype = 0 NO-LOCK BY MsRequest.CreStamp DESC:
                         /* Not in current month */
                    IF ( MONTH(Func.Common:mTSToDate(MsRequest.CreStamp)) NE MONTH(TODAY) ) AND 
                       ( YEAR(Func.Common:mTSToDate(MsRequest.CreStamp))  EQ YEAR(TODAY)  ) THEN LEAVE.
                    FIND CliType1 WHERE CliType1.CliType = MsRequest.ReqCParam1 NO-LOCK NO-ERROR.
                    FIND CliType2 WHERE CliType2.CliType = MsRequest.ReqCParam2 NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE CliType1 OR  NOT AVAILABLE CLiType2 THEN NEXT.
                    /* same tech then NEXT  */
                    IF CliType1.fixedlinetype EQ CliType2.fixedlinetype NEXT .
                    llReqExist4Month = TRUE .
                    LEAVE.
                END.
                IF llReqExist4Month THEN DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) +  ";" + STRING(liOrderID)  ";install_address_change_stc_request_fail;" + "There exist STC already for this month." SKIP.
                    PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";install_address_change_stc_request_fail;" + "There exist STC already for this month." SKIP.
                    NEXT.
                END.                                    
                liRequest = fCTChangeRequest ( Order.msseq,
                                lcNewCliType,
                                '' , /* DataBundle */
                                '' , /* Bank Acc */ 
                                ldActivationTS,
                                0  ,    /* Credit Check OK   */ 
                                0  ,    /* Request Flag = 0  */
                                "" ,    /* SalesMan          */
                                FALSE , /* Create Fees       */
                                FALSE , /* Send SMS          */
                                "" ,    /* Creator           */
                                0  ,    /* Charge            */
                                {&REQUEST_SOURCE_YOIGO_TOOL}, 
                                liOrderID , 
                                0  ,    /* Parent Request */
                                '' ,    /* Conract ID */
                                OUTPUT ocInfo).
                IF ocInfo > "" THEN DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) +  ";" + STRING(liOrderID)  ";install_address_change_stc_request_fail;" + ocInfo SKIP.
                    PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";install_address_change_stc_request_fail;" + ocInfo  SKIP.
                END.
                ELSE DO:
                    PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) +  ";" + STRING(liOrderID)  ";install_address_change_stc_request_done;Ok" SKIP.
                    PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(liOrderID) + ";install_address_change_stc_request_done;Ok"  SKIP.
                END.   
            END.
        END.
        PUT STREAM sOutgoingLog UNFORMATTED STRING(TIME,"hh:mm:ss") +  ";" + STRING(lcFileName) +  ";install_address_change_request_done;"  SKIP.
        PUT STREAM sCurrentLog UNFORMATTED STRING(TIME,"hh:mm:ss")  + ";install_address_change_request_done;"  SKIP.
    END.
    FINALLY:
        INPUT STREAM sCurrentFile CLOSE. 
        OUTPUT STREAM sOutgoingLog CLOSE.
        fMove2TransDir(lcFileName  , "", lcProcessedDirectory).
        fMove2TransDir(lcOutLogFile, "", lcOutGoingDirectory ).
    END FINALLY.
END.

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
