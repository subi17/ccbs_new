/* With this script you can make DWH dump changes by running each 
   configuration one by one with iiRunID.
   You can make test run with simulation mode liSimulate.
   Results can be check from CUI F7 - F1 - F1 */

/* Case 1 = Modify OrderFusion dump 
   Case 2 = Modify OrderCustomer dump 
   Case 3 = Modify MsOwner dump  
   Case 4 = Modify MsOwner Track dump  
   Case 5 = Modify mobsub dump
   Case 6 = Fusion Message table Configuration NEW DUMP 
   Case 7 = Fusion Message dump 
   Case 8 = Billing Item dump
*/

DEF VAR iiRunID     AS INT NO-UNDO INIT 8.      /* <--- Change the correct case number */
DEF VAR liSimulate  AS LOG NO-UNDO INIT TRUE.  /* <--- SIMULATE */
/***************************************************/

DEF VAR lcDumpName   AS CHAR NO-UNDO.
DEF VAR lcDFTable    AS CHAR NO-UNDO.
DEF VAR ldaFromDate  AS DATE NO-UNDO.
DEF VAR ldaToDate    AS DATE NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO.
DEF VAR lcFileName   AS CHAR NO-UNDO.
DEF VAR lcSpoolDir   AS CHAR NO-UNDO.
DEF VAR lcTransDir   AS CHAR NO-UNDO.
DEF VAR llEmptyFile      AS LOG NO-UNDO.
DEF VAR llCheckEventLog  AS LOG NO-UNDO.
DEF VAR lcEventLogFields AS CHAR NO-UNDO.
DEF VAR lcDescription    AS CHAR NO-UNDO.
DEF VAR lcLogicModule    AS CHAR NO-UNDO.
DEF VAR lcModCollModule  AS CHAR NO-UNDO.

/* Check if the field name can be found from table */
FUNCTION fFieldExists RETURNS CHARACTER (
   INPUT ipcTableName  AS CHARACTER,
   INPUT ipccFieldName AS CHARACTER):

   DEFINE VARIABLE hBufferHandle AS HANDLE NO-UNDO.
   DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.

   CREATE BUFFER hBufferHandle FOR TABLE ipcTableName NO-ERROR.
   lResult = VALID-HANDLE(hBufferHandle) AND
              VALID-HANDLE(hBufferHandle:BUFFER-FIELD(ipccFieldName)) NO-ERROR.
      IF lResult THEN RETURN (hBufferHandle:BUFFER-FIELD(ipccFieldName):LABEL).
   IF  VALID-HANDLE(hBufferHandle) THEN DELETE OBJECT hBufferHandle.
   RETURN "".
END FUNCTION.


/* Add new fields to dump fields */
FUNCTION fAddNewDumpRecord RETURNS LOGICAL (
   INPUT ilModify     AS LOG,       /* Create Or Modify */
   INPUT icDumpName   AS CHAR,      /* Dump Name */
   INPUT icMainTable  AS CHAR,      /* Main Table */
   INPUT icFileName   AS CHAR,      /* File Name  */
   INPUT icSpoolDir   AS CHAR,      /* Spool Directory */
   INPUT icTransDir   AS CHAR,      /* Transfer Directory */
   INPUT ilEmptyFile  AS LOG,       /* Create Empty File */
   INPUT ilCheckEventLog  AS LOG,   /* Check EventLog */
   INPUT icEventLogFields AS CHAR,  /* Check Field */
   INPUT icDescription    AS CHAR,  /* Description */
   INPUT icLogicModule    AS CHAR,  /* Logic Module */
   INPUT icModCollModule  AS CHAR): /* Collect Modified */

   DEF VAR iiDumpID     AS INT  NO-UNDO.

   FIND FIRST DumpFile WHERE DumpFile.DumpName = icDumpName EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE DumpFile AND NOT ilModify THEN DO:
      IF NOT ilModify THEN DO:
         MESSAGE "Dump configuration exist for Name " icDumpName VIEW-AS ALERT-BOX.
         RETURN FALSE.
      END.
      ELSE DO:
         iiDumpID = DumpFile.DumpID.
      END.
   END.
   IF NOT AVAILABLE DumpFile AND ilModify THEN DO:
      MESSAGE "Modify Dump not found for Name " icDumpName VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   IF NOT ilModify THEN DO:
      FIND LAST DumpFile USE-INDEX DumpID NO-LOCK NO-ERROR.
      IF AVAILABLE DumpFile
      THEN iiDumpID = DumpFile.DumpID + 1.
      ELSE iiDumpID = 1.
      IF NOT liSimulate THEN CREATE DumpFile.
   END.

   IF liSimulate THEN DO:
      DISPLAY iiDumpID icDumpName icMainTable icLogicModule icModCollModule with 1 col.
      DISPLAY skip(1).
   END.

   ELSE DO:
      ASSIGN 
      /* Dump data   */
         DumpFile.Brand             = "1"
         DumpFile.DumpID            = iiDumpID     /* Dump ID   */
         DumpFile.DumpName          = icDumpName   /* Dump Name */
         DumpFile.Active            = No  /* Use first NO and then change to Yes when deployed */
         DumpFile.FileCategory      = "DWH"        /* File Category */
         DumpFile.MainTable         = icMainTable  /* Main Table */
         DumpFile.FileName          = icFileName   /* File Name  */
         DumpFile.SpoolDir          = icSpoolDir   /* Spool Directory */
         DumpFile.TransDir          = icTransDir   /* Transfer Directory */
         DumpFile.DumpFormat        = "ASCII"      /* Dump Format */
         DumpFile.EmptyFile         = ilEmptyFile  /* Create Empty File */
         DumpFile.ModFromEventLog   = ilCheckEventLog    /* Check EventLog */
         DumpFile.EventLogFields    = icEventLogFields   /* Check Field */
         DumpFile.DumpDelimiter     = "|"                /* Delimiter */
         DumpFile.DecimalPoint      = "."                /* Decimal Point */
         DumpFile.AllowReplica      = No                 /* AllowReplica */
         DumpFile.Description       = icDescription      /* Description */

      /* ADDIT. SETTINGS */
         DumpFile.LogicModule       = icLogicModule      /* Logic Module */
         DumpFile.ModCollModule     = icModCollModule.   /* Collect Modified */

   /* These values go with initial values can be added if needed
         DumpFile.AveDurFull     
         DumpFile.AveDurMod      
         DumpFile.BatchID        
         DumpFile.ConfigParam    
         DumpFile.DumpCharSet    
         DumpFile.DumpLineFeed   
         DumpFile.FullCollModule 
         DumpFile.LinkKey        
         DumpFile.LogFile        
         DumpFile.ModFromField   
         DumpFile.QueryClause    
         DumpFile.SideTables     
         DumpFile.UseIndex */

         DISPLAY DumpFile.
         RELEASE DumpFile.
      END.
   RETURN TRUE.
END FUNCTION.




/* Add new fields to dump fields */
FUNCTION fAddFiels RETURNS LOGICAL (
   INPUT icDumpName   AS CHAR,
   INPUT icDFTable    AS CHAR,
   INPUT idaFromDate  AS DATE,
   INPUT idaToDate    AS DATE,
   INPUT icField      AS CHAR):

   DEF VAR iiOrderNbr   AS INT  NO-UNDO.
   DEF VAR ientry       AS INT  NO-UNDO.
   DEF VAR icLabel      AS CHAR NO-UNDO.

      FIND FIRST DumpFile WHERE DumpFile.DumpName = icDumpName NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DumpFile THEN DO:
         MESSAGE "Dump configuration not available for Name: " icDumpName VIEW-AS ALERT-BOX.
         RETURN FALSE.
      END.

      FOR EACH DFField NO-LOCK WHERE
               DFField.DumpID EQ DumpFile.DumpID:
         IF iiOrderNbr <  DFField.OrderNbr THEN iiOrderNbr = DFField.OrderNbr.

         IF liSimulate THEN DISPLAY DFField. 
      END.

      REPEAT ientry = 1 TO NUM-ENTRIES(icField):
         IF CAN-FIND(FIRST DFField WHERE  /* Already same field */
                           DFField.DumpID  EQ DumpFile.DumpID AND
                           DFField.DFField EQ ENTRY(ientry, icField)) THEN NEXT.

         iiOrderNbr = iiOrderNbr + 1.

         icLabel = fFieldExists(icDFTable,ENTRY(ientry, icField)).
         IF icLabel EQ "" THEN DO:
            MESSAGE "Field " ENTRY(ientry, icField) " does not exist in " icDFTable VIEW-AS ALERT-BOX.
            NEXT.
         END.
         IF liSimulate THEN DISPLAY iiOrderNbr ENTRY(ientry, icField) icLabel.
         ELSE DO:
            CREATE DFField.
            ASSIGN
               DFField.DFField  = ENTRY(ientry, icField)
               DFField.DFLabel  = icLabel
               DFField.DFTable  = icDFTable
               DFField.DumpID   = DumpFile.DumpID
               DFField.FromDate = idaFromDate
               DFField.OrderNbr = iiOrderNbr
               DFField.ToDate   = idaToDate.

            DISPLAY DFField.
            RELEASE DFField.
         END.
      END.

    RETURN TRUE.
END FUNCTION.


CASE iiRunID:
   WHEN 1 THEN DO: /* Modify OrderFusion dump */
      lcDumpName   = "OrderFusion".
      lcDFTable    = "OrderFusion".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "FixedCurrOperCode,SerialNumber,AppointmentDate,CancellationReason,FixedInstallationTS,ADSLLinkState,FixedStatusTS,FixedStatusDesc". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 2 THEN DO: /* Modify OrderCustomer dump */
      lcDumpName   = "OrderCustDumpTXT".
      lcDFTable    = "OrderCustomer".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "Gescal,StreetType,BisDuplicate,Block,Door,Letter,Stair,Hand,Km,Floor". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 3 THEN DO: /* Modify MSOwner dump */
      lcDumpName   = "MSOwner".
      lcDFTable    = "MSOwner".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "FixedNumber". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 4 THEN DO: /* Modify MSOwner Track dump */
      lcDumpName   = "MsOwnerTrack".
      lcDFTable    = "MSOwner".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "FixedNumber". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 5 THEN DO: /* Modify mobsub dump */
      lcDumpName   = "SubscriptionDumpTXT".
      lcDFTable    = "MobSub".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "FixedNumber". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 6 THEN DO: /* Fusion Message table NEW DUMP */
      lcDumpName        = "FusionMessage".
      lcDFTable         = "FusionMessage".
      lcFileName        = "#CAT#RUN_FUSIONMESSAGE_#MODE_#DATE.txt".
      lcSpoolDir        = "/store/riftp/dumpfiles/dwh/spool".
      lcTransDir        = "/store/riftp/dumpfiles/dwh/outgoing".
      llEmptyFile       = No.
      llCheckEventLog   = No.
      lcEventLogFields  = "".
      lcDescription     = "Fusion Message table dump".
      lcLogicModule     = "fusion_message_dump".
      lcModCollModule   = "".

      fAddNewDumpRecord( FALSE,lcDumpName,lcDFTable,lcFileName,lcSpoolDir,
                         lcTransDir,llEmptyFile,llCheckEventLog,
                         lcEventLogFields,lcDescription,lcLogicModule,
                         lcModCollModule).

   END.
   WHEN 7 THEN DO: /* Fusion Message dump */
      lcDumpName   = "FusionMessage".
      lcDFTable    = "FusionMessage".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "MessageSeq,OrderId,MSSeq,MessageType,Source,CreatedTS,UpdateTS,MessageStatus,OrderType,FixedStatus,FixedStatusTS,FixedStatusDesc,AdditionalInfo,ResponseCode". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 8 THEN DO: /* Modify mobsub dump */
      lcDumpName   = "BillingItems".
      lcDFTable    = "BItemGroup".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "GroupType". /* Put list here */

      fAddFiels( lcDumpName,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   OTHERWISE DISPLAY "Incorrect CASE ID " iiRunID.
END CASE.
