/* With this script you can make DWH dump changes by running each 
   configuration one by one with iiRunID.
   You can make test run with simulation mode liSimulate.
   Results can be check from CUI F7 - F1 - F1 */

/* Case 1 = Modify OrderFusion dump 
   Case 2 = Modify OrderCustomer dump 
   Case 3 = Modify MsOwner dump  
   Case 4 = Modify MsOwner Track dump  
   Case 5 = Modify mobsub dump
*/

DEF VAR iiRunID     AS INT NO-UNDO INIT 5.      /* <--- Change the correct case number */
DEF VAR liSimulate  AS LOG NO-UNDO INIT TRUE.  /* <--- SIMULATE */
/***************************************************/

DEF VAR lcDumpName   AS CHAR NO-UNDO.
DEF VAR lcDFTable    AS CHAR NO-UNDO.
DEF VAR ldaFromDate  AS DATE NO-UNDO.
DEF VAR ldaToDate    AS DATE NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO.

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
         DISPLAY "Dump configuration not available for Name " icDumpName.
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
            DISPLAY "Field " ENTRY(ientry, icField) " does not exist in " icDFTable.
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
   OTHERWISE DISPLAY "Incorrect CASE ID " iiRunID.
END CASE.
