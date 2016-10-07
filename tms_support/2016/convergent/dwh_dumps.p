/* With this script you can make DWH dump changes by running each 
   configuration one by one with iiRunID.
   You can make test run with simulation mode liSimulate.
   Results can be check from CUI F7 - F1   */

/* Case 1 = Modify OrderFusion dump 
   Case 2 = Modify OrderCustomer dump 
*/

DEF VAR iiRunID     AS INT NO-UNDO INIT 2.  /* Change the correct case number */
DEF VAR liSimulate  AS LOG NO-UNDO INIT FALSE.

DEF VAR liDumpID     AS INT  NO-UNDO.
DEF VAR lcDFTable    AS CHAR NO-UNDO.
DEF VAR ldaFromDate  AS DATE NO-UNDO.
DEF VAR ldaToDate    AS DATE NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO.

/* Check if the field name can be found from table */
FUNCTION fFieldExists RETURNS LOGICAL (
   INPUT ipcTableName  AS CHARACTER, 
   INPUT ipccFieldName AS CHARACTER):

   DEFINE VARIABLE hBufferHandle AS HANDLE NO-UNDO.
   DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
 
    CREATE BUFFER hBufferHandle FOR TABLE ipcTableName NO-ERROR.
    lResult = VALID-HANDLE(hBufferHandle) AND 
              VALID-HANDLE(hBufferHandle:BUFFER-FIELD(ipccFieldName)) NO-ERROR.
    IF  VALID-HANDLE(hBufferHandle) THEN DELETE OBJECT hBufferHandle.
    RETURN lResult.
END FUNCTION.

/* Add new fields to dump fields */
FUNCTION fAddFiels RETURNS LOGICAL (
   INPUT iiDumpID     AS INT,
   INPUT icDFTable    AS CHAR,
   INPUT idaFromDate   AS DATE,
   INPUT idaToDate     AS DATE,
   INPUT icField      AS CHAR):

   DEF VAR iiOrderNbr   AS INT  NO-UNDO.
   DEF VAR ientry       AS INT  NO-UNDO.

      FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DumpFile THEN DO:
         DISPLAY "Dump configuration not available for ID " iiDumpID.
         RETURN FALSE.
      END.

      FOR EACH DFField NO-LOCK WHERE
               DFField.DumpID = DumpFile.DumpID:
         IF iiOrderNbr <  DFField.OrderNbr THEN iiOrderNbr = DFField.OrderNbr.

         IF liSimulate THEN DISPLAY DFField. 
      END.

      REPEAT ientry = 1 TO NUM-ENTRIES(icField):
      IF CAN-FIND(FIRST DFField WHERE  /* Already same field */
                        DFField.DFField EQ ENTRY(ientry, icField)) THEN NEXT.

      iiOrderNbr = iiOrderNbr + 1.

      IF NOT fFieldExists (icDFTable,ENTRY(ientry, icField)) THEN DO:
         DISPLAY "Field " ENTRY(ientry, icField) " does not exist in " icDFTable.
         NEXT.
      END.
         IF liSimulate THEN DISPLAY iiOrderNbr ENTRY(ientry, icField).
         ELSE DO:
            CREATE DFField.
            ASSIGN
               DFField.DFField  = ENTRY(ientry, icField)
               DFField.DFLabel  = ENTRY(ientry, icField)
               DFField.DFTable  = icDFTable
               DFField.DumpID   = iiDumpID
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
      liDumpID     = 90.
      lcDFTable    = "OrderFusion".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "FixedCurrOperCode,SerialNumber,AppointmentDate,CancellationReason,FixedInstallationTS,ADSLLinkState,FixedStatusTS,FixedStatusDesc". /* Put list here */

      fAddFiels( liDumpID,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   WHEN 2 THEN DO: /* Modify OrderCustomer dump */
      liDumpID     = 13.
      lcDFTable    = "OrderCustomer".
      ldaFromDate  = TODAY.
      ldaToDate    = 12/31/49.
      lcField      = "Gescal,StreetType,BisDuplicate,Block,Door,Letter,Stair,Hand,Km,Floor". /* Put list here */

      fAddFiels( liDumpID,lcDFTable,ldaFromDate,ldaToDate,lcField).
   END.
   OTHERWISE DISPLAY "Incorrect CASE ID " iiRunID.
END CASE.
