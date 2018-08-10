/*------------------------------------------------------------------------
    File        : customeraccount_dump_create.p
    Purpose     : 

    Syntax      :

    Description : It will create the Dump Records for the CustomerAccount      

    Author(s)   : 
    Created     : 08-08-2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

ROUTINE-LEVEL ON ERROR UNDO, THROW.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

FUNCTION fUpdateDFField RETURNS LOGICAL
   (iiDumpID    AS INTEGER,
    icTable     AS CHARACTER,
    icField     AS CHARACTER,
    icLabel     AS CHARACTER,
    iiOrderNbr  AS INTEGER,
    idaFromDate AS DATE):

   FIND FIRST DFField EXCLUSIVE-LOCK WHERE
      DFField.DumpID   = iiDumpID AND
      DFField.OrderNbr = iiOrderNbr
   NO-ERROR.

   IF NOT AVAILABLE DFField
   THEN DO:
      CREATE DFField.
      ASSIGN
         DFField.DumpID   = iiDumpID
         DFField.OrderNbr = iiOrderNbr.
   END.

   ASSIGN
      DFField.DFField  = icField
      DFField.DFLabel  = icLabel
      DFField.DFTable  = icTable
      DFField.FromDate = idaFromDate
      DFFIeld.ToDate   = DATE(12,31,2049).

   RETURN FALSE.

END FUNCTION.

FIND FIRST DumpFile NO-LOCK WHERE
           DumpFile.Brand    = "1" AND
           DumpFile.DumpName = "HPD_Order".

fUpdateDFField(DumpFile.DumpID, "Order", "AccountID", "AccountID", 46, TODAY).
