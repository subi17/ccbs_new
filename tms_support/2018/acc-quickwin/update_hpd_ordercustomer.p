
/*------------------------------------------------------------------------
    File        : update_hpd_ordercustomer.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Thu May 31 10:57:50 EEST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

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
           DumpFile.DumpName = "HPD_OrderCustomer". 

fUpdateDFField(DumpFile.DumpID, "OrderCustomer", "OperAllMarketing", "OperAllMarketing", 44, TODAY).
fUpdateDFField(DumpFile.DumpID, "OrderCustomer", "FixedNumber", "FixedNumber", 45, TODAY).
fUpdateDFField(DumpFile.DumpID, "OrderCustomer", "BankCode", "BankCode", 46, TODAY).
fUpdateDFField(DumpFile.DumpID, "OrderCustomer", "Country", "Country", 47, TODAY).
fUpdateDFField(DumpFile.DumpID, "OrderCustomer", "#Language", "Language", 48, TODAY).
