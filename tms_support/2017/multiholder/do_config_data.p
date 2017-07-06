FUNCTION fUpdateMenuText RETURNS LOGICAL
   (iiMenuNum  AS INTEGER,
    icMenuText AS CHARACTER):
       
   FIND MenuText EXCLUSIVE-LOCK WHERE
      MenuText.MenuNum = iiMenuNum
   NO-ERROR.
   
   IF NOT AVAILABLE MenuText
   THEN DO:
      CREATE MenuText.
      ASSIGN
         MenuText.MenuNum = iiMenuNum.
   END.
   
   ASSIGN
      MenuText.MenuText = icMenuText.

END FUNCTION.

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

FUNCTION fUpdateTMSParam RETURNS LOGICAL
   (icParamCode AS CHARACTER,
    icParamName AS CHARACTER,
    icCharVal   AS CHARACTER):
       
   FIND TMSParam EXCLUSIVE-LOCK WHERE
      TMSParam.ParamGroup = "DMS" AND
      TMSParam.ParamCode  = icParamCode
   NO-ERROR.
   
   IF NOT AVAILABLE TMSParam
   THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.Brand      = "1"
         TMSParam.ParamGroup = "DMS"
         TMSParam.ParamCode  = icParamCode.
   END.
   
   ASSIGN
      TMSParam.CharVal   = icCharVal
      TMSParam.ParamName = icParamName
      TMSParam.ParamType = "C".

END FUNCTION.

fUpdateTMSParam("DMS_S20_T1.1", "DMS Matrix Stat 20 / Row 1.1", "1,2,3,5,9,11,13").
fUpdateTMSParam("DMS_S21_T1.1", "DMS Matrix Stat 21 / Row 1.1", "3,7,9,11,13").
fUpdateTMSParam("DMS_S33_T1.1", "DMS Matrix Stat 33 / Row 1.1", "3,7,9,11,13").
fUpdateTMSParam("DMS_S44_T1.1", "DMS Matrix Stat 44 / Row 1.1", "4,5,9,11,13").
fUpdateMenuText(2247, "USER    CUSTOMER").
fUpdateMenuText(2248, "MOBILE  DONOR").
fUpdateMenuText(2249, "FIXED   DONOR").
fUpdateMenuText(2250, "CUSTOMERMANAGEM.").
fUpdateDFField(225, "OrderCustomer", "AuthCustId", "AuthCustID", 34, TODAY).
fUpdateDFField(225, "OrderCustomer", "AuthCustIdType", "AuthCustIDType", 35, TODAY).
