DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttIfispx NO-UNDO LIKE ifiSpx.
DEF TEMP-TABLE ttTMSCodes NO-UNDO LIKE TMSCodes.
DEF TEMP-TABLE ttTMSParam NO-UNDO LIKE TMSParam.
DEF BUFFER bifiSpx FOR ifiSpx.

FIND FIRST BillItem WHERE
           BillItem.brand EQ "1" AND
           BillItem.billcode EQ "TS00000U1" NO-ERROR.
IF NOT AVAIL BillItem THEN DO:
   FIND FIRST BillItem WHERE
              BillItem.brand EQ "1" AND
              BillItem.billcode EQ "TS00000M1" NO-ERROR.
   IF AVAIL BillItem THEN DO:
      BUFFER-COPY BillItem TO ttBillItem.

      ttBillItem.billcode = "TS00000U1".
      ttBillItem.biName = "Prepaid Universal SIM".

      CREATE BillItem.
      BUFFER-COPY ttBillItem TO BillItem.
      DELETE ttBillItem.
   END.
END.

FIND FIRST BillItem WHERE
           BillItem.brand EQ "1" AND
           BillItem.billcode EQ "TS00000U3" NO-ERROR.
IF NOT AVAIL BillItem THEN DO:
   FIND FIRST BillItem WHERE
              BillItem.brand EQ "1" AND
              BillItem.billcode EQ "TS00000M3" NO-ERROR.
   IF AVAIL BillItem THEN DO:
      BUFFER-COPY BillItem TO ttBillItem.

      ttBillItem.billcode = "TS00000U3".
      ttBillItem.biName = "Postpaid Universal SIM".

      CREATE BillItem.
      BUFFER-COPY ttBillItem TO BillItem.
      DELETE ttBillItem.
   END.
END.

FIND FIRST SIMArt WHERE
           SimArt.brand EQ "1" AND
           SimArt.SAName EQ "Universal" NO-ERROR.
IF NOT AVAIL SimArt THEN DO:
   CREATE SimArt.
   ASSIGN
      SimArt.Brand = "1"
      SimArt.SAName = "Universal"
      SimArt.SimArt = "Universal".
END.      

FOR EACH SimArt:
   DISP SimArt.
END.


FIND FIRST ifispx WHERE
           ifispx.brand EQ "1" AND
           ifispx.version EQ "00004" NO-ERROR.
IF NOT AVAIL ifispx THEN DO:
   FOR EACH ifispx WHERE
            ifispx.brand EQ "1" AND
            ifispx.version EQ "00003":
      BUFFER-COPY ifiSpx TO ttifiSpx.
      ttifiSpx.version = "00004".
      CREATE bifiSpx.
      BUFFER-COPY ttifiSpx TO bifiSpx.
      DELETE ttifiSpx.
   END.
END.

FOR EACH ifiSpx:
   DISP ifiSpx.
END.

FIND FIRST TMSCodes WHERE
           TMSCodes.tablename EQ "IFiSpx" AND
           TMSCodes.FieldName EQ "SIMArt" AND
           TMSCodes.codevalue EQ "43" NO-LOCK NO-ERROR.
IF NOT AVAIL TMSCodes THEN DO:           
   FIND FIRST TMSCodes WHERE 
              TMSCodes.tablename EQ "IFiSpx" AND
              TMSCodes.FieldName EQ "SIMArt" NO-ERROR.
   BUFFER-COPY TMSCodes TO ttTMSCodes.
   ASSIGN
      ttTMSCodes.codename = "Universal SIM"
      ttTMSCodes.codevalue = "18"
      ttTMSCodes.configValue = "Universal".
   CREATE TMSCodes.
   BUFFER-COPY ttTMSCodes TO TMSCodes.

   ttTMSCodes.codevalue = "23".
   CREATE TMSCodes.
   BUFFER-COPY ttTMSCodes TO TMSCodes.

   ttTMSCodes.codevalue = "33".
   CREATE TMSCodes.
   BUFFER-COPY ttTMSCodes TO TMSCodes.

   ttTMSCodes.codevalue = "43".
   CREATE TMSCodes.
   BUFFER-COPY ttTMSCodes TO TMSCodes.

   DELETE ttTMSCodes.

END.

FIND FIRST TMSParam WHERE 
           TMSParam.brand EQ "1" AND
           TMSParam.paramgroup EQ "SIM" AND
           TMSParam.paramcode EQ "ICC_G&D_00004" NO-LOCK NO-ERROR.
IF NOT AVAIL TMSParam THEN DO:
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.paramgroup EQ "SIM" AND
              TMSParam.paramcode EQ "ICC_G&D_00003" NO-LOCK NO-ERROR.
   BUFFER-COPY TMSParam TO ttTMSParam.
   
   ttTMSParam.paramCode = "ICC_G&D_00004".
   CREATE TMSParam.
   BUFFER-COPY ttTMSParam TO TMSParam.

   ttTMSParam.paramCode = "ICC_Gemalto_00004".
   CREATE TMSParam.
   BUFFER-COPY ttTMSParam TO TMSParam.

   ttTMSParam.paramCode = "ICC_MORPHO_00004".
   CREATE TMSParam.
   BUFFER-COPY ttTMSParam TO TMSParam.
  
   ttTMSParam.paramCode = "ICC_OBERHTUR_00004".
   CREATE TMSParam.
   BUFFER-COPY ttTMSParam TO TMSParam.
 
   DELETE ttTMSParam.
END.

