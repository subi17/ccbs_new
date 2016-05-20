DEF TEMP-TABLE ttBillItem NO-UNDO LIKE BillItem.
DEF TEMP-TABLE ttIfispx NO-UNDO LIKE ifiSpx.
DEF TEMP-TABLE ttTMSCodes NO-UNDO LIKE TMSCodes.
DEF TEMP-TABLE ttTMSParam NO-UNDO LIKE TMSParam.
DEF TEMP-TABLE ttRepText NO-UNDO LIKE RepText.
DEF BUFFER bifiSpx FOR ifiSpx.

DEF VAR lcTrans_prep_L1 AS CHAR NO-UNDO INIT "Yoigo Tarjeta".
DEF VAR lcTrans_prep_L2 AS CHAR NO-UNDO INIT "Yoigo Tarjeta".
DEF VAR lcTrans_prep_L3 AS CHAR NO-UNDO INIT "Yoigo Tarjeta".
DEF VAR lcTrans_prep_L5 AS CHAR NO-UNDO INIT "Yoigo Pay as you go".
DEF VAR lcTrans_post_L1 AS CHAR NO-UNDO INIT "Yoigo Contrato".
DEF VAR lcTrans_post_L2 AS CHAR NO-UNDO INIT "Yoigo Contrato".
DEF VAR lcTrans_post_L3 AS CHAR NO-UNDO INIT "Yoigo Contrato".
DEF VAR lcTrans_post_L5 AS CHAR NO-UNDO INIT "Yoigo Pay monthly".
DEF VAR llError AS LOGICAL NO-UNDO.
DEF VAR ldaVAlidFrom AS DATE NO-UNDO INIT 05/20/2016.
DEF VAR liUpdateMode AS INT NO-UNDO INIT 1.

FUNCTION fcreateRepText RETURNS LOGICAL ( INPUT icBaseDCEvent AS CHAR,
                                           INPUT icDCEvent AS CHAR,
                                           INPUT icTransText AS CHAR,
                                           INPUT idaValidFrom AS DATE,
                                           INPUT iiLanguage AS INT,
                                           INPUT iiUpdateMode AS INT):
   FIND FIRST RepText WHERE
              RepText.LinkCode EQ icBaseDCEvent AND
              RepText.Language EQ iiLanguage AND
              RepText.ToDate > TODAY NO-ERROR.
   IF NOT AVAIL RepText THEN DO:
      MESSAGE "RepText not found / " + STRING(iiLanguage) VIEW-AS ALERT-BOX.
      RETURN FALSE.
   END.

   CREATE ttRepText.
   BUFFER-COPY RepText TO ttRepText.
   /*Set correct values to new entry*/
   ttRepText.LinkCode = icDCEvent.
   ttRepText.RepText = icTransText.
   ttRepText.FromDate = idaValidFrom.

   DISPLAY ttRepText with frame a.
   pause 0.
   IF iiUpdateMode NE 0 THEN DO:
      CREATE RepText.
      BUFFER-COPY ttRepText TO RepText.
      DELETE ttRepText. /*ror safety reasons*/
   END.
   IF AVAIL RepText THEN RELEASE RepText.
   RETURN TRUE.
END FUNCTION.



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
      llError = fcreateRepText("TS00000M1", "TS00000U1", 
                               lcTrans_prep_L1, ldaVAlidFrom, 1, liUpdateMode).
      llError = fcreateRepText("TS00000M1", "TS00000U1", 
                               lcTrans_prep_L2, ldaVAlidFrom, 2, liUpdateMode).
      llError = fcreateRepText("TS00000M1", "TS00000U1", 
                               lcTrans_prep_L3, ldaVAlidFrom, 3, liUpdateMode).
      llError = fcreateRepText("TS00000M1", "TS00000U1", 
                               lcTrans_prep_L5, ldaVAlidFrom, 5, liUpdateMode).
 

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
      llError = fcreateRepText("TS00000M3", "TS00000U3",
                               lcTrans_post_L1, ldaVAlidFrom, 1, liUpdateMode).
      llError = fcreateRepText("TS00000M3", "TS00000U3",
                               lcTrans_post_L2, ldaVAlidFrom, 2, liUpdateMode).
      llError = fcreateRepText("TS00000M3", "TS00000U3",
                               lcTrans_post_L3, ldaVAlidFrom, 3, liUpdateMode).
      llError = fcreateRepText("TS00000M3", "TS00000U3",
                               lcTrans_post_L5, ldaVAlidFrom, 5, liUpdateMode).
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


