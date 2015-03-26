&IF "{&detailvalue}" NE "YES"
&THEN

&GLOBAL-DEFINE detailvalue YES

DEFINE TEMP-TABLE ttHeader NO-UNDO
   FIELD tcVersion AS CHARACTER
   FIELD tcTitle   AS CHARACTER FORMAT "X(24)"
   FIELD tcFormat  AS CHARACTER
   FIELD tiNum     AS INTEGER
   
   INDEX Version AS PRIMARY
      tcVersion
      tcTitle.
   
FUNCTION fFillTempTable RETURNS LOGICAL() :

   DEFINE VARIABLE liLoop   AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTemp   AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcTitle  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcFormat AS CHARACTER NO-UNDO.
   
   FOR EACH CSVHeader NO-LOCK:

      DO liLoop = 1 TO NUM-ENTRIES(CSVHeader.CSV,"|"):
      
         ASSIGN
            lcTemp   = ""
            lcTitle  = ""
            lcFormat = "".
            
         lcTemp   = ENTRY(liLoop,CSVHeader.CSV,"|").
         lcTitle  = ENTRY(2,(ENTRY(1,lcTemp,">")),"=") NO-ERROR.
         lcFormat = ENTRY(2,(ENTRY(2,lcTemp,">")),"=") NO-ERROR.
      
         CREATE ttHeader.
         ASSIGN
            ttHeader.tcVersion = CSVHeader.Version
            ttHeader.tiNum     = liLoop
            ttHeader.tcTitle   = lcTitle
            ttHeader.tcFormat  = lcFormat.

      END.
      
   END.

END FUNCTION.

FUNCTION fGetValue RETURNS CHARACTER
  (INPUT pcVersion AS CHARACTER,
   INPUT pcTitle   AS CHARACTER,
   INPUT pdaDate   AS DATE,
   INPUT piDtlSeq  AS INTEGER):

   DEFINE VARIABLE lcReturn AS CHARACTER NO-UNDO.

   FIND FIRST ttHeader WHERE
              ttHeader.tcVersion = pcVersion AND
              ttHeader.tcTitle   = pcTitle
   NO-LOCK NO-ERROR.

   IF AVAIL ttHeader THEN DO:
      
      FIND FIRST MCDRDtl2 WHERE
                 MCDRDtl2.DateSt = pdaDate AND
                 MCDRDtl2.DtlSeq = piDtlSeq
      NO-LOCK NO-ERROR.

      IF AVAIL MCDRDtl2 THEN
         lcReturn = ENTRY(ttHeader.tiNum,MCDRDTl2.Detail,"|").

   END.
   
   RETURN lcReturn.

END FUNCTION.

FUNCTION fGetPosition RETURNS INTEGER
  (INPUT pcVersion AS CHARACTER,
   INPUT pcTitle   AS CHARACTER):

   DEFINE VARIABLE liReturn AS INTEGER NO-UNDO.

   FIND FIRST ttHeader WHERE
              ttHeader.tcVersion = pcVersion AND
              ttHeader.tcTitle   = pcTitle
   NO-LOCK NO-ERROR.

   IF AVAIL ttHeader THEN liReturn = ttHeader.tiNum.

   RETURN liReturn.

END FUNCTION.

fFillTempTable().

&ENDIF
