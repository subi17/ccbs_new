/* onlinereader_oldcdr.i    05.01.11/aam 
   save late arrivals to old dbs when new db is already in use 
*/

FUNCTION fSaveCDR2OldDb RETURNS LOGIC
  (INPUT icTableName AS CHAR,
   INPUT ihCDR       AS HANDLE):

   DEF VAR lhCDRTable AS HANDLE NO-UNDO.
   DEF VAR lcOldDB    AS CHAR   NO-UNDO.
   
   CASE icTableName:
   WHEN "MobCDR"   THEN lcOldDb = "OldMCDR".
   WHEN "PrepCDR"  THEN lcOldDb = "OldPrepCDR".
   WHEN "ErrorCDR" OR
   WHEN "RoamCDR"  OR 
   WHEN "RoamGPRS" THEN lcOldDb = "OldRoamCDR".
   WHEN "PrepEDR" THEN lcOldDb = "OldPrepEDR".
   OTHERWISE lcOldDb = "".
   END CASE.

   IF lcOldDb = "" THEN RETURN FALSE.
   /* if old db is connected then save to that, i.e. the initializer determines
      when event date older than today means saving to old db, not this 
      function */
   IF NOT CONNECTED(lcOldDb) THEN RETURN FALSE.

   icTableName = lcOldDb + "." + icTableName.
   
   CREATE BUFFER lhCDRTable FOR TABLE icTableName.

   lhCDRTable:BUFFER-CREATE.
   lhCDRTable:BUFFER-COPY(ihCDR) NO-ERROR.

   IF ERROR-STATUS:ERROR THEN DO:
      lhCDRTable:BUFFER-DELETE.
      RETURN FALSE.
   END.
   
   lhCDRTable:BUFFER-RELEASE.

   DELETE OBJECT lhCDRTable NO-ERROR.

   RETURN TRUE.
   
END FUNCTION.

FUNCTION fSaveDetail2OldDb RETURNS LOGIC
  (INPUT icTableName AS CHAR,
   INPUT idaDatest   AS DATE,
   INPUT iiDtlSeq    AS INT,
   INPUT icVersion   AS CHAR,
   INPUT icDetails   AS CHAR):

   DEF VAR lhDetailTable AS HANDLE NO-UNDO.
   DEF VAR lcOldDB       AS CHAR   NO-UNDO.
   
   CASE icTableName:
   WHEN "McdrDtl2" THEN lcOldDb = "OldMcdrDtl".
   WHEN "ErrorDtl" THEN lcOldDb = "OldRoamCDR".
   WHEN "EDRDtl" THEN lcOldDb = "OldPrepEDR".
   OTHERWISE lcOldDb = "".
   END CASE.

   IF lcOldDb = "" THEN RETURN FALSE.
   IF NOT CONNECTED(lcOldDb) THEN RETURN FALSE.

   icTableName = lcOldDb + "." + icTableName.
   
   CREATE BUFFER lhDetailTable FOR TABLE icTableName.

   lhDetailTable:BUFFER-CREATE.
   ASSIGN 
      lhDetailTable::DateSt  = idaDateSt 
      lhDetailTable::DtlSeq  = iiDtlSeq
      lhDetailTable::Version = icVersion
      lhDetailTable::Detail  = icDetails NO-ERROR.

   IF ERROR-STATUS:ERROR THEN DO:
      lhDetailTable:BUFFER-DELETE.
      RETURN FALSE.
   END.
   
   lhDetailTable:BUFFER-RELEASE.

   DELETE OBJECT lhDetailTable NO-ERROR.

   RETURN TRUE.
   
END FUNCTION.


