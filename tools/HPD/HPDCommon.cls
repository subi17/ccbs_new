/*------------------------------------------------------------------------
    File        : HPDCommon
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Tue Oct 28 14:42:43 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.HPDCommon FINAL:      
      
   METHOD PUBLIC STATIC INTEGER mGetDumpID
      (icDumpName AS CHARACTER):

      DEFINE VARIABLE lii           AS INTEGER NO-UNDO.
      DEFINE VARIABLE liReturnValue AS INTEGER NO-UNDO.

      DEFINE BUFFER DumpFile FOR DumpFile.
      
      FOR EACH DumpFile NO-LOCK WHERE
         DumpFile.Brand        = "1"         AND
         DumpFile.DumpName     = icDumpName  AND
         DumpFile.FileCategory = "HPD":
         
         lii = lii + 1.
         liReturnValue = DumpFile.DumpID.
      END.

      IF lii = 0
      THEN RETURN ERROR NEW Progress.Lang.AppError("DumpFile record named " + icDumpName + " doesn't exist.", 1).      
      IF lii > 1
      THEN RETURN ERROR NEW Progress.Lang.AppError("Multiple dumpfile records named " + icDumpName + " exist.", 1).

      RETURN liReturnValue.      
         
   END METHOD.


   METHOD PUBLIC STATIC DECIMAL mVATAmt
      (ideAmt     AS DECIMAL,
       ideVATPerc AS DECIMAL):

      DEFINE VARIABLE ldeVATAmt AS DECIMAL NO-UNDO.

      ldeVATAmt = ROUND(((ideAmt * ideVATPerc) / 100),2).

      IF ldeVATAmt = ?
      THEN RETURN 0.

      RETURN ldeVATAmt.

   END METHOD.


   METHOD PUBLIC STATIC CHARACTER mGetDumpTable
      (iiDumpID AS INTEGER):

      DEFINE BUFFER DumpFile FOR DumpFile.
      
      FIND FIRST DumpFile NO-LOCK WHERE
         DumpFile.DumpID       = iiDumpID
      NO-ERROR.
         
      IF AVAILABLE DumpFile
      THEN RETURN DumpFile.MainTable.
      
      RETURN ERROR NEW Progress.Lang.AppError("Dump ID " + STRING(iiDumpID) + " doesn't exist.", 1).         
         
   END METHOD. 

   /* Note! Blob and clob are not supported */
   METHOD PUBLIC STATIC CHARACTER mDynExport
      ( INPUT ihBufHandle  AS HANDLE,
        INPUT icDelim   AS CHARACTER):

      DEFINE VARIABLE lhFld     AS HANDLE    NO-UNDO.
      DEFINE VARIABLE liCnt     AS INTEGER   NO-UNDO.
      DEFINE VARIABLE liExtnt   AS INTEGER   NO-UNDO.
      DEFINE VARIABLE lcTmp     AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcArray   AS CHARACTER NO-UNDO.
      DEFINE VARIABLE lcResult  AS CHARACTER NO-UNDO.

      IF ihBufHandle:TYPE <> "BUFFER"
      THEN RETURN ?.

      DO liCnt = 1 TO ihBufHandle:NUM-FIELDS:

         ASSIGN lhFld = ihBufHandle:BUFFER-FIELD(liCnt).

         IF lhFld:DATA-TYPE = "clob" OR lhFld:DATA-TYPE = "blob"
         THEN NEXT.

         IF lhFld:EXTENT = 0
         THEN DO:
            IF lhFld:BUFFER-VALUE = ?
            THEN lcTmp = "?".
            ELSE
            CASE lhFld:DATA-TYPE:
               WHEN "character"
               THEN lcTmp = QUOTER(lhFld:BUFFER-VALUE).
               WHEN "raw"
               THEN lcTmp = '"' + STRING(lhFld:BUFFER-VALUE) + '"'.
               WHEN "datetime" OR WHEN "datetime-tz"
               THEN lcTmp = STRING(YEAR(lhFld:BUFFER-VALUE),"9999") + "-" +
                           STRING(MONTH(lhFld:BUFFER-VALUE),"99")  + "-" +
                           STRING(DAY(lhFld:BUFFER-VALUE),"99")    + "T" +
                           SUBSTRING(STRING(lhFld:BUFFER-VALUE),12).
               OTHERWISE lcTmp = STRING(lhFld:BUFFER-VALUE).
            END CASE.

            lcResult = lcResult + lcTmp + icDelim.
         END.
         ELSE DO:
            lcArray = "".
            DO liExtnt = 1 TO lhFld:EXTENT:
               IF lhFld:BUFFER-VALUE(liExtnt) = ?
               THEN lcTmp = "?".
               ELSE
               CASE lhFld:DATA-TYPE:
                  WHEN "character"
                  THEN lcTmp = QUOTER(lhFld:BUFFER-VALUE(liExtnt)).
                  WHEN "raw"
                  THEN lcTmp = '"' + STRING(lhFld:BUFFER-VALUE(liExtnt)) + '"'.
                  WHEN "datetime" OR WHEN "datetime-tz"
                  THEN lcTmp = STRING(YEAR(lhFld:BUFFER-VALUE(liExtnt)),"9999") + "-" +
                              STRING(MONTH(lhFld:BUFFER-VALUE(liExtnt)),"99")  + "-" +
                              STRING(DAY(lhFld:BUFFER-VALUE(liExtnt)),"99")    + "T" +
                              SUBSTRING(STRING(lhFld:BUFFER-VALUE(liExtnt)),12).
                  OTHERWISE lcTmp = STRING(lhFld:BUFFER-VALUE(liExtnt)).
               END CASE.
              lcArray = lcArray + lcTmp + icDelim.
            END.
            lcResult = lcResult + RIGHT-TRIM(lcArray,icDelim) + icDelim.
         END.
      END.

      RETURN RIGHT-TRIM(lcResult,icDelim).

   END METHOD.
      
END CLASS.