/*------------------------------------------------------------------------
    File        : DumpFile
    Purpose     : File handling as subclass for DumpBase
    Syntax      : 
    Description : High performance data file dump
    Author(s)   : tlamminmaki
    Created     : Tue Oct 28 14:35:00 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.HPDTime.
USING HPD.DumpBase.

{HPD/HPDEnvironment.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpFile INHERITS DumpBase: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   
   DEFINE PRIVATE VARIABLE lcFile                  AS CHARACTER     NO-UNDO.
   DEFINE PRIVATE VARIABLE llAllowEmptyFile        AS LOGICAL       NO-UNDO.
   DEFINE PRIVATE VARIABLE llAllowMove             AS LOGICAL INITIAL TRUE NO-UNDO.
   DEFINE PRIVATE VARIABLE lcTenant                AS CHARACTER     NO-UNDO.

   DEFINE PROTECTED BUFFER lbDumpHPD FOR DumpHPD.

   DEFINE PUBLIC PROPERTY FullDumpLimit AS INTEGER NO-UNDO
      GET:
         IF lbDumpHPD.MaxRecordsPerFile <= 0 OR lbDumpHPD.MaxRecordsPerFile = ?
         THEN RETURN 0.
         
         RETURN lbDumpHPD.MaxRecordsPerFile.

      END GET.
      
   DEFINE PUBLIC VARIABLE objHPDTime            AS CLASS HPDTime NO-UNDO.

   DEFINE PRIVATE STREAM str.

   
   CONSTRUCTOR PUBLIC DumpFile
      ( iiDumpID AS INTEGER,
        iobjHandler AS CLASS DumpHandler ):      
      THIS-OBJECT(iiDumpID, "", iobjHandler).
   END CONSTRUCTOR.   

	
   CONSTRUCTOR PUBLIC DumpFile
      ( iiDumpID    AS INTEGER,
        icFile      AS CHARACTER,
        iobjHandler AS CLASS DumpHandler ):
           
      SUPER(iiDumpID, iobjHandler).           
      
      FIND lbDumpHPD NO-LOCK WHERE
         lbDumpHPD.DumpID = lbDumpFile.DumpID
      NO-ERROR.

      IF NOT AVAILABLE lbDumpHPD
      THEN RETURN ERROR NEW Progress.Lang.AppError("DumpHPD record for DumpID " + STRING(lbDumpFile.DumpID) + " doesn't exist.", 1).

      IF lbDumpHPD.Active = NO
      THEN RETURN ERROR NEW Progress.Lang.AppError("DumpHPD record for DumpID " + STRING(lbDumpFile.DumpID) + " is not active.", 1).

      FIND CURRENT lbDumpHPD EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

      IF LOCKED lbDumpHPD
      THEN RETURN ERROR NEW Progress.Lang.AppError("DumpHPD record for DumpID " + STRING(lbDumpFile.DumpID) + " is locked.", 1).

      lcTenant = mGetTenant(lbDumpFile.MainTable).

      ASSIGN
         objHPDTime = NEW HPDTime()
         objHPDTime:CurrLocalTZTime = NOW
         llAllowEmptyFile = lbDumpFile.EmptyFile
         .

      mUpdateDumpHeader(1,"",lbDumpFile.MainTable).
      mUpdateDumpHeader(2,"","CREATE").

      /* Lets populate the header elements that
         are needed for offline dumps */
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
         mUpdateDumpHeader(3,"",objHPDTime:CurrUTCTime).
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
         mUpdateDumpHeader(3,"",ISO-DATE(objHPDTime:CurrLocalTZTime)).
      &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unsupported HPD environment", 1).
      &ENDIF

      IF lcTenant > ""
      THEN mUpdateDumpHeader(4,"",lcTenant).

      IF icFile <> ""
      THEN ASSIGN
              llAllowMove = FALSE
              lcFile      = icFile
              .
      ELSE lcFile = lbDumpFile.SpoolDir + "/" + IF lcTenant > "" THEN lcTenant ELSE "" +
                    REPLACE(lbDumpFile.FileName,"#DATE_#TIME",objHPDTime:FileTime).
              
      IF lbDumpFile.DumpCharSet > ""
      THEN OUTPUT STREAM str TO VALUE(lcFile) CONVERT TARGET lbDumpFile.DumpCharSet.
      ELSE OUTPUT STREAM str TO VALUE(lcFile).
                  
   END CONSTRUCTOR.
   
   
   DESTRUCTOR PUBLIC DumpFile( ):

      IF VALID-OBJECT(objHPDTime)
      THEN DELETE OBJECT objHPDTime.      
      
      IF AVAILABLE lbDumpHPD
      THEN DO:
         mInactivateDump(NO).

         FIND CURRENT lbDumpHPD NO-LOCK.
      END.
      
      DEFINE VARIABLE llEmptyFile AS LOGICAL NO-UNDO.
               
      llEmptyFile = ( SEEK(str) = 0 ).
      OUTPUT STREAM str CLOSE.

      /* If we had a problem or empty file generated
         and it is not allowed then we delete the file */      
      IF (llEmptyFile AND NOT llAllowEmptyFile) OR
         objHandler:llAllOK = FALSE
      THEN OS-DELETE VALUE(lcFile).
      
      /* Otherwise we move the file to transdir.
         If llAllowMode is false the move will be handled
         using dumpfile internal file moving logic.
         We check that lbDumpFile exists (although it should always in
         this situation) */
      ELSE IF llAllowMove AND AVAILABLE lbDumpFile
      THEN OS-COMMAND SILENT VALUE("cp -f " + lcFile + " " + lbDumpFile.TransDir + "/" +
                                   ENTRY(NUM-ENTRIES(lcFile,"/"),lcFile,"/") + " && " +
                                   "touch -r " + lcFile + " " + lbDumpFile.TransDir + "/" +
                                   ENTRY(NUM-ENTRIES(lcFile,"/"),lcFile,"/") + " && " +
                                   "rm -f " + lcFile).

   END DESTRUCTOR.


   METHOD PRIVATE CHARACTER mGetTenant
      ( icTableName AS CHARACTER ):

      DEFINE VARIABLE lhBuffer      AS HANDLE    NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.

      CREATE BUFFER lhBuffer FOR TABLE icTableName.

      lcReturnValue = TENANT-NAME(lhBuffer:DBNAME).

      &IF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
         IF lcReturnValue = "Default"
         THEN lcReturnValue = "yoigo".
         ELSE lcReturnValue = SUBSTRING(lcReturnValue,2).
      &ENDIF

      RETURN lcReturnValue.

      FINALLY:
         IF VALID-HANDLE(lhBuffer)
         THEN DELETE OBJECT lhBuffer.
      END FINALLY.

   END METHOD.


   METHOD PUBLIC VOID mInactivateDump
      ( ilForce AS LOGICAL ):

      IF AVAILABLE lbDumpHPD AND lbDumpHPD.Active = YES
      THEN DO:
         IF ilForce
         THEN ASSIGN
                 lbDumpHPD.Continuous = NO
                 lbDumpHPD.Active     = NO
                 .
         ELSE IF lbDumpFile.Continuous = NO
         THEN lbDumpHPD.Active = NO.
      END.

   END METHOD.


   METHOD PUBLIC VOID mWriteALine(): 

      PUT STREAM str UNFORMATTED mGetData() SKIP.

   END METHOD.


   METHOD PUBLIC VOID mWriteALine
      (icLine AS CHARACTER):

      PUT STREAM str UNFORMATTED icLine SKIP.

   END METHOD.


   METHOD PUBLIC VOID mWriteALineWithPrefix
      (icPrefix AS CHARACTER):

      PUT STREAM str UNFORMATTED icPrefix + mGetData() SKIP.

   END METHOD.

END CLASS.
