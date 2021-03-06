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
USING HPD.DumpSomeWhere.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{HPD/HPDEnvironment.i}

CLASS HPD.DumpFile INHERITS DumpSomeWhere:
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   
   DEFINE PRIVATE VARIABLE lcFile                  AS CHARACTER     NO-UNDO.
   DEFINE PRIVATE VARIABLE llAllowEmptyFile        AS LOGICAL       NO-UNDO.
   DEFINE PRIVATE VARIABLE llAllowMove             AS LOGICAL INITIAL TRUE NO-UNDO.

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

      /* Header time is same for every file dump line */
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
         mUpdateDumpHeader(5,"",objHPDTime:CurrUTCTime).
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
         mUpdateDumpHeader(5,"",ISO-DATE(objHPDTime:CurrLocalTZTime)).
      &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unsupported HPD environment", 1).
      &ENDIF

      llAllowEmptyFile = lbDumpFile.EmptyFile.

      DEFINE VARIABLE lcBrandName AS CHARACTER NO-UNDO.

      lcBrandName = multitenancy.TenantInformation:mGetEffectiveBrand().

      IF icFile <> ""
      THEN ASSIGN
              llAllowMove = FALSE
              lcFile      = icFile
              .
      ELSE ASSIGN
              lcFile = REPLACE(lbDumpFile.FileName,"#DATE_#TIME",objHPDTime:FileTime)
              lcFile = REPLACE(lcFile,"#TENANT",lcBrandName)
              lcFile = lbDumpFile.SpoolDir + "/" + lcFile.
              
      IF lbDumpFile.DumpCharSet > ""
      THEN OUTPUT STREAM str TO VALUE(lcFile) CONVERT TARGET lbDumpFile.DumpCharSet.
      ELSE OUTPUT STREAM str TO VALUE(lcFile).
                  
   END CONSTRUCTOR.
   
   
   DESTRUCTOR PUBLIC DumpFile( ):

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


   METHOD PUBLIC OVERRIDE VOID mWriteALine():

      PUT STREAM str UNFORMATTED mGetData() SKIP.

   END METHOD.


   METHOD PUBLIC OVERRIDE VOID mWriteALine
      (icLine AS CHARACTER):

      PUT STREAM str UNFORMATTED icLine SKIP.

   END METHOD.


   METHOD PUBLIC OVERRIDE VOID mWriteALineWithPrefix
      (icPrefix AS CHARACTER):

      PUT STREAM str UNFORMATTED icPrefix + mGetData() SKIP.

   END METHOD.

END CLASS.
