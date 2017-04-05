/*------------------------------------------------------------------------
    File        : DumpSomeWhere
    Purpose     : Abstract subclass for DumpBase for dumping somewhere
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

CLASS HPD.DumpSomeWhere ABSTRACT INHERITS DumpBase: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   
   DEFINE PROTECTED VARIABLE lcTenant                AS CHARACTER     NO-UNDO.

   DEFINE PUBLIC PROPERTY FullDumpLimit AS INTEGER NO-UNDO
      GET:
         DEFINE VARIABLE liNumber AS INTEGER NO-UNDO.

         liNumber = INTEGER(lbDumpFile.UseIndex) NO-ERROR.

         IF ERROR-STATUS:ERROR
         THEN ASSIGN
                 ERROR-STATUS:ERROR = FALSE
                 liNumber           = 0
                 .

         IF liNumber <= 0 OR liNumber = ?
         THEN RETURN 0.

         RETURN liNumber.
      END GET.

   DEFINE PUBLIC VARIABLE objHPDTime            AS CLASS HPDTime NO-UNDO.

   CONSTRUCTOR PUBLIC DumpSomeWhere
      ( iiDumpID    AS INTEGER,
        iobjHandler AS CLASS DumpHandler ):
           
      SUPER(iiDumpID, iobjHandler).

      lcTenant = mGetTenant(lbDumpFile.MainTable).

      ASSIGN
         objHPDTime = NEW HPDTime()
         objHPDTime:CurrLocalTZTime = NOW
         .

      mUpdateDumpHeader(3,"",lbDumpFile.MainTable).
      mUpdateDumpHeader(4,"","CREATE").

      /* Lets populate the header elements that
         are needed for offline dumps */
      &IF "{&HPD_ENVIRONMENT}" = "Salt"
      &THEN
         mUpdateDumpHeader(5,"",objHPDTime:CurrUTCTime).
      &ELSEIF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
         mUpdateDumpHeader(5,"",ISO-DATE(objHPDTime:CurrLocalTZTime)).
      &ELSE
         RETURN ERROR NEW Progress.Lang.AppError("Unsupported HPD environment", 1).
      &ENDIF

      IF lcTenant > ""
      THEN mUpdateDumpHeader(6,"",lcTenant).
                  
   END CONSTRUCTOR.
   
   
   DESTRUCTOR PUBLIC DumpSomeWhere( ):

      IF VALID-OBJECT(objHPDTime)
      THEN DELETE OBJECT objHPDTime.

   END DESTRUCTOR.


   METHOD PRIVATE CHARACTER mGetTenant
      ( icTableName AS CHARACTER ):

      DEFINE VARIABLE lhBuffer      AS HANDLE    NO-UNDO.
      DEFINE VARIABLE lcReturnValue AS CHARACTER NO-UNDO.
      DEFINE VARIABLE llFound       AS LOGICAL   NO-UNDO.

      CREATE BUFFER lhBuffer FOR TABLE icTableName.

      llFound = lhBuffer:FIND-FIRST("", NO-LOCK) NO-ERROR.

      IF NOT llFound
      THEN RETURN "".

      lcReturnValue = lhBuffer:BUFFER-TENANT-NAME.

      &IF "{&HPD_ENVIRONMENT}" = "Yoigo"
      &THEN
         IF lcReturnValue = "Default"
         THEN lcReturnValue = "yoigo".
         ELSE IF lcReturnValue > ""
         THEN lcReturnValue = SUBSTRING(lcReturnValue,2).
      &ENDIF

      RETURN lcReturnValue.

      FINALLY:
         IF VALID-HANDLE(lhBuffer)
         THEN DELETE OBJECT lhBuffer.
      END FINALLY.

   END METHOD.


   METHOD PUBLIC ABSTRACT VOID mWriteALine(). 

   METHOD PUBLIC ABSTRACT VOID mWriteALine(icLine AS CHARACTER).

   METHOD PUBLIC ABSTRACT VOID mWriteALineWithPrefix(icPrefix AS CHARACTER).


END CLASS.
