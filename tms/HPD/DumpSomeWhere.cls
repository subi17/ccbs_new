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

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpSomeWhere ABSTRACT INHERITS DumpBase: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
   
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

      ASSIGN
         objHPDTime = NEW HPDTime()
         objHPDTime:CurrLocalTZTime = NOW
         .

      mUpdateDumpHeader(3,"",lbDumpFile.MainTable).
      mUpdateDumpHeader(4,"","CREATE").

      /*
         If there is not yet database connection for the table
         we need to set the brand name to header later on when the
         connection is available
      */
      mSetBrandValueToHeader().

   END CONSTRUCTOR.
   
   
   DESTRUCTOR PUBLIC DumpSomeWhere( ):

      IF VALID-OBJECT(objHPDTime)
      THEN DELETE OBJECT objHPDTime.

   END DESTRUCTOR.


   METHOD PUBLIC ABSTRACT VOID mWriteALine(). 

   METHOD PUBLIC ABSTRACT VOID mWriteALine(icLine AS CHARACTER).

   METHOD PUBLIC ABSTRACT VOID mWriteALineWithPrefix(icPrefix AS CHARACTER).


END CLASS.
