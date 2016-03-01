/*------------------------------------------------------------------------
    File        : DumpTimeBased
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Wed Oct 29 10:44:20 EET 2014
    Notes       : Adds time range handling
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpFile.
USING HPD.DumpHandler.
USING HPD.HPDTime.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpTimeBased INHERITS DumpFile:
   
   DEFINE PUBLIC VARIABLE ldeFromTS AS DECIMAL  NO-UNDO.
   DEFINE PUBLIC VARIABLE ldeToTS   AS DECIMAL  NO-UNDO.
   
   
   CONSTRUCTOR PUBLIC DumpTimeBased
      ( iiDumpID AS INTEGER,
        iobjHandler AS CLASS DumpHandler ):
      THIS-OBJECT(iiDumpID, "", iobjHandler).   
   END CONSTRUCTOR.

   
   CONSTRUCTOR PUBLIC DumpTimeBased
      ( iiDumpID AS INTEGER,
        icFile   AS CHARACTER,
        iobjHandler AS CLASS DumpHandler ):
      
      SUPER(iiDumpID, icFile, iobjHandler).
      
      ldeFromTS = HPDTime:mDateHMS2TS(lbDumpFile.ModCollModule).

      /* If no previous runs has been done the last stamp
         is the first moment of current date. */
      IF ldeFromTS = ?
      THEN ldeFromTS = HPDTime:mMake2DT(DATE(objHPDTime:CurrLocalTZTime),0).
      
      /* Lets assume first that we can process to the current time. */ 
      ldeToTS = HPDTime:mMake2DT(DATE(objHPDTime:CurrLocalTZTime),
                                 INTEGER(TRUNCATE(MTIME(objHPDTime:CurrLocalTZTime) / 1000, 0))).

      mAdjustToTS().

   END CONSTRUCTOR.
      
   
   DESTRUCTOR PUBLIC DumpTimeBased( ):
      
      IF AVAILABLE lbDumpFile
      THEN DO:
         IF objHandler:llAllOK
         THEN lbDumpFile.ModCollModule = HPDTime:mTS2DateHMS(ldeToTS).
      END.

   END DESTRUCTOR.
   
   /*
      If there is a value in lbDumpFile.FullCollModule we
      must do the processing only for the count of the days
      specified in the field.
   */
   METHOD PRIVATE VOID mAdjustToTS():
      
      DEFINE VARIABLE liDays  AS INTEGER NO-UNDO.
      DEFINE VARIABLE ldaDate AS DATE    NO-UNDO.
      
      liDays = INTEGER(lbDumpFile.FullCollModule) NO-ERROR.
      
      IF ERROR-STATUS:ERROR
      THEN ASSIGN
              ERROR-STATUS:ERROR = FALSE
              liDays             = 0
              .
      
      IF liDays >= 1 AND liDays <= 10000
      THEN DO:
         
         ldaDate = HPDTime:mMake2Date(ldeFromTS) + liDays.
         
         IF ldaDate < DATE(objHPDTime:CurrLocalTZTime)
         THEN ldeToTS = HPDTime:mMake2DT(ldaDate,0). 
         
      END.
   
   END METHOD.
     

END CLASS.
