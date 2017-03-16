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
   
   DEFINE PUBLIC  VARIABLE ldeFromTS       AS DECIMAL  NO-UNDO.
   DEFINE PUBLIC  VARIABLE ldeToTS         AS DECIMAL  NO-UNDO.
   DEFINE PRIVATE VARIABLE ldeNewStartTS   AS DECIMAL  NO-UNDO.

   DEFINE PRIVATE BUFFER lbDumpHPD FOR DumpHPD.

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
      
      ldeFromTS = HPDTime:mDateHMS2TS(lbDumpHPD.StartTime).

      /* If no previous runs has been done the last stamp
         is the first moment of current date. */
      IF ldeFromTS = ?
      THEN ldeFromTS = HPDTime:mMake2DT(DATE(objHPDTime:CurrLocalTZTime),0).
      
      /* Lets assume first that we can process to the final time
         or if not given to the current time. */
      IF lbDumpHPD.FinalTime = ""
      THEN ldeToTS = HPDTime:mMake2DT(DATE(objHPDTime:CurrLocalTZTime),
                                      INTEGER(TRUNCATE(MTIME(objHPDTime:CurrLocalTZTime) / 1000, 0))).
      ELSE ldeToTS = HPDTime:mDateHMS2TS(lbDumpHPD.FinalTime).

      IF ldeToTS = ldeFromTS
      THEN DO:
         mInactivateDump(TRUE).
         RETURN ERROR NEW Progress.Lang.AppError("Start time and final time are equal. The dump is now inactivated.", 1).
      END.

      mAdjustToTS().

      LOG-MANAGER:WRITE-MESSAGE("Dumping from " + HPDTime:mTS2DateHMS(ldeFromTS) + " to " +
                                HPDTime:mTS2DateHMS(ldeFromTS) + " next start time will be " +
                                HPDTime:mTS2DateHMS(ldeNewStartTS)).

   END CONSTRUCTOR.
      
   
   DESTRUCTOR PUBLIC DumpTimeBased( ):
      
      IF AVAILABLE lbDumpHPD
      THEN DO:
         mInactivateDump(NO).

         IF objHandler:llAllOK
         THEN lbDumpHPD.StartTime = HPDTime:mTS2DateHMS(ldeToTS).

         FIND CURRENT lbDumpHPD NO-LOCK.
      END.

   END DESTRUCTOR.


   METHOD PUBLIC VOID mInactivateDump
      ( ilForce AS LOGICAL ):

      IF AVAILABLE lbDumpHPD AND lbDumpHPD.Active = YES
      THEN DO:
         IF ilForce
         THEN ASSIGN
                 lbDumpHPD.Continuous = NO
                 lbDumpHPD.Active     = NO
                 .
         ELSE IF lbDumpHPD.Continuous = NO
         THEN lbDumpHPD.Active = NO.
      END.

   END METHOD.


   METHOD PRIVATE VOID mAdjustToTS():

      IF lbDumpHPD.UnitsToDump < 0
      THEN RETURN ERROR NEW Progress.Lang.AppError("UnitsToDump cannot be less than zero.", 1).

      DEFINE VARIABLE ldeTempTs AS DECIMAL           NO-UNDO.

      IF ldeFromTS > ldeToTS
      THEN DO:
         ASSIGN
              ldeTempTs     = ldeToTS
              ldeToTs       = ldeFromTs
              ldeFromTs     = ldeTempTs
              .

         IF lbDumpHPD.UnitsToDump > 0
         THEN DO:
            ldeTempTs = HPDTime:mMake2DT(ADD-INTERVAL(HPDTime:mTS2DateTime(ldeToTS), lbDumpHPD.UnitsToDump * -1, lbDumpHPD.UnitType)).

            IF ldeTempTs > ldeFromTs
            THEN ldeFromTs = ldeTempTs.
         END.

         ldeNewStartTS = ldeFromTS.

      END.

      ELSE DO:

         IF lbDumpHPD.UnitsToDump > 0
         THEN ldeToTS = HPDTime:mMake2DT(ADD-INTERVAL(HPDTime:mTS2DateTime(ldeFromTS), lbDumpHPD.UnitsToDump, lbDumpHPD.UnitType)).

         ldeNewStartTS = ldeToTS.

      END.

   END METHOD.

END CLASS.
