/*------------------------------------------------------------------------
    File        : DumpMQ
    Purpose     : Used for doing full dumps to MQ
    Syntax      : 
    Description : High performance data MQ dump
    Author(s)   : tlamminmaki
    Created     : Tue Oct 28 14:35:00 EET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING HPD.DumpHandler.
USING HPD.HPDTime.
USING HPD.DumpSomeWhere.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS HPD.DumpMQ INHERITS DumpSomeWhere: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PRIVATE VARIABLE objMsgPublisher    AS CLASS Gwy.MqPublisher NO-UNDO.
   
   CONSTRUCTOR PUBLIC DumpMQ
      ( iiDumpID AS INTEGER,
        iobjHandler AS CLASS DumpHandler ):      
      THIS-OBJECT(iiDumpID, "", iobjHandler).
   END CONSTRUCTOR.   

   CONSTRUCTOR PUBLIC DumpMQ
      ( iiDumpID    AS INTEGER,
        icFile      AS CHARACTER,
        iobjHandler AS CLASS DumpHandler ):
           
      SUPER(iiDumpID, iobjHandler).

      IF lbDumpFile.SideTables = ""
      THEN RETURN ERROR NEW Progress.Lang.AppError("Dump ID " + STRING(iiDumpID) + " doesn't support MQ (SideTables field is empty)", 1).

      DEFINE VARIABLE llUseSeparateQueue AS LOGICAL NO-UNDO.

      DO ON ERROR UNDO, THROW:

         llUseSeparateQueue = Syst.Parameters:geti("HPD.Queue" + lbDumpFile.SideTables + ".Separate", "HPD.Interface") = 1.

         /* Handler code for any error condition. */
         CATCH anyErrorObject AS Progress.Lang.Error:
            /* If no parameter set then no separate queue is used */
            llUseSeparateQueue = NO.
         END CATCH.

      END.

      mUpdateDumpHeader(1,"","").
      mUpdateDumpHeader(2,"","").

      objMsgPublisher = NEW Gwy.MqPublisher("HPD.Queue" +
                                            IF llUseSeparateQueue
                                            THEN lbDumpFile.SideTables
                                            ELSE "").

   END CONSTRUCTOR.
   
   
   DESTRUCTOR PUBLIC DumpMQ( ):

      IF VALID-OBJECT(objMsgPublisher)
      THEN DELETE OBJECT objMsgPublisher.

      IF objHandler:llAllOK = FALSE
      THEN LOG-MANAGER:WRITE-MESSAGE("The dump was interrupped or something else went wrong!", "ERROR").

   END DESTRUCTOR.


   METHOD PUBLIC OVERRIDE VOID mWriteALine(): 

      mWriteALine(mGetData()).

   END METHOD.


   METHOD PUBLIC OVERRIDE VOID mWriteALine
      (icData AS CHARACTER):

      DEFINE VARIABLE lii AS INTEGER NO-UNDO.

      DO lii = 1 TO 2:
         
         IF NOT objMsgPublisher:send_message(icData)
         THEN DO:
            LOG-MANAGER:WRITE-MESSAGE("Failed to send message. Try to send again after a reconnect", "INFO").
            IF NOT objMsgPublisher:mReConnect()
            THEN DO:
               LOG-MANAGER:WRITE-MESSAGE("Failed to reconnect.", "ERROR").
               LOG-MANAGER:WRITE-MESSAGE("Error occurred when sending message: " + icData, "INFO").
               RETURN ERROR NEW Progress.Lang.AppError("Failed to reconnect", 1).
            END.
            ELSE DO:
               LOG-MANAGER:WRITE-MESSAGE("Was able to reconnect and will send the message again.", "INFO").
               NEXT.
            END.
         END.
         LEAVE.
      END.

   END METHOD.


   METHOD PUBLIC OVERRIDE VOID mWriteALineWithPrefix
      (icPrefix AS CHARACTER):

      mWriteALine(icPrefix + mGetData()).

   END METHOD.

END CLASS.
