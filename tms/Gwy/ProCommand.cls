 
 /*------------------------------------------------------------------------
    File        : ProCommand
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.ProCommand: 

   DEFINE PUBLIC VARIABLE aiOrderId             AS INTEGER     NO-UNDO.
   DEFINE PUBLIC VARIABLE aiMsRequest           AS INTEGER     NO-UNDO.
   DEFINE PUBLIC VARIABLE aiMsSeq               AS INTEGER     NO-UNDO.
   DEFINE PUBLIC VARIABLE aiProCommandId        AS INT64       NO-UNDO.
   DEFINE PUBLIC VARIABLE acProCommandTarget    AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE acProCommandTargetURL AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE acProCommandType      AS CHARACTER   NO-UNDO.
   DEFINE PUBLIC VARIABLE acProCommandVerb      AS CHARACTER   NO-UNDO.
   
   DEFINE PUBLIC VARIABLE aoCommandLine         AS CLASS JsonConstruct NO-UNDO.
   
   DEFINE PROTECTED BUFFER ProCommand FOR ProCommand.
   DEFINE PROTECTED BUFFER MsRequest  FOR MsRequest.

   CONSTRUCTOR PUBLIC ProCommand
      ( iiMsRequest AS INTEGER ):

      FIND MsRequest NO-LOCK WHERE MsRequest.MsRequest = iiMsRequest NO-ERROR.
      
      IF NOT AVAILABLE MsRequest
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find MsRequest &1",
                                               iiMsRequest)).

      ASSIGN
         aiMsRequest = iiMsRequest
         aiMsSeq     = MsRequest.MsSeq.

   END CONSTRUCTOR.

   DESTRUCTOR ProCommand():
      IF VALID-OBJECT(aoCommandLine)
      THEN DELETE OBJECT aoCommandLine.
   END DESTRUCTOR.

   METHOD PUBLIC VOID mSetCommandLine
      (ioJsonObject AS CLASS JsonObject):
      aoCommandLine = ioJsonObject.
   END METHOD.

   METHOD PUBLIC VOID mSetCommandLine
      (ioJsonArray AS CLASS JsonArray):
      aoCommandLine = ioJsonArray.
   END METHOD.

   METHOD PUBLIC VOID mSetProCommandId():
      aiProCommandId = NEXT-VALUE(Seq_ProCommand_ProCommandId).
   END METHOD.

   METHOD PUBLIC VOID mStoreProCommand():

      DEFINE VARIABLE lmpJsonData AS MEMPTR NO-UNDO.
      
      IF VALID-OBJECT(aoCommandLine)
      THEN DO:
         IF NOT aoCommandLine:Write(lmpJsonData, YES)
         THEN UNDO, THROW NEW AppError("Json generation failed").
      END.
      
      IF aiProCommandId EQ 0
      THEN mSetProCommandId().

      CREATE ProCommand.
      ASSIGN
         ProCommand.OrderId             = aiOrderId
         ProCommand.MSrequest           = aiMsRequest
         ProCommand.ProcommandId        = aiProCommandId
         ProCommand.ProCommandType      = acProCommandType
         ProCommand.CreatedTS           = NOW 
         ProCommand.Creator             = IF AVAILABLE MsRequest
                                          THEN MsRequest.UserCode
                                          ELSE Syst.Var:katun    
         ProCommand.MsSeq               = aiMsSeq
         ProCommand.ProCommandStatus    = {&PROCOMMANDSTATUS_NEW}
         ProCommand.ProCommandTarget    = acProCommandTarget
         ProCommand.ProCommandVerb      = acProCommandVerb
         ProCommand.ProCommandTargetURL = acProCommandTargetURL.

      IF GET-SIZE(lmpJsonData) > 0
      THEN COPY-LOB FROM OBJECT lmpJsonData TO OBJECT ProCommand.CommandLine NO-CONVERT.

      FINALLY:
         SET-SIZE(lmpJsonData) = 0.
      END FINALLY.
      
   END METHOD.

END CLASS.