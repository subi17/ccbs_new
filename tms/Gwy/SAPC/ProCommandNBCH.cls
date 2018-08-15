 
 /*------------------------------------------------------------------------
    File        : ProCommandNBCH
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.*.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.Service.IService.
USING Gwy.SAPC.ProCommandOrder.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandNBCH INHERITS ProCommandOrder: 
   
   DEFINE PRIVATE BUFFER MobSub FOR MobSub.

   CONSTRUCTOR ProCommandNBCH
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE VARIABLE loJson        AS CLASS JsonObject   NO-UNDO.

      CASE MsRequest.ReqType:
   
         WHEN {&REQTYPE_CONTRACT_ACTIVATION}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:ADD_DATAPLAN
                 acProCommandType = "ADD_DATAPLAN".
         WHEN {&REQTYPE_CONTRACT_TERMINATION}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:DELETE_DATAPLAN
                 acProCommandType = "DELETE_DATAPLAN".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandNBCH cannot " +
                                                       "handle ReqType &1",
                                                       MsRequest.ReqType)).
      END CASE.

      FIND MobSub NO-LOCK WHERE 
          MobSub.MsSeq = aiMsSeq
      NO-ERROR.
      
      IF NOT AVAILABLE MobSub
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find mobsub &1 for msrequest &2",
                                               aiMsSeq, MsRequest.MsRequest)).

      ASSIGN
         acProCommandTarget = STRING(Gwy.SAPC.TargetEnum:NB_CH)
         acCLI              = MobSub.CLI
         acICC              = MobSub.ICC
         acCLIType          = MobSub.CLIType
         alPayType          = MobSub.PayType.

      loJson = mOrderObject("Change").
    
      aoService = NEW Gwy.SAPC.Service.MobileLineSAPC.UnderRoot(THIS-OBJECT).
      aoService:mCreateServices(loJson).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END CONSTRUCTOR.
   
END CLASS.