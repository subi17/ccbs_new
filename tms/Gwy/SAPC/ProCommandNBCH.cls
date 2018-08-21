 
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

   METHOD PRIVATE VOID mSetTypeValues():

      DEFINE BUFFER lbMsRequest FOR MsRequest.
      DEFINE BUFFER DayCampaign FOR DayCampaign.

      /* The logic in percontr.p file dictates that
         if theMsRequest.ReqType EQ {&REQTYPE_CONTRACT_TERMINATION}
         in here then it cannot be subrequest for STC or
         bundle change request */

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_CONTRACT_TERMINATION}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:DATAPLAN_DELETE
                 acProCommandType = "DELETE_DATAPLAN".

         WHEN {&REQTYPE_CONTRACT_ACTIVATION}
         THEN DO:
            /* Parent request needs to be checked when
               requesttype is contract activation to get
               information if this a modify dataplan type.
               Only activation request can have the STC or
               bundle change parent request due to logic
               in percontr.p file */
            IF MsRequest.OrigRequest > 0
            THEN DO:
               FIND lbMsRequest NO-LOCK WHERE
                    lbMsRequest.MsRequest = MsRequest.OrigRequest NO-ERROR.

               IF NOT AVAILABLE lbMsRequest
               THEN UNDO, THROW NEW AppError(SUBSTITUTE("cannot find origRequest &1",
                                                        MsRequest.OrigRequest)).

               /* We need to verify that the termination contract has
                  is a bundle which is provisioned (i.e. the EMAcode > "")

                  => If it is not then normal activation provision command
                     is sent
               */
               IF ( lbMsRequest.ReqType EQ {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
                    lbMsRequest.ReqType EQ {&REQTYPE_BUNDLE_CHANGE} )
               THEN
               FOR EACH lbMsRequest NO-LOCK WHERE
                        lbMsRequest.MsRequest   EQ MsRequest.ReqIParam2 AND
                        lbMsRequest.OrigRequest EQ MsRequest.OrigRequest AND
                        lbMsRequest.ReqType     EQ {&REQTYPE_CONTRACT_TERMINATION},
                   FIRST DayCampaign NO-LOCK WHERE
                         DayCampaign.Brand   EQ "1" AND
                         DayCampaign.DCEvent EQ MsRequest.ReqCParam3 AND
                         DayCampaign.EMACode > "":
                  ASSIGN
                       aoOrderEnum      = OrderEnum:DATAPLAN_MODIFY
                       acProCommandType = "MODIFY_DATAPLAN"
                       acEMACode        = DayCampaign.EMACode.
               END.
               ELSE 
               IF lbMsRequest.ReqCparam3 MATCHES "*_UPSELL" THEN
                  ASSIGN
                     aoOrderEnum      = OrderEnum:UPSELL_ADD
                     acProCommandType = "ADD_UPSELL"
                     acEMACode        = DayCampaign.EMACode.
            END.

            IF acProCommandType EQ ""
            THEN ASSIGN
                    aoOrderEnum      = OrderEnum:DATAPLAN_ADD
                    acProCommandType = "ADD_DATAPLAN".
         END.
         WHEN {&REQTYPE_ICC_CHANGE}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:CHANGE_SIM
                 acProCommandType = "CHANGE_SIM".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandNBCH cannot " +
                                                       "handle ReqType &1",
                                                       MsRequest.ReqType)).
      END CASE.

   END METHOD.


   CONSTRUCTOR ProCommandNBCH
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE VARIABLE loJson        AS CLASS JsonObject   NO-UNDO.

      mSetTypeValues().

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
    
      aoService = NEW Gwy.SAPC.Service.ChangeAPI.UnderRoot(THIS-OBJECT).
      aoService:mCreateServices(loJson).

      FINALLY:
         IF VALID-OBJECT(loJson)
         THEN aoCommandLine = loJson.
      END FINALLY.

   END CONSTRUCTOR.
   
END CLASS.