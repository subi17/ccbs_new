 
 /*------------------------------------------------------------------------
    File        : ProCommandBPMMobSub
    Purpose     : BPM messages where no up to date order information
                  is available (e.g. reactivation request or subscription
                  termination)
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Aug 06 15:52:48 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Json.ObjectModel.JsonObject.
USING Gwy.SAPC.OrderEnum.
USING Gwy.SAPC.ProCommandBPM.

{Syst/tmsconst.i}

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Gwy.SAPC.ProCommandBPMMobSub INHERITS ProCommandBPM: 

   DEFINE PRIVATE BUFFER Customer FOR Customer.

   CONSTRUCTOR ProCommandBPMMobSub
      ( iiMsRequest AS INTEGER ):

      SUPER(iiMsRequest).

      DEFINE BUFFER MobSub FOR MobSub.

      CASE MsRequest.ReqType:
         WHEN {&REQTYPE_SUBSCRIPTION_REACTIVATION}
         THEN ASSIGN
                 aoOrderEnum      = OrderEnum:SUBSCRIPTION_REACTIVATE
                 acProCommandType = "SUBSCRIPTION_REACTIVATE"
                 acDonorOperator  = mDonorOperator(MsRequest.ReqIParam1) WHEN MsRequest.ReqIParam1 > 0.
         WHEN {&REQTYPE_SUBSCRIPTION_TERMINATION}
         THEN ASSIGN
                 aoOrderEnum = OrderEnum:SUBSCRIPTION_TERMINATE
                 acProCommandType = "SUBSCRIPTION_TERMINATE"
                 acDonorOperator = "005" WHEN MsRequest.ReqCParam2 > "".
         OTHERWISE UNDO, THROW NEW AppError(SUBSTITUTE("ProCommandBPMMobSub cannot " +
                                                       "handle ReqType &1",
                                                       MsRequest.ReqType)).
      END CASE.

      FIND MobSub NO-LOCK WHERE 
          MobSub.MsSeq = aiMsSeq
      NO-ERROR.
      
      IF NOT AVAILABLE MobSub
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find mobsub &1 for msrequest &2",
                                               aiMsSeq, MsRequest.MsRequest)).

      FIND Customer NO-LOCK WHERE 
          Customer.Custnum = MsRequest.CustNum
      NO-ERROR.
      
      IF NOT AVAILABLE Customer
      THEN UNDO, THROW NEW AppError(SUBSTITUTE("Cannot find customer &1 for msrequest &2",
                                               MsRequest.CustNum, MsRequest.MsRequest)).

      ASSIGN
         acCLI              = MobSub.CLI
         acICC              = MobSub.ICC
         acCLIType          = MobSub.CLIType
         alPayType          = MobSub.PayType.

      mCreateCommandLine(Customer.OrgId, Customer.Category).

   END CONSTRUCTOR.

   METHOD PROTECTED OVERRIDE JsonObject mContactObject():

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().

      loJsonObject:Add("firstName", Customer.FirstName).
      loJsonObject:Add("middleName", Customer.CustName).
      loJsonObject:Add("lastName", IF Customer.SurName2 NE ""
                                   THEN Customer.SurName2
                                   ELSE Customer.CustName).
      loJsonObject:Add("documentNumber", Customer.OrgId).
      loJsonObject:Add("documentType", Customer.CustIDType).
      loJsonObject:Add("email", Customer.Email).
      loJsonObject:Add("phoneNumber", IF Customer.SMSNumber > ""
                                      THEN Customer.SMSNumber
                                      ELSE Customer.Phone).

      RETURN loJsonObject.

   END METHOD.

END CLASS.