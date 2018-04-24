 
 /*------------------------------------------------------------------------
    File        : Order
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : tlamminmaki
    Created     : Mon Apr 23 12:30:10 EEST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.Order IMPLEMENTS tmsrpc.bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PUBLIC PROPERTY OrderId AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoCustomer AS CLASS bss.cls.Customer NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ContractId AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY Salesman AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY OrderChannel AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY CrStamp AS DECIMAL NO-UNDO
      GET.
      SET.

   /* A value for this property is populated in OrderProduct class */
   DEFINE PUBLIC PROPERTY MsSeq AS INTEGER NO-UNDO
      GET.
      SET(iiMSSeq AS INTEGER):
         DEFINE BUFFER MobSub FOR MobSub.
         FIND MobSub NO-LOCK WHERE MobSub.MSSeq = iiMSSeq NO-ERROR.
         IF NOT AVAILABLE MobSub
         THEN UNDO, THROW NEW InternalError
                  (SUBSTITUTE("MobSub having msseq '&1' doesn't exists", iiMSSeq)).
         THIS-OBJECT:MsSeq = iiMSSeq.
      END SET.

   DEFINE PUBLIC PROPERTY aoOrderProductFactory AS CLASS bss.cls.OrderProductFactory NO-UNDO
      GET.
      SET.

	CONSTRUCTOR PUBLIC Order():
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC Order():

      IF VALID-OBJECT(aoCustomer)
      THEN DELETE OBJECT aoCustomer.

      IF VALID-OBJECT(aoOrderProductFactory)
      THEN DELETE OBJECT aoOrderProductFactory.
      
	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData
	  (ioJsonObject AS CLASS JsonObject):

      DEFINE VARIABLE loAgrCust     AS CLASS JsonObject NO-UNDO.
      DEFINE VARIABLE ldaCreateDate AS DATE             NO-UNDO.

      ASSIGN
         loAgrCust = ioJsonObject:GetJsonObject("agreement-customer")
         aoCustomer = NEW bss.cls.Customer().
      
      IF loAgrCust:Has("custnum")
      THEN aoCustomer:mFindCustomer(loAgrCust:GetInteger("custnum")).
      ELSE aoCustomer:mCreateData(loAgrCust).

      ASSIGN
         ldaCreateDate = ioJsonObject:GetDate("created-at")
         THIS-OBJECT:ContractId   = ioJsonObject:GetCharacter("contractid")
         THIS-OBJECT:Salesman     = ioJsonObject:GetCharacter("salesman")
         THIS-OBJECT:OrderChannel = ioJsonObject:GetCharacter("channel")
         THIS-OBJECT:CrStamp      = YEAR(ldaCreateDate) * 10000 +
                                    MONTH(ldaCreateDate) * 100 +
                                    DAY(ldaCreateDate)
         .

         /*
         Order.InvCustRole     = 1
         Order.UserRole        = 1
         Order.StatusCode      = "1"
         Order.Reseller        = pcReseller
         Order.Source          = "newton"
         Order.OrdererIP       = pcIP
         Order.CLI        = pcCLI
         Order.CLIType    = pcSubType
         Order.mnpstatus  = INT(pcNumberType EQ "mnp")
         Order.paytype    = (IF CLIType.PayType = 2 THEN TRUE ELSE FALSE)  
         Order.oldpaytype = (pcOldBillCat EQ "prepaid")
         Order.ICC        = pcIcc
         Order.OldIcc     = pcOldIcc
         Order.CurrOper   = pcOldOperator
         Order.PortingDate = pcMNPPortingDate
         Order.FAT        = pfFAT WHEN pfFAT NE 0
         Order.FTGrp    = pcFATGrp
         Order.SMSType  = INT(plSendSMS)
         Order.Campaign = pcCampaignCode
         Order.CampaignType = pcCampaignType 
         Order.Referee  = pcReferee
         Order.Offer = pcOfferId 
         Order.ROIResult = pcROIresult
         Order.RiskCode = pcROIriskcode
         Order.MultiSimID = piMultiSimID
         Order.UsageType = pcUsageType
         Order.DeliveryType = piDeliveryType
         Order.DeliverySecure = piDeliverySecure
         Order.SendOffer = plSendOffer
         Order.ResignationPeriod  = plResignationPeriod 
         Order.RoiLevel = INT(pcRoilevel)
         Order.MultiSimType = piMultiSimType
         Order.MsSeq = (IF 
                        LOOKUP(pcNumberType,"new,mnp,migration") > 0 
                        THEN NEXT-VALUE(MobSub)
                        ELSE MobSub.MsSeq).
         Order.Multiorder = plMultiOrder.               
         */

      IF ioJsonObject:Has("items")
      THEN DO:
         aoOrderProductFactory = NEW bss.cls.OrderProductFactory().
         aoOrderProductFactory:mCreateData(ioJsonObject:GetJsonArray("items"), THIS-OBJECT).
      END.
      ELSE UNDO, THROW NEW ParamError("items array is mandatory").

	END METHOD.

   METHOD PUBLIC VOID mStoreData():

      /* If there is a new customer data we need to create it first */
      aoCustomer:mStoreData().
      
      CREATE Order.
      ASSIGN
         Order.OrderId       = NEXT-VALUE(OrderId)
         THIS-OBJECT:OrderId = Order.OrderId /* The OrderProducts needs this */
         Order.CustNum       = aoCustomer:CustNum
         Order.MsSeq         = THIS-OBJECT:MsSeq
         Order.ContractID    = THIS-OBJECT:ContractId
         Order.SalesMan      = THIS-OBJECT:Salesman
         Order.OrderChannel  = THIS-OBJECT:OrderChannel
         Order.CrStamp       = THIS-OBJECT:CrStamp.

      aoOrderProductFactory:mStoreData().

   END METHOD.

END CLASS.