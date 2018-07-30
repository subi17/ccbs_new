 
 /*------------------------------------------------------------------------
    File        : OrderProduct
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
USING Progress.Json.ObjectModel.JsonArray.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS pm.OrderProduct ABSTRACT IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/

   DEFINE PROTECTED BUFFER abOrderProduct FOR OrderProduct.

   DEFINE PUBLIC PROPERTY OrderProductID AS INTEGER NO-UNDO
      GET.
      SET.
   
   DEFINE PUBLIC PROPERTY OrderID AS INTEGER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ActionType AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ProductID AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY ProductOfferingID AS CHARACTER NO-UNDO
      GET.
      SET.

   DEFINE PUBLIC PROPERTY aoOrderProductFactory AS CLASS pm.OrderProductFactory NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY ITGroupID AS INTEGER NO-UNDO
      GET.
      SET.
      
   DEFINE PUBLIC PROPERTY ParentID AS INTEGER NO-UNDO
      GET.
      SET(INPUT iiParentID AS INTEGER):
          
          ASSIGN THIS-OBJECT:ParentID    =   iiParentID.
          
      END SET.
      
   DEFINE PUBLIC PROPERTY BillAcctIndex AS INTEGER NO-UNDO
       GET.
       SET.

   DEFINE PUBLIC VARIABLE aoOrder AS CLASS om.Order NO-UNDO.

	CONSTRUCTOR PUBLIC OrderProduct
     ( ioOrder AS CLASS om.Order ):
      aoOrder = ioOrder.	
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderProduct ( ):
      IF VALID-OBJECT(aoOrderProductFactory)
      THEN DELETE OBJECT aoOrderProductFactory.
      aoOrder = ?.
	END DESTRUCTOR.

   METHOD PUBLIC ABSTRACT VOID mCreateDataForType(ioItemObject AS CLASS JsonObject).

	METHOD PUBLIC VOID mCreateData
	  ( ioItemObject AS CLASS JsonObject ):

      ASSIGN
         ActionType         = ioItemObject:GetCharacter("type")
         ProductID          = ioItemObject:GetCharacter("product_id")  WHEN ioItemObject:Has("product_id")   
         ProductOfferingID  = ioItemObject:GetCharacter("product_offering_id").
         
      IF ActionType = {&ORDER_PRODUCT_SUBSCRIPTION} OR 
         ActionType = {&ORDER_PRODUCT_FIXED_LINE} 
      THEN DO:
          
          IF NOT CAN-FIND(FIRST CLIType WHERE CLIType.Brand = Syst.Var:gcBrand AND CLIType.CliType = ProductID)
          THEN UNDO, THROW NEW ParamError (SUBSTITUTE("Unknown Product: ", ProductID)).
             
      END.
     
      IF ioItemObject:Has("billing_account_id")
      THEN DO:
          /*This will be executed in case of existing customer.*/
          ASSIGN THIS-OBJECT:ITGroupID = ioItemObject:GetInteger("billing_account_id").
          mIsBillingAccountExists().
         
      END.       
      ELSE IF ioItemObject:Has("billing_account_index")
      THEN DO:    
          ASSIGN 
              THIS-OBJECT:BillAcctIndex = ioItemObject:GetInteger("billing-account-index")                 
              THIS-OBJECT:ITGroupID     = aoOrder:aoCustomer:mGetBillingAcctID(THIS-OBJECT:BillAcctIndex).
             
      END. 
      ELSE DO:
          
          IF THIS-OBJECT:ActionType = {&ORDER_PRODUCT_SUBSCRIPTION} OR 
             THIS-OBJECT:ActionType = {&ORDER_PRODUCT_FIXED_LINE}   OR 
             THIS-OBJECT:ActionType = {&ORDER_PRODUCT_ACCESSORY}  
          THEN UNDO , THROW  NEW ParamError("Billing Account information missing."). 
      END. 

      mCreateDataForType(ioItemObject).
      
      IF ioItemObject:Has("items")
      THEN DO:
         aoOrderProductFactory = NEW pm.OrderProductFactory().
         aoOrderProductFactory:mCreateData(ioItemObject:GetJsonArray("items"), aoOrder).
         IF  THIS-OBJECT:ActionType <> {&ORDER_PRODUCT_MAIN}
         THEN ASSIGN aoOrderProductFactory:isChildArray =  TRUE.
      END.
     
	END METHOD.

   METHOD PUBLIC ABSTRACT VOID mStoreDataForType().

   METHOD PUBLIC VOID mStoreData():
     
       IF THIS-OBJECT:ActionType <> {&ORDER_PRODUCT_MAIN} THEN            
       DO:
           CREATE abOrderProduct.
           ASSIGN
               abOrderProduct.OrderProductId    = NEXT-VALUE(OrderProductID)
               abOrderProduct.OrderId           = aoOrder:OrderId
               THIS-OBJECT:OrderProductID       = abOrderProduct.OrderProductId
               THIS-OBJECT:OrderID              = abOrderProduct.OrderId
               abOrderProduct.ActionType        = THIS-OBJECT:ActionType
               abOrderProduct.ProductID         = THIS-OBJECT:ProductID
               abOrderProduct.ProductOfferingID = THIS-OBJECT:ProductOfferingID
               abOrderProduct.ParentID          = THIS-OBJECT:ParentID
               abOrderProduct.ITGroupID         = THIS-OBJECT:ITGroupID
               .
       END.
     
       mStoreDataForType().

       IF VALID-OBJECT(aoOrderProductFactory)
       THEN DO:
           IF aoOrderProductFactory:isChildArray
           THEN ASSIGN aoOrderProductFactory:ParentID   =  THIS-OBJECT:OrderProductID.
           aoOrderProductFactory:mStoreData().
       END.

   END METHOD.
   
METHOD PUBLIC VOID mIsBillingAccountExists() :
        
    FIND FIRST InvoiceTargetGroup NO-LOCK WHERE 
        InvoiceTargetGroup.ITGroupID    =   THIS-OBJECT:ITGroupID 
        NO-ERROR.
                   
    IF NOT AVAILABLE InvoiceTargetGroup
        THEN UNDO, THROW NEW ParamError(SUBSTITUTE("Billing Account &1 does not exist.", THIS-OBJECT:ITGroupID)). 
        
    IF InvoiceTargetGroup.AccountID  <>  aoOrder:CustAcctNum 
        THEN UNDO , THROW NEW ParamError(SUBSTITUTE("Billing Account &1 does not associated to the entered customer account &2 .", 
                                                     THIS-OBJECT:ITGroupID , aoOrder:CustAcctNum )). 
                                                    
    ASSIGN 
        aoOrder:aoCustomer:OrderCustBankAcct = InvoiceTargetGroup.BankAccount.
                             
END METHOD.

   METHOD PUBLIC VOID mResult
      (ioJsonArray AS CLASS JsonArray):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("parent-id" , THIS-OBJECT:ParentID).
      loJsonObject:Add("order-item-id", THIS-OBJECT:OrderProductID).
      loJsonObject:Add("order-item-type" , THIS-OBJECT:ActionType).      
      ioJsonArray:Add(loJsonObject).

      IF VALID-OBJECT(aoOrderProductFactory)
      THEN aoOrderProductFactory:mResult(loJsonObject).

   END METHOD.

END CLASS.
