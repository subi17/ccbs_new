 
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
   
    DEFINE PUBLIC PROPERTY objOrderProductFactory AS CLASS pm.OrderProductFactory NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY orderProductID AS INTEGER NO-UNDO
        GET.
        SET.
   
    DEFINE PUBLIC PROPERTY orderID AS INTEGER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY actionType AS CHARACTER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY productId AS CHARACTER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC PROPERTY productOfferingID AS CHARACTER NO-UNDO
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY itGroupId AS INTEGER NO-UNDO
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY ParentID AS INTEGER NO-UNDO
        GET.
        SET.
      
    DEFINE PUBLIC PROPERTY billAcctIndex AS INTEGER NO-UNDO
        GET.
        SET.

    DEFINE PUBLIC VARIABLE objOrder AS CLASS om.Order NO-UNDO.

	CONSTRUCTOR PUBLIC OrderProduct( iObjOrder AS CLASS om.Order ):
	    
        ASSIGN objOrder = iObjOrder.
        	
	END CONSTRUCTOR.

   METHOD PUBLIC ABSTRACT VOID mCreateDataForType(ioItemObject AS CLASS JsonObject).

	METHOD PUBLIC VOID mCreateData
	  ( ioItemObject AS CLASS JsonObject ):

      ASSIGN
         THIS-OBJECT:actionType         = ioItemObject:GetCharacter("type")
         THIS-OBJECT:productID          = ioItemObject:GetCharacter("product_id")  WHEN ioItemObject:Has("product_id")   
         THIS-OBJECT:productOfferingID  = ioItemObject:GetCharacter("product_offering_id").
         
      IF ioItemObject:Has("billing_account_id") THEN
      DO:
          /*This will be executed in case of existing customer.*/
          ASSIGN THIS-OBJECT:itGroupId = ioItemObject:GetInteger("billing_account_id").
          mIsBillingAccountExists().
         
      END.       
      ELSE IF ioItemObject:Has("billing_account_index") THEN
      DO:              
          ASSIGN 
              THIS-OBJECT:billAcctIndex = ioItemObject:GetInteger("billing_account_index")                 
              THIS-OBJECT:itGroupId     = objOrder:objCustomer:mGetBillingAcctID(THIS-OBJECT:billAcctIndex).             
      END. 

      mCreateDataForType(ioItemObject).
      
      IF ioItemObject:Has("items") THEN
      DO:
          
         ASSIGN objOrderProductFactory = NEW pm.OrderProductFactory().
         objOrderProductFactory:mCreateData(ioItemObject:GetJsonArray("items"), objOrder).
         ASSIGN objOrderProductFactory:isChildArray =  TRUE.
             
      END.
     
	END METHOD.

   METHOD PUBLIC ABSTRACT VOID mStoreDataForType().

    METHOD PUBLIC VOID mStoreData():
      
        CREATE abOrderProduct.
        ASSIGN
            abOrderProduct.OrderProductId    = NEXT-VALUE(OrderProductID)
            abOrderProduct.OrderId           = objOrder:OrderId
            THIS-OBJECT:orderProductID       = abOrderProduct.OrderProductId
            THIS-OBJECT:orderID              = abOrderProduct.OrderId
            abOrderProduct.actionType        = THIS-OBJECT:actionType
            abOrderProduct.ProductID         = THIS-OBJECT:productID
            abOrderProduct.ProductOfferingID = THIS-OBJECT:productOfferingID
            abOrderProduct.ParentID          = THIS-OBJECT:ParentID
            abOrderProduct.ITGroupID         = THIS-OBJECT:itGroupId
            .
            
        IF THIS-OBJECT:ParentID = 0 THEN 
            abOrderProduct.StatusCode = objOrder:statusCode.   
     
        mStoreDataForType().

        IF VALID-OBJECT(objOrderProductFactory) THEN
        DO:
            IF objOrderProductFactory:isChildArray THEN
                ASSIGN objOrderProductFactory:ParentID = THIS-OBJECT:orderProductID.
            objOrderProductFactory:mStoreData().
        END.

    END METHOD.
   
   METHOD PUBLIC VOID mIsBillingAccountExists() :
            
       FIND FIRST InvoiceTargetGroup NO-LOCK WHERE 
           InvoiceTargetGroup.ITGroupID    =   THIS-OBJECT:itGroupId 
           NO-ERROR.
                       
      IF NOT AVAILABLE InvoiceTargetGroup THEN
          UNDO, THROW NEW ParamError(SUBSTITUTE("Billing Account &1 does not exist.", THIS-OBJECT:itGroupId)). 
          
      IF InvoiceTargetGroup.AccountID  <>  objOrder:custAcctNum THEN  
          UNDO , THROW NEW ParamError(SUBSTITUTE("Billing Account &1 does not associated to the entered customer account &2 .",THIS-OBJECT:itGroupId , objOrder:custAcctNum )). 
                                                        
      ASSIGN objOrder:objCustomer:OrderCustBankAcct = InvoiceTargetGroup.BankAccount.
                                 
    END METHOD.

   METHOD PUBLIC VOID mResult(ioJsonArray AS CLASS JsonArray):

      DEFINE VARIABLE loJsonObject AS CLASS JsonObject NO-UNDO.

      loJsonObject = NEW JsonObject().
      loJsonObject:Add("item-id", THIS-OBJECT:orderProductID).
      loJsonObject:Add("item-type" , THIS-OBJECT:actionType).      
      ioJsonArray:Add(loJsonObject).

      IF VALID-OBJECT(objOrderProductFactory) THEN
          objOrderProductFactory:mResult(loJsonObject).

   END METHOD.   
   
    DESTRUCTOR PUBLIC OrderProduct ( ):
        
        IF VALID-OBJECT(objOrderProductFactory) THEN
            DELETE OBJECT objOrderProductFactory.
        
        IF VALID-OBJECT(objOrder) THEN
            DELETE OBJECT objOrder.
        
    END DESTRUCTOR.

END CLASS.
