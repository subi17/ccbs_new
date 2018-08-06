 
 /*------------------------------------------------------------------------
    File        : OrderPayment
    Purpose     : To Store the Payment data while Order Creation
    Syntax      : 
    Description : 
    Author(s)   : Koundinya Maddali
    Created     : Wed May 30 16:32:40 IST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.

BLOCK-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS om.OrderPayment IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY payMethod   AS CHARACTER NO-UNDO
	   GET.
	   SET.
	   
	DEFINE PUBLIC PROPERTY ccReference AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY ccValid     AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY authNumber  AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY binNumber   AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY payerID AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY payMethodID AS INTEGER NO-UNDO
       GET.
       SET.
          
    DEFINE PROTECTED VARIABLE objOrder AS CLASS om.Order NO-UNDO.
    	
	CONSTRUCTOR PUBLIC OrderPayment ( ioOrder AS CLASS om.Order  ):
		
		ASSIGN objOrder = ioOrder.
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderPayment ( ):
	    
	    ASSIGN objOrder = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData (iObjPaymentStruct AS CLASS JsonObject) :	    
	   
	    ASSIGN THIS-OBJECT:payMethod  = iObjPaymentStruct:GetCharacter("method")  WHEN iObjPaymentStruct:Has("method").
	    
        IF THIS-OBJECT:payMethod = "creditcard"  THEN 
            ASSIGN 
                THIS-OBJECT:ccReference    = iObjPaymentStruct:GetCharacter("ccreference")    WHEN iObjPaymentStruct:Has("ccreference")
                THIS-OBJECT:ccValid        = iObjPaymentStruct:GetCharacter("ccvalid")        WHEN iObjPaymentStruct:Has("ccvalid")
                THIS-OBJECT:authNumber     = iObjPaymentStruct:GetCharacter("authnumber")     WHEN iObjPaymentStruct:Has("authnumber")
                THIS-OBJECT:binNumber      = iObjPaymentStruct:GetCharacter("binnumber")      WHEN iObjPaymentStruct:Has("binnumber")
                THIS-OBJECT:payMethodID      = {&ORDERPAYMENT_M_CREDIT_CARD}
                . 
        ELSE IF THIS-OBJECT:payMethod = "paypal" THEN 
            ASSIGN 
                 THIS-OBJECT:payerID  =  iObjPaymentStruct:GetCharacter("payerid") WHEN iObjPaymentStruct:Has("payerid")
                 THIS-OBJECT:payMethodID  =  {&ORDERPAYMENT_M_PAYPAL}
                 .
            
	END.
	
	METHOD PUBLIC VOID mStoreData() :
	    
	    CREATE OrderPayment.
        ASSIGN
            OrderPayment.Brand       = Syst.Var:gcBrand
            OrderPayment.OrderId     = objOrder:OrderId
            OrderPayment.Method      = THIS-OBJECT:payMethodID  
            OrderPayment.CCReference = THIS-OBJECT:ccReference
            OrderPayment.CCValid     = THIS-OBJECT:ccValid  
            OrderPayment.AuthNumber  = THIS-OBJECT:authNumber
            OrderPayment.BinNumber   = THIS-OBJECT:binNumber
            OrderPayment.CCNumber    = THIS-OBJECT:payerID
            .	    
	       
	END METHOD.

END CLASS. 
    
    