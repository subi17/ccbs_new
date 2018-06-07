 
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

CLASS bss.cls.OrderPayment IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY PayMethod   AS CHARACTER NO-UNDO
	   GET.
	   SET.
	   
	DEFINE PUBLIC PROPERTY CCReference AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CCValid     AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AuthNumber  AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BinNumber   AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY PaymentSource AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY PayerID AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY PMethod AS INTEGER NO-UNDO
       GET.
       SET.
          
    DEFINE PROTECTED VARIABLE aoOrder AS CLASS bss.cls.Order NO-UNDO.
    	
	CONSTRUCTOR PUBLIC OrderPayment ( ioOrder AS CLASS bss.cls.Order  ):
		
		ASSIGN aoOrder = ioOrder.
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderPayment ( ):
	    
	    ASSIGN aoOrder = ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData (ioPaymentObject AS CLASS JsonObject) :	    
	   
	    ASSIGN THIS-OBJECT:PayMethod  = ioPaymentObject:GetCharacter("method")  WHEN ioPaymentObject:Has("method").
	    
        IF THIS-OBJECT:PayMethod = "creditcard"  
        THEN ASSIGN 
                THIS-OBJECT:CCReference    = ioPaymentObject:GetCharacter("ccreference")    WHEN ioPaymentObject:Has("ccreference")
                THIS-OBJECT:CCValid        = ioPaymentObject:GetCharacter("ccvalid")        WHEN ioPaymentObject:Has("ccvalid")
                THIS-OBJECT:AuthNumber     = ioPaymentObject:GetCharacter("authnumber")     WHEN ioPaymentObject:Has("authnumber")
                THIS-OBJECT:BinNumber      = ioPaymentObject:GetCharacter("binnumber")      WHEN ioPaymentObject:Has("binnumber")
                THIS-OBJECT:PaymentSource  = ioPaymentObject:GetCharacter("payment-source") WHEN ioPaymentObject:Has("payment-source")
                THIS-OBJECT:PMethod        = {&ORDERPAYMENT_M_CREDIT_CARD}
                . 
        ELSE IF THIS-OBJECT:PayMethod = "paypal"
        THEN ASSIGN 
                 THIS-OBJECT:PayerID  =  ioPaymentObject:GetCharacter("payer-id") WHEN ioPaymentObject:Has("payer-id")
                 THIS-OBJECT:PMethod  =  {&ORDERPAYMENT_M_PAYPAL}
                 .
            
	END.
	
	METHOD PUBLIC VOID mStoreData() :
	    
	    CREATE OrderPayment.
        ASSIGN
            OrderPayment.Brand       = Syst.Var:gcBrand
            OrderPayment.OrderId     = aoOrder:OrderId
            OrderPayment.Method      = THIS-OBJECT:PMethod  
            OrderPayment.CCReference = THIS-OBJECT:CCReference
            OrderPayment.CCValid     = THIS-OBJECT:CCValid  
            OrderPayment.AuthNumber  = THIS-OBJECT:AuthNumber
            OrderPayment.BinNumber   = THIS-OBJECT:BinNumber
            OrderPayment.CCNumber    = THIS-OBJECT:PayerID
            .	    
	       
	END METHOD.

END CLASS. 
    
    