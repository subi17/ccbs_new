 
 /*------------------------------------------------------------------------
    File        : OrderTopup
    Purpose     : 
    Syntax      : 
    Description : This Will store the OrderTopUp details
    Author(s)   : kmaddali
    Created     : Wed Jun 27 12:16:29 IST 2018
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.ParamError.
USING profcgi.RPC.JSON.InternalError.
USING Progress.Json.ObjectModel.JsonArray.
USING Progress.Json.ObjectModel.JsonObject.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS bss.cls.OrderTopup: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
    DEFINE PUBLIC PROPERTY  Amount AS DECIMAL NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY  VATAmount AS DECIMAL NO-UNDO
       GET.
       SET.	
       
    DEFINE PUBLIC PROPERTY  DispAmount AS DECIMAL NO-UNDO
        GET.
        SET. 
        
    DEFINE PUBLIC PROPERTY TopupType AS INTEGER NO-UNDO
       GET.
       SET.  
       
       
    DEFINE PUBLIC PROPERTY aoOrder AS CLASS bss.cls.Order  NO-UNDO 
        GET.
        SET.
       
	CONSTRUCTOR PUBLIC OrderTopup ( ioorder AS CLASS bss.cls.Order ):
		
		ASSIGN aoOrder    =  ioorder.
		
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderTopup ( ):
	    
	    ASSIGN aoOrder   =  ?.

	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData(INPUT ioTopupJsonObj   AS CLASS JsonObject) :
	    
	    ASSIGN THIS-OBJECT:Amount              =   ioTopupJsonObj:GetDecimal("initial_amount")
	           THIS-OBJECT:DispAmount          =   ioTopupJsonObj:GetDecimal("display_amount")
	           THIS-OBJECT:TopupType           =   {&INITIAL_TOPUP}.
	    
	END METHOD.
	
	METHOD PUBLIC VOID mStoreData(INPUT iiOrderProductID AS INTEGER) :
	    
	    CREATE OrderTopUp.
	    ASSIGN OrderTopUp.Brand                =   Syst.Var:gcBrand
	           OrderTopUp.OrderID              =   aoOrder:OrderId
	           OrderTopUp.OrderProductID       =   iiOrderProductID
	           OrderTopUp.Amount               =   THIS-OBJECT:Amount
	           OrderTopUp.VATAmount            =   THIS-OBJECT:VATAmount
	           OrderTopUp.DisplayAmount        =   THIS-OBJECT:DispAmount
	           OrderTopUp.TopupType            =   THIS-OBJECT:TopupType
	           .
	    
	END METHOD.	
	

END CLASS.