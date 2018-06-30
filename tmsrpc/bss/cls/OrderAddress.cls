 
/*------------------------------------------------------------------------
   File        : OrderAddress
   Purpose     : This will store the Address Related for an Order coming from BSS API
   Syntax      : 
   Description : 
   Author(s)   : kmaddali
   Created     : Mon Jun 11 13:24:22 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS bss.cls.OrderAddress IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY AddrID AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY Street AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY StreetType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BuildingNum AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY BlockNum AS CHARACTER NO-UNDO
       GET.
       SET.
		
	DEFINE PUBLIC PROPERTY Floor AS CHARACTER NO-UNDO
       GET.
       SET.	
       
    DEFINE PUBLIC PROPERTY Stair AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY Door AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY Letter AS CHARACTER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY KM AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY Hand AS CHARACTER NO-UNDO
       GET.
       SET.
       
     DEFINE PUBLIC PROPERTY BisDuplicate AS CHARACTER NO-UNDO
        GET.
        SET.  
        
     DEFINE PUBLIC PROPERTY AdditionalAddress AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY NormalizedID AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY Latitude AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY Longitude AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY City AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY Region AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY ZIP AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY Country AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY CityCode AS CHARACTER NO-UNDO
        GET.
        SET.
		
	 DEFINE PUBLIC PROPERTY StreetCode AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY MunicipalityCode AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY OrderAddrID AS INTEGER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY Gescal AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY CoverageToken AS CHARACTER NO-UNDO
       GET.
       SET.
      
    DEFINE PUBLIC PROPERTY KeyValue AS INTEGER NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY AddrType AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY AddrRegisterID AS CHARACTER NO-UNDO
       GET.
       SET.   
    
		
	CONSTRUCTOR PUBLIC OrderAddress ( ):
		 
	END CONSTRUCTOR.

	DESTRUCTOR PUBLIC OrderAddress ( ):

	END DESTRUCTOR.
	
	METHOD PUBLIC VOID mCreateData( ioOrderAddress AS CLASS JsonObject):
	    
	    DEFINE VARIABLE AddrValidData AS CLASS JsonObject NO-UNDO.
	    
        ASSIGN 
            THIS-OBJECT:AddrID            = NEXT-VALUE(AddrID)
            THIS-OBJECT:Street            = ioOrderAddress:GetCharacter("street")
            THIS-OBJECT:StreetType        = ioOrderAddress:GetCharacter("street_type")
            THIS-OBJECT:BuildingNum       = ioOrderAddress:GetCharacter("building_number")
            THIS-OBJECT:BlockNum          = ioOrderAddress:GetCharacter("block")
            THIS-OBJECT:Floor             = ioOrderAddress:GetCharacter("floor")
            THIS-OBJECT:Stair             = ioOrderAddress:GetCharacter("stair")
            THIS-OBJECT:Door              = ioOrderAddress:GetCharacter("door")
            THIS-OBJECT:Letter            = ioOrderAddress:GetCharacter("letter")
            THIS-OBJECT:KM                = ioOrderAddress:GetCharacter("km")
            THIS-OBJECT:Hand              = ioOrderAddress:GetCharacter("hand")
            THIS-OBJECT:BisDuplicate      = ioOrderAddress:GetCharacter("bis_duplicate")
            THIS-OBJECT:AdditionalAddress = ioOrderAddress:GetCharacter("additional_address")
            THIS-OBJECT:NormalizedID      = ioOrderAddress:GetCharacter("normalizedid")
            THIS-OBJECT:Latitude          = ioOrderAddress:GetCharacter("latitude")
            THIS-OBJECT:Longitude         = ioOrderAddress:GetCharacter("longitude")
            THIS-OBJECT:City              = ioOrderAddress:GetCharacter("city")
            THIS-OBJECT:Region            = ioOrderAddress:GetCharacter("region")
            THIS-OBJECT:ZIP               = ioOrderAddress:GetCharacter("zip")
            THIS-OBJECT:Country           = mGetCountry(ioOrderAddress:GetCharacter("country"))
            THIS-OBJECT:Gescal            = ioOrderAddress:GetCharacter("gescal") WHEN ioOrderAddress:Has("gescal")
            THIS-OBJECT:CoverageToken     = ioOrderAddress:GetCharacter("coveragetoken") WHEN ioOrderAddress:Has("coveragetoken")
            THIS-OBJECT:AddrRegisterID    = ioOrderAddress:GetCharacter("address_id")
        .
	    
	    IF ioOrderAddress:Has("validation_data") 
	    THEN DO:
	        
	        
	        ASSIGN AddrValidData                =   NEW JsonObject()
	               AddrValidData                =   ioOrderAddress:GetJsonObject("validation_data")
                   THIS-OBJECT:CityCode         =   AddrValidData:GetCharacter("city_code")
                   THIS-OBJECT:StreetCode       =   AddrValidData:GetCharacter("street_code")
                   THIS-OBJECT:MunicipalityCode =   AddrValidData:GetCharacter("municipality_code")
                   .
	    END. 
	    ELSE UNDO, THROW NEW ParamError("Validation Data missing for the AddressID " + STRING(THIS-OBJECT:AddrID)).    
	    
	END.
	
	METHOD PUBLIC VOID mStoreData():
	    
        CREATE Address.
        ASSIGN 
            Address.AddressID         = THIS-OBJECT:AddrID 
            Address.Address           = THIS-OBJECT:Street
            Address.StreetType        = THIS-OBJECT:StreetType       
            Address.BuildingNum       = THIS-OBJECT:BuildingNum      
            Address.Block             = THIS-OBJECT:BlockNum         
            Address.Floor             = THIS-OBJECT:Floor            
            Address.Stair             = THIS-OBJECT:Stair            
            Address.Door              = THIS-OBJECT:Door             
            Address.Letter            = THIS-OBJECT:Letter           
            Address.km                = THIS-OBJECT:KM               
            Address.Hand              = THIS-OBJECT:Hand             
            Address.bisduplicate      = THIS-OBJECT:BisDuplicate     
            Address.addresscompl      = THIS-OBJECT:AdditionalAddress
            /*Address.normalizedid   = THIS-OBJECT:NormalizedID*/     
            Address.Latitude          = THIS-OBJECT:Latitude         
            Address.longitude         = THIS-OBJECT:Longitude        
            Address.City              = THIS-OBJECT:City             
            Address.Region            = THIS-OBJECT:Region           
            Address.zipCode           = THIS-OBJECT:ZIP              
            Address.Country           = THIS-OBJECT:Country
            Address.CityCode          = THIS-OBJECT:CityCode
            Address.StreetCode        = THIS-OBJECT:StreetCode
            Address.towncode          = THIS-OBJECT:MunicipalityCode
            Address.HostTable         = "Customer"
            Address.KeyValue          = STRING(THIS-OBJECT:KeyValue)
            Address.AddressType       = THIS-OBJECT:AddrType
            Address.Gescal            = THIS-OBJECT:Gescal
            Address.CoverageToken     = THIS-OBJECT:CoverageToken
            Address.AddressRegisterID = THIS-OBJECT:AddrRegisterID
            .    
         IF NOT CAN-FIND(FIRST CustomerReport WHERE CustomerReport.CustNum = INTEGER(Address.KeyValue))
         AND THIS-OBJECT:OrderAddrID = 1 
         THEN DO:
             
             CREATE CustomerReport.
             ASSIGN 
                 CustomerReport.CustNum     =   INTEGER(Address.KeyValue)
                 CustomerReport.CityCode    =   Address.CityCode
                 CustomerReport.StreetCode  =   Address.StreetCode
                 CustomerReport.TownCode    =   Address.towncode
                 .
             
         END.                      
	END.  
	
    METHOD PRIVATE CHARACTER mGetCountry
        (icCountry AS CHARACTER):

        FOR Country NO-LOCK WHERE
            Country.Country = icCountry:
            RETURN Country.Country.
        END.

        UNDO, THROW NEW ParamError(SUBSTITUTE("country-unknown|&1", icCountry)).

    END METHOD.
	
	METHOD PUBLIC JsonObject mResult() :
	    
	    DEFINE VARIABLE ioAddrObject AS CLASS JsonObject NO-UNDO.
	    
	    ASSIGN ioAddrObject = NEW JsonObject().
	    
	    ioAddrObject:Add("address-id" , THIS-OBJECT:AddrRegisterID).
	    
	    RETURN ioAddrObject.
	    
	END METHOD.

END CLASS.