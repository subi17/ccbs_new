 
/*------------------------------------------------------------------------
   File        : OrderAddress
   Purpose     : This will store the Address Related for an Order coming from BSS API
   Syntax      : 
   Description : 
   Author(s)   : Koundinya Maddali
   Created     : Mon Jun 11 13:24:22 IST 2018
   Notes       : 
 ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING profcgi.RPC.JSON.InternalError.
USING profcgi.RPC.JSON.ParamError.
USING Progress.Json.ObjectModel.JsonObject.
USING Progress.Json.ObjectModel.JsonArray.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Syst/tmsconst.i}

CLASS om.OrderAddress IMPLEMENTS bss.cls.IObjectStorage: 
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
            THIS-OBJECT:AddrRegisterID    = ioOrderAddress:GetCharacter("address_id")
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
	    
	    FIND FIRST Address WHERE Address.AddressID = THIS-OBJECT:AddrID EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
	    
	    IF LOCKED(Address) THEN 
            UNDO, THROW NEW ParamError("Address record is locked.").
	    
	    IF NOT AVAILABLE Address THEN CREATE Address.
	    
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
	
	
	METHOD PUBLIC VOID mUpdateAddressData(INPUT iiBillAddrID AS INTEGER  , INPUT ioBillAddrObj AS JsonObject) :
	    
	    DEFINE BUFFER bfAddress FOR Address.
	    
	    DEFINE VARIABLE loAddrValidationObj AS CLASS JsonObject NO-UNDO.
	    
	    FIND FIRST bfAddress WHERE bfAddress.AddressID  =  iiBillAddrID  NO-LOCK NO-ERROR.
	    
	    IF NOT AVAILABLE bfAddress THEN 
            UNDO, THROW NEW ParamError("Billing address is not available to update.").
            
        IF bfAddress.AddressType NE {&BILLING_ADDRESS} THEN 
            UNDO, THROW NEW ParamError("Billing Address update failed.").
            
        FIND FIRST CustomerReport WHERE CustomerReport.CustNum  = THIS-OBJECT:KeyValue NO-LOCK NO-ERROR.
           
        ASSIGN 
            THIS-OBJECT:AddrID            = iiBillAddrID
            THIS-OBJECT:AddrRegisterID    = (IF ioBillAddrObj:Has("address_id") THEN ioBillAddrObj:GetCharacter("address_id") ELSE bfAddress.AddressRegisterID )
            THIS-OBJECT:Street            = (IF ioBillAddrObj:Has("street") THEN ioBillAddrObj:GetCharacter("street") ELSE bfAddress.Address)
            THIS-OBJECT:StreetType        = (IF ioBillAddrObj:Has("street_type") THEN ioBillAddrObj:GetCharacter("street_type") ELSE bfAddress.StreetType)
            THIS-OBJECT:BuildingNum       = (IF ioBillAddrObj:Has("building_number") THEN ioBillAddrObj:GetCharacter("building_number") ELSE bfAddress.BuildingNum )
            THIS-OBJECT:BlockNum          = (IF ioBillAddrObj:Has("block") THEN ioBillAddrObj:GetCharacter("block") ELSE bfAddress.Block )
            THIS-OBJECT:Floor             = (IF ioBillAddrObj:Has("floor") THEN ioBillAddrObj:GetCharacter("floor") ELSE bfAddress.Floor )
            THIS-OBJECT:Stair             = (IF ioBillAddrObj:Has("stair") THEN ioBillAddrObj:GetCharacter("stair") ELSE bfAddress.Stair )
            THIS-OBJECT:Door              = (IF ioBillAddrObj:Has("door") THEN ioBillAddrObj:GetCharacter("door") ELSE  bfAddress.Door )
            THIS-OBJECT:Letter            = (IF ioBillAddrObj:Has("letter") THEN ioBillAddrObj:GetCharacter("letter") ELSE  bfAddress.Letter )
            THIS-OBJECT:KM                = (IF ioBillAddrObj:Has("km") THEN ioBillAddrObj:GetCharacter("km") ELSE bfAddress.km )
            THIS-OBJECT:Hand              = (IF ioBillAddrObj:Has("hand") THEN ioBillAddrObj:GetCharacter("hand") ELSE bfAddress.Hand )
            THIS-OBJECT:BisDuplicate      = (IF ioBillAddrObj:Has("bis_duplicate") THEN ioBillAddrObj:GetCharacter("bis_duplicate") ELSE bfAddress.bisduplicate  )
            THIS-OBJECT:AdditionalAddress = (IF ioBillAddrObj:Has("additional_address") THEN ioBillAddrObj:GetCharacter("additional_address") ELSE bfAddress.addresscompl  )
            /*THIS-OBJECT:NormalizedID      = (IF ioBillAddrObj:Has("normalizedid") THEN ioBillAddrObj:GetCharacter("normalizedid") ELSE bfAddress.normalizedid  )*/
            THIS-OBJECT:Latitude          = (IF ioBillAddrObj:Has("latitude") THEN ioBillAddrObj:GetCharacter("latitude") ELSE bfAddress.Latitude  )
            THIS-OBJECT:Longitude         = (IF ioBillAddrObj:Has("longitude") THEN ioBillAddrObj:GetCharacter("longitude") ELSE bfAddress.longitude  )
            THIS-OBJECT:City              = (IF ioBillAddrObj:Has("city") THEN ioBillAddrObj:GetCharacter("city") ELSE bfAddress.City  )
            THIS-OBJECT:Region            = (IF ioBillAddrObj:Has("region") THEN ioBillAddrObj:GetCharacter("region") ELSE bfAddress.Region  )
            THIS-OBJECT:ZIP               = (IF ioBillAddrObj:Has("zip") THEN ioBillAddrObj:GetCharacter("zip") ELSE bfAddress.zipCode  )
            THIS-OBJECT:Country           = (IF ioBillAddrObj:Has("country") THEN mGetCountry(ioBillAddrObj:GetCharacter("country")) ELSE bfAddress.Country )
            THIS-OBJECT:Gescal            = (IF ioBillAddrObj:Has("gescal") THEN ioBillAddrObj:GetCharacter("gescal") ELSE bfAddress.Gescal )
            THIS-OBJECT:CoverageToken     = (IF ioBillAddrObj:Has("coveragetoken") THEN ioBillAddrObj:GetCharacter("coveragetoken") ELSE bfAddress.CoverageToken )            
            .
        
        IF ioBillAddrObj:Has("validation_data") 
        AND AVAILABLE CustomerReport
        THEN DO:
            ASSIGN 
                loAddrValidationObj          = NEW JsonObject()
                loAddrValidationObj          = ioBillAddrObj:GetJsonObject("validation_data")
                THIS-OBJECT:CityCode         = (IF loAddrValidationObj:Has("city_code")   THEN  loAddrValidationObj:GetCharacter("city_code")   ELSE bfAddress.CityCode )
                THIS-OBJECT:StreetCode       = (IF loAddrValidationObj:Has("street_code") THEN  loAddrValidationObj:GetCharacter("street_code") ELSE bfAddress.StreetCode )
                THIS-OBJECT:MunicipalityCode = (IF loAddrValidationObj:Has("municipality_code") THEN  loAddrValidationObj:GetCharacter("municipality_code") ELSE bfAddress.towncode )
                .
        END. 
              
	END METHOD.

END CLASS.