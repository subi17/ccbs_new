 
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

CLASS om.Address IMPLEMENTS bss.cls.IObjectStorage: 
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	
	DEFINE PUBLIC PROPERTY addrId AS INTEGER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY keyValue AS INTEGER NO-UNDO 
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY addrType AS INTEGER NO-UNDO
        GET.
        SET.
       
    DEFINE PUBLIC PROPERTY street AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY streetType AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY buildingNum AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY blockNum AS CHARACTER NO-UNDO
       GET.
       SET.
		
	DEFINE PUBLIC PROPERTY floor AS CHARACTER NO-UNDO
       GET.
       SET.	
       
    DEFINE PUBLIC PROPERTY stair AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY door AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY letter AS CHARACTER NO-UNDO
       GET.
       SET.   
       
    DEFINE PUBLIC PROPERTY km AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY hand AS CHARACTER NO-UNDO
       GET.
       SET.
       
     DEFINE PUBLIC PROPERTY bisDuplicate AS CHARACTER NO-UNDO
        GET.
        SET.  
        
     DEFINE PUBLIC PROPERTY additionalAddress AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY normalizedID AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY latitude AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY longitude AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY city AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY region AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY zip AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY country AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY cityCode AS CHARACTER NO-UNDO
        GET.
        SET.
		
	 DEFINE PUBLIC PROPERTY streetCode AS CHARACTER NO-UNDO
        GET.
        SET.
        
     DEFINE PUBLIC PROPERTY municipalityCode AS CHARACTER NO-UNDO
        GET.
        SET.
        
    DEFINE PUBLIC PROPERTY gescal AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY coverageToken AS CHARACTER NO-UNDO
       GET.
       SET.
       
    DEFINE PUBLIC PROPERTY addrRegisterID AS CHARACTER NO-UNDO
       GET.
       SET.   
    
		
	CONSTRUCTOR PUBLIC Address ( ):
		 
	END CONSTRUCTOR.

		
	METHOD PUBLIC VOID mCreateData( iObjAddressStruct AS CLASS JsonObject):
	    
	    DEFINE VARIABLE objAddrValidationStruct AS CLASS JsonObject NO-UNDO.
	    
        ASSIGN 
            THIS-OBJECT:addrId            = NEXT-VALUE(AddrID)
            THIS-OBJECT:addrRegisterID    = iObjAddressStruct:GetCharacter("address_id")
            THIS-OBJECT:street            = iObjAddressStruct:GetCharacter("street")
            THIS-OBJECT:streetType        = iObjAddressStruct:GetCharacter("street_type") WHEN iObjAddressStruct:Has("street_type")
            THIS-OBJECT:buildingNum       = iObjAddressStruct:GetCharacter("building_number")
            THIS-OBJECT:blockNum          = iObjAddressStruct:GetCharacter("block")  WHEN iObjAddressStruct:Has("block")
            THIS-OBJECT:floor             = iObjAddressStruct:GetCharacter("floor")
            THIS-OBJECT:stair             = iObjAddressStruct:GetCharacter("stair")  WHEN iObjAddressStruct:Has("stair")
            THIS-OBJECT:door              = iObjAddressStruct:GetCharacter("door")   WHEN iObjAddressStruct:Has("door")
            THIS-OBJECT:letter            = iObjAddressStruct:GetCharacter("letter") WHEN iObjAddressStruct:Has("letter")
            THIS-OBJECT:km                = iObjAddressStruct:GetCharacter("km")     WHEN iObjAddressStruct:Has("km")
            THIS-OBJECT:hand              = iObjAddressStruct:GetCharacter("hand")   WHEN iObjAddressStruct:Has("hand")
            THIS-OBJECT:bisDuplicate      = iObjAddressStruct:GetCharacter("bis_duplicate")  WHEN iObjAddressStruct:Has("bis_duplicate")
            THIS-OBJECT:additionalAddress = iObjAddressStruct:GetCharacter("additional_address") WHEN iObjAddressStruct:Has("additional_address")
            THIS-OBJECT:normalizedID      = iObjAddressStruct:GetCharacter("normalizedid") WHEN iObjAddressStruct:Has("normalizedid")
            THIS-OBJECT:latitude          = iObjAddressStruct:GetCharacter("latitude")
            THIS-OBJECT:longitude         = iObjAddressStruct:GetCharacter("longitude")
            THIS-OBJECT:city              = iObjAddressStruct:GetCharacter("city")
            THIS-OBJECT:region            = iObjAddressStruct:GetCharacter("region")
            THIS-OBJECT:zip               = iObjAddressStruct:GetCharacter("zip")
            THIS-OBJECT:country           = mGetCountry(iObjAddressStruct:GetCharacter("country"))
            THIS-OBJECT:gescal            = iObjAddressStruct:GetCharacter("gescal") WHEN iObjAddressStruct:Has("gescal")
            THIS-OBJECT:coverageToken     = iObjAddressStruct:GetCharacter("coveragetoken") WHEN iObjAddressStruct:Has("coveragetoken")            
        .
	    
	    IF iObjAddressStruct:Has("validation_data")THEN 
	    DO:
	        
	        ASSIGN objAddrValidationStruct      =   NEW JsonObject()
	               objAddrValidationStruct      =   iObjAddressStruct:GetJsonObject("validation_data")
                   THIS-OBJECT:cityCode         =   objAddrValidationStruct:GetCharacter("city_code")
                   THIS-OBJECT:streetCode       =   objAddrValidationStruct:GetCharacter("street_code")
                   THIS-OBJECT:municipalityCode =   objAddrValidationStruct:GetCharacter("municipality_code")
                   .
	    END. 
	    ELSE UNDO, THROW NEW ParamError("Validation Data missing for the AddressID " + STRING(THIS-OBJECT:addrId)).      
	    
	END.
	
	METHOD PUBLIC VOID mStoreData():
	    
	    FIND FIRST Address WHERE Address.AddressID = THIS-OBJECT:addrId EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
	    
	    IF LOCKED(Address) THEN 
            UNDO, THROW NEW ParamError("Address record is locked.").
	    
	    IF NOT AVAILABLE Address THEN CREATE Address.
	    
        ASSIGN 
            Address.AddressID         = THIS-OBJECT:addrId 
            Address.Address           = THIS-OBJECT:street
            Address.StreetType        = THIS-OBJECT:streetType       
            Address.BuildingNum       = THIS-OBJECT:buildingNum      
            Address.Block             = THIS-OBJECT:blockNum         
            Address.Floor             = THIS-OBJECT:floor            
            Address.Stair             = THIS-OBJECT:stair            
            Address.Door              = THIS-OBJECT:door             
            Address.Letter            = THIS-OBJECT:letter           
            Address.km                = THIS-OBJECT:km               
            Address.Hand              = THIS-OBJECT:hand             
            Address.bisduplicate      = THIS-OBJECT:bisDuplicate     
            Address.addresscompl      = THIS-OBJECT:additionalAddress
            /*Address.normalizedid   = THIS-OBJECT:normalizedID*/     
            Address.Latitude          = THIS-OBJECT:latitude         
            Address.longitude         = THIS-OBJECT:longitude        
            Address.City              = THIS-OBJECT:city             
            Address.Region            = THIS-OBJECT:region           
            Address.zipCode           = THIS-OBJECT:zip              
            Address.Country           = THIS-OBJECT:country
            Address.CityCode          = THIS-OBJECT:cityCode
            Address.StreetCode        = THIS-OBJECT:streetCode
            Address.towncode          = THIS-OBJECT:municipalityCode
            Address.HostTable         = "Customer"
            Address.KeyValue          = STRING(THIS-OBJECT:keyValue)
            Address.AddressType       = THIS-OBJECT:addrType
            Address.Gescal            = THIS-OBJECT:gescal
            Address.CoverageToken     = THIS-OBJECT:coverageToken
            Address.AddressRegisterID = THIS-OBJECT:addrRegisterID
            .    
         IF NOT CAN-FIND(FIRST CustomerReport WHERE CustomerReport.CustNum = INTEGER(Address.KeyValue))THEN 
         DO:
             
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
	    
	    ioAddrObject:Add("address-id" , THIS-OBJECT:addrRegisterID).
	    
	    RETURN ioAddrObject.
	    
	END METHOD.
	
	DESTRUCTOR PUBLIC Address ( ):

    END DESTRUCTOR.

END CLASS.