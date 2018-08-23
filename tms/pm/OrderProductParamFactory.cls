 
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

CLASS pm.OrderProductParamFactory IMPLEMENTS bss.cls.IObjectStorage :
    
    
    DEFINE PRIVATE TEMP-TABLE ttOrderParamFactory NO-UNDO
        FIELD opobject AS CLASS  Progress.Lang.Object
        FIELD ordernb  AS INTEGER
        INDEX ordernb IS PRIMARY UNIQUE ordernb
        . 
    
    DEFINE PUBLIC PROPERTY objOrderProduct AS CLASS pm.OrderProduct NO-UNDO 
        GET.
        SET.
    
    CONSTRUCTOR PUBLIC OrderProductParamFactory(iObjOrderProduct AS CLASS pm.OrderProduct):
        ASSIGN 
            objOrderProduct = iObjOrderProduct.
    END CONSTRUCTOR.    
    
    DESTRUCTOR PUBLIC OrderProductParamFactory():
        
        FOR EACH ttOrderParamFactory :
            IF VALID-OBJECT(ttOrderParamFactory.opobject) THEN 
                DELETE OBJECT ttOrderParamFactory.opobject.
            DELETE ttOrderParamFactory.
        END.
        
    END DESTRUCTOR.
    
    METHOD PUBLIC VOID mCreateData(iObjParamStruct AS CLASS JsonObject ) :
        
        DEFINE VARIABLE objOrderProductParam AS CLASS pm.OrderProductParam NO-UNDO.
        
        IF iObjParamStruct:Has("fixedline_number_type") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("fixedline_number_type"),
                                            "fixedline_number_type",
                                            {&CHARACTER_PARAM}). 
                                     
        IF iObjParamStruct:Has("fixedline_number") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("fixedline_number"),
                                     "fixedline_number",
                                     {&CHARACTER_PARAM}). 
                                     
        IF iObjParamStruct:Has("estimated_speed") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("estimated_speed"),
                                     "estimated_speed",
                                     {&CHARACTER_PARAM}).   
                                     
        IF iObjParamStruct:Has("serial_number") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("serial_number"),
                                     "serial_number",
                                     {&CHARACTER_PARAM}).  
        
        IF iObjParamStruct:Has("fixedline_old_operator_code") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("fixedline_old_operator_code"),
                                     "fixedline_old_operator_code",
                                     {&CHARACTER_PARAM}).        
            
        IF iObjParamStruct:Has("fixedline_old_operator_name") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("fixedline_old_operator_name"),
                                     "fixedline_old_operator_name",
                                     {&CHARACTER_PARAM}).
                                                                   
        
        IF iObjParamStruct:Has("mobile_number_type") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mobile_number_type"),
                                     "mobile_number_type",
                                     {&CHARACTER_PARAM}).  
                                     
        IF iObjParamStruct:Has("mobile_number") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mobile_number"),
                                     "mobile_number",
                                     {&CHARACTER_PARAM}).  
                                             
        IF iObjParamStruct:Has("mnptime") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mnptime"),
                                    "mnptime",
                                    {&CHARACTER_PARAM}).
                                             
        IF iObjParamStruct:Has("icc") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("icc"),
                                     "icc",
                                     {&CHARACTER_PARAM}).  
                                     
        IF iObjParamStruct:Has("mobile_old_operator_code") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mobile_old_operator_code"),
                                     "mobile_old_operator_code",
                                     {&CHARACTER_PARAM}).  
                                     
        IF iObjParamStruct:Has("mobile_old_operator_name") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mobile_old_operator_name"),
                                     "mobile_old_operator_name",
                                     {&CHARACTER_PARAM}). 
                                     
        IF iObjParamStruct:Has("mobile_old_product_type") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mobile_old_product_type"),
                                     "mobile_old_product_type",
                                     {&CHARACTER_PARAM}). 
                                     
        IF iObjParamStruct:Has("mobile_old_icc") THEN 
            mCreateOrderProductParam(iObjParamStruct:GetCharacter("mobile_old_icc"),
                                     "mobile_old_icc",
                                      {&CHARACTER_PARAM}). 
                                     
        IF iObjParamStruct:Has("initial_topup_amount") THEN 
            mCreateOrderProductParam(STRING(iObjParamStruct:GetDecimal("initial_topup_amount")),
                                     "initial_topup_amount",
                                     {&DECIMAL_PARAM}).
                                     
        IF iObjParamStruct:Has("initial_topup_display_amount") THEN 
            mCreateOrderProductParam(STRING(iObjParamStruct:GetDecimal("initial_topup_display_amount")),
                                     "initial_topup_display_amount",
                                     {&DECIMAL_PARAM}).     
         
                     
    END METHOD.
    
    METHOD PUBLIC VOID mStoreData():
        
        FOR EACH ttOrderParamFactory :            
            CAST(ttOrderParamFactory.opobject, pm.OrderProductParam):mStoreData().            
        END. 
        
    END METHOD.
    
    METHOD PUBLIC INTEGER mGetNextSequence():
        
        FIND LAST ttOrderParamFactory NO-ERROR.
        
        IF NOT AVAILABLE ttOrderParamFactory THEN 
            RETURN 1.
            
        RETURN (ttOrderParamFactory.ordernb + 1).
        
    END METHOD.
    
    METHOD PUBLIC VOID mCreateOrderProductParam(INPUT icPropertyValue AS CHARACTER,
                                                INPUT icPropertyName  AS CHARACTER,
                                                INPUT icPropertyType  AS CHARACTER):
        
        DEFINE VARIABLE objOrderProductParam AS CLASS pm.OrderProductParam NO-UNDO.
        DEFINE VARIABLE liSequence AS INTEGER NO-UNDO.
        
        ASSIGN 
            objOrderProductParam = NEW pm.OrderProductParam(objOrderProduct)
            liSequence           = mGetNextSequence().
            
        objOrderProductParam:mCreateData(icPropertyValue,icPropertyName,icPropertyType).
        
        CREATE ttOrderParamFactory.
        ASSIGN 
            ttOrderParamFactory.opobject = objOrderProductParam
            ttOrderParamFactory.ordernb  = liSequence.
        
    END METHOD.
    
    
END CLASS.     