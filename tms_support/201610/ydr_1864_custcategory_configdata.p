FUNCTION create_tmsparam RETURNS LOG(icParamCode  AS CHAR,
                                      icParamGroup AS CHAR,
                                      icParamType  AS CHAR,
                                      icParamName  AS CHAR,
                                      iiParamValue AS char):
    IF NOT CAN-FIND(FIRST TMSParam WHERE TMSParam.Brand = "1" AND TMSParam.ParamCode = icParamCode AND TMSParam.ParamGroup = icParamGroup NO-LOCK) THEN 
    DO:                                          
        CREATE TMSParam.
        ASSIGN
            TMSParam.Brand      = "1"
            TMSParam.ParamCode  = icParamCode
            TMSParam.ParamGroup = icParamGroup
            TMSParam.ParamType  = icParamType
            TMSParam.ParamName  = icParamName
            TMSParam.CharVal     = iiParamValue.
    END.        
    
    RETURN TRUE.
                  
END FUNCTION.

DO ON ERROR UNDO, THROW:
    
    create_tmsparam("CustIDTypeNoTransfer","IFS","C","Customer ID Types not transferred","CFraud,Fraud,CInternal,Internal").
    
    RUN pCreateCustCat("06", "CIF Internal Customer", 1, 0, "CInternal", 999, 999, FALSE).
    
    RUN pCreateCustCat("07", "CIF Fraud Customer", 1, 0, "CFraud", 999, 999, FALSE).
    
    RUN pCreateCustCat("08", "Internal Customer", 1, 0, "Internal", 999, 999, FALSE).
    
    RUN pCreateCustCat("09", "Fraud Customer"   , 1, 0, "Fraud"   , 999, 999, FALSE).
END.    

PROCEDURE pCreateCustCat:
    DEFINE INPUT PARAMETER icCustCat      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCatName      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiInterestType AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiPaymTerm     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER icCustIdType   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iiSubsLimit    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER iiActLimit     AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ilSelfEmployed AS LOGICAL   NO-UNDO.
    
    /* I want program to wait, since CustCat and TMSCodes are not transactional data */    
    FIND FIRST CustCat WHERE CustCat.Brand    = "1"  AND 
                             CustCat.Category = icCustCat EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE CustCat THEN 
    DO:                         
        CREATE CustCat.
        ASSIGN
            CustCat.Brand    = "1"
            CustCat.Category = icCustCat.
    END.
    
    RUN pCreateTMSCodes("Customer","CustIDType","CustCare",icCustIdType,icCatName,"").
    
    ASSIGN 
        CustCat.CatName         = icCatName 
        CustCat.IntType         = iiInterestType
        CustCat.PaymTerm        = iiPaymTerm
        CustCat.CustIDType      = icCustIdType
        CustCat.MobSubLimit     = iiSubsLimit
        CustCat.ActivationLimit = iiActLimit
        CustCat.SelfEmployed    = ilSelfEmployed.
    
END PROCEDURE.    

PROCEDURE pCreateTMSCodes:
    DEFINE INPUT PARAMETER icTableName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icFieldName AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCodeGroup AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCodeValue AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icCodeName  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icConfigVal AS CHARACTER NO-UNDO.
    
    FIND FIRST TMSCodes WHERE TMSCodes.TableName = icTableName AND 
                              TMSCodes.FieldName = icFieldName AND 
                              TMSCodes.CodeValue = icCodeValue EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE TMSCodes THEN 
    DO:                          
        CREATE TMSCodes.
        ASSIGN 
            TMSCodes.Tablename   = icTableName
            TMSCodes.FieldName   = icFieldName
            TMSCodes.CodeValue   = icCodeValue.
    END.
    
    ASSIGN 
        TMSCodes.CodeGroup   = icCodeGroup
        TMSCodes.CodeName    = icCodeName     
        TMSCodes.ConfigValue = icConfigVal
        TMSCodes.InUse       = 1.
    
END PROCEDURE.    