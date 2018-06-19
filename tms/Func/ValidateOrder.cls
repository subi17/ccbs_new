 
 /*------------------------------------------------------------------------
    File        : ValidateOrder
    Purpose     : Order Related Validations
    Syntax      : 
    Description : 
    Author(s)   : kmaddali
    Created     : Mon Jun 04 19:18:22 IST 2018
    Notes       : 
  ----------------------------------------------------------------------*/


USING Progress.Lang.*.

BLOCK-LEVEL ON ERROR UNDO, THROW.

&GLOBAL-DEFINE nowTS (YEAR(TODAY) * 10000 + MONTH(TODAY) * 100 + DAY(TODAY) + TIME / 100000)
{Syst/tmsconst.i}

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Func.ValidateOrder FINAL: 
    
    
    CONSTRUCTOR PRIVATE ValidateOrder (  ):
        
    END CONSTRUCTOR.


    METHOD PUBLIC STATIC CHARACTER fCheckMSISDN (INPUT lcCLI AS CHARACTER , INPUT lcNumberType AS CHARACTER) :
        
        DEF VAR lcError AS CHAR NO-UNDO INIT "".
        
        DEFINE BUFFER bfMSISDN FOR MSISDN.
        DEFINE BUFFER bfMobSub FOR MobSub.

        IF lcNumberType EQ "new"  
        THEN DO:

            FIND FIRST bfMSISDN NO-LOCK
                WHERE bfMSISDN.Brand EQ Syst.Var:gcBrand 
                AND bfMSISDN.ValidTo    GE {&nowts}
                AND bfMSISDN.CLI        EQ lcCLI
                AND bfMSISDN.statuscode EQ 1 
                NO-ERROR.

            IF NOT AVAILABLE bfMSISDN THEN
                lcError = SUBST("Cli &1 not found or not free", lcCLI).
        END.
        ELSE IF lcNumberType EQ "MNP" OR 
                lcNumberType EQ "migration"  
        THEN DO:
            
            IF Func.ValidateOrder:misSubscriptionExists(lcCLI) 
            THEN lcError = "Subscription already exists with MSISDN " + lcCLI.
            
        END.
        
        RETURN lcError.
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL mIsPostpaidOrder(INPUT lcCLIType AS CHARACTER) :
        
        DEFINE BUFFER bfCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfCLIType WHERE bfCLIType.Brand    = Syst.Var:gcBrand     
                                           AND bfCLIType.CLIType  = lcCLIType
                                           AND bfCLIType.PayType  = {&CLITYPE_PAYTYPE_POSTPAID})).
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misFilledCLI(INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfOrder FOR Order.
        
        IF Func.ValidateOrder:misSubscriptionExists(lcCLI) 
        THEN RETURN YES.   
             
        RETURN (CAN-FIND(FIRST bfOrder WHERE bfOrder.Brand  =  Syst.Var:gcBrand
                                         AND bfOrder.CLI    =  lcCLI
                                         AND LOOKUP(bfOrder.StatusCode , {&ORDER_INACTIVE_STATUSES}) = 0 )).
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misSubscriptionExists(INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfMobSub FOR MobSub.
        
        RETURN (CAN-FIND(FIRST bfMobSub WHERE bfMobSub.Brand  = Syst.Var:gcBrand
                                          AND bfMobSub.CLI    = lcCLI)).
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misCLITypeExists(INPUT lcCLIType AS CHARACTER) :
        
        DEFINE BUFFER bfCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfCLIType WHERE bfCLIType.Brand = Syst.Var:gcBrand AND bfCLIType.CliType = lcCLIType)).
        
    END METHOD.
    
    
    

END CLASS.