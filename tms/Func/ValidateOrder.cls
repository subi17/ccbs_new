 
 /*------------------------------------------------------------------------
    File        : ValidateOrder
    Purpose     : Order Saving and Processing Related Validations
    Syntax      : 
    Description : 
    Author(s)   : Koundinya Maddali
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


    METHOD PUBLIC STATIC CHARACTER mCheckMSISDN (INPUT lcCLI AS CHARACTER , INPUT lcNumberType AS CHARACTER) :
        
        DEF VAR lcError AS CHAR NO-UNDO INIT "".
        
        IF lcNumberType EQ "NEW"  
        THEN DO:

            IF Func.ValidateOrder:misFreeCLI(lcCLI) = FALSE 
            THEN lcError = SUBST("Cli &1 not found or not free", lcCLI).
            
        END.
        ELSE IF lcNumberType EQ "MNP" OR 
                lcNumberType EQ "MIGRATION"  
        THEN DO:
            
            IF Func.ValidateOrder:misSubscriptionExists(lcCLI) 
            THEN lcError = "Subscription already exists with MSISDN " + lcCLI.
            
        END.
        
        RETURN lcError.
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL mIsPrepaidOrder(INPUT lcCLIType AS CHARACTER) :
        
        DEFINE BUFFER bfpcCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfpcCLIType WHERE bfpcCLIType.Brand    = Syst.Var:gcBrand     
                                             AND bfpcCLIType.CLIType  = lcCLIType
                                             AND bfpcCLIType.PayType  = {&CLITYPE_PAYTYPE_PREPAID})).
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misFreeCLI(INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfpcMSISDN FOR MSISDN.
        
        RETURN (CAN-FIND(FIRST bfpcMSISDN WHERE bfpcMSISDN.Brand      EQ Syst.Var:gcBrand 
                                            AND bfpcMSISDN.ValidTo    GE {&nowts}
                                            AND bfpcMSISDN.CLI        EQ lcCLI
                                            AND bfpcMSISDN.statuscode EQ 1 )) .        
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misFilledFixedNumber(INPUT lcFixedNumber AS CHARACTER) :
        
        DEFINE BUFFER bfpcMobSub            FOR MobSub.
        DEFINE BUFFER bfpcOrderfusion       FOR OrderFusion.
        DEFINE BUFFER bfpcOrder             FOR Order.
        DEFINE BUFFER bfpcOrderProductParam FOR OrderProductParam.
        
        IF CAN-FIND(FIRST bfpcMobSub WHERE bfpcMobSub.Brand          =    Syst.Var:gcBrand
                                       AND bfpcMobSub.FixedNumber    =    lcFixedNumber
                                       AND (bfpcMobSub.MsStatus      =    {&MSSTATUS_ACTIVE}  OR
                                            bfpcMobSub.MsStatus      =    {&MSSTATUS_BARRED}))
        THEN RETURN TRUE.
        
        FIND FIRST bfpcOrderfusion WHERE bfpcOrderfusion.Brand         =    Syst.Var:gcBrand 
                                     AND bfpcOrderfusion.FixedNumber   =    lcFixedNumber
                                   NO-LOCK NO-ERROR.
        IF AVAILABLE bfpcOrderfusion
        THEN DO:
            
            IF CAN-FIND(FIRST bfpcOrder WHERE bfpcOrder.Brand   =   bfpcOrderfusion.Brand
                                          AND bfpcOrder.OrderID >   bfpcOrderfusion.OrderID
                                   AND LOOKUP(bfpcOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0)
            THEN RETURN TRUE.
            
        END.

        RETURN FALSE.        
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misSubscriptionExists(INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfpcMobSub FOR MobSub.
        
        RETURN (CAN-FIND(FIRST bfpcMobSub WHERE bfpcMobSub.Brand  = Syst.Var:gcBrand
                                            AND bfpcMobSub.CLI    = lcCLI)).
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misCLITypeExists(INPUT lcCLIType AS CHARACTER) :
        
        DEFINE BUFFER bfpcCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfpcCLIType WHERE bfpcCLIType.Brand   = Syst.Var:gcBrand 
                                             AND bfpcCLIType.CliType = lcCLIType)).
        
    END METHOD.
    
    
    METHOD PUBLIC STATIC LOGICAL mIsConvergentORFixedOnly(INPUT icCLIType AS CHARACTER) :
                               
       RETURN (Func.ValidateOrder:mIsConvergentTariff(icCLIType) OR
               Func.ValidateOrder:mIsFixedOnlyTariff(icCLIType)).
      
    END.
    
    METHOD PUBLIC STATIC LOGICAL mIsConvergentTariff(INPUT icCLIType AS CHARACTER):
        
        DEFINE BUFFER bfpcCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfpcCLIType NO-LOCK WHERE
                               bfpcCLIType.Brand      EQ Syst.Var:gcBrand AND
                               bfpcCLIType.CliType    EQ icCLIType        AND 
                               bfpcCLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_CONVERGENT})) .
        
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL mIsFixedOnlyTariff(INPUT icCLIType AS CHARACTER) :
        
        /* logic is based on the fIsFixedOnly fixedlinefunc.i*/
        
        DEFINE BUFFER bfpcCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfpcCLIType NO-LOCK WHERE
                               bfpcCLIType.Brand      = Syst.Var:gcBrand           AND
                               bfpcCLIType.CLIType    = icCLIType                         AND
                               bfpcCLIType.TariffType = {&CLITYPE_TARIFFTYPE_FIXEDONLY})).
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misValidICC(INPUT icICC AS CHARACTER):
        
        DEFINE BUFFER bfpcSIM FOR SIM.
        
        RETURN (CAN-FIND(FIRST bfpcSIM WHERE bfpcSIM.ICC = icICC)).
        
    END METHOD.    
    
    METHOD PUBLIC STATIC LOGICAL misFreeICC(INPUT icICC AS CHARACTER) :
        
        /*Avoided the SIM Type validation as we are going to use the universal sim*/
        
        DEFINE BUFFER bfpcSIM FOR SIM.
        
        RETURN (CAN-FIND(FIRST bfpcSIM WHERE bfpcSIM.brand   EQ Syst.Var:gcBrand 
                                         AND bfpcSIM.ICC     EQ icICC
                                         AND bfpcSIM.simstat EQ 1 )).
        
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL mIsBarredSubscriptionExistsToCustomer( INPUT liCustNum AS INTEGER):
        
        DEFINE BUFFER bfpcMobSub FOR MobSub.
        
        FOR EACH bfpcMobSub NO-LOCK WHERE 
                 bfpcMobSub.Brand     = Syst.Var:gcBrand AND 
                 bfpcMobSub.CustNum   = liCustNum AND 
                 bfpcMobSub.MsStatus  = {&MSSTATUS_BARRED} : 
                     
            IF Func.BarrMethod:mSubsHaveActiveBarring(bfpcMobSub.MsSeq,{&FRAUD_BARR_CODES})
            THEN RETURN TRUE.
            
        END.
        
        RETURN FALSE.        
        
    END.
    
    METHOD PUBLIC STATIC INTEGER mGetMobSubCustNum (INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfpcMobSub FOR MobSub.
        
        FIND FIRST bfpcMobSub WHERE bfpcMobSub.Brand  =  Syst.Var:gcBrand
                                AND bfpcMobSub.CLI    =  lcCLI
                              NO-LOCK NO-ERROR.
                              
        IF AVAILABLE bfpcMobSub THEN RETURN bfpcMobSub.CustNum.
        
        RETURN 0.                              
        
        
    END METHOD.
    
    METHOD PUBLIC STATIC LOGICAL misCustomerExists(INPUT lcCustID     AS CHARACTER ,
                                                   INPUT lcCustIDType AS CHARACTER ,
                                                   INPUT liCustNum    AS INTEGER) :
                                                       
        DEFINE BUFFER bfpcCustomer FOR Customer.                                                       
                                                       
        RETURN (CAN-FIND(FIRST bfpcCustomer WHERE
                               bfpcCustomer.Brand       =   Syst.Var:gcBrand AND
                               bfpcCustomer.OrgId       =   lcCustID         AND
                               bfpcCustomer.CustidType  =   lcCustIDType     AND
                               bfpcCustomer.Custnum     =   liCustNum)).                                                     
                                                       
    END METHOD.       
    
    METHOD PUBLIC STATIC LOGICAL misDuplicateContractExists(INPUT lcContractID AS CHARACTER ,
                                                            INPUT lcCLI        AS CHARACTER) :
                                                                
        DEFINE BUFFER bfOrder FOR Order.                                                                

        RETURN (CAN-FIND(FIRST bfOrder WHERE bfOrder.Brand       =   Syst.Var:gcBrand AND 
                                             bfOrder.ContractID  =   lcContractID     AND
                                             bfOrder.CLI         <>  lcCLI   )) .
                                                                
    END METHOD.     
    
    METHOD PUBLIC STATIC CHARACTER mGetSubscriptionFixedNumber(INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfpcMobSub FOR MobSub.
        
        FIND FIRST bfpcMobSub WHERE bfpcMobSub.Brand = Syst.Var:gcBrand
                                AND bfpcMobSub.CLI   = lcCLI
                                NO-LOCK NO-ERROR.
                                
        IF NOT AVAILABLE bfpcMobSub THEN RETURN "".
        
        RETURN bfpcMobSub.FixedNumber.
        
        
    END METHOD.      
    
    METHOD PUBLIC STATIC LOGICAL mCheckOngoingFixedOrders (INPUT lcFixedNumber AS CHARACTER ,
                                                           INPUT lcFixNumType  AS CHARACTER) :
                                                          
        DEF BUFFER bfpcOrder       FOR Order.
        DEF BUFFER bfpcOrderFusion FOR OrderFusion.

        FOR EACH bfpcOrderFusion NO-LOCK WHERE
                 bfpcOrderFusion.FixedNumber EQ lcFixedNumber :
                     
            FOR EACH  bfpcOrder NO-LOCK WHERE
                      bfpcOrder.brand       EQ      Syst.Var:gcBrand        AND
                      bfpcOrder.OrderId     EQ      bfpcOrderFusion.OrderId AND
                      LOOKUP(bfpcOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0:
   
                IF (lcFixNumType EQ "renewal" OR lcFixNumType EQ "retention") AND 
                bfpcOrder.OrderType EQ {&ORDER_TYPE_STC} THEN NEXT.

                RETURN TRUE.
                
            END. /* FOR EACH  bfpcOrder */
            
        END. /* FOR EACH bfpcOrderFusion */

        RETURN FALSE.       
                                                          
    END METHOD.       
    
    METHOD PUBLIC STATIC LOGICAL mIsMigrationCLI(INPUT lcCLI AS CHARACTER) :
        
        DEFINE BUFFER bfOrder FOR Order.
        
        RETURN (CAN-FIND(FIRST bfOrder WHERE
                               bfOrder.CLI        EQ lcCLI AND
                              (bfOrder.StatusCode EQ {&ORDER_STATUS_MIGRATION_PENDING} OR 
                               bfOrder.StatusCode EQ {&ORDER_STATUS_MIGRATION_ONGOING}))).
        
        
    END METHOD.   
    
    METHOD PUBLIC STATIC INTEGER mGetMsSeq(INPUT lcCLI AS CHARACTER):
        
        DEFINE BUFFER bfpcMobSub FOR MobSub.
        
        FIND FIRST bfpcMobSub WHERE bfpcMobSub.Brand   =  Syst.Var:gcBrand
                                AND bfpcMobSub.CLI     =  lcCLI 
                              NO-LOCK NO-ERROR.
                            
        IF AVAILABLE bfpcMobSub THEN RETURN bfpcMobSub.MSSeq.
        
        RETURN 0.
        
    END METHOD.     
    
    METHOD PUBLIC STATIC LOGICAL mCheckOtherOngoingOrders(INPUT lcCLI        AS CHARACTER ,
                                                          INPUT lcNumberType AS CHARACTER):
                                                              
        DEFINE BUFFER bfDuplicateOrder FOR Order.
        
        DEF VAR liExcludeOrderType AS INT NO-UNDO. 

        IF lcNumberType EQ "stc" 
        THEN liExcludeOrderType = {&ORDER_TYPE_RENEWAL}.
        ELSE IF lcNumberType EQ "renewal" OR lcNumberType EQ "retention" 
        THEN liExcludeOrderType = {&ORDER_TYPE_STC}.
        ELSE liExcludeOrderType = -1.
   
        FOR EACH bfDuplicateOrder NO-LOCK WHERE
                 bfDuplicateOrder.brand     EQ Syst.Var:gcBrand               AND
                 bfDuplicateOrder.CLI       EQ lcCLI                          AND
          LOOKUP(bfDuplicateOrder.statuscode,{&ORDER_INACTIVE_STATUSES}) EQ 0 AND
                 bfDuplicateOrder.OrderType NE liExcludeOrderType             AND
                 bfDuplicateOrder.OrderType <= {&ORDER_TYPE_STC}:
           
            IF lcNumberType EQ "retention" AND
               bfDuplicateOrder.StatusCode = {&ORDER_STATUS_OFFER_SENT} 
            THEN NEXT. 
            
            RETURN TRUE.
            
        END.
   
        RETURN FALSE.
                                                              
    END METHOD.        
    
    METHOD PUBLIC STATIC INTEGER mGetPayType (INPUT lcCLIType AS CHARACTER) :
        
        DEFINE BUFFER bfpcCLIType FOR CLIType.
        
        FIND FIRST bfpcCLIType WHERE bfpcCLIType.Brand    =   Syst.Var:gcBrand
                                 AND bfpcCLIType.CLIType  =   lcCLIType NO-LOCK NO-ERROR.
                                 
        IF AVAILABLE bfpcCLIType THEN 
            RETURN bfpcCLIType.PayType.
                                         
        
    END METHOD.           
    
    METHOD PUBLIC STATIC LOGICAL mIsFiberType(INPUT lcCLIType AS CHARACTER) :
        
        DEFINE BUFFER bfpcCLIType FOR CLIType.
        
        RETURN (CAN-FIND(FIRST bfpcCLIType WHERE bfpcCLIType.Brand          =   Syst.Var:gcBrand  
                                             AND bfpcCLIType.CLIType        =   lcCLIType
                                             AND bfpcCLIType.FixedLineType  =   {&FIXEDLINE_TYPE_FIBER})).
        
    END METHOD.       
    
END CLASS.