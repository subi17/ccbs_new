&IF "{&EXTRALINEFUNC_I}" NE "YES"
&THEN

&GLOBAL-DEFINE EXTRALINEFUNC_I YES

{Syst/tmsconst.i}
{Mc/dpmember.i}
{Func/cparam2.i}
{Func/profunc.i}

/* Returns comma delimited character list of extraline clitypes (tariffs) */ 
FUNCTION fExtraLineCLITypes RETURNS CHARACTER:

   DEF VAR lcReturnValue AS CHAR NO-UNDO.
   DEF VAR liCount       AS INT  NO-UNDO. 

   FOR EACH TMSRelation NO-LOCK WHERE 
            TMSRelation.TableName     EQ {&ELTABLENAME} AND 
            TMSRelation.KeyType       EQ {&ELKEYTYPE}   BREAK BY TMSRelation.ChildValue: 
   
      IF LAST-OF(TMSRelation.ChildValue) THEN DO:
         
         liCount = INT(TMSRelation.RelationType) NO-ERROR.

         IF liCount EQ 0 THEN NEXT.
         
         IF lcReturnValue EQ "" THEN 
            lcReturnValue = TMSRelation.ChildValue.
         ELSE    
            lcReturnValue = lcReturnValue + "," + TMSRelation.ChildValue.

         liCount = 0.

      END.

   END.

   RETURN LEFT-TRIM(lcReturnValue, ",").

END FUNCTION.

/* Check if a mainline clitype is allowed for an extraline clitype  */ 
FUNCTION fCLITypeAllowedForExtraLine RETURNS LOGICAL
   (icMainLineCLIType  AS CHARACTER,
    icExtraLineCLIType AS CHARACTER,
    OUTPUT oiExtraLineCount AS INT):

   DEFINE BUFFER TMSRelation FOR TMSRelation.

   FIND FIRST TMSRelation NO-LOCK WHERE 
              TMSRelation.TableName     EQ {&ELTABLENAME}     AND 
              TMSRelation.KeyType       EQ {&ELKEYTYPE}       AND 
              TMSRelation.ChildValue    EQ icExtraLineCLIType AND 
              TMSRelation.ParentValue   EQ icMainLineCLIType  AND 
          INT(TMSRelation.RelationType) GT 0                  NO-ERROR. 

   IF AVAIL TMSRelation THEN DO:
      oiExtraLineCount = INT(TMSRelation.RelationType).
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.


/* Check which extraline clitype a mainline clitype is using.
   If the given clitype is not mainline clitype the function returns
   empty string */ 
FUNCTION fExtraLineForMainLine RETURNS CHARACTER
   (icMainLineCLIType  AS CHARACTER):

   DEFINE BUFFER TMSRelation FOR TMSRelation.

   /* Check for the mandatory extraline for mainline */
   FIND FIRST TMSRelation NO-LOCK WHERE 
              TMSRelation.TableName     EQ {&ELTABLENAME}     AND 
              TMSRelation.KeyType       EQ {&ELKEYTYPE}       AND 
              TMSRelation.ParentValue   EQ icMainLineCLIType  AND 
              TMSRelation.RelationType  EQ {&ELMANDATORY}     NO-ERROR.
   
   IF AVAIL TMSRelation THEN 
      RETURN ENTRY(1,TMSRelation.ChildValue,"_").
   ELSE DO:
      FIND FIRST TMSRelation NO-LOCK WHERE 
                 TMSRelation.TableName     EQ {&ELTABLENAME}     AND 
                 TMSRelation.KeyType       EQ {&ELKEYTYPE}       AND 
                 TMSRelation.ParentValue   EQ icMainLineCLIType  AND 
             INT(TMSRelation.RelationType) GT 0                  NO-ERROR.

      IF AVAIL TMSRelation THEN
         RETURN TMSRelation.ChildValue. 
   END.
   
   RETURN "".

END FUNCTION.

/* Check if the clitype is extraline clitype */
FUNCTION fCLITypeIsExtraLine RETURNS LOGICAL
   (icExtraLineCLIType AS CHARACTER):

   IF CAN-FIND(FIRST TMSRelation NO-LOCK WHERE 
                     TMSRelation.TableName     EQ {&ELTABLENAME}     AND 
                     TMSRelation.KeyType       EQ {&ELKEYTYPE}       AND                    
                     TMSRelation.ChildValue    EQ icExtraLineCLIType AND 
                 INT(TMSRelation.RelationType) GT 0)                 THEN 
      RETURN TRUE.
   ELSE 
      RETURN FALSE.

END.
/* Check if the clitype is mainline clitype
   (i.e. it is part of any of extraline clitype mainline list) */ 
FUNCTION fCLITypeIsMainLine RETURNS LOGICAL
   (icCLIType  AS CHARACTER):

   DEFINE BUFFER TMSRelation FOR TMSRelation.

   IF CAN-FIND(FIRST TMSRelation NO-LOCK WHERE 
                     TMSRelation.TableName     EQ {&ELTABLENAME} AND 
                     TMSRelation.KeyType       EQ {&ELKEYTYPE}   AND 
                     TMSRelation.ParentValue   EQ icCLIType      AND 
                 INT(TMSRelation.RelationType) GT 0)             THEN 
      RETURN TRUE.
  ELSE  
      RETURN FALSE.

END FUNCTION.

FUNCTION fCheckProMainlineForExtraLine RETURNS LOGICAL
   (icCLIType AS CHARACTER):

   DEFINE BUFFER TMSRelation FOR TMSRelation.

   IF CAN-FIND(FIRST TMSRelation NO-LOCK WHERE
                     TMSRelation.TableName   EQ {&ELTABLENAME} AND
                     TMSRelation.KeyType     EQ {&ELKEYTYPE}   AND
                     TMSRelation.ParentValue EQ icCLIType      AND
                     TMSRelation.ChildValue  EQ {&ELPRO})      THEN
      RETURN TRUE.
   ELSE
      RETURN FALSE.

END FUNCTION.

/* Function checks if STC is possible when the new clitype is
   extraline. It is possible when the customer has a free
   mainline and the mainline is suitable for the extraline */
FUNCTION fValidateExtraLineSTC RETURNS LOGICAL
   (iiCustNum    AS INTEGER,
    icNewCLIType AS CHARACTER):

   DEF VAR liELCount AS INT NO-UNDO. 

   DEFINE BUFFER MobSub FOR MobSub.

   /* Find suitable mainline mobsub from the customer */
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.Brand       EQ Syst.Var:gcBrand       AND
            MobSub.CustNum     EQ iiCustNum              AND
           (MobSub.MsStatus    EQ {&MSSTATUS_ACTIVE}  OR
            MobSub.MsStatus    EQ {&MSSTATUS_BARRED}):

      IF NOT fCLITypeIsMainLine(MobSub.CLIType) THEN NEXT.

      IF NOT fCLITypeAllowedForExtraLine(MobSub.CLIType, icNewCLIType,
                                         OUTPUT liELCount)
         THEN NEXT.
      ELSE 
         RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckForMandatoryExtraLine RETURNS LOGICAL
   (INPUT iiMultiSimId  AS INT,
    INPUT iiCustNum     AS INT,
    INPUT icMainCLIType AS CHAR,
    INPUT icELCLIType   AS CHAR,
    INPUT llgExisting   AS LOG):

   DEFINE BUFFER TMSRelation FOR TMSRelation.
   DEFINE BUFFER MobSub      FOR MobSub.
   DEFINE BUFFER Order       FOR Order.

   FOR EACH TMSRelation NO-LOCK WHERE 
            TMSRelation.TableName       EQ {&ELTABLENAME} AND 
            TMSRelation.KeyType         EQ {&ELKEYTYPE}   AND 
            TMSRelation.ParentValue     EQ icMainCLIType  AND 
            TMSRelation.RelationType    EQ {&ELMANDATORY}:
   
      IF NUM-ENTRIES(TMSRelation.ChildValue,"_") < 3 THEN NEXT.

      IF ENTRY(3,TMSRelation.ChildValue,"_") NE icELCLIType THEN NEXT.
      
      IF llgExisting THEN DO: 
         IF CAN-FIND(MobSub NO-LOCK USE-INDEX CustNum WHERE
                     MobSub.Brand        EQ Syst.Var:gcBrand                    AND
                     MobSub.CLIType      EQ ENTRY(1,TMSRelation.ChildValue,"_") AND
                     MobSub.CustNum      EQ iiCustNum                           AND
                     MobSub.PayType      EQ FALSE                               AND
                     MobSub.MultiSimId   EQ iiMultiSimId                        AND 
                     MobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE})          THEN 
            RETURN TRUE.
         ELSE 
            RETURN FALSE.
      END.      
      ELSE DO:
         IF iiMultiSimId > 0 AND
            CAN-FIND(Order NO-LOCK USE-INDEX MultiSimID WHERE 
                     Order.Brand        EQ Syst.Var:gcBrand                    AND 
                     Order.MultiSimId   EQ iiMultiSimId                        AND
                     LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0   AND 
                     Order.CLIType      EQ ENTRY(1,TMSRelation.ChildValue,"_") AND 
                     Order.OrderType    NE {&ORDER_TYPE_RENEWAL}               AND 
                     Order.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE})          THEN
            RETURN TRUE.
         ELSE 
            RETURN FALSE.
      END. 

   END. 

   RETURN TRUE.

END FUNCTION.    

FUNCTION fGetMandatoryExtraLineForMainLine RETURNS CHARACTER
   (INPUT icMainCLIType AS CHAR):

   DEFINE BUFFER TMSRelation FOR TMSRelation.

   DEF VAR lcMandatoryCLITypes AS CHAR NO-UNDO.

   FOR EACH TMSRelation NO-LOCK WHERE
            TMSRelation.TableName    EQ {&ELTABLENAME} AND
            TMSRelation.KeyType      EQ {&ELKEYTYPE}   AND
            TMSRelation.ParentValue  EQ icMainCLIType  AND
            TMSRelation.RelationType EQ {&ELMANDATORY}:

      IF lcMandatoryCLITypes EQ "" THEN
         lcMandatoryCLITypes = ENTRY(1,TMSRelation.ChildValue,"_").
      ELSE
         lcMandatoryCLITypes = lcMandatoryCLITypes + "," + ENTRY(1,TMSRelation.ChildValue,"_").

   END.

   RETURN lcMandatoryCLITypes.

END FUNCTION.

FUNCTION fGetAllowedExtraLinesForMainLine RETURN CHARACTER 
    (INPUT icMainCLIType AS CHAR) :
    
    DEFINE BUFFER bfTMSRelation FOR TMSRelation.
    
    DEFINE VARIABLE lcExtraLineTypes AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lcExtraLineCount AS INTEGER NO-UNDO.
    
    FOR EACH bfTMSRelation NO-LOCK WHERE 
             bfTMSRelation.TableName    EQ {&ELTABLENAME} AND
             bfTMSRelation.KeyType      EQ {&ELKEYTYPE}   AND
             bfTMSRelation.ParentValue  EQ icMainCLIType  :
            
        ASSIGN lcExtraLineCount = INT(bfTMSRelation.RelationType) NO-ERROR.
        
        IF ERROR-STATUS:ERROR OR 
           lcExtraLineCount = 0 THEN NEXT.
        
        ASSIGN lcExtraLineTypes = lcExtraLineTypes + "," +  bfTMSRelation.ChildValue.
                 
    END.  
     
    ASSIGN lcExtraLineTypes = LEFT-TRIM(lcExtraLineTypes,",").
     
    RETURN lcExtraLineTypes.
END.        


FUNCTION fGetOngoingExtralineCount RETURNS LOGICAL
   (INPUT icExtraLineCLIType AS CHAR,
    INPUT icCustIDType       AS CHAR,
    INPUT icCustID           AS CHAR,
    INPUT liMLOrderId        AS INT,
    OUTPUT oiELCount         AS INT):

   DEFINE BUFFER bELOrderCustomer FOR OrderCustomer.
   DEFINE BUFFER bELOrder         FOR Order.

   FOR EACH bELOrderCustomer NO-LOCK WHERE
            bELOrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            bELOrderCustomer.CustId     EQ icCustID         AND
            bELOrderCustomer.CustIdType EQ icCustIDType     AND
            bELOrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH bELOrder NO-LOCK WHERE
            bELOrder.Brand        EQ Syst.Var:gcBrand                  AND
            bELOrder.OrderId      EQ bELOrderCustomer.OrderId          AND           
            bELOrder.CLIType      EQ icExtraLineCLIType                AND
            bELOrder.OrderType    NE {&ORDER_TYPE_RENEWAL}             AND           
            bELOrder.MultiSimId   EQ liMLOrderId                       AND
            bELOrder.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:

         IF LOOKUP(bELOrder.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0 THEN
            NEXT.

         oiELCount = oiELCount + 1.
   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fCheckExistingMainLineAvailForExtraLine RETURNS INTEGER
   (INPUT icExtraLineCLIType AS CHAR,
    INPUT icCustIDType       AS CHAR,
    INPUT icCustID           AS CHAR,
    OUTPUT liMLMsSeq         AS INT):

   DEF VAR liELCount AS INT NO-UNDO. 
   DEF VAR liCount   AS INT NO-UNDO. 
   DEF VAR liOngoingELCnt  AS INT NO-UNDO.

   IF icExtraLineCLIType EQ "" THEN RETURN 0.

   DEFINE BUFFER ELMobSub FOR MobSub.
   DEFINE BUFFER Order    FOR Order.
   DEFINE BUFFER MobSub   FOR MobSub.
   DEFINE BUFFER Customer FOR Customer.

   FOR FIRST Customer NO-LOCK WHERE
             Customer.Brand      EQ Syst.Var:gcBrand AND
             Customer.OrgId      EQ icCustID         AND
             Customer.CustidType EQ icCustIDType     AND
             Customer.Roles      NE "inactive":

      FOR EACH MobSub NO-LOCK USE-INDEX CustNum WHERE
               MobSub.Brand    EQ Syst.Var:gcBrand      AND
               MobSub.CustNum  EQ Customer.CustNum      AND
               MobSub.PayType  EQ FALSE                 AND
              (MobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
               MobSub.MsStatus EQ {&MSSTATUS_BARRED})   BY MobSub.ActivationTS:

          IF (NOT fCLITypeIsMainLine(MobSub.CLIType)             OR  
              NOT fCLITypeAllowedForExtraLine(MobSub.CLIType, icExtraLineCLIType,
                                              OUTPUT liELCount)) THEN 
             NEXT.
   
          FIND LAST Order NO-LOCK WHERE
                    Order.MsSeq      EQ MobSub.MsSeq              AND
                    Order.CLIType    EQ MobSub.CLIType            AND
                    Order.StatusCode Eq {&ORDER_STATUS_DELIVERED} AND
             LOOKUP(STRING(Order.OrderType),"0,1,4") > 0          NO-ERROR.
          IF NOT AVAIL Order THEN 
             FIND LAST Order NO-LOCK WHERE
                       Order.MsSeq      EQ MobSub.MsSeq              AND
                       Order.StatusCode EQ {&ORDER_STATUS_DELIVERED} AND
                LOOKUP(STRING(Order.OrderType),"0,1,4") > 0         NO-ERROR.

          IF NOT AVAIL Order THEN NEXT.

          liCount = 0.

          FOR EACH ELMobSub NO-LOCK WHERE 
                   ELMobSub.Brand        EQ Syst.Var:gcBrand   AND 
                   ELMobSub.CLIType      EQ icExtraLineCLIType AND 
                   ELMobSub.CustNum      EQ MobSub.CustNum     AND 
                   ELMobSub.PayType      EQ FALSE              AND 
                   ELMobSub.MultiSimId   EQ MobSub.MsSeq       AND 
                   ELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:  
             liCount = liCount + 1.   
          END.
          
          fGetOngoingExtralineCount(icExtraLineCLIType, 
                                    icCustIDType, 
                                    icCustID, 
                                    Order.OrderId, 
                                    OUTPUT liOngoingELCnt ).
          
          liCount  =  liCount + liOngoingELCnt. 
                   
          IF liCount EQ 0 THEN DO:
             IF NOT fCheckForMandatoryExtraLine(MobSub.MsSeq,
                                                Customer.CustNum,
                                                MobSub.CLIType,
                                                icExtraLineCLIType,
                                                TRUE) THEN DO: 
                IF NOT fCheckForMandatoryExtraLine(Order.OrderId,
                                                   Customer.CustNum,
                                                   Order.CLIType,
                                                   icExtraLineCLIType,
                                                   FALSE) THEN
                   NEXT.  
             END.      
          END.      

          IF liCount < liELCount THEN DO:

             IF fCheckProMainlineForExtraLine(MobSub.CLIType) AND
                NOT fIsPro(Customer.Category)                 THEN NEXT.

             liMLMsSeq = MobSub.MsSeq.
             RETURN Order.OrderId.  
          END.

      END.
   END.

   RETURN 0.

END FUNCTION.

FUNCTION fCheckOngoingMainLineAvailForExtraLine RETURNS INTEGER
   (INPUT icExtraLineCLIType AS CHAR,
    INPUT icCustIDType       AS CHAR,
    INPUT icCustID           AS CHAR):

   DEF VAR liELCount AS INT NO-UNDO. 
   DEF VAR liCount   AS INT NO-UNDO.    
   
   DEFINE BUFFER OrderCustomer FOR OrderCustomer.
   DEFINE BUFFER Order         FOR Order.
   DEFINE BUFFER OrderFusion   FOR OrderFusion.
   DEFINE BUFFER ELOrder       FOR Order.

   IF NOT icExtraLineCLIType > "" THEN RETURN 0.
   
   FOR EACH OrderCustomer NO-LOCK WHERE
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND
            OrderCustomer.CustId     EQ icCustID         AND
            OrderCustomer.CustIdType EQ icCustIDType     AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH Order NO-LOCK WHERE
            Order.Brand        EQ Syst.Var:gcBrand      AND
            Order.orderid      EQ OrderCustomer.Orderid AND
            Order.OrderType    NE {&ORDER_TYPE_RENEWAL},
      FIRST OrderFusion NO-LOCK WHERE
            OrderFusion.Brand   = Syst.Var:gcBrand AND
            OrderFusion.OrderID = Order.OrderID    BY Order.CrStamp:

      IF (LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) > 0       OR 
         Order.StatusCode EQ {&ORDER_STATUS_PENDING_FIXED_LINE_CANCEL}) THEN 
         NEXT.  

      IF (NOT fCLITypeIsMainLine(Order.CLIType)              OR
          NOT fCLITypeAllowedForExtraLine(Order.CLIType, icExtraLineCLIType,
                                          OUTPUT liELCount)) THEN 
         NEXT.

      liCount = 0.

      fGetOngoingExtralineCount(icExtraLineCLIType,
                                icCustIDType,
                                icCustID,
                                Order.OrderId, /* Mainline OrderId */
                                OUTPUT liCount).
      
      IF liCount EQ 0 THEN DO:
         IF NOT fCheckForMandatoryExtraLine(Order.OrderId,
                                            Order.CustNum,
                                            Order.CLIType,
                                            icExtraLineCLIType,
                                            FALSE) THEN 
          NEXT.  
      END.      
      
      IF liCount < liELCount THEN DO:
         
         IF fCheckProMainlineForExtraLine(Order.CLIType) AND
            NOT fIsPro(OrderCustomer.Category) THEN NEXT.

         RETURN Order.OrderId.

      END.

   END.

   RETURN 0.

END FUNCTION.

FUNCTION fCheckActiveExtraLinePair RETURNS LOG
   (INPUT iiMsSeq        AS INT,
    INPUT icCLIType      AS CHAR,
    OUTPUT lcDSSBundleId AS CHAR):

   DEFINE BUFFER lbMLMobSub FOR MobSub.
   DEFINE BUFFER lbELMobSub FOR MobSub.

   DEF VAR llgMatrixAvailable    AS LOG  NO-UNDO. 
   DEF VAR lcPrimaryCLIType      AS CHAR NO-UNDO. 
   DEF VAR lcDSS2PrimarySubsType AS CHAR NO-UNDO. 
   DEF VAR lcDSS4PrimarySubsType AS CHAR NO-UNDO. 

   ASSIGN lcDSS2PrimarySubsType = fCParamC("DSS2_PRIMARY_SUBS_TYPE")
          lcDSS4PrimarySubsType = fCParamC("DSS4_PRIMARY_SUBS_TYPE").

   IF fCLITypeIsMainLine(icCLIType) THEN DO:

      FIND FIRST lbMLMobSub NO-LOCK WHERE
                 lbMLMobSub.MsSeq      = iiMsSeq             AND
                (lbMLMobSub.MsStatus   = {&MSSTATUS_ACTIVE}  OR
                 lbMLMobSub.MsStatus   = {&MSSTATUS_BARRED}) NO-ERROR.
      IF AVAIL lbMLMobSub THEN DO:

         FOR FIRST lbELMobSub NO-LOCK WHERE
                   lbELMobSub.Brand        = Syst.Var:gcBrand      AND
                   lbELMobSub.MultiSimId   = lbMLMobSub.MsSeq      AND
                   lbELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}:
            ASSIGN llgMatrixAvailable = TRUE
                   lcPrimaryCLIType   = lbMLMobSub.CLIType.
         END.

      END.

   END.
   ELSE IF fCLITypeIsExtraLine(icCLIType) THEN DO:

      FOR FIRST lbELMobSub NO-LOCK WHERE
                lbELMobSub.MsSeq        = iiMsSeq               AND
                lbELMobSub.MultiSimId  <> 0                     AND
                lbELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}:

         FIND FIRST lbMLMobSub NO-LOCK WHERE
                    lbMLMobSub.MsSeq        = lbELMobSub.MultiSimId AND
                   (lbMLMobSub.MsStatus     = {&MSSTATUS_ACTIVE} OR
                    lbMLMobSub.MsStatus     = {&MSSTATUS_BARRED})   NO-ERROR.
         IF AVAIL lbMLMobSub THEN
            ASSIGN llgMatrixAvailable = TRUE
                   lcPrimaryCLIType   = lbMLMobSub.CLIType.

      END.

   END.

   IF LOOKUP(lcPrimaryCLIType,lcDSS4PrimarySubsType) > 0 THEN 
      lcDSSBundleId = {&DSS4}.
   ELSE IF LOOKUP(lcPrimaryCLIType,lcDSS2PrimarySubsType) > 0 THEN 
      lcDSSBundleId = {&DSS2}.

   IF llgMatrixAvailable THEN 
      RETURN TRUE.
   ELSE    
      RETURN FALSE.

END FUNCTION.

FUNCTION fCheckMainLineOrderStatus RETURNS LOGICAL
   (INPUT liMainLineOrderId  AS INT):

   DEFINE BUFFER Order FOR Order.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand     EQ Syst.Var:gcBrand      AND
              Order.OrderId   EQ liMainLineOrderId     AND
              Order.OrderType NE {&ORDER_TYPE_RENEWAL} NO-ERROR.

   IF AVAIL Order                                             AND
     LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) GT 0 THEN
      RETURN TRUE.

   RETURN FALSE.

END FUNCTION.

/* this function has to be removed while refactoring extralines */
FUNCTION fCheckFixedLineInstalledForMainLine RETURNS LOGICAL
   (INPUT liMainLineOrderId  AS INT):

   DEFINE BUFFER Order       FOR Order. 
   DEFINE BUFFER OrderFusion FOR OrderFusion.

   FIND FIRST Order NO-LOCK WHERE
              Order.Brand        EQ Syst.Var:gcBrand         AND
              Order.OrderId      EQ liMainLineOrderId        AND
       LOOKUP(Order.StatusCode,{&ORDER_CLOSE_STATUSES}) EQ 0 AND
              Order.OrderType    NE {&ORDER_TYPE_RENEWAL}    NO-ERROR.

   IF AVAIL Order THEN DO: 
     
      /* If Fixed line is installed for Main line Convergent Order 
         THEN dont move extra line order to 76 */
      FIND FIRST OrderFusion NO-LOCK WHERE
                 OrderFusion.Brand        = Syst.Var:gcBrand                 AND
                 OrderFusion.OrderID      = Order.OrderID                    AND 
                 OrderFusion.FusionStatus = {&FUSION_ORDER_STATUS_FINALIZED} NO-ERROR.
                 
      IF AVAIL OrderFusion THEN 
         RETURN TRUE.

   END.   

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckAndAssignOrphanExtraline RETURNS LOGICAL 
   (INPUT iiMLMsSeq   AS INT,
    INPUT iiMLCustNum AS INT,
    INPUT icMLCLIType AS CHAR):

   DEFINE BUFFER bELMobSub FOR MobSub.
   DEFINE BUFFER MobSub    FOR MobSub.

   DEF VAR liELCount AS INT NO-UNDO. 
   DEF VAR liCount   AS INT NO-UNDO. 

   FOR EACH MobSub EXCLUSIVE-LOCK USE-INDEX CustNum WHERE
            MobSub.Brand    EQ Syst.Var:gcBrand      AND
            MobSub.CustNum  EQ iiMLCustNum           AND
            MobSub.PayType  EQ FALSE                 AND
           (MobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
            MobSub.MsStatus EQ {&MSSTATUS_BARRED})   BY MobSub.ActivationTS: 

      IF NOT CAN-FIND(FIRST CLIType NO-LOCK WHERE 
                            CLIType.Brand    EQ Syst.Var:gcBrand AND 
                            CLIType.CLIType  EQ MobSub.CLIType   AND 
                            CLIType.LineType EQ {&CLITYPE_LINETYPE_EXTRA}) THEN 
         NEXT.                      

      IF MobSub.MultiSimId   NE 0 AND 
         MobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} THEN NEXT.


      IF NOT fCLITypeAllowedForExtraLine(icMLCLIType, MobSub.CLIType, 
                                         OUTPUT liELCount) THEN
         NEXT.

      FOR EACH bELMobSub NO-LOCK WHERE 
               bELMobSub.Brand        EQ Syst.Var:gcBrand AND 
               bELMobSub.CustNum      EQ MobSub.CustNum   AND 
               bELMobSub.PayType      EQ FALSE            AND 
               bELMobSub.MultiSimId   EQ iiMLMsSeq        AND 
               bELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:  
         liCount = liCount + 1.   
      END.
       
      IF liCount EQ 0 THEN DO:
         IF NOT fCheckForMandatoryExtraLine(iiMLMsSeq,
                                            MobSub.CustNum,
                                            icMLCLIType,
                                            MobSub.CLIType,
                                            TRUE) THEN 
            NEXT.  
       END.      
       
       IF liCount < liELCount THEN DO:
          
          ASSIGN MobSub.MultiSimId   = iiMLMsSeq
                 MobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}.

          fCreateExtraLineDiscount(MobSub.MsSeq,
                                   MobSub.CLIType + "DISC",
                                   TODAY).  
          
          liCount = liCount + 1.

          IF liCount = liELCount THEN 
             LEAVE.
       
       END.

   END.

   RETURN TRUE.

END FUNCTION.

FUNCTION fGetOldestCLITypeOfMobSub RETURNS CHAR
   (iiCustNum AS INT):

   DEFINE BUFFER MobSub FOR MobSub.

   DEF VAR lcOldestCLIType AS CHAR NO-UNDO.

   FOR EACH MobSub NO-LOCK WHERE MobSub.Brand = Syst.Var:gcBrand AND
                                 MobSub.CustNum = iiCustNum 
                                 BY CreationDate:
      IF fCLITypeIsMainLine(MobSub.CLIType) THEN DO:
         lcOldestCLIType = MobSub.CLIType.
         LEAVE.
      END.
   END.

   RETURN lcOldestCLIType.

END FUNCTION.

FUNCTION fGetOldestCLITypeOfOrder RETURNS CHAR
   (iiCustNum AS INT):

   DEFINE BUFFER Order FOR Order.

   DEF VAR lcOldestCLIType AS CHAR NO-UNDO.

   FOR EACH Order NO-LOCK WHERE Order.Brand = Syst.Var:gcBrand AND
                      Order.CustNum = iiCustNum AND
                      LOOKUP(Order.StatusCode,{&ORDER_INACTIVE_STATUSES}) = 0
                      BY CrStamp:                      
      IF fCLITypeIsMainLine(Order.CLIType) THEN DO:
         lcOldestCLIType = Order.CLIType.
         LEAVE.            
      END.
   END.

   RETURN lcOldestCLIType.
   
END FUNCTION.

FUNCTION fGetPayType RETURNS CHAR
   (iiCustNum AS INT):

   DEF VAR lcPayType AS CHAR NO-UNDO.
   DEF VAR lcCLIType AS CHAR NO-UNDO.

   /* YOT-5618 Handle correctly Way of payment for 66 and 67 */
   lcCLIType = fGetOldestCLITypeOfMobSub(iiCustNum).
   IF lcCLIType EQ "" THEN
      lcCLIType = fGetOldestCLITypeOfOrder(iiCustNum).

   IF INDEX(lcCLIType,"DSL") > 0 THEN
      lcPayType = "66".
   ELSE IF INDEX(lcCLIType,"TFH") > 0 THEN
      lcPayType = "67".
   ELSE lcPayType = "68".
    
   RETURN lcPayType.    
      
END FUNCTION.

FUNCTION fExtraLineCountForMainLine RETURN INTEGER
    (INPUT iiMLMsSeq AS INT,
     INPUT iiCustNum AS INT) :

    DEFINE BUFFER bfELMobSub FOR MobSub.
    DEFINE VARIABLE iiELCount  AS INTEGER NO-UNDO.

    FOR EACH bfELMobSub NO-LOCK WHERE
             bfELMobSub.Brand        EQ Syst.Var:gcBrand      AND
             bFELMobSub.CustNum      EQ iiCustNum             AND
             bfELMobSub.MultiSimID   EQ iiMLMsSeq             AND
             bfELMobsub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:

        ASSIGN iiELCount = iiELCount + 1 .

    END.

    RETURN iiELCount.

END FUNCTION.

FUNCTION fGetExtraLineMandatoryCLIType RETURN CHARACTER 
        (INPUT lcMLCLIType            AS CHARACTER ,       
         INPUT lcDependentCLIType     AS CHARACTER ) : 
             
             
    DEFINE BUFFER bfTMSRelation FOR TMSRelation.         
             
    FOR EACH bfTMSRelation NO-LOCK WHERE
             bfTMSRelation.TableName     =    {&ELTABLENAME}  AND
             bfTMSRelation.KeyType       =    {&ELKEYTYPE}    AND 
             bfTMSRelation.RelationType  =    {&ELMANDATORY}  AND 
             bfTMSRelation.ParentValue   =    lcMLCLIType  :
                    
        IF ENTRY(3,bfTMSRelation.childValue,"_") = lcDependentCLIType
        THEN RETURN ENTRY(1,bfTMSRelation.childValue,"_").               
                    
    END.
    
    RETURN "".           
                      
END FUNCTION.             
   
/* Returns true if there is any Extra Line CliType allowed for any 
   subscription (active or ongoing) for this customer.             */ 
FUNCTION fELCliTypeAllowedForCustomer RETURNS LOGICAL 
   (INPUT icIDType    AS CHAR,
    INPUT icPersonID  AS CHAR,
    INPUT icELCliType AS CHAR):
   
   DEF VAR llAllowedActive  AS LOG NO-UNDO.
   DEF VAR llAllowedOnGoing AS LOG NO-UNDO.
                   
   /* Active subscriptions */
   llAllowedActive = FALSE.
   FOR FIRST Customer NO-LOCK WHERE
             Customer.Brand      EQ Syst.Var:gcBrand AND
             Customer.OrgId      EQ icPersonID       AND
             Customer.CustidType EQ icIDType         AND
             Customer.Roles      NE "inactive",      
       EACH MobSub NO-LOCK WHERE
            MobSub.Brand    EQ Syst.Var:gcBrand   AND
            MobSub.CustNum  EQ Customer.CustNum   AND
            MobSub.PayType  EQ FALSE              AND
           (MobSub.MsStatus EQ {&MSSTATUS_ACTIVE} OR
            MobSub.MsStatus EQ {&MSSTATUS_BARRED})    
       USE-INDEX CustNum 
       BY MobSub.ActivationTS:  
      IF CAN-FIND(FIRST TMSRelation WHERE 
                        TMSRelation.TableName   EQ {&ELTABLENAME} AND 
                        TMSRelation.KeyType     EQ {&ELKEYTYPE}   AND 
                        TMSRelation.ParentValue EQ MobSub.CLIType AND  
                        TMSRelation.ChildValue  EQ icELCliType    AND
                        INT(TMSRelation.RelationType) > 0)
      THEN DO:
         IF fCheckProMainlineForExtraLine(MobSub.CliType) AND
            NOT fIsPro(Customer.Category) THEN 
            NEXT.         
         llAllowedActive = TRUE.      
         LEAVE.                
      END.                                                   
   END.
   
   /* Ongoing orders */
   llAllowedOnGoing = FALSE.
   FOR EACH OrderCustomer NO-LOCK WHERE   
            OrderCustomer.Brand      EQ Syst.Var:gcBrand AND 
            OrderCustomer.CustId     EQ icPersonID       AND
            OrderCustomer.CustIdType EQ icIDType         AND
            OrderCustomer.RowType    EQ {&ORDERCUSTOMER_ROWTYPE_AGREEMENT},
       EACH Order NO-LOCK WHERE
            Order.Brand     EQ Syst.Var:gcBrand       AND
            Order.orderid   EQ OrderCustomer.Orderid  AND
            Order.OrderType NE {&ORDER_TYPE_RENEWAL} 
       BY Order.CrStamp:
      IF CAN-FIND(FIRST TMSRelation WHERE 
                        TMSRelation.TableName   EQ {&ELTABLENAME} AND 
                        TMSRelation.KeyType     EQ {&ELKEYTYPE}   AND 
                        TMSRelation.ParentValue EQ Order.CLIType  AND  
                        TMSRelation.ChildValue  EQ icELCliType    AND
                        INT(TMSRelation.RelationType) > 0) 
      THEN DO:
         IF fCheckProMainlineForExtraLine(Order.CLIType) AND
            NOT fIsPro(OrderCustomer.Category) THEN 
            NEXT.         
         llAllowedOnGoing = TRUE.      
         LEAVE.                
      END.       
   END.
   
   RETURN (llAllowedActive OR llAllowedOnGoing).        

END. /* fELCliTypeAllowedForCustomer */

&ENDIF
