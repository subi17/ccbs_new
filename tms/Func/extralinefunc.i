&IF "{&EXTRALINEFUNC_I}" NE "YES"
&THEN

&GLOBAL-DEFINE EXTRALINEFUNC_I YES

{Syst/tmsconst.i}

/* Returns comma delimited character list of extraline clitypes (tariffs) */ 
FUNCTION fExtraLineCLITypes RETURNS CHARACTER:

   DEFINE VARIABLE lcReturnValue  AS CHARACTER NO-UNDO.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq AND
             MXItem.MXName  = "SubsTypeTo":

      lcReturnValue = lcReturnValue + "," + MXItem.MXValue.
   END.

   RETURN LEFT-TRIM(lcReturnValue, ",").

END FUNCTION.

/* Check if a mainline clitype is allowed for an extraline clitype  */ 
FUNCTION fCLITypeAllowedForExtraLine RETURNS LOGICAL
   (icCLIType          AS CHARACTER,
    icExtraLineCLIType AS CHARACTER):

   DEFINE BUFFER MXItemExtra FOR MXItem.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItemExtra NO-LOCK WHERE
             MXItemExtra.MXSeq   = Matrix.MXSeq   AND
             MXItemExtra.MXName  = "SubsTypeTo" AND
             MXItemExtra.MXValue = icExtraLineCLIType,       
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq AND
             MXItem.MXName  = "SubsTypeFrom" AND
             MXItem.MXValue = icCLIType:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.


/* Check which extraline clitype a mainline clitype is using.
   If the given clitype is not mainline clitype the function returns
   empty string */ 
FUNCTION fExtraLineForMainLine RETURNS CHARACTER
   (icMainLineCLIType  AS CHARACTER):

   DEFINE BUFFER MXItemMain FOR MXItem.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItemMain NO-LOCK WHERE
             MXItemMain.MXSeq   = Matrix.MXSeq AND
             MXItemMain.MXName  = "SubsTypeFrom" AND
             MXItemMain.MXValue = icMainLineCLIType,
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = MXItemMain.MXSeq AND
             MXItem.MXName  = "SubsTypeTo":
                
       RETURN MXItem.MXValue.
   END.
   
   RETURN "".

END FUNCTION.

/* Check if the clitype is extraline clitype */
FUNCTION fCLITypeIsExtraLine RETURNS LOGICAL
   (icExtraLineCLIType AS CHARACTER):

   DEFINE VARIABLE lcReturnValue  AS CHARACTER NO-UNDO.

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq   AND
             MXItem.MXName  = "SubsTypeTo" AND
             MXItem.MXValue = icExtraLineCLIType:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Check if the clitype is mainline clitype
   (i.e. it is part of any of extraline clitype mainline list) */ 
FUNCTION fCLITypeIsMainLine RETURNS LOGICAL
   (icCLIType  AS CHARACTER):

   FOR EACH  Matrix NO-LOCK WHERE
             Matrix.Brand  = Syst.Var:gcBrand   AND
             Matrix.MXKey  = {&EXTRALINEMATRIX},
       FIRST MXItem NO-LOCK WHERE
             MXItem.MXSeq   = Matrix.MXSeq AND
             MXItem.MXName  = "SubsTypeFrom" AND
             MXItem.MXValue = icCLIType:
      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

/* Function checks if STC is possible when the new clitype is
   extraline. It is possible when the customer has a free
   mainline and the mainline is suitable for the extraline */
FUNCTION fSTCPossible RETURNS LOGICAL
   (iiCustNum    AS INTEGER,
    icNewCLIType AS CHARACTER):

   IF NOT fCLITypeIsExtraLine(icNewCLIType)
   THEN RETURN TRUE.

   DEFINE BUFFER MobSub FOR MobSub.

   /* Find suitable mainline mobsub from the customer */
   FOR EACH MobSub NO-LOCK WHERE
            MobSub.CustNum      EQ iiCustNum      AND
            MobSub.MultiSimId   EQ 0                   AND
            MobSub.MultiSimtype EQ 0                   AND
            (MobSub.MsStatus    EQ {&MSSTATUS_ACTIVE}  OR
             MobSub.MsStatus    EQ {&MSSTATUS_BARRED}):

      IF NOT fCLITypeAllowedForExtraLine(MobSub.CLIType, icNewCLIType)
      THEN NEXT.

      RETURN TRUE.
   END.

   RETURN FALSE.

END FUNCTION.

FUNCTION fCheckExistingMainLineAvailForExtraLine RETURNS INTEGER
   (INPUT icExtraLineCLIType AS CHAR,
    INPUT icCustIDType       AS CHAR,
    INPUT icCustID           AS CHAR):

   DEF VAR liELCount AS INT NO-UNDO. 
   DEF VAR liCount   AS INT NO-UNDO. 

   IF NOT icExtraLineCLIType > "" THEN RETURN 0.

   DEFINE BUFFER Customer FOR Customer.
   DEFINE BUFFER MobSub   FOR MobSub.
   DEFINE BUFFER ELMobSub FOR MobSub.
   DEFINE BUFFER Order    FOR Order.

   CASE icExtraLineCLIType:
      WHEN {&AZULMORADAEL}   THEN liELCount = {&AZULMORDADAEXTRALINE}.
      WHEN {&INTERMINABLEEL} THEN liELCount = {&INTERMINABLEEXTRALINE}.  
   END CASE.

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

          IF (NOT fCLITypeIsMainLine(MobSub.CLIType)                               OR  
              NOT fCLITypeAllowedForExtraLine(MobSub.CLIType, icExtraLineCLIType)) THEN 
             NEXT.
   
          FIND LAST Order NO-LOCK WHERE
                    Order.MsSeq      = MobSub.MsSeq              AND
                    Order.CLIType    = MobSub.CLIType            AND
                    Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND
             LOOKUP(STRING(Order.OrderType),"0,1,4") > 0         NO-ERROR.
          IF NOT AVAIL Order THEN 
             FIND LAST Order NO-LOCK WHERE
                       Order.MsSeq      = MobSub.MsSeq              AND
                       Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND
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

          IF liCount < liELCount THEN
             RETURN Order.OrderId.  

      END.

   END.

   RETURN 0.

END FUNCTION.

FUNCTION fCheckOngoingMainLineAvailForExtraLine RETURNS INTEGER
   (INPUT icExtraLineCLIType AS CHAR,
    INPUT icCustIDType       AS CHAR,
    INPUT icCustID           AS CHAR):

   IF NOT icExtraLineCLIType > "" THEN RETURN 0.

   DEF VAR liELCount AS INT NO-UNDO. 
   DEF VAR liCount   AS INT NO-UNDO. 
   
   DEFINE BUFFER OrderCustomer FOR OrderCustomer.
   DEFINE BUFFER Order         FOR Order.
   DEFINE BUFFER OrderFusion   FOR OrderFusion.
   DEFINE BUFFER ELOrder       FOR Order.

   CASE icExtraLineCLIType:
      WHEN {&AZULMORADAEL}   THEN liELCount = {&AZULMORDADAEXTRALINE}.
      WHEN {&INTERMINABLEEL} THEN liELCount = {&INTERMINABLEEXTRALINE}.  
   END CASE.

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

      IF (NOT fCLITypeIsMainLine(MobSub.CLIType)                              OR
          NOT fCLITypeAllowedForExtraLine(Order.CLIType, icExtraLineCLIType)) THEN 
         NEXT.

      liCount = 0.

      FOR EACH ELOrder NO-LOCK WHERE 
               ELOrder.Brand        EQ Syst.Var:gcBrand                  AND 
               ELOrder.StatusCode   EQ {&ORDER_STATUS_PENDING_MAIN_LINE} AND 
               ELOrder.CustNum      EQ Order.CustNum                     AND
               ELOrder.CLIType      EQ icExtraLineCLIType                AND 
               ELOrder.OrderType    NE {&ORDER_TYPE_RENEWAL}             AND 
               ELOrder.MultiSimId   NE 0                                 AND 
               ELOrder.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:
         liCount = liCount + 1.
      END.

      IF liCount < liELCount THEN
         RETURN Order.OrderId.

   END.

   RETURN 0.

END FUNCTION.

FUNCTION fCheckAndAssignOrphanExtraline RETURNS LOGICAL
   (INPUT iiMainLineOrderId AS INT, 
    INPUT iiMLMsSeq         AS INT, 
    INPUT liMLCustNum       AS INT,
    INPUT icMLCLIType       AS CHAR):

   DEFINE BUFFER lbELMobSub FOR MobSub. 
   DEFINE BUFFER lbELOrder  FOR Order.

   DEF VAR lcExtraLineCLIType AS CHAR NO-UNDO. 
   DEF VAR liCount            AS INT  NO-UNDO. 
   DEF VAR liELCount          AS INT  NO-UNDO. 

   ASSIGN lcExtraLineCLIType = fExtraLineForMainLine(icMLCLIType)
          liCount            = 0
          liELCount          = 0.

   CASE cExtraLineCLIType:
      WHEN {&AZULMORADAEL}   THEN liELCount = {&AZULMORDADAEXTRALINE}.
      WHEN {&INTERMINABLEEL} THEN liELCount = {&INTERMINABLEEXTRALINE}.
   END CASE.

   /* Check existing subscription for mainline */
   FOR EACH lbELMobSub NO-LOCK WHERE 
            lbELMobSub.Brand        EQ Syst.Var:gcBrand   AND 
            lbELMobSub.CLIType      EQ lcExtraLineCLIType AND 
            lbELMobSub.CustNum      EQ liMLCustNum        AND 
            lbELMobSub.PayType      EQ FALSE              AND 
            lbELMobSub.MultiSimId   EQ iiMLMsSeq          AND 
            lbELMobSub.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE}:  
       liCount = liCount + 1.   
   END.

   /* Check ongoing order for mainline */
   FOR EACH lbELOrder NO-LOCK WHERE
            lbELOrder.Brand        EQ Syst.Var:gcBrand          AND
            lbELOrder.MultiSimId   EQ iiMainLineOrderId         AND
            lbELOrder.MultiSimType EQ {&MULTISIMTYPE_EXTRALINE} AND 
            lbELOrder.CustNum      EQ liMLCustNum:           

      IF LOOKUP(lbELOrder.StatusCode,{&ORDER_CLOSE_STATUSES}) > 0 THEN NEXT.

      liCount = liCount + 1.

   END.

   IF liCount < liELCount THEN DO:
      
      FOR EACH lbELMobSub NO-LOCK WHERE 
               lbELMobSub.Brand        EQ Syst.Var:gcBrand   AND
               lbELMobSub.CLIType      EQ lcExtraLineCLIType AND
               lbELMobSub.CustNum      EQ liMLCustNum        AND
               lbELMobSub.PayType      EQ FALSE              AND
               lbELMobSub.MultiSimId   EQ 0                  AND
               lbELMobSub.MultiSimType EQ 0:

         ASSIGN lbELMobSub.MultiSimId   = iiMLMsSeq 
                lbELMobSub.MultiSimType = {&MULTISIMTYPE_EXTRALINE}.

         fCreateExtraLineDiscount(lbELMobSub.MsSeq,
                                  lbELMobSub.CLIType + "DISC",
                                  TODAY).

         liCount = liCount + 1.

         IF liCount = liELCount THEN LEAVE.

      END.

   END.

   RETURN TRUE.

END FUNCTION.   

&ENDIF
