/* This cron job is to provide the addline discount if the addline is
   eligible to get the discount */

{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Mc/dpmember.i}
{Func/ftransdir.i}

DEFINE VARIABLE ldtDate   AS DATE NO-UNDO.
DEFINE VARIABLE liTime    AS INT  NO-UNDO.
DEFINE VARIABLE ldtCronDate  AS DATE NO-UNDO.
DEFINE VARIABLE liCronTime   AS INT  NO-UNDO.
DEFINE VARIABLE liHours      AS INT  NO-UNDO.
DEFINE VARIABLE lcResult     AS CHAR NO-UNDO.
DEFINE VARIABLE lcMainLine   AS CHAR NO-UNDO.
DEFINE VARIABLE lcFile       AS CHAR NO-UNDO.
DEFINE VARIABLE lcSpoolDir   AS CHAR NO-UNDO.
DEFINE VARIABLE lcOutDir     AS CHAR NO-UNDO.
DEFINE VARIABLE lcCurrTimeStamp LIKE Order.CrStamp NO-UNDO.

DEFINE TEMP-TABLE tt-data
    FIELD addlinemsisdn     AS CHAR
    FIELD addlineclitype    AS CHAR
    FIELD addlinesaleschnl  AS CHAR
    FIELD addlineordcrstamp AS DECIMAL
    FIELD mainmsisdn        AS CHAR
    FIELD mainclitype       AS CHAR
    FIELD mainsaleschnl     AS CHAR
    FIELD mainordcrstamp    AS DECIMAL.

DEFINE STREAM strout.

DEFINE BUFFER bOrder FOR Order.
DEFINE BUFFER bOrdCust FOR OrderCustomer.
DEFINE BUFFER bDiscountPlan FOR DiscountPlan.
DEFINE BUFFER bDiscountPlanMob FOR DiscountPlan.
DEFINE BUFFER lbOrderMain  FOR Order.


FIND FIRST TmsParam WHERE
           TmsParam.Brand      = gcBrand AND
           TmsParam.ParamGroup = "AddLineCron" AND
           TmsParam.ParamCode  = "OrdCreateHours" AND 
           TmsParam.ParamType  = "I" NO-LOCK NO-ERROR.

IF AVAIL TmsParam THEN
   liHours = TmsParam.IntVal.

ASSIGN lcSpoolDir   = fCParam("AddLineCron","OutSpoolDir")
       lcOutDir     = fCParam("AddLineCron","OutDir").

ASSIGN
   lcFile = lcSpoolDir + "addlinedisc_" + STRING(TODAY,"99999999") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".

lcCurrTimeStamp = fMakeTS().

fSplitTS(lcCurrTimeStamp, OUTPUT ldtCronDate, OUTPUT liCronTime).

ord-blk:
FOR EACH Order NO-LOCK WHERE 
         Order.Brand = gcBrand AND 
         ENTRY(1,STRING(Order.CrStamp),".") = ENTRY(1,STRING(lcCurrTimeStamp),".") AND         
         LOOKUP(STRING(Order.OrderType),"0,1,4") > 0 :
   IF Order.StatusCode <> {&ORDER_STATUS_DELIVERED} OR
      Order.PayType     = TRUE THEN NEXT ord-blk.

   fSplitTS(Order.crstamp, OUTPUT ldtDate, OUTPUT liTime).

   RUN pOrderCheck(INPUT liCronTime, INPUT liTime, OUTPUT lcResult).
   IF lcResult = "Next" THEN
      NEXT ord-blk.   

   RUN pCheckCliType(INPUT Order.CliType, OUTPUT lcResult).
   IF lcResult = "Next" THEN
      NEXT ord-blk.

   RUN pCheckAddlineDisc(INPUT Order.StatusCode, 
                         INPUT Order.OrderID, 
                         INPUT Order.CliType, 
                         INPUT Order.MsSeq,
                         OUTPUT lcResult).

   IF lcResult = "Next" THEN
      NEXT ord-blk.

   /* This find is just to find the custidtype and custid
      for that order */
   FIND FIRST OrderCustomer NO-LOCK WHERE
              OrderCustomer.Brand   = gcBrand       AND
              OrderCustomer.OrderId = Order.OrderId AND
              OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.
         
   IF AVAIL OrderCustomer THEN
   DO:
      OrdCust-blk:
      FOR EACH bOrdCust NO-LOCK WHERE
               bOrdCust.Brand      = gcBrand                  AND
               bOrdCust.CustIDType = OrderCustomer.CustIDType AND
               bOrdCust.CustID     = OrderCustomer.CustID     AND
               bOrdCust.RowType    = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT}: 

         FOR FIRST bOrder NO-LOCK WHERE
                   bOrder.Brand      EQ gcBrand AND
                   bOrder.Orderid    EQ bOrdCust.Orderid:                  

            IF bOrder.StatusCode <> {&ORDER_STATUS_DELIVERED} OR
               LOOKUP(STRING(bOrder.OrderType),"0,1,4") = 0 OR
               bOrder.PayType     = TRUE THEN NEXT OrdCust-blk.

            fSplitTS(bOrder.crstamp, OUTPUT ldtDate, OUTPUT liTime).

            /* To check if the order is created within the hours configured in TMS */
            RUN pOrderCheck(INPUT liCronTime, INPUT liTime, OUTPUT lcResult).
            IF lcResult = "Next" THEN
               NEXT OrdCust-blk.

            /* To check if clitype available */
            RUN pCheckCliType(INPUT bOrder.CliType, OUTPUT lcResult).
            IF lcResult = "Next" THEN
               NEXT OrdCust-blk.

            /* To skip the order that does have any additional line disc */
            RUN pCheckAddlineDisc(INPUT bOrder.StatusCode, 
                                  INPUT bOrder.OrderID, 
                                  INPUT bOrder.CliType, 
                                  INPUT bOrder.MsSeq,
                                  OUTPUT lcResult).

            IF lcResult = "Next" THEN
               NEXT OrdCust-blk.

            FIND FIRST bDiscountPlan WHERE
                       bDiscountPlan.Brand = gcBrand AND
                       bDiscountPlan.DPRuleID = ENTRY(LOOKUP(bOrder.CliType, {&ADDLINE_CLITYPES}), 
                                                      {&ADDLINE_DISCOUNTS}) NO-LOCK NO-ERROR.

            FIND FIRST bDiscountPlanMob WHERE
                       bDiscountPlanMob.Brand = gcBrand AND
                       bDiscountPlanMob.DPRuleID = ENTRY(LOOKUP(bOrder.CliType, {&ADDLINE_CLITYPES}), 
                                                      {&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

            /* Now to check the policy of "Additional line 50% discount 
             with Convergent" and "Additional line 50% discount with Mobile Only" 
             and if customer is eligible to get the additional line discount */
                                           
            IF AVAIL bDiscountPlan THEN
            DO:                  
               RUN pFinalConvProcess(OUTPUT lcMainLine,
                                     OUTPUT lcResult). 
               IF lcResult = "Next" THEN
                  NEXT OrdCust-blk.
            END.

            IF AVAIL bDiscountPlanMob THEN
            DO:                  
               RUN pFinalMobOnlyProcess(OUTPUT lcMainLine,
                                        OUTPUT lcResult). 
               IF lcResult = "Next" THEN
                  NEXT OrdCust-blk.
            END.                                          
         END.
      END.   
   END.
END.

IF CAN-FIND(FIRST tt-data NO-LOCK) THEN
DO:   
   OUTPUT STREAM strout TO VALUE(lcFile).

   FOR EACH tt-data NO-LOCK:
      EXPORT STREAM strout DELIMITER "|" tt-data.
   END.

   OUTPUT STREAM strout CLOSE.

   fMove2TransDir(lcFile, "", lcOutDir).
END.

PROCEDURE pOrderCheck:   
   DEFINE INPUT PARAMETER iliCronTime  AS INT  NO-UNDO.
   DEFINE INPUT PARAMETER iliOrdCrTime AS INT  NO-UNDO.
   DEFINE OUTPUT PARAMETER olcResult   AS CHAR NO-UNDO.

   IF iliCronTime - iliOrdCrTime > (liHours * 60 * 60)  THEN
      olcResult = "Next".

END PROCEDURE.

PROCEDURE pCheckCliType:
   DEFINE INPUT PARAMETER ilcCliType AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER olcResult AS CHAR NO-UNDO.

   FIND FIRST CliType WHERE 
              CliType.Brand      = gcBrand AND
              CliType.CliTYpe    = ilcCliTYpe NO-LOCK NO-ERROR.

   IF NOT AVAIL CliType OR
      (CliType.TariffType <> {&CLITYPE_TARIFFTYPE_MOBILEONLY} AND
       CliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} AND 
       CliType.PayType    <> 1) THEN
      olcResult = "Next".

END PROCEDURE.

PROCEDURE pCheckAddlineDisc:
   DEFINE INPUT PARAMETER  ilcOrdStatus AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER  iliOrdId     AS INT  NO-UNDO.
   DEFINE INPUT PARAMETER  ilcCliType   AS CHAR NO-UNDO.
   DEFINE INPUT PARAMETER  iliMsSeq     AS INT  NO-UNDO.   
   DEFINE OUTPUT PARAMETER olcResult    AS CHAR NO-UNDO.

   DEFINE BUFFER lbMobSub FOR MobSub.
   
   
   FIND FIRST lbMobSub NO-LOCK WHERE 
              lbMobSub.Brand = gcBrand AND
              lbMobSub.MsSeq = iliMsSeq NO-ERROR.
   IF AVAIL lbMobSub THEN
   DO:
      FIND FIRST DiscountPlan WHERE
                 DiscountPlan.Brand = gcBrand AND
                 DiscountPlan.DPRuleID = ENTRY(LOOKUP(lbMobSub.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_20}) NO-LOCK NO-ERROR.
      IF AVAIL DiscountPlan THEN 
      DO: 
         IF CAN-FIND(FIRST DPMember WHERE
                           DPMember.DPId      = DiscountPlan.DPId AND
                           DPMember.HostTable = "MobSub" AND
                           DPMember.KeyValue  = STRING(lbMobSub.MsSeq) AND
                           DPMember.ValidTo   >= TODAY) THEN
         DO:            
            olcResult = "Next".
            RETURN.
         END.
      END.

      FIND FIRST DiscountPlan WHERE
                 DiscountPlan.Brand = gcBrand AND
                 DiscountPlan.DPRuleID = ENTRY(LOOKUP(lbMobSub.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

      IF AVAIL DiscountPlan THEN 
      DO: 
         IF CAN-FIND(FIRST DPMember WHERE
                           DPMember.DPId      = DiscountPlan.DPId AND
                           DPMember.HostTable = "MobSub" AND
                           DPMember.KeyValue  = STRING(lbMobSub.MsSeq) AND
                           DPMember.ValidTo   >= TODAY) THEN
         DO:
            olcResult = "Next".
            RETURN.
         END.
      END.
      
      FIND FIRST DiscountPlan WHERE
                 DiscountPlan.Brand = gcBrand AND
                 DiscountPlan.DPRuleID = ENTRY(LOOKUP(lbMobSub.CliType, {&ADDLINE_CLITYPES}), {&ADDLINE_DISCOUNTS}) NO-LOCK NO-ERROR.

      IF AVAIL DiscountPlan THEN 
      DO:             
         IF CAN-FIND(FIRST DPMember WHERE
                           DPMember.DPId      = DiscountPlan.DPId AND
                           DPMember.HostTable = "MobSub" AND
                           DPMember.KeyValue  = STRING(lbMobSub.MsSeq) AND
                           DPMember.ValidTo   >= TODAY) THEN
         DO:
            olcResult = "Next".
            RETURN.
         END.
      END.
   END.   
END PROCEDURE.

PROCEDURE pConvCheck :
   DEFINE INPUT  PARAMETER icCustIDType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCustID      AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCliType     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocMainLineCli AS CHAR NO-UNDO.   

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   DEFINE BUFFER bClitype  FOR Clitype.

   for-blk:
   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = Syst.Parameters:gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.ROLES     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = Syst.Parameters:gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.PayType = FALSE                   AND
            (bMobSub.MsStatus = {&MSSTATUS_ACTIVE}     OR
             bMobSub.MsStatus = {&MSSTATUS_BARRED}),
       FIRST bCliType WHERE bCliType.Brand = gcBrand AND bCliType.CliType = bMobSub.CliType NO-LOCK:
      
       IF bCliType.TariffType <> {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN 
          NEXT.

       IF fIsConvergentAddLineOK(bMobSub.CLIType,icCliType) THEN 
       DO:
          ASSIGN ocMainLineCli = bMobSub.cli.
          LEAVE for-blk.
       END.
   END.   
END PROCEDURE.

PROCEDURE pMobOnlyCheck :
   DEFINE INPUT  PARAMETER icCustIDType  AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCustID      AS CHAR NO-UNDO.
   DEFINE INPUT  PARAMETER icCliType     AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocMainLineCli AS CHAR NO-UNDO.

   DEFINE BUFFER bCustomer FOR Customer.
   DEFINE BUFFER bMobSub   FOR MobSub.
   
   FIND FIRST DiscountPlan WHERE
              DiscountPlan.Brand = gcBrand AND
              DiscountPlan.DPRuleID = ENTRY(LOOKUP(icCliType, {&ADDLINE_CLITYPES}),{&ADDLINE_DISCOUNTS_HM}) NO-LOCK NO-ERROR.

   for-blk:
   FOR FIRST bCustomer WHERE
             bCustomer.Brand      = gcBrand AND
             bCustomer.OrgId      = icCustID                AND
             bCustomer.CustidType = icCustIDType            AND
             bCustomer.Roles     NE "inactive"              NO-LOCK,
       EACH  bMobSub NO-LOCK WHERE
             bMobSub.Brand   = gcBrand AND
             bMobSub.InvCust = bCustomer.CustNum       AND
             bMobSub.MsSeq  <> bOrder.MsSeq            AND
             bMobSub.PayType = FALSE:

       /* To handle the scenario where more than
          one sim only order for La sinfin or la infinita 5GB */

       IF (icCliType = ENTRY(3,{&ADDLINE_CLITYPES} ) OR 
           icCliType = ENTRY(4,{&ADDLINE_CLITYPES} )) AND 
          CAN-FIND(FIRST DPMember WHERE
                         DPMember.DPId = DiscountPlan.DPId AND
                         DPMember.HostTable = "MobSub" AND
                         DPMember.KeyValue  = STRING(bMobSub.MsSeq) AND
                         DPMember.ValidTo   >= TODAY) THEN NEXT.

       IF fIsMobileOnlyAddLineOK(bMobSub.CLIType,icCliType) THEN
       DO:
          ASSIGN ocMainLineCli = bMobSub.cli.
          LEAVE for-blk.
       END.          
   END.

END PROCEDURE.

PROCEDURE pFinalConvProcess:   
   DEFINE OUTPUT PARAMETER ocMainline    AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocNext        AS CHAR NO-UNDO.

   DEFINE BUFFER lbMobSubMain FOR MobSub.     

   RUN pConvCheck(OrderCustomer.CustIDType,OrderCustomer.CustID,bOrder.CliType, OUTPUT ocMainline).   
                  
   /* If there is no existing convergent main line */   
   IF ocMainline <> "" THEN
   DO:      
      /* To Create dpmember or orderaction*/
      
      FOR EACH DPMember WHERE
          DPMember.HostTable = "MobSub" AND
          DPMember.KeyValue  = STRING(bOrder.MsSeq) AND
          DPMember.ValidTo   > TODAY AND
          DPMember.ValidTo  >= DPMember.ValidFrom EXCLUSIVE-LOCK:

          ASSIGN DPMember.ValidTo = DATE(MONTH(TODAY), 1, YEAR(TODAY)) - 1.
      END.
      fCreateAddLineDiscount(bOrder.MsSeq,
                             bOrder.CliType,
                             TODAY,
                             bDiscountPlan.DPRuleID).
      FIND FIRST lbMobSubMain WHERE
                 lbMobSubMain.Brand = gcBrand AND
                 lbMobSubMain.Cli   = ocMainline NO-LOCK NO-ERROR.
      IF AVAIL lbMobSubMain THEN
      DO:
         FIND FIRST lbOrderMain WHERE 
                    lbOrderMain.MsSeq   = lbMobSubMain.MsSeq AND
                    lbOrderMain.CustNum = lbMobSubMain.AgrCust AND
                    lbOrderMain.Cli     = lbMobSubMain.Cli AND
                    lbOrderMain.CliType = lbMobSubMain.CliTYpe NO-LOCK NO-ERROR.
         IF AVAIL lbOrderMain THEN
         DO:            
            RUN pFillTempData.
         END.
      END.                        
         /* Go to next record */
      ASSIGN ocNext = "Next".                     
   END. 
   ELSE 
   DO: 
      /* This code is required for the scenario like
         addline is delivered, convergent mainline is ongoing,
         cont25 is also delivered so it should give convergent
         discount when convergent main line is delivered but not
         give Additional line mobile only discount*/
      IF fCheckOngoingConvergentOrder(OrderCustomer.CustIDType,
                                      OrderCustomer.CustID,
                                      bOrder.CliType) THEN
         ASSIGN ocNext = "Next".
   END.

END PROCEDURE.

PROCEDURE pFinalMobOnlyProcess:   
   DEFINE OUTPUT PARAMETER ocMainline    AS CHAR NO-UNDO.
   DEFINE OUTPUT PARAMETER ocNext        AS CHAR NO-UNDO.

   DEFINE BUFFER lbMobSubMain FOR MobSub.   

   RUN pMobOnlyCheck(OrderCustomer.CustIDType,OrderCustomer.CustID,bOrder.CliType, OUTPUT ocMainline).
   
   /* If there is existing convergent main line */                     
   IF ocMainline <> "" THEN
   DO:         
      IF CAN-FIND(FIRST SubsTerminal WHERE
                        SubsTerminal.Brand = gcBrand AND
                        SubsTerminal.OrderID = bOrder.OrderId NO-LOCK) THEN 
      DO:
         ASSIGN ocNext = "Next".
         RETURN.
      END.
      
      /* To delete any other discount */
      FOR EACH DPMember WHERE
          DPMember.HostTable = "MobSub" AND
          DPMember.KeyValue  = STRING(bOrder.MsSeq) AND
          DPMember.ValidTo   > TODAY AND
          DPMember.ValidTo  >= DPMember.ValidFrom EXCLUSIVE-LOCK:

          ASSIGN DPMember.ValidTo = DATE(MONTH(TODAY), 1, YEAR(TODAY)) - 1.
      END.
      
      fCreateAddLineDiscount(bOrder.MsSeq,
                             bOrder.CliType,
                             TODAY,
                             bDiscountPlanMob.DPRuleID).

      FIND FIRST lbMobSubMain WHERE
                 lbMobSubMain.Brand = gcBrand AND
                 lbMobSubMain.Cli   = ocMainline NO-LOCK NO-ERROR.
      IF AVAIL lbMobSubMain THEN
      DO:
         FIND FIRST lbOrderMain WHERE 
                    lbOrderMain.MsSeq   = lbMobSubMain.MsSeq AND
                    lbOrderMain.CustNum = lbMobSubMain.AgrCust AND
                    lbOrderMain.Cli     = lbMobSubMain.Cli AND
                    lbOrderMain.CliType = lbMobSubMain.CliTYpe NO-LOCK NO-ERROR.
         IF AVAIL lbOrderMain THEN
         DO:            
            RUN pFillTempData.
         END.
      END.            
         
      /* Go to next record */
      ASSIGN ocNext = "Next".                     
   END.                      
END PROCEDURE.

PROCEDURE pFillTempData:        

    CREATE tt-data.
    ASSIGN tt-data.addlinemsisdn     = bOrder.Cli
           tt-data.addlineclitype    = bOrder.CliType
           tt-data.addlinesaleschnl  = bOrder.OrderChannel
           tt-data.addlineordcrstamp = bOrder.CrStamp
           tt-data.mainmsisdn        = lbOrderMain.Cli
           tt-data.mainclitype       = lbOrderMain.CliType
           tt-data.mainsaleschnl     = lbOrderMain.OrderChannel
           tt-data.mainordcrstamp    = lbOrderMain.CrStamp.

END PROCEDURE.


