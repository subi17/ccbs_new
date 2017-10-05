/* This cron job is to provide the addline discount if the addline is
   eligible to get the discount */

{Syst/commali.i}
{Syst/eventval.i}
{Func/timestamp.i}
{Syst/tmsconst.i}
{Mm/fbundle.i}
{Mc/dpmember.i}
{Func/ftransdir.i}
{Mm/addlinedisccron.i}  

DEF VAR liIntervalHours  AS INT      NO-UNDO. 
DEF VAR ldeBegTimeStamp  AS DEC      NO-UNDO. 
DEF VAR ldeEndTimeStamp  AS DEC      NO-UNDO. 
DEF VAR ldtEndDateTime   AS DATETIME NO-UNDO.
DEF VAR lcBeginDateStamp AS CHAR     NO-UNDO. 
DEF VAR lcSpoolDir       AS CHAR     NO-UNDO. 
DEF VAR lcOutDir         AS CHAR     NO-UNDO. 
DEF VAR lcAddLineDisc    AS CHAR     NO-UNDO. 
DEF VAR lcFileName       AS CHAR     NO-UNDO. 

DEFINE TEMP-TABLE ttCollectOrderData NO-UNDO 
   FIELD OrderID    AS INT
   FIELD CLIType    AS CHAR
   FIELD MsSeq      AS INT
   FIELD CustID     AS CHAR
   FIELD CustIDType AS CHAR
   FIELD Discount   AS CHAR
   FIELD TariffType AS INT.

DEFINE TEMP-TABLE ttOutputData NO-UNDO
    FIELD AddlineCLI           AS CHAR
    FIELD AddlineCLIType       AS CHAR
    FIELD AddlineSaleschannel  AS CHAR
    FIELD AddlineOrdercrstamp  AS DEC
    FIELD MainCLI              AS CHAR
    FIELD MainCLIType          AS CHAR
    FIELD MainlineSaleschannel AS CHAR
    FIELD MainlineOrdcrstamp   AS DEC.

DEFINE STREAM strout.

ASSIGN liIntervalHours = INT(fCParam("AddLineCron","OrdCreateHours"))
       lcSpoolDir      = fCParam("AddLineCron","OutSpoolDir")
       lcOutDir        = fCParam("AddLineCron","OutDir")
       lcAddLineDisc   = {&ADDLINE_DISCOUNTS} + "," + {&ADDLINE_DISCOUNTS_HM}
       lcFileName      = lcSpoolDir + "addlinedisc_" + STRING(TODAY,"99999999") + "_" + REPLACE(STRING(TIME,"HH:MM:SS"),":","") + ".log".
       
IF liIntervalHours = ? OR
   liIntervalHours = 0 THEN
   ASSIGN liIntervalHours = 1.

ASSIGN ldeEndTimeStamp  = fMakeTS()
       ldtEndDateTime   = fTimeStamp2DateTime(ldeEndTimeStamp)
       lcBeginDateStamp = STRING(ADD-INTERVAL(ldtEndDateTime,-(liIntervalHours),"hours"))
       ldeBegTimeStamp  = fHMS2TS(DATE(ENTRY(1,lcBeginDateStamp," ")),
                                       ENTRY(2,lcBeginDateStamp," ")).

/* Collect orders delieverd within time Interval Hours 
   with additional line orderaction */
RUN pCollectOrderData.

/* Validate and Create eligible additional line discount for delivered order */
RUN pEligibleDiscountForSubscription.

IF CAN-FIND(FIRST ttOutputData NO-LOCK) THEN
DO:
   OUTPUT STREAM strout TO VALUE(lcFileName).

   PUT STREAM strout UNFORMATTED 
      "AddlineMSISDN"        "|"
      "AddlineCLIType"       "|"
      "AddlineSaleschannel"  "|"
      "AddlineOrdercrstamp"  "|"
      "MainMSISDN"           "|"
      "MainCLIType"          "|"
      "MainlineSaleschannel" "|"
      "MainlineOrdcrstamp"   SKIP.

   FOR EACH ttOutputData NO-LOCK:
      EXPORT STREAM strout DELIMITER "|" ttOutputData.
   END.

   OUTPUT STREAM strout CLOSE.

   fMove2TransDir(lcFileName, "", lcOutDir).
END.

PROCEDURE pCollectOrderData:

   FOR EACH Order NO-LOCK WHERE 
            Order.Brand      = Syst.Parameters:gcBrand   AND 
            Order.StatusCode = {&ORDER_STATUS_DELIVERED} AND
            Order.CrStamp   >= ldeBegTimeStamp           AND 
            Order.CrStamp   <= ldeEndTimeStamp           AND
            Order.PayType    = FALSE:

      IF LOOKUP(STRING(Order.OrderType),"0,1,4") = 0 THEN NEXT.

      FIND FIRST CLIType NO-LOCK WHERE
                 CLIType.Brand      EQ Syst.Parameters:gcBrand          AND
                 CLIType.CLIType    EQ Order.CLIType                    AND
                 CLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_CONVERGENT} NO-ERROR.
         IF NOT AVAIL CLIType THEN 
            FIND FIRST CLIType NO-LOCK WHERE 
                       CLIType.Brand      EQ Syst.Parameters:gcBrand          AND
                       CLIType.CLIType    EQ Order.CLIType                    AND 
                       CLIType.TariffType EQ {&CLITYPE_TARIFFTYPE_MOBILEONLY} AND 
                       CLIType.PayType    EQ {&CLITYPE_PAYTYPE_POSTPAID}      AND
                LOOKUP(CLIType.CLIType,{&ADDLINE_CLITYPES}) GT 0              NO-ERROR.

      IF NOT AVAIL CLIType THEN NEXT.           

      FIND FIRST OrderCustomer NO-LOCK WHERE
                 OrderCustomer.Brand   = gcBrand       AND
                 OrderCustomer.OrderId = Order.OrderId AND
                 OrderCustomer.RowType = {&ORDERCUSTOMER_ROWTYPE_AGREEMENT} NO-ERROR.

      IF NOT AVAIL OrderCustomer THEN NEXT.

      CREATE ttCollectOrderData.
      ASSIGN ttCollectOrderData.OrderId    = Order.OrderId
             ttCollectOrderData.CLIType    = Order.CLIType
             ttCollectOrderData.MsSeq      = Order.MsSeq
             ttCollectOrderData.CustID     = OrderCustomer.CustId
             ttCollectOrderData.CustIDType = OrderCustomer.CustIdType 
             ttCollectOrderData.TariffType = CLIType.TariffType.

   END. 

END PROCEDURE.
   
PROCEDURE pEligibleDiscountForSubscription:

   DEFINE BUFFER bMobSub  FOR MobSub.
   DEFINE BUFFER bALOrder FOR Order.

   DEF VAR lcMainLineCLI           AS CHAR NO-UNDO. 
   DEF VAR lcMainLineCLIType       AS CHAR NO-UNDO. 
   DEF VAR lcMainLineOrderChannel  AS CHAR NO-UNDO. 
   DEF VAR ldeMainLineOrderCrStamp AS DEC  NO-UNDO.
   DEF VAR lcReturnValue           AS CHAR NO-UNDO. 

   FOR EACH ttCollectOrderData EXCLUSIVE-LOCK:

      FIND FIRST MobSub NO-LOCK WHERE 
                 MobSub.MsSeq   = ttCollectOrderData.MsSeq   AND 
                 MobSub.CLIType = ttCollectOrderData.CLIType NO-ERROR.

      IF NOT AVAIL MobSub THEN DO:
         DELETE ttCollectOrderData.
         NEXT.
      END.

      ADD-LINE:
      FOR EACH bMobSub NO-LOCK WHERE 
               bMobSub.Brand   = Syst.Parameters:gcBrand AND 
               bMobSub.CustNum = MobSub.CustNum          AND 
               bMobSub.PayType = FALSE:

         IF LOOKUP(bMobSub.CLIType,{&ADDLINE_CLITYPES}) EQ 0 THEN
            NEXT ADD-LINE.

         ASSIGN lcMainLineCLI           = ""
                lcMainLineCLIType       = ""
                lcMainLineOrderChannel  = ""
                ldeMainLineOrderCrStamp = 0 
                lcReturnValue           = "".

         CASE ttCollectOrderData.TariffType:
            WHEN {&CLITYPE_TARIFFTYPE_CONVERGENT} THEN DO:
               IF NOT fCheckExistingConvergentForAddLine(ttCollectOrderData.CustIDType,
                                                         ttCollectOrderData.CustID,
                                                         bMobSub.CliType, 
                                                         OUTPUT lcMainLineCLI,
                                                         OUTPUT lcMainLineCLIType,
                                                         OUTPUT lcMainLineOrderChannel,
                                                         OUTPUT ldeMainLineOrderCrStamp) THEN 
                  NEXT ADD-LINE. 

               CASE bMobSub.CLIType:
                  WHEN "CONT10" THEN ttCollectOrderData.Discount = "DISCCONT10H".
                  WHEN "CONT15" THEN ttCollectOrderData.Discount = "DISCCONT15H".
                  WHEN "CONT25" THEN ttCollectOrderData.Discount = "DISCCONT25H".
                  WHEN "CONT26" THEN ttCollectOrderData.Discount = "DISCCONT26H".
               END CASE.
            END.
            WHEN {&CLITYPE_TARIFFTYPE_MOBILEONLY} THEN DO:
               IF NOT fCheckExistingMobileOnlyForAddLine (ttCollectOrderData.CustIDType,
                                                          ttCollectOrderData.CustID,
                                                          bMobSub.CliType, 
                                                          OUTPUT lcMainLineCLI,
                                                          OUTPUT lcMainLineCLIType,
                                                          OUTPUT lcMainLineOrderChannel,
                                                          OUTPUT ldeMainLineOrderCrStamp) THEN
                  NEXT ADD-LINE. 

               CASE bMobSub.CLIType:
                  WHEN "CONT10" THEN ttCollectOrderData.Discount = "DISCCONT10HM".
                  WHEN "CONT15" THEN ttCollectOrderData.Discount = "DISCCONT15HM".
                  WHEN "CONT25" THEN ttCollectOrderData.Discount = "DISCCONT25HM".
                  WHEN "CONT26" THEN ttCollectOrderData.Discount = "DISCCONT26HM".
               END CASE.    
            END.
         END CASE.

         /* If there are any existing additional line discounts (50% Convergent or Mobile only) 
            are available then don't create new discount. */ 
         IF fCheckAvailDiscount(STRING(bMobSub.MsSeq),bMobSub.CLIType) THEN 
            NEXT ADD-LINE.                       

         FOR EACH DCCLI NO-LOCK WHERE
                  DCCLI.MsSeq      = bMobSub.MsSeq AND
                  DCCLI.DCEvent   BEGINS "TERM"    AND
                  DCCLI.ValidTo   >= TODAY         AND
                  DCCLI.ValidFrom <= DCCLI.ValidTo AND
                  DCCLI.CreateFees = TRUE,
            FIRST DayCampaign NO-LOCK WHERE
                  DayCampaign.Brand        = Syst.Parameters:gcBrand AND
                  DayCampaign.DCEvent      = DCCLI.DCEvent           AND
                  DayCampaign.DCType       = {&DCTYPE_DISCOUNT}      AND
                  DayCampaign.TermFeeModel <> ""                     AND
                  DayCampaign.TermFeeCalc  > 0                       BY DCCLI.ValidFrom DESC:
            NEXT ADD-LINE.  
         END.

         /* Close discounts other than additional line disocunts which are available 
            to last date of previous month*/
         FOR EACH DPMember EXCLUSIVE-LOCK WHERE   
                  DPMember.HostTable  = "MobSub" AND
                  DPMember.KeyValue   = STRING(bMobSub.MsSeq) AND
                  DPMember.ValidTo   >= TODAY                 AND
                  DPMember.ValidFrom <= DPMember.ValidTo:
            
            ASSIGN DPMember.ValidTo = DATE(MONTH(TODAY), 1, YEAR(TODAY)) - 1.
         END. 

         lcReturnValue = fCreateAddLineDiscount(bMobSub.MsSeq,
                                                bMobSub.CliType,
                                                TODAY,
                                                ttCollectOrderData.Discount).

         IF NOT lcReturnValue BEGINS "ERROR" THEN DO: 
            
            FIND LAST bALOrder NO-LOCK WHERE
                      bALOrder.MsSeq      = bMobSub.MsSeq             AND
                      bALOrder.CLIType    = bMobSub.CLIType           AND
                      bALOrder.StatusCode = {&ORDER_STATUS_DELIVERED} AND
               LOOKUP(STRING(bALOrder.OrderType),"0,1,4") > 0         NO-ERROR.
            IF NOT AVAIL bALOrder THEN
                FIND LAST bALOrder NO-LOCK WHERE
                          bALOrder.MsSeq      = bMobSub.MsSeq             AND
                          bALOrder.StatusCode = {&ORDER_STATUS_DELIVERED} AND
                   LOOKUP(STRING(bALOrder.OrderType),"0,1,4") > 0         NO-ERROR.
    
            IF AVAIL bALOrder THEN DO:
               CREATE ttOutputData.
               ASSIGN ttOutputData.AddlineCLI           = bMobSub.CLI
                      ttOutputData.AddlineCLIType       = bMobSub.CLIType
                      ttOutputData.AddlineSaleschannel  = bALOrder.OrderChannel
                      ttOutputData.AddlineOrdercrstamp  = bALOrder.CrStamp
                      ttOutputData.MainCLI              = lcMainLineCLI
                      ttOutputData.MainCLIType          = lcMainLineCLIType
                      ttOutputData.MainlineSaleschannel = lcMainLineOrderChannel
                      ttOutputData.MainlineOrdcrstamp   = ldeMainLineOrderCrStamp NO-ERROR.
            END.
         END.

      END.        
      
   END.

END PROCEDURE.

