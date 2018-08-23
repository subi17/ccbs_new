/* rating_package.i    31.01.11/aam separated from man_rate2/onlinereader

   In this version only one package can be used for one cdr. Includes though
   possibility to be changed so that one cdr can be spread across many
   packages; all counter handling functions return the amount that was left
   over from that counter (ldPackageAmt), and total price is calculated from 
   subtotals.
   One subscription may have several overlapping packages which are used in 
   the priority order defined in SLGAnalyse. 
*/

&GLOBAL-DEFINE MOBILE_SUBTYPES                 "CONTD,CONTF,CONTS,CONTFF,CONTSF,CONT6,CONT7,CONT8,CONT9,CONT10,CONT15,CONT23,CONT24,CONT25,CONT26,CONT27,CONT28,CONT29,CONT31,CONT33,CONT34"
&GLOBAL-DEFINE ADSL-CONVERGENT-SUBTYPES        "CONTDSL35,CONTDSL39,CONTDSL40,CONTDSL45,CONTDSL48,CONTDSL52,CONTDSL58,CONTDSL59,CONTDSL99,CONTDSL3G,CONTDSL7G,CONTDSL2G"
&GLOBAL-DEFINE FIBER-CONVERGENT-SUBTYPES-LIST1 "CONTFH35_50,CONTFH39_50,CONTFH40_50,CONTFH45_50,CONTFH48_50,CONTFH52_50,CONTFH58_50,CONTFH59_50,CONTFH99_50,CONTFH3G_50,CONTFH7G_50,CONTFH2G_50"
&GLOBAL-DEFINE FIBER-CONVERGENT-SUBTYPES-LIST2 "CONTFH45_300,CONTFH49_300,CONTFH50_300,CONTFH55_300,CONTFH58_300,CONTFH62_300,CONTFH68_300,CONTFH69_300,CONTFH109_300,CONTFH3G_300,CONTFH7G_300,CONTFH2G_300"
&GLOBAL-DEFINE FIBER-CONVERGENT-SUBTYPES-LIST3 "CONTFH65_1000,CONTFH69_1000,CONTFH76_1000,CONTFH82_1000,CONTFH89_1000,CONTFH129_1000,CONTFH3G_1000,CONTFH7G_1000,CONTFH2G_1000"
&GLOBAL-DEFINE FIBER-CONVERGENT-SUBTYPES-LIST4 "CONTFHNB109_300,CONTFHNB2G_300,CONTFHNB3G_300,CONTFHNB45_300,CONTFHNB58_300,CONTFHNB62_300,CONTFHNB69_300,CONTFHNB7G_300"
&GLOBAL-DEFINE FIBER-CONVERGENT-SUBTYPES-LIST5 "CONTDSLTB59,CONTFHTB59_50,CONTFHTB69_300,CONTFHTB89_1000,CONTFHNBTB69_300"

&GLOBAL-DEFINE DSS2_SUBTYPES "CONTS,CONTM2,CONTM,CONTSF,CONT15,CONTDSL45,CONTDSL52,CONTFH45_50,CONTFH52_50,CONTFH55_300,CONTFH62_300,CONTDSL58,CONTDSL59,CONTFH58_50,CONTFH59_50,CONTFH68_300,CONTFH69_300,CONT28,CONTFH89_1000,CONTFH82_1000,CONT29,CONTDSL99,CONTFH99_50,CONTFH109_300,CONTFH129_1000,CONTDSL3G,CONTFH3G_50,CONTFH3G_300,CONTFH3G_1000,CONTDSL7G,CONTFH7G_50,CONTFH7G_300,CONTFH7G_1000,CONTDSL2G,CONTFH2G_50,CONTFH2G_300,CONTFH2G_1000,CONTFHNB109_300,CONTFHNB2G_300,CONTFHNB3G_300,CONTFHNB7G_300,CONTFHNB45_300,CONTFHNB58_300,CONTFHNB62_300,CONTFHNB69_300"

FUNCTION fIncludedUnit RETURNS DEC
   (iiInclUnit AS INT):
 
   CASE iiInclUnit:
   WHEN {&INCLUNIT_MEGABYTE} OR WHEN {&INCLUNIT_GIGABYTE} THEN DO:
      IF ttCall.PPFlag > 0 THEN DO:
         IF ((ttCall.DataIn + ttCall.DataOut) * 1024) > 2147397248 THEN
            RETURN 2147397248.
         ELSE RETURN ((ttCall.DataIn + ttCall.DataOut) * 1024).
      END.
      ELSE
         RETURN (ttCall.DataIn + ttCall.DataOut).
   END. /* WHEN {&INCLUNIT_MEGABYTE} THEN DO: */
   WHEN {&INCLUNIT_QUANTITY} OR WHEN {&INCLUNIT_BDEST_QTY} THEN
      RETURN 1.
   WHEN {&INCLUNIT_AMOUNT} THEN
      RETURN ttCall.Amount.
   OTHERWISE RETURN c_dur.
   END CASE.
    
END FUNCTION.

FUNCTION fPackageCalculation RETURNS LOGIC:

   DEF VAR llPackageUsed         AS LOG  NO-UNDO.
   DEF VAR lcSLGroup             AS CHAR NO-UNDO.
   DEF VAR liSLGAType            AS INT  NO-UNDO.
   DEF VAR lcSLGroupList         AS CHAR NO-UNDO.
   DEF VAR lcSLGATypeList        AS CHAR NO-UNDO.
   DEF VAR llServLimit           AS LOG  NO-UNDO.
   DEF VAR lcInBDest             AS CHAR NO-UNDO.
   DEF VAR lcOutBDest            AS CHAR NO-UNDO.
   DEF VAR llServiceGrp          AS LOG  NO-UNDO.
   DEF VAR liInclUnit            AS INT  NO-UNDO.
   DEF VAR liSLSeq               AS INT  NO-UNDO.
   DEF VAR ldPackageAmt          AS DEC  NO-UNDO.
   DEF VAR ldTotalPrice          AS DEC  NO-UNDO.
   DEF VAR liSLGPacket           AS INT  NO-UNDO.
   DEF VAR ldePCCBalance         AS DEC  NO-UNDO.
   DEF VAR liUnitUsed            AS INT  NO-UNDO.
   DEF VAR liSLGAUsed            AS INT  NO-UNDO. 
   DEF VAR ldLimitTo             AS DEC  NO-UNDO.
   DEF VAR ldAmtUsed             AS DEC  NO-UNDO.
   DEF VAR lcNewGroupList        AS CHAR NO-UNDO.
   DEF VAR lcNewTypeList         AS CHAR NO-UNDO.
   DEF VAR llUpsell              AS LOG NO-UNDO. 
   DEF VAR liPrevious            AS INT  NO-UNDO.
   DEF VAR llVoice_Data_subs_DSS AS LOG  NO-UNDO.
   DEF VAR llActiveDSS           AS LOG  NO-UNDO.
   DEF VAR llActiveDSS_UPSELL    AS LOG  NO-UNDO.
   DEF VAR lcPreviousGroup       AS CHAR NO-UNDO.
   DEF VAR lcPreviousType        AS CHAR NO-UNDO.
   DEF VAR lcOrigBillCode        AS CHAR NO-UNDO.
   DEF VAR liBDestLimit          AS INT NO-UNDO. 
   DEF VAR liBDestAmt            AS INT NO-UNDO. 
   DEF VAR liMSID                AS INT NO-UNDO. 
   DEF VAR liTime                AS INT NO-UNDO. 
   DEF VAR ldtDate               AS DATE NO-UNDO. 
   DEF VAR liCallPeriod          AS INT NO-UNDO. 
   DEF VAR ldeEndTs              AS DEC NO-UNDO. 
   DEF VAR lcCliTypeList         AS CHAR NO-UNDO.

   DEFINE BUFFER bf_Customer FOR Customer.
   DEFINE BUFFER bf_CustCat  FOR CustCat.

   ASSIGN	
      ttCall.BillCode = bsub-prod
      lcOrigBillCode  = bsub-prod
      lcCliTypeList   = {&MOBILE_SUBTYPES}                 + "," +
                        {&ADSL-CONVERGENT-SUBTYPES}        + "," +
                        {&FIBER-CONVERGENT-SUBTYPES-LIST1} + "," +
                        {&FIBER-CONVERGENT-SUBTYPES-LIST2} + "," +
                        {&FIBER-CONVERGENT-SUBTYPES-LIST3} + "," +
                        {&FIBER-CONVERGENT-SUBTYPES-LIST4} + "," +
                        {&FIBER-CONVERGENT-SUBTYPES-LIST5}

      ldPackageAmt    = 0
      ldTotalPrice    = 0
      liUnitUsed      = ?
      liSLGAUsed      = ?.
   
   IF ttCall.MSCID = "NRTRDE" THEN DO:
      ttCall.ErrorCode = 0.
      RETURN FALSE. 
   END.
   
   fPacketAnalyse(INPUT  MSOwner.cliType,
                  INPUT  ttCall.BillCode,
                  INPUT  ttCall.RateCCN,
                  INPUT  ttCall.Bdest,
                  INPUT  ttCall.Datest,
                  OUTPUT lcSLGroupList,
                  OUTPUT lcSLGATypeList).

   IF lcSLGroupList = "" AND 
      LOOKUP(MSOwner.CLIType,"CONT2") > 0 THEN 
      fPacketAnalyse(INPUT  MSOwner.cliType,
                     INPUT  ttCall.BillCode,
                     INPUT  0,
                     INPUT  "",
                     INPUT  ttCall.Datest,
                     OUTPUT lcSLGroupList,
                     OUTPUT lcSLGATypeList).
      
   llPackageUsed = FALSE.               

   IF LOOKUP(MSOwner.CLIType,lcCliTypeList) > 0 THEN DO:
      FOR EACH ttServiceLimit NO-LOCK WHERE
          {Func/dss_search.i "ttServiceLimit.GroupCode"},
          FIRST MServiceLimit NO-LOCK WHERE
                MServiceLimit.CustNum  = MSOwner.CustNum      AND
                MServiceLimit.SlSeq    = ttServiceLimit.SlSeq AND
                MServiceLimit.FromTS  <= CallTimeStamp        AND
                MServiceLimit.EndTS   >= CallTimeStamp:

         IF ttServiceLimit.GroupCode = {&DSS} OR
            (ttServiceLimit.GroupCode = "DSS2" AND
             LOOKUP(MSOwner.CLIType,{&DSS2_SUBTYPES}) > 0) THEN
            llVoice_Data_subs_DSS = TRUE.
      END. /* FOR FIRST ttServiceLimit NO-LOCK WHERE */
   END. /* IF MSOwner.CLIType = "CONT6" THEN DO: */

   /* if more than one bundle possible then check what are active for 
      this subscription -> try to fill the bundle that has lower priority 
      before starting to fill the next bundle */
   IF NOT llVoice_Data_subs_DSS AND
      LOOKUP(MSOwner.CLIType,lcCliTypeList) > 0
      AND NUM-ENTRIES(lcSLGroupList) > 1 THEN DO:
      
      ASSIGN
         lcNewGroupList = ""
         lcNewTypeList  = ""
         llUpsell = FALSE.

      DO liSLGPacket = 1 TO NUM-ENTRIES(lcSLGroupList):
      
         IF LOOKUP(ENTRY(liSLGPacket,lcSLGATypeList),"1,4,6") = 0 THEN NEXT.
         
         FOR EACH  ttServiceLimit NO-LOCK WHERE
                   ttServiceLimit.GroupCode = ENTRY(liSLGPacket,lcSLGroupList),
             FIRST MServiceLimit NO-LOCK WHERE
                   MServiceLimit.MsSeq = MSOwner.MsSeq AND
                   MServiceLimit.DialType = liDialType AND
                   MServiceLimit.SlSeq    = ttServiceLimit.SlSeq AND
                   MServiceLimit.FromTS <= CallTimeStamp AND
                   MServiceLimit.EndTS  >= CallTimeStamp:
        
            ASSIGN 
               lcNewGroupList = lcNewGroupList + 
                                (IF lcNewGroupList > "" THEN "," ELSE "") + 
                                ttServiceLimit.GroupCode 
               lcNewTypeList  = lcNewTypeList + 
                                (IF lcNewTypeList > "" THEN "," ELSE "") + 
                                ENTRY(liSLGPacket,lcSLGATypeList).
                                  
            /* if upsell gets full then last base/bono bundle is used again */
            IF ENTRY(liSLGPacket,lcSLGATypeList) NE "6"
            THEN ASSIGN 
               lcPreviousGroup = ttServiceLimit.GroupCode 
               lcPreviousType  = ENTRY(liSLGPacket,lcSLGATypeList)
               llUpsell = FALSE.
            ELSE llUpsell = TRUE.
         END.                        
      END.

      IF lcNewGroupList > "" THEN ASSIGN
         lcNewGroupList = lcNewGroupList + "," +
                          lcPreviousGroup WHEN llUpsell
         lcNewTypeList  = lcNewTypeList  + "," +
                          lcPreviousType WHEN llUpsell
         lcSLGroupList  = lcNewGroupList
         lcSLGATypeList = lcNewTypeList.
   END.
   ELSE IF llVoice_Data_subs_DSS         AND 
           NUM-ENTRIES(lcSLGroupList) > 1 AND 
           LOOKUP("FIX_VOICE1000", lcSLGroupList) > 0 THEN 
   DO:
      FIND FIRST bf_Customer WHERE bf_Customer.CustNum = MSOwner.CustNum NO-LOCK NO-ERROR.
      IF AVAIL bf_Customer THEN 
         FIND FIRST bf_CustCat WHERE bf_CustCat.Brand EQ Syst.Var:gcBrand AND bf_CustCat.Category EQ bf_Customer.Category NO-LOCK NO-ERROR.

      IF NOT (AVAIL bf_CustCat AND bf_CustCat.Pro) THEN 
      DO liSLGPacket = 1 TO NUM-ENTRIES(lcSLGroupList):

         IF LOOKUP(ENTRY(liSLGPacket,lcSLGroupList), "FIX_VOICE1000") > 0 THEN 
             NEXT.

         ASSIGN 
             lcNewGroupList = lcNewGroupList + (IF lcNewGroupList > "" THEN "," ELSE "") + ENTRY(liSLGPacket,lcSLGroupList) 
             lcNewTypeList  = lcNewTypeList  + (IF lcNewTypeList  > "" THEN "," ELSE "") + ENTRY(liSLGPacket,lcSLGATypeList).
      END.
      ELSE
         ASSIGN 
             lcNewGroupList = lcSLGroupList
             lcNewTypeList  = lcSLGATypeList.

      ASSIGN
         lcSLGroupList  = lcNewGroupList
         lcSLGATypeList = lcNewTypeList.
   END.

   ASSIGN
      lcSLGroupList = TRIM(lcSLGroupList,",")
      lcSLGATypeList = TRIM(lcSLGATypeList,",").

   PACKET:
   REPEAT liSLGPacket = 1 TO NUM-ENTRIES(lcSLGroupList):
         
      ASSIGN 
         lcSLGroup = ENTRY(liSLGPacket,lcSLGroupList)
         liSLGAType = INT(ENTRY(liSLGPacket,lcSLGATypeList)).

      IF lcSLGroup = "" THEN NEXT. 
        
      /* Servicelimit group */ 
      IF liSLGAType = 1 THEN DO:

         /* If DSS is active and Dialtype is GPRS then
            all data will go into DSS */
         IF liDialtype = {&DIAL_TYPE_GPRS} AND llActiveDSS THEN NEXT PACKET.

         /* call forwarding is handled like normal calls */
         IF liDialtype = 12 
         THEN liDialtype = 4.
         
         IF liDialtype EQ 4 AND 
            (lcSLGroup BEGINS "CONTS" OR
             lcSLGroup EQ "CONT24" OR
             lcSLGroup EQ "CONT23") AND
             NOT CAN-FIND(FIRST ServiceLimit WHERE
                                ServiceLimit.GroupCode = lcSLGroup       AND
                                ServiceLimit.DialType  = liDialType      AND
                                ServiceLimit.ValidFrom <= ttCall.DateSt  AND
                                ServiceLimit.ValidTo   >= ttCall.DateSt)
         THEN liDialtype = 0.

         llServLimit = fCheckTarget(INPUT  MSOwner.MsSeq,
                                    INPUT  MSOwner.Custnum,
                                    INPUT  liDialtype,
                                    INPUT  CallTimeStamp,
                                    INPUT  lcOrigBillCode,
                                    INPUT  lcSLGroup,
                                    OUTPUT liMSID,
                                    OUTPUT lcInBDest,
                                    OUTPUT lcOutBDest,
                                    OUTPUT liSLSeq,
                                    OUTPUT liInclUnit,
                                    OUTPUT ldLimitTo,
                                    OUTPUT liBDestLimit).
       
         IF llServLimit THEN DO WHILE TRUE:

            IF NOT llPackageUsed THEN ASSIGN 
               liUnitUsed = liInclUnit
               liSLGAUsed = liSLGAType.
            ELSE IF liInclUnit NE liUnitUsed OR
                    liSLGAType NE liSLGAUsed
            THEN NEXT PACKET.
            
            IF NOT llPackageUsed THEN DO:
               ldPackageAmt = fIncludedUnit(liInclUnit).   
           
               IF liBDestLimit > 0 OR
                  liInclUnit EQ {&INCLUNIT_BDEST_QTY} THEN DO:
                  
                  liBDestAmt = f{&CounterHandling}CheckSLCounterItem
                                 (MSOwner.MsSeq,
                                  liSLseq,
                                  CallTimeStamp,
                                  ttCall.GsmBnr).
               
                  IF liInclUnit EQ {&INCLUNIT_BDEST_QTY} THEN ASSIGN
                     ldPackageAmt = liBDestAmt
                     liBDestAmt = 0
                     liBDestLimit = 0.
               END.
            END.

            ASSIGN
               llPackageUsed = TRUE
               ldAmtUsed = ldPackageAmt.
 
            llServiceGrp = f{&CounterHandling}IsServiceLimitAllowed
                                    ( liMSID,
                                      MSOwner.MsSeq,
                                     (IF liDialtype = {&DIAL_TYPE_GPRS} AND
                                      llActiveDSS THEN MSOwner.Custnum ELSE 0),
                                           ttCall.InvSeq,
                                           liSLSeq,
                                           liInclUnit,
                                           ldLimitTo,
                                           CallTimeStamp,
                                           liSLGAType,
                                           liBDestLimit,
                                           liBDestAmt,  
                                     INPUT-OUTPUT ldPackageAmt).

            ASSIGN
               rate-plcode = ""
               ldAmtUsed = ldAmtUsed - ldPackageAmt.

            IF llServiceGrp THEN DO:

               /* if a voice cdr fits only partially to package then 
                  the leftover is rated with normal tariff but without 
                  starting charge if it's the last available package */
               IF ldPackageAmt > 0 AND 
                 (liDialType = {&DIAL_TYPE_VOICE} OR
                  liDialType = {&DIAL_TYPE_FIXED_VOICE}) AND
                  liSLGPacket EQ NUM-ENTRIES(lcSLGroupList)
               THEN DO:
                  c_dur = ldPackageAmt.
                  fTariff().
                  IF rc ne 0 THEN DO:
                     ttCall.ErrorCode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}.
                     RETURN FALSE. 
                  END.
          
                  ASSIGN 
                     ldTotalPrice = ldTotalPrice + bprice - base
                     base = 0
                     ldPackageAmt = 0.
               END.
                
               /* note; so far all gprs traffic is rated with zero price and
                  leftover is not necessarily handled at all here (if this is
                  the last package in the list), but if that changes, i.e. 
                  there will be a rate for outside package traffic then also
                  gprs leftover needs to be taken care of */
                  
               ASSIGN
                  ttCall.BDest   = lcInBDest
                  b_dest         = lcInBDest
                  bsubs          = lcInBDest
                  ttCall.DCType  = "1"
                  ttCall.DCEvent = lcSLGroup.

 
               IF liDialType = {&DIAL_TYPE_VOICE} OR
                  liDialType = {&DIAL_TYPE_FIXED_VOICE} OR
                  liDialType = {&DIAL_TYPE_GPRS} THEN
                     c_dur = ldAmtUsed.
            END.

            ELSE DO:
            
               /* there are other packages available */
               IF liSLGPacket < NUM-ENTRIES(lcSLGroupList)                                  AND
                  (lcOutBDest = ""                                                          OR 
                   (MSOwner.CLIType BEGINS "CONTF" AND NOT MSOwner.CLIType BEGINS "CONTFH") OR 
                   LOOKUP("FIX_VOICE1000",lcSLGroupList) > 0)                               THEN 
               DO:
                  IF lcOutBDest = "" THEN 
                      llPackageUsed = FALSE.
                      
                  NEXT PACKET.
               END.
               
               ASSIGN
                  ttCall.BDest = IF lcOutBDest ne "" 
                                 THEN lcOutBDest
                                 ELSE ttCall.bdest
                  b_dest       = IF lcOutBDest ne "" 
                                 THEN lcOutBDest
                                 ELSE b_dest
                  bsubs        = IF lcOutBDest ne "" 
                                 THEN lcOutBDest
                                 ELSE bsubs.
            END.

            fTariff().

            IF rc ne 0 THEN DO:
               ttCall.ErrorCode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}.
               RETURN FALSE. 
            END.
            
            ASSIGN 
               ldTotalPrice = ldTotalPrice + bprice
               ttCall.BillCode = bsub-prod.
            LEAVE.
         END.
         
      END.   /* SLGAType=1 */
            
      /* Daycampaign */ 
      ELSE IF liSLGAType = 2 THEN DO:
                 
         FIND FIRST DayCampaign WHERE
                    DayCampaign.Brand      = MSOwner.Brand  AND
                    DayCampaign.DCEvent    = lcSLGroup      AND 
                    DayCampaign.ValidFrom <= ttCall.Datest  AND 
                    DayCampaign.ValidTo   >= ttCall.Datest 
         NO-LOCK NO-ERROR.

         IF Avail DayCampaign AND 
            CAN-FIND(FIRST DCCLI WHERE 
                           DCCLI.MsSeq   = MSOwner.MsSeq AND 
                           DCCLI.DCEvent = lcSLGroup AND
                           DCCLI.ValidTo >= ttCall.DateSt AND
                           DCCLI.ValidFrom <= ttCall.DateSt)
         THEN DO:
               
            IF DayCampaign.WeekDay > "" AND
               INDEX(DayCampaign.WeekDay,STRING(WEEKDAY(ttCall.DateSt))) = 0
            THEN NEXT PACKET.

            IF NOT llPackageUsed THEN ASSIGN 
               liUnitUsed = DayCampaign.InclUnit 
               liSLGAUsed = liSLGAType.
            ELSE IF DayCampaign.InclUnit NE liUnitUsed OR
                    liSLGAType NE liSLGAUsed
            THEN NEXT PACKET.
  
            llPackageUsed = TRUE.     
            
            IF      DayCampaign.InclUnit = {&INCLUNIT_AMOUNT} 
            THEN ldPackageAmt = bprice - base.
            ELSE IF DayCampaign.InclUnit = {&INCLUNIT_SECOND}            
            THEN ldPackageAmt = ttCall.BillDur.

            IF DayCampaign.DurType = 1 THEN DO:
               f{&CounterHandling}Price4Day 
                                  (INPUT  MSOwner.MsSeq,
                                   INPUT  ttCall.CLI,
                                   INPUT  ttCall.Datest,
                                   INPUT  ttCall.Timest,
                                   INPUT  lcSLGroup,
                                   INPUT  ldPackageAmt,
                                   INPUT  ttCall.CCN,
                                   INPUT  TRUE,
                                   OUTPUT ldPackageAmt,
                                   INPUT-OUTPUT ttCall.BillCode,
                                   OUTPUT ttcall.ccn) .
               /* special case for yoigoyoigo */
               IF ttCall.SPOCmt = 30 AND ttCall.BillCode = "YOIGOYOIGO" THEN 
                  ttCall.BillCode = "CFYOIGOYOIGO". 
            END.
    
            ELSE IF DayCampaign.DurType = 2 THEN DO:
               fDayCampaignAnalyse (INPUT  MSOwner.MsSeq,
                                    INPUT  ttCall.Datest,
                                    INPUT  ttCall.Timest,
                                    INPUT  lcSLGroup,
                                    INPUT  ldPackageAmt,
                                    INPUT  ttCall.ccn,
                                    INPUT  ttCall.VatIncl,
                                    OUTPUT ldPackageAmt ,
                                    INPUT-OUTPUT ttCall.BillCode,
                                    INPUT-OUTPUT ttCall.CCN).
            END.           

            /*************************************
             *  6/1 Amount  free up to            *
             *  6/2 Amount  free after            *
             *  2/1 Seconds free up to            *
             *  2/2 Secondf free after            *
             *************************************/

            IF  DayCampaign.InclUnit = 6 AND DayCampaign.CalcMethod = 1  
            THEN bPrice = bprice - ldPackageAmt.
            ELSE IF DayCampaign.InclUnit = 6 AND DayCampaign.CalcMethod = 2  
            THEN bPrice = ldPackageAmt.     
            ELSE IF DayCampaign.InclUnit = 2 AND DayCampaign.CalcMethod = 1  
            THEN DO: 
               bPrice = (bprice - base)  * 
                         ((ttCall.BillDur - ldPackageAmt) / ttCall.BillDur).

               IF bprice = ? then bprice = 0.
            END.
            ELSE IF DayCampaign.InclUnit = 2 AND DayCampaign.CalcMethod = 2  
            THEN bPrice = (bprice - base) * (ldPackageAmt / ttCall.BillDur).   
 
            IF DayCampaign.InclStartCharge THEN bprice = bprice + base.
            ELSE base = 0.

            ASSIGN
               ldTotalPrice = bprice
               ldPackageAmt = 0.
         END.
               
      END.  /* SLGAType=2 */

      ELSE IF liSLGAType = 4 THEN DO:
         IF llActiveDSS THEN NEXT PACKET.

         DAYCAMP:
         FOR EACH ttServiceLimit NO-LOCK WHERE
                  ttServiceLimit.GroupCode = lcSLGroup:
            IF LOOKUP(ttServiceLimit.GroupCode,{&DSS_BUNDLES}) > 0 THEN
               FIND FIRST MServiceLimit NO-LOCK WHERE
                   MServiceLimit.Custnum  = MSOwner.Custnum AND
                   MServiceLimit.DialType = liDialType AND
                   MServiceLimit.SlSeq   = ttServiceLimit.SlSeq AND
                   MServiceLimit.FromTS <= CallTimeStamp AND
                   MServiceLimit.EndTS  >= CallTimeStamp NO-ERROR.
            ELSE
               FIND FIRST MServiceLimit NO-LOCK WHERE
                   MServiceLimit.MsSeq    = MSOwner.MsSeq AND
                   MServiceLimit.DialType = liDialType AND
                   MServiceLimit.SlSeq   = ttServiceLimit.SlSeq AND
                   MServiceLimit.FromTS <= CallTimeStamp AND
                   MServiceLimit.EndTS  >= CallTimeStamp NO-ERROR.

            IF NOT AVAIL MServiceLimit THEN NEXT.
            ELSE IF LOOKUP(ttServiceLimit.GroupCode,{&DSS_BUNDLES}) > 0 THEN
               llActiveDSS = TRUE.

            IF NOT llPackageUsed THEN ASSIGN 
               liUnitUsed = MServiceLimit.InclUnit
               liSLGAUsed = liSLGAType.

            ELSE IF MServiceLimit.InclUnit NE liUnitUsed OR
                    LOOKUP(STRING(liSLGAUsed),
                           {&PERCONTRACT_RATING_PACKAGE}) = 0
            THEN NEXT.

            liCallPeriod = YEAR(ttCall.Datest) * 100 + 
                           MONTH(ttCall.Datest).
             
            DO WHILE TRUE:
                     
            /* get current value from counter */
            ldePCCBalance = f{&CounterHandling}PCCQuery
                                 (      MServiceLimit.MSID,
                                        MSOwner.MsSeq ,              
                                  INPUT (IF liDialtype = {&DIAL_TYPE_GPRS} AND
                                         llActiveDSS 
                                         THEN MSOwner.Custnum 
                                         ELSE 0),
                                  INPUT MServiceLimit.Slseq,
                                        liCallPeriod).

            IF MServiceLimit.InclUnit = {&INCLUNIT_MEGABYTE} THEN 
               ldePCCBalance = ldePCCBalance / 1024 / 1024.
            ELSE IF MServiceLimit.InclUnit = {&INCLUNIT_GIGABYTE} THEN 
               ldePCCBalance = ldePCCBalance / 1024 / 1024 / 1024.

            IF NOT llActiveDSS AND
               LOOKUP(MSOwner.CLIType,lcCliTypeList) > 0 AND 
               ldePCCBalance >= MServiceLimit.InclAmt AND 
               liSLGPacket < NUM-ENTRIES(lcSLGroupList) THEN NEXT PACKET.

            FIND FIRST ProgLimit WHERE 
                       ProgLimit.GroupCode   = ttServiceLimit.GroupCode AND 
                       ProgLimit.SLSeq       = ttServiceLimit.SLSeq     AND 
                       ProgLimit.Validto    >= ttCall.DateSt          AND
                       ProgLimit.ValidFrom  <= ttCall.Datest          AND
                       ProgLimit.LimitTo    >= ldePCCBalance          AND
                       ProgLimit.LimitFrom  <= ldePCCBalance 
            NO-LOCK NO-ERROR.

            IF AVAILABLE ProgLimit AND ProgLimit.LimitTo = ldePCCBalance THEN
            FIND FIRST ProgLimit WHERE 
                       ProgLimit.GroupCode   = ttServiceLimit.GroupCode AND 
                       ProgLimit.SLSeq       = ttServiceLimit.SLSeq     AND 
                       ProgLimit.Validto    >= ttCall.DateSt          AND
                       ProgLimit.ValidFrom  <= ttCall.Datest          AND
                       ProgLimit.LimitTo    >= ldePCCBalance + 0.0001 AND
                       ProgLimit.LimitFrom  <= ldePCCBalance + 0.0001
            NO-LOCK NO-ERROR.

            IF AVAIL ProgLimit THEN DO:

               IF NOT llPackageUsed THEN 
                  ldPackageAmt = fIncludedUnit(MServiceLimit.InclUnit).   

               CASE MServiceLimit.InclUnit:
               WHEN {&INCLUNIT_MEGABYTE} THEN 
                  ldLimitTo = ProgLimit.LimitTo * 1024 * 1024.
               WHEN {&INCLUNIT_GIGABYTE} THEN 
                  ldLimitTo = ProgLimit.LimitTo * 1024 * 1024 * 1024.
               OTHERWISE ASSIGN
                  ldLimitTo = ProgLimit.LimitTo.
               END CASE.
               
               ldAmtUsed = ldPackageAmt.     
               IF NOT f{&CounterHandling}ProgLimitAnalyse 
                                    (MServiceLimit.MSID,
                                     MServiceLimit.MsSeq,
                                     (IF liDialtype = {&DIAL_TYPE_GPRS} AND
                                      llActiveDSS THEN MSOwner.Custnum ELSE 0),
                                     MServiceLimit.slseq ,
                                     MServiceLimit.InclUnit,
                                     CallTimeStamp  ,
                                     (IF llActiveDSS THEN
                                      (MServiceLimit.InclAmt * 1024 * 1024)
                                      ELSE ldLimitTo),
                                     ttServiceLimit.GroupCode, 
                                     liCallPeriod,
                        INPUT-OUTPUT ldPackageAmt) 
               THEN NEXT PACKET.

               /* IF DSS is active and package amount does not */
               /* fit then mark Call Bdest as low speed        */
               IF llActiveDSS AND ldAmtUsed = ldPackageAmt THEN DO:
                  IF ttServiceLimit.GroupCode = {&DSS} THEN
                     ttCall.BDest = {&DSS_LOWSPEED_BDEST}.
                  ELSE
                     ttCall.BDest = {&DSS2_LOWSPEED_BDEST}.
               END.
               ELSE
                  ttCall.BDest     = ProgLimit.Bdest.

               ASSIGN 
                  llPackageUsed = TRUE
                  ldAmtUsed = ldAmtUsed - ldPackageAmt.

               IF MServiceLimit.InclUnit = {&INCLUNIT_MEGABYTE} OR
                  MServiceLimit.InclUnit = {&INCLUNIT_GIGABYTE} OR
                  MServiceLimit.InclUnit = {&INCLUNIT_SECOND}   OR
                  MServiceLimit.InclUnit = {&INCLUNIT_MINUTE} THEN 
                      c_dur = ldAmtUsed.

               ASSIGN
                  b_dest           = ttCall.BDest
                  bsubs            = ttCall.BDest
                  rate-plcode      = ""  
                  ttCall.DCType    = "4"
                  ttCall.DCEvent   = ttServiceLimit.GroupCode.

               fTariff().
            
               IF rc ne 0 THEN DO:
                  ttCall.errorcode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}.
                  RETURN FALSE. 
               END.
                     
               ldTotalPrice = ldTotalPrice + bPrice.
                           
               ttCall.BillCode = bsub-prod .

               /* to leave the while loop */
               IF llActiveDSS THEN ldPackageAmt = 0.

            END.
            
            ELSE LEAVE.
             
            IF llPackageUsed AND ldPackageAmt = 0 THEN LEAVE DAYCAMP.

            END.  /* do while */         
                   
         END.  /* DAYCAMP */
         
      END.   /* SLGAType=4 */
      
      ELSE IF liSLGAType = 6 THEN DO:
         IF llActiveDSS_UPSELL AND
            NOT (lcSLGroup BEGINS {&DSS}) THEN NEXT PACKET.
         llServLimit = fCheckTarget(INPUT  MSOwner.MsSeq,
                                    INPUT  MSOwner.CustNum,
                                    INPUT  liDialtype,
                                    INPUT  CallTimeStamp,
                                    INPUT  lcOrigBillCode, 
                                    INPUT  lcSLGroup,
                                    OUTPUT liMSID,
                                    OUTPUT lcInBDest,
                                    OUTPUT lcOutBDest,
                                    OUTPUT liSLSeq,
                                    OUTPUT liInclUnit,
                                    OUTPUT ldLimitTo,
                                    OUTPUT liBDestLimit).

         IF llServLimit THEN DO:
            IF lcSLGroup BEGINS {&DSS} THEN llActiveDSS_UPSELL = TRUE.

            IF NOT llPackageUsed THEN ASSIGN 
               liUnitUsed = liInclUnit
               liSLGAUsed = 4.
            ELSE IF liInclUnit NE liUnitUsed OR
                    LOOKUP(STRING(liSLGAUsed),
                           {&PERCONTRACT_RATING_PACKAGE}) = 0
            THEN NEXT PACKET.
 
            IF NOT llPackageUsed THEN 
               ldPackageAmt = fIncludedUnit(liInclUnit).   
        
            ldAmtUsed = ldPackageAmt.
               
            IF f{&CounterHandling}IsServiceLimitAllowed
                             ( liMSID,
                               MSOwner.MsSeq,
                               (IF liDialtype = {&DIAL_TYPE_GPRS} AND
                                llActiveDSS_UPSELL THEN MSOwner.Custnum
                                ELSE 0),
                               ttCall.InvSeq,
                               liSLSeq,
                               liInclUnit,
                               ldLimitTo,
                               CallTimeStamp,
                               liSLGAType,
                               0,
                               0,
                         INPUT-OUTPUT ldPackageAmt)
            THEN ASSIGN
               llPackageUsed = TRUE     
               ttCall.BDest  = lcInBDest
               b_dest        = lcInBDest
               bsubs         = lcInBDest
               rate-plcode   = ""
               ttCall.DCType = "6"
               ttCall.DCEvent = lcSLGroup.
            ELSE NEXT PACKET.

            ldAmtUsed = ldAmtUsed - ldPackageAmt.
            
            IF liInclUnit = {&INCLUNIT_MEGABYTE} OR
               liInclUnit = {&INCLUNIT_GIGABYTE} OR
               liInclUnit = {&INCLUNIT_SECOND}   OR
               liInclUnit = {&INCLUNIT_MINUTE} THEN 
                   c_dur = ldAmtUsed.

            fTariff().
                       
            IF rc ne 0 THEN DO:
               ttCall.errorcode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}.
               RETURN FALSE. 
            END.
                 
            ldTotalPrice = ldTotalPrice + bprice.                              

            ttCall.BillCode = bsub-prod.
         END.
         
      END. /* SLGAType 6 */
      ELSE IF liSLGAType = 8 THEN DO:
         
         /* Automatic daily contract creation for HSPA_ROAM_EU */
            
         FOR FIRST ttServiceLimit NO-LOCK WHERE
                   ttServiceLimit.GroupCode = lcSLGroup AND
                   ttServiceLimit.DialType = liDialtype:
            
            FIND FIRST MServiceLimit NO-LOCK WHERE
                       MServiceLimit.MsSeq    = MSOwner.MsSeq AND
                       MServiceLimit.DialType = liDialType AND
                       MServiceLimit.SlSeq   = ttServiceLimit.SlSeq AND
                       MServiceLimit.FromTS <= CallTimeStamp AND
                       MServiceLimit.EndTS  >= CallTimeStamp NO-ERROR.

            IF NOT AVAIL MServiceLimit THEN DO:
               
               Func.Common:mSplitTS(CallTimeStamp,
                        OUTPUT ldtDate,
                        OUTPUT liTime).
               
               ldeEndTS = Func.Common:mMake2DT(ldtDate,86399).
               
               FIND LAST MServiceLimit NO-LOCK WHERE
                         MServiceLimit.MsSeq    = MSOwner.MsSeq AND
                         MServiceLimit.DialType = liDialType AND
                         MServiceLimit.SlSeq   = ttServiceLimit.SlSeq AND
                         MServiceLimit.EndTS <= ldeEndTS AND
                         MServiceLimit.FromTS >= Func.Common:mMake2DT(ldtDate,0)
               USE-INDEX MsSeq NO-ERROR.
               IF AVAIL MServiceLimit THEN
                  ldeEndTS = Func.Common:mSecOffSet(MServiceLimit.EndTS,-1).

               CREATE mServiceLimit.
               ASSIGN
                  mServiceLimit.MSID     = NEXT-VALUE(mServiceLimit)
                  mServiceLimit.SLSeq    = ttServiceLimit.SLSeq          
                  mServiceLimit.MSSeq    = mSOwner.MSSeq       
                  mServiceLimit.Custnum  = 0
                  mServiceLimit.DialType = ttServiceLimit.DialType          
                  mServiceLimit.InclUnit = ttServiceLimit.InclUnit
                  mServiceLimit.InclAmt  = ttServiceLimit.InclAmt
                  mServiceLimit.FromTS   = Func.Common:mMake2DT(ldtDate,0)
                  mServiceLimit.EndTS    = ldeEndTS NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DELETE mServiceLimit.
               ELSE IF llDoEvent THEN 
                  fMakeCreateEvent((BUFFER mServiceLimit:HANDLE),
                                   "",
                                   Syst.Var:katun,
                                   "").

            END.
         END.

         DAYCAMP:
         FOR EACH ttServiceLimit NO-LOCK WHERE
                  ttServiceLimit.GroupCode = lcSLGroup,
             EACH MServiceLimit NO-LOCK WHERE
                  MServiceLimit.MsSeq    = MSOwner.MsSeq AND
                  MServiceLimit.DialType = liDialType AND
                  MServiceLimit.SlSeq   = ttServiceLimit.SlSeq AND
                  MServiceLimit.FromTS <= CallTimeStamp AND
                  MServiceLimit.EndTS  >= CallTimeStamp
             USE-INDEX Active BREAK BY MServiceLimit.FromTS:

            /* in case CDR is split in many packages */
            IF NOT llPackageUsed THEN ASSIGN 
               liUnitUsed = MServiceLimit.InclUnit
               liSLGAUsed = liSLGAType.
            ELSE IF MServiceLimit.InclUnit NE liUnitUsed OR
                    liSLGAUsed NE 8
            THEN NEXT.

            liCallPeriod = YEAR(ttCall.Datest) * 10000 + 
                           MONTH(ttCall.Datest) * 100 + 
                           DAY(ttCall.DateSt).
             
            DO WHILE TRUE:
                     
            /* get current value from counter */
            ldePCCBalance = f{&CounterHandling}PCCQuery
                                 (MServiceLimit.MSID,
                                  INPUT MSOwner.MsSeq ,              
                                  0, /* custnum */
                                  INPUT MServiceLimit.Slseq,
                                  liCallPeriod).

            IF MServiceLimit.InclUnit = {&INCLUNIT_MEGABYTE} THEN 
               ldePCCBalance = ldePCCBalance / 1024 / 1024.
            ELSE IF MServiceLimit.InclUnit = {&INCLUNIT_GIGABYTE} THEN 
               ldePCCBalance = ldePCCBalance / 1024 / 1024 / 1024.

            FIND FIRST ProgLimit WHERE 
                       ProgLimit.GroupCode   = ttServiceLimit.GroupCode AND 
                       ProgLimit.SLSeq       = ttServiceLimit.SLSeq     AND 
                       ProgLimit.Validto    >= ttCall.DateSt          AND
                       ProgLimit.ValidFrom  <= ttCall.Datest          AND
                       ProgLimit.LimitTo    >= ldePCCBalance          AND
                       ProgLimit.LimitFrom  <= ldePCCBalance 
            NO-LOCK NO-ERROR.

            IF AVAILABLE ProgLimit AND ProgLimit.LimitTo = ldePCCBalance THEN
            FIND FIRST ProgLimit WHERE 
                       ProgLimit.GroupCode   = ttServiceLimit.GroupCode AND 
                       ProgLimit.SLSeq       = ttServiceLimit.SLSeq     AND 
                       ProgLimit.Validto    >= ttCall.DateSt          AND
                       ProgLimit.ValidFrom  <= ttCall.Datest          AND
                       ProgLimit.LimitTo    >= ldePCCBalance + 0.0001 AND
                       ProgLimit.LimitFrom  <= ldePCCBalance + 0.0001
            NO-LOCK NO-ERROR.

            IF AVAIL ProgLimit THEN DO:
            
               /* Used to detect if upsell must be used */
               IF NOT LAST (MServiceLimit.FromTS) AND
                  ProgLimit.LimitTo > 99999999 THEN NEXT DAYCAMP.

               IF NOT llPackageUsed THEN 
                  ldPackageAmt = fIncludedUnit(MServiceLimit.InclUnit).   

               CASE MServiceLimit.InclUnit:
               WHEN {&INCLUNIT_MEGABYTE} THEN 
                  ldLimitTo = ProgLimit.LimitTo * 1024 * 1024.
               WHEN {&INCLUNIT_GIGABYTE} THEN 
                  ldLimitTo = ProgLimit.LimitTo * 1024 * 1024 * 1024.
               OTHERWISE ASSIGN
                  ldLimitTo = ProgLimit.LimitTo.
               END CASE.
               
               ldAmtUsed = ldPackageAmt.     
               IF NOT f{&CounterHandling}ProgLimitAnalyse 
                              (      MServiceLimit.MSID,
                                     MServiceLimit.MsSeq,
                                     0, /* custnum */
                                     MServiceLimit.slseq ,
                                     MServiceLimit.InclUnit,
                                     CallTimeStamp,
                                     ldLimitTo,
                                     ttServiceLimit.GroupCode, 
                                     liCallPeriod,
                        INPUT-OUTPUT ldPackageAmt) 
               THEN NEXT PACKET.

               ASSIGN 
                  ttCall.BDest = ProgLimit.Bdest
                  llPackageUsed = TRUE
                  ldAmtUsed = ldAmtUsed - ldPackageAmt.

               IF MServiceLimit.InclUnit = {&INCLUNIT_MEGABYTE} OR
                  MServiceLimit.InclUnit = {&INCLUNIT_GIGABYTE} OR
                  MServiceLimit.InclUnit = {&INCLUNIT_SECOND}   OR
                  MServiceLimit.InclUnit = {&INCLUNIT_MINUTE} THEN 
                      c_dur = ldAmtUsed.

               ASSIGN
                  b_dest           = ttCall.BDest
                  bsubs            = ttCall.BDest
                  rate-plcode      = ""  
                  ttCall.DCType    = "8"
                  ttCall.DCEvent   = ttServiceLimit.GroupCode.

               fTariff().
               
               IF rc ne 0 THEN DO:
                  ttCall.errorcode = {&CDR_ERROR_NO_RATE_PLAN_FOUND}.
                  RETURN FALSE. 
               END.
                     
               ldTotalPrice = ldTotalPrice + bPrice.
                           
               ttCall.BillCode = bsub-prod .

            END.
            
            ELSE LEAVE.
             
            IF llPackageUsed AND ldPackageAmt = 0 THEN LEAVE DAYCAMP.

            END.  /* do while */         
                   
         END.  /* DAYCAMP */
         
      END.   /* SLGAType=8 */

      IF llPackageUsed AND ldPackageAmt = 0 THEN LEAVE PACKET.
         
   END. /* PACKET */

   IF llPackageUsed THEN bprice = ldTotalPrice.

   ttCall.ErrorCode = 0.
   
   RETURN llPackageUsed.
   
END FUNCTION.

