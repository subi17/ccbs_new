/* daycampaign.i     2005/jp

   changes:          07.02.06/aam to TF
                     15.05.07/aam brand to dccli
*/
{commali.i}

DEF TEMP-TABLE ttDCCounter NO-UNDO
   LIKE DCCounter.
   
DEF BUFFER bDCSub FOR MobSub.

FUNCTION fGenerateCounters RETURN INTEGER
(INPUT iiMsseq AS INT,
 INPUT icEvent AS CHAR).
 
   DEF VAR ldDate      AS DATE NO-UNDO.
   DEF VAR ldStartDAte AS date NO-UNDO.
   DEF VAR iLoop       AS INT  NO-UNDO.
   DEF VAR liQty       AS INT  NO-UNDO.
   DEF VAR llVatIncl   AS LOG  NO-UNDO.
   
   FIND FIRST dcCli WHERE 
              dcCli.Msseq      = iiMsseq AND 
              dcCli.dcEvent    = icEvent 
              NO-LOCK NO-ERROR.
               
   FIND FIRST DayCampaign WHERE 
              DayCampaign.Brand   = gcBrand  AND
              DayCampaign.dcEvent = icEvent 
   NO-LOCK NO-ERROR.

   IF NOT AVAIL dccli OR 
      NOT AVAIL DayCampaign THEN RETURN 0.

   /* online rating generate continuously counters automatically*/

   IF DayCampaign.DCType = "1" THEN RETURN 0.
   
   ldStartDate = dcCli.ValidFrom.

   /* which limit should be taken from DayCampaign */
   llVatIncl = TRUE.
   FOR FIRST bDCSub NO-LOCK WHERE
             bDCSub.MsSeq = iiMsSeq,
       FIRST BillTarget NO-LOCK WHERE
             BillTarget.CustNum    = bDCSub.CustNum AND
             BillTarget.BillTarget = bDCSub.BillTarget,
        EACH PListConf USE-INDEX RatePlan NO-LOCK WHERE
             PListConf.Brand    = gcBrand           AND
             PListConf.RatePlan = BillTarg.RatePlan AND
             PListConf.dFrom   <= ldStartDate       AND
             PListConf.dTo     >= ldStartDate,
       FIRST PriceList OF PListConf NO-LOCK:

      llVatIncl = PriceList.InclVat.
      LEAVE.
   END.
      
   DATE:
   DO ldDate = ldStartDate TO dcCli.ValidTo:

      DO iLoop = 1 TO LENGTH(daycampaign.weekday):
         
         IF INT(SubString(daycampaign.weekday,iloop,1)) = Weekday(ldDate) 
         THEN DO TRANSACTION:
         
            FIND FIRST DCCounter WHERE 
                       DCCounter.MSSeq     = iiMsseq  AND 
                       DCCounter.dcEvent   = icEvent  AND 
                       DCCounter.dcDate    = ldDate 
            NO-LOCK NO-ERROR.
                       
            IF NOT AVAIL DCCounter THEN DO:
            
               CREATE DCCounter.
               ASSIGN
                  DCCounter.MSSeq      = iiMsseq 
                  DCCounter.dcEvent    = icEvent
                  DCCounter.dcDate     = ldDate
                  DCCounter.Amount     = 0
                  DCCounter.BillCode   = DayCampaign.BillCode
                  DCCounter.DCTarget   = DayCampaign.dcTarget
                  DCCounter.CalcMethod = DayCampaign.CalcMethod
                  DCCounter.InclUnit   = DayCampaign.InclUnit
                  DCCounter.dcType     = DayCampaign.DCType
                  DCCounter.Latest     = 0 
                  DCCounter.CCN        = DayCampaign.CCN.

               IF llVatIncl THEN    
                  DCCounter.MaxCharge = DayCampaign.MaxChargeIncl.
               ELSE 
                  DCCounter.MaxCharge = DayCampaign.MaxChargeExcl. 

               liQty = liQty + 1.    
            END.
       
            IF today + 365 < DCCounter.dcdate THEN LEAVE DATE.
            
         END.
      END.
   END.

   RETURN liQty.
   
END.

FUNCTION fRemoveCounters RETURN INTEGER
(INPUT iiMsseq  AS INT,
 INPUT icEvent  AS CHAR,
 INPUT ldtDate  AS DATE). 

   DEF VAR liQty AS INT NO-UNDO.
   
   liQty = 0 .
   
   FIND FIRST dcCli WHERE
              dcCli.Msseq   = iiMsseq AND
              dcCli.dcEvent = icEvent NO-LOCK NO-ERROR.
                               
   IF NOT AVAIL dcCli THEN DO:
       RETURN liQty.
   END.    

   FOR EACH DCCounter WHERE 
            DCCounter.Msseq   = iiMsseq AND 
            DCCounter.dcEvent = icEvent AND 
            DCCounter.dcDate  >= ldtDate .

      liQty = liQty + 1.
      
      DELETE DCCounter.
             
   END.          

   Return liqty.
END.

FUNCTION fWeekday RETURN LOGICAL
(INPUT lcWeekDay AS CHAR).

 DEF VAR iLoop AS INT NO-UNDO.

 DO iLoop = 1 to LENGTH(lcWeekday):
    
    IF INDEX("1234567",substring(lcWeekday,iloop,1)) = 0 
    THEN DO:
        RETURN FALSE.
    END.
 END.
 RETURN TRUE.
END.

FUNCTION  fDayTriggerEvent RETURN LOGICAL
(INPUT CallTimeStamp AS DEC,
 INPUT iiMSSeq       AS INT,
 INPUT icLAtest      AS CHAR):


   FIND FIRST TriggerConf WHERE
              TriggerConf.TriggerConfID = "MobCDR"        AND
              TriggerConf.EventRule     > 0               AND
              TriggerConf.ValidTo       >= Today          AND
              TriggerConf.ValidFrom     <= Today NO-LOCK NO-ERROR.
                                                                                                       
   IF AVAIL TriggerConf THEN DO:      

      CREATE TriggerEvent.
      ASSIGN
      TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
      TriggerEvent.TriggerConfID  = "MobCDR"
      TriggerEvent.EventSource    = "MODIFY"
      TriggerEvent.Created        = DateTime(Today,mtime)
      TriggerEvent.TableID        = iiMSSeq
      TriggerEvent.TableName      = "Mobcdr"
      TriggerEvent.Keyvalue       = STRING(CallTimeStamp)
      TriggerEvent.ChangedFields  = "DCCounter"
      TriggerEvent.ChangedValues  = icLatest.
   
   END.
   
   RELEASE TriggerEvent.
   
END FUNCTION.

FUNCTION fWeekdayName RETURN CHARACTER
(INPUT lcWeekDay AS CHAR).

 DEF VAR iLoop      AS INT  NO-UNDO.
 DEF VAR lcwdy      AS CHAR NO-UNDO INIT
    "Sun,Mon,Tue,Wed,Thu,Fri,Sat".

 DEF VAR lcWeekDayName AS CHAR NO-UNDO.
 DEF VAR lcWeekList AS CHAR NO-UNDO FORMAT "X(60)" .
 
 lcWeekList = "".
 DO iLoop = 1 to LENGTH(lcWeekday):
   lcWeekDayName = ENTRY(INT(Substring(lcWeekDay,iloop,1)),lcwdy).

   IF LOOKUP(lcWeekDayName,lcWeeklist) = 0 THEN 
   lcWeekList = lcWeekList +  "," + lcWeekDayName.
               
 END.
 IF Substring(lcWeekList,1,1) = "," THEN lcWeekList = Substring(lcWeeklist,2).
 lcweeklist = "  (" + lcweeklist + ")".
 return lcweeklist.
END.

FUNCTION fLatestOrNot RETURN INTEGER
   (INPUT idtDAte       AS DATE,
    INPUT iiTimeCompare AS INT,
    INPUT iiTimeCounter AS INT,
    INPUT ilFullPacket  AS LOG,
    INPUT iiMSSeq       AS INT).  

   IF ilFullPacket AND iiTimeCompare < iiTimeCounter THEN DO:

      fDayTriggerEvent(DEC(YEAR(idtDate) * 10000 + MONTH(idtDAte) * 100 + DAy(idtDate)),
                       iiMSSeq,
                       STRING(iiTimeCompare)).
      RETURN iiTimeCompare.
   END.
   ELSE IF ilFullPacket     AND iiTimeCompare > iiTimeCounter THEN  RETURN iiTimeCounter.
   ELSE IF NOT ilFullPacket AND iiTimeCompare < iiTimeCounter THEN  RETURN iiTimeCounter.
   ELSE IF NOT IlFullPAcket AND iiTimeCompare > iiTimeCounter THEN  RETURN iiTimeCompare.

END .

FUNCTION fDayCampaignAnalyse RETURN LOGICAL
(INPUT        iiMsseq    AS INT ,
 INPUT        idtDAte    AS DATE,
 INPUT        iiTime     AS INT,
 INPUT        icTarget   AS CHAR,
 INPUT        ideAmount  AS DEC,
 INPUT        iiccn      AS INT,
 INPUT        llVatIncl  AS LOG,
 OUTPUT       odeAmount  AS DEC,
 INPUT-OUTPUT oBillCode  AS CHAR,
 INPUT-OUTPUT oiccn      AS INT).
 
   DEF VAR ldMaxCharge LIKE mcdr.Mobcdr.Amount NO-UNDO .
   DEF VAR liQty       AS INT  NO-UNDO.
   
   DEF BUFFER xxCounter FOR DCCounter.
   
   FIND FIRST DCCounter WHERE
              DCCounter.Msseq    = iiMsseq  AND 
              DCCounter.dcDate   = idtDate  AND 
              DCCounter.dcEvent  = icTarget   NO-LOCK NO-ERROR.
                               
   IF NOT avail DCCounter THEN LEAVE.

   ldMaxCharge = DCCounter.MaxCharge.

   IF DCCounter.Amount = ldMaxCharge THEN DO:
   
      odeAmount = 0.
      
      IF iiTime > DCCounter.LAtest THEN DO:
   
         FIND FIRST xxCounter WHERE RECID(xxcounter) = RECID(DCCounter) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                         
         xxCounter.Latest = fLatestOrNot(DCCounter.DCDate, iitime, INT(DCCounter.Latest), TRUE, dcCounter.MSSeq).
      
         RELEASE xxCounter.
      END.
   
   END.
   ELSE DO:

      liQty = 0.
      
      DO WHILE TRUE :
   
         FIND FIRST xxCounter WHERE RECID(xxcounter) = RECID(DCCounter) 
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            
         liQty = liQty + 1.
         IF liQty > 30 THEN LEAVE.
        
         IF LOCKED(xxCounter) THEN DO:
            PAUSE 1 NO-MESSAGE.
            NEXT.                             
         END.                       

         LEAVE.
      END.   

      IF NOT AVAILABLE xxCounter THEN RETURN FALSE.

      IF ideAmount + DCCounter.Amount > ldMaxCharge THEN DO:
         ASSIGN
            odeAmount        = ldMaxCharge - DCCounter.Amount
            xxCounter.Amount = ldMaxCharge.
      END.
      ELSE DO:
         ASSIGN 
            odeAmount        = ideamount
            xxCounter.Amount = DCCounter.Amount + ideamount.
      END.
   
      xxCounter.Latest = fLatestOrNot(DCCounter.DCDate, iitime, INT(DCCounter.Latest), FALSE, dccounter.MSSeq).
   
   END.
      
   ASSIGN
      oiccn     = DCCounter.CCN
      oBillCode = DCCounter.BillCode WHEN DCCounter.BillCode NE "#OrigBillItem".
END. 
       

FUNCTION fPrice4Day RETURN LOGICAL
 (INPUT  iiMsseq    AS INT ,
  INPUT  icCli      AS CHAR,
  INPUT  idtDAte    AS DATE,
  INPUT  iiTime     AS INT,
  INPUT  icTarget   AS CHAR,
  INPUT  ideAmount  AS DEC,
  INPUT  iiccn      AS INT,
  INPUT  llVatIncl  AS LOG,
  OUTPUT odeAmount  AS DEC,
  INPUT-OUTPUT iocBillCode  AS CHAR,
  OUTPUT oiccn      AS INT).
 
   DEF VAR ldMaxCharge  LIKE mcdr.Mobcdr.Amount NO-UNDO.
   DEF VAR liQty        AS INT  NO-UNDO.
   DEF VAR liContractID AS INT  NO-UNDO.

   DEF BUFFER xxCounter    FOR DCCounter.
   
   ASSIGN odeamount = ideamount
          oiccn     = iiccn.

   FIND FIRST DCCounter WHERE
              DCCounter.Msseq    = iiMsseq  AND 
              DCCounter.dcDate   = idtDate  AND 
              DCCounter.dcTarget = icTarget
   NO-LOCK NO-ERROR.

   FIND FIRST DayCampaign WHERE
              DayCampaign.Brand   = gcBrand AND
              DayCampaign.DCEVENT = icTarget NO-LOCK NO-ERROR.
        
   IF NOT avail DCCounter THEN DO:

      CREATE DCCounter.
      ASSIGN 
         DCCounter.MSSeq      = iiMSSeq 
         DCCounter.DCDate     = idtDate 
         DCCounter.DCEvent    = DayCampaign.DCEvent
         DCCounter.DCTarget   = icTarget
         DCCounter.BillCode   = DayCamPaign.BillCode
         DCCounter.InclUnit   = DayCampaign.InclUnit
         DCCounter.CalcMethod = DayCampaign.CalcMethod
         DCCounter.dcType     = DayCampaign.DCType
         DCCounter.Amount     = 0
         DCCounter.Latest     = 0
         DCCounter.MaxCharge  = DayCamPaign.MaxChargeIncl
         DCCounter.CCN        = DayCampaign.CCN.
   END.

   ldMaxCharge =  DCCounter.MaxCharge. 

   IF DCCounter.Amount = ldMaxCharge THEN DO:
      
      odeAmount = 0.
   
      IF iiTime > DCCounter.Latest THEN DO:            
             
         FIND FIRST xxCounter WHERE RECID(xxcounter) = RECID(DCCounter) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                       
         IF NOT LOCKED(xxCounter) THEN DO:
         
            xxCounter.Latest = fLatestOrNot(DCCounter.DCDate, iitime, INT(DCCounter.Latest), TRUE, dcCounter.MSSeq).

            RELEASE xxCounter.
         
         END.
         
      END.
   
   END.

   ELSE DO:

      liQty = 0.
        
      DO WHILE TRUE :
   
         FIND FIRST xxCounter WHERE RECID(xxcounter) = RECID(DCCounter) 
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            
         liQty = liQty + 1.
         IF liQty > 30 THEN LEAVE.
        
         IF LOCKED(xxCounter) THEN DO:
            PAUSE 1 NO-MESSAGE.
            NEXT.                             
         END.                       

         LEAVE.
      END.   

      IF NOT AVAILABLE xxCounter THEN RETURN FALSE.

      IF ideAmount + DCCounter.Amount > ldMaxCharge THEN DO:
         ASSIGN
            odeAmount        = ldMaxCharge - DCCounter.Amount
            xxCounter.Amount = ldMaxCharge.
      
      END.
      ELSE DO:
         ASSIGN 
            odeAmount        = ideamount
            xxCounter.Amount = DCCounter.Amount + ideamount.
      END.
      
      xxCounter.Latest = fLatestOrNot(DCCounter.DCDate, iitime, INT(DCCounter.Latest), FALSE, dcCounter.MSSeq).
      
      RELEASE xxCounter.
   END.
   
   IF DCCounter.BillCode NE "#OrigBillItem" THEN
      iocBillCode = DCCounter.BillCode.
   
   RELEASE DCCounter.
END. 

FUNCTION fTempTablePrice4Day RETURN LOGICAL
 (INPUT  iiMsseq    AS INT ,
  INPUT  icCli      AS CHAR,
  INPUT  idtDAte    AS DATE,
  INPUT  iiTime     AS INT,
  INPUT  icTarget   AS CHAR,
  INPUT  ideAmount  AS DEC,
  INPUT  iiCCN      AS INT,
  INPUT  llVatIncl  AS LOG,
  OUTPUT odeAmount  AS DEC,
  INPUT-OUTPUT iocBillCode AS CHAR,
  OUTPUT oiCCN      AS INT):
 
   DEF VAR ldMaxCharge  LIKE mcdr.Mobcdr.Amount NO-UNDO.
   DEF VAR liContractID AS INT  NO-UNDO.

   ASSIGN 
      odeAmount  = ideAmount
      oiCCN      = iiCCN.

   FIND FIRST ttDCCounter WHERE
              ttDCCounter.Msseq    = iiMsseq  AND 
              ttDCCounter.dcDate   = idtDate  AND 
              ttDCCounter.dcTarget = icTarget NO-ERROR.

   IF NOT AVAILABLE ttDCCounter THEN DO:

      FIND FIRST DayCampaign WHERE
                 DayCampaign.Brand   = gcBrand AND
                 DayCampaign.DCEvent = icTarget NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DayCampaign THEN RETURN FALSE.           

      CREATE ttDCCounter.
      ASSIGN 
         ttDCCounter.MSSeq      = iiMSSeq 
         ttDCCounter.DCDate     = idtDate 
         ttDCCounter.DCEvent    = DayCampaign.DCEvent
         ttDCCounter.DCTarget   = icTarget
         ttDCCounter.BillCode   = DayCampaign.BillCode
         ttDCCounter.InclUnit   = DayCampaign.InclUnit
         ttDCCounter.CalcMethod = DayCampaign.CalcMethod
         ttDCCounter.dcType     = DayCampaign.DCType
         ttDCCounter.Amount     = 0
         ttDCCounter.Latest     = 0
         ttDCCounter.MaxCharge  = DayCampaign.MaxChargeIncl
         ttDCCounter.CCN        = DayCampaign.CCN.
   END.

   ldMaxCharge = ttDCCounter.MaxCharge. 

   IF ttDCCounter.Amount = ldMaxCharge THEN DO:

      odeAmount = 0.
         
      ttDCCounter.Latest = fLatestOrNot(ttDCCounter.DCDate, iitime, INT(ttDCCounter.Latest), TRUE, ttdcCounter.MSSeq).
   
   END.

   ELSE DO:
      IF ideAmount + ttDCCounter.Amount > ldMaxCharge THEN DO:
         ASSIGN
            odeAmount          = ldMaxCharge - ttDCCounter.Amount
            ttDCCounter.Amount = ldMaxCharge.

      END.
      ELSE DO:
         ASSIGN 
            odeAmount          = ideAmount
            ttDCCounter.Amount = ttDCCounter.Amount + ideAmount.
      END.
      ttDCCounter.LAtest =  fLatestOrNot(ttDCCounter.DCDate, iiTime, INT(ttDCCounter.Latest), FALSE,  ttdccounter.MSSeq).

   END.

   IF ttDCCounter.BillCode NE "#OrigBillItem" THEN
      iocBillCode = ttDCCounter.BillCode.

   RETURN TRUE.
END. 

FUNCTION fDCCounter2Temp RETURNS LOGICAL
   (INPUT iiMsSeq     AS INT,
    INPUT idaFromDate AS DATE,
    INPUT idaToDate   AS DATE):

   FOR EACH DCCounter NO-LOCK WHERE
            DCCounter.MsSeq   = iiMsSeq     AND 
            DCCounter.DCDate >= idaFromDate AND
            DCcounter.DCDate <= idaToDate:

      IF CAN-FIND(FIRST ttDCCounter WHERE
                    ttDCCounter.MSSeq    = DCCounter.MSSeq   AND 
                    ttDCCounter.DCDate   = DCCounter.DCDate  AND
                    ttDCCounter.DCTarget = DCCounter.DCTarget)
      THEN NEXT. 
      
      CREATE ttDCCounter.
      BUFFER-COPY DCCounter TO ttDCCounter.
      ttDCCounter.Amount = 0.
      ttDCCounter.Latest = 0.
   END.
    
END FUNCTION.    

FUNCTION fTemp2DCCounter RETURNS LOGICAL:

   DEF BUFFER bDblCounter FOR DCCounter.
    
   FOR EACH ttDCCounter:
   
      GetDCCounter:
      REPEAT:
         FIND FIRST DCCounter WHERE
                    DCCounter.MSSeq    = ttDCCounter.MSSeq   AND 
                    DCCounter.DCDate   = ttDCCounter.DCDate  AND
                    DCCounter.DCTarget = ttDCCounter.DCTarget
         EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

         IF LOCKED(DCCounter) THEN DO:
            PAUSE 2 NO-MESSAGE.
            NEXT GetDCCounter.
         END.
         ELSE LEAVE GetDCCounter.
      END.   

      IF NOT AVAILABLE DCCounter THEN CREATE DCCounter.
      ELSE DO:
         /* remove possible double counter */
         FIND FIRST bDblCounter WHERE
                    bDblCounter.MSSeq  = ttDCCounter.MSSeq   AND
                    bDblCounter.DCDate = ttDCCounter.DCDate AND 
                    bDblCounter.DCTarget = ttDCCounter.DCTarget  AND
                    RECID(bDblCounter) NE RECID(DCCounter)
         NO-LOCK NO-ERROR.
         IF AVAILABLE bDblCounter THEN DO:
            FIND CURRENT bDblCounter EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
            IF AVAILABLE bDblCounter THEN DELETE bDblCounter.
         END.
      END.    

      BUFFER-COPY ttDCCounter TO DCCounter.

      RELEASE DCCounter.
   END.
   
   EMPTY TEMP-TABLE ttDCCounter.
   
   RETURN TRUE. 

END FUNCTION.



