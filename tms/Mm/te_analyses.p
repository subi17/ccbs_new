/* ----------------------------------------------------------------------------
  MODULE .......: triggerevent_analysis.p
  FUNCTION .....: Analyse rows in triggerevent 
  APPLICATION ..: TMS
  AUTHOR .......: JP
  --------------------------------------------------------------------------- */
{Syst/commpaa.i}
ASSIGN
   gcBrand = "1"
   katun   = "Cron".
   
{Func/timestamp.i}
{Func/cparam2.i}
{Func/heartbeat.i}
{Syst/tmsconst.i}

DEF VAR lhField       AS HANDLE NO-UNDO.
DEF VAR liField       AS INT    NO-UNDO. 
DEF VAR ldQty         AS DEC    NO-UNDO.
DEF VAR ldDone        AS DEC    NO-UNDO.
DEF VAR ldCounter     AS DEC    NO-UNDO.
DEF VAR liActions     AS INT    NO-UNDO.
DEF VAR llMatch       AS LOG    NO-UNDO.
DEF VAR liLoop        AS INT    NO-UNDO.
DEF VAR liInterval    AS INT    NO-UNDO.
DEF VAR liTransQty    AS INT    NO-UNDO.
DEF VAR lcCurrent     AS CHAR   NO-UNDO.
DEF VAR lcStarted     AS CHAR   NO-UNDO.
DEF VAR lcLogFile     AS CHAR   NO-UNDO.
DEF VAR ldCurrent     AS DEC    NO-UNDO.
DEF VAR liDays        AS INT    NO-UNDO.
DEF VAR liSeconds     AS INT    NO-UNDO.
DEF VAR liDuration    AS INT    NO-UNDO.
DEF VAR ldValue       AS DEC    NO-UNDO.
DEF VAR ldtUpdated    AS DATE   NO-UNDO.
DEF VAR ldtLatest     LIKE TriggerEvent.Created   NO-UNDO.
DEF VAR liLimitID     AS INT    NO-UNDO.
DEF VAR llMonitor     AS LOG    NO-UNDO.
DEF VAR lcMonitor     AS CHAR   NO-UNDO.
DEF VAR ldSaveCounter AS DEC    NO-UNDO.
DEF VAR ldtPeriodEnd  AS DATE   NO-UNDO.
DEF VAR ldtPeriodStart  AS DATE NO-UNDO.
DEF VAR liQty           AS INT  NO-UNDO.
DEF VAR oiQty           AS INT  NO-UNDO.
DEF VAR llAnal          AS LOG  NO-UNDO.
DEF VAR ocText          AS CHAR NO-UNDO.
DEF VAR lrRecid         AS RECID NO-UNDO.
DEF VAR liNewRequest    AS INT   NO-UNDO.
DEF VAR liDelCounter    AS INT   NO-UNDO.
DEF VAR ldtOldest       AS DATETIME NO-UNDO INIT 1/1/2010.
DEF VAR ldtLastDeleteDate AS DATE NO-UNDO.
DEF VAR liLaunchHandlers  AS INT  NO-UNDO.
DEF VAR lcLine            AS CHAR NO-UNDO.
DEF VAR lcCustNum         AS INT  NO-UNDO.
DEF VAR lcCli             AS CHAR NO-UNDO.

DEFINE TEMP-TABLE ttEvent
FIELD FieldName AS CHAR
FIELD fieldcont AS CHAR
FIELD oldvalue  AS CHAR
FIELD newvalue  AS CHAR.

DEFINE TEMP-TABLE ttCustNum
FIELD InvCust   LIKE msowner.InvCust
FIELD Cli       LIKE Msowner.Cli
FIELD ActDate   AS   DATE
INDEX InvCust InvCust Cli.

FUNCTION fCreateTempTable RETURN LOG
(INPUT icValues AS CHAR).

DEF VAR liLoop       AS INT NO-UNDO.            
DEF VAR liValueCount AS INT NO-UNDO.
   
   livaluecount = num-entries(icValues, "ÿ") / 3.

   DO liLoop = 1 TO livaluecount  :
      CREATE ttEvent.
      ASSIGN
      ttEvent.FieldName  = entry(1 * liloop ,icvalues,"ÿ")
      ttEvent.oldvalue   = entry(2 * liloop ,icvalues,"ÿ")
      ttEvent.newvalue   = entry(3 * liloop ,icvalues,"ÿ")
      NO-ERROR.

   END.
   
END FUNCTION.
   
FORM
   lcStarted     COLON 20 FORMAT "X(20)"         LABEL "Started" 
   lcCurrent     COLON 20 FORMAT "X(20)"         LABEL "Latest Loop" 
      SKIP(1)
   ldQty         COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Events Read"
   ldtLatest     COLON 20                        LABEL "Latest Event Date" 
      SKIP(1)
   ldDone        COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Events Handled"
   ldCounter     COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Analysed Item"
   ldSaveCounter COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Created Item"
   liDelCounter  COLON 20 FORMAT ">>>>>>>>>>>9"  LABEL "Deleted Item"
   ldtOldest NO-LABEL

WITH CENTERED ROW 5 SIDE-LABELS 
     TITLE " TRIGGEREVENTS FOR TRIGGERITEMS " FRAME fQty.

DEF VAR lcTariffFields  AS CHAR NO-UNDO.
DEF VAR lcBdest         AS CHAR NO-UNDO.
DEF VAR lcMobcdr        AS CHAR NO-UNDO.
DEF VAR lcPriceList     AS CHAR NO-UNDO.
DEF VAR lcmservicelimit AS CHAR NO-UNDO.
DEF VAR lcTeam          AS CHAR NO-UNDO.
DEF VAR lcTeamMember    AS CHAR NO-UNDO.
DEF VAR lcTeamPair      AS CHAR NO-UNDO.
DEF VAR lcLeague        AS CHAR NO-UNDO.
DEF VAR lcMsowner       AS CHAR NO-UNDO.

/* test rules */

DEF BUFFER bTriggerEvent    FOR TriggerEvent.
DEF BUFFER bTriggerItem     FOR TriggerItem.

DEF TEMP-TABLE ttTriggerItem LIKE TriggerItem
INDEX Invcust InvCust cli period.


DEF BUFFER bttTriggerItem   FOR ttTriggerItem.

FUNCTION fGenerateTriggerItem RETURN LOG
 (INPUT iiTriggerEventID AS INT,
  INPUT icTriggerConfID  AS Char,
  INPUT iiInvCust        AS INT,
  INPUT icCli            AS CHAR,
  INPUT iiPeriod         AS INT).   

  DEF VAR ldeFirstSecond AS DEC NO-UNDO. 
  DEF VAR ldeLastSecond AS DEC NO-UNDO. 
  DEF VAR ldaDate AS DATE NO-UNDO. 

  /* force customer level rerate if dss is/has been active during the
     rating period */
  IF icCli > "" THEN DO:
  
     ASSIGN
        ldeFirstSecond = iiPeriod * 100 + 1
        ldaDate = DATE(iiPeriod MOD 100, 1, INT(iiPeriod / 100)) + 32
        ldeLastSecond = YEAR(ldaDate) * 10000 + MONTH(ldaDate) * 100 + 1.

     FOR FIRST ServiceLimit NO-LOCK WHERE
               ServiceLimit.GroupCode = {&DSS},
         FIRST MServiceLimit NO-LOCK WHERE
               MServiceLimit.CustNum = iiInvCust AND
               MServiceLimit.DialType = ServiceLimit.DialType AND
               MServiceLimit.SlSeq   = ServiceLimit.SlSeq AND
               MServiceLimit.EndTS  >= ldeFirstSecond AND
               MServiceLimit.FromTS < ldeLastSecond:
        icCli = "".
     END.
  END.

  FIND FIRST ttTriggerItem WHERE 
             ttTriggerItem.InvCust = iiInvCust AND 
             ttTriggerItem.Cli     = icCli     AND 
             ttTRiggerItem.Period  = iiPeriod NO-LOCK NO-ERROR.

  IF AVAIL ttTriggerItem THEN RETURN FALSE.

  CREATE ttTriggerItem.
  ASSIGN 
     ttTriggerItem.TriggerEventID = iiTriggerEventID 
     ttTriggerItem.TriggerConfID  = icTriggerConfID
     ttTriggerItem.InvCust        = iiInvCust
     ttTriggerItem.Cli            = icCli 
     ttTriggerItem.Period         = iiPeriod
     ttTriggerItem.Created        = DATETIME(Today,Mtime).

  ldCounter = ldCounter + 1.
  
  DISP ldCounter WITH FRAME fQty. pause 0.

  RETURN TRUE.
  
END.

FUNCTION fAnalyseTriggerEvent RETURN INT
  (INPUT  icTarget  AS CHAR,
   INPUT  icSource  AS CHAR,
   OUTPUT iiCreated AS INT,
   OUTPUT icText    AS CHAR).

   DEF VAR ldeStamp  AS DEC  NO-UNDO.
   DEF VAR lcPrior   AS CHAR NO-UNDO.
   DEF VAR loopDate  AS DATE NO-UNDO.
   DEF VAR ldaDate   AS DATE NO-UNDO.
   DEF VAR ldaDate2  AS DATE NO-UNDO.
   DEF VAR lcCli     AS CHAR NO-UNDO.
   DEF VAR liReqType AS INT  NO-UNDO.
   DEF VAR liLoop    AS INT  NO-UNDO.
   DEF VAR lcSpecialCli AS CHAR NO-UNDO.
   DEF VAR liSpecialInvCust AS INT NO-UNDO.
   DEF VAR ldaPickDate  AS DATE NO-UNDO.
   DEF VAR ldaActivated AS DATE NO-UNDO.
   DEF VAR liTime       AS INT  NO-UNDO.
   DEF VAR lcValue      AS CHAR NO-UNDO.

   ASSIGN
      iiCreated = 0
      ldaDate   = Today
      ldeStamp  = DEC(YEAR(Today)  * 10000 +
                      MONTH(Today)  * 100 +
                      DAY(Today)).
                      
   ldaActivated = DATE(bTriggerEvent.Activated).
   IF ldaActivated = ? THEN ldaActivated = TODAY.
   IF DAY(ldaActivated) > 1 THEN ldaPickDate = ldaActivated.
   ELSE ldaPickDate = DATE(MONTH(ldaActivated),1,YEAR(ldaActivated)) - 1.
   
   fCreateTempTable(bTriggerEvent.ChangedValues).

   CASE ictarget:

      WHEN "ALL" THEN DO:
         FOR EACH MsOwner NO-LOCK WHERE
                  MsOwner.Brand = gcBrand AND
                  MsOwner.PayType = FALSE
         BREAK BY MsOwner.MsSeq
               BY MsOwner.InvCust:
         
            IF FIRST-OF(MsOwner.InvCust) THEN 
            FOR EACH InvSeq NO-LOCK WHERE 
                 InvSeq.MsSeq    = MsOwner.MsSeq    AND 
                 InvSeq.CustNum  = MsOwner.InvCust  AND 
                 InvSeq.Billed   = FALSE            AND 
                 InvSeq.ToDate   >= ldaPickDate:
                 
               IF fGenerateTriggerItem
                  (bTriggerEvent.TriggerEventID,
                   bTriggerEvent.TriggerConfID,
                   MsOwner.InvCust,
                   "",            
                   YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
               THEN iiCreated = iiCreated + 1.
            END.
         END.
      END.
      
      WHEN "BDest" THEN DO:
         
         IF bTRiggerEvent.TableID ne ? THEN DO:
         
            FIND FIRST Bdest WHERE 
                 RECID(Bdest) = bTriggerEvent.TableID NO-LOCK NO-ERROR.
         
            IF Avail Bdest THEN
            DO loopDate = Bdest.FromDate TO Today:

               FOR EACH Mobcdr WHERE 
                        Mobcdr.Datest = LoopDate AND 
                        Mobcdr.gsmbnr begins Bdest.bdest NO-LOCK.

                  IF fGenerateTriggerItem
                    (bTriggerEvent.TriggerEventID,
                     bTriggerEvent.TriggerConfID,
                     Mobcdr.invcust,
                     "",  /* mobcdr.cli */
                     Year(Mobcdr.Datest) * 100 + MONTH(MObcdr.Datest)) = TRUE
                  THEN iiCreated = iiCreated + 1.
               
               END.      
            END.         
         END.
      END.  
      
      WHEN "Mobcdr" THEN DO:

         FIND FIRST Msowner where
            Msowner.Brand    = "1"                         AND 
            MSowner.MSSeq    = INT(TriggerEvent.TableID)   AND 
            Msowner.tsbegin  <= DEC(SUBSTRING(TriggerEvent.Keyvalue,1,8)) AND 
            Msowner.tsend    >= DEC(SUBSTRING(TriggerEvent.KeyValue,1,8))  
         NO-LOCK NO-ERROR.

         ldaDate = DATE(INT(SUBSTRING(TriggerEvent.Keyvalue,5,2)),
                        INT(SUBSTRING(TriggerEvent.Keyvalue,7,2)),
                        INT(SUBSTRING(TriggerEvent.Keyvalue,1,4))) NO-ERROR.
         IF ldaDate = ? THEN ldaDate = ldaPickDate.
                        
         IF AVAIL Msowner THEN 
         FOR EACH InvSeq WHERE 
                  InvSeq.CustNUm = Msowner.InvCust AND
                  InvSeq.MSSeq   = MSowner.MSSeq     AND
                  InvSeq.Billed  = FALSE AND
                  InvSeq.ToDate >= ldaDate NO-LOCK.
                                                         
            IF fGenerateTriggerItem
                  (bTriggerEvent.TriggerEventID,
                   bTriggerEvent.TriggerConfID,
                   MSowner.InvCust,
                   MSOwner.Cli,
                   YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
            THEN iiCreated = iiCreated + 1.
         END.
      END.

      WHEN "MSOwner"   THEN DO:
         
         FIND FIRST MSOwner WHERE
              RECID(MSOwner) = bTriggerEvent.TableID NO-LOCK NO-ERROR.

         IF AVAIL MSowner THEN DO:
            
            Find FIRST TermMobsub WHERE 
                       TermMobsub.MSSeq = Msowner.MSseq NO-LOCK NO-ERROR.

            IF AVAIL TermMobsub THEN NEXT.

            /* Rerate should be skipped with PostPaid->PostPaid
               STC cases because a new data bundle might not be created yet.
               Otherwise the rerate might incorrectly trigger a barring.
               STC handling creates a separate rerate request after new data
               bundle is created. YOB-540 */
            FIND FIRST MsRequest NO-LOCK WHERE
                       MsRequest.MsSeq = Msowner.MsSeq AND
                       MsRequest.Actstamp = fSecOffSet(Msowner.TSEnd,1) AND
                       MsRequest.Reqtype = 0 NO-ERROR.
            IF AVAIL MsRequest THEN NEXT.

            FOR EACH InvSeq WHERE
                     InvSeq.CustNUm = MSOwner.InvCust  AND
                     InvSeq.Billed  = FALSE AND 
                     InvSeq.ToDate >= ldaActivated  NO-LOCK.

               IF fGenerateTriggerItem
                          (bTriggerEvent.TriggerEventID,
                           bTriggerEvent.TriggerConfID,
                           MSowner.InvCust,
                           MSOwner.Cli,
                           YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
                        THEN iiCreated = iiCreated + 1.
            END.
         END.
      END.

      WHEN "MSRequest" THEN DO:

         FIND FIRST MSRequest WHERE
              RECID(MSRequest) = bTriggerEvent.TableID NO-LOCK NO-ERROR.
                           
         CASE MSRequest.ReqType:

            /* Subscription type change */
            WHEN 0 OR WHEN 1 THEN DO:
               
               FOR EACH Msowner where 
                        Msowner.MSSeq    = MSrequest.MSSeq     AND 
                        MSOwner.TSBegin <= MSRequest.ActStamp  AND 
                        MSOwner.TsEnd   >= MSRequest.ActStamp  NO-LOCK.

                  FOR EACH InvSeq WHERE
                           InvSeq.CustNUm = MSOwner.InvCust  AND
                           InvSeq.MSSeq   = MsREquest.MSSeq  AND
                           InvSeq.Billed  = FALSE AND
                           InvSeq.ToDate >= ldaActivated NO-LOCK.
                  
                     IF fGenerateTriggerItem
                       (bTriggerEvent.TriggerEventID,
                        bTriggerEvent.TriggerConfID,
                        MSowner.InvCust,
                        MSOwner.Cli,
                        YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
                     THEN iiCreated = iiCreated + 1.
             
                  ENd.

               END.
               
            END.

         END CASE.
      END.
      
      WHEN "PlistConf" THEN DO:
         
         IF bTRiggerEvent.TableID ne ? THEN 
         FIND FIRST PlistConf WHERE 
              RECID(PlistConf) = bTriggerEvent.TableID NO-LOCK NO-ERROR.
         
         IF AVAIL plistConf THEN DO:
            ASSIGN lcPrior = PlistConf.RatePlan ldaDate = PlistConf.dFrom.
         END.
         
         IF ldaDate > Today THEN DO:
             NEXT.
         END. 
         
         PLISTCONF:
         FOR FIRST PListConf NO-LOCK WHERE 
                  PlistConf.PriceList = lcPrior,
             EACH BillTarget NO-LOCK where 
                  BillTarget.RatePlan = PlistConf.ratePlan,
             EACH  Msowner where
                   Msowner.Brand      = "1"                   AND 
                   MSOwner.CustNum    = BillTArget.Custnum    AND 
                   Msowner.BillTarget = BillTarget.BillTarget AND
                   Msowner.tsbegin    < ldestamp              AND
                   Msowner.tsend      > ldestamp  NO-LOCK.

             FOR EACH InvSeq WHERE
                      InvSeq.CustNum = Msowner.CustNum AND
                      InvSeq.MSSeq   = MSowner.MSSeq     AND
                      InvSeq.Billed  = FALSE AND
                      InvSeq.ToDate >= ldaPickDate NO-LOCK.
                                                        
                 IF fGenerateTriggerItem 
                    (bTriggerEvent.TriggerEventID,
                     bTriggerEvent.TriggerConfID,
                     Msowner.invcust,
                     "",
                     Year(InvSeq.ToDate) * 100 + MONTH(InvSeq.Todate)) = TRUE 
                 THEN iiCreated = iiCreated + 1.
            END.
         END.
      END.

      WHEN "SPECIAL_FILE" THEN DO:

         EMPTY TEMP-TABLE ttCustNum.

         INPUT FROM VALUE(TriggerEvent.ChangedValues).
         REPEAT:
            IMPORT UNFORMATTED lcLine.
         
            CREATE ttCustNum.
            IF       NUM-ENTRIES(lcLine,",") = 1 THEN ttCustNum.InvCust = INT(lcline) NO-ERROR.
            ELSE IF  NUM-ENTRIES(lcLine,",") = 2 THEN DO:
               ASSIGN ttCustNum.InvCust = INT(ENTRY(1,lcline,",")) 
                      ttCustNum.Cli     = ENTRY(2,lcline,",") NO-ERROR.
            END.

            IF NUM-ENTRIES(lcline,",") > 2 THEN  ttCustNum.ActDate = DATE(ENTRY(3,lcLine,",")) NO-ERROR.
                          
            IF ttCustNum.ActDate = ? THEN ttCustNum.ActDate = ldaPickDate. 
         END.

         FOR EACH ttCustNum NO-LOCK where ttCustNum.InvCust > 0.
        
            IF ttCustNum.Cli ne "" THEN
            FIND FIRST Msowner WHERE 
                       Msowner.Cli     = ttCustNum.CLI     AND 
                       MSOwner.InvCust = ttCustNUm.InvCust NO-LOCK NO-ERROR.

            IF ttCustNum.InvCust > 0 AND ttCustNum.Cli > "" AND AVAIL MSOwner 
            THEN DO:
              
               FOR  EACH InvSeq WHERE 
                         InvSeq.CustNUm = ttCustNum.InvCust AND 
                         InvSeq.MSSeq   = msowner.MSSeq     AND 
                         InvSeq.Billed  = FALSE             AND
                         InvSeq.ToDate >= ldaDate NO-LOCK.
                 
                  IF fGenerateTriggerItem
                   (bTriggerEvent.TriggerEventID,
                    bTriggerEvent.TriggerConfID,
                    ttCustNum.InvCust,
                    ttCustNum.Cli, 
                    YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
                  THEN iiCreated = iiCreated + 1.
               END.
            END.
            ELSE 
            FOR  EACH InvSeq WHERE 
                      InvSeq.CustNUm = ttCustNum.InvCust AND 
                      InvSeq.Billed  = FALSE             AND
                      InvSeq.ToDate >= ldaDate NO-LOCK.

               IF fGenerateTriggerItem
                  (bTriggerEvent.TriggerEventID,
                   bTriggerEvent.TriggerConfID,
                   ttCustNum.InvCust,
                   "",            
                   YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
               THEN iiCreated = iiCreated + 1.
            END.
         END.
         EMPTY TEMP-TABLE ttCustNum.
      END.

      WHEN "SPECIAL_LIST" THEN DO:
      
         DO liloop = 1 TO NUM-ENTRIES(TriggerEvent.ChangedValues,"|"):
      
            ASSIGN
            lcValue = ENTRY(liloop,TriggerEvent.ChangedValues,"|")
            lcSpecialCLI = ""
            ldaDate = ?.
       
            liSpecialInvCust = INT(ENTRY(1,lcValue,",")) NO-ERROR.
            IF NUM-ENTRIES(lcValue,",") > 1 THEN lcSpecialCli = ENTRY(2,lcValue,",") NO-ERROR.
            IF NUM-ENTRIES(lcValue,",") > 2 THEN ldaDate = DATE(ENTRY(3,lcValue,",")) NO-ERROR.
       
            IF ldaDate = ? THEN ldaDate = ldaPickDate.
      
            IF lcSpecialCli > "" THEN
            FIND FIRST MsOwner WHERE 
                       MsOwner.CLI = lcSpecialCli AND
                       MsOwner.InvCust = liSpecialInvCust
            NO-LOCK NO-ERROR.
      
            IF liSpecialInvCust > 0 AND lcSpecialCli > "" AND AVAIL MSOwner THEN DO:
      
               FOR EACH InvSeq WHERE InvSeq.CustNUm = liSpecialInvCust AND
                        InvSeq.MSSeq = msowner.MSSeq AND
                        InvSeq.Billed = FALSE AND
                        InvSeq.ToDate >= ldaDate NO-LOCK.

                  IF fGenerateTriggerItem
                     (bTriggerEvent.TriggerEventID,
                      bTriggerEvent.TriggerConfID,
                      liSpecialInvCust,
                      lcSpecialCli,
                      YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
                  THEN iiCreated = iiCreated + 1.
               END.
            END.
            ELSE
            FOR EACH InvSeq WHERE
                     InvSeq.CustNUm = liSpecialInvCust AND
                     InvSeq.Billed = FALSE AND
                     InvSeq.ToDate >= ldaDate NO-LOCK.
      
               IF fGenerateTriggerItem
               (bTriggerEvent.TriggerEventID,
                bTriggerEvent.TriggerConfID,
                liSpecialInvCust,
                 "",
               YEAR(InvSeq.Todate) * 100 + MONTH(InvSeq.ToDate)) = TRUE
               THEN iiCreated = iiCreated + 1.
            END.
         END.
      END.
      
      WHEN "SLGANALYSE" THEN DO:
         
         IF bTRiggerEvent.TableID ne ? THEN 
         FIND FIRST SLGAnalyse WHERE 
              RECID(SLGAnalyse) = bTriggerEvent.TableID NO-LOCK NO-ERROR.
         
         IF AVAIL SLGAnalyse THEN DO:
            ASSIGN lcPrior = SLGAnalyse.CliType ldaDate = SLGAnalyse.ValidFrom.
         END.
         
         IF ldaDate > Today THEN DO:
             NEXT.
         END. 
         
         SLGANALYSE:
         FOR FIRST CLIType NO-LOCK WHERE 
                   CliType.CLIType = lcPrior,
             EACH BillTarget NO-LOCK where 
                  BillTarget.RatePlan = CliType.PricePlan,
             EACH  Msowner where
                   Msowner.Brand      = "1"                   AND 
                   MSOwner.CustNum    = BillTArget.Custnum    AND 
                   Msowner.BillTarget = BillTarget.BillTarget AND
                   Msowner.tsbegin    < ldestamp              AND
                   Msowner.tsend      > ldestamp  NO-LOCK.

             FOR EACH InvSeq WHERE
                      InvSeq.CustNum = Msowner.CustNum AND
                      InvSeq.MSSeq   = MSowner.MSSeq     AND
                      InvSeq.Billed  = FALSE AND
                      InvSeq.ToDate >= ldaPickDate NO-LOCK.
                                                        
                 IF fGenerateTriggerItem 
                    (bTriggerEvent.TriggerEventID,
                     bTriggerEvent.TriggerConfID,
                     Msowner.invcust,
                     "",
                     Year(InvSeq.ToDate) * 100 + MONTH(InvSeq.Todate)) = TRUE 
                 THEN iiCreated = iiCreated + 1.
            END.
         END.
      END.

      
      WHEN "Tariff" THEN DO:

         IF bTRiggerEvent.TableID ne ? THEN 
         FIND FIRST Tariff WHERE 
              RECID(Tariff) = bTriggerEvent.TableID NO-LOCK NO-ERROR.

         IF AVAIL Tariff THEN DO:
            ASSIGN
               ldaDate  = Tariff.ValidFrom
               ldaDate2 = Tariff.ValidTo
               lcPrior  = Tariff.PriceList.
         END.
         ELSE IF NOT AVAIL Tariff and bTriggerEvent.EventSource = "DELETE" 
         THEN DO:

            ASSIGN
               lcPrior  = ENTRY(3,btriggerevent.keyvalue,"ÿ")
               ldaDate  = ldaPickDate
               ldaDate2 = ldaPickDate.
         END.

         IF ldaDate > today THEN DO:
            icText = "The Tariff for future".
            NEXT.
         END.

         IF bTRiggerEvent.EventSource = "CREATE" AND 
            ldaDate2 < Today       THEN DO:              
            icText = "Tariff closed".
            NEXT.
         END.   

         TARIFF:
         FOR FIRST PListConf NO-LOCK WHERE 
                   PlistConf.PriceList = lcPrior,
            EACH BillTarget NO-LOCK where 
                 BillTarget.RatePlan = PlistConf.ratePlan,
            FIRST Msowner where
                  Msowner.Brand      = "1"                   AND 
                  Msowner.CustNum    = BillTarget.CustNum    AND 
                  Msowner.BillTarget = BillTarget.BillTarget AND
                  Msowner.tsbegin    < ldestamp              AND
                  Msowner.tsend      > ldestamp   NO-LOCK.

            FOR EACH InvSeq WHERE
                     InvSeq.CustNum = Msowner.CustNum AND
                     InvSeq.MSSeq   = MSowner.MSSeq     AND
                     InvSeq.Billed  = FALSE AND
                     InvSeq.ToDate >= ldaDate AND
                     InvSeq.FromDate <= ldaDate2 NO-LOCK.

               IF fGenerateTriggerItem 
                  (bTriggerEvent.TriggerEventID,
                   bTriggerEvent.TriggerConfID,
                   Msowner.invcust,
                   "",
                   Year(InvSeq.ToDAte) * 100 + MONTH(InvSeq.ToDate)) = TRUE 
               THEN iiCreated = iiCreated + 1.
            END.
      
         END.
      END.
        
   END CASE.

END FUNCTION.

ASSIGN
   lcStarted  = fTS2HMS(fMakeTS())
   llMonitor  = FALSE
   lcMonitor  = "ANALYSER:TriggerEvent"  
   lcLogFile  = fCParamC("TMQueueDeleteLog")
   liInterval = fCParamI("TMQueueInterval")
   ldtLastDeletedate = today - 1
   liTransQty = fCParamI("TMQueueTransQty")
   liLaunchHandlers = fCParamI("LaunchRerateHandlerLimit").

IF liInterval = ? THEN liInterval = 30.
IF liTransQty = ? THEN liTransQty = 2000.
IF lcLogFile  = ? THEN lcLogFile  = "".
IF liLaunchHandlers = ? OR liLaunchHandlers = 0 THEN 
   liLaunchHandlers = 100000.

lcLogFile = REPLACE(lcLogFile,"#DATE",STRING(TODAY,"999999")).

MainLoop:
DO WHILE TRUE:
    
   ASSIGN 
   liDays      = 0
   liInterval = 10.
   ocText     = "".

   IF ldtUpdated = ? OR 
      Today    >= ldtUpdated THEN DO:
      
   END.   

   PAUSE 0.
   DISP ldQty    
        ldtLatest
        ldDone 
        ldCounter 
        WITH FRAME fQty.

   QueueLoop:
   /* Those triggers NOT need any actions */
   FOR EACH TriggerConf USE-INDEX Prior NO-LOCK WHERE
            TriggerConf.ValidFrom <= Today   AND
            TriggerConf.ValidTo   >= Today   AND
            TriggerConf.EventRule  = 1,
      FIRST  TriggerEvent  NO-LOCK WHERE
             TriggerEvent.TriggerConfID = TriggerConf.TriggerConfID AND
             TriggerEvent.StatusCode    = 0   .
              
       FIND bTriggerEvent WHERE RECID(bTriggerEvent) = RECID(TriggerEvent)
              EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
               
       IF LOCKED(bTriggerEvent) THEN NEXT.
       
       ASSIGN
          bTriggerEvent.Activate   = DATETIME(Today,mtime)
          bTriggerEvent.Handled    = DATETIME(Today,mtime)
          bTriggerEvent.StatusCode = 9.
   END.
   
   /* These triggers needs rating */
   
   lrRecid = ?.

   MAINTRIGGER:
   FOR EACH  TriggerConf USE-INDEX Prior NO-LOCK WHERE 
             TriggerConf.ValidFrom <= Today   AND 
             TriggerConf.ValidTo   >= Today   AND 
/*             triggerconf.triggerconfid begins "special_" and  */
             TriggerConf.EventRule  = 4,
       EACH  TriggerEvent  NO-LOCK WHERE 
             TriggerEvent.TriggerConfID = TriggerConf.TriggerConfID AND
             TriggerEvent.StatusCode    = 0  .

          lrRecid    = ?.

          FIND bTriggerEvent WHERE RECID(bTriggerEvent) = RECID(TriggerEvent)      
          EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

          IF LOCKED(bTriggerEvent) THEN NEXT MAINTRIGGER.

          ASSIGN 
          ldtLatest                = bTriggerEvent.Created
          bTriggerEvent.Activate   = DATETIME(Today,mtime)
          liDuration               = mtime
          bTriggerEvent.StatusCode = 1
          lrRecid                  = RECID(bTriggerEvent)
          ldQty                    = ldQty + 1
          liQty                    = 0.
      
       LEAVE MainTrigger.
      
   END.

   lcCurrent = fTS2HMS(fMakeTS()).

   IF lrRecid = ? THEN DO:

      PAUSE 0.
      DISP ldQty
       ldtLatest
       ldDone
       ldCounter
       WITH FRAME fQty.
                                         
      
      PUT SCREEN ROW 22 COL 2
      "F8 TO QUIT, Waiting For Next Free TriggerEvent (Interval=" + STRING(liInterval) + ")".
                      
      IF liInterval > 0 THEN DO:
         
         RUN RemoveOldItem.
         
         PAUSE 0.
         DISP liDelCounter ldtOldest lccurrent WITH FRAME fQty.
         
         READKEY PAUSE liInterval.
         
      END.
      
      IF LOOKUP(KEYLABEL(LASTKEY),"F8,8") > 0 THEN DO:
         LEAVE MainLoop.
      END.
                
      NEXT Mainloop.
   
   END.
   
   PAUSE 0.
   DISP lcStarted
        ldQty    
        ldtLatest
        ldDone 
        ldCounter 
        lcCurrent 
   WITH FRAME fQty.

   PUT SCREEN ROW 22 COL 2 FILL(" ",70).

   FIND FIRST bTriggerEvent WHERE 
        RECID(bTriggerEvent) = lrRecID AND 
              bTriggerEvent.StatusCode = 1 NO-LOCK NO-ERROR.

   IF NOT AVAIL bTriggerEvent THEN DO:
      
      NEXT mainloop.
   
   END.

   llAnal = FALSE.

   /* Check that changes really needs anal actions */ 
   FOR EACH   TriggerField WHERE 
              TriggerField.TableName = bTriggerEvent.TableName   NO-LOCK.
              
       IF LOOKUP(bTriggerEvent.ChangedField,TriggerField.FieldName) > 0   THEN llAnal = TRUE.

   END.           

   IF LOOKUP(bTriggerEvent.TriggerConfID,"SPECIAL_FILE,SPECIAL_LIST,ALL,Mobcdr") > 0 OR 
      bTriggerEvent.EventSource   = "CREATE" OR 
      (bTriggerEvent.EventSource   = "DELETE" AND 
       bTriggerEvent.TriggerConfID = "TARIFF") THEN llAnal = TRUE.

   IF llAnal THEN fAnalyseTriggerEvent
                  (INPUT  bTriggerEvent.TableName,
                   INPUT  bTriggerEvent.EventSource,
                   OUTPUT oiQty, OUTPUT ocText).
   ELSE ASSIGN 
      ocText = "Not necessary"
      oiQty  = 0.


   ASSIGN liNewRequest = 0.

   FOR EACH ttTriggerItem BREAK BY ttTriggerItem.cli DESC TRANS:

      /* skip cli based triggeritem, if  customer based triggeritem exists */ 
      IF ttTriggerItem.Cli NE "" THEN DO:
         FIND FIRST bttTriggerItem WHERE 
                    bttTriggerItem.InvCust = ttTriggerItem.InvCust   AND 
                    bttTriggerItem.Period  = ttTriggerItem.Period    AND 
                    bttTriggerItem.Cli     = "" NO-LOCK NO-ERROR.
         IF AVAIL bttTriggerItem THEN NEXT.
      
      END.

      FIND FIRST TriggerItem WHERE 
                 TriggerItem.InvCust    = ttTriggerItem.InvCust AND 
                 TriggerItem.StatusCode = 0                     AND 
                 TriggerItem.Period     = ttTriggerItem.Period  AND 
                 TriggerItem.Cli        = ""             NO-LOCK NO-ERROR.

      IF AVAIL TriggerItem THEN NEXT.
      
      IF ttTriggerItem.Cli NE "" THEN
      FIND FIRST TriggerItem WHERE
                 TriggerItem.InvCust    = ttTriggerItem.InvCust AND
                 TriggerItem.StatusCode = 0                     AND 
                 TriggerItem.Period     = ttTriggerItem.Period  AND 
                 TriggerItem.Cli        = ttTriggerItem.Cli NO-LOCK NO-ERROR.

      IF AVAIL TriggerItem THEN NEXT.                                                  

      IF ttTriggerItem.InvCust = 0 THEN NEXT.

      CREATE TriggerItem.
      BUFFER-COPY ttTriggerItem TO TriggerItem.
      RELEASE TriggerItem.
      
      ASSIGN 
      liNewRequest  = liNewRequest  + 1
      ldSaveCounter = ldSAveCounter + 1.
      PAUSE 0.
      DISP ldSAveCounter WITH FRAME fQty.
   
   END.  

   ACTIVITYCOUNTER:
   DO WHILE TRUE TRANS:
      FIND FIRST ActivityCounter WHERE 
                 ActivityCounter.Brand      = "1"            AND 
                 ActivityCounter.ACDate     = Today          AND 
                 AcTivityCounter.ACValue    = "TRIGGERRATE"  AND 
                 ActivityCounter.ACSubValue = "QTY"  
      EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF LOCKED(ActivityCounter) THEN DO:
         PAUSE 3 NO-MESSAGE.
         NEXT.
      END.
      
      IF NOT AVAIL ActivityCounter THEN DO:
         CREATE ActivityCounter.
         ASSIGN
            ActivityCounter.Brand      = "1"  
            ActivityCounter.ACDate     = Today  
            AcTivityCounter.ACValue    = "TRIGGERRATE"
            ActivityCounter.ACSubValue = "QTY".
      END.
      ASSIGN ActivityCounter.intValue = ActivityCounter.IntValue + liNewRequest.
      
      RELEASE ActivityCounter.
      LEAVE ActivityCounter.
      
   END.
   
   EMPTY TEMP-TABLE ttTriggerItem.
   EMPTY TEMP-TABLE ttEvent.
      
   PAUSE 0.
   DISP ldQty    
        ldtLatest
        ldDone 
        ldCounter 
        WITH FRAME fQty.

   DO TRANS:
      
      FIND FIRST bTriggerEvent WHERE
           RECID(bTriggerEvent) = lrRecID EXCLUSIVE-LOCK NO-ERROR.

      ASSIGN  
         bTriggerEvent.StatusCode = 4               
         bTriggerEvent.Duration   = (mtime - liduration) / 1000
         bTriggerEvent.Qty        = oiQty
         bTriggerEvent.Handled    = DATETIME(Today,mtime).

      IF ocText ne "" THEN 
      ASSIGN 
      bTriggerEvent.StatusCode = 9
      bTriggerEvent.Reason     = ocText.
      

      /* launch more rerate handlers */
      IF oiQty >= liLaunchHandlers THEN  
         RUN pLaunchHandlers.
   END.

   RELEASE bTriggerEvent.

   ldDone = ldDone + 1.
    
   IF ldQty MOD 1 = 0 THEN DO:
      lcCurrent = fTS2HMS(fMakeTS()).
        
      PAUSE 0.
      DISP ldQty    
              ldtLatest
              ldDone 
              ldCounter 
              lcCurrent 
      WITH FRAME fQty.
   END.
END.  /* MainLoop */
 
PROCEDURE RemoveOldItem.


   IF ldtLAstDeleteDate = today then LEAVE.
   ldtLAstDeleteDate    = today.
   DELETELOOP:
   FOR EACH triggerconf NO-LOCK.
   
      FOR EACH Triggerevent WHERE
               TriggerEvent.TriggerConf = TriggerConf.TriggerConf  AND
               TriggerEvent.Statuscode  = 6                        AND 
               TriggerEvent.Created  < Datetime(today - 30, mtime) NO-LOCK.

         FOR EACH TriggerItem WHERE
                  TriggerItem.TriggerConf    = TriggerConf.TriggerConf AND
                  TriggerItem.TriggerEventID = TriggerEvent.TriggerEventID AND
                  TriggerItem.TriggerConfID  = TriggerConf.TriggerconfID NO-LOCK.
            
            DO TRANS:

               FIND FIRST bTriggerItem WHERE
                    RECID(bTriggerItem) = RECID(TriggerItem) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

               IF LOCKED(bTriggerItem) THEN NEXT.
               
               liDelCounter = liDelCounter + 1.
               ldtOldest = MAX(ldtOldest,TriggerItem.Created).

                PAUSE 0.
                DISP liDelCounter ldtOldest lccurrent WITH FRAME fQty.
                
               DELETE bTriggerItem.
                              
            END.
                                         
         END.
                                                
         FIND FIRST TriggerItem WHERE
                    TriggerItem.TriggerConf   = TriggerConf.TriggerConf AND
                    TriggerItem.TriggerConfID = TriggerConf.TriggerconfID NO-LOCK NO-ERROR.
                  
         IF NOT AVAIL TriggerItem THEN DO TRANS:
                         
            FIND FIRST bTriggerEvent WHERE
                 RECID(bTriggerEvent) = RECID(TriggerEvent) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

            IF LOCKED(bTriggerEvent) THEN NEXT.
          
            DELETE bTriggerEvent.
                    
         END.
      END.
   
   END.

END PROCEDURE.                              

PROCEDURE pLaunchHandlers:

   DEF VAR liFuncRunQueue AS INT  NO-UNDO.
   DEF VAR ldCheckFrom    AS DEC  NO-UNDO.
   DEF VAR liSeq          AS INT  NO-UNDO.
   DEF VAR liDelay        AS INT  NO-UNDO.
   
   ASSIGN 
      liFuncRunQueue = fCParamI("TriggerRerateFuncRun")
      liDelay        = fCParamI("TriggerRerateDelay").
   IF liFuncRunQueue = 0 OR liFuncRunQueue = ? THEN RETURN.
   IF liDelay = ? THEN liDelay = 600.
   
   /* already scheduled or running */
   ldCheckFrom = fMake2DT(TODAY - 3,0).
   IF CAN-FIND(FIRST FuncRunQSchedule WHERE
       FuncRunQSchedule.FRQueueID = liFuncRunQueue AND
       FuncRunQSchedule.StartTS > ldCheckFrom AND
       LOOKUP(FuncRunQSchedule.RunState,"Scheduled,Initialized,Running") > 0)
   THEN RETURN.

   liSeq = 1.
   FIND LAST FuncRunQSchedule USE-INDEX FRQScheduleID NO-LOCK NO-ERROR.
   IF AVAILABLE FuncRunQSchedule THEN 
      liSeq = FuncRunQSchedule.FRQScheduleID + 1.
           
   CREATE FuncRunQSchedule.
   ASSIGN 
      FuncRunQSchedule.FRQueueID     = liFuncRunQueue
      FuncRunQSchedule.FRQScheduleID = liSeq
      FuncRunQSchedule.RunState      = "Scheduled"
      FuncRunQSchedule.RunMode       = "Production"
      FuncRunQSchedule.StartTS       = fSecOffSet(fMakeTS(),liDelay + 300).
      
   RUN funcrunqsparam_initialize.p (FuncRunQSchedule.FRQScheduleID).
   
   RELEASE FuncRunQSchedule.

END PROCEDURE.


QUIT.




