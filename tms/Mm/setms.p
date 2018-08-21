/* ----------------------------------------------------------------------
 MODULE .......: SOG-ST.P
  TASK .........: SET selected parameters into HLR ( EXCEPT CFNRY ...
                  because CFNRY cannot be set alone, use cfnrych.p instead )
  APPLICATION ..: nn
  AUTHOR .......: jp
  CREATED ......: 08-07-99
  CHANGED ......: 12.08.99 pt final tuning
                  16.08.99 pt set ALL if param list = "*",  NO CFNRY
                  01.04.04 jp break by SubSer.ssstat
                  01.07.04 tk simbatch removed
                  13.12.04/aam use SubSer.SSDate
                  22.12.04/aam SubSer.SologStat, use ttSolog
                  18.08.05/aam if barring is set, check that there aren't
                               pending barring closures
                  13.09.05/aam use ssparam only if ssstat > 0             
                  29.11.06/aam subser is not necessarily created yet,
                               new values for rc 
  Version ......: 
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Func/cparam2.i}
{Mm/barrgrp.i}
{Gwy/provision.i}
{Func/sharperconfid.i}
{Mm/active_bundle.i}
{Mm/ongoing_bundle.i}

DEF INPUT  PARAMETER  iiMsRequest LIKE MSRequest.msrequest NO-UNDO.
DEF INPUT  PARAMETER  batch       AS LOG             NO-UNDO. 
DEF OUTPUT PARAMETER  rc          AS INTEGER         NO-UNDO.
DEF OUTPUT PARAMETER  ocError    AS CHAR NO-UNDO. 
   /* -1: error occurred 
       0: no need to create solog
       1: solog created 
   */

FUNCTION fDoubleParam RETURNS CHARACTER
  (INPUT pcCommLine AS CHARACTER):

   DEFINE VARIABLE liIdx  AS INTEGER   NO-UNDO.
   DEFINE VARIABLE lcTemp AS CHARACTER NO-UNDO.

   IF INDEX(pcCommLine,"PAYTYPE=P") > 0 AND
      INDEX(pcCommLine,"PAYTYPE=-") > 0 THEN DO:
               
      DO liIdx = 1 TO NUM-ENTRIES(pcCommLine):
         IF NOT ENTRY(liIdx,pcCommLine) BEGINS "PAYTYPE=P" THEN
            lcTemp = lcTemp + "," + ENTRY(liIdx,pcCommLine).
      END.

      lcTemp = SUBSTR(lcTemp,2).

   END.
   ELSE lcTemp = pcCommLine.

   RETURN lcTemp.

END FUNCTION.

DEF VAR ldSchedule    AS DEC                        NO-UNDO. 
DEF VAR ldTime        AS DEC                        NO-UNDO. 
DEF VAR ldCurrent     AS DEC                        NO-UNDO. 
DEF VAR ldActStamp    AS DEC                        NO-UNDO. 
DEF VAR ldOrder       AS DEC                        NO-UNDO. 
DEF VAR llSkip        AS LOG                        NO-UNDO. 
DEF VAR lcActiveBundles AS CHAR                     NO-UNDO.
DEF VAR lcServName    AS CHAR                       NO-UNDO.
DEF VAR lcBundle      AS CHAR                       NO-UNDO.
DEF VAR liNumEntries  AS INT                        NO-UNDO.
DEF VAR liCount       AS INT                        NO-UNDO.
DEF VAR lcServiceClass AS CHAR                      NO-UNDO.
DEF VAR liCurrentServiceClass AS INT                NO-UNDO.
DEF VAR lcError       AS CHAR                       NO-UNDO.
DEF VAR lcPrepaidVoiceTariffs  AS CHAR              NO-UNDO.
DEF VAR lcBBProfile1           AS CHAR              NO-UNDO.
DEF VAR lcBBProfile2           AS CHAR              NO-UNDO.
DEF VAR lcShaperProfile        AS CHAR              NO-UNDO.
DEF VAR ldaActiveDate          AS DATE              NO-UNDO.
DEF VAR liActiveTime           AS INT               NO-UNDO.
DEF VAR llCheckSC AS LOG NO-UNDO INIT TRUE.
DEF VAR lcShaperConfId AS CHAR NO-UNDO.
DEF VAR lcBaseBundle AS CHAR NO-UNDO.
DEF VAR lcParam AS CHAR NO-UNDO. 

DEF BUFFER bSubSer FOR SubSer.
DEF BUFFER bSSPara FOR SubSerPara.
DEF BUFFER bttBarr FOR ttBarring.
DEF BUFFER bOrigRequest FOR MSRequest.
DEF BUFFER bMsRequest   FOR MSRequest.
DEF BUFFER bCLIType     FOR CLIType.

DEF TEMP-TABLE ttSolog NO-UNDO
   FIELD ServCom   AS CHAR
   FIELD CommLine  AS CHAR
   FIELD Solog     AS INT 
   FIELD ActStamp  AS DEC
   FIELD MSREquest AS INT
   INDEX ServCom ServCom ActStamp.
 
DEF TEMP-TABLE ttDone NO-UNDO
   FIELD ServCom AS CHAR
   FIELD SSStat  AS INT
   INDEX ServCom ServCom.

   
ldCurrent = Func.Common:mMakeTS().

rc = -1.

FIND FIRST MSrequest WHERE 
           MSRequest.MSrequest = iiMSrequest NO-LOCK NO-ERROR.
IF NOT AVAILABLE MsRequest THEN RETURN.           

FIND MobSub WHERE MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
IF NOT AVAILABLE MobSub THEN RETURN.

/* IMSI number Record (we get KI ) */
FIND FIRST IMSI WHERE IMSI.ICC = MobSub.ICC NO-LOCK NO-ERROR.
IF NOT AVAILABLE IMSI THEN DO:
   ocError = "ERROR:IMSI not found".
   RETURN ocError.
END.

/* SIM Card  (Here we get the Batch # ) */
FIND SIM where SIM.ICC = IMSI.ICC NO-LOCK NO-ERROR.
IF NOT AVAILABLE SIM THEN DO:
   ocError = "ERROR:SIM not found".
   RETURN ocError.
END.

ASSIGN lcPrepaidVoiceTariffs = fCParamC("PREPAID_VOICE_TARIFFS")
       lcBBProfile1          = fCParamC("BB_PROFILE_1")
       lcBBProfile2          = fCParamC("BB_PROFILE_2").

IF MobSub.CLIType = "TARJ5" AND MsRequest.ReqCParam1 = "HSDPA"
THEN DO:
   RUN Gwy/air_get_account_details.p(MobSub.CLI, 
                                 OUTPUT liCurrentServiceClass,
                                 OUTPUT lcError).
   IF lcError BEGINS "ERROR" THEN DO:
      ocError = lcError.
      RETURN ocError.
   END.
END.

/* Replace Service Name SHAPER to "UPSELL" in solog */
IF MsRequest.ReqCParam1 = "SHAPER" AND
   MsRequest.OrigRequest > 0 THEN DO:
   FIND FIRST bMsRequest WHERE 
              bMsRequest.MsRequest = MsRequest.OrigRequest
        NO-LOCK NO-ERROR.
   IF AVAILABLE bMsRequest THEN DO:
      IF bMsRequest.ReqCparam3 MATCHES "*_UPSELL" THEN
         lcServName = "QUOTA".
      ELSE IF (bMsRequest.ReqType EQ 0 OR 
               bMsRequest.ReqType EQ 81) AND
         INDEX(MsRequest.ReqCParam2,"HSPA") > 0 THEN
         lcServName = "QUOTA".
   END.
END. /* IF MsRequest.ReqCParam1 = "SHAPER" AND */

IF MsRequest.ReqCparam1 = "CF" THEN DO:
   
   IF MsRequest.ReqCParam2 > "" THEN DO:
      
      IF LENGTH(MsRequest.ReqCParam2) NE 3 THEN DO:
         ocError = "ERROR:Incorrect CF parameters".
         RETURN ocError.
      END.

      DO liCount = 1 TO 3:
         lcServName = lcServName + ENTRY(liCount,"CFB,CFNRC,CFNRY").
         CASE SUBSTRING(MsRequest.ReqCparam2,liCount,1):
            WHEN "0" THEN lcServName = lcServName + "=0".
            WHEN "1" THEN lcServName = lcServName + "=34633633633".
            WHEN "2" THEN lcServName = lcServName + "=34633633556".
            OTHERWISE DO:
               ocError = "ERROR:Incorrect CF parameters".
               RETURN ocError.
            END.
         END.
         IF liCount NE 3 THEN lcServName = lcServName + ",".
      END.
   END.
   ELSE DO:
      CASE MsRequest.ReqIParam1:
         /* OFF */
         WHEN 0 THEN
            lcServName = "CFB=0,CFNRC=0,CFNRY=0".
         /* CF */
         WHEN 1 THEN
            lcServName = "CFB=34633633633,CFNRC=34633633633,CFNRY=34633633633".
         /* MCA */
         WHEN 2 THEN
            lcServName = "CFB=34633633556,CFNRC=34633633556,CFNRY=0".
         OTHERWISE DO:
            ocError = "ERROR:Incorrect CF parameters".
            RETURN ocError.
         END.
      END CASE.
   END.
END.

IF lcServName = "" THEN lcServName = MsRequest.ReqCParam1.

FIND ServCom WHERE
     ServCom.Brand   = Syst.Var:gcBrand AND
     ServCom.ServCom = MsRequest.ReqCParam1 NO-LOCK NO-ERROR.

rc = 0.

/* update to HLR */  
IF ServCom.ActType = 0 THEN DO:
 
   rc = -1.
   
   CREATE ttSolog.
   ASSIGN ttSolog.ServCom = ""
          ttSolog.MSRequest = iiMsRequest

   ldActStamp = MAX(MsRequest.ActStamp,ldCurrent).
  
  IF lcServName EQ "SHAPER" AND
     MsRequest.ReqIParam1 > 0 AND 
     MsRequest.ReqCParam2 > "" THEN DO:

     lcShaperProfile = fGetShaperConfCommline(MsRequest.ReqCParam2).

     IF AVAILABLE bMsRequest THEN DO:

        Func.Common:mSplitTS(bMsRequest.ActStamp,
                 OUTPUT ldaActiveDate,
                 OUTPUT liActiveTime).

        IF LOOKUP(bMsRequest.ReqCparam3,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0 AND 
           bMsRequest.ReqType = 8 THEN
           lcShaperProfile = lcShaperProfile +
                             ",RESET_DAY=" + STRING(DAY(ldaActiveDate)).
        ELSE IF MsRequest.ReqCParam2 <> "DEFAULT" THEN DO:

           IF (bMsRequest.ReqSource = {&REQUEST_SOURCE_STC} OR
               bMsRequest.ReqSource = {&REQUEST_SOURCE_BTC} OR
               bMsRequest.ReqType   = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} OR
               bMsRequest.ReqType   = {&REQTYPE_BUNDLE_CHANGE}) THEN
              lcShaperProfile = REPLACE(lcShaperProfile,"HSPA_MONTHLY_ADD",
                                                        "HSPA_MONTHLY").
           ELSE IF
              bMsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_CREATION} OR
              bMsRequest.ReqSource = {&REQUEST_SOURCE_SUBSCRIPTION_REACTIVATION}
           THEN
              lcShaperProfile = REPLACE(lcShaperProfile,"HSPA_MONTHLY_ADD",
                                                        "HSPA_MONTHLY").
        END.
     END. /* IF AVAILABLE bMsRequest AND */

     ttSolog.Commline = ttSolog.Commline + lcShaperProfile + ",".
  END. /* IF lcServName EQ "SHAPER" AND */
  ELSE IF LOOKUP(MsRequest.ReqCParam1,"CF") > 0 THEN
     ttSolog.CommLine = ttSolog.CommLine + lcServName + ",".
  ELSE DO:

     lcParam = STRING(MsRequest.ReqIParam1).

     IF MsRequest.ReqIParam1 > 0 AND
        MsRequest.ReqCparam2 NE "" THEN lcParam = MsRequest.ReqCParam2.
     ELSE IF lcServName EQ "NW" THEN DO:
       CASE MsRequest.ReqIParam1:
          WHEN 1 THEN lcParam = "YOIGO".
          WHEN 2 THEN lcParam = "YOIGO_ORANGE".
          WHEN 3 THEN lcParam = "YOIGO_ORANGE_TELEFONICA".
       END.
     END.
      
     ttSolog.CommLine = ttSolog.CommLine +
                      TRIM(lcServName)   + "="  +
                      lcParam + ",".
  END.
END.     

/* entries to db */
FOR EACH ttSolog WHERE 
         ttSolog.CommLine > ""
BY ttSolog.ActStamp:

   /* remove last comma */
   SUBSTR(ttSolog.CommLine,length(ttSolog.CommLine)) = " ".

   IF ttSolog.CommLine = "" THEN NEXT.
   
   ldTime = ttSolog.ActStamp.
   
   IF ttSolog.ActStamp > ldCurrent 
   THEN ldSchedule = ttSolog.ActStamp.
   ELSE ldSchedule = ldCurrent.
   
   REPEAT:
      /* make sure that there is atleast 1 second gap between Sologs */
      IF NOT CAN-FIND(FIRST Solog WHERE
                            Solog.MsSeq = MobSub.MsSeq AND
                            Solog.Stat  = 0            AND
                            Solog.ActivationTS = ldTime)
      THEN LEAVE.

      ldTime = ldTime  + 0.00001.
      
      IF ldSchedule > 0 THEN ldSchedule = ldSchedule + 0.00010.
   END. 

   CREATE Solog.
   ASSIGN
      Solog.Solog        = NEXT-VALUE(Solog)
      Solog.CreatedTS    = ldTime            /* Ceated NOW               */
      Solog.ActivationTS = ldTime            /* Activate NOW             */
      Solog.MsSeq        = MobSub.MsSeq      /* Mobile Subscription No.    */
      Solog.CLI          = MobSub.CLI        /* MSISDN                     */
      Solog.Stat         = 0                 /* just created               */
      Solog.Brand        = Syst.Var:gcBrand 
      Solog.Users        = Syst.Var:katun    
      Solog.MSrequest    = ttSolog.MSrequest.
   
   /* Special handling for Prepaid Bono8 HSDPA, SER-1345  */
   IF liCurrentServiceClass > 0 OR
      (MSrequest.ReqCParam1 = "HSDPA" AND
       LOOKUP(Mobsub.CLIType,lcPrepaidVoiceTariffs) > 0) THEN DO:

      FIND FIRST bCliType WHERE
                 bCliType.CliType = Mobsub.CliType
      NO-LOCK NO-ERROR.

      IF MsRequest.ReqCParam1 = "HSDPA" AND
         MsRequest.ReqIParam1 = 0 AND
         LOOKUP(Mobsub.CLIType,lcPrepaidVoiceTariffs) > 0 THEN DO:
         FIND FIRST bMsRequest NO-LOCK WHERE
                    bMsRequest.MsSeq = Mobsub.MsSeq AND
                    bMsRequest.ReqType = {&REQTYPE_SUBSCRIPTION_TYPE_CHANGE} AND
                    LOOKUP(STRING(bMsRequest.ReqStat),"4,9,99,3") = 0 AND
                    bMsRequest.ActStamp = MsRequest.ActStamp AND
                    LOOKUP(bMsRequest.ReqCparam2,"TARJ7,TARJ9,TARJ10,TARJ11,TARJ12,TARJ13") > 0
              USE-INDEX MsSeq NO-ERROR.
         IF AVAILABLE bMsRequest THEN
            ASSIGN lcServiceClass = ""
                   llCheckSC      = FALSE.
      END. /* IF MsRequest.ReqCParam1 = "HSDPA" AND */

      /* Check Service Class */
      IF llCheckSC THEN
      CASE Mobsub.CLIType:
         WHEN "TARJ" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0081".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
         END. /* WHEN "TARJ" THEN DO: */
         WHEN "TARJ4" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0084".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
         END. /* WHEN "TARJ4" THEN DO: */
         WHEN "TARJ5" THEN DO:
         
            IF MsRequest.ReqIParam1 EQ 1 THEN DO:
               IF liCurrentServiceClass EQ {&SC_TARJ5_PROMOTIONAL_BONO}
               THEN lcServiceClass = "".
               ELSE lcServiceClass = ",SERVICECLASS=0086".
            END.
            ELSE DO:
               IF liCurrentServiceClass EQ {&SC_TARJ5_PROMOTIONAL}
               THEN lcServiceClass = "".
               ELSE lcServiceClass = (IF AVAIL bCliType THEN
                    ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
            END.
         END. /* WHEN "TARJ5" THEN DO: */
         WHEN "TARJ6" THEN DO:
            /* TODO: This is to prevent HSDPA=0 command (should be possible
               only from STC bundle termination) to override SERVICECLASS in
               STC MODIFY command */  
            IF MsRequest.ReqIParam1 EQ 0 THEN
               lcServiceClass = "".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass
                                 ELSE "").
         END. /* WHEN "TARJ6" THEN DO: */
         WHEN "TARJ7" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0003".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ7" THEN DO: */
         WHEN "TARJ8" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0082".
            ELSE
               lcServiceClass = (IF AVAIL bCliType THEN
                                 ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
         END. /* WHEN "TARJ8" THEN DO: */
         WHEN "TARJ9" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0009".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ9" THEN DO: */
         WHEN "TARJ10" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0010".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ10" THEN DO: */
         WHEN "TARJ11" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0011".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ11" THEN DO: */
         WHEN "TARJ12" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0012".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ12" THEN DO: */
         WHEN "TARJ13" THEN DO:
            IF MsRequest.ReqIParam1 EQ 1 THEN
               lcServiceClass = ",SERVICECLASS=0020".
            ELSE lcServiceClass = "".
         END. /* WHEN "TARJ13" THEN DO: */
         OTHERWISE
            lcServiceClass = (IF AVAIL bCliType AND
                                       bCliType.ServiceClass > "" THEN
                              ",SERVICECLASS=" + bCliType.ServiceClass ELSE "").
      END CASE.

      ttSolog.CommLine = ttSolog.CommLine + lcServiceClass.
   END. /* IF MSrequest.ReqCParam1 = "HSDPA" AND */

   /* Special handling for BB service  */
   IF MSrequest.ReqCParam1 = "BB" AND MsRequest.ReqIParam1 = 1 THEN DO:

      IF Mobsub.CLIType = "TARJ6" OR MobSub.CLIType = "CONTS" THEN
         ttSolog.Commline = TRIM(ttSolog.CommLine) + "|1".
      ELSE DO:
      lcActiveBundles = fGetActOngoingDataBundles(INPUT Mobsub.MsSeq,
                                                  INPUT ldActStamp).

      /* Main logic to find the correct profile */
      IF lcActiveBundles > "" THEN DO:
         ASSIGN ttSolog.Commline = TRIM(ttSolog.Commline, " ")
                liNumEntries = NUM-ENTRIES(lcActiveBundles).

         DO liCount = 1 TO liNumEntries:
            lcBundle = ENTRY(liCount,lcActiveBundles).
            IF LOOKUP(lcBundle,lcBBProfile1) > 0 THEN DO:
               ASSIGN ttSolog.Commline = ttSolog.CommLine + "|1"
                      llSkip = TRUE.
               LEAVE.
            END. /* IF LOOKUP(lcBundle,lcBBProfile1) > 0 THEN DO: */
         END. /* DO liCount = 1 TO liNumEntries: */

         IF NOT llSkip THEN DO:
            liCount = 0.
            DO liCount = 1 TO liNumEntries:
               lcBundle = ENTRY(liCount,lcActiveBundles).
               IF LOOKUP(lcBundle,lcBBProfile2) > 0 THEN DO:
                  ttSolog.Commline = ttSolog.CommLine + "|2".
                  LEAVE.
               END. /* IF LOOKUP(lcBundle,lcBBProfile2) > 0 THEN */
            END. /* DO liCount = 1 TO liNumEntries: */
         END. /* IF NOT llSkip THEN DO: */
      END. /* IF lcActiveBundles > "" THEN DO: */
      END.
   END. /* IF MSrequest.ReqCParam1 = "BB" THEN DO: */

   ASSIGN
      Solog.CommLine    = fMakeCommline(Solog.solog,"MODIFY") + ttSolog.CommLine
      SoLog.CommLine    = fDoubleParam(SoLog.CommLine)
      SoLog.CommLine    = TRIM(REPLACE(SoLog.CommLine,",,",","),",")
      Solog.TimeSlotTMS = ldSchedule
      rc                = 1.
  
   IF Solog.timeslotTMS = 0  THEN DO:

      IF NOT batch THEN MESSAGE 
         "Service order request #" string(Solog.Solog) 
         "has been saved to the system."
                                                                      SKIP
         "Request is sent to the activation server. "                 SKIP(1)
         "ALL sent Service Order requests and their current status "  SKIP
         "can be browsed from service order log (Solog)." 

         VIEW-AS ALERT-BOX TITLE "Service Order Request".
   END.

   ELSE IF NOT batch THEN  MESSAGE 
      "Service order request #" string(Solog.Solog) 
      "has been saved to the system."                             SKIP(1)
      "This activation request is scheduled and will be sent to "  SKIP
      "activation server" Func.Common:mTS2HMS(Solog.TimeSlotTMS) "."             
      VIEW-AS ALERT-BOX TITLE "Service Order Request".  

END.

FINALLY:
   EMPTY TEMP-TABLE ttSolog.
   EMPTY TEMP-TABLE ttBarring.
END FINALLY.
