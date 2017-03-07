/* ----------------------------------------------------------------------     
  MODULE .......: createms.i
  TASK .........: Create a Solog CR record from ONE MobSub annd, if NOT batch,
                  Create a Mobile Subscriber Via SOG Gateway
  APPLICATION ..: Ticket Master
  AUTHOR .......: pt
  CREATED ......: 21-07-99
  CHANGED ......: 23.01.01 pt Automatic creation of VMSUB
                  03.09.01 pt MNP
                  19.10.01/aam timeslot from msisdn when type 2
                  29.10.01/jp  Solog trigger removed, now NEXT-VALUE
                  15.11.01/pt  2 alternative CR commands 
                  16.02.02/jp     COS
                  25.03.03/jp  Cubio activation
                  02.03.04/jp  subserpara
                  15.04.04 jp  mnp flag for Solog
                  21.04.04 jp  profile comman disabled
                  13.12.04/aam SSDate for SubSer and SubSerPara
                  21.12.04/aam create Sologs from services that differ from
                               default values (of clitype),
                               use ttSolog (SepHLR) 
                  20.07.05/tk  find ttSolog with NO-ERROR
                  22.07.05/tk  made this a function
                  04.04.06/aam use cparam2.i instead of tmsparam3.i             
  VERSION ......: M15
  ---------------------------------------------------------------------- */
 
{Syst/commali.i}
{Func/timestamp.i}
{Func/sog.i}
{Func/fctserval.i}
{Func/cparam2.i}

DEF BUFFER bSubSer FOR SubSer.
DEF BUFFER bSSPara FOR SubSerPara.

DEF TEMP-TABLE ttSolog NO-UNDO
   FIELD ServCom  AS CHAR
   FIELD CommLine AS CHAR
   FIELD Solog    AS INT 
   INDEX ServCom ServCom.
 

FUNCTION fCreateMs RETURNS LOGICAL
   (INPUT piMsSeq  AS INTEGER,
    INPUT batch  AS LOGICAL,
    INPUT xtype  AS INTEGER,  /* type of activation   */
    INPUT pdate  AS DATE, /* Date of INport       */
    INPUT adate  AS DATE, /* Date of Activation   */

    OUTPUT new-so-seq    AS INTEGER): /* Seq # of CREATE Solog */

   /******** xtype values **************************
   * 1: normal, immediate activation               *
   * 2: FNSUB during Time Slot + later activation  *
   * 3: Later activation during Time Slot          *
   ************************************************/

   DEF VAR DefIMSI          AS C  NO-UNDO.
   DEF VAR xTime            AS I  NO-UNDO.
   DEF VAR NormalActivation AS I  NO-UNDO INIT 1.
   DEF VAR InportAndFnsub   AS I  NO-UNDO INIT 2.
   DEF VAR InportNoFnsub    AS I  NO-UNDO INIT 3.
   DEF VAR SOGCreateCmd     AS C  NO-UNDO.
   DEF VAR DefCOS           AS C  NO-UNDO.
   DEF VAR lcMNP            AS C  NO-UNDO.

   DEF VAR lcValue          AS CHAR NO-UNDO. 
   DEF VAR liValue          AS INT  NO-UNDO. 
   DEF VAR llAllowed        AS LOG  NO-UNDO. 
   DEF VAR lcServPac        AS CHAR NO-UNDO. 
   DEF VAR lcServCom        AS CHAR NO-UNDO.  
   DEF VAR ldTimeSlotTMS    AS DEC  NO-UNDO.  
   DEF VAR ldTime           AS DEC  NO-UNDO.  
    
   DefIMSI = fCParamC("DefIMSI"). 
   DefCOS  = fCParamC("DefCOS").

   /* subscriber record  (here we get MSISDN and ICC */          
   FIND FIRST MobSub WHERE 
              MobSub.MsSeq = piMsSeq 
   EXCLUSIVE-LOCK NO-ERROR. 
   /* IMSI Number Record (we get Ki ) */ 
   FIND FIRST IMSI WHERE 
              IMSI.ICC = MobSub.ICC 
   NO-LOCK NO-ERROR.  
   
   /* SIm Card  (Here we get the Batch # ) */ 
   FIND FIRST SIM WHERE 
              SIM.ICC = IMSI.ICC
   NO-LOCK NO-ERROR.

   /* SIM batch  (Here we get the ADKEY #) */ 
   FIND FIRST SimBatch WHERE  
              SimBatch.Brand    = gcBrand AND  
              SimBatch.SimBatch = sim.SimBatch  
   NO-LOCK NO-ERROR.
            
   /********************************************* 
   * Create a   C R E A T E                     * 
   * Service Order first into Billing           * 
   * database - next send it to HLR using       * 
   * SOG GWY in Billing Server Platform         * 
   *********************************************/
   

   /********************************************* 
   * Define if FNSUB has to be deleted in       * 
   * accordance with CREATE HLRSUB              * 
   *                                            * 
   * Also note that both CR commands execute    * 
   * CREATE:VMSUB by default                    *    
   *********************************************/

   IF xtype = InportAndFnsub THEN SOGCreateCMD = "CR+DFN".
                             ELSE SOGCreateCMD = "CR".
   
   ASSIGN 
      lcCLI = fMakeIntNumber(MobSub.cli).

   EMPTY TEMP-TABLE ttSolog.


   CREATE ttSolog.
   ASSIGN 
      ttSolog.ServCom = ""    
      ldTimeSlotTMS   = 0.

   /*********************************** 
   * If this is a FUTURE activation,  *
   * set the time slot for            *
   * activation Date at time 00:00.   *
   ***********************************/
   IF xtype <> NormalActivation THEN DO:
       /* if this is a MNP then get the time stamp from msisdn */

       FIND FIRST msisdn WHERE 
                  msisdn.CLI = mobsub.CLI 
       NO-LOCK NO-ERROR.

       IF AVAILABLE MSISDN AND 
           (MSISDN.PortingDate ne ? OR
            MSISDN.PortingTime ne 0 )
       THEN DO:

          ASSIGN xTime         = TRUNC(MSISDN.PortingTime,0) * 3600 +
                                 integer(substring(
                                 string(msisdn.PortingTime,"99.99"),4)) * 60
                 ldTimeSlotTMS = fMake2Dt(msisdn.PortingDate,xTime).

       END.
       ELSE ASSIGN ldTimeSlotTMS = fMake2Dt(adate,0).              
   END.
   
   /******************************* 
   * Next DEFine the possible     * 
   * PROFILE code                 * 
   ******************************/

   /* services that differ from default values */

   FOR EACH subser NO-LOCK WHERE
            subser.MsSeq = piMsSeq, 
      FIRST ServCom NO-LOCK WHERE  
            ServCom.Brand   = gcBrand           AND  
            ServCom.ServCom = subser.Servcom    AND  
            ServCom.ActType  = 0 
   BREAK BY SubSer.ServCom
         BY SubSer.SSDate DESC:
     
      /* use newest */
      IF FIRST-OF(SubSer.ServCom) THEN DO:
   
         lcServPac = "".
         /* check against default value the need to send */
         liValue = fServComValue(MobSub.CLIType,  
                                 SubSer.ServCom,
                                 OUTPUT llAllowed).
       
         IF AVAILABLE CTServEl 
         THEN lcServPac = CTServEl.ServPac.
         ELSE 
         FOR FIRST ServEl NO-LOCK WHERE
                   ServEl.Brand   = gcBrand AND
                   ServEl.ServCom = SubSer.ServCom:
            lcServPac = ServEl.ServPac.
         END.
                                        
         /* same as default value and belongs to profile package */
         IF liValue = SubSer.SSStat AND lcServPac BEGINS "*" 
         THEN NEXT. 

         /* doesn't belong to profile, and should not be activated */
         IF SubSer.SSStat = 0 AND NOT lcServPac BEGINS "*"
         THEN NEXT.
       
         /* mark that solog has been created */
         FIND bSubSer WHERE 
              RECID(bSubSer) = RECID(SubSer) 
         EXCLUSIVE-LOCK NO-ERROR.
       
         /* update to HLR */
         IF ServCom.ActType = 0 THEN DO: 
            /* should a separate Solog be created */
            IF ServCom.SepHLR THEN DO:
               CREATE ttSolog.
               ttSolog.ServCom = SubSer.ServCom.
            END.
            ELSE DO:
               FIND FIRST ttSolog WHERE 
                          ttSolog.ServCom = ""
               EXCLUSIVE-LOCK.
            END.
          
            ttSolog.CommLine = ttSolog.CommLine +
                               subser.ServCom   + "="        +        
                              (IF subser.SSParam = "" THEN 
                               string(subser.SSStat)  ELSE
                               subser.SSParam)+ ",".     
                                    
            bSubSer.SologStat = 2. 
         END.
 
         /* no need to update */
         ELSE bSubSer.SologStat = 0. 
              
      END.   
   
   END.     

   FOR EACH subserpara NO-LOCK WHERE 
            subserpara.MsSeq = piMsSeq,
      FIRST SubSer NO-LOCK WHERE
            SubSer.MsSeq   = piMsSeq AND
            SubSer.ServCom = SubSerPara.ServCom AND
            SubSer.SSStat  = 1,
      FIRST ServCom NO-LOCK WHERE
            ServCom.Brand   = gcBrand AND
            ServCom.ServCom = SubSerPara.ServCom
   BREAK BY SubSerPara.ServCom
         BY SubSerPara.ParaName
         BY SubSerPara.SSDate DESC:
      
      /* use newest */
      IF FIRST-OF(SubSerPara.ParaName) THEN DO:
      
         /* mark that solog has been created */
         FIND bSSPara WHERE 
              RECID(bSSPara) = RECID(SubSerPara) 
         EXCLUSIVE-LOCK NO-ERROR.
      
         /* update to HLR */
         IF ServCom.ActType = 0 THEN DO: 
      
            /* a separate Solog line or a combined one */
            lcServCom = "".
            IF ServCom.SepHLR THEN lcServCom = ServCom.ServCom.
      
            FIND FIRST ttSolog WHERE 
                       ttSolog.ServCom = lcServCom 
            EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE ttSolog THEN DO:
                CREATE ttSolog.
                ttSolog.ServCom = lcServCom.
            END.   

            ttSolog.CommLine = ttSolog.CommLine + Subserpara.Servcom + "." +
                                                  subserpara.paraname + "=" +
                                                  subserpara.paravalue + ",".                                                     
            bSSPara.SologStat = 2. 
         END.
                                                  
         /* no need to update */
         ELSE bSSPara.SologStat = 0. 
 
      END. 
   END.         

   /* Is this mnp number */ 
   FIND FIRST Order WHERE 
              Order.MsSeq = MobSub.MsSeq 
   NO-LOCK NO-ERROR.

   IF NOT AVAIL Order THEN DO:
      FIND FIRST Order WHERE 
                 Order.CLI     = MobSub.CLI  AND 
                 Order.CustNum = MobSub.CustNum 
      NO-LOCK NO-ERROR.
   END.     
   IF AVAIL Order AND 
            Order.mnpstatus > 0 THEN 
        lcMNP = "MNP=1,".
   ELSE lcMNP = "MNP=0,".         

   FIND FIRST ttSolog WHERE 
              ttSolog.ServCom = ""
   EXCLUSIVE-LOCK.
   ttSolog.CommLine = ttSolog.CommLine + lcMNP.

   /* entries to db */
   FOR EACH ttSolog WHERE ttSolog.CommLine > "":

      /* remove last comma */
      SUBSTR(ttSolog.CommLine,length(ttSolog.CommLine)) = " ".
 
      ldTime = fMakeTS().
   
      REPEAT:
         /* make sure that there is atleast 1 second gap between sologs */
         IF NOT CAN-FIND(FIRST Solog WHERE
                               Solog.MsSeq = MobSub.MsSeq AND
                               Solog.Stat  = 0            AND
                               Solog.ActivationTS = ldTime)
         THEN LEAVE.
         ASSIGN ldTime        = ldTime        + 0.00001.
         IF ldTimeSlotTMS > 0 THEN             
            ldTimeSlotTMS = ldTimeSlotTMS + 0.00600. 
      END. 

      CREATE Solog.                            
      ASSIGN
         Solog.Solog        = NEXT-VALUE(Solog) 
         ttSolog.Solog      = Solog.Solog 
         Solog.MsSeq        = MobSub.MsSeq   /* Mobile Subscription No.    */
         Solog.CLI          = MobSub.CLI     /* MSISDN                  */
         Solog.Stat         = 0              /* just created             */
         Solog.Brand        = MobSub.Brand  
         Solog.Users        = katun 
         Solog.TimeSlotTMS  = ldTimeSlotTMS 
         Solog.CreatedTS    = ldTime         /* Created NOW         */
         Solog.ActivationTS = ldTime.        /* Activate NOW         */
      
      IF ttSolog.ServCom = ""
      THEN Solog.CommLine = "CR".
      ELSE Solog.CommLine = "ST".
   
      Solog.CommLine = Solog.CommLine + ","  
                                      + lcCLI      + ","    
                                      + MobSub.icc + "," 
                                   + ttSolog.CommLine.

      IF ttSolog.ServCom = "" THEN new-so-seq = Solog.Solog.

   END.

   /* different loop for sending to HLR */
   FOR EACH ttSolog,
   FIRST Solog EXCLUSIVE-LOCK WHERE
         Solog.Solog = ttSolog.Solog:
   
      IF Solog.TimeSlotTMS = 0 THEN Solog.TimeSlotTMS = fMakeTS().
      
      IF Solog.TimeSlotTMS = 0 THEN DO:
         IF NOT batch THEN MESSAGE 
            "Service Order request #" string(Solog.Solog) 
            " has been saved to the system."                            SKIP
            "Request is send to the activation server. "                SKIP
            "ALL Sent Service Order requests and their current status " SKIP
            "can be browsed from service Order log (Solog)." 
         VIEW-AS ALERT-BOX TITLE "Service Order Request".
      END.
      ELSE DO:
         IF NOT batch THEN MESSAGE 
            "Service Order request #" string(Solog.Solog) 
            "has been saved to the system."                             SKIP(1)
            "This activation request is scheduled and will be sent to " SKIP
            "activation server " fTS2HMS(Solog.TimeSlotTMS) "."
         VIEW-AS ALERT-BOX TITLE "Service Order Request".  
      END.
   END.

   HIDE FRAME main NO-PAUSE.
   HIDE MESSAGE.

END.
