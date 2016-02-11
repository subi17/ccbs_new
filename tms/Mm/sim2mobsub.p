/* ----------------------------------------------------------------------
  MODULE .......: sim2mobsub.p
  TASK .........: activate SIM  to Mobsub
  APPLICATION ..: 
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......: 
  VERSION ......: TF
  ---------------------------------------------------------------------- */
{Syst/commali.i} 
{Func/func.p}
{Func/fgettxt.i}

DEF INPUT  PARAMETER   msseq  AS INT . 

DEF VAR lcname              AS C              NO-UNDO.
DEF VAR lcICC             LIKE Mobsub.ICC     NO-UNDO.
DEF VAR llOK                AS LOG            NO-UNDO. 
DEF VAR SogTime             AS DEC            NO-UNDO.
DEF VAR sim-in-cont       LIKE SIM.SimStat    NO-UNDO.
DEF VAR new-so-seq          AS I              NO-UNDO.
DEF VAR iiLang              AS INT            NO-UNDO.
DEF VAR lcAlarmMess         AS CHAR           NO-UNDO.
DEF VAR ldDate              AS DATE           NO-UNDO.
DEF VAR PortDays            AS INT            NO-UNDO.
DEF VAR liPortingTime1      AS INT            NO-UNDO init 8.
DEF VAR liPortingTime2      AS INT            NO-UNDO init 17.


{Func/tmsparam.i SimStatusContSig return}.  sim-in-cont = TMSParam.IntVal.

FIND mobsub NO-LOCK  WHERE 
     mobsub.msseq = msseq NO-ERROR.

IF NOT AVAIL mobsub THEN DO:
   MESSAGE 
   "Unknown Mobile subscription"
   VIEW-AS ALERT-BOX.
   RETURN.
END.

IF Mobsub.msstatus ne 1 THEN DO:
   MESSAGE
   "You can ONLY activate  subscribtion status 1"
   VIEW-AS ALERT-BOX.
           
   RETURN.
              
END.

IF MobSub.msstatus >= 6 AND
   MobSub.MsStatus <= 7 THEN DO:
           
   MESSAGE 
   "MNP number activation not allowed here!" SKIP
   "Use 'Read MNP Response -program'"
   VIEW-AS ALERT-BOX.

   RETURN.
   
END.   

IF MobSub.msstatus >= 11 AND
   MobSub.MsStatus <= 12 THEN DO:
                   
   MESSAGE
      "You can not activate MNP before master inquiry"
   VIEW-AS ALERT-BOX.

   RETURN.

END.

FIND Customer OF MobSub NO-LOCK NO-ERROR.

lcname = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                        BUFFER Customer).

FORM
SKIP(2)
"  Activate subscription" SKIP(2)
"  CLI......: " mobsub.cli SKIP
"  Name.....: " lcname FORMAT "x(40)" SKIP
"  ICC......: " Mobsub.icc            SKIP
"  SIM Stock: " Sim.Stock             SKIP(1)
"Note that this MSISDN has a porting Time Slot" SKIP(1)
"  Date.....:" MSISDN.PortingDate     SKIP
"  Time ....:" MSISDN.PortingTime            SKIP(1)

WITH
   CENTERED WIDTH 60 OVERLAY COLOR VALUE(cfc) TITLE COLOR VALUE(ctc)
   " Activate subscription  " + mobsub.cli + " "
   NO-LABELS FRAME lis.
DEF VAR s AS INT NO-UNDO.


loop:
REPEAT WITH FRAME lis:

   FIND FIRST SIM WHERE 
              SIM.ICC = Mobsub.ICC NO-LOCK NO-ERROR.
   DISPLAY
      mobsub.cli
      lcname 
      Mobsub.icc
      Sim.Stock WHEN AVAIl SIM
   WITH FRAME lis. 
   PAUSE 0.
   
   action:
   REPEAT WITH FRAME lis:
      ASSIGN ufk = 0 ufk[1] = 0 ufk[8] = 8 ufk[5] = 1967  ehto = 0.
      RUN Syst/ufkey.
      IF toimi = 1 THEN LEAVE action.
      IF toimi = 8 THEN DO:
         LEAVE loop.
      END.
      ELSE IF toimi = 5 THEN DO :
         
         FIND Mobsub   WHERE
              Mobsub.msseq = msseq NO-ERROR.
           
         /*****************************
         * IMMEDIATE ACTIVATION:      *
         * - enter SIM.ICC            *
         * - check activation DATE    *
         * - connect tables           *
         * - RUN SOG GWY              *
         *****************************/

         FIND FIRST msowner WHERE 
                   MSOwner.MsSeq  = MobSub.MsSeq
         EXCLUSIVE-LOCK NO-ERROR.

         IF NOT AVAIL msowner THEN DO:

            MESSAGE
            "SYSTEM ERROR"                               SKIP(1)
            "There is no current msowner record for"     SKIP
            "this Mobile subscriber " MobSub.CLI         SKIP(1)
            "CONTACT TECH SUPPORT"
            VIEW-AS ALERT-BOX TITLE "ACTIVATION FAILED".
            LEAVE.
         END.

         IF MobSub.msstatus > 1 AND 
            MobSub.msstatus < 9 THEN DO:
            MESSAGE
            "This mobile subscription has already a SIM card !"
            VIEW-AS ALERT-BOX ERROR.

            MESSAGE "Do You still want to retry activation (Y/N) ?"
            UPDATE llOK.
            IF NOT llOK THEN LEAVE.
         END.

         IF MobSub.ActivationTS = 0 THEN DO:
            
            FIND FIRST msisdn WHERE 
                       msisdn.cli = MobSub.cli NO-LOCK NO-ERROR.
            IF AVAIL msisdn AND 
                     msisdn.portingDate ne ? AND 
                     msisdn.portingDate >= today THEN 
               MobSub.ActivationDate = msisdn.PortingDate.
            ELSE          
            ASSIGN
               MobSub.activationts   = fmakets()  
               MobSub.ActivationDate = TODAY. 

         END.
         PAUSe 0.
         UPDATE 
            MobSub.ICC  
            MSISDN.PortingDate  WHEN avail msisdn 
            MSISDN.PortingTime  WHEN AVAIL msisdn 
                      
         WITH FRAME lis EDITING:
            READKEY.
            
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
               PAUSE 0.
               IF FRAME-FIELD = "icc" THEN DO:
                  IF INPUT FRAME LIS MobSub.ICC = "" THEN DO:
                     HIDE FRAME lis NO-PAUSE.
                     UNDO, LEAVE.
                  END.

                  FIND sim WHERE 
                       SIM.Brand = MobSub.Brand   AND 
                       SIM.ICC   = INPUT FRAME lis MobSub.ICC NO-LOCK NO-ERROR.
                  IF NOT AVAIL sim THEN DO:
                     BELL.
                     MESSAGE "Unknown ICC number !".
                     NEXT.
                  END. 

                  IF SIM.SimStat > 4 AND
                     Sim.SimStat ne 9 THEN DO:
                     
                     MESSAGE "This sim card is already in use !"
                     VIEW-AS ALERT-BOX ERROR TITLE
                     " SIM IS NOT USABLE ".
                     NEXT.
                  END.

                  DISP SIM.Stock WITH FRAME lis.
                  PAUSE 0.

                  /* FIND some data TO lis FRAME */
                  FIND FIRST imsi  WHERE 
                             IMSI.ICC = SIM.ICC     
                  NO-LOCK NO-ERROR.
    
                  IF NOT AVAIL imsi THEN DO:
                     MESSAGE
                     "This SIM Card does not have ANY IMSI nos."  SKIP
                     "attached.  This is an error that should"    SKIP
                     "NOT occur.  This SIM cannot be used."
                     VIEW-AS ALERT-BOX ERROR
                     TITLE "SIM WITHOUT IMSI !".
                     /* go TO ask FOR another ICC */
                     NEXT.
                  END.   
                  ASSIGN MobSub.IMSI = IMSI.IMSI.
               END.      
               ELSE IF frame-field = "PortingDate" THEN DO:
                  ASSIGN input MSISDN.portingdate.

                  IF MSISDN.PortingDate < TODAY + portdays then do:
                     MESSAGE 
                        "Porting date must be at least" skip
                        portdays " days from today."
                     VIEW-AS ALERT-BOX.
                     NEXT-PROMPT MSISDN.Portingdate.
                     NEXT.
                  END.
                  
                  IF MSISDN.PortingDate > TODAY + 90 then do:
                     MESSAGE 
                     "Warning: Check porting date again ..."
                     VIEW-AS ALERT-BOX.
                     NEXT-PROMPT MSISDN.Portingdate.
                     NEXT.
                  END.
                     
                  ELSE IF WEEKDAY(MSISDN.PortingDate) = 7 THEN DO:
                     MESSAGE 
                        "Porting day cannot be Saturday, porting is " SKIP
                        "transferred to next Monday."
                     VIEW-AS ALERT-BOX.
                     
                     ASSIGN
                        MSISDN.PortingDate = MSISDN.PortingDate + 2.
     
                     DISP MSISDN.PortingDate WITH FRAME portframe.
                
                     NEXT-PROMPT MSISDN.Portingdate. 
                     NEXT.
                  END.      

                  ELSE IF WEEKDAY(MSISDN.PortingDate) = 1 THEN DO:
                     MESSAGE 
                     "Porting day cannot be Sunday, porting is " SKIP
                     "transferred to next Mobday."
                     VIEW-AS ALERT-BOX.
                     
                     ASSIGN
                        MSISDN.PortingDate = MSISDN.PortingDate + 1.
                     
                     DISP MSISDN.PortingDate WITH FRAME portframe. 
                     NEXT-PROMPT MSISDN.Portingdate. 
                     NEXT.
                  END.  
               END.
             END.

             ELSE IF frame-field = "PortingTime" THEN DO:
                ASSIGN input MSISDN.portingtime.
                IF  MSISDN.PortingTime < liPortingtime1  OR
                    MSISDN.PortingTime > liPortingtime2  
                THEN DO:
                   BELL.
                   MESSAGE 
                      "Porting time must be set between "
                      liportingtime1 " and " liportingtime2
                   VIEW-AS ALERT-BOX.
                   NEXT-PROMPT MSISDN.PortingTime.
                   NEXT.
                END.
                ELSE  
                   IF  MSISDN.PortingTime - 
                        trunc(MSISDN.PortingTime,0) > 0.59 
                THEN DO:
                   BELL.
                   MESSAGE
                      "Porting minutes must be set between 0 and 59 "
                   VIEW-AS ALERT-BOX.
                   NEXT-PROMPT MSISDN.PortingTime.
                   NEXT.
                END.
            END.
            APPLY LASTKEY.  
         END. /* EDITING */
         LEAVE.
      END.
   END.
   
   IF MobSub.ActivationTS NE 0 THEN DO:

      PAUSE 0.  MESSAGE "Synchronising System Clocks, wait ...".
      sogtime = fmakets().
      PAUSE 0.
            
   END.

   llok = FALSE.
   
   MESSAGE "Do You REALLY want to START ACTIVATION (Y/N) ?" 
   UPDATE llok.
            
   IF NOT llok THEN DO:
      HIDE FRAME lis NO-PAUSE.
      UNDO, LEAVE.
   END. 

   FIND FIRST msowner WHERE MSOwner.CLI    = MobSub.CLI  AND
                            MSOwner.TsEnd >= 99999999
   EXCLUSIVE-LOCK NO-ERROR.

   /* Set CURRENT time stamp (usage starts immediately) */
   ASSIGN MSOwner.TsBegin = fMakeTS()
          MSOwner.IMSI    = MobSub.IMSI.

   /* RUN PROCEDURE which connects sim TO MobSub AND 
     starts the activation TO the SOG             */
        
   RUN connect-tables.
 
   LEAVE.
END.

HIDE FRAME stat.

    
{Mm/connect-tables.i}    

  

 
   
