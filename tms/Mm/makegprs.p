/* ----------------------------------------------------------------------
  MODULE .......: MAKEgprs
  TASK .........: make data numberF
  APPLICATION ..: tms
  AUTHOR .......: jp
  CREATED ......: 02.03.04
  CHANGED ......: 
                  13.12.04/aam SSDate for SubSer
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/msisdn.i}
{Func/sog.i}
{Func/tmsparam3.i}

DEF INPUT PARAMETER   MsSeq LIKE MobSub.MsSeq NO-UNDO.
DEF INPUT PARAMETER   isolog AS i NO-UNDO.

DEF BUFFER xMobsub FOR mobsub.
DEF BUFFER xsolog  FOR Solog.
DEF BUFFER csolog  FOR Solog.

FIND MobSub WHERE MobSub.MsSeq = MsSeq NO-LOCK.

FIND Subser WHERE 
     Subser.msseq   = msseq AND 
     Subser.Servcom = "GPRS" AND
     SubSer.SSDate  = TODAY NO-ERROR.

IF NOT AVAIL Subser THEN DO:
   CREATE SubSer.
   ASSIGN
      SubSer.MsSeq   = MsSeq
      SubSer.ServCom = "GPRS"
      SubSer.SSDate  = TODAY
      Subser.ssstat  = 1 .
END.

IF iSOLOG > 0 THEN DO:
   find xSolog WHERE 
        xsolog.solog = isolog NO-LOCK NO-error.

   CREATE csolog.
   BUFFER-COPY xsolog except 
               xsolog.ActivationTS 
               xsolog.solog  to csolog.
   ASSIGN
   csolog.ActivationTS = fmakeTS() + 0.00002
   csolog.CreatedTS    = csolog.ActivationTS
   csolog.stat      = 0
   csolog.response     = ""
   csolog.solog     = NEXT-VALUE(solog) .
   ASSIGN
   csolog.CommLine  = "ST"                         +  "," +   
                      ENTRY(2,xsolog.commline,",") +  "," +
                      ENTRY(3,xsolog.commline,",") +  "," +
                             subser.servcom + "=" + 
                      STRING(subser.ssstat).    
                      
   IF csolog.TimeSlotTMS > 0 THEN 
   csolog.TimeSlotTMS = xsolog.TimeSlotTMS + 0.00600.



   IF csolog.TimeSlotTMS = 0 THEN DO:
      csolog.timeslottms = fmakets().
      MESSAGE 
      "Service order request #" string(csolog.solog) 
      " has been saved to the system."
                                                                   SKIP
      "Request is send to the activation server. "                    SKIP
      "Service will be activated within 20 minutes."                  SKIP(1)
      "ALL Sent Service Order requests and their current status "     SKIP
      "can be browsed from service order log (SOLog)." 

      VIEW-AS ALERT-BOX TITLE "Service Order Request".
   END.
   ELSE 
   MESSAGE 
   "Service order request #" string(csolog.solog) 
   " has been saved to the system."
                                                                    SKIP(1)
   "This activation request is scheduled and will be sent to "         SKIP
   "activation server " fTS2HMS(csolog.TimeSlotTMS) "."                           VIEW-AS ALERT-BOX TITLE "Service Order Request".  
END.   




