/*===========================================================================
 MODULE ........: msopenbatch.i
 APPLICATION ...: TMS
 TASK ..........: create open callalarm
 CREATED .......: 29.01.03 jp
 CHANGED .......: 29.12.05/aam separated from msopenbatch.p, 
                               use fUpdateSubSer etc. 
                  22.06.06/aam new value of msstatus 47,
                               set dl8 on 
 VERSION .......:
============================================================================*/

/* callers: msopenbatch.p
            msrequest.i
*/

&IF "{&MSOPENBATCH_I}" NE "YES"
&THEN

&GLOBAL-DEFINE MSOPENBATCH_I YES

DEF BUFFER bSaldoMobSub FOR MobSub.

DEF VAR lcOpenServCom AS CHAR NO-UNDO INIT "KLLSALDO".


FUNCTION fOpenSaldoBarring RETURNS LOGIC:

   DEF VAR ldCurrTime AS DEC  NO-UNDO.
   DEF VAR lcCommLine AS CHAR NO-UNDO.
   
   /* handle only those that have been closed due to saldo agreement */
   IF LOOKUP(STRING(MobSub.MsStatus),"7,37,47") = 0 THEN RETURN FALSE.
   
   /* set service component to zero */
   fUpdateSubSer(MobSub.MsSeq,
                 lcOpenServCom,
                 TODAY,
                 0,
                 "",
                 FALSE).

   IF MobSub.MsStatus = 7 THEN DO:
   
      lcCommLine =  "ST"  + ","                      /* Action = SET */
                          +  MobSub.CLI      + ","   /* MSISDN       */
                          +  MobSub.ICC      + "," 
                          +  lcOpenServCom + "=0" .
      
      /* check that there already isn't a pending solog for opening */
      IF NOT CAN-FIND(FIRST Solog NO-LOCK WHERE
                            Solog.MsSeq    = MobSub.MsSeq AND
                            Solog.Stat     = 0            AND
                            Solog.CommLine = lcCommLine)
      THEN DO:
      
         ldCurrTime = fMakeTS().
      
         CREATE SOLog.
         ASSIGN SOlog.Solog        = NEXT-VALUE(Solog)
                SOLog.CreatedTS    = ldCurrTime      /* Created NOW     */
                SOLog.ActivationTS = ldCurrTime      /* Activate NOW    */ 
                Solog.TimeslotTms  = ldCurrTime + 0.00540  /* 9 minutes */ 
                SOLog.MsSeq        = MobSub.MsSeq   /* Mobile Subscription */
                SOLog.CLI          = MobSub.CLI     /* MSISDN          */
                SOLog.Stat         = 0              /* just created    */
                Solog.Brand        = gcBrand 
                SOLog.CommLine     = lcCommLine
                SoLog.Users        = katun.
      END.                                 
   END.                

   FIND bSaldoMobSub WHERE RECID(bSaldoMobSub) = RECID(MobSub) EXCLUSIVE-LOCK.
   
   IF  bSaldoMobSub.MsStatus = 37 THEN bSaldoMobSub.MsStatus = 30.
   ELSE IF bSaldoMobSub.MsStatus = 47 THEN bSaldoMobSub.MsStatus = 40.
   ELSE bSaldoMobSub.MsStatus = 4.
   
   /* set debt lock on */
   IF bSaldoMobSub.MsStatus = 40 THEN DO:
   
      /* alarmbatch creates sologs from callalarms */
      CREATE CallAlarm.
      ASSIGN CallAlarm.ActStamp    = ldCurrTime + 0.01080 /* 9 min. after kll */
             CallAlarm.CLSeq       = 0
             CallAlarm.CASeq       = NEXT-VALUE(CallAlarm)
             CallAlarm.DeliStat    = 1            
             CallAlarm.Limit       = 100
             CallAlarm.Brand       = gcBrand 
             CallAlarm.CreditType  = 66
             CallAlarm.CustNo      = bSaldoMobSub.CustNum
             CallAlarm.CLI         = bSaldoMobSub.CLI
             CallAlarm.Delitype    = 4
             CallAlarm.DeliMsg     = "Barr"
             CallAlarm.DeliPara    = "22"
             /* must be set to normal, so that alarmbatch can handle this */
             bSaldoMobSub.MsStatus = 4.

      RELEASE CallAlarm.
   
   END. 
   
   RELEASE bSaldoMobSub.
   
   RETURN TRUE.
   
END FUNCTION.

&ENDIF