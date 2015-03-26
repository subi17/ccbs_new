PROCEDURE connect-tables:

   /********************************************************
   * This PROCEDURE is RUN after a SIM/ICC is assigned     *
   * TO a mobile subscriber.  It sets some key values of   *
   * related records AS desired.                           *
   *                                                       *
   * The associated Mobsub record is now in memory.        *
   ********************************************************/

   /******************************************
   * SIM card record FIRST ...               *
   ******************************************/

   FIND sim WHERE SIM.ICC = MobSub.ICC EXCLUSIVE-LOCK.
   
   ASSIGN 
      SIM.SimStat = sim-in-cont         /* StatusCode: SIM is in use and                                                   contract signed */
      SIM.CustNum = MobSub.CustNum.    /* CustNo is  assigned TO SIM  also*/

   /*******************************************
   * IMSI is already assigned TO a SIM via    *
   * the ICC code.  ASSIGN IMSI now into      *
   * mobsub  AND  customer etc ...            *
   *******************************************/

   FIND FIRST imsi WHERE IMSI.ICC = MobSub.ICC EXCLUSIVE-LOCK.

/* in this version ve assume that there is only one IMSI FOR EACH SIM */

   ASSIGN 
      IMSI.CustNum = MobSub.CustNum.  /* Customer's No   */

   /*******************************************
   * AND THEN MSISDN record                   *
   *******************************************/

   FIND msisdn WHERE MSISDN.CLI = MobSub.CLI EXCLUSIVE-LOCK.

   /************************
   * Connect TO SOG AND    *
   * perform the activation*
   * immediately           *
   ************************/

   RUN createms(MobSub.MsSeq,
                FALSE,              /* no batch, make it into SOG NOW !    */
                3,                  /* Normal, "own", immediate activation */
                msisdn.portingDate, /* Omit INport DATE                    */
                msisdn.portingtime,  /* Omit INport Time                    */
                MobSub.ActivationDate,    /* Activation DATE               */

                OUTPUT new-so-seq). /* no. of NEW solog                    */



   /* did it go well ? */
   FIND solog WHERE 
        Solog.Brand = gcBrand AND 
        SOLog.SoLog = new-so-seq NO-LOCK.

   IF AVAIL solog then ASSIGN mobsub.MsStatus = 2. /* On going */

   IF avail customer THEN iilang = customer.Language.
   else                   iilang = 1.

   lcAlarmMess = fGetTxt(INPUT "SMS",
                          "MNP2",
                          TODAY,
                          iiLang).
   FIND first msisdn WHERE 
              msisdn.cli = mobsub.cli  no-error.
 
   lddate = msisdn.portingdate.

   IF lddate = ? then lddate = today.

   fReplaceSMS (INPUT lcalarmmess, mobsub.msseq , lddate, OUTPUT lcalarmmess).
 
   
   CREATE CallAlarm.
   ASSIGN
      CallAlarm.ActStamp   = fmakets().
      
   ASSIGN
      CallAlarm.CLSeq      =  0
      CallAlarm.CASeq      =  NEXT-VALUE(CallAlarm)
      CallAlarm.CustNo     =  Mobsub.CustNum 
      CallAlarm.CLI        =  solog.cli
      CallAlarm.DeliStat   =  1            
      CallAlarm.Delitype   =  1  
      CallAlarm.DeliPara   = "1"
      CallAlarm.DeliMsg    = lcAlarmMess
      CallAlarm.Limit      = 0
      CallAlarm.CreditType = 9
      CallAlarm.Brand      = gcBrand .

END PROCEDURE.

