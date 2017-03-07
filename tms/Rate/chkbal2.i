/* chkbal.i         2003/jp

   changes:         21.05.03/aam fInvBal, fclbal,
                                 fGetText, fAlarmMessage etc.

                    17.06.03/jp  fIsPnpAllowed             
                    21.06.04 jp  fCreateServiceLimit
*/

DEF TEMP-TABLE TTCallLimit NO-UNDO
   LIKE CallLimit.

DEF VAR lcDeliPara   AS CHAR NO-UNDO.
DEF VAR lcAlarmMess  AS CHAR NO-UNDO. 
DEF VAR liDefCLCust  AS INT  NO-UNDO.
DEF VAR liDefMobCust AS INT  NO-UNDO.

ASSIGN liDefMobCust = fCParamI("DefCreditLimitMCust")
       liDefCLCust  = fCParamI("DefCreditLimitCust").

{Func/fgettxt.i}

/* get alarm message from information texts */
FUNCTION fAlarmMessage RETURNS LOGICAL
   (iiLang AS INT,
    icCLI  AS CHAR,
    idBal  AS DEC).

   DEF VAR lcKey   AS CHAR NO-UNDO.

   CASE ttCallLimit.CreditType:
   WHEN 1 THEN lcKey = "CLimit".
   WHEN 2 THEN lcKey = "Reminder".
   WHEN 3 THEN lcKey = "Agree".
   WHEN 9 THEN lckey = "info".
   END CASE.

   ASSIGN lcAlarmMess = ""
          lcDeliPara  = "".

   /* SMS REMINDER */          
   IF ttCallLimit.DeliType = 1  THEN DO:

      IF ttCallLimit.cli = "" OR lcKey = "Climit" 
      THEN lcAlarmMess = fGetTxt(INPUT "SMS",
                                 lcKey + "_" + String(ttCallLimit.Limit),
                                 TODAY,
                                 iiLang).

      ELSE lcAlarmMess = fGetTxt(INPUT "SMS",
                                 lcKey + "_common",
                                 TODAY,
                                 iiLang).
   END.

   /* EMAIL */
   ELSE IF ttCallLimit.DeliType = 2 THEN DO:

      lcAlarmMess = fGetTxt(INPUT "EMAIL",
                            lcKey,
                            TODAY,
                            iiLang).

      /* eMail address */
      IF ttCallLimit.DeliPara NE "" 
      THEN lcDelipara = ttCallLimit.DeliPara.
      ELSE lcDelipara = Customer.email.
   END.

   /* eLetter */
   ELSE IF ttCallLimit.DeliType = 3 THEN DO:

      lcAlarmMess = fGetTxt(INPUT "EKIRJE",
                            lcKey,
                            TODAY,
                            iiLang).

      lcDelipara = ttCallLimit.DeliPara.

   END.

   ELSE ASSIGN lcDelipara  = ttCallLimit.DeliPara
               lcAlarmMess = "SULKU".

   ASSIGN lcAlarmMess = REPLACE(lcAlarmMess,
                                "#limit%",                                     
                                STRING(ttCallLimit.Limit) + "%")
          lcAlarmMess = REPLACE(lcAlarmMess,
                                "#MSISDN", 
                                icCLI)
          lcAlarmMess = REPLACE(lcAlarmMess,
                                "#SALDO",
                                STRING(idBal) + " EUR")
          lcAlarmMess = REPLACE(lcAlarmMess,
                                "#CLI", 
                                icCLI)
                                . 


END FUNCTION.


FUNCTION fIsPnpAllowed RETURNS LOG
   (INPUT  Cli          AS CHAR,
    INPUT  CallDate     AS DATE,
    INPUT  Old_PNP      AS LOG,
    INPUT  New_PNP      AS LOG,
    OUTPUT PNPAllowed   AS LOG).

   DEF VAR period      AS INT NO-UNDO.
   DEF VAR liCalc      AS INT NO-UNDO.

   /* Compare Pnp Values before and after pnp analysis */
   IF      old_pnp = TRUE  AND
           new_pnp = TRUE     THEN liCalc =  0.
   ELSE IF old_pnp = TRUE  AND
           new_pnp = FALSE    THEN liCalc = -1.
   ELSE IF old_pnp = FALSE AND
           new_pnp = TRUE     THEN liCalc = 1.
   ELSE IF old_pnp = FALSE AND
           new_pnp = FALSE    THEN liCalc = 0.

   ASSIGN
      Period = Year(CallDate) * 100 + Month(CallDate).

   FIND FIRST PnpQty WHERE
              PnpQty.CLI    = CLI  AND
              pnpQty.Period = Period EXCLUSIVE-LOCK NO-ERROR.

   IF NOT AVAIL pnpQty THEN DO:
      CREATE PnpQty.
      ASSIGN
         PnpQty.Cli    = CLI
         PnpQty.Period = period
         PnpQty.Qty    = 1
         PnpAllowed    = TRUE.
   END.
END.   


FUNCTION fChkMobsubLimit RETURNS LOG
   (INPUT CustNo       AS INT,
    INPUT iccli        AS CHAR,
    INPUT MSSeq        AS INT,
    INPUT NotifyNumber AS CHAR,
    INPUT CallDate     AS DATE,
    INPUT MobSubLimit  AS DEC,
    INPUT CreditType   AS INT).

   DEF VAR unbilledbal   AS DEC   NO-UNDO INIT 0.
   DEF VAR CallTimestamp AS DEC   NO-UNDO.
   DEF VAR lcLimit       AS INT   NO-UNDO.
   DEF VAR liPeriod      AS INT   NO-UNDO.

   DEF VAR Target         AS CHAR   NO-UNDO.
   DEF VAR iLang          AS INT    NO-UNDO.
   DEF VAR ldaPeriod      AS DATE   NO-UNDO.
   DEF VAR ldeActStamp    AS DEC    NO-UNDO FORMAT "99999999.99999".

   ASSIGN 
      ldaPeriod   = DATE(Month(TODAY),1,Year(TODAY))
      liPeriod    = YEAR(CallDate) * 100 + MONTH(CallDate).

   FOR EACH ttCallLimit.
       DELETE ttCallLimit.
   END.

   DEF Buffer xxCallLimit FOR ttCallLimit.

   ASSIGN
   unbilledbal   = 0 .


   FOR EACH SaldoCounter NO-LOCK WHERE
            SaldoCounter.MSSEq  = Msseq AND
            SaldoCounter.Period = liPeriod  
   BY 
   SaldoCounter.moblimit.

       ASSIGN  
         unbilledbal = unbilledbal + SaldoCounter.amt
         lcLimit     = SaldoCounter.moblimit.

   END.


   IF CAN-FIND(FIRST callLimit WHERE 
                     CallLimit.Cli        =  icCli and 
                     CallLimit.CreditType = CreditType) THEN DO:

      FOR EACH   callLimit WHERE
                 CallLimit.Cli       = icCli      AND
                 CallLimit.dto      >= callDate  AND
                 CallLimit.dfrom    <= Calldate  AND
                 CallLimit.Limit    > lcLimit    AND
                 CallLimit.CreditType = CreditType NO-LOCK .

         CREATE ttCallLimit.
         BUFFER-copy CallLimit TO ttCallLimit.
      END.
   END.   
   ELSE DO:
      FOR EACH CallLimit WHERE
               CallLimit.Brand    = gcBrand      AND  
               CallLimit.CustNo   = liDefMobCust AND
               CallLimit.dto     >= CallDate     AND
               CallLimit.dfrom   <= CallDate     AND
               CallLimit.limit   > lclimit       AND
               CallLimit.CreditType = CreditType NO-LOCK.
         CREATE ttCallLimit.
         BUFFER-copy CallLimit TO ttCallLimit.
      END.
   END.      

   FOR EACH ttCallLimit BREAK BY ttCallLimit.Limit.   
      /* IF alarmed, dont make double alarm */
      IF CAN-FIND (First CallAlarm WHERE
                         CallAlarm.cli        = ttCallLimit.CLI   AND
                         CallAlarm.Delistat   = 1                 AND
                         CallAlarm.limit      = ttCallLimit.limit AND
                         CallAlarm.CreditType = CreditType        AND
                         CallAlarm.DeliType   = ttCallLimit.DeliType)
      THEN NEXT.                   

      /* CREATE alarm */
      IF unbilledbal > MobSubLimit * ttCallLimit.limit / 100 THEN DO:

         ASSIGN lcDeliPara = "".

         FIND Customer where Customer.CustNum = Custno NO-LOCK NO-ERROR.

         IF avail Customer THEN iLang = Customer.Lang.
         ELSE                  iLang = 1.

         fAlarmMessage(iLang,
                       icCLI,
                       MobSubLimit). 

         
         ldeActStamp = fmakets().
         
         IF ttCallLimit.delitype   = 1 AND 
            ttCallLimit.CreditType = 2 THEN
            ldeActStamp = fMakeOfficeTS() . 
         
         CREATE CallAlarm.
         CallAlarm.ActStamp = ldeActStamp.
         
         ASSIGN
            CallAlarm.CLSeq      = ttCallLimit.CLSeq
            CallAlarm.CASeq      = NEXT-VALUE(CallAlarm)
            CallAlarm.CustNo     = Customer.CustNum 
            CallAlarm.CLI        = iccli
            CallAlarm.DeliStat   = 1            
            CallAlarm.Delitype   = ttCallLimit.DeliType            
            CallAlarm.DeliPara   = lcDeliPara
            CallAlarm.DeliMsg    = lcAlarmMess
            CallAlarm.Limit      = ttCallLimit.limit
            CallAlarm.Brand      = gcbrand
            CallAlarm.CreditType = ttCallLimit.CreditType.

         RELEASE CallAlarm.

         /* alarm for notified number also */
         IF NotifyNumber NE "" AND 
            ttCallLimit.delitype = 1 THEN DO:

            CREATE CallAlarm.
            CallAlarm.ActStamp = ldeActStamp.
            
            ASSIGN
               CallAlarm.CLSeq    = ttCallLimit.CLSeq
               CallAlarm.CASeq    = NEXT-VALUE(CallAlarm)
               CallAlarm.CustNo   = Customer.CustNum
               CallAlarm.CLI      = NotifyNumber
               CallAlarm.DeliStat = 1
               CallAlarm.Delitype = ttCallLimit.DeliType
               CallAlarm.DeliPara = lcDeliPara
               CallAlarm.DeliMsg  = lcAlarmMess
               CallAlarm.Limit    = ttCallLimit.limit
               CallAlarm.Brand    = gcbrand
               CallAlarm.CreditType = ttCallLimit.CreditType.

            RELEASE CallAlarm.

         END.
   
         FIND FIRST xxCallLimit WHERE
              RECID(xxCallLimit) = RECID(ttCallLimit) EXCLUSIVE-LOCK NO-ERROR.

         ASSIGN
            xxCallLimit.ActStamp = fmakets().

         FOR EACH SaldoCounter WHERE
                  SaldoCounter.MSSeq  = MSSeq AND
                  SaldoCounter.Period = liPeriod:

            ASSIGN  
               SaldoCounter.moblimit = ttCallLimit.Limit.
            RELEASE SaldoCounter.
         END.
      END.
   END.
END.
