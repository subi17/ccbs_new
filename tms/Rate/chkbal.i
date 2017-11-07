/* chkbal.i         2003/jp

   changes:         21.05.03/aam fInvBal, fclbal,
                                 fGetText, fAlarmMessage etc.

                    14.12.05/aam notifynumber from subser
*/
{Syst/commali.i}
{Func/cparam2.i}       
{Func/finvbal.i}  
{Func/fclbal.i}    

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
                                STRING(idBal) + " EUR"). 
                                
          lcAlarmMess = REPLACE(lcAlarmMess,
                                "#CLI",
                                 iccli).


END FUNCTION.

