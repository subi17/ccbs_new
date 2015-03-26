{testpaa.i}
{refcode.i}

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liFlag AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcOld AS CHARACTER NO-UNDO. 
/*
input from SMSRULES.csv.

def stream sout.
output stream sout to result.txt.

DEFINE TEMP-TABLE ttSendRule
   FIELD i AS INT
   FIELD keyvalue AS char
INDEX keyvalue IS PRIMARY UNIQUE keyvalue. 

import unformatted lcline.
repeat:
   import unformatted lcLine.

   lcOld = "".

   FIND FIRST invtext where
      invtext.language = 1 and
      trim(invtext.invtext) = trim(entry(2,lcLine,";"))  and
      todate > today NO-LOCK NO-ERROR.

   liFlag = int(trim(entry(9,lcLine,";")) eq "x") * 4 +
      int(trim(entry(10,lcLine,";")) eq "x") * 2 +
      int(trim(entry(11,lcLine,";")) eq "x").

   
    IF NOT AVAIL invtext then do:
        FIND FIRST invtext where
         invtext.language = 1 and
         trim(invtext.invtext) = trim(entry(2,lcLine,";"))  and
         todate <= today NO-LOCK NO-ERROR.
      IF AVAIL invtext then lcOld  = "<OLD>".
   end.
   ELSE DO:
      FIND FIRST ttSendRule where
         ttSendRule.keyvalue = invtext.keyvalue NO-LOCK no-error.
      IF NOT AVAIL ttSendRule then do:
         create ttSendRule.
         assign
            ttSendRule.keyvalue = invtext.keyvalue
            ttSendRule.i = liFlag.
      end.
   END.
         
   put stream sout unformatted 
         lcOld
      (if avail invtext then 
         invtext.keyvalue else "N/A") "|"
         liFlag "|"
         entry(8,lcLine,";") "|"
         entry(2,lcLine,";") 
         skip.
end.
*/

DEFINE VARIABLE lcLang AS CHARACTER NO-UNDO. 
lcLang = "es_ES,es_CA,es_EU,es_GA,en".

DEFINE VARIABLE lcFormat AS CHARACTER NO-UNDO. 

DEFINE TEMP-TABLE ttText
   FIELD token as char
   FIELD type as char
   FIELD content AS char
INDEX token IS PRIMARY type token. 

def buffer binvtext for invtext.

FOR EACH invtext where
   lookup(target,"sms,email,orderconf") > 0 and
   Todate >= TODAY NO-LOCK:

   find binvtext where
      binvtext.target = invtext.target and
      binvtext.keyvalue = invtext.keyvalue and
      binvtext.lang = 1 and
      binvtext.fromdate <= today and
      binvtext.todate >= today NO-LOCK.

END.

def stream slog.
lcFormat = "luna".
if lcFormat = "luna" then
output stream slog to /apps/yoigo/tms_support/testing/log/invtexts_luna.txt.
else output stream slog to /apps/yoigo/tms_support/testing/log/invtexts.txt.

DEFINE VARIABLE lcLangIDs AS CHARACTER NO-UNDO. 
lcLangIDs = "1,5,3,2".
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcTranslations AS CHARACTER EXTENT 5 NO-UNDO.
DEFINE VARIABLE liLang AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcDesc AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcProcess AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcWhenSent AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSender AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcExplain AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcInvText AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTagText AS CHARACTER NO-UNDO. 

FOR EACH invtext where
   lookup(target,"sms,email,orderconf") > 0 and
   fromdate <= TODAY AND
   Todate >= TODAY NO-LOCK:

   if lcFormat = "luna" and lang ne 1 then next.

   /* not used */
   IF lookup(invtext.keyvalue,"DATALIMIT1,PG009,PG018,PG023") > 0 THEN NEXT.

   /* propably not used */
   IF lookup(invtext.keyvalue,"InvInfo,PostToPre,PreToPost,TARJ2_CONT,TARJ2_CONT2,TARJ2_TARJ,TARJ_CONT,TARJ_CONT2,TARJ_TARJ2,TopUpATM,TopUpATMCan,TopUpConf,TopUpSaldo,YOIGOYOIBALNOK,EmailConfOld,EmailConfOldII") > 0 THEN NEXT.

/*   if target ne "sms" then next. */
   
   lcTranslations = "".

   do i = 1 to num-entries(lcLangIDs).
      lilang = int(entry(i, lcLangIDs)).
        
      IF lilang NE 1 THEN DO:
         find binvtext where
            binvtext.keyvalue = invtext.keyvalue and
            binvtext.lang = int(entry(i, lcLangIDs)) and
            binvtext.fromdate <= today and
            binvtext.todate >= today NO-LOCK NO-ERROR.
         IF AVAIL binvtext THEN DO:
            lcInvText = binvtext.InvText.
         END.
         ELSE DO:
            FIND FIRST RepText NO-LOCK WHERE
                       RepText.Brand      = gcBrand AND
                       RepText.TextType   = 32 AND
                       RepText.LinkCode   = STRING(InvText.ITNum) AND
                       RepText.FromDate  <= TODAY   AND
                       RepText.ToDate    >= TODAY   AND
                       RepText.Language   = lilang NO-ERROR.
             IF AVAIL RepText THEN lcInvText = RepText.RepText.
             ELSE lcInvText = "".
         END.
       END.
       ELSE lcInvText = InvText.InvText.
         
      
      IF lcInvText > "" THEN DO:
            lcExplain = "".
            
            if invtext.target ne "sms" then do:
               lcTranslations[lilang] = "\{noformat\}Subject: " + invtext.txttitle + chr(10) + "Content:" + chr(10).          
            
               if index(lcinvtext,"#OCONTRID") > 0 THEN lcInvText = REPLACE(lcInvText,"#OCONTRID","#ORDER_CONTRACT_ID").
               if index(lcinvtext,"#CUSTNAME") > 0 THEN lcInvText = REPLACE(lcInvText,"#CUSTNAME","#CUSTOMER_OR_COMPANY_FULL_NAME").
               if index(lcinvtext,"#CUSTADDR") > 0 THEN lcInvText = REPLACE(lcInvText,"#CUSTADDR","#CUSTOMER_ADDRESS").
               if index(lcinvtext,"#CUSTPOST") > 0 THEN lcInvText = REPLACE(lcInvText,"#CUSTPOST","#CUSTOMER_ZIPCODE+POSTOFFICE").
               if index(lcinvtext,"#DELADDR") > 0 THEN lcInvText = REPLACE(lcInvText,"#DELADDR","#DELIVERY_ADDRESS").
               if index(lcinvtext,"#DELPOST") > 0 THEN lcInvText = REPLACE(lcInvText,"#DELPOST","#DELIVERY_ZIPCODE+POSTOFFICE").
               if index(lcinvtext,"#CTNAME") > 0 THEN lcInvText = REPLACE(lcInvText,"#CTNAME","#SUBSCRIPTION_TYPE_NAME").

               lcTranslations[liLang] = lcTranslations[liLang] + lcinvtext + " \{noformat\}".          
               
               lcTagtext = " *TAG EXPLANATIONS* ".
               if index(lcInvText,"#MGMTIENDA") > 0 THEN DO:
                  lcTagText = lcTagText + chr(10) + "* #MGMTIENDA (all channels):" + "\{noformat\}" + fTeksti(307,liLang) + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#MGMREFEREE") > 0 THEN DO:
                  lcTagText = lcTagText + chr(10) + "* #MGMREFEREE (when MGM order):" 
                     + "\{noformat\}" + fTeksti(306,liLang)
                     + " + [ORDER_REFEREE_MSISDN]" + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#CAMPAIGN") > 0 THEN DO:
                  lcTagText = lcTagText + chr(10) + 
                     "* #CAMPAIGN (when order with campaign):" 
                      + "\{noformat\}" + fTeksti(298,liLang) + CHR(10) +
                           FILL("-",LENGTH(fTeksti(298,liLang)) + 2) + CHR(10) + fTeksti(299,liLang) + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#FATIME") > 0 THEN DO:
                  lcTagText = lcTagText + chr(10) + 
                     "* #FATIME (order with FAT):" + "\{noformat\}"
                     + fTeksti(300,liLang) + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#OFEES") > 0 THEN DO:
                  lcTagText = lcTagText +  chr(10) +
                     "* #OFEES (order fees):" + "\{noformat\}"
                     + STRING("[FEE_ITEM_1] [AMOUNT] EUR","X(30)") + CHR(10)
                     + STRING("...","X(30)") + CHR(10)
                     + STRING("[FEE_ITEM_N] [AMOUNT] EUR","X(30)") + CHR(10) 
                     + STRING(fTeksti(80,liLang),"X(30)") 
                     + "[AMOUNT] EUR" + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#PENALTYFEE") > 0 THEN DO:

                  lcTagText = lcTagText +  chr(10) +
                     "* #PENALTYFEE (order terminal penalty fee, different text for  penalty fee > 100 and minimum consumption or monthly fee > lowest value:" +
                     "\{noformat\}" +
                      CHR(10) + fTeksti(510,liLang)  + "\{noformat\}" + 
                      "\{noformat\}" +
                      CHR(10) + fTeksti(509,liLang)  + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#POSTDISC") > 0 THEN DO:

                  lcTagText = lcTagText + chr(10) +
                     "* #POSTDISC (renewal order discount info):" + "\{noformat\}" +
                     CHR(10) + fTeksti(304,liLang) + "\{noformat\}".
               end.
               
               if index(invtext.InvText,"#CONTACTDATA") > 0 THEN DO:

                  lcTagText = lcTagText + chr(10) +
                     "* #CONTACTDATA (corporate order contact customer (available / not available):" + "\{noformat\}" +

                 "[FIRSTNAME]" + " " +
                 "[SURNAME1]" + " " +
                 "[SURNAME2]" + CHR(10) +
                 "[MOBILE_NUMBER]" + CHR(10) +
                 "[EMAIL]" + CHR(10) +
                 "[ADDRESS]" + " " + "[ZIPCODE]" + " " +
                 "[POSTOFFICE]" + " " + "[REGION_NAME]" + chr(10) + 

                chr(10) + fTeksti(302,liLang) + "\{noformat\}".

               end.
               
               if index(invtext.InvText,"#OMNPDATA") > 0 THEN DO:

                  lcTagText = lcTagText + chr(10) + 
                     "* #OMNPDATA (MNP order):" +  "\{noformat\}"
                    
                        + chr(10) + fTeksti(287,liLang)
                        + CHR(10) + 
                     FILL("-",LENGTH(fTeksti(287,liLang)) + 2) + CHR(10)
                     
                      + fTeksti(288,liLang) + " " +
                              "[OLD_OPERATOR_NAME]" + CHR(10)  

                     + fTeksti(289,liLang) + " " 
                     + "[" + fTeksti(263,liLang) + "]" + CHR(10) +
                         fTeksti(290,liLang) + " " + "[OLD_ICC]" + "\{noformat\}".
               end.

               if index(invtext.InvText,"#OPMETHOD") > 0 THEN DO:
                     lcTagText = lcTagText +  chr(10) +
                        "* #OPMETHOD (order payment method, IF cashfee.amount > 0 ELSE empty):" + "\{noformat\}"
               + chr(10) + fTeksti(292,liLang)
                        + CHR(10) +
                         FILL("-",LENGTH(fTeksti(292,liLang)) + 2) + CHR(10) 
                 + "[" + fTeksti(263,liLang) + "]" + "\{noformat\}".

               end.
               
               if index(invtext.InvText,"#MDUB") > 0 THEN DO:
                     lcTagText = lcTagText + chr(10) +
                        "* #MDUB (order with Bono 8):" + "\{noformat\}"
                     + fTeksti(369,liLang) + "\{noformat\}"
                     + chr(10) +
                        "* #MDUB (order with Bono 15):" + "\{noformat\}"
                     + fTeksti(511,liLang) + "\{noformat\}".
               END.
               
               if index(invtext.InvText,"#LEGALFINANCING") > 0 THEN DO:
                  lcTagText = lcTagText + chr(10) + "* #LEGALFINANCING (order with installment (payterm) contract):" 
                     + "\{noformat\}" + CHR(10) + fTeksti(517,liLang) + "\{noformat\}".
               end.
                  
               if index(invtext.InvText,"#OBANKDATA") > 0 THEN DO:
                  
                  lcTagText = lcTagText + chr(10) +
                        "* #OBANKDATA (bank account info for postpaid order):" + "\{noformat\}".
                  
    /*              if lilang = 5 then   
                  lcTagText = lcTagText + chr(10) + 
                        "#OBANKDATA (bank account info for postpaid order, english renewal + normal):" + chr(10). */
         
            DEFINE VARIABLE lcBankAccHeader AS CHARACTER NO-UNDO.
            DEFINE VARIABLE lcBkAccHolderFieldLabel AS CHARACTER NO-UNDO.
            DEFINE VARIABLE lcBkAccFieldLabel AS CHARACTER NO-UNDO.
/*            IF liLang = 5 THEN
            DO:
                lcBankAccHeader = fTeksti(506,liLang).
                lcBkAccHolderFieldLabel = fTeksti(507,liLang).
                lcBkAccFieldLabel = fTeksti(508,liLang).
            
               lcTagText = lcTagText + CHR(10) +
                lcBankAccHeader + CHR(10) +
                FILL("-",LENGTH(lcBankAccHeader) + 2) + CHR(10) +
                lcBkAccHolderFieldLabel + " " + "[ORDER_CUSTOMER_FULL_NAME]" + 
                CHR(10) + lcBkAccFieldLabel + " " + "[BANK_ACCOUNT]" + CHR(10).
             
            END.
  */          
            lcBankAccHeader = fTeksti(293, liLang).
            lcBkAccHolderFieldLabel = fTeksti(294, liLang).
            lcBkAccFieldLabel = fTeksti(295, liLang).

            lcTagText = CHR(10) + lcTagText + CHR(10) +
                lcBankAccHeader + CHR(10) +
                FILL("-",LENGTH(lcBankAccHeader) + 2) + CHR(10) +
                lcBkAccHolderFieldLabel + " " + "[CUSTOMER_OR_COMPANY_FULL_NAME]" + CHR(10) +
                      lcBkAccFieldLabel + " " + "[BANK_ACCOUNT]" + " \{noformat\}".

               end.

               lcTranslations[liLang] =  lcTranslations[liLang] + lcTagtext.          
            end.
            ELSE lcTranslations[liLang] =  lcTranslations[liLang] + lcinvtext.
         end.
         else lcTranslations[lilang] = "N/A".          
   end.

    /* Description||Spanish||English||Catalan||Process||When sent||Sender" skip. */
   ASSIGN
      lcProcess = ""
      lcWhenSent = ""
      lcSender = "800622800"
      lcDesc = "".

   case invtext.keyvalue:
      when "ACC_Accepted" then assign
         lcProcess = "ACC"
         lcWhenSent = "ACC is accepted (validations are passed)".
      when "ACC_Cancelled" then assign
         lcProcess = "ACC"
         lcWhenSent = "ACC is manually cancelled".
      when "ACC_PreviousDay" then assign
         lcProcess = "ACC"
         lcWhenSent = "Previous day at 19 before ACC".
      when "ACC_Done" then assign
         lcProcess = "ACC"
         lcWhenSent = "Actual ACC happens".
      when "ACC_Rejected" then assign
         lcProcess = "ACC"
         lcWhenSent = "ACC request is rejected".
      when "CompInfo" then assign
         lcProcess = "Prepaid Top Ups. Manual positive compensation (script originated)"
         lcWhenSent = "Top up is sent and handled".
      when "corporateWelcome" then assign
         lcSender = "622622622"
         lcDesc = "Welcome SMS for new Corporate customers"
         lcProcess = "Order handling"
         lcWhenSent = "At 10:00 (same or following day)".
      when "DATALIMIT2" then assign
         lcDesc = "IPL data limit fraud control"
         lcProcess = "Fraud Control"
         lcWhenSent = "Defined data limit (GB) is exceeded".
      when "INTERNATFRAUD1" then assign
         lcDesc = "International calls fraud control"
         lcProcess = "Fraud Control"
         lcWhenSent = "Defined limit (Euros) is exceeded".
      when "INTERNATFRAUD2" then assign
         lcDesc = "International calls fraud control"
         lcProcess = "Fraud Control"
         lcWhenSent = "Defined limit (euros) is exceeded".
      when "MGMPromotedAct" then assign
         lcProcess = "MGM"
         lcWhenSent = "Commissions (FAT/TopUp) are created".
      when "MGMRefereeAct" then assign
         lcProcess = "MGM"
         lcWhenSent = "Commissions (FAT/TopUp) are created".
      when "MGMRefereeCre" then assign
         lcProcess = "MGM"
         lcWhenSent = "Subscription is created".
      when "MGMTopupRecharge" then assign
         lcProcess = "MGM"
         lcWhenSent = "MGM topup is charged".
      when "MNPConf" then assign
         lcProcess = "MNP"
         lcWhenSent = "Incoming MNP is confirmed"
         lcSender = "800622600".
      when "MNPConfTime" then assign
         lcProcess = "MNP"
         lcWhenSent = "Incoming MNP is started"
         lcSender = "800622600".
      when "MNPDone" then assign
         lcProcess = "MNP"
         lcWhenSent = "Incoming MNP is done, subsription is activated"
         lcSender = "622".
      when "MNPFinRemDirect" OR when "MNPFinRemPos" then assign
         lcProcess = "MNP"
         lcWhenSent = "Day before incoming MNP porting time at 09:00"
         lcSender = "800622600".
      when "MNPReject" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is rejected"
         lcSender = "800622111".
      when "MNPEnumePOS" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is rejected"
         lcSender = "800622111".
      when "MNPIccidPos" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is rejected"
         lcSender = "800622111".
      when "MNPRejectedPosClose" then assign
         lcProcess = "MNP"
         lcWhenSent = "Rejected MNP IN POS order with reason AREC ENUME,RECH_BNUME,AREC EXIST,RECH_IDENT or RECH_ICCID is closed"
         lcSender = "800622600".
      when "MNPCancel" then assign
         lcProcess = "MNP"
         lcWhenSent = "Outgoing MNP is cancelled"
         lcSender = "800622111". /* 622 ?*/
      when "MNPINCancel" OR WHEN "MNPINCancelPOS" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN process is cancelled"
         lcSender = "800622600".
      when "MNPCanAfterConf" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is cancelled after receiving ACON and order channel is cc,telesales,self"
         lcSender = "800622600".
      when "MNPCanBeforeConf" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is cancelled before receiving ACON and order channel is cc,telesales,self"
         lcSender = "800622600".
      when "MNPIdentDirect" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is rejected (IDENT reason), and orderchannel is not POS"
         lcSender = "800622111".
      when "MNPIdentPOS" then assign
         lcProcess = "MNP"
         lcWhenSent = "MNP IN is rejected (IDENT reason), and orderchannel is POS"
         lcSender = "800622111".
      when "MNPCancelRetention" then assign
         lcProcess = "MNP"
         lcWhenSent = "Pending retention order is released after MNP OUT process is cancelled"
         lcSender = "622".
      when "MNPCancelRetentionOnHold" then assign
         lcProcess = "MNP"
         lcWhenSent = "Pending retention order is moved to on hold order queue after MNP OUT process is cancelled"
         lcSender = "622".
      when "MNPCloseRetention" then assign
         lcProcess = "MNP"
         lcWhenSent = "Pending retention order is closed after MNP OUT process is finished"
         lcSender = "<Retention platform SMS sender>".
      when "MNPRescue" then assign
         lcProcess = "MNP"
         lcWhenSent = "Subscription is marked to MNP retention file"
         lcSender = "<Retention platform SMS>".
      when "PPMinimCons" then assign
         lcProcess = "Prepaid Minimum Consumption"
         lcWhenSent = "Monthly minimum consumption is created"
         lcSender = "622".
      when "PREMIUMFRAUD1" then assign
         lcProcess = "Fraud Control"
         lcWhenSent = "Defined limit (euros) is exceeded".
      when "PREMIUMFRAUD2" then assign
         lcProcess = "Fraud Control"
         lcWhenSent = "Defined limit (euros) is exceeded".
      when "RenoveOrderConf" then assign
         lcProcess = "Renove"
         lcWhenSent = "Direct channel renove order is handled in CCBS".
      when "RenoveOrderPOS" then assign
         lcProcess = "Renove"
         lcWhenSent = "Indirect channel (pos) renove order is handled in CCBS".
      when "RenoveOnHold" then assign
         lcProcess = "Renove"
         lcSender = "622"
         lcWhenSent = "When renewal order qoes to queue 31 (RENEWAL HOLD)".
      when "RenoveOrderInst" then assign
         lcProcess = "Renove"
         lcWhenSent = "Direct channel renove order with installment is handled in CCBS".
      when "SMSBundle" then assign
         lcProcess = "SMS Bundle"
         lcWhenSent = "SMS Bundle is activated".
      when "STC_DONE" then assign
         lcProcess = "STC"
         lcWhenSent = "After subscription type is changed in NW and CCBS (10:00-19:00)"
         lcSender = "622".
      when "CancelledSTCDueToBarring" then assign
         lcProcess = "STC"
         lcWhenSent = "After Fraud Barring activation if ongoing STC is cancelled".
      when "TopUpOrder" then assign
         lcProcess = "Order handling"
         lcWhenSent = "After prepaid subscription is activated and topup is successfully charged"
         lcSender = "800622800 when campaign, otherwise 622".
      when "TopUpReminder" then assign
         lcProcess = "Topup"
         lcWhenSent = "?".
      when "TOTALTRAFFIC1" then assign
         lcProcess = "Fraud Control"
         lcWhenSent = "Defined limit (euros) is exceeded".
      when "TOTALTRAFFIC2" then assign
         lcProcess = "Fraud Control"
         lcWhenSent = "After prepaid subscription is activated and topup is successfully charged".
      when "YOIGOYOIBALNOK" then assign
         lcProcess = "YOIGOYOIGO"
         lcWhenSent = "Balance query is failed"
         lcSender = "? (not sent by CCBS)".
      when "YOIYOIBALANCE" then assign
         lcProcess = "YOIGOYOIGO"
         lcWhenSent = "Balance query is successful"
         lcSender = "? (not sent by CCBS)".
      when "FusionEmail" then assign
         lcDesc = "Fusion summary email".
      when "ActEmailInvoice" then assign
         lcDesc = "Email invoice activation".
      when "SendEmailInvoice" then assign
         lcDesc = "Email invoice".
      when "EmailConf" then assign
         lcDesc = "Order confirmation".
      when "EmailConfCif" then assign
         lcDesc = "Order confirmation (corporate customer)".
      when "RenewalConf" then assign
         lcDesc = "Renewal order confirmation".
      when "RenewalConfCIF" then assign
         lcDesc = "Renewal order confirmation (corporate customer)".
      when "SMSInvoice" then assign
         lcSender = "Fact.Yoigo"
         lcProcess = "Invoice"
         lcWhenSent = "Invoice contains one sub invoices. Invoice amount is greater then 0. (included in the CSB19 file) AND (Is the first invoice of the subscription OR has this service active) (10:00-21:00)".
      when "LogisticDelivery_ALL" then assign
         lcSender = "622"
         lcProcess = "Logistics"
         lcWhenSent = "Order delivery is confirmed".
      when "LogisticIncident_ASM" or 
      when "LogisticIncident_SEUR" or 
      when "LogisticIncident_CORREOS" then assign
         lcSender = "622"
         lcProcess = "Logistics"
         lcWhenSent = "Order has not been picked up from the address provided".
      when "LogisticNotDeliv_ASM" or
      when "LogisticNotDeliv_SEUR" or
      when "LogisticNotDeliv_CORREOS"
      then assign
         lcSender = "622"
         lcProcess = "Logistics"
         lcWhenSent = "Unable to deliver order".
      when "FRONTLIMIT1" then assign
         lcProcess = "FRONT"
         lcWhenSent = "1st limit exceeded".
      when "FRONTLIMIT2" then assign
         lcProcess = "FRONT"
         lcWhenSent = "2nd limit exceeded".
      when "PINRESET" then assign
         lcProcess = "IVR"
         lcWhenSent = "Subscription PIN code is reseted through IVR".
      when "BBActMan" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is manually activated"
         lcSender = "622".
      when "BBActNewSim" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "New SIM only subscription is purchased with BlackBerry service"
         lcSender = "622".
      when "BBActNewTerm" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "New subscription with a terminal is purchased with BlackBerry service"
         lcSender = "622".
      when "BBDeActBundPost_1" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "If BlackBerry service is active when a manual postpaid voice data bundle deactivation request is created"
         lcSender = "622".
      when "BBDeActBundPost_2" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is deactivated due to postpaid voice data bundle manual termination"
         lcSender = "622".
      when "BBDeActBundPre_1" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "If BlackBerry service is active when a manual prepaid voice data bundle deactivation request is created"
         lcSender = "622".
      when "BBDeActBundPre_2" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is deactivated due to prepaid voice data bundle manual termination"
         lcSender = "622".
      when "BBDeActMan" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is manually deactivated"
         lcSender = "622".
      when "BBDeActSTCPostV1" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service will be deactivated due to STC"
         lcSender = "622".
      when "BBDeActSTCPostV2" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is deactivated due to STC"
         lcSender = "622".
      when "BBDeActSTCPreD_1" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service will be deactivated due to STC"
         lcSender = "622".
      when "BBDeActSTCPreD_2" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is deactivated due to STC"
         lcSender = "622".
      when "BBDeActSTCPreV_1" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service will be deactivated due to STC"
         lcSender = "622".
      when "BBDeActSTCPreV_2" then assign
         lcProcess = "BlackBerry"
         lcWhenSent = "BlackBerry service is deactivated due to STC"
         lcSender = "622".

      when "BundleActivation" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Bono/IPL bundle or Bono/IPL/DSS upsell is activated"
         lcSender = "22642 (Bono 8) / 22622 (Bono 12 or any upsell) / 22644 (Other Bono)".

      when "PMDUBBalCheck" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 renewal balance check, subscription has balance less than 10 euros"
         lcSender = "22642".
      when "PMDUBBalCheck1" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 renewal balance check, 3 days before end of the month"
         lcSender = "22642".
      when "PMDUBBalCheck1FM" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 renewal first month balance check, 3 days before end of the month"
         lcSender = "22642".
      when "PMDUBBalCheck2" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 renewal balance check, last day of the month"
         lcSender = "22642".
      when "PMDUBBalCheck2FM" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 renewal first month balance check, last day of the month"
         lcSender = "22642".

      when "PMDUBBalChk" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 manual activation fails due to insufficiennt balance"
         lcSender = "22642".
      when "PMDUBBalFail" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 manual activation charging fails"
         lcSender = "22642".
      when "PMDUBDeActBatch" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 is deactivated due to error in automatic monthly renewal"
         lcSender = "22642".
      when "PMDUBAct" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 is activated"
         lcSender = "22642". 

      when "TARJ7Act" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After contract activation"
         lcSender = "22622". 
      when "TARJ7DeAct" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After contract deactivation"
         lcSender = "22622". 
      when "TARJ7RenewalReminder" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Day before contract renewal"
         lcSender = "22622". 
      when "UpsellTARJ7Act" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After upsell activation"
         lcSender = "22622". 
      when "UpsellTARJ7Failed" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After upsell activation failure"
         lcSender = "622". 
      when "UpsellTARJ7NoBal" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After upsell activation failure due to balance"
         lcSender = "622". 
      
      when "WelcomeSubs" then assign
         lcProcess = "Activation"
         lcWhenSent = "After subscription activation"
         lcSender = "22622". 

      when "PMDUBDeActMan" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 is manually deactivated"
         lcSender = "22642". 
      when "PMDUBDeActSTC" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 is deactivated due to STC"
         lcSender = "22642".
      when "PMDUBUBalChk" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 Upsell manual activation fails due to insufficient balance"
         lcSender = "622".
      when "PMDUBUBalFail" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 Upsell activation charging fails"
         lcSender = "622".
      when "UpsellBonoLimit" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Bono HSDPA usage limit is exceeded"
         lcSender = "22622".
      
      when "UpsellTARJ6NoBal" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Before request creation and activation"
         lcSender = "622".
      when "UpsellTARJ6Req" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After request creation"
         lcSender = "22622".

      when "UpsellPMDUBHSDPA" or
      when "UpsellPMDUBLimit" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Prepaid Bono 8 HSDPA usage limit is exceeded"
         lcSender = "622".
      
      when "UpsellDSSHSDPA" or
      when "UpsellDSSLimit" then assign
         lcProcess = "DSS"
         lcWhenSent = "DSS HSDPA usage limit is exceeded"
         lcSender = "622".
     
     when "UpsellHSDPA" or when
           "UpsellIPLLimit" or when
           "DUBLimit" then assign
         lcProcess = "Upsell"
         lcWhenSent = "95 percent of maximum data limit is exceeded"
         lcSender = "622".
      
      when "UpsellCONTFVoice1" then assign
         lcProcess = "Flat Tariffs"
         lcWhenSent = "80 percent of maximum voice package limit is exceeded"
         lcSender = "622".
      when "UpsellCONTFVoice2" then assign
         lcProcess = "Flat Tariffs"
         lcWhenSent = "100 percent of maximum voice package limit is exceeded"
         lcSender = "622".
      when "WelcomeCONTF" then assign
         lcProcess = "Flat Tariffs"
         lcWhenSent = "New subscription is activated"
         lcSender = "622".

      when "UpsellLimit" then assign
         lcProcess = "Upsell"
         lcWhenSent = "Maximum upsell purchases per month is exceeded through external API"
         lcSender = "622".
      
      when "BundleTerminate" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "Bono X bundle is terminated with STC"
         lcSender = "22642 (Bono 8) / 22622 (Bono 12) / 22644 (Other Bono X)".
      
      when "BTCBundelDeAct" then assign
         lcProcess = "BTC"
         lcWhenSent = "Ongoing BTC request is cancelled when current bundle cancellation request is created (requests created in TMS are excluded)"
         lcSender = "622".

      when "BTCAct" then assign
         lcProcess = "BTC"
         lcWhenSent = "Postpaid voice BTC request is created (requests created in TMS are excluded)"
         lcSender = "622".
      
      when "BTCDeAct" then assign
         lcProcess = "BTC"
         lcWhenSent = "Postpaid voice BTC request is cancelled (cancellations done in TMS are excluded)"
         lcSender = "622".
      
      when "BTCActUpgrade" then assign
         lcProcess = "BTC"
         lcWhenSent = "BTC request with upgrade upsell is created"
         lcSender = "22622".
      
      when "MDUBDeActMan" then assign
         lcProcess = "Data bundle"
         lcWhenSent = "After Bono 8 manual termination request creation"
         lcSender = "22642".
      
      when "MDUB5DeActMan" or
      when "MDUB9DeActMan" then assign
         lcProcess = "Data bundle"
         lcWhenSent = "After manual termination request creation"
         lcSender = "22622".
      
      when "MDUB2DeActMan" or
      when "MDUB3DeActMan" or 
      when "MDUB4DeActMan" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After manual termination request creation"
         lcSender = "22644".
      
      when "DATA5DeActMan" then assign
         lcProcess = "Data Bundle"
         lcWhenSent = "After manual termination request creation"
         lcSender = "22622".
      
      when "MultiSIMPrimarySTC" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "After STC"
         lcSender = "22622".
      when "MultiSIMSecondaryAct" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "After subscription creation"
         lcSender = "22622".
      when "MultiSIMSecondaryTerm" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "After subscription termination"
         lcSender = "22622".
      when "MultiSIMSecondarySTC" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "Before STC"
         lcSender = "22622".
      when "MultiSIMSecondaryTermRem1" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "After primary subscription termination"
         lcSender = "22622".
      when "MultiSIMSecondaryTermRem2" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "Seconday SIM termination reminder 2 (20 days before)"
         lcSender = "22622".
      when "MultiSIMSecondaryTermRem3" then assign
         lcProcess = "MultiSIM"
         lcWhenSent = "Seconday SIM termination reminder 3 (2 days before)"
         lcSender = "22622".
      
      when "STCCONT" or
      when "STCCONT4" or
      when "STCCONT5" or
      when "STCCONT6" or
      when "STCCONT7" or
      when "STCCONT8" or
      when "STCCONTS" or
      when "STCCONTF10" or
      when "STCCONTF20" or
      when "STCCONTF20D" or
      when "STCCONTF30" or
      when "STCCONTF40" or
      when "STCCONTF55" or
      when "STCCONTD9" or
      when "STCCONTS30" or
      when "STCCONTS39" or
      when "STCIPL25" or
      when "STCIPL35" or
      when "STCIPL8" or
      when "STCIPL15" or
      when "STCTARJ" or
      when "STCTARJ4" or
      when "STCTARJ5" or
      when "STCTARJ6" or
      when "STCTARJRD1" or 
      when "STCCONTSF10" or 
      when "STCCONTSF14" or 
      when "STCCONTFF" then assign
         lcProcess = "STC"
         lcWhenSent = "After STC request is created"
         lcSender = "622".

      when "STCORDERCONTFF" or when
           "STCORDERCONTSF10" or when
           "STCORDERCONTSF14" then assign
         lcProcess = "STC"
         lcWhenSent = "Fusion STC order is stored to TMS"
         lcSender = "22622".

      when "STC_Cancelled" then assign
         lcProcess = "STC"
         lcWhenSent = "STC request cancellation"
         lcSender = "622".
      
      when "STC_Requested" then assign
         lcProcess = "STC"
         lcWhenSent = "STC request is created"
         lcSender = "22622".
      
      when "DSSAct" then assign
         lcProcess = "DSS"
         lcWhenSent = "After successful DSS activation"
         lcSender = "622".
      when "DSSActCancel" then assign
         lcProcess = "DSS"
         lcWhenSent = "After automatic cancellation of DSS activation request pending in queue more that 14 days"
         lcSender = "622".
      when "DSSActOtherSubs" then assign
         lcProcess = "DSS"
         lcWhenSent = "After succesfull subscription addition to existing DSS group"
         lcSender = "622".
      when "DSSAutoDeAct" then assign
         lcProcess = "DSS"
         lcWhenSent = "After DSS is automatically deactivated due to STC or ACC"
         lcSender = "622".
      when "DSSManDeAct" then assign
         lcProcess = "DSS"
         lcWhenSent = "After DSS deactivation request is created manually or by some other manual process"
         lcSender = "622".
      
      when "HSPARoamEUActivation" then assign
         lcProcess = "HSPA_ROAM_EU"
         lcWhenSent = "After bundle activation"
         lcSender = "+34633800800".
      when "HSPARoamEUErrorGeneral" then assign
         lcProcess = "HSPA_ROAM_EU"
         lcWhenSent = "General provisioning error"
         lcSender = "+34633800800".
      when "HSPARoamEUErrorUsage" then assign
         lcProcess = "HSPA_ROAM_EU"
         lcWhenSent = "Usage provisioning error"
         lcSender = "+34633800800".
      when "HSPARoamEURequest" then assign
         lcProcess = "HSPA_ROAM_EU"
         lcWhenSent = "After request creation"
         lcSender = "+34633800800".
      
      when "100_VoiceBDest" or
      when "300_VoiceBDest" then assign
         lcProcess = "BDest limit"
         lcWhenSent = "After BDest limit is exceeded".
      
      when "GetConsultID" then assign
         lcProcess = "SelfService"
         lcWhenSent = "Order contract ID query from Self Service external API"
         lcSender = "622622622".
      
      when "GetMultiConsultID" then assign
         lcProcess = "SelfService"
         lcWhenSent = "Orders contract ID query from Self Service external API"
         lcSender = "622622622".
      
      when "Voice3000Limit" then assign
         lcProcess = "VoiceBundle"
         lcWhenSent = "After limit is exceeded"
         lcSender = invtext.sender.

      when "WelcomeTARJ6" then assign
         lcProcess = "Order handling"
         lcWhenSent = "After subscription activation"
         lcSender = "622".

      when "WelcomeNewSubs" then assign
         lcProcess = "Order handling"
         lcWhenSent = "After NEW CONTS subscription activation"
         lcSender = "22622".
      when "ActEmailInvoiceReminder" then assign
         lcProcess = "Invoice"
         lcSender = "Fact. Yoigo"
         lcWhenSent = "Email not confirmed in Fusion email sending".
         
      when "AdditionalSIMTermRem1" then assign
         lcProcess = "MultiSIM"
         lcSender = "22622"
         lcWhenSent = "After primary subcription is terminated".
      when "AdditionalSIMTermRem2" then assign
         lcProcess = "MultiSIM"
         lcSender = "22622"
         lcWhenSent = "20 days before additional line termination".
      when "AdditionalSIMTermRem3" then assign
         lcProcess = "MultiSIM"
         lcSender = "22622"
         lcWhenSent = "2 days before additional line termination".
      
      otherwise MESSAGE invtext.keyvalue VIEW-AS ALERT-BOX.
   end.

   if invtext.target ne "sms" then do:
      assign
      lcProcess = "Order handling"
      lcSender = "clientes@yoigo.com"
      lcWhenSent = "Order processing starts in CCBS".
   end.

   if invtext.targe = "sms" and lcDesc EQ "" THEN DO:
      lcDesc = invtext.maintitle.
      if lcDesc ne "" and invtext.txttitle ne "" then lcDesc = lcDesc + " / ".
      lcDesc = lcDesc + invtext.txttitle.
   end.

   IF lcFormat = "luna" then do:

      create ttText.
      assign
         ttText.type = invtext.target
         ttText.token = lcProcess
         ttText.content = "|" +
            lcDesc + " | " +
            lcTranslations[1] +  " | " +
            lcTranslations[5] +  " | " +
            lcTranslations[3] +  " | " +
            lcTranslations[2] +  " | " +
            lcProcess + " | " +
            lcWhenSent + " | "  +
            lcSender + " | " + 
            invtext.keyvalue + "|" +
            invtext.sendrule + " |".
/*
      put unformatted "| "
         lcDesc " | "
         lcTranslations[1] " | "
         lcTranslations[5] " | "
         lcTranslations[3] " | "
         lcTranslations[2] " | "
         lcProcess " | "
         lcWhenSent " | " 
         lcSender " | " skip. */
   end.
   else do: 

      put stream slog unformatted 
         "TYPE......: " invtext.target skip
         "LANGUAGE..: " invtext.lang " " entry(int(invtext.lang), lcLang) skip
         "FROM .....: " invtext.fromdate " - " invtext.todate skip
         "TOKEN ....: " invtext.keyvalue skip
         "PROCESS...: " lcProcess skip
         "WHEN SENT.: " lcWhenSent skip
         "SENDER....: " lcSender skip
         "MAIN_TITLE: " invtext.maintitle skip
         "TEXT_TITLE: " invtext.txttitle skip
         "TEXT......: " lcinvtext skip
         "EXPLAIN ..: " lcExplain skip(5). 
   end.

end.

IF lcFormat = "luna" then 
FOR EACH ttText NO-LOCK break by type desc by token:
   if first-of(type) then
   put stream slog unformatted "h2. " type skip "||Description||Spanish||English||Basque||Catalan||Process||When sent||Sender||SMS_ID||Send Time Rule||" skip.
   put stream slog unformatted ttText.Content skip.
end.

output stream slog close.
