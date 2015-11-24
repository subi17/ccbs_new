/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_sender.p
  task .........: Send collected Q25 SMS mesages
  application ..: tms
  author .......: kaaikas
  created ......: 20.11.15
  version ......: yoigo
---------------------------------------------------------------------- */
/* Sending SMS messages. To be triggered by cron for each hour between
   10:00 and 21:00. All messages should be sent before 22:00. HPD is capable
   to send 2-5 messages per second. (5 if length is less than 160 characters) 
   */ 

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{q25functions.i}

/* send collected Quota 25 messages to customers */
fSendQ25SMSMessages().

