/* ----------------------------------------------------------------------
  module .......: Mm/q25_reminder_sender.p
  task .........: Send collected Q25 SMS mesages
  application ..: tms
  author .......: kaaikas
  created ......: 20.11.15
  version ......: yoigo
---------------------------------------------------------------------- */

{commpaa.i}
ASSIGN gcBrand = "1"
       katun   = "CRON".
{q25functions.i}

/* send collected Quota 25 messages to customers */
fSendQ25SMSMessages().

