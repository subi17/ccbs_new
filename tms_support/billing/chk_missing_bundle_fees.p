{date.i}
{commpaa.i}
katun = "anttis".
gcBrand = "1".

DEFINE VARIABLE ldeNow AS DECIMAL NO-UNDO. 
ldeNow = fmakets().
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcBillCode AS CHARACTER NO-UNDO. 
def buffer stcrequest for msrequest. 

def stream sout.
output stream sout to /apps/yoigo/tms_support/billing/chk_missing_bundle_fees.log append.

DEFINE VARIABLE ldaDate AS DATE NO-UNDO. 
DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcReqChar AS CHARACTER NO-UNDO. 

def buffer bMServicelimit for mservicelimit.
/*
put stream sout unformatted 
   "CONTRACT|CONTRACT_BEGIN_TS|RENEWAL_TS|STC_ACT_TS|STC_DONE_TS|ORDERCHANNEL|OLD_TYPE|NEW_TYPE|MSSEQ|MSISDN" skip. */
looppi:
FOR EACH servicelimit where
   lookup(servicelimit.groupcode,"CONTD2,CONTDATA,MDUB,MDUB2") > 0 NO-LOCK:

   find daycampaign where
      daycampaign.dcevent = servicelimit.groupcode NO-LOCK.

   FOR EACH mservicelimit where
            mservicelimit.slseq = servicelimit.slseq and
            mservicelimit.dialtype = servicelimit.dialtype and
            mservicelimit.endts > ldeNow NO-LOCK:
      
      find first mobsub where
                 mobsub.msseq = mservicelimit.msseq NO-LOCK no-error.
      IF NOT AVAIL mobsub then next.
   
      case servicelimit.groupcode:
         when "contd2" then lcBillCode = "CONTD2MF".
         when "contdata" then lcBillCode = "CONTDATAMF".
         when "mdub2" then lcBillCode = "MDUB2MF".
         when "mdub" then do:
            if mobsub.clitype = "contrd3" then lcBillCode = "CONTD3MF".
            else lcBillCode = "MDUBMF".
         end.
      end.

      fSplitTs(mservicelimit.fromts,output ldaDate, output liTime).
      liPeriod = year(ldaDate) * 100 + month(ldaDate).

      i = i + 1.
      status default string(i).

      FIND FIRST fixedfee where
            fixedfee.brand = "1" and
            hosttable = "mobsub" and
            keyvalue = string(mservicelimit.msseq) and
            fixedfee.billcode = lcBillCode and
            begperiod >= liPeriod NO-LOCK no-error.

      IF NOT AVAIL fixedfee then do:
         find first msrequest where
                    msrequest.msseq = mservicelimit.msseq and
                    msrequest.reqtype = 46 NO-LOCK use-index msactstamp no-error.
         if not avail msrequest then next.
         find first stcrequest where
                    stcrequest.msseq = mservicelimit.msseq and
                    stcrequest.reqtype = 0 and
                    stcrequest.reqstatus = 2 NO-LOCK use-index msactstamp no-error.
         if not avail stcrequest then next.
         find order where
              order.brand = "1" and
              order.orderid = msrequest.reqiparam1 NO-LOCK no-error.
         if not avail order then next.
         FIND FIRST fixedfee where
               fixedfee.brand = "1" and
               hosttable = "mobsub" and
               keyvalue = string(mservicelimit.msseq) and
               fixedfee.billcode = lcBillCode NO-LOCK no-error.

      put stream sout unformatted
               servicelimit.groupcode "|"
               fts2hms(mservicelimit.fromts) "|"
               fts2hms(msrequest.actstamp) "|"
               fts2hms(stcrequest.actstamp) "|"
               fts2hms(stcrequest.donestamp) "|"
               order.orderchannel "|"
               stcrequest.reqcparam1 "|"
               stcrequest.reqcparam2 "|"
               mservicelimit.msseq "|"
               mobsub.cli "|"
               avail fixedfee skip.
      end.
   end.
end.
disp i.
