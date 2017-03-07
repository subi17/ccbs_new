{Func/date.i}
{Func/timestamp.i}

def var ldastcdate as date no-undo.
def var ldactstamp as dec no-undo.
def var ldnextday as dec no-undo.
def var ldprevfrom as dec no-undo.
def var ldprevto as dec no-undo.
def var ldprevlast as dec no-undo.
DEFINE VARIABLE lcOutputFolder AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcOutputfile AS CHARACTER NO-UNDO. 
def var i as int no-undo.
def var j as int no-undo.

DEFINE VARIABLE STC_DATE AS DATE NO-UNDO. 
STC_DATE = fLastDayOfMonth(TODAY) + 1.
lcOutputFolder = "/apps/yoigo/tms_support/billing/".
form
   STC_DATE format "99-99-9999" label  "BTC/STC DATE.." skip
   lcOutputFolder format "x(50)" label "OUTPUT FOLDER." skip
   i label                             "REQ. ANALYSED." skip
   j label                             "CASES FOUND..." skip
   lcOutputfile format "x(60)" label   "RESULT FILE..." skip
WITH  OVERLAY ROW 8 centered
TITLE "This program will make a report of all possible STC/BTC cases" side-LABELS
FRAME a.

UPDATE STC_DATE lcOutputFolder WITH FRAME a.
IF STC_DATE EQ ? THEN QUIT.

IF lcOutputFolder > "" AND 
   SUBSTRING(lcOutputFolder,
             LENGTH(lcOutputFolder),
             1) NE "/" then lcOutputFolder = lcOutputFolder + "/".

lcOutputfile = 
   "find_stc_cases_" + 
   STRING(year(STC_DATE) * 10000 + MONTH(STC_DATE) * 100 + DAY(STC_DATE))
   + ".txt".

disp lcOutputFile with frame a.
lcOutputFile = lcOutputFolder + lcOutputFile.

assign 
   ldastcdate = STC_DATE 
   ldactstamp = fmake2dt(ldastcdate,0)
   ldnextday  = fmake2dt(ldastcdate + 1,0)
   ldprevto   = fmake2dt(ldastcdate - 1,86399)
   ldprevlast = fmake2dt(ldastcdate - 1,0)
   ldprevfrom = if month(ldastcdate) = 1
                then fmake2dt(date(12,1,year(ldastcdate) - 1),0)
                else fmake2dt(date(month(ldastcdate) - 1,1,
                              year(ldastcdate)),0).
                


def stream slog.
output stream slog to value(lcOutputfile).

put stream slog unformatted
   "MSISDN"    chr(9)
   "Subscr.ID" chr(9)
   "Req.Type"  chr(9)
   "Handled"   chr(9)
   "From"      chr(9)
   "To"        chr(9)
   "Package"   chr(9)
   "P.First.Month" chr(9)
   "P.Last.Month" chr(9)
   "BTC/STC.Exists" skip.

def var lcbundle as char no-undo.
def var lllastmonth as log no-undo.
def var llfirstmonth as log no-undo.

def temp-table ttstc no-undo
   field reqtype as int
   field ctfrom as char
   field ctto as char
   field bundle as char
   field firstmonth as log
   field lastmonth as log 
   field msisdn1 as char
   field msseq1 as int
   field order1 as int
   field act1 as dec
   field msisdn2 as char
   field msseq2 as int
   field order2 as int
   field act2 as dec
   field other as log
   index ctfrom ctfrom ctto bundle.

def buffer bcontr for msrequest.

DEFINE VARIABLE liReqType AS INTEGER NO-UNDO. 
DEFINE VARIABLE liReqStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE llAvailOther AS LOGICAL NO-UNDO. 

def buffer bmsrequest for msrequest.

do liReqType = 0 to 81 by 81:
   
   if liReqType eq 0 then liReqStatus = 8.
   else liReqStatus = 0.

   for each msrequest no-lock where
            brand = "1" and
            reqtype = liReqType and
            reqstat = liReqStatus and
            actstamp >= ldactstamp and
            actstamp < ldnextday
   by msrequest.actstamp
   by msrequest.msrequest:

      i = i + 1.
      
      assign 
         lcbundle = ""
         llfirstmonth = false
         lllastmonth = false.
         
      for each mservicelimit no-lock where
               mservicelimit.msseq = msrequest.msseq and
               mservicelimit.endts > ldprevfrom and
               mservicelimit.fromts < ldprevto,
         first servicelimit no-lock where
               servicelimit.slseq = mservicelimit.slseq,
         first daycampaign no-lock use-index dcevent where
               daycampaign.brand = "1" and
               daycampaign.dcevent = servicelimit.groupcode and
               daycampaign.dctype = "4":
         
         lcbundle = daycampaign.dcevent.
         
         for first bcontr no-lock where
                   bcontr.msseq = msrequest.msseq and
                   bcontr.reqtype = 9 and
                   bcontr.actstamp >= ldprevlast and
                   bcontr.reqcparam3 = daycampaign.dcevent:
            lllastmonth = true.
         end.          
                   
         if index(daycampaign.dcevent,"act") > 0 then llfirstmonth = true.
         else for first bcontr no-lock where 
                   bcontr.msseq = msrequest.msseq and
                   bcontr.reqtype = 8 and
                   bcontr.actstamp >= ldprevfrom and
                   bcontr.actstamp <= ldprevto and
                   bcontr.reqcparam3 = daycampaign.dcevent:
            llfirstmonth = true.
         end.          
         
         leave.
      end.
      
      release bmsrequest.
      if msrequest.reqtype = 0 then do:
         FIND FIRST bmsrequest where
                    bmsrequest.msseq = msrequest.msseq and
                    bmsrequest.reqtype = 81 and
                   lookup(string(bmsrequest.reqstatus),"2,4,9") = 0
         NO-LOCK no-error.
      end.
      else do:
         FIND FIRST bmsrequest where
                    bmsrequest.msseq = msrequest.msseq and
                    bmsrequest.reqtype = 0 and
                   lookup(string(bmsrequest.reqstatus),"2,4,9") = 0
         NO-LOCK no-error.
      end.
      llAvailOther = AVAIL(bmsrequest).

      find first ttstc where
                 ttstc.ctfrom = msrequest.reqcparam1 and
                 ttstc.ctto   = msrequest.reqcparam2 and
                 ttstc.bundle = lcbundle and
                 ttstc.firstmonth = llfirstmonth and
                 ttstc.lastmonth = lllastmonth and
                 ttstc.other = llAvailOther no-error.

      if not available ttstc then do:
         create ttstc.
         assign 
            ttstc.ctfrom = msrequest.reqcparam1
            ttstc.ctto   = msrequest.reqcparam2
            ttstc.bundle = lcbundle
            ttstc.other = llAvailOther
            ttstc.reqtype = msrequest.reqtype
            ttstc.firstmonth = llfirstmonth
            ttstc.lastmonth = lllastmonth.
      end.

      if ttstc.msisdn1 = "" then assign
         ttstc.msisdn1 = msrequest.cli
         ttstc.msseq1 = msrequest.msseq
         ttstc.order1 = i
         ttstc.act1 = msrequest.actstamp
         j = j + 1.
      else if ttstc.msisdn2 = "" then assign
         ttstc.msisdn2 = msrequest.cli
         ttstc.msseq2 = msrequest.msseq
         ttstc.order2 = i
         ttstc.act2 = msrequest.actstamp
         j = j + 1.

      pause 0.
      disp i j with with frame a.
   end.
end.

for each ttstc:
   put stream slog unformatted
      ttstc.msisdn1 chr(9)
      ttstc.msseq1 chr(9)
      ttstc.reqtype chr(9)
      ttstc.order1 chr(9)
      ttstc.ctfrom chr(9)
      ttstc.ctto   chr(9)
      ttstc.bundle chr(9)
      ttstc.firstmonth chr(9)
      ttstc.lastmonth  chr(9)
      ttstc.other  skip.

   if ttstc.msisdn2 > "" then 
   put stream slog unformatted
      ttstc.msisdn2 chr(9)
      ttstc.msseq2 chr(9)
      ttstc.reqtype chr(9)
      ttstc.order2 chr(9)
      ttstc.ctfrom chr(9)
      ttstc.ctto   chr(9)
      ttstc.bundle chr(9)
      ttstc.firstmonth chr(9)
      ttstc.lastmonth  chr(9)
      ttstc.other  skip.
end.
      
output stream slog close.

MESSAGE lcOutputfile VIEW-AS ALERT-BOX TITLE "REPORT CREATED".

