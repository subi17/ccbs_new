def stream sout.

def var i as int no-undo.
def var j as int no-undo.
DEFINE VARIABLE liPeriod AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPeriodFrom AS INTEGER NO-UNDO. 
DEFINE VARIABLE liPeriodTo AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldaPeriod AS DATE NO-UNDO. 
DEFINE VARIABLE ldaTodate AS DATE NO-UNDO. 
DEFINE VARIABLE liSubs AS INTEGER NO-UNDO. 
DEFINE VARIABLE liSubsChecked AS INTEGER NO-UNDO. 

DEFINE VARIABLE lcFile AS CHARACTER NO-UNDO. 
DEFINE VARIABL lcFolder AS CHARACTER NO-UNDO. 

lcfolder = "/apps/yoigo/tms_support/billing/log/".

def frame fNegativeIRC 
   ldaPeriod FORMAT "99-99-9999" AT 1 LABEL "Period (any day)" SKIP
   lcFolder  FORMAT "X(60)" LABEL "Output folder" SKIP
   lcFile  FORMAT "X(60)" LABEL "Output file" SKIP(1)
   liSubsChecked  LABEL "Subs checked" SKIP
   i label "IRCs checked"  skip
   j label "Errors found" 
with overlay NO-labels 1 column row 4 title " Negative Invoice Row Counter Check ".

ldaPeriod = date(month(today),1,year(today)).

update ldaPeriod with frame fNegativeIRC.
if ldaPeriod eq ? then return.

ASSIGN
   liPeriod = YEAR(ldaPeriod) * 100 + MONTH(ldaPeriod)
   liPeriodFrom = liPeriod * 100 + 1
   ldaTodate = add-interval(ldaPeriod,1,"months")
   liPeriodTo = YEAR(ldaTodate) * 10000 + MONTH(ldaTodate) * 100 + 1
   ldaTodate = DATE(MONTH(ldaTodate),1,YEAR(ldaTodate)) - 1
   lcFile = "check_negative_irc_" +  STRING(liPeriod) + ".txt".

disp lcFile with frame fNegativeIRC.

update lcFolder lcFile with frame fNegativeIRC.
if lcfolder eq "" or lcfolder eq ? then return.
if lcFile eq "" or lcFile eq ? then return.

DEFINE TEMP-TABLE ttMsSeq NO-UNDO
FIELD msseq AS INT
INDEX msseq IS PRIMARY UNIQUE msseq. 

output stream sout to value(lcfolder + "/" + lcFile).

for each msowner NO-LOCK where
         msowner.tsend >= liPeriodFrom and
         msowner.tsbegin < liPeriodTo and
         msowner.paytype = false :
   
   FIND FIRST ttMsSeq NO-LOCK where
              ttMsSeq.msseq = msowner.msseq no-error.
   IF AVAIL ttMsSeq then next.
      
   CREATE ttMsSeq.
   ASSIGN
      ttMsSeq.msseq = msowner.msseq.

   liSubsChecked = liSubsChecked + 1.

   for each invrowcounter where
         invrowcounter.msseq = ttMsSeq.msseq and
         invrowcounter.todate = ldaTodate no-lock:

      i = i + 1.

      if invrowcounter.amount < 0 or
         invrowcounter.quantity < 0 then do:
         put stream sout unformatted invrowcounter.msseq "|"
                         invrowcounter.cli "|"
                         invrowcounter.billcode "|"
                         invrowcounter.ccn "|"
                         invrowcounter.invcust skip.
         j = j + 1.
      end.

   end.
      
   if liSubsChecked mod 100 eq 0 THEN DO:
      disp liSubsChecked i j with frame fNegativeIRC.
      pause 0.
   END.

end.

disp liSubsChecked i j with frame fNegativeIRC.

