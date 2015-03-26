DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcGsmBnrs AS CHARACTER NO-UNDO. 
lcGsmBnrs = "622,622FF,622622622,633,633633633,633800800,1705,1706,1707,1708,1709".
DEFINE VARIABLE lcGsmBnr AS CHARACTER NO-UNDO. 


DEFINE VARIABLE ldaDate AS DATE NO-UNDO init 7/1/2011 .

def stream sout.
output stream sout to /apps/yoigo/tms_support/billing/yts_2929.txt append.
def stream sout2.
output stream sout2 to /apps/yoigo/tms_support/billing/yts_2929_2.txt append.

put stream sout unformatted 
   "Billing period|a-number|b-number|Price of call|Billed"
skip.

def buffer bMobCDR for MobCDR.

DEFINE VARIABLE liBilled AS INTEGER NO-UNDO. 
DEFINE VARIABLE liUnBilled AS INTEGER NO-UNDO. 
etime(true).
DO WHILE ldaDate <= 7/31/2011: 

   FOR EACH mobcdr where
            mobcdr.readdate = ldaDate and
     lookup(mobcdr.gsmbnr,lcGsmBnrs) > 0 and
            mobcdr.rateccn = 3 and
            mobcdr.errorcode = 0 NO-LOCK
      use-index readdate:
      
      if mobcdr.amount <= 0 then next.
      
      i = i + 1.
      disp i ldaDate mobcdr.GsmBnr format "x(10)" liBilled liUnBilled.
      pause 0.

      find invseq where
           invseq.invseq = mobcdr.invseq NO-LOCK.

      if not invseq.billed then do trans:
         find bMobCDR WHERE
              ROWID(bMobCDR) = ROWID(MobCDR) EXCLUSIVE-LOCK.
         put stream sout2 unformatted
            invseq.fromdate "-" invseq.todate "|"
            bmobcdr.cli "|" bmobcdr.gsmbnr "|"
            bmobcdr.amount "|" invseq.billed "|" invseq.invseq "|"
            recid(mobcdr) skip.
         assign 
            bMobCDR.invseq = 0. 
            bMobCDR.errorcode = 8040. 
         release bMobCDR.  
         liUnBilled = liUnBilled + 1. 
      end.
      else liBilled = liBilled + 1.

      put stream sout unformatted
         invseq.fromdate "-" invseq.todate "|"
         mobcdr.cli "|" mobcdr.gsmbnr "|"
         mobcdr.amount "|" invseq.billed skip.
   end.

   ldaDate = ldaDate + 1.
end.
MESSAGE "DONE" etime VIEW-AS ALERT-BOX.
