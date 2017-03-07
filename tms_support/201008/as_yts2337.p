{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/timestamp.i}
{Func/fmakemsreq.i}

DEFINE STREAM sout.
OUTPUT STREAM sout TO "as_yts2337.log" append.
def stream sffitem.
output stream sffitem to as_yts2337_ffitem.d append.
DEFINE VARIABLE lcSep AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liRequest AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO. 

lcSep = "|". 

def buffer bmdubact for mservicelimit.
def buffer bmdub for mservicelimit.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
/*   
put stream sout unformatted
   "mobsub.msseq" "|" 
   "mobsub.cli" "|" 
   "bmdubact.fromts" "|" 
   "bmdubact.endts" "|"
   "bmdub.fromts" "|"
   "servicelcounter.slseq" "|"
   "fixedfee.begperiod" "|" 
   "fixedfee.begdate" "|" 
   "ffitem.billed" "|" 
   "mdubact_termrequest" skip. 
*/
MAIN_LOOP:
FOR EACH ServiceLimit NO-LOCK WHERE
         ServiceLimit.SLCode = "MDUB",
    EACH MServiceLimit NO-LOCK WHERE
         MServiceLimit.SLSeq = ServiceLimit.SLSeq and
         mservicelimit.fromts = 20100801.00000,
    EACH MobSub NO-LOCK WHERE
         MobSub.MsSeq = MServiceLimit.MsSeq AND
         MobSub.ActivationTS >= MServiceLimit.FromTS :

   i = i + 1.

   do transaction:
   /* 1) Move MDUBACT contract from July  to August */

   find bmdubact where
        bmdubact.slseq = 11 and
        bmdubact.msseq = MobSub.msseq and
        bmdubact.endts = 20100731.86399 EXCLUSIVE-LOCK.
  
   put stream sout unformatted
      mobsub.msseq "|" 
      mobsub.cli "|" 
      bmdubact.fromts "|" 
      bmdubact.endts "|".
  
   assign
      bmdubact.fromts = MobSub.ActivationTS
      bmdubact.endts = 20100831.86399.

   /* 2) Move MDUB contract to start from 1.9 instead of 1.8  */
   find bmdub where
      rowid(bmdub) = rowid(mservicelimit) EXCLUSIVE-LOCK.
   
   put stream sout unformatted
      bmdub.fromts "|".

   assign
      bmdub.fromts = 20100901.00000.

   /* 3) Change possible august MDUB traffic counter type (SLSeq)
         from MDUB to MDUBACT  */
   FIND ServiceLCounter WHERE
        ServiceLCounter.Msseq   = MServiceLimit.MsSeq  AND
        ServiceLCounter.SLseq   = ServiceLimit.Slseq   AND
        ServiceLCounter.Period  = 201008 EXCLUSIVE-LOCK NO-ERROR. 

   IF AVAIL ServiceLCounter then do:
      put stream sout unformatted
         servicelcounter.slseq "|". 
      
      assign
         servicelcounter.slseq = 11.
      
      release servicelcounter.
   end.
   else put stream sout unformatted "N/A|". 
   

   /* 4) Change MDUB Fixed Fee "Contract date" and "Begin period"
         from August to September  */
   find first fixedfee EXCLUSIVE-LOCK where
      fixedfee.brand = "1"  and
      fixedfee.custnum = MobSub.custnum and
      fixedfee.hosttable = "mobsub" and
      fixedfee.keyvalue = string(mobsub.msseq) and
      fixedfee.begperiod = 201008 and
      fixedfee.begdate  = 8/1/2010 and
      fixedfee.billcode = "mdubmf".
      
   put stream sout unformatted
      fixedfee.begperiod "|" 
      fixedfee.begdate "|". 
   
   /* 3) Delete august MDUB Fixed Fee Item */
   
   find ffitem of fixedfee where
      ffitem.billperiod = 201008 EXCLUSIVE-LOCK no-error.
   put stream sout unformatted ffitem.billed "|". 

   export stream sffitem ffitem.
   
   delete ffitem.
   
   assign
      fixedfee.begperiod = 201009
      fixedfee.begdate = 9/1/2010.
   
   release fixedfee.

   /* 5) If there is an ongoing MDUB termination request, create termination request also for MDUBACT */

   find first msrequest where
      msrequest.msseq = MobSub.msseq and
      msrequest.reqtype = 9 and
      msrequest.reqcparam2 = "term" and
      msrequest.reqcparam3 = "mdub" and
      msrequest.reqstatus = 0 NO-LOCK no-error.

   if avail msrequest then do:
      
      liRequest = fPCActionRequest(MobSub.MsSeq,
                                   "MDUBACT",
                                   "term",
                                   20100831.86399,
                                   TRUE, /* create fee if is defined*/
                                   "5",
                                   "",
                                   OUTPUT lcError). 
      put stream sout unformatted
         liRequest " " lcError. 
   end.
   else put stream sout unformatted
         "N/A". 
   
   put stream sout unformatted skip. 
   release bmdubact.
   release bmdub.

   if i >= 5000 then
   LEAVE MAIN_LOOP.

   end. /*trans*/


END.

disp i.

OUTPUT STREAM sout CLOSE.

