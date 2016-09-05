{Syst/testpaa.i}
katun = "anttis".

{Syst/eventval.i}
{Func/coinv.i}
{Func/fmakemsreq.i}
{Func/msisdn.i}
{Func/timestamp.i}
{Func/ftmrlimit.i}

def var lcline     as char no-undo.
def var litarget   as int  no-undo.
def var lisource   as int  no-undo.
def var liMsSeq    as int  no-undo.
def var ldactstamp as dec  no-undo.
def var lcorgid    as char no-undo.
def var lcmarked   as char no-undo init "inactive".
def var liall      as int  no-undo.
def var limarked   as int  no-undo.
def var limoved    as int  no-undo.
def var lisubs     as int  no-undo.
def var lcmessage  as char no-undo.

def stream slog.
def stream sread.

def buffer btargetcust for customer.
def buffer bsourcecust for customer.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhMobSub AS HANDLE NO-UNDO.
   lhMobSub = BUFFER MobSub:HANDLE.
   RUN StarEventInitialize(lhMobSub).

   DEFINE VARIABLE lhsourcecust AS HANDLE NO-UNDO.
   lhsourcecust = BUFFER bsourcecust:HANDLE.
   RUN StarEventInitialize(lhsourcecust).


END.
function flogerror returns logic
   (icmessage as char):

   put stream slog unformatted
      liMsSeq  chr(9)
      litarget  chr(9)
      lisource  chr(9)
      icmessage skip.
      
end function.

output stream slog to /apps/snet/200911/as_custnum300_problem.log append.
input stream sread from /apps/snet/200911/as_custnum300_problem.input.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
i = 0.
repeat:

   import stream sread unformatted lcline.

   i = i + 1.

   assign 
      liMsSeq  = integer(entry(1,lcline,"|"))
      lisource = integer(entry(2,lcline,"|"))
      litarget = integer(entry(3,lcline,"|"))
      no-error.
      
   if error-status:error or litarget = 0 or lisource = 0 then next.
   
   find first btargetcust where btargetcust.custnum = litarget
   no-lock no-error.
   if not available btargetcust then do:
      flogerror("target not found").
      next.
   end.

   find first bsourcecust where bsourcecust.custnum = lisource
   no-lock no-error.
   if not available bsourcecust then do:
      flogerror("source not found").
      next.
   end.
    
   liall = liall + 1.
   pause 0.
   disp liall limoved limarked with 1 down.
   
   if btargetcust.roles = lcmarked or 
      bsourcecust.roles = lcmarked
   then next.

   lisubs = 0.
   lcmessage = "".
   
   for each mobsub exclusive-lock where
            mobsub.msseq = liMsSeq and
            mobsub.agrcust = bsourcecust.custnum:
   
      if can-find(first msrequest where
                        msrequest.msseq = mobsub.msseq and
                        lookup(string(msrequest.reqstat),"2,4,9") = 0)
      then do:
         flogerror(mobsub.cli + " has unhandled requests").
         next.
      end.

      run pownerchange.
   end.
   
   if lisubs > 0 then assign
      limoved = limoved + 1
      lcmessage = string(lisubs) + " subscriptions moved from " +
                  string(bsourcecust.custnum) + " to " +
                  string(btargetcust.custnum).
   /*    
   if bsourcecust.roles ne lcmarked then do:
      find current bsourcecust exclusive-lock.
      IF llDoEvent THEN RUN StarEventSetOldBuffer(lhsourcecust).
      assign
         bsourcecust.roles = lcmarked
         limarked          = limarked + 1.
      if lcmessage = "" then lcmessage = string(bsourcecust.custnum) + 
                                         " marked as inactive".
      IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhsourcecust).
   end.
   */
   if lcmessage > "" then do:
      flogerror(lcmessage).
   end.

end.   

disp liall limoved limarked.

fCleanEventObjects().


/* agreement customer change for subscription */
PROCEDURE pOwnerChange:

   DEF VAR liNewOwner   AS INT  NO-UNDO.
   DEF VAR liNewInvCust AS INT  NO-UNDO.
   DEF VAR liNewUser    AS INT  NO-UNDO.
   DEF VAR llNewCust    AS LOG  NO-UNDO. 
   DEF VAR liNewTarget  AS INT  NO-UNDO.
   DEF VAR liFeePeriod  AS INT  NO-UNDO. 
   DEF VAR ldtFeeFrom   AS DATE NO-UNDO.
   DEF VAR ldtFeeTo     AS DATE NO-UNDO. 
   DEF VAR ldFeeAmt     AS DEC  NO-UNDO. 
   DEF VAR liPerDays    AS INT  NO-UNDO.
   DEF VAR ldtFeeDate   AS DATE NO-UNDO.
   DEF VAR ldEndStamp   AS DEC  NO-UNDO.

   DEF BUFFER bFixedFee   FOR FixedFee.
   DEF BUFFER bFFItem     FOR FFItem.
   DEF BUFFER bFatime     FOR Fatime.
   DEF BUFFER bBillTarget FOR BillTarget.
   DEF BUFFER bOwner      FOR MSOwner.
   def buffer bcounter    for tmcounter.

   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TSEnd >= 99999999 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq NO-LOCK NO-ERROR.
              
   IF NOT AVAILABLE MsOwner OR MsOwner.CustNum NE MobSub.CustNum OR
      MsOwner.AgrCust NE MobSub.AgrCust OR MsOwner.TSEnd < 20090612 
   THEN DO:
      flogError("Invalid MSOwner data").
      RETURN.
   END. 
 
   fSplitTS(MobSub.activationts,
            OUTPUT ldtActDate,
            OUTPUT liActTime).
 
   ASSIGN liNewOwner   = btargetcust.custnum
          liNewUser    = btargetcust.custnum
          liNewInvCust = btargetcust.custnum.

   /* user has been changed -> mark new user nbr to related tables */
   IF MobSub.CustNum NE liNewUser AND liNewUser > 0 THEN DO:
      
      /* make sure that new user has a similar billing target */
      FIND bBillTarget WHERE
           bBillTarget.CustNum    = MobSub.CustNum AND
           bBillTarget.BillTarget = MobSub.BillTarget NO-LOCK NO-ERROR.
         
      FIND BillTarget WHERE
           BillTarget.CustNum    = liNewUser AND
           BillTarget.BillTarget = MobSub.BillTarget NO-LOCK NO-ERROR.
   
      liNewTarget = 0.

      IF AVAILABLE BillTarget AND AVAILABLE bBillTarget THEN DO: 

         IF BillTarget.RatePlan NE bBillTarget.RatePlan OR
            BillTarget.DiscPlan NE bBillTarget.DiscPlan
         THEN DO liReqCnt = 30 TO 99:
            IF NOT CAN-FIND(BillTarget WHERE
                            BillTarget.CustNum    = liNewUser AND
                            BillTarget.BillTarget = liReqCnt)
            THEN DO:
               liNewTarget = liReqCnt.
              LEAVE.
            END.
         END.   
      END.

      IF NOT AVAILABLE BillTarget OR liNewTarget > 0 THEN DO:
      
         CREATE BillTarget.
      
         IF AVAILABLE bBillTarget THEN DO:
            BUFFER-COPY bBillTarget EXCEPT CustNum TO BillTarget.
            IF liNewTarget > 0 THEN BillTarget.BillTarget = liNewTarget.
         END.

         ELSE DO:
        
            FIND CLIType WHERE 
                 CLIType.Brand   = gcBrand AND
                 CLIType.CLIType = MobSub.CLIType NO-LOCK NO-ERROR.
            IF AVAILABLE CLIType THEN ASSIGN 
               BillTarget.BillTarget = CLIType.BillTarget
               BillTarget.RatePlan   = CLIType.PricePlan
               BillTarget.DiscPlan   = CLIType.DiscPlan.
            ELSE ASSIGN 
               BillTarget.BillTarget = 1.
         END.
      
         BillTarget.CustNum = liNewUser.
      END.

      /* change period */
      liFeePeriod = YEAR(ldtActDate) * 100 + MONTH(ldtActDate).
    
      FOR EACH FATime EXCLUSIVE-LOCK USE-INDEX MobSub WHERE
               FATime.Brand  = gcBrand      AND
               FATime.MsSeq  = MobSub.MsSeq AND
               FATime.InvNum = 0            AND
               FATime.Period >= liFeePeriod:
      
         IF FATime.CustNum = liNewUser THEN NEXT.
         
         /* transfer newer fatimes totally */

         /* memo */
         fWriteMemo("FATime",
                    STRING(Fatime.FatNum),
                    liNewUser,
                    "User Change",
                    "Transferred from customer " + STRING(Fatime.CustNum)).

         FATime.CustNum = liNewUser. 
      END.
  /* 
      FIND FIRST MSISDN NO-LOCK WHERE 
                 MSISDN.Brand = gcBrand AND
                 MSISDN.CLI = MobSub.CLI NO-ERROR.
      IF AVAILABLE MSISDN THEN DO:
         fLocalMakeMsidnHistory(RECID(MSISDN)).
      END.
      FIND CURRENT MSISDN EXCLUSIVE-LOCK.
      ASSIGN MsIsdn.CustNum = liNewUser.
    */  
      /* IMSI */
      FIND FIRST IMSI EXCLUSIVE-LOCK WHERE
                 IMSI.ICC = MobSub.ICC NO-ERROR.
      IF AVAILABLE IMSI THEN IMSI.CustNum = liNewUser.
             
      /* SIM */
      FIND FIRST SIM EXCLUSIVE-LOCK WHERE
                 SIM.Brand = gcBrand   AND
                 SIM.ICC   = MobSub.ICC NO-ERROR.
      IF AVAILABLE SIM THEN SIM.CustNum = liNewUser.
   END. 

   /* invoicing customer is changed -> move fees */
   IF MobSub.InvCust NE liNewInvCust AND liNewInvCust > 0 THEN DO:

      ldtFeeDate = ldtActDate.
  
      IF DAY(ldtFeeDate) > 1 THEN DO:
         IF MONTH(ldtFeeDate) = 12
         THEN ldtFeeDate = DATE(12,31,YEAR(ldtFeeDate)).
         ELSE ldtFeeDate = DATE(MONTH(ldtFeeDate) + 1,1,YEAR(ldtFeeDate)) - 1. 
      END.

      liFeePeriod = YEAR(ldtFeeDate) * 10000 + 
                    MONTH(ldtFeeDate) * 100  + 
                    DAY(ldtFeeDate).
                    
      FOR EACH FixedFee EXCLUSIVE-LOCK WHERE
               FixedFee.Brand     = gcBrand              AND
               FixedFee.HostTable = "MobSub"             AND 
               FixedFee.KeyValue  = STRING(MobSub.MsSeq) AND
               FixedFee.InUse     = TRUE AND
               FixedFee.Custnum   =  lisource:


         FixedFee.Custnum = liNewInvCust.

         FOR EACH FFItem OF FixedFee EXCLUSIVE-LOCK: 
             ASSIGN 
                FFItem.CustNum = FixedFee.CustNum.
         END. 
         
         /* memo */
         fWriteMemo("FixedFee",
                    STRING(FixedFee.FFNum),
                    FixedFee.CustNum,
                    "User Change",
                    "Transferred from customer " + STRING(liSource)).
          
      END. 

      liFeePeriod = TRUNCATE(liFeePeriod / 100,0).
      
      FOR EACH SingleFee EXCLUSIVE-LOCK WHERE
               SingleFee.Brand      = gcBrand              AND
               SingleFee.HostTable  = "MobSub"             AND 
               SingleFee.KeyValue   = STRING(MobSub.MsSeq) AND
               SingleFee.Active     = TRUE                 AND
               SingleFee.Billed     = FALSE                AND
               SingleFee.BillPeriod >= liFeePeriod:
               
         /* memo */
         fWriteMemo("SingleFee",
                    STRING(SingleFee.FMItemId),
                    liNewInvCust,
                    "User Change",
                    "Transferred from customer " + STRING(SingleFee.CustNum)).
                    
         SingleFee.CustNum = liNewInvCust.
      END. 
      
   END.
   
   /* end current msowner and create a new one */
   /*
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TSEnd >= 99999999
   EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq EXCLUSIVE-LOCK.
   
   ASSIGN 
      MsOwner.CustNum = liNewUser    WHEN liNewUser > 0
      MsOwner.AgrCust = liNewOwner 
      MsOwner.InvCust = liNewInvCust WHEN liNewInvCust > 0.
   */

   FOR EACH msowner where
      msowner.msseq = mobsub.msseq and
      msowner.custnum = liSource EXCLUSIVE-LOCK:
      ASSIGN 
         MsOwner.CustNum = liNewUser    WHEN liNewUser > 0
         MsOwner.AgrCust = liNewOwner 
         MsOwner.InvCust = liNewInvCust WHEN liNewInvCust > 0.
   END.


 /*  
   ASSIGN ldEndStamp    = MsOwner.TSEnd
          MsOwner.TSEnd = ldActStamp - 0.00001.
          
   CREATE bOwner.
   BUFFER-COPY MsOwner EXCEPT CustNum TSBeg TSEnd TO bOwner.
   ASSIGN bOwner.CustNum = liNewUser    WHEN liNewUser > 0
          bOwner.TSBeg   = ldActStamp
          bOwner.TSEnd   = ldEndStamp
          bOwner.InvCust = liNewInvCust WHEN liNewInvCust > 0
          bOwner.AgrCust = liNewOwner.
*/
/*   RELEASE MsOwner.*/
/*   RELEASE bOwner.     */
     
   for each tmcounter exclusive-lock where
            tmcounter.msseq = mobsub.msseq and
            tmcounter.custnum = mobsub.custnum and
            tmcounter.todate > ldtactdate:
      find first bcounter where
                 bcounter.msseq = mobsub.msseq and
                 bcounter.tmruleseq = tmcounter.tmruleseq and
                 bcounter.todate = tmcounter.todate and
                 bcounter.custnum = linewuser no-error.
      if not available bcounter then do:
         tmcounter.custnum = liNewUser.      
         next.
      end.
      
      else do:
         assign bcounter.amount = bcounter.amount + tmcounter.amount.
         if tmcounter.limitid > bcounter.limitid then 
            bcounter.limitid = tmcounter.limitid.
            
         delete tmcounter.
      end.
   end.
   
   /* limits? no */
   /* custbal? no */
   
   /* new user to mobsub */

   FOR EACH msrequest where
            msrequest.msseq = mobsub.msseq and
            lookup(string(msrequest.reqstat),"2,4,9") > 0 and
            msrequest.custnum = liSource EXCLUSIVE-LOCK:
      msrequest.custnum = liNewUser.
   END.

   FIND CURRENT MobSub EXCLUSIVE-LOCK.

   IF llDoEvent THEN RUN StarEventSetOldBuffer(lhMobSub).
   ASSIGN MobSub.CustNum = liNewUser    WHEN liNewUser > 0
          MobSub.InvCust = liNewInvCust WHEN liNewInvCust > 0
          MobSub.AgrCust = liNewOwner
          lisubs         = lisubs + 1.
   IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub).

   fWriteMemo("MobSub",
              STRING(MobSub.MsSeq),
              liNewInvCust,
              "Owner Change",
              "Transferred from customer " + STRING(bsourcecust.CustNum)).
 
   IF MobSub.PayType THEN RUN pPrepaidRate(ldtactdate,mobsub.cli).
   ELSE RUN pReRate(ldtactdate,mobsub.cli).
   
   RELEASE MobSub.
   
END PROCEDURE.


PROCEDURE pReRate:

   DEFINE INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER icCLI    AS CHAR NO-UNDO.
 
   RUN Rate/cli_rate.p (icCLI,
                 idtFrom,
                 11/30/9,
                 TRUE).    
 
END PROCEDURE.


PROCEDURE pPrepaidRate:
   
   DEFINE INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER icCLI    AS CHAR NO-UNDO.

   RUN Rate/cli_prepaidrate.p (icCLI,      
                        idtFrom, 
                        11/30/9,     
                        TRUE).      /* silent = true */  
END PROCEDURE.

 
