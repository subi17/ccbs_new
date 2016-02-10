{Syst/testpaa.i}
katun = "ari".

{Syst/eventval.i}
{Func/coinv.i}
{Func/fmakemsreq.i}
{Func/msisdn.i}
{Func/timestamp.i}
{Func/ftmrlimit.i}

def var lcline     as char no-undo.
def var litarget   as int  no-undo.
def var lisource   as int  no-undo.
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

FUNCTION fLocalMakeMsidnHistory RETURNS CHAR
   (INPUT   irRecID AS RECID).
   
   DEF VAR ldNewFrom  AS DEC  NO-UNDO.
   DEF VAR ldtNewDate AS DATE NO-UNDO.
   DEF VAR liNewTime  AS INT  NO-UNDO.
   
   FIND FIRST HistMSISDN WHERE 
              RECID(HistMSISDN) = irREcID EXCLUSIVE-LOCK.
              
   ASSIGN HistMSISDN.ValidTo = ldactstamp.

   ldNewFrom = HistMSISDN.ValidTo.
   
   /* make sure that there is atleast 1 second gap between rows */
   REPEAT:
      IF NOT CAN-FIND(MSISDN WHERE
                      MSISDN.Brand     = HistMSISDN.Brand AND
                      MSISDN.CLI       = HistMSISDN.CLI   AND
                      MSISDN.ValidFrom = ldNewFrom)
      THEN LEAVE.

      fSplitTS(ldNewFrom,
               OUTPUT ldtNewDate,
               OUTPUT liNewTime).
      IF liNewTime >= 86400 THEN ASSIGN
         ldtNewDate = ldtNewDate + 1
         liNewTime  = 1.
      ELSE liNewTime = liNewTime + 1.
      
      ldNewFrom = fMake2Dt(ldtNewDate,liNewTime).
   END.
   
   CREATE MSISDN.
   BUFFEr-COPY HistMSISDN EXCEPT ValidFrom validTo TO MSISDN.
   ASSIGN
      MSISDN.ValidFrom  = ldNewFrom
      MSISDN.ValidTo    = 99999999.99999
      MSISDN.ActionDate = 1/1/9.
END.    
 
function flogerror returns logic
   (icmessage as char):

   put stream slog unformatted
      lcorgid   chr(9)
      litarget  chr(9)
      lisource  chr(9)
      icmessage skip.
      
end function.


output stream slog to /apps/snet/200901/double_custid_move.log append.

input stream sread from 
   /apps/snet/200901/double_custid_qvantel_remains-to_process.txt.

ldactstamp = 20090101.00001. 


repeat:

   import stream sread unformatted lcline.

   assign 
      lcorgid  = entry(3,lcline,chr(9))
      litarget = integer(entry(1,lcline,chr(9)))
      lisource = integer(entry(7,lcline,chr(9)))
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

   /*  
   message "target" btargetcust.custnum btargetcust.custname
           "source" bsourcecust.custnum bsourcecust.custname
           view-as alert-box.
   */
   /*   
   if can-find(first mobsub where 
                     mobsub.brand = "1" and
                     mobsub.agrcust = bsourcecust.custnum)
   then next.
   if can-find(first mobsub where 
                     mobsub.brand = "1" and
                     mobsub.agrcust = btargetcust.custnum)
   then next.
   */
    
   lisubs = 0.
   lcmessage = "".
   
   for each mobsub exclusive-lock where
            mobsub.brand = "1" and
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
      MsOwner.AgrCust NE MobSub.AgrCust OR MsOwner.TSEnd < ldActStamp
   THEN DO:
      flogError("Invalid MSOwner data").
      RETURN.
   END. 
 
   fSplitTS(ldActStamp,
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
   
      FIND FIRST MSISDN NO-LOCK WHERE 
                 MSISDN.Brand = gcBrand AND
                 MSISDN.CLI = MobSub.CLI NO-ERROR.
      IF AVAILABLE MSISDN THEN DO:
         fLocalMakeMsidnHistory(RECID(MSISDN)).
      END.
      FIND CURRENT MSISDN EXCLUSIVE-LOCK.
      ASSIGN MsIsdn.CustNum = liNewUser.
      
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
               FixedFee.InUse     = TRUE:
         
         IF NOT CAN-FIND(FIRST FFItem OF FixedFee WHERE
                               FFItem.Billed = FALSE AND
                               FFItem.Concerns[2] > liFeePeriod)
         THEN NEXT.
         
         /* shouldn't be, but better to check */   
         IF FixedFee.CustNum = liNewInvCust THEN NEXT. 
         
         CREATE bFixedFee.
         BUFFER-COPY FixedFee EXCEPT CustNum FFNum TO bFixedFee.
         
         ASSIGN bFixedFee.FFNum   = NEXT-VALUE(Contract)
                bFixedFee.CustNum = liNewInvCust
                bFixedFee.BegDate = ldtFeeDate. 
                
         FOR EACH FFItem OF FixedFee EXCLUSIVE-LOCK WHERE
                  FFItem.Billed      = FALSE AND
                  FFItem.Concerns[2] > liFeePeriod:
              
            /* split for both customers */
            IF FFItem.Concerns[1] < liFeePeriod THEN DO:
            
               CREATE bFFItem.
               BUFFER-COPY FFItem EXCEPT FFNum FFItemNum TO bFFItem.
               ASSIGN bFFItem.FFItemNum   = NEXT-VALUE(Citem)
                      bFFItem.FFNum       = bFixedFee.FFNum
                      bFFItem.CustNum     = bFixedFee.CustNum
                      bFFItem.Concerns[1] = liFeePeriod.
                      
               ldtFeeFrom = fInt2Date(FFItem.Concerns[1],1).
               ldtFeeTo   = fInt2Date(FFItem.Concerns[2],2).
               
               ASSIGN ldFeeAmt            = FFItem.Amt / 
                                            (ldtFeeFrom - ldtFeeTo + 1)
                      ldFeeAmt            = ldFeeAmt * 
                                            (ldtFeeDate - ldtFeeFrom)
                      FFItem.Amt          = ldFeeAmt
                      FFItem.Concerns[2]  = YEAR(ldtFeeDate - 1) * 10000 + 
                                            MONTH(ldtFeeDate - 1) * 100  +
                                            DAY(ldtFeeDate - 1)
                      bFFItem.Amt         = bFFItem.Amt - FFItem.Amt.
                      
            END. 

            ELSE ASSIGN FFItem.FFNum   = bFixedFee.FFNum
                        FFItem.CustNum = bFixedFee.CustNum.
         END. 

         FIND LAST FFItem OF FixedFee NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FFItem 
         THEN ASSIGN FixedFee.EndPer = FixedFee.BegPer
                     FixedFee.InUse  = FALSE.
         ELSE FixedFee.EndPer = FFItem.BillPeriod.
         
         FIND FIRST bFFItem OF bFixedFee NO-LOCK NO-ERROR.
         IF NOT AVAILABLE bFFItem
         THEN bFixedFee.InUse = FALSE.
         ELSE bFixedFee.BegPer = bFFItem.BillPer.

         /* memo */
         fWriteMemo("FixedFee",
                    STRING(bFixedFee.FFNum),
                    bFixedFee.CustNum,
                    "User Change",
                    "Transferred from customer " + STRING(FixedFee.CustNum)).
          
      END. 

      liFeePeriod = TRUNCATE(liFeePeriod / 100,0).
      
      FOR EACH SingleFee EXCLUSIVE-LOCK WHERE
               SingleFee.Brand      = gcBrand              AND
               SingleFee.HostTable  = "MobSub"             AND 
               SingleFee.KeyValue   = STRING(MobSub.MsSeq) AND
               SingleFee.Active     = TRUE                 AND
               SingleFee.Billed     = FALSE                AND
               SingleFee.BillPeriod > liFeePeriod:
               
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
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq AND
              MsOwner.TSEnd >= 99999999
   EXCLUSIVE-LOCK NO-ERROR.
   
   IF NOT AVAILABLE MsOwner THEN 
   FIND FIRST MsOwner WHERE
              MsOwner.MsSeq = MobSub.MsSeq EXCLUSIVE-LOCK.
   
   ASSIGN ldEndStamp    = MsOwner.TSEnd
          MsOwner.TSEnd = ldActStamp - 0.00001.
          
   CREATE bOwner.
   BUFFER-COPY MsOwner EXCEPT CustNum TSBeg TSEnd TO bOwner.
   ASSIGN bOwner.CustNum = liNewUser    WHEN liNewUser > 0
          bOwner.TSBeg   = ldActStamp
          bOwner.TSEnd   = ldEndStamp
          bOwner.InvCust = liNewInvCust WHEN liNewInvCust > 0
          bOwner.AgrCust = liNewOwner.

   RELEASE MsOwner.
   RELEASE bOwner.     
     
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
              "Transferred from customer " + STRING(bsourcecust.CustNum) +
              ", YTS-1088").
 
   IF MobSub.PayType THEN RUN pPrepaidRate(1/1/9,mobsub.cli).
   ELSE RUN pReRate(1/1/9,mobsub.cli).
   
   RELEASE MobSub.
   
END PROCEDURE.


PROCEDURE pReRate:

   DEFINE INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER icCLI    AS CHAR NO-UNDO.
 
   RUN Rate/cli_rate (icCLI,
                 idtFrom,
                 1/31/9,
                 TRUE).    
 
END PROCEDURE.


PROCEDURE pPrepaidRate:
   
   DEFINE INPUT PARAMETER idtFrom  AS DATE NO-UNDO.
   DEFINE INPUT PARAMETER icCLI    AS CHAR NO-UNDO.

   RUN Rate/cli_prepaidrate (icCLI,      
                        idtFrom, 
                        1/31/9,     
                        TRUE).      /* silent = true */  
END PROCEDURE.

 
