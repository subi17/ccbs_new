{Syst/testpaa.i}
katun = "anttis".
{Func/barrfunc.i}

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var lcbarr  as char no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.
def var linew   as int  no-undo init 50.
def var licurr  as int  no-undo.
def var ldjuly  as dec  no-undo init 20090731.86399.
def var ldaugustbeg as dec  no-undo init 20090801.00000.
def var ldaugustend as dec  no-undo init 20090831.86399.
def var ldseptember as dec  no-undo init 20090901.00000.
def var ldstamp as dec  no-undo.
def var lldone  as log  no-undo.
def var lcsep   as char no-undo.

def buffer btarget for billtarget.
def buffer bmobsub for mobsub.
def buffer bowner  for msowner.
def buffer cowner  for msowner.

def stream sread.
input stream sread from /apps/snet/200908/sms_discount.csv.

def stream slog.
output stream slog to /apps/snet/200908/as_yot29.log append.

put stream slog unformatted
   "Started " 
   string(today,"99.99.99") " "
   string(time,"hh:mm:ss") skip.


function flog returns logic
   (icmessage as char):

   put stream slog unformatted
      lcline lcsep
      icmessage skip.
   
end function.


lcsep = ";".

repeat:

   import stream sread unformatted lcline.
   
   assign 
      lccli   = substring(entry(1,lcline,lcsep),3)
      no-error.
     
   if error-status:error then next.
   
   i = i + 1.
   pause 0.
   disp i j with 1 down.
  
   if i <= 12 then next.

   find first mobsub where mobsub.cli = lccli no-lock no-error.
   if not available mobsub then do:
      flog("Invalid subscription").
      next.
   end.

   if lookup(mobsub.clitype,"cont,cont4") = 0 then do:
      flog("Wrong clitype " + mobsub.clitype).
      next.
   end.

   lcbarr = fCheckStatus(mobsub.msseq).
   
   if lcbarr begins "y_" or lcbarr begins "d_" then do:
      flog("Barring active").
      next.
   end.
   
   find first billtarget where
              billtarget.custnum    = mobsub.custnum and
              billtarget.billtarget = mobsub.billtarget no-lock no-error.
   if not available billtarget then do:
      flog("Current billtarget invalid").
      next.
   end.

   if not can-find(first btarget where 
                         btarget.custnum = mobsub.custnum and
                         btarget.billtarget = linew)
   then do:
      /* new billing target, use fixed nbr 50 */
      create btarget.
      assign
         btarget.custnum    = mobsub.custnum
         btarget.billtarget = linew
         btarget.rateplan   = billtarget.rateplan
         btarget.discplan   = "SMSAUG2009".
   end.
   
   /* note: don't update mobsub, because this is done in the end of the 
      period and original billing target will be used again from the 
      beginning of next period
   find first bmobsub where recid(bmobsub) = recid(mobsub) exclusive-lock.
   assign 
      licurr = mobsub.billtarget
      bmobsub.billtarget = linew.
   */
   
   lldone = false.
   
   /* mark new target only for august */
   for each msowner exclusive-lock where
            msowner.msseq = mobsub.msseq and
            msowner.cli   = mobsub.cli   and
            msowner.tsbegin < ldaugustend  and
            msowner.tsend > ldaugustbeg    and
            msowner.billtarget ne linew
   by msowner.tsend desc:

      if msowner.tsend > ldaugustend then do:
         assign 
            ldstamp       = msowner.tsend  
            msowner.tsend = ldaugustend.

         /* return to original billing target in september */ 
         create bowner.
         buffer-copy msowner except tsbegin tsend to bowner.
         assign 
            bowner.tsbegin = ldseptember
            bowner.tsend   = ldstamp.
      end.   

      if msowner.tsbegin < ldaugustbeg then do:
         /* use original billing target to the end of july */
         create bowner.
         buffer-copy msowner except tsend to bowner.
         assign
            bowner.tsend    = ldjuly
            msowner.tsbegin = ldaugustbeg.
      end.      
         
      assign
         msowner.billtarget = linew
         lldone = true.
   end.
      

   if not lldone then do:
      flog("Nothing to do").
      next.
   end.
   
   RUN Rate/cli_rate (mobsub.cli,
                   8/1/9,
                   8/31/9,
                   true).
    
   flog("OK").
   
   j = j + 1.
end.

input stream sread close.

disp i j.


