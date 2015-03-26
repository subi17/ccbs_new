{testpaa.i}
katun = "ari".
{barrfunc.i}

def var lcline  as char no-undo.
def var limsseq as int  no-undo.
def var lccli   as char no-undo.
def var lcbarr  as char no-undo.
def var i       as int  no-undo.
def var j       as int  no-undo.
def var linew   as int  no-undo init 50.
def var licurr  as int  no-undo.
def var ldjune    as dec  no-undo init 20090630.86399.
def var ldjulybeg as dec  no-undo init 20090701.00000.
def var ldjulyend as dec  no-undo init 20090731.86399.
def var ldaugust  as dec  no-undo init 20090801.00000.
def var ldstamp as dec  no-undo.
def var lldone  as log  no-undo.
def var lcsep   as char no-undo.

def buffer btarget for billtarget.
def buffer bmobsub for mobsub.
def buffer bowner  for msowner.
def buffer cowner  for msowner.

def stream sread.
input stream sread from /apps/snet/200907/sms_discount.txt.

def stream slog.
output stream slog to /apps/snet/200907/aam_ycm1654.log append.

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


lcsep = chr(9).

repeat:

   import stream sread unformatted lcline.
   
   assign 
      limsseq = integer(entry(1,lcline,lcsep))
      lccli   = entry(2,lcline,lcsep)
      no-error.
     
   if error-status:error or limsseq = 0 then next.
   
   i = i + 1.
   pause 0.
   disp i j with 1 down.
   
   find first mobsub where mobsub.msseq = limsseq no-lock no-error.
   if not available mobsub or mobsub.cli ne lccli then do:
      flog("Invalid subscription").
      next.
   end.

   lcbarr = fCheckStatus(limsseq).
   
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
         btarget.discplan   = "SMSJULY2009".
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
   
   /* mark new target only for july */
   for each msowner exclusive-lock where
            msowner.msseq = mobsub.msseq and
            msowner.cli   = mobsub.cli   and
            msowner.tsbegin < ldjulyend  and
            msowner.tsend > ldjulybeg    and
            msowner.billtarget ne linew
   by msowner.tsend desc:

      if msowner.tsend > ldjulyend then do:
         assign 
            ldstamp       = msowner.tsend  
            msowner.tsend = ldjulyend.

         /* return to original billing target in august */ 
         create bowner.
         buffer-copy msowner except tsbegin tsend to bowner.
         assign 
            bowner.tsbegin = ldaugust
            bowner.tsend   = ldstamp.
      end.   

      if msowner.tsbegin < ldjulybeg then do:
         /* use original billing target to the end of june */
         create bowner.
         buffer-copy msowner except tsend to bowner.
         assign
            bowner.tsend    = ldjune
            msowner.tsbegin = ldjulybeg.
      end.      
         
      assign
         msowner.billtarget = linew
         lldone = true.
   end.
      

   if not lldone then do:
      flog("Nothing to do").
      next.
   end.
   
   run cli_rate.p (mobsub.cli,
                   7/1/9,
                   7/31/9,
                   true).
    
   flog("OK").
   
   j = j + 1.
end.

input stream sread close.

disp i j.


