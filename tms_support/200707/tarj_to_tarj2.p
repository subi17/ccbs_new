{Syst/testpaa.i}
katun = "ari".

{Func/timestamp.i}

def stream sread.

input stream sread from /apps/snet/200707/tarj_to_tarj2.txt.

def stream slog.

output stream slog to /apps/snet/200707/tarj_to_tartj2.txt append.

def var lcline  as char no-undo.
def var ldOpen  as dec  no-undo.
def var ldClose as dec  no-undo.

def buffer bowner for msowner.

ASSIGN
   ldOpen  = fmake2dt(08/01/2007,0)
   ldClose = fmake2dt(07/31/2007,86399).

FIND FIRST CliType WHERE
           CliType.CliType = "TARJ2"
NO-LOCK NO-ERROR.

repeat:

   import stream sread unformatted lcline.
   
   find mobsub where mobsub.cli = lcline no-lock no-error.

   if not available mobsub then next.

   find last msowner where
             msowner.cli = lcline
   exclusive-lock no-error.

   if msowner.clitype ne "tarj" or msowner.tsend ne 99999999.99999 then next.

   find billtarget where
        billtarget.custnum    = mobsub.custnum and
        billtarget.billtarget = clitype.billtarget
   no-lock no-error.

   if not available billtarget then do:
      CREATE BillTarg.
      ASSIGN
         BillTarg.CustNum    = mobsub.CustNum
         BillTarg.BillTarget = CliType.BillTarget
         BillTarg.DiscPlan   = CliType.DiscPlan
         BillTarg.RatePlan   = CliType.PricePlan.
   end.
   
   find current mobsub exclusive-lock.

   assign
      mobsub.clitype    = "TARJ2"
      mobsub.billtarget = clitype.billtarget.
          
   msowner.tsend = ldClose.

   put stream slog unformatted
      msowner.cli      chr(9)
      msowner.clitype  chr(9)
      ldOpen            skip.
      
   create bowner.
   buffer-copy msowner except tsbeg tsend to bowner.
   
   assign
      bowner.tsbeg      = ldOpen
      bowner.tsend      = 99999999.99999
      bowner.clitype    = mobsub.clitype
      bowner.billtarget = mobsub.billtarget.
              

end.


