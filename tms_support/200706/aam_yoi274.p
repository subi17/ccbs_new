{Syst/testpaa.i}
Syst.Var:katun = "ari".


def stream sread.
input stream sread from /apps/snet/200706/clitype_change_yoi274.txt.

def stream slog.
output stream slog to /apps/snet/200706/aam_yoi274.log append.

def var lcline  as char no-undo.
def var lccli   as char no-undo.
def var lcold   as char no-undo.
def var lcnew   as char no-undo.
def var i       as int  no-undo.
def var ldchg   as dec  no-undo.
def var lcres   as char no-undo.
def var ldtcurr as date no-undo.

ldtcurr = 6/20/7. 

def buffer bowner for msowner.

ldchg = Func.Common:mMake2DT(ldtcurr,28800).


repeat:

   import stream sread unformatted lcline.
   
   assign lccli = entry(5,lcline,chr(9))
          lcold = entry(7,lcline,chr(9))
          lcnew = entry(9,lcline,chr(9))
          i     = i + 1.
          
   find mobsub where mobsub.cli = lccli no-lock no-error.
   if not available mobsub then next.

   find first clitype where
              clitype.brand = "1" and
              clitype.cliname = lcold no-lock no-error.
   if not available clitype or clitype.clitype ne mobsub.clitype then next.
   lcold = clitype.clitype.
              
   find first clitype where
              clitype.brand = "1" and
              clitype.cliname = lcnew no-lock no-error.
   if not available clitype then next.
   lcnew = clitype.clitype.
              
   find billtarget where
        billtarget.custnum    = mobsub.custnum and
        billtarget.billtarget = clitype.billtarget no-lock no-error.

   find first msowner where
              msowner.msseq = mobsub.msseq and
              msowner.tsend >= 99999999 exclusive-lock no-error.
 
   if not available msowner then do:
      message "check msowner:" mobsub.cli
      view-as alert-box.
      next.
   end.
   
   disp i
        mobsub.cli mobsub.clitype
        lcold format "x(10)" 
        lcnew format "x(10)"
        clitype.billtarget
        can-find(billtarget).

   if not available billtarget then do:
      CREATE BillTarg.
      ASSIGN
         BillTarg.CustNum    = mobsub.CustNum
         BillTarg.BillTarget = CliType.BillTarget
         BillTarg.DiscPlan   = CliType.DiscPlan
         BillTarg.RatePlan   = CliType.PricePlan.
   end.
   
   find current mobsub exclusive-lock.
   assign mobsub.clitype    = lcnew
          mobsub.billtarget = clitype.billtarget.
          
   msowner.tsend = ldchg.

   put stream slog unformatted
      msowner.cli      chr(9)
      msowner.clitype  chr(9)
      lcnew            chr(9)
      ldchg            skip.
      
   create bowner.
   buffer-copy msowner except tsbeg tsend to bowner.
   
   assign bowner.tsbeg      = ldchg + 0.00001
          bowner.tsend      = 99999999.99999
          bowner.clitype    = mobsub.clitype
          bowner.billtarget = mobsub.billtarget.
              
   /* monthly fee for cont2 */
   if clitype.feemodel1 > "" then do:
   
      RUN Mc/creasfee.p (MobSub.CustNum,
                    MobSub.MsSeq,
                    ldtcurr,
                    "MobSub",
                    CliType.FeeModel1,
                    1,
                    "",    /* memo   */
                    FALSE,           /* no messages to screen */
                    OUTPUT lcres).
   end.

end.


