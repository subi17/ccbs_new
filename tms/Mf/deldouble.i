/* -----------------------------------------------------------------
  MODULE .......: deldouble.i
  FUNCTION .....: Erases double calls fom database
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 11.12.2000
  CHANGED ......: 20.12.2000 kl xbsub was used in mobtraf checking (now xsub)
                  19.01.2001 kl pause 1 if Customer is locked
                  06.03.2003 kl MASTER 1.0 changes
                  09.09.2003 aam brand

  VERSION ......: M15
  ------------------------------------------------------------------ */

{Mf/cdrconst.i}
{Mf/TMSQueue.i}
{Func/fcustcnt.i}

def var mobpref  as c  no-undo.
def var deldbl   as i  no-undo.
def var bOkToDel as lo no-undo.
def var xsub     as c  no-undo.
def var i-mth    as i  no-undo.
def var xloop    as i  no-undo.

/* all mobile prefixes into a string ... */
for each mobpref no-lock.
   assign mobpref = mobpref + "," + mobpref.Prefix.
end.
assign mobpref = substr(mobpref,2).

function fDelDouble returns integer
  (input callrec as recid, input bDel as log, input pDBFile as char).

   /*
   /* buffer has to be inside function */
   def buffer delcall for bufcdr.
   */

   /* reset double counter */
   deldbl = 0.

   find first bufcdr where 
        recid(bufcdr) = callrec
   no-lock no-error.

   for each double no-lock where
            double.Date      = bufcdr.Date      and
            double.InvCust   = bufcdr.InvCust   and
            double.EXCode    = bufcdr.EXCode    and
            double.TrunkIn   = bufcdr.TrunkIn   and
            double.TrunkOut  = bufcdr.TrunkOut  and
            double.CLI       = bufcdr.CLI       and
            double.BSub      = bufcdr.BSub      and
            double.TimeStart = bufcdr.TimeStart and
            double.Duration  = bufcdr.Duration  and
            double.pcmin     = bufcdr.pcmin     and
            double.tsin      = bufcdr.tsin      and
            double.pcmout    = bufcdr.pcmout    and
            double.tsout     = bufcdr.tsout     and
            double.CliType   = bufcdr.CliType   and
            double.BsubType  = bufcdr.BsubType  and
            recid(double)  ne recid(bufcdr).

      deldbl = deldbl + 1.

      find first invseq where
                 invseq.invseq = double.invseq
      no-lock no-error.

      /* OK to DEL when
         1: compared call is sent to operator but double is not
         - missing the case when both are sent to operator
         2: double does not belong to any invoice
         - missing the case when customer belongs to uninvoiced InvGroup
      */

      /* invseq must be: found & not billed */
      bOkToDel = (avail invseq AND NOT invseq.billed).

      if bOkToDel then 
         if double.OperSent AND NOT bufcdr.OperSent then bOkToDel = FALSE.

      if bOkToDel AND bDel then do:
         /* remove possible prefixes */
         xsub = double.BSub.
         do while substr(xsub,1,1) > "9":
            xsub = substr(xsub,2).
         end.

         IF pDBFile = "FIXED" THEN DO:

            /* reduce monthly call counter values */
            assign i-mth = (year(double.Date) * 100) + month(double.Date).

            /* add updated mthcall value to queue */
            fAddMthQ(double.InvCust, i-mth, 
                     0 - (double.GrossPrice - double.DiscValue)).

            find first invgroup where   
                       invgroup.brand    = gcBrand AND
                       invgroup.InvGroup = Customer.InvGroup
            no-lock no-error.

            if avail invgroup and invgroup.UpdCustBal then
               fCustCount(Customer.CustNum,"UB",
                          0 - (double.GrossPrice - double.DiscVal)).

         END.   

         /* now delete the double call */
         DBL:
         do while true:

            find first delcall where
                 recid(delcall) = recid(double)
            exclusive-lock no-error no-wait.

            if locked(delcall) then next DBL.
            else do:
               delete delcall.
            /* deldbl = deldbl + 1. */
            end.

            leave DBL.

         end.   

      end.  

   end. /* for each double */

   /* how many duplicates were deleted */
   return deldbl.

end. 


