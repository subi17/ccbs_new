/* RUN BEFORE CHANGING BillTarget.RatePlan values */

DEF VAR i AS INT NO-UNDO.
DEF VAR j AS INT NO-UNDO.
DEF VAR lcCLiTypes AS CHAR NO-UNDO.
DEF VAR lcBilltargets AS CHAR NO-UNDO.

DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO.
llSimulate = true.

def stream sout.

if llSimulate then
   output stream sout to /apps/yoigo/tms_support/201803/yco_169_split_shared_billtargets.simu.txt.
else
   output stream sout to /apps/yoigo/tms_support/201803/yco_169_split_shared_billtargets.txt append.

FOR EACH clitype NO-LOCK where
         clitype.basebundle = "cont34":
   lcCLiTypes = lcCLiTypes + "," + clitype.clitype.
   lcBilltargets = lcBilltargets + "," + string(clitype.billtarget).
end.
lcCLiTypes = trim(lcCLiTypes,",").
lcBilltargets = trim(lcBilltargets,",").

def buffer bmsowner for msowner.

/* BillTarget records might be shared between CLITypes, 
   so they must be split BEFORE billtarget.rateplan is changed */
FOR EACH msowner NO-LOCK:

   i = i + 1.

   if i mod 10000 eq 0 THEN DO:
      disp  i j with frame b.
      pause 0.
   END.

   if lookup(msowner.clitype,lcCLiTypes) = 0 then do:
      if lookup(string(msowner.billtarget),lcBilltargets) > 0 then do:

         find clitype NO-LOCK where
              clitype.clitype = msowner.clitype no-error.
      end.
      else next.
   end.
   else find clitype NO-LOCK where
             clitype.clitype = msowner.clitype.

   if msowner.billtarget eq clitype.billtarget then next.

   do trans:

   FIND FIRST billtarget NO-LOCK where
              billtarget.custnum = msowner.custnum and
              billtarget.billtarget = clitype.billtarget no-error.
   IF NOT AVAIL billtarget then do:

      if not llSimulate then do:
         create billtarget.
         assign
            billtarget.custnum = msowner.custnum
            billtarget.billtarget = clitype.billtarget
            billtarget.rateplan = clitype.priceplan.
      end.

      put stream sout unformatted
         "BILLTARGET_CREATED" ";"
         msowner.custnum ";"
         clitype.billtarget ";"
         clitype.priceplan ";"
         clitype.clitype skip.
   end.

   j = j + 1.

   put stream sout unformatted
      "MSOWNER" ";"
      msowner.custnum ";"
      msowner.msseq ";"
      msowner.cli ";"
      msowner.clitype ";"
      msowner.billtarget ";"
      recid(msowner) skip.

   if not llSimulate then do:
       find bmsowner EXCLUSIVE-LOCK where
            rowid(bmsowner) = rowid(msowner).
       bmsowner.billtarget = clitype.billtarget.
       release bmsowner.
   end.

   FIND FIRST mobsub NO-LOCK where
              mobsub.msseq = msowner.msseq and
              mobsub.clitype = msowner.clitype and
              mobsub.billtarget ne clitype.billtarget no-error.
   IF AVAIL mobsub then do:
      put stream sout unformatted
         "MOBSUB" ";"
         mobsub.custnum ";"
         mobsub.msseq ";"
         mobsub.cli ";"
         mobsub.clitype ";"
         mobsub.billtarget skip.

      if not llSimulate then do:
         find current mobsub EXCLUSIVE-LOCK.
         mobsub.billtarget = clitype.billtarget.
         release mobsub.
      end.
   end.

   FIND FIRST termmobsub NO-LOCK where
              termmobsub.msseq = msowner.msseq and
              termmobsub.clitype = msowner.clitype and
              termmobsub.billtarget ne clitype.billtarget no-error.
   IF AVAIL termmobsub then do:
      put stream sout unformatted
         "TERMMOBSUB" ";"
         termmobsub.custnum ";"
         termmobsub.msseq ";"
         termmobsub.cli ";"
         termmobsub.clitype ";"
         termmobsub.billtarget ";"
         recid(termmobsub) skip.
      if not llSimulate then do:
         find current termmobsub EXCLUSIVE-LOCK.
         termmobsub.billtarget = clitype.billtarget.
         release termmobsub.
      end.
   end.
   end. /* trans */

end.
