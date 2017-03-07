/* -----------------------------------------------------------------
  MODULE .......: NNDOCO.P
  TASK .........: Erases double calls fom database
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 11.10.1997
  CHANGED ......: 11.05.98 kl => reduce b-sub counter values when needed
                  12.05.98 kl => reduce mthcall counter values + as-limi[3]
                  03.09.98 pt/kl => transaction limitation to double
                  22.10.98 kl => reduce also CGR counter values
                  25.11.98 kl => bug when reducing CGR counter values fixed
                                 check also switch & CGRs when matching
                  17.02.99 kl => reduce mobtraf counters if needed
                  23.02.99 kl => reduce inttraf counters if needed
                  29.06.99 kl => locking handling
                  27.09.99 kl => use PulseCodeModulation & TimeStamp for
                                 checking double calls 
                  03.11.99 kl => bDisp
                  09.11.99 kl => asubamt reducing
                  02.02.99 kl => check a-type & b-type & pu-as-l
                  03.10.99 kl => repeats into 'do while true'
                  11.09.01/aam   logfile didn't work properly
                  09.09.2002 kl buffers for FIXcdr
                  04.03.03 tk tokens
  VERSION ......: M15
  ------------------------------------------------------------------ */

{Mf/errors.i}

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'fixcdr'}


def buffer double  for FIXcdr.
def buffer bufcdr  for FIXcdr.
def buffer delcall for FIXcdr.
{Mf/deldouble.i}

{Func/excel.i}

def var exdir     as c  no-undo.
def var exname    as c  no-undo.
def var cadate1   as da no-undo.
def var cadate2   as da no-undo.
def var starttid  as c  no-undo.
def var i         as i  no-undo.
def var ok        as lo no-undo format "Yes/No".
def var prevdate  as da no-undo.
def var logfile   as c  no-undo.
def var hinta     as de no-undo.
def var chinta    as c  no-undo.
def var rubrik    as c  no-undo.
def var xbsub     as c  no-undo.
def var prefix    as c  no-undo.
def var bDisp     as lo no-undo format "Yes/No".
def var erase     as lo no-undo format "Yes/No".
def var lDeleted  as i  no-undo.
def var xDelete   as i  no-undo.

rubrik =
"Date,Started,Length,A-sub,B-sub,Cust.nr,Cust. name,Invd.,Price".


/* initial values */
tab = chr(9).
if opsys = "unix" then exdir = "/arlec/sspec".
else                   exdir = ".".

form
   skip(1)
   "  Instruction:  This program searches all DOUBLE CALLS within the"
   "                time period determined below.                    " skip(1)
   "                UNINVOICED double calls can be erased.    " skip(1)
   "                It is possible to create a TAB-separated ASCII-file "
   "                with the information of the double calls." skip(1)
   "                Calls between ...............:" cadate1
   format "99-9~9-99"
   help "Earliest call date"
   "-" cadate2 format "99-99-99" help "Latest call date"  skip
   "                Shall double calls be erased :" erase
   help "Do you wnat to erase all found double calls ?"
   "                Name of log -file ...........:"  logfile format "x(24)"
   help "Logfile's name,  empty: no log"                   skip
   "                Display double calls ........:" bDisp  skip(4)
with
   width 80 overlay color value(cfc) title color value(ctc)
   " " + ynimi + " ERASE DOUBLE RETAIL CALLS " + string(pvm,"99-99-99") + " "
   no-labels frame start.

assign
   cadate2 = date(month(today),1,year(today)) - 1
   cadate1 = date(month(cadate2),1,year(cadate2)).

cfc = "sel". RUN Syst/ufcolor.p.

/* all mobile prefixes into a string ... 
for each mobpref no-lock.
   assign mobpref = mobpref + mobpref.mp-pref + ",".
end.
assign mobpref = substr(mobpref,1,length(mobpref) - 1).
*/

CRIT:
repeat with frame start:
   ehto = 9. RUN Syst/ufkey.p.

   update
      cadate1  validate(cadate1 ne ?,"Give first date !")
      cadate2  validate(input cadate2 >= input cadate1,"Wrong order !")
      erase
      logfile
      bDisp
   with frame start
   /*
   editing.
      readkey.
      if lookup(keylabel(lastkey),poisnap) > 0 then do:
         pause 0.
         if frame-field = "exasno" then do:
            assign frame start exasno.
            if exasno = 0 then return.
            find Customer where Customer.as-nro = exasno no-lock no-error.
            if not avail Customer then do:
               bell.
               message "Unknown customer !".
               next.
            end.
            display Customer.CustName.
         end.
      end.
      apply lastkey.
   end. /* editing */
   */.
task:
   repeat with frame start:
      assign ufk = 0 ufk[1] = 7 ufk[5] = 178 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      if toimi = 1 then next  CRIT.
      if toimi = 8 then leave CRIT.

      if toimi = 5 then do:
         bell. message
         "Are You sure You want to start ERASING (Y/N) ?" update ok.
         if ok then leave TASK.
      end.
   end.

   message "Erasing double calls ...".

   if logfile ne "" then do:
      output stream excel to value(logfile).
      do i = 1 to num-entries(rubrik).
         put stream excel unformatted entry(i,rubrik) tab.
      end.
      RUN Syst/uexskip.p(2).
   end.

   i = time.

   for each FIXcdr  no-lock where
            FIXcdr.Date >= cadate1 and
            FIXcdr.Date <= cadate2,

      first invseq no-lock where
            invseq.invseq = FIXcdr.invseq.

      assign
         xDelete  = fDelDouble(recid(FIXcdr),erase,"FIXED")
         lDeleted = lDeleted + xDelete.

      if time - i >= 30 then do:
         put screen row 17 col 3 
            string(FIXcdr.Date,"99-99-99")  + " " + 
            string(FIXcdr.TimeStart,"hh:mm:ss") + " " + 
            string(lDeleted).
         i = time.
      end.

      if xDelete ne 0 and logfile ne "" then do:

         find Customer where Customer.CustNum = FIXcdr.InvCust no-lock.
         assign
            hinta = FIXcdr.GrossPrice - FIXcdr.DiscValue
            chinta = string(hinta,"zzzz9.99")
            substr(chinta,6,1) = ",".

         put stream excel unformatted
            string(FIXcdr.Date,"99-99-9999")  tab
            string(FIXcdr.TimeStart,"hh:mm:ss")   tab
            string(FIXcdr.Duration,"hh:mm:ss")  tab
            FIXcdr.CLI                          tab
            FIXcdr.BSub                      tab
            FIXcdr.InvCust                      tab
            CustName                             tab
            invseq.billed  format "Yes/No"      tab
            chinta                              my-nl.

      end.

   end. /* repeat */

   if logfile ne "" then output stream excel close.
   message "All done - press ENTER !".
   pause no-message.

   leave crit.

end.  /* crit */
hide frame log no-pause.
hide frame start no-pause.
