{Syst/testpaa.i}
katun = "Qvantel".
gcBrand = "1".
{Func/msreqfunc.i}

DEFINE VARIABLE clis AS CHARACTER NO-UNDO. 
clis = "32936699 32936757 32936764 32936768 32936780 32936783 32936905 32936908 32936910 32936922 32936940 32936951 32936983 32936985 32936994 32937024".

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE limsrequest AS INTEGER NO-UNDO. 

def frame a with 17 down.
do i = 1 to num-entries(clis, " "):

   limsrequest = int(entry(i,clis, " ")).

   find msrequest where
        msrequest.msrequest = limsrequest and
        msrequest.reqtype = 18 and
        msrequest.reqstatus = 3 NO-LOCK no-error.
   IF NOT AVAIL msrequest then next.

   find termmobsub where
        termmobsub.msseq = msrequest.msseq NO-LOCK no-error.
   IF NOT AVAIL termmobsub then next.

   freqstatus(2,"").
   disp msrequest.cli msrequest.reqtype msrequest.reqstatus with frame a.
   down 1 with frame a.
   
end.
