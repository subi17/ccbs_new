input from as_yts2457_2.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lrRecid AS RECID NO-UNDO. 
DEFINE VARIABLE ldeStamp AS DECIMAL NO-UNDO. 
{Func/date.i}

DEFINE TEMP-TABLE ttCli
FIELD cli AS CHAR format "x(12)" label "GSMBNR"
FIELD paiva AS  date
INDEX cli IS PRIMARY UNIQUE cli paiva.

def stream slog.
output stream slog to as_yts2457_2_report.txt.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
repeat:
   import unformatted lrRecid.
   find mobcdr where
      recid(mobcdr) = lrRecid NO-LOCK.

   ldeStamp = fhms2ts(mobcdr.datest, string(mobcdr.timestart,"HH:MM:SS")).

   find first ttCli where
              ttCli.cli = mobcdr.cli and
              ttCli.paiva = mobcdr.datest
   EXCLUSIVE-LOCK no-error.
      
   IF NOT AVAIL ttCli then do:
      create ttCli.
      assign
         ttCli.cli = mobcdr.cli
         ttCli.paiva = mobcdr.datest.
      i = i + 1.
   end.
  
/*
   find first msowner where
      msowner.cli = mobcdr.gsmbnr and
      msowner.tsend > ldeStamp and
      msowner.tsbegin < ldeStamp NO-LOCK no-error.
   
   IF AVAIL msowner then do:
      find first ttCli where
         ttCli.cli = msowner.cli and
         ttCli.rateccn = mobcdr.rateccn EXCLUSIVE-LOCK no-error.
      IF NOT AVAIL ttCli then do:
         create ttCli.
         assign
            ttCli.rateccn = mobcdr.rateccn
            ttCli.cli = mobcdr.gsmbnr.
      end.
      ttCli.i = ttCli.i + 1.
   end. */
end.

disp i.

put stream slog unformatted 
   "A_NUMBER|B_NUMBER|CC|DATE|TIME" skip.

FOR EACH ttCli NO-LOCK:

   FOR EACH mobcdr NO-LOCK where
            mobcdr.cli = ttCli.cli and
            mobcdr.datest = ttCli.paiva and
            mobcdr.rateccn = 96 and
            mobcdr.errorcode = 0:

      put stream slog unformatted 
         mobcdr.cli "|" 
         mobcdr.gsmbnr "|"
         mobcdr.rateccn "|"
         mobcdr.datest format "99-99-9999" "|"
         string(mobcdr.timest,"HH:MM:SS") skip.
   END.
   
   FOR EACH mobcdr NO-LOCK where
            mobcdr.cli = ttCli.cli and
            mobcdr.datest = ttCli.paiva and
            mobcdr.rateccn = 94 and
            mobcdr.errorcode = 0:

      put stream slog unformatted 
         mobcdr.cli "|" 
         mobcdr.gsmbnr "|"
         mobcdr.rateccn "|"
         mobcdr.datest format "99-99-9999" "|"
         string(mobcdr.timest,"HH:MM:SS") skip.
   END.
END.
