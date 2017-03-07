input from as_yts2457.txt.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lrRecid AS RECID NO-UNDO. 
DEFINE VARIABLE ldeStamp AS DECIMAL NO-UNDO. 
{Func/date.i}

DEFINE TEMP-TABLE ttCli
FIELD rateccn AS int format ">>9" label "RATECCN"
FIELD cli AS CHAR format "x(12)" label "GSMBNR"
FIELD i AS INT label "COUNT"
INDEX cli IS PRIMARY UNIQUE cli.

repeat:
   import unformatted lrRecid.
   find mobcdr where
      recid(mobcdr) = lrRecid NO-LOCK.

   ldeStamp = fhms2ts(mobcdr.datest, string(mobcdr.timestart,"HH:MM:SS")).

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
   end.
end.


FOR EACH ttCli NO-LOCK:
    disp ttCli.
end.
