/* nagios monitoring script for dump logs */
{date.i}

DEFINE VARIABLE ldaStart AS DATE NO-UNDO. 
DEFINE VARIABLE liStart AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldaEnd AS DATE NO-UNDO. 
DEFINE VARIABLE liEnd AS INTEGER NO-UNDO. 
DEFINE VARIABLE liMaxDelay AS INTEGER NO-UNDO. 

DEFINE VARIABLE ldeCheckFrom AS DECIMAL NO-UNDO. 

DEFINE VARIABLE liEndedError AS INTEGER NO-UNDO. 
DEFINE VARIABLE liEndedSuccess AS INTEGER NO-UNDO. 
DEFINE VARIABLE liActive AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCancelled AS INTEGER NO-UNDO. 
DEFINE VARIABLE liActiveDelayed AS INTEGER NO-UNDO. 
DEFINE VARIABLE liQueued AS INTEGER NO-UNDO. 
DEFINE VARIABLE liQueuedDelayed AS INTEGER NO-UNDO. 

ldeCheckFrom = fOffset(fMakeTS(),-24 * 14).

FOR EACH dumplog NO-LOCK where
         dumplog.createstart  > ldeCheckFrom,
   first dumpfile NO-LOCK where
         dumpfile.dumpid = dumplog.dumpid:

   if dumplog.dumplogstatus eq 0 then do:

      fSplitTs(dumplog.createstart, output ldaStart, output liStart).

      /*if dumplog.createend > 0 then
         fSplitTs(dumplog.createend, output ldaEnd, output liEnd).
      else */ ASSIGN
         ldaEnd = TODAY
         liEnd = TIME.

      if dumpfile.dumpname eq "ActiveServices" and
         dumplog.dumptype eq "full" then liMaxDelay = 60.
      else if dumpfile.dumpname eq "PerContrDump" and
         dumplog.dumptype eq "full" then liMaxDelay = 30.
      else liMaxDelay = 8.

         if (datetime(ldaEnd,liEnd * 1000) -  datetime(ldaStart,liStart * 1000)
            ) > liMaxDelay * 3600 * 1000 then 
             liActiveDelayed = liActiveDelayed + 1.
   end.

   case dumplog.dumplogstatus:
      when 0 then liActive = liActive + 1.
      when 1 then liEndedError = liEndedError + 1.
      when 2 or when 3 then liEndedSuccess = liEndedSuccess + 1.
      when 5 then liCancelled = liCancelled + 1.
   end.     
end.

FOR EACH dumpfile NO-LOCK where
         dumpfile.active = true,
    EACH dftimetable of dumpfile NO-LOCK where
         dftimetable.ongoing > 0:

    IF CAN-FIND(FIRST dumplog NO-LOCK where
                      dumplog.dumpid = dumpfile.dumpid and
                      dumplog.dumplogstatus = 0) then next.

    liQueued = liQueued + 1.
    fSplitTs(dftimetable.ongoing, output ldaStart, output liStart).

   if (datetime(TODAY,TIME * 1000) -  datetime(ldaStart,liStart * 1000)
      ) > 8 * 3600 * 1000 then 
      liQueuedDelayed = liQueuedDelayed + 1.
      
END.

message
   liActiveDelayed
   liQueuedDelayed 
   liActive
   liQueued
   liEndedError
   liCancelled
   liEndedSuccess
   .
