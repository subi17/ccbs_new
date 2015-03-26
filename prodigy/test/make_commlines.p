ROUTINE-LEVEL ON ERROR UNDO, THROW.
USING gearbox.fixtures.FixtureReader.

OUTPUT TO VALUE(ENTRY(2, SESSION:PARAMETER)).

DEF VAR lfr AS FixtureReader NO-UNDO.
DEF BUFFER lbRequest FOR MsRequest.

{Syst/commpaa.i}
gcBrand = "1".

lfr = NEW FixtureReader().
lfr:verbosity = -1.
lfr:read(ENTRY(1, SESSION:PARAMETER)).
lfr:loadAll().
DELETE OBJECT lfr.

FOR EACH Solog EXCLUSIVE-LOCK:
  DELETE Solog.
END.

FOR EACH lbRequest:
  RUN Gwy/createsolog.p (lbRequest.MsRequest).
END.

FOR EACH Solog:
  PUT UNFORMATTED Solog.Commline SKIP.
END.

QUIT.
