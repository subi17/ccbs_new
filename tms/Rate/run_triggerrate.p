{Syst/commpaa.i}
ASSIGN
   Syst.Var:katun = "Cron"
   Syst.Var:gcBrand = "1".

DEF VAR liDone AS INT  NO-UNDO.


RUN Rate/triggerrate.p(0,0,OUTPUT liDone).
