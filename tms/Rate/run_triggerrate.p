{Syst/commpaa.i}
ASSIGN
   Syst.CUICommon:katun = "Cron"
   Syst.CUICommon:gcBrand = "1".

DEF VAR liDone AS INT  NO-UNDO.


RUN Rate/triggerrate.p(0,0,OUTPUT liDone).
