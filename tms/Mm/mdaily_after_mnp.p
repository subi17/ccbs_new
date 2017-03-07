{Syst/commpaa.i}
{Func/timestamp.i}
gcbrand = "1".
katun = "cron".

{Syst/eventlog.i}

def var period  as int    no-undo.
DEF VAR liQty   AS INT    NO-UNDO.
DEF VAR liDel   AS INT    NO-UNDO. 
DEF VAR lcError AS CHAR   NO-UNDO. 
DEF VAR oiQty   AS INT    NO-UNDO.

fELog("MDAILY_AMNP","HighSpenderStarted").
RUN Mm/highusagerep.p(INPUT fMake2Dt(INPUT today - 90, INPUT 0),0).
fELog("MDAILY_AMNP","HighSpenderStopped").

fELog("MDAILY_AMNP","IccMSISDNRepStarted").
RUN Mm/icc_msisdn_rep.p.
fELog("MDAILY_AMNP","IccMSISDNRepStopped").

quit.
