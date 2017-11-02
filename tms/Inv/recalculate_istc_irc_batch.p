/* ----------------------------------------------------------------------
MODULE .......: recalculate_istc_irc_batch.p
TASK .........: Recalculates possible erroneous invrowcounters after iSTC
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 12.02.14
Version ......: yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
Syst.Var:katun = "Cron".
Syst.Var:gcBrand = "1".

DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO. 

lcParam = SESSION:PARAMETER.
IF lcParam NE "month" THEN lcParam = "".

RUN Inv/recalculate_istc_irc.p(TODAY,lcParam).
