/* ----------------------------------------------------------------------
MODULE .......: recalculate_istc_irc_batch.p
TASK .........: Recalculates possible erroneous invrowcounters after iSTC
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 12.02.14
Version ......: yoigo
----------------------------------------------------------------------- */

{commpaa.i}
katun = "Cron".
gcBrand = "1".

DEFINE VARIABLE lcParam AS CHARACTER NO-UNDO. 

lcParam = SESSION:PARAMETER.
IF lcParam NE "month" THEN lcParam = "".

run recalculate_istc_irc.p(TODAY,lcParam).
