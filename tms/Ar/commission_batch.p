/* ---------------------------------------------------------------------------
  MODULE .......: commission_batch.P
  FUNCTION .....: batch process for calculating commission
  SYSTEM .......: TMS
  AUTHOR .......: aam
  CREATED ......: 31.10.08
  CHANGED ......: 
  Version ......: Yoigo
  ------------------------------------------------------------------------- */

{Syst/commpaa.i}

ASSIGN 
   Syst.Var:gcBrand = "1" 
   Syst.Var:katun   = "Cron".
       
{Syst/eventlog.i}

DEF VAR liChecked AS INT  NO-UNDO.
DEF VAR liDone    AS INT  NO-UNDO.

fELog("COMMISSION","started").

RUN Ar/commission_run.p (OUTPUT liChecked,
                    OUTPUT liDone).

fELog("COMMISSION","stopped:Activated" + STRING(liDone)).


fELog("COMMISSION_CANCEL","started").

RUN Ar/commission_cancel.p (OUTPUT liChecked,
                       OUTPUT liDone).

fELog("COMMISSION_CANCEL","stopped:Cancelled" + STRING(liDone)).


QUIT.

