/*
   RES-885 National roaming traffic restrreiction.
   Mass integration for all subscriptions.
*/

def STREAM sout.

DEFINE VARIABLE iDispInterval AS INTEGER NO-UNDO INITIAL 10000.
DEFINE VARIABLE iDate AS INT NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

DEF VAR lcServpac AS CHAR NO-UNDO. 

OUTPUT STREAM sout TO res_885_migrate_subsc_profile.log append.

FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand = "1":

   /* pending mobile line activation */
   if MobSub.msstatus eq 16 then next.
   /* fixed only / mobile line not activated earlier */
   else if MobSub.msstatus eq 17 then do:
      FIND FIRST subser NO-LOCK where
                 subser.msseq = MobSub.msseq and
                 lookup(subser.servcom,"callspec,smsinvoice") = 0 no-error.
      IF NOT AVAIL subser then next.
   end.

   /* skip those that exist */
   FIND FIRST SubSer WHERE
              SubSer.MsSeq = MobSub.MsSeq AND
              SubSer.ServCom = "NW" NO-LOCK NO-ERROR.
   IF AVAIL SubSer THEN NEXT. /* Supposed that SubSer.SSStat is OK (2/3) */
   
  if MobSub.clitype begins "cont" then lcServpac = "*1".
  else lcServpac = "*2".

   FIND FIRST CTServEL NO-LOCK WHERE
              CTServEL.Brand = MobSub.Brand AND
              CTServEL.CLIType = MobSub.clitype and
              CTServEL.servpac = lcServpac and
              CTServEL.Servcom  = "NW"  NO-ERROR.
   IF NOT AVAIL CTServEL then do:
      PUT STREAM sout UNFORMATTED MobSub.CLI "|ERROR:CTServEL not found: " MobSub.CLITYPE SKIP.
      next.
   end.

   /* Before 1.4. value 3 Yoigo + Orange + Telefónica
      After 1.4.  value 2 Yoigo + Orange */
   /* iDate = Func.Common:mDate2TS(SubSer.SSDate).*/
   iDate = Func.Common:mDate2TS(MobSub.CreationDate).

   CREATE SubSer.
   ASSIGN
      SubSer.MSSeq   = MobSub.MSSeq
      SubSer.ServPac = CTServEl.ServPac
      SubSer.ServCom = CTServEl.ServCom
      SubSer.SSDate  = TODAY
      SubSer.SSStat  = (IF iDate < 20180401 THEN 3 else CTServEl.DefValue)
      SubSer.ssparam = (IF iDate < 20180401 then "YOIGO_ORANGE_TELEFONICA" else CTServEl.defparam).

   PUT STREAM sout UNFORMATTED MobSub.CLI "|" MobSub.msseq "|" recid(subser) SKIP.

   iCount = iCount + 1.
   IF iDispInterval > 0 AND iCount MOD iDispInterval EQ 0 THEN DO:
      DISP iCount with frame a.
      pause 0.
   end.
END.
