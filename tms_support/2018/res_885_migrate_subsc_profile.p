/*
   RES-885 National roaming traffic restrreiction.
   Mass integration for all subscriptions.
*/

def STREAM sout.

DEFINE VARIABLE iDispInterval AS INTEGER NO-UNDO INITIAL 10000.
DEFINE VARIABLE ldaDate AS DATE NO-UNDO.
DEFINE VARIABLE iDate AS INT NO-UNDO.
DEFINE VARIABLE iCount AS INTEGER NO-UNDO.

FIND FIRST ServPac WHERE
           ServPac.ServPac = "*1" NO-LOCK NO-ERROR.

IF NOT AVAIL ServPac THEN DO:
   MESSAGE "ServPac (*1) not found. EXIT."  VIEW-AS ALERT-BOX.
   RETURN.
END.

OUTPUT STREAM sout TO nw_profile_mobsub.log append.

FOR EACH MobSub NO-LOCK WHERE
         MobSub.Brand = "1" AND
         MobSub.CliType BEGINS "CONT":
   /* skip those that exist */
   FIND FIRST SubSer WHERE
              SubSer.MsSeq = MobSub.MsSeq AND
              SubSer.ServCom = "NW" NO-LOCK NO-ERROR.
   IF AVAIL SubSer THEN NEXT. /* Supposed that SubSer.SSStat is OK (2/3) */

   FOR EACH ServEl OF ServPac where
      ServEl.servcom = "NW" NO-LOCK:
      FIND FIRST SubSer WHERE
                 SubSer.MsSeq   = MobSub.MsSeq AND
                 SubSer.ServCom = ServEl.ServCom
      NO-LOCK NO-ERROR.
      IF NOT AVAIL SubSer THEN DO:
         /* Before 1.4. value 3 Yoigo + Orange + Telefónica
            After 1.4.  value 2 Yoigo + Orange */
         /* iDate = Func.Common:mDate2TS(SubSer.SSDate).*/
         iDate = Func.Common:mDate2TS(MobSub.CreationDate).
         IF iDate < 20180401 THEN DO:
            CREATE SubSer.
            ASSIGN
               SubSer.MSSeq   = MobSub.MSSeq
               SubSer.ServPac = ServEl.ServPac
               SubSer.ServCom = ServEl.ServCom
               SubSer.SSDate  = TODAY
               SubSer.SSStat  = 3. /*  ServEl.SeValue.*/
         END.
         ELSE DO: /* 1.4. onwards */
            CREATE SubSer.
            ASSIGN
               SubSer.MSSeq   = MobSub.MSSeq
               SubSer.ServPac = ServEl.ServPac
               SubSer.ServCom = ServEl.ServCom
               SubSer.SSDate  = TODAY
               SubSer.SSStat  = 2. /*  ServEl.SeValue.*/
         END.
         PUT STREAM sout UNFORMATTED MobSub.CLI "|" recid(subser) SKIP.
      END.
   END.
   /* iCount = iCount + 1.
   IF iDispInterval > 0 AND iCount MOD iDispInterval EQ 0 THEN DISP iCount.*/
/*   if iCount > 1 then leave. */
END.
