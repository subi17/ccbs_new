
DISABLE TRIGGERS FOR LOAD OF MobSub.

{commpaa.i}
katun = "Cron".
gcBrand = "1".
{timestamp.i}
{fbundle.i}

DEF VAR lcTariff     AS CHAR NO-UNDO.
DEF VAR liNum        AS INT  NO-UNDO.
DEF VAR ldaActDate   AS DATE NO-UNDO.

DEF BUFFER bMobSub   FOR MobSub.

OUTPUT TO "/store/riftp/pupu_dumps/logs/update_mobsub_tariff_actdate.txt".

FOR EACH MobSub NO-LOCK:

   liNum = liNum + 1.
   STATUS DEFAULT STRING(liNum).

   lcTariff = fGetCurrentTariff(BUFFER MobSub,OUTPUT ldaActDate).

   DO TRANSACTION:
      FIND FIRST bMobSub WHERE
                 ROWID(bMobSub) = ROWID(MobSub)
           EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL bMobSub THEN DO:
         bMobSub.TariffActDate = ldaActDate.

         PUT UNFORMATTED
             STRING(MobSub.MsSeq) "|" MobSub.CLI "|" lcTariff "|"
             STRING(MobSub.CreationDate) "|" STRING(ldaActDate) "|" STRING(bMobSub.TariffActDate) SKIP.
      END.
      RELEASE MobSub.
  END.
END.

OUTPUT CLOSE.
