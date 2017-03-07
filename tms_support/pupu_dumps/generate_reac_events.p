DEF VAR gcBrand AS CHAR NO-UNDO.

DEF BUFFER bTermMsRequest FOR MsRequest.

{Func/matrix.i}
{Func/timestamp.i}

OUTPUT TO "/store/riftp/pupu_dumps/logs/generate_reac_events.txt".

FOR EACH MsRequest WHERE
         MsRequest.Brand     = "1"  AND
         MsRequest.ReqType   = 82   AND
         MsRequest.ReqStatus = 2    AND
         MsRequest.ActStamp >= 20130625 NO-LOCK:

   FIND FIRST MobSub WHERE
              MobSub.MsSeq = MsRequest.MsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL MobSub THEN NEXT.

   FIND LAST bTermMsRequest WHERE
             bTermMsRequest.MsSeq     = MsRequest.MsSeq AND
             bTermMsRequest.ReqType   = 18 AND
             bTermMsRequest.ReqStatus = 2 NO-LOCK NO-ERROR.
   IF NOT AVAIL bTermMsRequest THEN NEXT.

   /* HPD - Trigger some extra events to Cassandra */
   RUN pTriggerEvents(INPUT MobSub.MsSeq,
                      INPUT MobSub.CLIType,
                      INPUT bTermMsRequest.ActStamp).

   PUT UNFORMATTED STRING(MobSub.MsSeq) "|" MobSub.CLI "|"
                   STRING(bTermMsRequest.ActStamp) "|" STRING(MsRequest.ActStamp) SKIP.

END. /* FOR EACH MsRequest WHERE */

OUTPUT CLOSE.

PROCEDURE pTriggerEvents:

   DEF INPUT PARAMETER iiMsSeq        AS INT  NO-UNDO.
   DEF INPUT PARAMETER icCLIType      AS CHAR NO-UNDO.
   DEF INPUT PARAMETER ideTermStamp   AS DEC  NO-UNDO.

   DEF VAR ldStartDate                AS DATE NO-UNDO.
   DEF VAR ldTermDate                 AS DATE NO-UNDO.
   DEF VAR liTermTime                 AS INT  NO-UNDO.
   DEF VAR ldeStartStamp              AS DEC  NO-UNDO.
   DEF VAR ldtTimeStamp               AS DEC  NO-UNDO.
   DEF VAR lcReqChar                  AS CHAR NO-UNDO.
   DEF VAR lcDel2                     AS CHAR NO-UNDO.
   DEF VAR lcServList                 AS CHAR NO-UNDO.
   DEF VAR lcServCom                  AS CHAR NO-UNDO.
   DEF VAR liCount                    AS INT  NO-UNDO.
   DEF VAR liNumEntries               AS INT  NO-UNDO.

   fSplitTS(ideTermStamp,OUTPUT ldTermDate,OUTPUT liTermTime).

   ASSIGN ldStartDate   = DATE(MONTH(ldTermDate),1,YEAR(ldTermDate))
          ldeStartStamp = fMake2Dt(ldStartDate,0)
          ldtTimeStamp  = DATETIME(TODAY,MTIME) + 15000
          lcServList    = "LANG,CALLSPEC,LTE"
          liNumEntries  = NUM-ENTRIES(lcServList)
          lcDel2        = CHR(255).

   CREATE Ordercanal.RepLog.
   ASSIGN
      Ordercanal.RepLog.RecordId  = RECID(MobSub)
      Ordercanal.RepLog.TableName = "MobSub"
      Ordercanal.RepLog.EventType = "MODIFY"
      Ordercanal.RepLog.KeyValue  = STRING(iiMsSeq)
      Ordercanal.RepLog.EventTS   = DATETIME(TODAY,MTIME).

   RELEASE Ordercanal.RepLog.

   FOR EACH MServiceLimit WHERE
            MServiceLimit.MsSeq  = iiMsSeq AND
            MServiceLimit.EndTS >= ldeStartStamp NO-LOCK,
      FIRST ServiceLimit WHERE
            ServiceLimit.SlSeq = MServiceLimit.SlSeq NO-LOCK:

      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        ServiceLimit.GroupCode + ";" + icCLIType,
                        OUTPUT lcReqChar) NE 1 THEN NEXT.

      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(MServiceLimit)
         Common.RepLog.TableName = "MServiceLimit"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(MServiceLimit.MSID)
         Common.RepLog.EventTS   = ldtTimeStamp.

      RELEASE Common.RepLog.
 
   END. /* FOR EACH MServiceLimit WHERE */

   FOR EACH MServiceLPool WHERE
            MServiceLPool.MsSeq  = iiMsSeq AND
            MServiceLPool.EndTS >= ldeStartStamp NO-LOCK,
      FIRST ServiceLimit WHERE
            ServiceLimit.SlSeq = MServiceLPool.SlSeq NO-LOCK:

      IF fMatrixAnalyse(gcBrand,
                        "PERCONTR",
                        "PerContract;SubsTypeTo",
                        ServiceLimit.GroupCode + ";" + icCLIType,
                        OUTPUT lcReqChar) NE 1 THEN NEXT.

      CREATE Common.RepLog.
      ASSIGN
         Common.RepLog.RecordId  = RECID(MServiceLPool)
         Common.RepLog.TableName = "MServiceLPool"
         Common.RepLog.EventType = "MODIFY"
         Common.RepLog.KeyValue  = STRING(MServiceLPool.MsSeq) + lcDel2 +
                                   STRING(MServiceLPool.SLSeq) + lcDel2 +
                                   STRING(MServiceLPool.EndTS)
         Common.RepLog.EventTS   = ldtTimeStamp.

      RELEASE Common.RepLog.
 
   END. /* FOR EACH MServiceLPool WHERE */

   FOR EACH DCCLI WHERE
            DCCLI.MsSeq    = iiMsSeq AND
            DCCLI.ValidTo >= ldStartDate NO-LOCK:

      CREATE Mobile.RepLog.
      ASSIGN
         Mobile.RepLog.RecordId  = RECID(DCCLI)
         Mobile.RepLog.TableName = "DCCLI"
         Mobile.RepLog.EventType = "MODIFY"
         Mobile.RepLog.KeyValue  = STRING(DCCLI.PerContractID)
         Mobile.RepLog.EventTS   = ldtTimeStamp.

      RELEASE Mobile.RepLog.

   END. /* FOR EACH DCCLI WHERE */

   DO liCount = 1 TO liNumEntries:

      lcServCom = ENTRY(liCount,lcServList).

      FOR FIRST SubSer WHERE
                SubSer.MsSeq   = MobSub.MsSeq AND
                SubSer.ServCom = lcServCom NO-LOCK:

         CREATE Mobile.RepLog.
         ASSIGN
            Mobile.RepLog.RecordId  = RECID(SubSer)
            Mobile.RepLog.TableName = "SubSer"
            Mobile.RepLog.EventType = "MODIFY"
            Mobile.RepLog.KeyValue  = STRING(SubSer.MsSeq) + lcDel2 +
                                      SubSer.ServCom       + lcDel2 +
                                      STRING(SubSer.SSDate)
            Mobile.RepLog.EventTS   = ldeTimeStamp.

         RELEASE Mobile.RepLog.

      END. /* FOR FIRST SubSer WHERE */
   END. /* DO liCount = 1 TO liNumEntries: */

END PROCEDURE.
