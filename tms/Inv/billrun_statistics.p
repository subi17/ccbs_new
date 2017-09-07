/* ----------------------------------------------------------------------
  MODULE .......: billrun_statistics.p
  TASK .........: Calculate statistics from billing run performance
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 26.02.10
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Func/cparam2.i}

DEF INPUT PARAMETER idaInvDate AS DATE NO-UNDO.
DEF INPUT PARAMETER iiInvType  AS INT  NO-UNDO.
DEF INPUT PARAMETER icRunMode  AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiInvQty  AS INT  NO-UNDO.

DEF VAR liCnt      AS INT  NO-UNDO.
DEF VAR liQty      AS INT  NO-UNDO.
DEF VAR ldaDate    AS DATE NO-UNDO.
DEF VAR liTime     AS INT  NO-UNDO.
DEF VAR liHour     AS INT  NO-UNDO.
DEF VAR llCDRs     AS LOG  NO-UNDO.
DEF VAR liSubs     AS INT  NO-UNDO.
DEF VAR liCDRQty   AS INT  NO-UNDO.
DEF VAR liSubQty   AS INT  NO-UNDO.
DEF VAR ldStart    AS DEC  NO-UNDO.
DEF VAR ldEnd      AS DEC  NO-UNDO.
DEF VAR ldtStart   AS DATETIME NO-UNDO.
DEF VAR ldtEnd     AS DATETIME NO-UNDO.
DEF VAR liDays     AS INT  NO-UNDO.
DEF VAR lcDur      AS CHAR NO-UNDO.
DEF VAR liDur      AS INT  NO-UNDO.
DEF VAR liTotInv   AS INT  NO-UNDO.
DEF VAR liTotCDR   AS INT  NO-UNDO.
DEF VAR liTotSub   AS INT  NO-UNDO.
DEF VAR ldAbsHour  AS DEC  NO-UNDO EXTENT 35.
DEF VAR lcFile     AS CHAR NO-UNDO.
DEF VAR lcTransDir AS CHAR NO-UNDO.

DEF TEMP-TABLE ttHourStat NO-UNDO
   FIELD ttDay  AS INT 
   FIELD ttHour AS INT COLUMN-LABEL "Hour" FORMAT ">9"
   FIELD StartStamp AS DEC 
   FIELD InvQty AS INT COLUMN-LABEL "Invoices" 
   FIELD CDRQty AS INT COLUMN-LABEL "CDRs"
   FIELD SubQty AS INT 
   INDEX ttHour ttDay ttHour
   INDEX StartStamp StartStamp.
    
DEF TEMP-TABLE ttBillRun NO-UNDO
   FIELD ttDay AS INT
   FIELD ttHour AS INT
   FIELD BillRun AS CHAR
   FIELD InvQty AS INT
   FIELD CDRQty AS INT
   FIELD SubQty AS INT
   INDEX ttHour ttDay ttHour BillRun.

DEF TEMP-TABLE ttCumulative NO-UNDO
   FIELD ttDay AS INT
   FIELD ttHour AS INT
   FIELD StartStamp AS DEC 
   FIELD InvQty AS INT
   FIELD CDRQty AS INT
   FIELD SubQty AS INT
   INDEX ttHour ttDay ttHour.

DEF STREAM sLog.


IF idaInvDate = ? THEN RETURN "ERROR:Invoice date not defined".

lcFile = fCParamC("BillRunStatFile").
IF icRunMode = "test" THEN DO:
   lcTransDir = fCParamC("FRTestRunDir").
   IF lcTransDir > "" THEN 
      lcFile = lcTransDir + "/" + ENTRY(NUM-ENTRIES(lcFile,"/"),lcFile,"/").
END.
IF lcFile = ? OR lcFile = "" THEN
   lcFile = SUBST("/tmp/&1_billrun_statistics_#IDATE.txt",
                  Syst.Parameters:Tenant).
lcFile = REPLACE(lcFile,"#IDATE",STRING(YEAR(idaInvDate),"9999") +
                                 STRING(MONTH(idaInvDate),"99") + 
                                 STRING(DAY(idaInvDate),"99")).

ASSIGN 
   ldStart = 99999999
   llCDRs  = TRUE.

FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand   = "1" AND
         Invoice.InvDate = idaInvDate AND
         Invoice.InvType = iiInvType AND
         Invoice.ChgStamp > 0:

    ASSIGN         
       ldStart = MIN(ldStart,Invoice.ChgStamp)
       ldEnd   = MAX(ldEnd,Invoice.ChgStamp).

    liQty = liQty + 1.
    IF NOT SESSION:BATCH AND liQty MOD 1000 = 0 THEN DO:
       PAUSE 0.
       DISP liQty LABEL "1. Invoices" 
         WITH 1 DOWN ROW 1 OVERLAY FRAME fQty1.
    END.
 
END.

ldAbsHour[1] = ldStart.
DO liCnt = 2 to EXTENT(ldAbsHour):
   ldAbsHour[liCnt] = fSecOffSet(ldAbsHour[liCnt - 1],3600). 
END.

DO liCnt = 1 TO EXTENT(ldAbsHour):

   fSplitTS(ldAbsHour[liCnt],
            OUTPUT ldaDate,
            OUTPUT liTime).
 
   CREATE ttHourStat.
   ASSIGN 
      ttHourStat.ttDay  = DAY(ldaDate)
      ttHourStat.ttHour = liCnt
      ttHourStat.StartStamp = ldAbsHour[liCnt].
END.

liQty = 0.

FOR EACH Invoice NO-LOCK USE-INDEX InvDate WHERE
         Invoice.Brand = "1" AND
         Invoice.InvDate = idaInvDate AND
         Invoice.InvType = iiInvType AND
         Invoice.ChgStamp > 0:

    oiInvQty = oiInvQty + 1.

    IF llCDRs THEN DO:
       liCnt = 0.
       FOR EACH InvRow OF Invoice NO-LOCK WHERE
                InvRow.RowType = 2:
           liCnt = liCnt + InvRow.Qty.
       END.
    END.
      
    fSplitTS(Invoice.ChgStamp,
             OUTPUT ldaDate,
             OUTPUT liTime).
    
    FIND LAST ttHourStat WHERE 
              ttHourStat.StartStamp <= Invoice.ChgStamp NO-ERROR.
    IF NOT AVAILABLE ttHourStat THEN DO:
       IF NOT SESSION:BATCH THEN 
          MESSAGE "Check stamp:" Invoice.ExtInvId
                  Invoice.ChgStamp
          VIEW-AS ALERT-BOX.
       NEXT.
    END.

    liSubs = 0.
    FOR EACH SubInvoice OF Invoice NO-LOCK:
       liSubs = liSubs + 1.
    END.
    
    ASSIGN
       ttHourStat.InvQty = ttHourStat.InvQty + 1
       ttHourStat.CDRQty = ttHourStat.CDRQty + liCnt
       ttHourStat.SubQty = ttHourStat.SubQty + liSubs
       liHour        = ttHourStat.ttHour.

    FIND FIRST ttBillRun WHERE 
               ttBillRun.ttDay   = DAY(ldaDate) AND
               ttBillRun.ttHour  = liHour AND
               ttBillRun.BillRun = Invoice.BillRun NO-ERROR.
    IF NOT AVAILABLE ttBillRun THEN DO:
       CREATE ttBillRun.
       ASSIGN 
          ttBillRun.ttDay   = DAY(ldaDate)
          ttBillRun.ttHour  = liHour
          ttBillRun.BillRun = Invoice.BillRun.
    END.
    ASSIGN
       ttBillRun.InvQty = ttBillRun.InvQty + 1
       ttBillRun.CDRQty = ttBillRun.CDRQty + liCnt
       ttBillRun.SubQty = ttBillRun.SubQty + liSubs.
    
    ASSIGN
       liCDRQty = liCDRQty + liCnt
       liSubQty = liSubQty + liSubs.

    liQty = liQty + 1.
    IF NOT SESSION:BATCH AND liQty MOD 1000 = 0 THEN DO:
       PAUSE 0.
       DISP liQty LABEL "2. Invoices" WITH 1 DOWN OVERLAY FRAME fQty2.
    END.
    
END.

IF NOT SESSION:BATCH THEN DO:
   HIDE FRAME fQty1 NO-PAUSE. 
   HIDE FRAME fQty2 NO-PAUSE.
END.


OUTPUT STREAM sLog TO VALUE(lcFile).
UNIX SILENT VALUE("chmod 644 " + lcFile + ">/dev/null").

FOR EACH ttHourStat:
   
   IF ttHourStat.InvQty = 0 THEN NEXT. 
    
   DISPLAY STREAM sLog 
      ttHourStat.ttHour
      ttHourStat.InvQty
      ttHourStat.CDRQty FORMAT ">>>,>>>,>>9".
   
   liCnt = 0.
   FOR EACH ttBillRun WHERE           
            ttBillRun.ttDay  = ttHourStat.ttDay AND
            ttBillRun.ttHour = ttHourStat.ttHour:
      liCnt = liCnt + 1.
   END.
   
   DISP STREAM sLog 
      ttHourStat.CDRQty / ttHourStat.InvQty 
         COLUMN-LABEL "CDRs/inv"
         FORMAT ">>,>>>,>>9"
      liCnt COLUMN-LABEL "Runs"
      ttHourStat.InvQty / liCnt 
         COLUMN-LABEL "Inv./run"
         FORMAT ">>>>>>9"
      ttHourStat.CDRQty / liCnt             
         COLUMN-LABEL "CDRs/run"
         FORMAT ">>>,>>>,>>9".

   ASSIGN 
      liTotInv = liTotInv + ttHourStat.InvQty
      liTotCDR = liTotCDR + ttHourStat.CDRQty
      liTotSub = liTotSub + ttHourStat.SubQty.
      
   CREATE ttCumulative.
   ASSIGN 
      ttCumulative.ttDay  = ttHourStat.ttDay
      ttCumulative.StartStamp = ttHourStat.StartStamp
      ttCumulative.ttHour = ttHourStat.ttHour
      ttCumulative.InvQty = liTotInv
      ttCumulative.CDRQty = liTotCDR
      ttCumulative.SubQty = liTotSub.
END.


ASSIGN
   ldtStart = fTimeStamp2DateTime(ldStart)
   ldtEnd   = fTimeStamp2DateTime(ldEnd)
   liDur    = INTERVAL(ldtEnd,ldtStart,"seconds")
   liDays   = TRUNCATE(liDur / 86400,0)
   liDur    = liDur MOD 86400
   lcDur    = STRING(liDur,"hh:mm:ss")
   liDur    = INTEGER(ENTRY(1,lcDur,":"))
   liDur    = liDur + liDays * 24
   lcDur    = STRING(liDur,"99") + ":" +
              ENTRY(2,lcDur,":") + ":" + 
              ENTRY(3,lcDur,":").

PUT STREAM sLog UNFORMATTED
   SKIP(1)
   "Invoice qty: "      TRIM(STRING(oiInvQty,">>>,>>>,>>9")) SKIP 
   "Subscription qty: " TRIM(STRING(liSubQty,">>>,>>>,>>9")) SKIP
   "CDR qty: "          TRIM(STRING(liCDRQty,">>>,>>>,>>9")) SKIP
   "Duration: "         lcDur
   SKIP(1).

FOR EACH ttCumulative:
   DISP STREAM sLog 
      ttCumulative.ttHour  COLUMN-LABEL "Hour" FORMAT ">9"
      fts2hms(ttCumulative.StartStamp) 
         COLUMN-LABEL "Started"
         FORMAT "x(19)"
      ttCumulative.InvQty  
         COLUMN-LABEL "Cumul. Inv.Qty"
         FORMAT ">>>,>>>,>>9"
      ttCumulative.SubQty  
         COLUMN-LABEL "Cumul. Subscr.Qty"
         FORMAT ">>>,>>>,>>9"
      ttCumulative.CDRQty   
         COLUMN-LABEL "Cumul. CDR Qty"
         FORMAT ">,>>>,>>>,>>9".
END.      
                                  
OUTPUT STREAM sLog CLOSE.


