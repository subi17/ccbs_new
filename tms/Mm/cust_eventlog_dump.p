/* ----------------------------------------------------------------------
  MODULE .......: cust_eventlog_dump.p
  TASK .........: daily dump file for customers event log information
  APPLICATION ..: TMS
  AUTHOR .......: ivvekov
  CREATED ......: 20.08.14
  CHANGED ......: 
  Version ......: yoigo
---------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/dumpfile_run.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF VAR lcNumeric    AS CHAR       NO-UNDO.
DEF VAR lcDel        AS CHAR       NO-UNDO.
DEF VAR lcModFields  AS CHAR       NO-UNDO.
DEF VAR liAmtMod     AS INT        NO-UNDO.
DEF VAR lcModified   AS CHAR       NO-UNDO.
DEF VAR lc255        AS CHAR       NO-UNDO.
DEF VAR lcCustNum    AS CHAR       NO-UNDO.

DEF STREAM sFile.

FORM 
   oiEvents AT 2 LABEL "Collecting" FORMAT ">>>>>>>>9"
   WITH OVERLAY ROW 10 CENTERED TITLE " Customer's events " 
        FRAME fPassTime.

FUNCTION fCollectEvent RETURNS LOGICAL
   (INPUT icCustNum AS CHAR):

   ASSIGN lcModified = "".

   IF EventLog.Action EQ "Modify" THEN
      DO liAmtMod = 1 TO NUM-ENTRIES(EventLog.DataValues,lc255) BY 3:

         lcModified = lcModified + ENTRY(liAmtMod,EventLog.DataValues,lc255)     + ":"  +
                                   ENTRY(liAmtMod + 1,EventLog.DataValues,lc255) + ":"  +
                                   ENTRY(liAmtMod + 2,EventLog.DataValues,lc255).
      
        IF liAmtMod + 2 < NUM-ENTRIES(EventLog.DataValues,lc255) THEN
           lcModified = lcModified + "<>".
   END.

   PUT STREAM sFile UNFORMATTED 
      Eventlog.usercode               + lcDel +
      Eventlog.TableName              + lcDel +
      Eventlog.action                 + lcDel +
      STRING(Eventlog.eventdate)      + lcDel +
      Eventlog.eventtime              + lcDel +
      icCustNum                       + lcDel +
      lcModified                        SKIP.

   oiEvents = oiEvents + 1.
   PAUSE 0.
   DISPLAY oiEvents WITH FRAME fPassTime. 

   RETURN TRUE. 

END FUNCTION.    

lcNumeric = SESSION:NUMERIC-FORMAT.

FIND FIRST DumpFile WHERE DumpFile.DumpID = icDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   ASSIGN lcDel        = fInitDelimiter(DumpFile.DumpDelimiter)
          lcModFields  = DumpFile.EventLogFields.
   
   IF DumpFile.DecimalPoint = "." 
   THEN SESSION:NUMERIC-FORMAT = "AMERICAN".
   ELSE SESSION:NUMERIC-FORMAT = "EUROPEAN".
END.
ELSE DO:
   ASSIGN 
      lcDel = CHR(9)
      SESSION:NUMERIC-FORMAT = "AMERICAN".
END.

OUTPUT STREAM sFile TO VALUE(icFile).

ASSIGN lc255 = CHR(255).

FOR EACH Eventlog NO-LOCK WHERE 
         EventLog.EventDate = TODAY - 1:

   lcCustNum = "".

   CASE EventLog.TableName:
      WHEN "Customer" THEN DO:
         lcCustNum = EventLog.Key.
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "Payment" THEN DO:
         lcCustNum = ENTRY(1,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "CustIntEvent" THEN DO:
         lcCustNum = ENTRY(1,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "Fatime" THEN DO:
         lcCustNum = ENTRY(2,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "FixedFee" THEN DO:
         lcCustNum = ENTRY(1,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "Memo" THEN DO:
         lcCustNum = ENTRY(3,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "SingleFee" THEN DO:
         lcCustNum = ENTRY(1,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "BillTarget" THEN DO:
         lcCustNum = ENTRY(1,EventLog.Key,CHR(255)).
         IF lcCustNum NE "" THEN fCollectEvent(lcCustNum).
      END.
      WHEN "Invoice" THEN DO:
         FIND FIRST Invoice NO-LOCK WHERE
                    Invoice.InvNum = INT(EventLog.Key) 
                    NO-ERROR.
         IF AVAILABLE Invoice THEN fCollectEvent(STRING(Invoice.CustNum)).
      END.
      WHEN "CgMember" THEN DO:
         FIND FIRST CgMember NO-LOCK WHERE
                    CgMember.CustGroup = ENTRY(1,EventLog.Key,CHR(255)) AND 
                    CgMember.CustNum   = INT(ENTRY(2,EventLog.Key,CHR(255)))
                    NO-ERROR.
         IF AVAILABLE CgMember THEN fCollectEvent(STRING(CgMember.CustNum)).
      END.
      WHEN "FFItem" THEN DO:
         FIND FIRST FixedFee NO-LOCK WHERE
                    FixedFee.FFNum = INT(ENTRY(1,EventLog.Key,CHR(255)))
                    NO-ERROR.
         IF AVAILABLE FixedFee THEN fCollectEvent(STRING(FixedFee.CustNum)).
      END.
      WHEN "PaymPlan" THEN DO:
         FIND FIRST PaymPlan NO-LOCK WHERE
                    PaymPlan.PPlanID = INT(EventLog.Key) 
                    NO-ERROR.
         IF AVAILABLE PaymPlan THEN fCollectEvent(STRING(PaymPlan.CustNum)).
      END.
      WHEN "MsOwner" THEN DO:
         FIND FIRST MsOwner NO-LOCK WHERE
                    MsOwner.CLI    = ENTRY(1,EventLog.Key,CHR(255)) AND 
                    MsOwner.TsEnd  = DECIMAL(ENTRY(2,EventLog.Key,CHR(255)))
                    NO-ERROR.
         IF AVAILABLE MsOwner THEN fCollectEvent(STRING(MsOwner.CustNum)).
      END.
      WHEN "MSISDN" THEN DO:
         FIND FIRST MSISDN NO-LOCK WHERE
                    MSISDN.CLI = EventLog.Key
                    NO-ERROR.
         IF AVAILABLE MSISDN THEN fCollectEvent(STRING(MSISDN.CustNum)).
      END.
      WHEN "IMSI" THEN DO:
         FIND FIRST IMSI NO-LOCK WHERE
                    IMSI.IMSI = EventLog.Key
                    NO-ERROR.
         IF AVAILABLE IMSI THEN fCollectEvent(STRING(IMSI.CustNum)).
      END.
      WHEN "SIM" THEN DO:
         FIND FIRST SIM NO-LOCK WHERE
                    SIM.ICC = EventLog.Key
                    NO-ERROR.
         IF AVAILABLE SIM THEN fCollectEvent(STRING(SIM.CustNum)).
      END.
      WHEN "MobSub" THEN DO:
         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.MsSeq = INT(EventLog.Key)
                    NO-ERROR.
         IF AVAILABLE MobSub THEN fCollectEvent(STRING(MobSub.CustNum)).
         ELSE DO:
            FIND FIRST TermMobSub NO-LOCK WHERE
                       TermMobSub.MsSeq = INT(EventLog.Key)
                       NO-ERROR.
            IF AVAILABLE TermMobSub THEN fCollectEvent(STRING(TermMobSub.CustNum)).
         END.
      END.
      WHEN "SubSer" THEN DO:
         FIND FIRST MobSub NO-LOCK WHERE
                    MobSub.MsSeq = INT(EventLog.Key)
                    NO-ERROR.
         IF AVAILABLE MobSub THEN fCollectEvent(STRING(MobSub.CustNum)).
      END.
   END.

END. /* FOR EACH Eventlog NO-LOCK WHERE */

IF NOT SESSION:BATCH THEN 
   HIDE FRAME fQty NO-PAUSE.
 
OUTPUT STREAM sFile CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.
