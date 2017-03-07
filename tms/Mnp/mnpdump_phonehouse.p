/* ----------------------------------------------------------------------
  MODULE .......: mnpdump_phonehouse.p
  TASK .........:i XML file with PhoneHouse MNP orders. YOT-1926
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 03.09.12
  CHANGED ......:
  Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}
{Syst/dumpfile_run.i}
{Func/date.i}
{Syst/tmsconst.i}
{Func/cparam2.i}

DEF INPUT  PARAMETER icDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEFINE VARIABLE lcStatusesOngoing AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcStatusesFinal AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE ldeFrom AS DECIMAL NO-UNDO. 

FORM
    oiEvents    AT 2  LABEL "Picked " FORMAT ">>>>>>>9"
WITH SIDE-LABELS 1 DOWN ROW 8 CENTERED OVERLAY
    TITLE " Collecting " FRAME fColl.

ASSIGN
   lcStatusesOngoing = "0,2,4,5"
   lcStatusesFinal = "6,7,8".

DEFINE VARIABLE liLoop AS INTEGER NO-UNDO. 

DEFINE VARIABLE lhSaxWriter AS HANDLE NO-UNDO. 
DEF VAR lhXML AS HANDLE NO-UNDO.

ldeFrom = fOffSet(fMakeTS(),-24).
CREATE SAX-WRITER lhXML.
lhXML:FORMATTED = TRUE.
lhXML:ENCODING = 'UTF-8'.
lhXML:SET-OUTPUT-DESTINATION("FILE",icFile).
lhXML:START-DOCUMENT().
lhXML:START-ELEMENT("xml").

DO liLoop = 1 TO NUM-ENTRIES(lcStatusesOngoing):

   liStatus = INT(ENTRY(liLoop, lcStatusesOngoing)).

   FOR EACH mnpprocess NO-LOCK where
            mnpprocess.brand = gcBrand and
            mnpprocess.mnptype = {&MNP_TYPE_IN} and
            mnpprocess.statuscode = liStatus,
      FIRST Order NO-LOCK USE-INDEX OrderID WHERE
            Order.Brand = gcBrand AND
            Order.Orderid = mnpprocess.orderid ANd
            LOOKUP(Order.orderchannel,{&ORDER_CHANNEL_INDIRECT}) > 0,
      FIRST SalesMan NO-LOCK WHERE
            SalesMan.Brand = gcBrand AND
            SalesMan.SalesMan = Order.SalesMan AND
            SalesMan.Reseller = "PH":
      
      RUN pDumpToXMLFile.

      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.
   END.
END.

DO liLoop = 1 TO NUM-ENTRIES(lcStatusesFinal):

   liStatus = INT(ENTRY(liLoop, lcStatusesFinal)).

   FOR EACH mnpprocess NO-LOCK USE-INDEX updatets WHERE
            mnpprocess.brand = gcBrand and
            mnpprocess.mnptype = {&MNP_TYPE_IN} and
            mnpprocess.updatets > ldeFrom AND
            mnpprocess.statuscode = liStatus,
      FIRST Order NO-LOCK USE-INDEX OrderID WHERE
            Order.Brand = gcBrand AND
            Order.Orderid = mnpprocess.orderid AND
            LOOKUP(Order.orderchannel,{&ORDER_CHANNEL_INDIRECT}) > 0,
      FIRST SalesMan NO-LOCK WHERE
            SalesMan.Brand = gcBrand AND
            SalesMan.SalesMan = Order.SalesMan AND
            SalesMan.Reseller = "PH":
      
      RUN pDumpToXMLFile.
      
      oiEvents = oiEvents + 1.
      IF NOT SESSION:BATCH AND oiEvents MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP oiEvents WITH FRAME fColl.
      END.
   END.
END.

PROCEDURE pDumpToXMLFile:

   DEFINE VARIABLE lcMNPStatus AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE ldaChangeDate AS DATE NO-UNDO. 
   DEFINE VARIABLE lcChangeDate AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE liTime AS INTEGER NO-UNDO. 

   FIND TMSCodes WHERE 
        TMSCodes.TableName = "MNPProcess" AND
        TMSCodes.FieldName = "StatusCode" AND
        TMSCodes.CodeGroup = "MNP" AND
        TMSCodes.CodeValue = STRING(MNPProcess.StatusCode)
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN lcMNPStatus = TMSCodes.CodeName.
   ELSE lcMNPStatus = STRING(MNPProcess.StatusCode).

   lhXML:WRITE-EMPTY-ELEMENT("order").
   lhXML:INSERT-ATTRIBUTE("status", lcMNPStatus).
   lhXML:INSERT-ATTRIBUTE("phonenumber", order.cli).
   IF MNPProcess.Portrequest > "" THEN DO:
      
      fSplitTS(mnpprocess.portingtime,
               OUTPUT ldaChangeDate,
               OUTPUT liTime). 

      lcChangeDate = STRING(YEAR(ldaChangeDate)) + "-" +
                     STRING(MONTH(ldaChangeDate),"99") + "-" + 
                     STRING(DAY(ldaChangeDate),"99").

      lhXML:INSERT-ATTRIBUTE("code", mnpprocess.portrequest).
      lhXML:INSERT-ATTRIBUTE("changedate", lcChangeDate).
   END.

END PROCEDURE. 

lhXML:END-ELEMENT("xml").
lhXML:END-DOCUMENT().

IF VALID-HANDLE(lhSaxWriter) THEN DELETE OBJECT lhSAXWriter.
