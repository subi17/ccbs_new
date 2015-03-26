{commali.i}
{date.i}
{cparam2.i}
{ftransdir.i}
{tmsconst.i}


DEF INPUT  PARAMETER iiDumpID      AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFile        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icDumpMode    AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idLastDump    AS DEC  NO-UNDO.
DEF INPUT  PARAMETER icEventSource AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icEventFields AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER oiEvents      AS INT  NO-UNDO.
DEF OUTPUT PARAMETER olInterrupted AS LOG  NO-UNDO.

DEF STREAM sdump.

DEFINE VARIABLE lcPickType AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcDelimiter AS CHARACTER NO-UNDO. 

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcPickType = DumpFile.ConfigParam.
   lcDelimiter = DumpFile.DumpDelimiter.
END.

FUNCTION fCreateMNPDump RETURNS LOGICAL
(iiType AS INTEGER):

   DEFINE VARIABLE ldDumpFile AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcCode AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lcmsisdns AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcMSSeqs AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcICCs AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDumpfile AS CHARACTER NO-UNDO. 
   DEFINE VARIABLE lcDumpfileOut AS CHARACTER NO-UNDO. 
   
   OUTPUT STREAM sdump TO value (icFile).
   
   FOR EACH MNPProcess WHERE
            MNPProcess.Brand = gcBrand AND
            MNPProcess.MNPType = iiType NO-LOCK:
      
      ASSIGN
         lcmsisdns = ""
         lcMSSeqs = ""
         lcICCs = "".

      IF LOOKUP(MNPProcess.PortRequest,"MNP_IN_UNKNOWN,MNP_OUT_UNKNOWN") > 0
         THEN NEXT.
      
      FOR EACH mnpsub where
               mnpsub.mnpseq = mnpprocess.mnpseq NO-LOCK:
         lcmsisdns = lcmsisdns + mnpsub.cli + ",".
         IF iiType = {&MNP_TYPE_IN} OR iiType = {&MNP_TYPE_OUT} THEN ASSIGN
            lcMsSeqs = lcMsSeqs + string(mnpsub.msseq) + ","
            lcICCs = lcICCs + mnpsub.icc + ",".
      END.
     
      /* Convert TMS status to character value */
      FIND TMSCodes WHERE 
           TMSCodes.TableName = "MNPProcess" AND
           TMSCodes.FieldName = "StatusCode" AND
           TMSCodes.CodeGroup = "MNP" AND
           TMSCodes.CodeValue = STRING(MNPProcess.StatusCode) NO-LOCK NO-ERROR.
      IF AVAIL TMSCodes THEN
         lcCode = (IF TMSCodes.CodeName = "AREC_CLOSED" THEN "AREC"
                   ELSE TMSCodes.CodeName).
      ELSE lcCode = STRING(MNPProcess.StatusCode).
      
      IF iiType = {&MNP_TYPE_IN} OR iiType = {&MNP_TYPE_OUT} THEN DO:
      
         FIND MNPDetails WHERE
              MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-LOCK.
         
         put stream sdump unformatted 
            mnpprocess.portrequest lcDelimiter
            lccode lcDelimiter
            fts2hms(mnpprocess.updatets) lcDelimiter
            mnpprocess.statusreason lcDelimiter
            fts2hms(mnpprocess.portingtime) lcDelimiter
            SUBSTRING(lcmsisdns,1,LENGTH(lcmsisdns) - 1) lcDelimiter
            MNPDetails.receptornrn lcDelimiter
            mnpprocess.formrequest lcDelimiter
            MNPDetails.donorcode lcDelimiter
            MNPDetails.receptorcode lcDelimiter.

         IF iiType EQ {&MNP_TYPE_IN} THEN 
            put stream sdump unformatted MNPProces.OrderId lcDelimiter.
        
         put stream sdump unformatted
            SUBSTRING(lcMsSeqs,1,LENGTH(lcMsSeqs) - 1) lcDelimiter
            SUBSTRING(lcICCs,1,LENGTH(lcICCs) - 1) skip.
      END.
      ELSE IF iiType = 3 THEN DO:
         
         put stream sdump unformatted 
            mnpprocess.portrequest lcDelimiter
            lccode lcDelimiter
            fts2hms(mnpprocess.createdts) lcDelimiter
            fts2hms(mnpprocess.updatets) lcDelimiter
            SUBSTRING(lcmsisdns,1,LENGTH(lcmsisdns) - 1) lcDelimiter
            mnpprocess.mnpseq skip.
      END.
      oiEvents = oiEvents + 1.
   END.

   OUTPUT STREAM sdump close.
   
END FUNCTION. 

fCreateMNPDump(INT(lcPickType)).
