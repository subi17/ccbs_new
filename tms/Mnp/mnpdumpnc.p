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


DEFINE TEMP-TABLE ttMnpDetails
   FIELD PortRequest AS CHAR
   FIELD CCode AS CHAR
   FIELD UpdateTS AS DEC
   FIELD StatusReason AS CHAR
   FIELD Portingtime AS DEC
   FIELD Msisdns AS CHAR
   FIELD ReceptorNrn AS CHAR
   FIELD FormRequest AS CHAR
   FIELD DonorCode AS CHAR
   FIELD ReceptorCode AS CHAR
   FIELD MNPSeq AS INT
   FIELD OrderId AS INT
   FIELD MsSeqLst AS CHAR
   FIELD ICCLst AS CHAR
   INDEX MNPSeq IS PRIMARY UNIQUE MNPSeq.

FIND FIRST DumpFile WHERE DumpFile.DumpID = iiDumpID NO-LOCK NO-ERROR.
IF AVAILABLE DumpFile THEN DO:
   lcPickType = DumpFile.ConfigParam.
   lcDelimiter = DumpFile.DumpDelimiter.
END.

FUNCTION fCollectMNPDetails RETURNS LOGICAL
(iiType AS INT, lccode AS CHAR, lcmsisdns AS CHAR,
 lcMsSeqList AS CHAR, lcICCList AS CHAR):

   FIND MNPDetails WHERE
        MNPDetails.MNPSeq = MNPProcess.MNPSeq NO-LOCK.

   IF NOT CAN-FIND( FIRST ttMnpDetails NO-LOCK WHERE
         ttMnpDetails.MNPSeq = MNPDetails.MNPSeq) THEN DO:
      CREATE ttMNPDetails.
      ASSIGN
         ttMNPDetails.PortRequest = mnpprocess.portrequest
         ttMNPDetails.CCode = lccode
         ttMNPDetails.UpdateTS = mnpprocess.updatets
         ttMNPDetails.StatusReason = mnpprocess.statusreason
         ttMNPDetails.Portingtime = mnpprocess.portingtime
         ttMNPDetails.Msisdns = SUBSTRING(lcmsisdns,1,LENGTH(lcmsisdns) - 1)
         ttMNPDetails.ReceptorNrn = MNPDetails.receptornrn
         ttMNPDetails.FormRequest = mnpprocess.formrequest
         ttMNPDetails.DonorCode = MNPDetails.donorcode
         ttMNPDetails.ReceptorCode = MNPDetails.receptorcode
         ttMNPDetails.MNPSeq = MNPDetails.MNPSeq
         ttMNPDetails.MsSeqLst = lcMsSeqList
         ttMNPDetails.ICCLst = lcICCList.
      IF iiType = {&MNP_TYPE_IN} THEN
         ttMNPDetails.OrderId = MNPProcess.OrderId.
   END.
END FUNCTION.

FUNCTION fPrintMNPDump RETURNS LOGICAL
(iiType AS INTEGER):
   FOR EACH ttMNPDetails NO-LOCK:
      PUT STREAM sdump UNFORMATTED
         ttMNPDetails.PortRequest lcDelimiter
         ttMNPDetails.CCode lcDelimiter
         fts2hms(ttMNPDetails.UpdateTS) lcDelimiter
         ttMNPDetails.StatusReason lcDelimiter
         fts2hms(ttMNPDetails.Portingtime) lcDelimiter
         ttMNPDetails.Msisdns lcDelimiter
         ttMNPDetails.ReceptorNrn lcDelimiter
         ttMNPDetails.FormRequest lcDelimiter
         ttMNPDetails.DonorCode lcDelimiter
         ttMNPDetails.ReceptorCode lcDelimiter
         STRING(ttMNPDetails.MNPSeq) lcDelimiter.
      IF iiType = {&MNP_TYPE_IN} THEN
         PUT STREAM sdump UNFORMATTED
         STRING(ttMNPDetails.OrderId) lcDelimiter.
      PUT STREAM sdump UNFORMATTED 
         ttMNPDetails.MsSeqLst lcDelimiter
         ttMNPDetails.ICCLst SKIP.
   END.
END FUNCTION.


FUNCTION fCreateMNPDump RETURNS LOGICAL
(iiType AS INTEGER):

   DEF VAR ldDumpFile AS CHARACTER NO-UNDO.
   DEF VAR lcCode AS CHARACTER NO-UNDO.
   DEF VAR lcmsisdns AS CHARACTER NO-UNDO.
   DEF VAR lcMSSeqs AS CHARACTER NO-UNDO.
   DEF VAR lcICCs AS CHARACTER NO-UNDO.
   DEF VAR lcDumpfile AS CHARACTER NO-UNDO.
   DEF VAR lcDumpfileOut AS CHARACTER NO-UNDO.
   DEF VAR lcMsSeqList AS CHAR NO-UNDO.
   DEF VAR lcICCList AS CHAR NO-UNDO.
   
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
         ASSIGN
            lcMsSeqList = SUBSTRING(lcMsSeqs,1,LENGTH(lcMsSeqs) - 1)
            lcICCList = SUBSTRING(lcICCs,1,LENGTH(lcICCs) - 1).
         fCollectMNPDetails(iiType, lcCode, lcmsisdns, lcMsSeqList, lcICCList).
      END.
      ELSE IF iiType = {&MNP_TYPE_TERMINATION} THEN DO:
         put stream sdump unformatted 
            mnpprocess.portrequest lcDelimiter
            lccode lcDelimiter
            fts2hms(mnpprocess.createdts) lcDelimiter
            fts2hms(mnpprocess.updatets) lcDelimiter
            SUBSTRING(lcmsisdns,1,LENGTH(lcmsisdns) - 1) lcDelimiter
            mnpprocess.mnpseq SKIP.
      END.
      oiEvents = oiEvents + 1.
   END.
   IF iiType = {&MNP_TYPE_IN} OR iiType = {&MNP_TYPE_OUT} THEN
      fPrintMNPDump(iiType).

   OUTPUT STREAM sdump close.
   
END FUNCTION. 

/* MAIN PROGRAM START */
fCreateMNPDump(INT(lcPickType)).

/* MAIN PROGRAM ENDS */
