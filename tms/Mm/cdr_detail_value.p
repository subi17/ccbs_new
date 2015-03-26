/* cdr_detail_value.p     14.01.10/aam 
   get a value from cdr details 
*/   
  
{commali.i}        
{callquery.i}


DEF INPUT  PARAMETER icCDRTable AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idaDate    AS DATE NO-UNDO.
DEF INPUT  PARAMETER iiDtlSeq   AS INT  NO-UNDO.
DEF INPUT  PARAMETER icField    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocValue    AS CHAR NO-UNDO.

DEF VAR liCnt     AS INT  NO-UNDO.
DEF VAR lcTitle   AS CHAR NO-UNDO.
DEF VAR lcVersion AS CHAR NO-UNDO.
DEF VAR lhDetail  AS HANDLE NO-UNDO.

DEF TEMP-TABLE ttCDRDtl NO-UNDO LIKE MCDRDtl2.


lhDetail = TEMP-TABLE ttCDRDtl:HANDLE.

IF icCDRTable = "" THEN icCDRTable = "MobCDR".

fGetCDRDtl(icCDRTable,
           idaDate,
           iiDtlSeq,
           INPUT-OUTPUT lhDetail).

FIND FIRST ttCDRDtl NO-ERROR.
IF NOT AVAILABLE ttCDRDtl THEN RETURN "ERROR:Details not found".


lcVersion = ttCDRDtl.version.
IF INDEX(lcVersion,"YC") = 0 THEN 
   lcVersion = lcVersion + ENTRY(4,ttCDRDtl.Detail,"|").
 
FIND FIRST CSVHeader WHERE
           CSVHeader.Version = lcVersion NO-LOCK NO-ERROR.
IF NOT AVAILABLE CSVHeader THEN RETURN "ERROR:Unknown version".

DO liCnt = 1 TO NUM-ENTRIES(CSVHeader.CSV,"|"):
      
   ASSIGN
      lcTitle  = ENTRY(liCnt,CSVHeader.CSV,"|")
      lcTitle  = REPLACE(ENTRY(2,lcTitle,"<"),">","")
      lcTitle  = ENTRY(2,lcTitle,"=").
      
   IF lcTitle = icField THEN DO:
      ocValue  = ENTRY(liCnt,ttCDRDtl.Detail,"|") NO-ERROR.
      LEAVE.
   END.

END.

EMPTY TEMP-TABLE ttCDRDtl.

DELETE OBJECT lhDetail NO-ERROR. 
