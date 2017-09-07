{Syst/tmsconst.i}

DEFINE VARIABLE llSimulate AS LOGICAL NO-UNDO. 
llSimulate = true.

DEFINE VARIABLE lcLine AS CHARACTER NO-UNDO. 

DEFINE VARIABLE lcDbName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcSeqName AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liSeqVal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liNewSeqVal AS INTEGER NO-UNDO. 

DEFINE VARIABLE lhQuery      AS HANDLE    NO-UNDO.
DEFINE VARIABLE lhBuffer     AS HANDLE    NO-UNDO.

DEF VAR lcHostname AS CHAR NO-UNDO.
INPUT THROUGH hostname.
IMPORT lcHostName.
INPUT CLOSE.

IF LOOKUP(lcHostName,SUBST("&1,&2",
          {&HOSTNAME_STAGING},{&HOSTNAME_DEVEL})) = 0 THEN DO:
   MESSAGE 'This script is not allowed to run in'
   lcHostName VIEW-AS ALERT-BOX.
   RETURN.
END.


def stream sin.
input stream sin from ../tms_support/utilities/tabledump/sequences.txt.

def stream sout.
output stream sout to ../tms_support/utilities/tabledump/initialize_sequences.log.

CREATE QUERY lhQuery.

put stream sout unformatted 
   "DB|SEQNAME|LOCAL_VALUE|PROD_VALUE|NEW_VALUE" skip.

repeat trans:

   import stream sin unformatted lcLine.

   assign
     lcDbName = entry(1,lcLine,";")
     lcSeqName = entry(2,lcLine,";")
     liSeqVal = int(entry(3,lcLine,";")).

   if liSeqVal eq 0 then  next.

   IF NOT VALID-HANDLE(lhBuffer) OR
      lhBuffer:DBNAME NE lcDbName THEN DO:
   
      IF VALID-HANDLE(lhBuffer) THEN
         DELETE OBJECT lhBuffer.

      CREATE BUFFER lhBuffer FOR TABLE lcDbName + "._sequence".  
      lhQuery:SET-BUFFERS(lhBuffer).
   END.

   lhQuery:QUERY-PREPARE(SUBST(
      "FOR EACH &1._sequence NO-LOCK WHERE _sequence._seq-name EQ &2",
      lcDbName, QUOTER(lcSeqName))).
   lhQuery:QUERY-OPEN().

   lhQuery:GET-FIRST().
   IF NOT lhQuery:QUERY-OFF-END THEN DO:

      IF dynamic-current-value(lhBuffer::_seq-name, lcDBname) >= liSeqVal then do:

         put stream sout unformatted
            lhBuffer:DBNAME ";"
            lhBuffer::_seq-name ";"
            dynamic-current-value(lhBuffer::_seq-name, lcDBname) ";"
            liSeqVal ";" 
            "SKIPPED" SKIP.
         NEXT.
      end.

      if lhBuffer::_seq-name eq "mobsub" then liNewSeqVal = 50000000.
      else if liSeqVal < 10 then liNewSeqVal = 100.
      else if liSeqVal < 100 then liNewSeqVal = 1000.
      else if liSeqVal < 1000 then liNewSeqVal = 10000.
      else if liSeqVal < 10000 then liNewSeqVal = 100000.
      else if liSeqVal < 100000 then liNewSeqVal = 1000000.
      else if liSeqVal < 1000000 then liNewSeqVal = 10000000.
      else if liSeqVal < 10000000 then liNewSeqVal = 100000000.
      else if liSeqVal < 100000000 then liNewSeqVal = 1000000000.
      Else liNewSeqVal = 1500000000.

      if liNewSeqVal / liSeqVal > 3 THEN liNewSeqVal = liNewSeqVal / 2.

      put stream sout unformatted
         lhBuffer:DBNAME ";"
         lhBuffer::_seq-name ";"
         dynamic-current-value(lhBuffer::_seq-name, lcDBname) ";"
         liSeqVal ";" 
         liNewSeqVal
         skip.

      if not llSimulate then 
         dynamic-current-value(lhBuffer::_seq-name, lcDBname) = liNewSeqVal.
   END.
end.
input stream sin close.
if valid-handle (lhBuffer ) then delete object lhBuffer.
if valid-handle (lhQuery ) then delete object lhQuery.
