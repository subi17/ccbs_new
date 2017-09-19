/* The script will try to get the most recent mobsub as per clitype
   and will output customer list from those mobsubs. There might
   be duplicated custnums in file /tmp/custlist_{&FIRSTCHECK}_first.txt
   so manual processing is needed. For example:
   sort /tmp/custlist_{&FIRSTCHECK}_first.txt | uniq > /tmp/sorted.txt
*/    
   
/*   
   You can change to first check from TermMobSub by changing the
   following preprocess values to other way around.
*/ 
&GLOBAL-DEFINE FIRSTCHECK MobSub
&GLOBAL-DEFINE THENCHECK TermMobSub

DEFINE STREAM strout.
DEFINE STREAM stddebug.

OUTPUT STREAM strout TO VALUE("/tmp/custlist_{&FIRSTCHECK}_first.txt").
OUTPUT STREAM stddebug TO VALUE("/tmp/debuglist_{&FIRSTCHECK}_first.txt").

DEFINE VARIABLE lii       AS INTEGER NO-UNDO.
DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE ttCLIType NO-UNDO
   FIELD CLIType   AS CHARACTER
   FIELD CustNum   AS INTEGER
   FIELD Processed AS LOGICAL INITIAL FALSE
   INDEX CLIType CLIType Processed
   INDEX Processed Processed CLIType.

FOR EACH CLIType NO-LOCK:
   CREATE ttCLIType.
   ASSIGN
      ttCLIType.CLIType = CLIType.CLIType.
END.

FOR EACH {&FIRSTCHECK} NO-LOCK USE-INDEX MSSeq BY {&FIRSTCHECK}.MSSeq DESCENDING:
   lii = lii + 1.
   FOR ttCLIType WHERE
      ttCLIType.CLItype   = {&FIRSTCHECK}.CLIType AND
      ttCLIType.Processed = NO:
      ASSIGN
         ttCLIType.Processed = YES
         ttCLIType.CustNum   = {&FIRSTCHECK}.CustNum.
   END.
   IF lii > 1000000 THEN LEAVE. 
END.

lii = 0.
FOR EACH {&THENCHECK} NO-LOCK USE-INDEX MSSeq BY {&THENCHECK}.MSSeq DESCENDING:
   lii = lii + 1.
   FOR ttCLIType WHERE
      ttCLIType.CLItype   = {&THENCHECK}.CLIType AND
      ttCLIType.Processed = NO:
      ASSIGN
         ttCLIType.Processed = YES
         ttCLIType.CustNum   = {&THENCHECK}.CustNum.
   END.
   IF lii > 1000000 THEN LEAVE. 
END.

FOR EACH ttCLIType WHERE ttCLIType.Processed = NO:
   lii = 0.
   FOR EACH {&FIRSTCHECK} NO-LOCK WHERE
      {&FIRSTCHECK}.Brand   = "1" AND
      {&FIRSTCHECK}.CLIType = ttCLIType.CLIType:
      IF lii < {&FIRSTCHECK}.MSSeq
      THEN ASSIGN
               liCustNum = {&FIRSTCHECK}.CustNum
               lii = {&FIRSTCHECK}.MSSeq.
   END. 
   IF lii > 0
   THEN ASSIGN
          ttCLIType.Processed = YES
          ttCLIType.CustNum   = liCustNum.
END.

FOR EACH ttCLIType WHERE ttCLIType.Processed = NO:
   lii = 0.
   FOR EACH {&THENCHECK} NO-LOCK WHERE
      {&THENCHECK}.Brand   = "1" AND
      {&THENCHECK}.CLIType = ttCLIType.CLIType:
      IF lii < {&THENCHECK}.MSSeq
      THEN ASSIGN
               liCustNum = {&THENCHECK}.CustNum
               lii = {&THENCHECK}.MSSeq.
   END. 
   IF lii > 0
   THEN ASSIGN
          ttCLIType.Processed = YES
          ttCLIType.CustNum   = liCustNum.
END.

FOR EACH ttCLIType:
   IF ttCLIType.CustNum > 0
   THEN PUT STREAM strout UNFORMATTED ttCLIType.Custnum SKIP.  

   PUT STREAM stddebug UNFORMATTED ttCLIType.Processed " " ttCLIType.CLItype " " ttCLIType.Custnum SKIP.
END.

OUTPUT STREAM strout CLOSE.
OUTPUT STREAM stddebug CLOSE.
