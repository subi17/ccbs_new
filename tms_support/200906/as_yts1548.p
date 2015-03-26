DEFINE TEMP-TABLE ttReport
FIELD gsmbnr AS CHAR 
FIELD clitype AS CHAR 
FIELD kpl AS INT
FIELD wrong_amount AS DEC
FIELD correct_amount AS DEC
FIELD total_duration AS DEC
INDEX i IS PRIMARY UNIQUE clitype gsmbnr.

DEFINE VARIABLE ldePrice AS DEC NO-UNDO. 
ldePrice = 0.12 / 60.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
def frame a.
FOR EACH mobcdr where
         mobcdr.datest >= 9/1/2008 AND
         mobcdr.datest <= 5/31/2009 AND
         mobcdr.clitype = "cont4" NO-LOCK:
   
   i = i + 1.
   if i mod 10000 = 0 then disp i mobcdr.datest with frame a.
   pause 0.

   if 
      mobcdr.rateccn = 81 and
      mobcdr.ccn = 9 and
      LOOKUP(mobcdr.gsmbnr,"060,061,062,080,090,091,092,1415") > 0 then do:
   
      FIND FIRST InvSeq NO-LOCK WHERE
                 InvSeq.InvSeq = MobCDR.invSeq NO-ERROR.
      IF AVAIL InvSeq AND InvSeq.Billed = TRUE THEN DO:

         FIND ttReport WHERE
            ttReport.clitype = mobcdr.clitype and
            ttReport.gsmbnr  = mobcdr.gsmbnr EXCLUSIVE-LOCK NO-ERROR.
         IF NOT AVAIL ttReport THEN DO:
             CREATE ttReport.
             ASSIGN
               ttReport.CLIType = mobcdr.cliType
               ttReport.gsmbnr = mobcdr.gsmbnr.
         END.
         ASSIGN
            ttReport.kpl = ttReport.kpl + 1
            ttReport.wrong_amount = ttReport.wrong_amount + mobcdr.amount
            ttReport.correct_amount = ttReport.correct_amount + 
               (mobcdr.billdur * ldePrice) + 0.12
            ttReport.total_duration = ttReport.total_duration + mobcdr.billdur.
      END.
   END.
END.

def stream sout.
output stream sout to /apps/snet/200906/as_yts1548_cont4.log.

FOR EACH ttReport NO-LOCK:

   put stream sout unformatted 
      ttReport.CLIType "|"
      ttReport.gsmbnr  "|"
      ttReport.kpl  "|"
      ttReport.total_duration "|"
      ttReport.wrong_amount   "|"
      ttReport.correct_amount skip.

END.
