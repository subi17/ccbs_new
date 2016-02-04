{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
{Func/timestamp.i}
{Func/barrfunc.i}

def buffer MsRequest2 for MsRequest.
def stream slog.
def var lcBarring AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcError AS CHARACTER NO-UNDO.
output stream slog to /apps/snet/200911/as_yot318.log append.

DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEFINE VARIABLE j AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcBarrComList AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcBarrStatus AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeTime AS DECIMAL NO-UNDO.

ldeTime = fMakeTS().

FOR EACH mobsub where
   mobsub.brand  = gcBrand and
   mobsub.clitype = "tarj3" NO-LOCK:

   i = i + 1.
   if i <= 3 then next.
/*   if i mod 750 = 0 then do:
      j = j + 1.
*/
      ldeTime = fSecOffSet(fMakeTS(), 5 * i).
/*   end. */
/*   lcBarring = fCheckStatus(MobSub.MsSeq). */
   run checkmsbarring(mobsub.msseq,
      "anttis",
      output lcBarrComList,
      output lcBarrStatus).

   lcError = "".
   IF lcBarrStatus EQ "Y_SARC" THEN DO:
      lcError = "OK: Y_SARC is allready set".
   END.
   ELSE IF lcBarrStatus = "ONC" THEN DO:
      lcError = "ERROR: ONGOING NW Command".
   END.
   ELSE IF lcBarrStatus = "NAD" THEN DO:
      lcError = "ERROR: Not allowed".
   END.
   ELSE DO:


       RUN Mm/barrengine (MobSub.MsSeq,
                   "Y_SARC",
                   "5",                 /* source  */
                   "YOT-317",           /* creator */
                   ldetime,           /* activate */
                   "",                  /* sms */
                   OUTPUT lcError).

   END.

   put stream slog unformatted 
      mobsub.msseq " " 
      mobsub.cli " " 
      lcBarrStatus " "
      fts2hms(ldeTime) " " 
      lcError skip.
   
END.

output stream slog close.
