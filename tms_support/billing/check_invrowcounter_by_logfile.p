{Syst/commpaa.i}
gcbrand = "1".
{Func/date.i}
{Inv/chk_cdr_invrowcounter.i}

def var i as int no-undo.
def var ldatodate as date no-undo.
def var liMsSeq as int no-undo.
def var lcline as char no-undo.
def var lcinputfile as char no-undo.
def var liCounterQty AS INTEGER NO-UNDO. 
def var lcRunID AS char NO-UNDO. 

def temp-table ttMsSeq no-undo
    FIELD MsSeq AS INT.

def stream sin.

pause 0.
update lcinputfile label "Enter Input File Path:" 
   FORMAT "X(256)" VIEW-AS FILL-IN SIZE 45 BY 1 skip
   with overlay row 10 centered title " CHECK COUNTER " 
        side-labels frame fCheck.
hide frame fCheck no-pause.

IF lcinputfile = "" THEN DO:
   MESSAGE "Please Enter Input File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcinputfile = "" THEN DO: */

assign 
  ldatodate = flastdayofmonth(TODAY).

input stream sin from value(lcinputfile).

repeat:
   import stream sin unformatted lcline.

   if lcline = "" or lcline begins "MSISDN" then next.
   liMsSeq = int(entry(2,lcline, chr(9))) no-error.

   IF error-status:error then do:
      MESSAGE "Cannot parse MsSeq (2nd value):" skip
         lcline VIEW-AS ALERT-BOX.
      RETURN.
   end.

   if not can-find (first ttMsSeq WHERE ttMsSeq.MsSeq = liMsSeq) then do:
      create ttMsSeq.
             ttMsSeq.MsSeq = liMsSeq.
   end.
end.

FOR EACH ttMsSeq,
    first msowner no-lock use-index MsSeq where
          msowner.MsSeq = ttMsSeq.MsSeq:

   i = i + 1.

   CREATE ttSubs.
   ASSIGN
      ttSubs.MsSeq   = msowner.msseq
      ttSubs.InvCust = msowner.custnum
      ttSubs.Order   = i.

   IF NOT SESSION:BATCH THEN DO:
      IF i MOD 100 = 0 THEN DO:
         PAUSE 0.
         DISP MsOwner.CLI MsOwner.MsSeq i
            WITH 1 DOWN.
      END.
   END.

END.

lcRunID = REPLACE(STRING(TIME,"HH:MM:SS"),":","") + "_logfile".
   
RUN Inv/chk_cdr_invrowcounter.p(INPUT TABLE ttSubs BY-REFERENCE,
                            lcRunID, /* fr run id */
                            ldatodate,
                            0, /* fr process id */
                            0, /* fr update interval */
                            OUTPUT liCounterQty).

EMPTY TEMP-TABLE ttMsSeq NO-ERROR.
EMPTY TEMP-TABLE ttSubs NO-ERROR.
