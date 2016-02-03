/* ----------------------------------------------------------------------
MODULE .......: stc_counter.p
TASK .........: Counts STC/BTC/manual contract termiantions
                for the 1st day of the month
APPLICATION ..: TMS
AUTHOR .......: anttis 
CREATED ......: 3.9.2012
CHANGED ......:
Version ......: Yoigo
----------------------------------------------------------------------- */

{Syst/commpaa.i}
{Func/timestamp.i}
ASSIGN
   katun = "Qvantel"
   gcBrand = "1".

DEFINE VARIABLE ldadate AS DATE FORMAT "99.99.9999" NO-UNDO.
DEF VAR ufkey AS LOG NO-UNDO INIT TRUE.
DEFINE VARIABLE i AS INTEGER NO-UNDO. 
DEF VAR lcDetails AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcTypes AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liTypeLoop AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcStatuses AS CHARACTER NO-UNDO. 
DEFINE VARIABLE ldeDate AS DEC NO-UNDO. 
DEFINE VARIABLE lcTypeDesc AS CHARACTER NO-UNDO. 
DEFINE VARIABLE liStatus AS INTEGER NO-UNDO. 
DEFINE VARIABLE liStatusLoop AS INTEGER NO-UNDO. 
DEFINE VARIABLE liType AS INT NO-UNDO. 
DEFINE VARIABLE liTotal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liDone AS INTEGER NO-UNDO. 
DEFINE VARIABLE liErrors AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCancelled AS INTEGER NO-UNDO. 
DEFINE VARIABLE liDoneTotal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liErrorsTotal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liCancelledTotal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liOngoingTotal AS INTEGER NO-UNDO. 
DEFINE VARIABLE liOngoing AS INTEGER NO-UNDO. 
DEFINE VARIABLE llSTC AS LOGICAL NO-UNDO INIT TRUE.
DEFINE VARIABLE llBTC AS LOGICAL NO-UNDO INIT TRUE.
DEFINE VARIABLE llTerms AS LOGICAL NO-UNDO INIT TRUE.

ldaDate = DATE(MONTH(TODAY),1,YEAR(TODAY)).

FORM
    "Date:" ldaDate i skip
    "STC:" llSTC "BTC:" llBTC "Term:" llTerms skip(1)
    lcDetails FORMAT "X(65)" VIEW-AS EDITOR Size 60 BY 12 skip
    SKIP
WITH OVERLAY ROW 2 centered
    TITLE " STC/BTC/Term. Counter "  
    NO-LABELS 
    FRAME lis.
RUN pUserInput.

IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
KEYLABEL(lastkey) = "F4" OR ldaDate EQ ? THEN QUIT.

lcTypes = "0,81,9". 
lcStatuses = "0,1,2,3,4,5,6,7,8". 
lcTypeDesc = "---STC---,---BTC---,---Manual contract terminations---".

ehto = 4. run ufkey.

LOOPPI:
DO liTypeLoop = 1 TO NUM-ENTRIES(lcTypes) WITH FRAME lis:
   
   ASSIGN
      liTotal = 0
      liDone = 0 
      liErrors = 0
      liCancelled = 0
      liOngoing = 0
      liType = INT(ENTRY(liTypeLoop,lcTypes)).

   if litype = 0 and not llSTC THEN NEXT.
   else if litype = 81 and not llBTC THEN NEXT.
   else if litype = 9 and not llTerms THEN NEXT.

   ASSIGN
      lcDetails = lcDetails  + 
                 ENTRY(liTypeLoop,lcTypeDesc) + CHR(10) + "Requests:"
      ldeDate = (IF liType = 9 THEN fMake2Dt(ldaDate - 1,86399)
                 ELSE fMake2Dt(ldaDate,0)).
   
   DO liStatusLoop = 1 TO num-entries(lcStatuses):

      liStatus = int(entry(liStatusLoop,lcStatuses)).
      i = 0.

      lcDetails = lcDetails + " " + STRING(liStatus) +  ":".
      DISP lcDetails WITH FRAME lis.

      FOR EACH msrequest NO-LOCK where
               msrequest.brand = gcBrand and
               msrequest.reqtype = liType and
               msrequest.reqstatus = liStatus and
               msrequest.actstamp = ldeDate :
         if msrequest.reqtype = 9 and
            msrequest.crestamp > msrequest.actstamp then next.
         i = i + 1.
         if i mod 100 = 0 then do:
            DISP i WITH FRAME lis.
         end.
      end.
      
      IF liStatus EQ 2 THEN liDone = liDone + i.
      ELSE IF liStatus EQ 3 THEN liErrors = liErrors + i.
      ELSE IF liStatus EQ 4 THEN liCancelled = liCancelled + i.
      ELSE liOngoing = liOngoing + i.

      lcDetails = lcDetails + STRING(i).
      DISP lcDetails WITH FRAME lis.
   end.

   assign
      liDoneTotal = liDoneTotal + liDone
      liOngoingTotal = liOngoingTotal + liOngoing
      liCancelledTotal = liCancelledTotal + liCancelled
      liErrorsTotal = liErrorsTotal + liErrors.

   lcDetails = lcDetails + CHR(10) + 
      SUBST(" Done: &1, Ongoing: &2, Cancelled: &3, Errors: &4", liDone, liOngoing, liCancelled, liErrors) +
      CHR(10).
    disp lcDetails with frame lis. 
END.
i = 0.
disp i with frame lis.

lcDetails = lcDetails + CHR(10) +  "---TOTAL---"  + chr(10)  +
   subst("Done: &1, Ongoing: &2, Cancelled: &3, Errors: &4",
         liDoneTotal, liOngoingTotal, liCancelledTotal, liErrorsTotal).
disp lcDetails with frame lis. 

PROCEDURE pUserInput:

   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
   
      DISP ldaDate
         llSTC
         llBTC
         llTerms WITH FRAME lis.

      UPDATE
         ldaDate
         llSTC
         llBTC
         llTerms

      WITH FRAME lis EDITING:
      
         IF ufkey THEN DO:
            ASSIGN ehto = 9. RUN ufkey.p.
            ufkey = false.
         END.

         READKEY.
         
         nap = keylabel(lastkey).

         IF LOOKUP(nap,poisnap) > 0 THEN DO:

         END.

         APPLY LASTKEY.

      END.
      LEAVE.
   END.
END.
