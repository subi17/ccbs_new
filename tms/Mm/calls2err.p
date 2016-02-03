~/* ===========================================================================
 MODULE ........: NotInvoicableCalls
 APPLICATION ...: Ticket Master
 TASK ..........: 
 CREATED .......: 
 CHANGED .......: 
                  
 VERSION .......: 
 ============================================================================*/
         
{Syst/commali.i} 
{Func/timestamp.i}
{Rate/error_codes.i}

DEF STREAM msg.
         
DEF VAR cdate1             AS DA  NO-UNDO.
DEF VAR cdate2             AS DA  NO-UNDO.
DEF VAR ok                 AS LO  NO-UNDO FORMAT "Yes/No".
DEF VAR liQty              AS INT NO-UNDO.
DEF VAR bbatch             AS LO  NO-UNDO.
DEF VAR lcStart            AS CHAR NO-UNDO.
DEF VAR lcEnd              AS CHAR NO-UNDO.

DEF BUFFER xxCall  FOR Mobcdr.

/* Default starts values */
ASSIGN 
  cdate2 = today - 35
  cdate1 = DATE(MONTH(cdate2),1,YEAR(CDATE2)).
  cdate2 =  DATE(MONTH(cdate1 + 45 ),1,
                  YEAR(cdate1 + 45)) - 1.


form
skip(1)
"  This program will move all error code tickets    " SKIP
"  from billing according to the chosen period.     " SKIP
"  Moved calls will not be automatically processed. " SKIP(1)

"  Call dates .....:" cdate1 FORMAT "99-99-99"
HELP "Earliest call date" "-" cdate2 FORMAT "99-99-99"
HELP "Latest call date"
VALIDATE(INPUT cdate1 <= INPUT cdate2,"Invalid order of dates !")        

 WITH  
   OVERLAY 
   ROW 3 
   WIDTH 60   CENTERED
   COLOR VALUE(cfc) 
   TITLE COLOR VALUE(ctc) 
    " " + ynimi + " MOVE TICKETS TO UNINVOICABLE   " + 
    string(pvm,"99.99.99") + " "  NO-LABELS  FRAME main.
   
PAUSE 0.


MAIN:
REPEAT WITH FRAME main:

IF NOT bbatch THEN DO:
   ehto = 9. RUN ufkey.
    
 DISPLAY
 cdate1 cdate2 with frame main.
    
    UPDATE 
     cdate1   cdate2
        
WITH FRAME main  EDITING:
      READKEY.
            
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME main:
         PAUSE 0.
         
         if frame-field = "ig-code" then do:
         end.

      END.    
      APPLY LASTKEY.
   END. /* EDITING */
        
   ACTION:
   REPEAT WITH FRAME main:
      ASSIGN
      ufk = 0 ehto = 0
      ufk[1] = 7 
      ufk[5] = 795
      ufk[8] = 8.
      RUN ufkey.
      
      IF toimi = 1 THEN NEXT  main.
      IF toimi = 8 THEN LEAVE main.
      IF TOIMI = 5 THEN DO:
         ok = false.
         MESSAGE "Do You REALLY want to MOVE tickets to error (Y/N)?" UPDATE ok.
         IF NOT ok THEN NEXT action.
         ELSE LEAVE action.
      END.
   END. /* Action */      
END. /* bbatch */

lcStart = fTS2HMS(fMakeTS()).

FOR EACH MobError NO-LOCK WHERE 
         MobERror.MobError >     0 .

   IF MobError.MobError > 8000 AND 
      MobError.MobError < 8050 THEN NEXT.

   PUT SCREEN ROW 1 STRING(MobError.MobError).

   FOR EACH MobCDR USE-INDEX ErrorCode NO-LOCK WHERE   
            Mobcdr.ErrorCode = MobError.MobError AND
            MobCDR.datest  >= cdate1             AND
            MobCDR.datest  <= cdate2:
                 
         FIND   xxCall WHERE 
          RECID(xxCall) = RECID(MobCDR) EXCLUSIVE-LOCK NO-ERROR.

         liQty = liQty + 1.             
         
         ASSIGN 
            xxCall.xsub      = STRING(Mobcdr.ErrorCode)
            xxCall.ErrorCode = {&CDR_ERROR_NON_INVOICEABLE_CALL}
            xxCall.Invseq    = 0. 

         IF liQty mod 50 = 0 THEN put screen row 2 string(liQty).
   END.

END.

DO FOR ActionLog TRANS:

   lcEnd = fTS2HMS(fMakeTS()).
    
   CREATE ActionLog.
   
   ASSIGN
      ActionLog.ActionTS     = fMakeTS()
      ActionLog.Brand        = gcBrand
      ActionLog.TableName    = "MobCDR"
      ActionLog.KeyValue     = STRING(YEAR(cDate2),"9999") + 
                               STRING(MONTH(cDate2),"99") + 
                               STRING(DAY(cDate2),"99")
      ActionLog.UserCode     = katun
      ActionLog.ActionID     = "ERRORCLEAN"
      ActionLog.ActionPeriod = YEAR(cDate2) * 100 + 
                               MONTH(cDate2)
      ActionLog.FromDate     = cDate1
      ActionLog.ToDate       = cDate2
      ActionLog.ActionStatus = 2
      ActionLog.ActionDec    = liQty
      ActionLog.ActionChar   = "Started: " + lcStart + CHR(10) +
                               "Ended: " + lcEnd + CHR(10) +
                               "Moved: " + STRING(liQty).
   RELEASE ActionLog.   
END.


LEAVE.

END.        

