/* ----------------------------------------------------------------------
  MODULE .......: Mobguard.p
  TASK .........: 
  APPLICATION ..: 
  AUTHOR .......: JP
  CREATED ......: 13.11.2005
  CHANGED ......: 
  Version ......: 
  ---------------------------------------------------------------------- */


{Syst/commali.i}

DEF VAR ReasonC        AS CHAR NO-UNDO.
DEF VAR ReasonT        AS CHAR NO-UNDO FORMAT "X(60)" .
DEF NEW SHARED VAR  Siirto AS CHAR NO-UNDO.

FORMAT
"Enter the text/code for browsing the detailded information of the call." SKIP(1)
 
"NOTICE that your user information, date and time of browsing, "   SKIP 
"A-Number and B-Number are stored to EventLog Database."           SKIP(1)   
"Reason Codes:"                                                    SKIP
" 1 - TroubleShooting "                                            SKIP
" 2 - Authority requests"                                          SKIP
" 3 - Invoicing "                                                  SKIP
" 4 - Other reason   "                                             SKIP(1)      

"Reason Code" ReasonC format "9"  Tmscode.codename FORMAT "X(35)"  SKIP
"Reason Text" ReasonT format "x(60)"                     

WITH CENTERED ROW 2 COLOR VALUE(cfc) TITLE 
"BROWSE CDRS" OVERLAY side-label no-label FRAME reason .


RUN local-Show-record.

IF reasonc > "0" THEN DO:

   hide frame reason no-pause.

   CREATE eventlog.
   ASSIGN
      eventlog.eventdate  = TODAY
      eventlog.eventtime  = STRING(TIME,"HH:MM:SS")
      eventlog.usercode   = katun
      eventlog.action     = 'Check'.
 
   ASSIGN
      eventlog.KEY        =  reasonT + chr(255) + 
                             STRING(TODAY,"99-99-9999") + chr(255) + 
                             string(Time, "hh:mm:ss")
      eventlog.tablename      = "MobCDR"
      eventlog.FieldFormats   = "Char"
      eventlog.DataValues     =  "".
                                   
END.

run mobcall.



PROCEDURE local-Show-record:
   REPEAT ON ENDKEY UNDO, LEAVE:

      DISP
      WITH FRAME reason.
      UPDATE
          ReasonC
          reasonT
      WITH FRAME reason
      EDITING:
             READKEY.
             IF FRAME-FIELD = "ReasonC" AND keylabel(lastkey) = "F9" 
             THEN DO:
                RUN h-tmscodes(INPUT "FixCDR",  /* TableName*/
                                     "ReasonCode", /* FieldName */
                                     "ReasonCode", /* GroupCode */
                               OUTPUT siirto).
                 ASSIGN ReasonC = siirto.
                 find first TMSCodes WHERE
                              TMSCodes.TableName = "FixCDR"       AND
                              TMSCodes.FieldName = "ReasonCode"   AND
                              TMSCodes.CodeGroup = "ReasonCode"   AND
                              TMSCodes.CodeValue =  reasonC
                 no-lock no-error.
                 IF avail tmscodes then do:
                    disp reasonc tmscodes.codename with frame reason. 
                     reasont = tmscodes.codename.
                    IF INPUT  FRAME Reason  reasonC NE "4" THEN LEAVE.
                 ENd.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME reason:
                PAUSE 0.
                IF FRAME-FIELD = "ReasonC" THEN DO:
                   find first TMSCodes WHERE
                              TMSCodes.TableName = "FixCDR"  AND
                              TMSCodes.FieldName = "ReasonCode" AND
                              TMSCodes.CodeGroup = "ReasonCode"   AND
                              TMSCodes.CodeValue = INPUT  FRAME Reason  reasonC
                   no-lock no-error.
                   if not available TMSCodes then do:
                      MESSAGE 
                         "Unknown Reason  Code" 
                         input  FRAME reason reasonC       
                         VIEW-AS ALERT-BOX.
                      NEXT-PROMPT reasonC. NEXT.
                   END.
                   disp tmscodes.codename with frame reason.
                   reasont = tmscodes.codename.
                   IF INPUT  FRAME Reason  reasonC NE "4" THEN 
                   LEAVE.
                END.
                IF FRAME-FIELD = "ReasonT" THEN DO:
                    IF INPUT ReaSonT = "" THEN DO:
                       BELL.
                       MESSAGE 
                       "Must be some reason?".
                       NEXT-PROMPT reasont. NEXT.
                    END.
                    Assign ReasonT.

                ENd.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.


