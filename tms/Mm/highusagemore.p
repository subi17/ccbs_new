/*-----------------------------------------------------------------------------
  MODULE .......: highusagemore.p
  FUNCTION .....: More highusage
  SOVELLUTUS ...: 
  AUTHOR .......: 
  CREATED ......: jp
  changePVM ....:
                 
                 
                 
  Version ......: M15
  SHARED .......: INPUT: msseq
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
DEF INPUT PARAMETER iinvseq  AS INT  NO-UNDO.
DEF INPUT PARAMETER icli     AS CHAR NO-UNDO.

DEF  NEW  shared VAR siirto AS CHAR.
DEF VAR username as c no-undo.
DEF VAR lcEmail  AS C NO-UNDO.
FIND FIRST     HighUsage WHERE 
               HighUsage.Invseq = iinvseq AND 
               HighUsage.cli    = icli no-lock no-error.


FIND FIRST msowner where msowner.cli = HighUsage.cli no-lock no-error.


DEF VAR menuc   AS C  EXTENT 6                   NO-UNDO.
DEF VAR inv-rep AS LO FORMAT "Invoiced/Reported" NO-UNDO.
DEF VAR ok      AS LO                            NO-UNDO.
PAUSE 0.



DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey.p. 
 
 DISPLAY
 "A) Total call amount per day            "  @ menuc[1]    SKIP 
 "B) Total call amount per product        "  @ menuc[2]    SKIP 
 "C) Browse call                          "  @ menuc[3]    SKIP 
 "D) High usage history                   "  @ menuc[4]    SKIP 
 "E) Print High usage report              "  @ menuc[5]    SKIP 
 "X) QUIT          "                         @ menuc[6]   SKIP

   WITH OVERLAY WIDTH 48 FRAME choices NO-LABELS centered.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  highusage.CLI 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mm/callstat.p(INPUT highusage.invseq,highusage.cli,"DATE").
   END.

   ELSE IF FRAME-INDEX = 2 THEN DO:
      RUN Mm/callstat.p(INPUT highusage.invseq,highusage.cli,"PRODUCT").

   END.

   ELSE IF FRAME-INDEX = 3 THEN DO:
      RUN Mm/mobcallis.p(highusage.cli,highusage.invseq).
   
   END.             

   ELSE IF FRAME-INDEX = 4 THEN DO:
      RUN Mm/highusagehist.p(highusage.cli).
   END.
   
   ELSE IF FRAME-INDEX = 5 THEN DO:
      lcEmail = "".
      FIND first tmsuser WHERE 
                 tmsuser.UserCode = katun NO-LOCK NO-ERROR.

      if avail tmsuser and 
               tmsuser.email ne "" THEN ASSIGN 
      lcemail = tmsuser.email .

      MESSAGE 
      "Select Status code of High usage?"
      VIEW-AS ALERT-BOX TITLE "STATUS".

      RUN Help/h-tmscodes.p(INPUT "HighUsage",   /* TableName*/
                           "HiUsageStatus", /* FieldName */
                           "HighUsage",   /* GroupCode */
                           OUTPUT siirto).

      
      
      
      RUN Mm/highusagerep.p(INPUT fMake2Dt(INPUT today - 90, INPUT 0),   
                             lcEmail,
                             int(siirto)).
   END.
   
    
   ELSE IF FRAME-INDEX = 6 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



