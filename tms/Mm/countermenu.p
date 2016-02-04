/*-----------------------------------------------------------------------------
  MODULE .......: countermenu.p
  FUNCTION .....: 
  SOVELLUTUS ...: 
  AUTHOR .......: 
  CREATED ......: 
  changePVM ....:
  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT  PARAMETER iiMsSeq  AS INT .

DEF VAR menuc      AS C  EXTENT 3                     NO-UNDO.
DEF VAR ok         AS LO                              NO-UNDO.
DEF VAR lcUserName AS CHAR                            NO-UNDO FORMAT "X(30)".
DEF VAR liCustnum AS INT NO-UNDO. 

PAUSE 0.

FIND MobSub  WHERE 
     MobSub.Msseq = iiMsSeq NO-LOCK NO-ERROR.
IF AVAIL MobSub THEN liCustnum = MobSub.Custnum.
ELSE DO:
   FIND TermMobsub WHERE
        TermMobsub.MsSeq = iiMsSeq NO-LOCK NO-ERROR.
   IF NOT AVAIL TermMobsub THEN DO:
      MESSAGE "Subscription" iiMsSeq "not found" VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
   liCustnum = TermMobsub.Custnum.
END.

FIND Customer where 
     Customer.CustNum = liCustnum no-lock no-error.

IF Avail Customer THEN lcUserName =  DYNAMIC-FUNCTION("fDispCustName" IN
                                             ghFunc1, BUFFER Customer).
ELSE lcUserName = "".

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
 
 DISPLAY
 "A) Service Limits              "  @ menuc[1]    SKIP
 "B) General Counters            "  @ menuc[2]    SKIP
 "C) Invoice Row Counters        "  @ menuc[3]    SKIP

   WITH OVERLAY WIDTH 40 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE  lcUsername 
   CENTERED WITH COL 1 ROW 2.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mm/mservicelimit.p(iiMsSeq, 0, 0).
   END.
   ELSE IF FRAME-INDEX = 2 THEN DO:
      RUN Mc/counter.p("MobSub",STRING(iiMsSeq)).
   END.
   ELSE IF FRAME-INDEX = 3 THEN 
      RUN Inv/invrowcounter.p (0,iiMsSeq,0,"").

   ELSE IF FRAME-INDEX > 3 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.


