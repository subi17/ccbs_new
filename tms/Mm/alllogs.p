/*-----------------------------------------------------------------------------
  MODULE .......: countermenu.p
  FUNCTION .....: 
  SOVELLUTUS ...: 
  AUTHOR .......: 
  CREATED ......: 
  changePVM ....: 17.01.06/aam solog here, 'sent text log' removed 
                  20.09.06/tk  m2mmsseq -> m2mmobsub
                  13.03.07 kl  ICC log (mslog) commented

  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT  PARAMETER pimsseq  AS INT .

DEF VAR menuc      AS C  EXTENT 4                     NO-UNDO.
DEF VAR lcUserName AS CHAR                            NO-UNDO FORMAT "X(30)".
DEF VAR lcCLi AS CHAR NO-UNDO.
DEF VAR liAgrCust AS INT NO-UNDO.

PAUSE 0.

FIND MobSub WHERE 
     MobSub.Msseq = pimsseq NO-LOCK NO-ERROR.
IF AVAIL MobSub THEN ASSIGN
   lcCLi = MobSub.Cli
   liAgrCust = MobSub.AgrCust.
ELSE DO:
   FIND TermMobSub WHERE 
        TermMobSub.Msseq = pimsseq NO-LOCK NO-ERROR.
   IF AVAIL TermMobSub THEN ASSIGN
      lcCLi = TermMobSub.Cli
      liAgrCust = TermMobSub.AgrCust.
   ELSE DO:
      MESSAGE "MobSub/TermMobsub" pimsseq "not found!" VIEW-AS ALERT-BOX ERROR.
      RETURN.
   END.
END.

FIND Customer where 
     Customer.CustNum = liAgrCust no-lock no-error.

IF Avail Customer THEN lcUserName =  DYNAMIC-FUNCTION("fDispCustName" IN
                                             ghFunc1, BUFFER Customer).
ELSE lcUserName = "".

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
 
 DISPLAY
 "A) Subscription EventLog       "  @ menuc[1]    SKIP
 "B) Service Order LOG (HLR)     "  @ menuc[2]    SKIP
 "C) MNP Messages                "  @ menuc[3]    SKIP
 "D) Sent SMS Log                "  @ menuc[4] 
   WITH OVERLAY WIDTH 40 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  lcCLI + " " + lcUsername 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mm/evbrmob.p(pimsseq).
   END.

   ELSE IF FRAME-INDEX = 2 THEN DO :
      RUN Mm/solog2.p(pimsSeq).
   END.

   ELSE IF FRAME-INDEX = 3 THEN DO:
      FIND FIRST Order WHERE
                 Order.MsSeq = piMsSeq AND
                 Order.MNPStatus > 0 NO-LOCK NO-ERROR.   
      IF NOT AVAIL Order THEN 
         MESSAGE "Order not found!" VIEW-AS ALERT-BOX ERROR.
      ELSE RUN Mnp/mnpbr.p(Order.OrderId,0,0).
   END.
   
   ELSE IF FRAME-INDEX EQ 4 THEN DO:   
      RUN Mm/callalarm.p(0, lccli).
   END.
   ELSE IF FRAME-INDEX > 4 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.
