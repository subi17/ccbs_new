/*-----------------------------------------------------------------------------
  MODULE .......: mobsubsudo.p
  FUNCTION .....: More Mobsub -> Supervisor actions Menu
  SOVELLUTUS ...: 
  AUTHOR .......: mvi
  CREATED ......: 
  changePVM ....: 25.05.05 mvi/created
                  
                  
  Version ......: M15
  SHARED .......: INPUT: msseq
  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT  PARAMETER msseq  AS INT .

DEF VAR menuc      AS C  EXTENT 2                   NO-UNDO.
DEF VAR inv-rep    AS LO FORMAT "Invoiced/Reported" NO-UNDO.
DEF VAR ok         AS LO                            NO-UNDO.
DEF VAR lcUserName AS CHAR                          NO-UNDO.
PAUSE 0.
FIND MobSub  WHERE MobSub.Msseq = msseq NO-LOCK NO-ERROR.

FIND Customer where Customer.CustNum = MobSub.AgrCust no-lock no-error.

IF Avail Customer THEN lcUserName =  DYNAMIC-FUNCTION("fDispCustName" IN
                                     ghFunc1, BUFFER Customer).
ELSE                    lcUserName = "".
                                     

DO WHILE TRUE:
   ASSIGN  ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
 DISPLAY
 "A) Change status to subscription            "  @ menuc[1]   SKIP
 "X) QUIT                                     "  @ menuc[2]   SKIP


   WITH OVERLAY WIDTH 50 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  MobSub.CLI + " " + lcUserName 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX = 1 THEN DO:
     RUN Mm/msstatch(MobSub.msseq).
   END.

   ELSE IF FRAME-INDEX = 2 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



