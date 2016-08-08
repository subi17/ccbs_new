/*-----------------------------------------------------------------------------
  MODULE .......: mobsubmore.p
  FUNCTION .....: More Mobsub
  SOVELLUTUS ...: 
  AUTHOR .......: 
  CREATED ......: 
  changePVM ....: 07.11.03 jp contsig/stvmpwd
                  23.01.04 jp Send sms to subscription
                  23.01.04/aam call specification (mclispec)
                  11.02.04/aam print infotxt (prininfo)
                  17.03.04/tk  input parameter mobsub.custnum for callalarm
                  22.09.04/aam create fat to "M" (order confirmation removed)
                  15.04.05/aam only one parameter to mspnpgrp
                  25.05.05/mvi Changed menu H -> "Supervisor actions" 
                               Details in Copernicus Task 6737
                  05.10.05/aam fonecta (numberinq)
                  04.01.06/aam new parameter to nnasse.p
  Version ......: M15
  SHARED .......: INPUT: msseq
                  OUTPUT Kille
  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT  PARAMETER msseq  AS INT .

DEF VAR menuc      AS C  EXTENT 7                     NO-UNDO.
DEF VAR inv-rep    AS LO FORMAT "Invoiced/Reported"   NO-UNDO.
DEF VAR ok         AS LO                              NO-UNDO.
DEF VAR Fromperiod AS INT                             NO-UNDO FORMAT "999999".
DEF VAR EndPeriod  AS INT                             NO-UNDO FORMAT "999999".
DEF VAR lcUserName AS CHAR                            NO-UNDO FORMAT "X(30)".

PAUSE 0.
FIND MobSub  WHERE 
     MobSub.Msseq = msseq NO-LOCK NO-ERROR.

FIND Customer where 
     Customer.CustNum = MobSub.CustNum no-lock no-error.

IF Avail Customer THEN lcUserName =  DYNAMIC-FUNCTION("fDispCustName" IN
                                             ghFunc1, BUFFER Customer).
ELSE lcUserName = "".

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
 
 DISPLAY
 "A) Agreement Customer          "  @ menuc[1]    SKIP
 "B) Invoice Customer            "  @ menuc[2]    SKIP
 "C) User Customer               "  @ menuc[3]    SKIP(2)
 "D) Change Agreement Customer   "  @ menuc[4]    SKIP
 "E) Change Invoice Customer     "  @ menuc[5]    SKIP
 "F) Change User                 "  @ menuc[6]    SKIP
 "X) QUIT                        "  @ menuc[7]    
 
   WITH OVERLAY WIDTH 40 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  MobSub.CLI + " " + lcUsername 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mc/nnasse.p(Mobsub.AgrCust,
                 "").
   END.

   ELSE IF FRAME-INDEX = 2 THEN DO:
     RUN Mc/nnasse.p(mobsub.InvCust,
                "").
   END.

   ELSE IF FRAME-INDEX = 3 THEN DO:
      RUN Mc/nnasse.p(mobsub.Custnum,
                 "").
   END.             
  
   ELSE IF FRAME-INDEX > 3 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



