/*-----------------------------------------------------------------------------
  MODULE .......: PERSONDATA.P
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
                  11.01.06/aam chgmscust.p
                  24.02.06/aam chgmsowner.p
                  14.03.06/aam ownerreq.p
                  27.11.06/aam invoice customer handling not allowed
                  06.03.07 kl  IF CAN-FIND for cases 1 and 3
                  19.03.07 kl  If no MobSub try TermMobSub

  Version ......: M15
  SHARED .......: INPUT: msseq
                  OUTPUT Kille
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/lib/accesslog.i}

DEF INPUT  PARAMETER msseq  AS INT .

DEF VAR menuc       AS C  EXTENT 8                     NO-UNDO.
DEF VAR inv-rep     AS LO FORMAT "Invoiced/Reported"   NO-UNDO.
DEF VAR ok          AS LO                              NO-UNDO.
DEF VAR Fromperiod  AS INT                             NO-UNDO FORMAT "999999".
DEF VAR EndPeriod   AS INT                             NO-UNDO FORMAT "999999".
DEF VAR lcUserName  AS CHAR                            NO-UNDO FORMAT "X(30)".
DEF VAR lcPassword  AS CHAR                            NO-UNDO. 
DEF VAR lcAskPassWd AS CHAR                            NO-UNDO.
DEF VAR liCustNum   AS INT                             NO-UNDO.
DEF VAR liInvCust   AS INT                             NO-UNDO.
DEF VAR liAgrCust   AS INT                             NO-UNDO.
DEF VAR lcCLI       AS CHAR                            NO-UNDO.
DEF VAR lcProgram   AS CHAR                            NO-UNDO.

lcProgram  = PROGRAM-NAME(1).
lcPassword = fCParamC("TestUser").

IF lcPassword = ? THEN lcPassword = "".

PAUSE 0.
FIND FIRST MobSub  WHERE 
           MobSub.Msseq = msseq
NO-LOCK NO-ERROR.

IF AVAIL MobSub THEN ASSIGN
   liCustNum = MobSub.CustNum
   liInvCust = MobSub.InvCust
   liAgrCust = MobSub.AgrCust
   lcCLI     = MobSub.CLI.
ELSE DO:
   FIND FIRST TermMobSub  WHERE 
              TermMobSub.Msseq = msseq
   NO-LOCK NO-ERROR.
   ASSIGN
      liCustNum = TermMobSub.CustNum
      liInvCust = TermMobSub.InvCust
      liAgrCust = TermMobSub.AgrCust
      lcCLI     = TermMobSub.CLI.
END.

FIND FIRST Customer where 
           Customer.CustNum = liCustNum
NO-LOCK NO-ERROR.

IF Avail Customer THEN lcUserName = Func.Common:mDispCustName(BUFFER Customer).
ELSE lcUserName = "".

DO WHILE TRUE:
   ASSIGN Syst.Var:ufk = 0 Syst.Var:ufk[8] = 8 Syst.Var:ehto = 3. RUN Syst/ufkey.p. 
 
 DISPLAY
 "A) Agreement Customer          "  @ menuc[1]    SKIP
 "B) Invoice Customer            "  @ menuc[2]    SKIP
 "C) User Customer               "  @ menuc[3]    SKIP(1)
 "D) Agreement Customer Change   "  @ menuc[4]    SKIP
 "E) * Not in use *              "  @ menuc[5]    SKIP
 "F) Change User                 "  @ menuc[6]    SKIP(1)
 "G) Agr.Customer Change Requests"  @ menuc[7]    SKIP
 "X) QUIT                        "  @ menuc[8]    
 
   WITH OVERLAY WIDTH 40 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  lcCLI + " " + lcUsername 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      IF CAN-FIND(FIRST Customer WHERE
                        Customer.CustNum = liAgrCust) THEN DO:
         RUN CreateReadAccess("Customer", Syst.Var:katun, liAgrCust, lcProgram, "CustNum" ).
         RUN Mc/nnasse.p(liAgrCust,"").
      END.
      ELSE 
         MESSAGE
            "Agreement customer " + STRING(liAgrCust) + " does not exist!"
         VIEW-AS ALERT-BOX ERROR.
   END.

   ELSE IF FRAME-INDEX = 2 THEN DO:
   
      IF liInvCust NE liAgrCust THEN 
         MESSAGE "Not in use"
         VIEW-AS ALERT-BOX INFORMATION.

      ELSE DO:
         IF CAN-FIND(FIRST Customer WHERE
                           Customer.CustNum = liInvCust) THEN DO:
            RUN CreateReadAccess("Customer", Syst.Var:katun, liInvCust, lcProgram, "CustNum" ).
            RUN Mc/nnasse.p(liInvCust,"").
         END.
         ELSE 
            MESSAGE
               "Invoice customer " + STRING(liAgrCust) + " does not exist!"
            VIEW-AS ALERT-BOX ERROR.
      END.
   END.
   
   ELSE IF FRAME-INDEX = 3 THEN DO:
      IF CAN-FIND(FIRST Customer WHERE
                        Customer.CustNum = liCustNum) THEN DO:
         RUN CreateReadAccess("Customer", Syst.Var:katun, liCustNum, lcProgram, "CustNum" ).
         RUN Mc/nnasse.p(liCustNum,"").
      END.
      ELSE 
         MESSAGE
            "User customer " + STRING(liCustNum) + " does not exist!"
         VIEW-AS ALERT-BOX ERROR.
   END.             

   ELSE IF FRAME-INDEX = 4 THEN DO:
      
      RUN Mm/chgmsowner.p(MobSub.MsSeq,
                     0,
                     "new").
   END. 
   
   ELSE IF FRAME-INDEX = 5 THEN DO:
     MESSAGE "Not in use!"
     VIEW-AS ALERT-BOX INFORMATION.
   END.
 
   ELSE IF FRAME-INDEX = 6 THEN DO:
      MESSAGE "Feature under construction!" VIEW-AS ALERT-BOX INFORMATION.
   END.

   ELSE IF FRAME-INDEX = 7 THEN DO:
      RUN Mm/ownerreq.p(MobSub.MsSeq,0,?).
   END. 
    
   ELSE IF FRAME-INDEX = 8 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.


