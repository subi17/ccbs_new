/* -----------------------------------------------------------------
  MODULE .......: mobsubrep.p
  TASK .........: Printing criteria for mobsub report
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.09.03
  CHANGED ......: 
  Version ......:
  ------------------------------------------------------------------ */

{Syst/commali.i}  
{Mm/mobsub1.i}

DEF VAR InvGroup  LIKE Customer.InvGroup NO-UNDO.
DEF VAR CustNum1  AS I   no-undo format "zzzzzz9".
DEF VAR CustNum2  AS I   no-undo format "zzzzzz9".
DEF VAR CLIType   LIKE MobSub.CLIType NO-UNDO.
DEF VAR MSStatus  AS I FORMAT "z9" NO-UNDO init 0.
DEF VAR actdate1  AS DA  NO-UNDO.
DEF VAR actdate2  AS DA  NO-UNDO.
DEF VAR details   AS LOG NO-UNDO.

DEF VAR CliName LIKE CLIType.CliName NO-UNDO.
DEF VAR StatName AS CHAR FORMAT "x(20)" NO-UNDO.

form
   skip(1)
"         This program prints out a mobile subscription report." skip
"         Report will contain subscriptions activated in given period." skip
skip(3)
"                Invoicing Group ......:" InvGroup 
HELP "Invoicing group, empty for ALL" SKIP
"                Customers ............:" CustNum1 "-" CustNum2 SKIP
"                Subscription type ....:" CLIType 
HELP "Subscription Type, empty for ALL"  CliName SKip
"                Subscription status ..:" MSStatus
HELP "Subscription Status, 0 for all" StatName SKIP
"                Activation period ....:" actdate1 format "99-99-99"
"-" actdate2 format "99-99-99" SKIP
"                Detailed information .:" details SKIP
SKIP(5)
WITH
   width 80 OVERLAY COLOR value(cfc) TITLE COLOR value(ctc)
   " " + ynimi + " SUBSCRIPTION REPORT " + string(pvm,"99-99-99") + " "
   NO-LABELS FRAME start.

ASSIGN 
   custnum1  = 0
   custnum2  = 9999999
   actdate2 = date(month(TODAY),1,year(TODAY)) - 1
   actdate1 = date(month(actdate2),1,year(actdate2)).


cfc = "sel". RUN Syst/ufcolor.p.

CRIT:
repeat WITH FRAME start:
   ehto = 9. RUN Syst/ufkey.p.

   UPDATE
      InvGroup
      CustNum1
      CustNum2
      CLIType
      MSStatus
      actdate1   validate(actdate1 ne ?,"First Date missing")
      actdate2   validate(input actdate2 >= input actdate1,"Invalid order !")
      details
   WITH FRAME start EDITING.
      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
         PAUSE 0.
         if frame-field = "CustNum1" THEN DO:
            ASSIGN FRAME start CustNum1.
            IF CustNum1 = 0 THEN disp 9999999 @ CustNum2.
            
         END.
         if frame-field = "CustNum2" THEN DO:
            IF INPUT CustNum2 = 0 THEN DISP CustNum1 @ CustNum2.
            ELSE IF INPUT CustNum2 < CustNum1 THEN DO:
               MESSAGE "Wrong Order !".
               NEXT.
            END.
         END.
         if frame-field = "CLIType" THEN DO:
            ASSIGN CLIType.
            IF CLIType NE "" THEN DO:
               FIND CLIType WHERE 
                    Clitype.Brand   = gcBrand AND 
                    CLIType.CLIType = CLIType NO-LOCK NO-ERROR.
               IF NOT AVAIL CLIType THEN DO:
                  MESSAGE "Unknown CLIType !".
                  NEXT.
            
               END.
               ELSE disp CLIType.CliName @ CLIName.
            END.
            ELSE DISP "ALL" @ CLIName.
         END.
         if FRAME-FIELD = "MSStatus" THEN DO:
            ASSIGN MSStatus.
            IF MSStatus = 0 THEN disp "ALL" @ StatName.
            ELSE IF MsStatus > num-entries(stnames) - 1 THEN DO:
               MESSAGE "Unknown status !".
               next.
            END.
            ELSE disp entry(msstatus + 1,stnames) @ StatName.
         END.
      END.
      APPLY LASTKEY.
   END. /* EDITING */

task:
   repeat WITH FRAME start:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN Syst/ufkey.p.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         RUN Mm/mobsubreppr.p(
            InvGroup,
            CustNum1,
            CustNum2,
            CLIType, 
            MSStatus,
            actdate1,
            actdate2,
            details). 

         MESSAGE "Printing complete !" VIEW-AS ALERT-BOX.   
            
         NEXT CRIT.
      END.
   END.
END.
HIDE FRAME start no-pause.

