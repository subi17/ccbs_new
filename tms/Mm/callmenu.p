/*-----------------------------------------------------------------------------
  MODULE .......: callmenu.p
  FUNCTION .....: 
  SOVELLUTUS ...: 
  AUTHOR .......: 
  CREATED ......: 
  changePVM ....:
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER msseq  AS INT .

DEF VAR menuc      AS CHAR EXTENT 8 NO-UNDO.
DEF VAR lcUserName AS CHAR NO-UNDO FORMAT "X(30)".
DEF VAR lhSub AS HANDLE NO-UNDO. 

PAUSE 0.

FIND MobSub  WHERE 
     MobSub.Msseq = msseq NO-LOCK NO-ERROR.

IF AVAIL MobSub THEN lhSub = BUFFER MobSub:HANDLE.
ELSE DO: 
   FIND TermMobSub WHERE 
        TermMobSub.Msseq = msseq NO-LOCK NO-ERROR.
   IF NOT AVAIL TermMobSub THEN RETURN.
   lhSub = BUFFER TermMobSub:HANDLE.
END.

FIND Customer where 
     Customer.CustNum = lhSub::Custnum no-lock no-error.

IF Avail Customer THEN lcUserName =  DYNAMIC-FUNCTION("fDispCustName" IN
                                             ghFunc1, BUFFER Customer).
ELSE lcUserName = "".

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
 
 DISPLAY
 "A) Calls Browse                "                    @ menuc[1] SKIP
 "B) Calls Value                 "  WHEN AVAIL MobSub @ menuc[2] SKIP
 "C) Subscription Fixed Fees     "  WHEN AVAIL MobSub @ menuc[3] SKIP
 "D) Billing Permission          "                    @ menuc[4] SKIP
 "E) Invoice Targets             "  WHEN AVAIL MobSub @ menuc[5] SKIP
 "F) Minimum Consumption         "                    @ menuc[6] SKIP
 "G) EDR Browse                  "                    @ menuc[7] SKIP
 "H) NRTRDE Browse               "                    @ menuc[8] SKIP
 
   WITH OVERLAY WIDTH 40 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  lhSub::CLI + " " + lcUsername 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mm/mobcallm.p(lhSub::cli).
   END.   

   ELSE IF FRAME-INDEX = 2 THEN DO:
      RUN Mm/msisdniv.p(Mobsub.MsSeq).
   END.

   ELSE IF FRAME-INDEX = 3 THEN DO:
      RUN Mc/nncuco.p(Mobsub.InvCust,STRING(Mobsub.MsSeq)).
   END.
   
   ELSE IF FRAME-INDEX = 4 THEN DO:
      RUN Mc/limit.p(lhSub::InvCust, Msseq, {&LIMIT_TYPE_BILLPERM}, 0).
   END.
   
   ELSE IF FRAME-INDEX = 5 THEN DO:
      RUN Mc/invoicetarget.p(0, MobSub.Msseq).
   END.

   ELSE IF FRAME-INDEX = 6 THEN DO:
      RUN Inv/minconsumption.p(lhSub::Msseq).
   END.
   
   ELSE IF FRAME-INDEX = 7 THEN DO:
      RUN Mm/edrm.p(lhSub::cli).
   END.
   
   ELSE IF FRAME-INDEX = 8 THEN DO:
      RUN Mm/fraudm.p(lhSub::cli).
   END.
             
   ELSE IF FRAME-INDEX > 8 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.

