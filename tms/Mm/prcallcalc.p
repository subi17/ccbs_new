/*-----------------------------------------------------------------------------
  MODULE .......: prcallcalc.p
  FUNCTION .....: 
  SOVELLUTUS ...: 
  AUTHOR .......: vk
  CREATED ......: 03.07.07 by vk This is based on the module callmenu.p, but
                                 this is used with a CLI of prepaid type.  
  changePVM ....:
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/tmsconst.i}

DEF INPUT  PARAMETER msseq  AS INT .

DEF VAR menuc      AS C  EXTENT 5                    NO-UNDO.
DEF VAR inv-rep    AS LO FORMAT "Invoiced/Reported"   NO-UNDO.
DEF VAR ok         AS LO                              NO-UNDO.
DEF VAR lcUserName AS CHAR                            NO-UNDO FORMAT "X(30)".
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
     Customer.CustNum = lhSub::CustNum no-lock no-error.

IF Avail Customer THEN lcUserName =  DYNAMIC-FUNCTION("fDispCustName" IN
                                             ghFunc1, BUFFER Customer).
ELSE lcUserName = "".

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey. 
 
 DISPLAY
 "A) Calls Browse                "  @ menuc[1]    SKIP
 "B) Calls Value                 "  @ menuc[2]    SKIP
 "C) Billing Permission          "  @ menuc[3]    SKIP
 "D) EDR Browse                  "  @ menuc[4]    SKIP
 "E) NRTRDE Browse               "  @ menuc[5]    SKIP
 
   WITH OVERLAY WIDTH 40 FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " +  lhSub::CLI + " " + lcUsername 
   CENTERED WITH COL 1 ROW 3.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX = 1 THEN DO:
       /* Show the prepaid calls of this cli. */
       RUN Mm/mobcallm.p(lhSub::cli).
   END.   

   ELSE IF FRAME-INDEX = 2 THEN DO:
      /* Sum the prepaid calls of this cli and show the current balance. */
      RUN Mm/msisdni2(lhSub::MsSeq).
   END.
         
   ELSE IF FRAME-INDEX = 3 THEN DO:
      RUN Mc/limit.p(lhSub::InvCust, lhSub::Msseq, {&LIMIT_TYPE_BILLPERM}, 0).
   END.
   
   ELSE IF FRAME-INDEX = 4 THEN DO:
      RUN Mm/edrm.p(lhSub::cli).
   END.   
   
   ELSE IF FRAME-INDEX = 5 THEN DO:
      RUN Mm/fraudm.p(lhSub::cli).
   END.   

   ELSE IF FRAME-INDEX > 5 OR FRAME-INDEX = 0 THEN LEAVE.

END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.
