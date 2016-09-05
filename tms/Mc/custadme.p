/*-----------------------------------------------------------------------------
  MODULE .......: custadme
  FUNCTION .....: customer admin actions menu
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam (from commontt.p)
  CREATED ......: 16.12.05
  changePVM ....: 
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.
                                 
DEF VAR menuc     AS CHAR EXTENT 6 FORMAT "X(35)"  NO-UNDO.
DEF VAR liQty     AS INT                           NO-UNDO. 
DEF VAR lcName    AS CHAR                          NO-UNDO. 
DEF VAR liInvType AS INT                           NO-UNDO.

DO FOR Customer:
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
   lcName = STRING(Customer.CustNum) + " " +
            DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                             BUFFER Customer).
END. 

/* invoice type to be created */
liInvType = fCParamI("InvCreType").
IF liInvType = 0 OR liInvType = ? THEN liInvType = 3.

PAUSE 0.

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey.p. 

      DISPLAY

       " A) Private Prices                 "             @ menuc[1] SKIP
       " B) Salesman Contracts             "             @ menuc[2] SKIP
       " C) Information Texts              "             @ menuc[3] SKIP
       " D) External Customer Groups       "             @ menuc[4] SKIP
       " E) Contact Events                 "             @ menuc[5] SKIP
       " X) QUIT   (F8)                    "             @ menuc[6] SKIP
       WITH OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " " + SUBSTR(lcName,1,50) + " "  CENTERED ROW 5.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   ELSE IF FRAME-INDEX = 1 THEN DO:
      RUN Mc/tariff.p(0,0,"",iiCustNum,"",0). 
   END.               

   ELSE IF FRAME-INDEX EQ 2 THEN DO:
      RUN Mc/contract.p(iiCustNum,
                   "").
   END.

   ELSE IF FRAME-INDEX EQ 3 THEN DO:
      RUN Mc/invotxt.p("Customer",iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 4 THEN DO:
      RUN Mc/nncgme2.p(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 5 THEN DO:
      RUN Ar/conlist.p ("",
                   ?,
                   iiCustNum).
   END. 

   ELSE IF FRAME-INDEX = 6 OR FRAME-INDEX = 0 THEN LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.

