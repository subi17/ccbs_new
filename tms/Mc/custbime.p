/*-----------------------------------------------------------------------------
  MODULE .......: custbime
  FUNCTION .....: customer billing menu
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam (from commontt.p)
  CREATED ......: 16.12.05
  changePVM ....: 31.01.06/aam callvalueic and mobcallic
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}
{Func/fcustdata.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF VAR menuc     AS CHAR EXTENT 10 FORMAT "X(35)"  NO-UNDO.
DEF VAR liQty     AS INT                            NO-UNDO. 
DEF VAR lcName    AS CHAR                           NO-UNDO. 
DEF VAR liInvType AS INT                            NO-UNDO.
DEF VAR lcRoles   AS CHAR                           NO-UNDO.

DO FOR Customer:
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
   lcName = STRING(Customer.CustNum) + " " +
            DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                             BUFFER Customer).
                             
   lcRoles = fCustRoles(BUFFER Customer).                          
END. 

/* invoice type to be created */
liInvType = fCParamI("InvCreType").
IF liInvType = 0 OR liInvType = ? THEN liInvType = 3.

PAUSE 0.

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey.p. 

      DISPLAY

       " A) Create Invoice                 "             @ menuc[1] SKIP
       " B) Create Deposit/Adv.Paym Invoice"             @ menuc[2] SKIP
       " C) Billing Targets                "             @ menuc[3] SKIP
       " D) Refund Balances                "             @ menuc[4] SKIP
       " E) Print Customer Balances        "             @ menuc[5] SKIP
       " F) Inv.Customer's Calls (Browse)  "             @ menuc[6] SKIP
       " G) Inv.Customer's Calls (Value)   "             @ menuc[7] SKIP
       " H) Invoice Target Groups          "             @ menuc[8] SKIP
       " I) DSS Billing Information        "             @ menuc[9] SKIP
       " X) QUIT   (F8)                    "             @ menuc[10] SKIP
       WITH OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " " + SUBSTR(lcName,1,50) + " "  CENTERED ROW 5.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   ELSE IF FRAME-INDEX EQ 1 THEN DO:
      RUN Inv/nnlamu3.p(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 2 THEN DO:
      RUN Inv/nnlamu5.p(iiCustNum,
                  0,
                  "",
                  liInvType,
                  FALSE,
                  OUTPUT liQty).
   END.

   ELSE IF FRAME-INDEX EQ 3 THEN DO:
      RUN Mc/invtarg.p(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 4 THEN DO:
      RUN Ar/refubal.p(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 5 THEN DO:
   
      MESSAGE "Not in use"
      VIEW-AS ALERT-BOX INFORMATION.
      NEXT.
      
/*      RUN Ar/custbalrel.p(iiCustNum). */
   END.

   ELSE IF FRAME-INDEX >= 6 AND FRAME-INDEX <= 7 THEN DO:

      IF SUBSTRING(lcRoles,2,1) NE "1" THEN DO:
         MESSAGE "This is not an invoice customer"
         VIEW-AS ALERT-BOX INFORMATION.
         NEXT.
      END. 

      IF FRAME-INDEX = 6 THEN RUN Mm/mobcallic.p (iiCustNum).
      ELSE IF FRAME-INDEX = 7 THEN RUN Mm/callvalueic.p(iiCustNum).
   END.
   
   ELSE IF FRAME-INDEX EQ 8 THEN DO:
      RUN Mc/invoicetargetgroup.p(iiCustnum).
   END.

   ELSE IF FRAME-INDEX EQ 9 THEN DO:
      RUN Mm/dss_billing_info.p(iiCustnum).
   END.
   
   ELSE IF FRAME-INDEX = 10 OR FRAME-INDEX = 0 THEN LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



