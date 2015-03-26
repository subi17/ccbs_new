/*------------------------------------------------------
  MODULE .......: mobilett.P
  FUNCTION .....: MobileCust tarkat tiedot -valikko
  SOVELLUTUS ...: NN
  AUTHOR .......: PT
  CREATED ......: 
  changePVM ....: 
  Version ......: M15
  SHARED .......: INPUT: CustNum
  ------------------------------------------------------ */

{commali.i}

DEF  INPUT PARAMETER   CustNum AS INT.

DEF VAR menuc   AS C  EXTENT 11                  NO-UNDO.
DEF VAR inv-rep AS LO FORMAT "Invoiced/Reported" NO-UNDO.
DEF VAR ok      AS LO                           NO-UNDO.
PAUSE 0.

FIND Customer WHERE Customer.CustNum = CustNum NO-LOCK. 

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN ufkey. 

      DISPLAY
       " A) Customer's Mobile Subscriptions(browse)"    @ menuc[1] SKIP
       " B) Customer's Calls (browse)      "            @ menuc[2]  SKIP 
       " C) Customer's Calls (VALUE)       "            @ menuc[3]  SKIP 
       " D) Monthly call counters          "            @ menuc[4]  SKIP
       " E) Customer's Active MSISDN Numbers"           @ menuc[5]  SKIP
       " F) Customer's Reserved MSISDN Numbers"         @ menuc[6]  SKIP
       " G) Customer's SIM Cards"                       @ menuc[7]  SKIP       
       " H) Customer's Mobile Services"                 @ menuc[8]  SKIP       
       " I) Customer's Terminals"                       @ menuc[9]  SKIP
       " J) SIM Card report"                            @ menuc[10]  SKIP
       " X) QUIT   (F8)                    "            @ menuc[11] SKIP
       

       WITH OVERLAY FRAME choices NO-LABELS.
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
   TITLE " " + SUBSTR(Customer.CustName,1,24) + " "  CENTERED WITH COL 2 ROW 4.
   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.



   IF FRAME-INDEX EQ 1 THEN DO:
      IF NOT CAN-FIND (FIRST mobsub WHERE
                            mobsub.CustNum = CustNum) THEN DO:
         MESSAGE
         "Customer no:" custnum " hasn't got any mobile subscriptions!"    SKIP
         VIEW-AS ALERT-BOX.
      END.
      ELSE RUN mobsub(CustNum, "").
   END.


   ELSE IF FRAME-INDEX EQ 2 THEN DO:
       RUN mobcallic(CustNum).
   END.

   ELSE IF FRAME-INDEX EQ 3 THEN DO:
      RUN callvalueic(CustNum).
   END.

   ELSE IF FRAME-INDEX EQ 4 THEN DO:
       RUN nnmtcu(CustNum).
   END.

   ELSE IF FRAME-INDEX EQ 5 THEN DO:
       RUN custmsisdn(CustNum,"3")   /* Active */.
   END.

   ELSE IF FRAME-INDEX EQ 6 THEN DO:
       RUN custmsisdn(CustNum,"2").  /* Reserved */
   END.

   ELSE IF FRAME-INDEX EQ 7 THEN DO:
       RUN custsim(CustNum). 
   END.

   ELSE IF FRAME-INDEX EQ 8 THEN DO:
       RUN custserv(CustNum) /* Customer's services */.
   END.

   ELSE IF FRAME-INDEX EQ 9 THEN DO:
       RUN custerm(CustNum) /* Customer's services */.
   END.

   ELSE IF FRAME-INDEX EQ 10 THEN DO:
       RUN simrep(CustNum) /* SIM Card report */.
   END.

   ELSE IF FRAME-INDEX = 11 OR FRAME-INDEX = 0 THEN LEAVE.
   ELSE DO:
      MESSAGE 
      "Not implement yet"
      VIEW-as aLERt-BOX.

   END.
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



