/*-----------------------------------------------------------------------------
  MODULE .......: custrqme
  FUNCTION .....: customer request menu
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 10.02.06
                  12.12.06 mvi new param ro RUN Mm/msrequest.p (reqstat =  ?)  
                  03.09.07/aam Credit notes  
                  31.10.07 jp  new parameter for msrequest
                  
  changePVM ....: 
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF VAR menuc     AS CHAR EXTENT 10 FORMAT "X(35)"  NO-UNDO.
DEF VAR liQty     AS INT                            NO-UNDO. 
DEF VAR lcName    AS CHAR                           NO-UNDO. 
DEF VAR liInvType AS INT                            NO-UNDO.

DO FOR Customer:
   FIND Customer WHERE Customer.CustNum = iiCustNum NO-LOCK.
   lcName = STRING(Customer.CustNum) + " " +
            DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                             BUFFER Customer).
END. 

PAUSE 0.

DO WHILE TRUE:
   ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN Syst/ufkey.p. 

      DISPLAY

       " A) Address Change Requests        "             @ menuc[1] SKIP
       " B) User Account Requests          "             @ menuc[2] SKIP
       " C) Credit Note Requests           "             @ menuc[3] SKIP
       " D) Refund Requests                "             @ menuc[4] SKIP
       " E) Payment Requests               "             @ menuc[5] SKIP
       " F) Manual Payment Requests        "             @ menuc[6] SKIP
       " G) Payment Plan Requests          "             @ menuc[7] SKIP
       " H) DSS Requests                   "             @ menuc[8] SKIP
       " I) Email Activation Requests      "             @ menuc[9] SKIP
       " X) QUIT   (F8)                    "             @ menuc[10] SKIP
       WITH OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " " + SUBSTR(lcName,1,50) + " "  CENTERED ROW 5.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mm/msrequest.p(6,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 2 THEN DO:
      RUN Mm/msrequest.p(5,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 3 THEN DO:
      RUN Mm/msrequest.p(22,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 4 THEN DO:
      RUN Mm/msrequest.p(23,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 5 THEN DO:
      RUN Mm/msrequest.p(31,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 6 THEN DO:
      RUN Mm/msrequest.p(34,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 7 THEN DO:
      RUN Mm/msrequest.p(11,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 8 THEN DO:
      RUN Mm/msrequest.p(83,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX EQ 9 THEN DO:
      RUN Mm/msrequest.p(84,
                    ?, /* reqstat ? for all */
                    0,
                    iiCustNum,
                    0,
                    "").
   END.

   ELSE IF FRAME-INDEX = 10 OR FRAME-INDEX = 0 THEN LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



