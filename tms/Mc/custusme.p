/*-----------------------------------------------------------------------------
  MODULE .......: custusme
  FUNCTION .....: customer user account menu
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 18.01.06
  changePVM ....: 13.03.06/aam change without sending letter (3)
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAMETER iiCustNum AS INT NO-UNDO.

DEF VAR menuc     AS CHAR EXTENT 4 FORMAT "X(35)"   NO-UNDO.
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
       " A) Username And Password          "             @ menuc[1] SKIP
       " B) Print A User Account Letter    "             @ menuc[2] SKIP
       " C) Change User Account, No Letter "             @ menuc[3] SKIP
       " X) QUIT   (F8)                    "             @ menuc[4] SKIP
       WITH OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " " + SUBSTR(lcName,1,50) + " "  CENTERED ROW 5.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

   ELSE IF FRAME-INDEX EQ 1 THEN DO:
      RUN Mc/custuserid.p(iiCustNum).
   END.

   ELSE IF FRAME-INDEX EQ 2 THEN DO:
      RUN Mc/custuserchg.p(iiCustNum,TRUE).
   END.

   ELSE IF FRAME-INDEX EQ 3 THEN DO:
      RUN Mc/custuserchg.p(iiCustNum,FALSE).
   END.

   ELSE IF FRAME-INDEX = 4 OR FRAME-INDEX = 0 THEN LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.



