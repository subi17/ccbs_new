/*-----------------------------------------------------------------------------
  MODULE .......: msreqarme
  FUNCTION .....: ms request menu, ar based
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 16.04.07
  changePVM ....: 
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF VAR lcMenuc   AS CHAR  NO-UNDO EXTENT 6 FORMAT "X(35)". 
DEF VAR liReqType AS INT   NO-UNDO EXTENT 6.
DEF VAR liMenu    AS INT   NO-UNDO.
DEF VAR liCount   AS INT   NO-UNDO.
DEF VAR lcLetters AS CHAR  NO-UNDO.

lcLetters = "A,B,C,D,E,F,G,H,I,J,K,L,M".

FOR EACH RequestType NO-LOCK WHERE
         RequestType.Brand = gcBrand AND
         RequestType.ReqType >= 20:

   IF LOOKUP(STRING(RequestType.ReqType),"20,22,23,31,34") > 0 
   THEN ASSIGN 
      liMenu            = liMenu + 1
      lcMenuc[liMenu]   = " " + ENTRY(liMenu,lcLetters) + ") " +
                          RequestType.ReqName
      liReqType[liMenu] = RequestType.ReqType.   
END.

ASSIGN liMenu          = liMenu + 1
       lcMenuc[liMenu] = " X) QUIT   (F8)".
                                             
PAUSE 0.

DO WHILE TRUE:
   ASSIGN ufk    = 0 
          ufk[8] = 8 
          ehto   = 3. 
   RUN Syst/ufkey.p. 

   DISPLAY
      lcMenuc
   WITH 1 COLUMN OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD lcMenuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " ACCOUNT RECEIVABLE REQUESTS "  CENTERED ROW 6.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,8,F8") > 0 THEN LEAVE.

   ELSE IF FRAME-INDEX >= 1 AND FRAME-INDEX <= liMenu - 1 
   THEN DO:
      RUN Mm/msreqstat.p (liReqType[FRAME-INDEX],
                     0).
   END.

   ELSE LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.


