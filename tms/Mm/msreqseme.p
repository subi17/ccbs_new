/*-----------------------------------------------------------------------------
  MODULE .......: msreqseme
  FUNCTION .....: ms request menu, service based
  SOVELLUTUS ...: TMS
  AUTHOR .......: aam 
  CREATED ......: 13.02.06
  changePVM ....: 12.12.06/mvi new param to RUN Mm/msrequest.p (reqstat = ?)
                  31.10.07 jp  new parameter for msrequest
                  
  Version ......: M15
  -------------------------------------------------------------------------- */

{Syst/commali.i}

DEF VAR lcMenuc   AS CHAR  NO-UNDO EXTENT 3 FORMAT "X(35)". 
DEF VAR liReqType AS INT   NO-UNDO EXTENT 3.
DEF VAR liMenu    AS INT   NO-UNDO.
DEF VAR liCount   AS INT   NO-UNDO.
DEF VAR lcLetters AS CHAR  NO-UNDO.

lcLetters = "A,B,C,D,E,F,G,H,I,J,K,L,M".

FOR EACH RequestType NO-LOCK WHERE
         RequestType.Brand = Syst.Var:gcBrand AND
         RequestType.ReqType <= 2:

   IF LOOKUP(STRING(RequestType.ReqType),"1,2") > 0 
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
   ASSIGN Syst.Var:ufk    = 0 
          Syst.Var:ufk[8] = 8 
          Syst.Var:ehto   = 3. 
   RUN Syst/ufkey.p. 

   DISPLAY
      lcMenuc
   WITH 1 COLUMN OVERLAY FRAME choices NO-LABELS.
       
   CHOOSE FIELD lcMenuc AUTO-RETURN go-on (F8) WITH FRAME choices
      TITLE " SERVICE REQUESTS "  CENTERED ROW 6.

   HIDE FRAME choices.

   IF LOOKUP(KEYLABEL(LASTKEY),"x,8,F8") > 0 THEN LEAVE.

   ELSE IF FRAME-INDEX >= 1 AND FRAME-INDEX <= liMenu - 1 
   THEN DO:
      RUN Mm/msrequest.p (liReqType[FRAME-INDEX],
                     ?,
                     0,
                     0,
                     0,
                     "").
   END.

   ELSE LEAVE.
 
END. /* DO WHILE */

HIDE FRAME choices NO-PAUSE.
HIDE MESSAGE.


