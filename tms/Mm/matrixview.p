
DEFINE INPUT PARAMETER  iiMXSeq AS INT NO-UNDO.

DEF TEMP-TABLE ttNAme 
   FIELD Numero AS INT 
   FIELD Name   AS CHAR FORMAT "X(12)" 
   FIELD Val    AS CHAR EXTENT 20.

DEF TEMP-TABLE ttMAtrix
   FIELD Val  AS CHAR FORMAT "X(12)" EXTENT 5
   FIELD colu AS CHAR FORMAT "X(20)" EXTENT 5.

DEF VAR kpl     AS i no-undo.
DEF VAR lii     AS I NO-UNDO.
DEF VAR liloop  as i no-undo.
DEF VAR llFound AS LOG NO-UNDO.

FOR EACH MXItem WHERE 
         MXItem.MXSeq = iiMXSeq  NO-LOCK.

   FIND FIRST ttName WHERE 
              ttName.name = MXItem.MXName no-error.

   IF not avail ttName then do:
      lii = lii + 1.
      CREATE ttName.
      ASSIGN
         ttName.Numero = lii
         ttname.name  = mxItem.MXName.
    END.           

    
    DO liloop = 1 to  5:
   
       IF ttName.Val[liloop] = "" then do:
          ttName.Val[liloop] = MXItem.mxValue.
          kpl = kpl + 1.   
          LEAVE.
       END.
   END.

END.         

liloop = 0.

DEF VAR liloop2 AS INT  NO-UNDO.
DEF VAR arvo    as INT  NO-UNDO.
DEF VAR lista   AS CHAR NO-UNDO FORMAT "X(78)" .
DEF VAR lista2  as CHAR No-UNDO.


DO liloop2 =   EXP(10, lii - 1 ) to EXP(10,lii)  :

   llfound = FALSE.

   ETSI:
   DO liloop = 1 TO LENGTH(STRING(liloop2)):

      ASSIGN 
         arvo = INT(SUBSTRING(STRING(liloop2),liloop,1))
         lista = "".

      IF arvo = 0 then leave etsi.
      
      FIND FIRST ttName WHERE 
                 ttName.numero = liloop AND 
                 ttName.val[arvo] ne ""       
      NO-LOCK NO-ERROR.

      IF NOT AVAIL ttName THEN DO:
          ASSIGN
             llFound = false
             lista   = "".
          LEAVE ETSI.
      END.
      ELSE DO:
         ASSIGN
         llFound = TRUE
         lista   = lista  +  STRING(liloop2).
      END.   

   END.

   IF llFound and 
      lista ne "" then do liloop = 1 to lii:

      arvo = INT(SUBSTRING(lista,liloop,1)).
              
      FIND FIRST ttName  WHERE 
                 ttName.Numero    = liloop AND
                 ttName.val[arvo] ne ""      
      NO-LOCK NO-ERROR.
      
      IF liloop = 1 THEN DO:
         CREATE ttMatrix.
         ASSIGN
         ttMatrix.Colu[liloop] = ttName.Name.
      END.

      ASSIGN 
         ttMatrix.Val[liloop] = ttName.val[arvo].
   END.

END.   


/* ----------------------------------------------------------------------
  MODULE .......: 
  TASK .........: 
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commpaa.i} katun = "polkki". gcbrand = "1".
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhttMatrix AS HANDLE NO-UNDO.
   lhttMatrix = BUFFER ttMatrix:HANDLE.
   RUN StarEventInitialize(lhttMatrix).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhttMatrix).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.

form
    ttMatrix.Val[1]  
    ttMatrix.Val[2]  COLUMN-LABEL ""
    ttMatrix.Val[3]  COLUMN-LABEL ""
    ttMatrix.Val[4]  COLUMN-LABEL ""
    ttMatrix.Val[5]  COLUMN-LABEL ""

WITH ROW FrmRow width 70 OVERLAY CENTERED FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    "  MATRIX  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FOR EACH ttName 
Break BY ttName.numero.
   
   IF LAST-OF(ttName.numero) THEN DO:  

      if ttName.numero = 1 THEN 
      ttMatrix.Val[1]:LABEL IN FRAME sel = ttName.name. 
      else if ttName.numero = 2 THEN 
      ttMatrix.Val[2]:LABEL IN FRAME sel = ttName.name. 
      ELSE if ttName.numero = 3 THEN 
      ttMatrix.Val[3]:LABEL IN FRAME sel = ttName.name. 
      ELSE if ttName.numero = 4 THEN
      ttMatrix.Val[4]:LABEL IN FRAME sel = ttName.name.
      ELSE if ttName.numero = 5 THEN
      ttMatrix.Val[5]:LABEL IN FRAME sel = ttName.name.
   
   END.    
END.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".


FIND FIRST ttMatrix
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ttMatrix THEN ASSIGN
   Memory       = recid(ttMatrix)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No matrix items available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a ttMatrix  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           ASSIGN
           Memory = recid(ttMatrix)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ttMatrix
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ttMatrix THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttMatrix WHERE recid(ttMatrix) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttMatrix THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttMatrix).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.
      END.


      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttMatrix.Val[1] ;(uchoose.i;) 
        NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttMatrix.Val WITH FRAME sel.
      END.
      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttMatrix WHERE recid(ttMatrix) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttMatrix THEN
              ASSIGN FIRSTrow = i Memory = recid(ttMatrix).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttMatrix THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(ttMatrix)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttMatrix THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ttMatrix).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttMatrix WHERE recid(ttMatrix) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttMatrix THEN DO:
           Memory = recid(ttMatrix).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttMatrix THEN Memory = recid(ttMatrix).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND ttMatrix WHERE recid(ttMatrix) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */


     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttMatrix) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttMatrix) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttMatrix WHERE recid(ttMatrix) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttMatrix WHERE recid(ttMatrix) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttMatrix
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttMatrix
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttMatrix
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttMatrix
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttMatrix.Val[1]
        ttMatrix.Val[2]
        ttMatrix.Val[3]
        ttMatrix.Val[4]
        ttMatrix.Val[5]

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          ttMatrix.Val[1]
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

