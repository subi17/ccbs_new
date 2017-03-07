/* ----------------------------------------------------------------------
  MODULE .......: choosefile.p
  TASK .........: choose an external file from a list
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 12.08.2002
  CHANGED ......: 16.07.2004/aam icFilter can include second parameter 
                                 (llHideDir)
  VERSION ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

DEF INPUT  PARAMETER icFilter AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocChosen AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcFileName   AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR llHideDir    AS LOG                    NO-UNDO.


DEF TEMP-TABLE ttFile NO-UNDO
    FIELD FName AS CHAR
    INDEX FName FName. 

form
    ttFile.FName    
       COLUMN-LABEL "File"
       FORMAT "X(75)"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
    " Choose File "
    FRAME sel.

form /* seek  BY  name */
    lcFileName
    HELP "Enter name of file"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND FILE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.

IF INDEX(icFilter,"¤") > 0 
THEN ASSIGN llHideDir = (ENTRY(2,icFilter,"¤") > "")
            icFilter  = ENTRY(1,icFilter,"¤").
ELSE llHideDir = FALSE.
 
ASSIGN FRAME sel:TITLE = " CHOOSE FILE (" + icFilter + ") ". 

PAUSE 0. 
VIEW FRAME sel.

orders = "By Name,By 3, By 4".
           
/* find files according to given filter */
RUN pFindFiles. 

FIND FIRST ttFile
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE ttFile THEN ASSIGN
   memory       = recid(ttFile)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttFile WHERE recid(ttFile) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttFile THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttFile).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 11 /* 5 */     ufk[6]= 0 /* 4 */ 
        ufk[7]= 0   ufk[8]= 8  ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        {Syst/uright1.i '"5,6"'}.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttFile.FName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttFile.FName WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND ttFile WHERE recid(ttFile) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttFile THEN
              ASSIGN FIRSTrow = i memory = recid(ttFile).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttFile THEN DO:
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
                rtab[1] = recid(ttFile)
                memory  = rtab[1].
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
           IF NOT AVAILABLE ttFile THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttFile).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ttFile WHERE recid(ttFile) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttFile THEN DO:
           memory = recid(ttFile).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttFile THEN memory = recid(ttFile).
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
           memory = rtab[FRAME-DOWN].
           FIND ttFile WHERE recid(ttFile) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcFileName WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcFileName ENTERED THEN DO:
          FIND FIRST ttFile WHERE ttFile.FName >= lcFileName
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE ttFile THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some ttFile/ttFile was found */
          ASSIGN order = 1 memory = recid(ttFile) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */


     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO:  /* choose */
        RUN local-find-this(FALSE).
        ASSIGN ocChosen = ttFile.FName. 
        
        LEAVE LOOP. 
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}.
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ttFile.FName.

       RUN local-find-NEXT.
       IF AVAILABLE ttFile THEN memory = recid(ttFile).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttFile THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(ttFile).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ttFile.FName.
       IF ok THEN DO:

           DELETE ttFile.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ttFile
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ttFile) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ttFile) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         ocChosen = "".
         LEAVE LOOP.
     END. 

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttFile WHERE recid(ttFile) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttFile WHERE recid(ttFile) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttFile
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttFile
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttFile
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttFile
       /* srule */ NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttFile.FName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.


PROCEDURE pFindFiles:

    IF OPSYS = "UNIX" THEN 
       INPUT THROUGH VALUE("ls -1 " + icFilter).
    ELSE DO: 
       OS-COMMAND SILENT VALUE("dir /b " + icFilter + " >tsmflist.lst").
       INPUT FROM tmsflist.lst NO-ECHO.
    END.

    REPEAT:
       IMPORT UNFORMATTED lcFileName.
       IF lcFileName = "" OR 
          lcFileName MATCHES ("*o such file or*") 
       THEN NEXT.
       CREATE ttFile.
       ASSIGN ttFile.FName = lcFileName. 
      
       /* show only file, not directory */
       IF llHideDir THEN DO:
          IF INDEX(ttFile.FName,"/") > 0
          THEN ASSIGN i            = R-INDEX(ttFile.FName,"/")
                      ttFile.FName = SUBSTRING(ttFile.FName,i + 1).
       END.         
            
    END.
    INPUT CLOSE.

    IF NOT CAN-FIND(FIRST ttFile) THEN DO:
       CREATE ttFile.
       ASSIGN ttFile.FName = "< No files found >".
    END. 

END PROCEDURE.


