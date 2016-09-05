/* ----------------------------------------------------------------------
  MODULE .......: all_cparam.P
  TASK .........: PARAM browser
  APPLICATION ..: TMS
  AUTHOR .......: Vikas
  CREATED ......: 13-02-2012
  CHANGED ......: Copied from main "cparam.p" program

  Version ......: 
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable TMSParam

{Syst/commali.i}
{Func/function.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMSParam'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTMSParam AS HANDLE NO-UNDO.
   lhTMSParam = BUFFER TMSParam:HANDLE.
   RUN StarEventInitialize(lhTMSParam).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhTMSParam).
   END.

END.

DEF INPUT PARAM icGroupCode     AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

/* print out TMSParam TO copy it TO other countries */
DEF STREAM TMSParam.

DEF VAR ParamCode  LIKE TMSParam.ParamCode  NO-UNDO.
DEF VAR ParamGroup LIKE TMSParam.ParamGroup NO-UNDO.
DEF VAR ParamName  LIKE TMSParam.ParamName NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
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
DEF VAR pvalue       AS c                      NO-UNDO.
DEF VAR lcVal        AS CHAR                   NO-UNDO.

DEFINE BUFFER bufParam FOR TMSParam.

form
    TMSParam.Brand        COLUMN-LABEL "Bran"      FORMAT "X(4)"
    TMSParam.ParamGroup                            format "x(13)"
    TMSParam.ParamCode    column-label "Parameter" format "x(24)"
    TMSParam.ParamName    column-label "Name"      format "x(22)"
    pvalue            column-label "Value"
    TMSParam.Online  column-label "OL"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " Parameter Browser "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    TMSParam.Brand         
    TMSParam.ParamCode     label "Parameter"
    FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1
    TMSParam.ParamGroup    label "Group"
    FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1
    TMSParam.ParamType
    TMSParam.ParamName     label "Name"
    FORMAT "x(256)" VIEW-AS FILL-IN SIZE 30 BY 1
    WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 1 columns
    FRAME lis.

{Func/brand.i}

form /* seek PARAM  BY  ParamCode */
    "Brand:" lcBrand skip
    "Code :" ParamCode
    help "Enter Code of param"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek PARAM BY ParamGroup */
    "Brand:" lcBrand skip
    "Group:" ParamGroup
    help "Enter Group of param"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND GROUP "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek PARAM BY ParamName */
    "Brand:" lcBrand skip
    "Name :" ParamName
    help "Enter Name of param"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

form    
    tmsparam.intVal label " VALUE "
    help "Give a value of integer"
    WITH ROW 11 col 16 width 50 
    side-labels OVERLAY FRAME lis-I.
form    
    TMSParam.CharVal COLUMN-label "VALUE"
    FORMAT "x(512)" VIEW-AS FILL-IN SIZE 60 BY 1
    help "Give a value of character"
    WITH ROW 10 centered /*width 20*/
    OVERLAY FRAME lis-C.
form    
    TMSParam.DecVal label " VALUE "
    help "Give a value of decimal"
    WITH ROW 11 col 16 width 50
    side-labels OVERLAY FRAME lis-DE.

form    
    TMSParam.DateVal label " VALUE " 
    help "Give a value of date"
    WITH ROW 11 col 16 width 50 
    side-labels OVERLAY FRAME lis-DA.


form
    TMSParam.Memo

    WITH OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " memo: " + TMSParam.ParamCode + " " WITH NO-LABELS 1 columns
    FRAME f4.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = " By Code , By Group , By Name , By 4".

IF icGroupCode > "" THEN
   FIND FIRST TMSParam WHERE
              TMSParam.Brand = lcBrand AND
              TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
ELSE
   FIND FIRST TMSParam WHERE
              TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.

IF AVAILABLE TMSParam THEN ASSIGN
   memory       = recid(TMSParam)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No TMS parameters available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.

order = 1.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 35 entry(order,orders).
    END.

   IF must-add THEN DO:  /* Add a TMSParam  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.


      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSAction:
           CLEAR FRAME lis NO-PAUSE.
           DISPLAY lcBrand @ TMSParam.Brand.

           PROMPT-FOR TMSParam.ParamCode.
           
           IF INPUT TMSParam.ParamCode = "" THEN LEAVE ADD-ROW.
      
           CREATE TMSParam.
           ASSIGN
              TMSParam.Brand     = lcBrand
              TMSParam.ParamCode = INPUT FRAME lis TMSParam.ParamCode.

           RUN local-update-record.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMSParam).

           ASSIGN
           memory = recid(TMSParam)
           xrecid = memory.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST TMSParam
      WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE TMSParam THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TMSParam WHERE recid(TMSParam) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMSParam THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMSParam).
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
        PAUSE 0 no-MESSAGE.

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
        ufk[1]= 973 ufk[2]= 35 ufk[3]= 30 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 1752
        ufk[8]= 8
        ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 2 THEN DO:
        CHOOSE ROW TMSParam.ParamCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMSParam.ParamCode WITH FRAME sel.
      END.
      ELSE IF order = 1 THEN DO:
        CHOOSE ROW TMSParam.ParamGroup {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMSParam.ParamGroup WITH FRAME sel.
      END.
    IF order = 3 THEN DO:
        CHOOSE ROW TMSParam.ParamName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TMSParam.ParamName WITH FRAME sel.
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
        FIND TMSParam WHERE recid(TMSParam) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-prev.
           IF AVAILABLE TMSParam THEN
              ASSIGN FIRSTrow = i memory = recid(TMSParam).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF NOT AVAILABLE TMSParam THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(TMSParam)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE TMSParam THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(TMSParam).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND TMSParam WHERE recid(TMSParam) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE TMSParam THEN DO:
           memory = recid(TMSParam).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-prev.
              IF AVAILABLE TMSParam THEN memory = recid(TMSParam).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
        END.
     END. /* previous page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           memory = rtab[FRAME-DOWN].
           FIND TMSParam WHERE recid(TMSParam) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ParamCode = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              ParamCode WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF ParamCode <> "" THEN DO:
          FIND FIRST TMSParam WHERE 
                     TMSParam.Brand      = lcBrand AND
                     TMSParam.ParamCode >= ParamCode 
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY col 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ParamGroup = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              ParamGroup WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF ParamGroup <> "" THEN DO:
          FIND FIRST TMSParam WHERE 
                     TMSParam.Brand       = lcBrand AND
                     TMSParam.ParamGroup >= ParamGroup
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ParamName = "".
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME F3.
       UPDATE lcBrand WHEN gcAllBrand
              ParamName WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.

       IF ParamName <> "" THEN DO:
          FIND FIRST TMSParam WHERE 
                     TMSParam.Brand      = lcBrand AND
                     TMSParam.ParamName >= ParamName
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(3) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-3 */


     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.p.
        RUN local-find-this(TRUE).

        DISPLAY TMSParam.Memo WITH FRAME f4.

        IF lcRight = "RW" THEN DO:

           ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSParam).

           UPDATE TMSParam.Memo WITH FRAME f4.

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSParam).

        END.   
        ELSE PAUSE.
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       TMSParam.ParamCode TMSParam.ParamGroup TMSParam.ParamName.

       RUN local-find-NEXT.
       IF AVAILABLE TMSParam THEN memory = recid(TMSParam).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-prev.
          IF AVAILABLE TMSParam THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(TMSParam).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TMSParam.ParamCode TMSParam.ParamGroup TMSParam.ParamName.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMSParam).

           DELETE TMSParam.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST TMSParam
           WHERE TMSParam.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"CTRL-P") > 0 THEN DO:
        FIND FIRST TMSParam where
                   recid(TMSParam) = rtab[FRAME-LINE]
        no-lock no-error.
        IF AVAIL TMSParam THEN DO:
           OUTPUT STREAM TMSParam TO TMSParam.d append.
           EXPORT STREAM TMSParam TMSParam.
           OUTPUT STREAM TMSParam CLOSE.
        END.
        APPLY LASTKEY.
     END.                           

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     CHANGE:    
     REPEAT WITH FRAME lis TRANSACTION ON ENDKEY UNDO change, NEXT LOOP.
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p.
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY TMSParam.ParamCode
               TMSParam.ParamGroup 
               TMSParam.ParamType
               TMSParam.ParamName.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSParam).

       RUN local-update-record.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSParam).

       HIDE FRAME lis NO-PAUSE.

       RUN local-disp-row.
       xrecid = recid(TMSParam).
       LEAVE.

     END.

     ELSE IF LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(TMSParam) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,e") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(TMSParam) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN Mc/eventsel.p("TMSParam", gcBrand + chr(255) + TMSParam.ParamGroup +
                                   chr(255) + TMSParam.ParamCode).
        ufkey = TRUE.
        NEXT.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS lo NO-UNDO.
    IF exlock THEN
      FIND TMSParam WHERE recid(TMSParam) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
    ELSE
       FIND TMSParam WHERE recid(TMSParam) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
    IF icGroupCode > "" THEN DO:
       IF order = 1 THEN FIND FIRST TMSParam
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
    END. /* IF icGroupCode > "" THEN DO: */
    ELSE DO:
       IF order = 1 THEN FIND FIRST TMSParam
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND FIRST TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
    END. /* ELSE DO: */
END PROCEDURE.

PROCEDURE local-find-LAST:
    IF icGroupCode > "" THEN DO:
       IF order = 1 THEN FIND LAST TMSParam
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
    END. /* IF icGroupCode > "" THEN DO: */
    ELSE DO:
       IF order = 1 THEN FIND LAST TMSParam
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND LAST TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
    END. /* ELSE DO: */
END PROCEDURE.

PROCEDURE local-find-NEXT:
    IF icGroupCode > "" THEN DO:
       IF order = 1 THEN FIND NEXT TMSParam
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
    END. /* IF icGroupCode > "" THEN DO: */
    ELSE DO:
       IF order = 1 THEN FIND NEXT TMSParam
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND NEXT TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
    END. /* ELSE DO: */
END PROCEDURE.

PROCEDURE local-find-prev:
    IF icGroupCode > "" THEN DO:
       IF order = 1 THEN FIND PREV TMSParam
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand AND
             TMSParam.ParamGroup MATCHES "*" + icGroupCode + "*" NO-LOCK NO-ERROR.
    END. /* IF icGroupCode > "" THEN DO: */
    ELSE DO:
       IF order = 1 THEN FIND PREV TMSParam
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV TMSParam USE-INDEX ParamCode
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 3 THEN FIND PREV TMSParam USE-INDEX ParamName
       WHERE TMSParam.Brand = lcBrand NO-LOCK NO-ERROR.
    END. /* ELSE DO: */
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       /* set pvalue */
       if TMSParam.ParamType = "I"  
       THEN pvalue = string(tmsparam.intVal).  
       ELSE if TMSParam.ParamType = "de" 
       THEN pvalue = string(TMSParam.DecVal).       
       ELSE if TMSParam.ParamType = "da" 
       then pvalue = string(TMSParam.DateVal,"99-99-99").        
       ELSE if TMSParam.ParamType = "c" 
       THEN pvalue = TMSParam.CharVal.   


       DISPLAY
       TMSParam.Brand
       TMSParam.ParamCode
       TMSParam.ParamGroup
       TMSParam.ParamName
       pvalue
       TMSParam.Online
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:
   RUN local-find-others.

   DISP
   TMSParam.Brand
   TMSParam.ParamType
   TMSParam.ParamGroup
   TMSParam.ParamName
   TMSParam.Online
   WITH FRAME lis.

   IF lcRight = "RW" THEN DO:


      UPDATE
          TMSParam.ParamGroup    when NEW TMSParam
          TMSParam.ParamType     when NEW TMSParam
          TMSParam.ParamName  /*   when NEW TMSParam      */
          TMSParam.Online
      WITH FRAME lis EDITING:
         READKEY.
         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            IF FRAME-FIELD = "ParamGroup" THEN DO:
               FIND FIRST bufParam WHERE
                          bufParam.ParamGroup = INPUT TMSParam.ParamGroup AND
                          bufParam.ParamCode  = INPUT TMSParam.ParamCode
               NO-LOCK NO-ERROR.
               IF AVAIL bufParam THEN DO:
                  MESSAGE "TMSParam exists allready!" VIEW-AS ALERT-BOX.
               END.
            END.
         END.
         APPLY LASTKEY.
      END.

      IF TMSParam.Online entered OR TMSParam.Online THEN fOLRefresh(TRUE).

   END.

   PAUSE 0.
   IF TMSParam.ParamType = "C" THEN DO:
      IF lcRight = "RW" THEN 
      UPDATE TMSParam.CharVal WITH FRAME lis-C.
      ELSE DO: 
         DISP TMSParam.CharVal WITH FRAME lis-C.
         PAUSE.
      END.   
      HIDE FRAME lis-c.
   END.   
   else IF TMSParam.ParamType = "I" THEN DO:
      IF lcRight = "RW" THEN 
      UPDATE tmsparam.intVal  WITH FRAME lis-I.
      ELSE DO:
         DISP tmsparam.intVal  WITH FRAME lis-I.
         PAUSE.
      END.
      HIDE FRAME lis-I.
   END.   
   else IF TMSParam.ParamType = "DA" THEN DO:
      IF lcRight = "RW" THEN
      UPDATE TMSParam.DateVal   WITH FRAME lis-DA.
      ELSE DO:
         DISP  TMSParam.DateVal   WITH FRAME lis-DA.
         PAUSE.
      END.   
      HIDE FRAME lis-DA.
   END.
   else IF TMSParam.ParamType = "DE" THEN DO:
      IF lcRight = "RW" THEN
      UPDATE TMSParam.DecVal   WITH FRAME lis-DE.
      ELSE DO:
         DISP TMSParam.DecVal   WITH FRAME lis-DE.
         PAUSE.
      END.   
      HIDE FRAME lis-DE.
   END.   

END PROCEDURE.



