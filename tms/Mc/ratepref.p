/* ----------------------------------------------------------------------
  MODULE .......: RatePref
  TASK .........: UPDATEs table RatePref
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 23.09.02
  CHANGED ......: 27.02.03 tk tokens
                  17.09.03/aam brand
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RatePref

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'ratepref'}

{eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRatePref AS HANDLE NO-UNDO.
   lhRatePref = BUFFER RatePref:HANDLE.
   RUN StarEventInitialize(lhRatePref).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRatePref).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcPrefix   LIKE RatePref.Prefix     NO-UNDO.
DEF VAR liDialType LIKE RatePref.DialType   NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR lcDTName     AS CHAR                   NO-UNDO. 

form
    RatePref.Brand
    RatePref.Prefix     /* COLUMN-LABEL FORMAT */
    RatePref.DialType    
    lcDTName            COLUMN-LABEL "Name" FORMAT "X(30)" 
    RatePref.RatePref
    RatePref.CustRate
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " Rating Prefixes "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    RatePref.Prefix    COLON 16   /* LABEL FORMAT */
    RatePref.DialType  COLON 16   
       VALIDATE(CAN-FIND(DialType WHERE 
                         DialType.DialType = INPUT RatePref.DialType),
                "Unknown dialling type")
    DialType.DTName NO-LABEL 
    RatePref.RatePref COLON 16
    RatePref.CustRate COLON 16
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

{brand.i}

form /* seek  Prefix */
    "Brand :" lcBrand skip
    "Prefix:" lcPrefix
    HELP "Enter Prefix"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Prefix "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  RatePref */
    "Brand ..:" lcBrand skip
    "DialType:" liDialType
    HELP "Enter Dialling Type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Dialling Type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "   By Prefix        ,By Dialling Type  , By 3, By 4".


FIND FIRST RatePref WHERE RatePref.Brand = lcBrand USE-INDEX Prefix
NO-LOCK NO-ERROR.

IF AVAILABLE RatePref THEN ASSIGN
   Memory       = recid(RatePref)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No ratepref records available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a RatePref  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR RatePref.Prefix RatePref.DialType.

           IF INPUT FRAME lis RatePref.Prefix = "" THEN 
           LEAVE add-row.

           IF CAN-FIND(FIRST RatePref WHERE
                       RatePref.Brand  = lcBrand AND
                       RatePref.Prefix = INPUT FRAME lis RatePref.Prefix AND
                       RatePref.DialType = INPUT FRAME lis RatePref.DialType)
           THEN DO:
              MESSAGE 
              "Dialling Type" 
              INPUT FRAME lis RatePref.Prefix "/" 
              INPUT FRAME lis RatePref.DialType
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE RatePref.
           ASSIGN
           RatePref.Brand    = lcBrand
           RatePref.Prefix   = INPUT FRAME lis RatePref.Prefix
           RatePref.DialType = INPUT FRAME lis RatePref.DialType.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRatePref).

           ASSIGN
           Memory = recid(RatePref)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST RatePref WHERE RatePref.Brand = lcBrand
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE RatePref THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND RatePref WHERE recid(RatePref) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RatePref THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(RatePref).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW RatePref.Prefix ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RatePref.Prefix WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW RatePref.DialType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RatePref.DialType WITH FRAME sel.
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
        FIND RatePref WHERE recid(RatePref) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE RatePref THEN
              ASSIGN FIRSTrow = i Memory = recid(RatePref).
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
           IF NOT AVAILABLE RatePref THEN DO:
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
                rtab[1] = recid(RatePref)
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
           IF NOT AVAILABLE RatePref THEN DO:
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
              rtab[FRAME-DOWN] = recid(RatePref).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND RatePref WHERE recid(RatePref) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE RatePref THEN DO:
           Memory = recid(RatePref).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE RatePref THEN Memory = recid(RatePref).
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
           FIND RatePref WHERE recid(RatePref) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcPrefix WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcPrefix ENTERED THEN DO:
          FIND FIRST RatePref WHERE 
                     RatePref.Brand = lcBrand AND  
                     RatePref.Prefix >= lcPrefix
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET liDialType WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF liDialType ENTERED THEN DO:
          FIND FIRST RatePref WHERE 
                     RatePref.Brand = lcBrand AND
                     RatePref.DialType >= liDialType
          USE-INDEX DialType  NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       RatePref.DialType RatePref.Prefix .

       RUN local-find-NEXT.
       IF AVAILABLE RatePref THEN Memory = recid(RatePref).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE RatePref THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(RatePref).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RatePref.DialType RatePref.Prefix .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRatePref).

           DELETE RatePref.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST RatePref WHERE RatePref.Brand = lcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY 
          RatePref.Prefix
          RatePref.DialType 
          RatePref.DialType
          RatePref.RatePref
          RatePref.CustRate.

       IF lcRight = "RW" THEN DO:

          IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRatePref).

          RUN local-UPDATE-record.                                  

          /* IF  User Wanted TO Cancel this Change TRANSACTION */
          IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
          KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

          IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRatePref).
       END.
       ELSE PAUSE.


       HIDE FRAME lis NO-PAUSE.

       RUN local-disp-row.
       xrecid = recid(RatePref).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(RatePref) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(RatePref) must-print = TRUE.
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
      FIND RatePref WHERE recid(RatePref) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND RatePref WHERE recid(RatePref) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST RatePref WHERE 
           RatePref.Brand = lcBrand USE-INDEX Prefix
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST RatePref WHERE 
           RatePref.Brand = lcBrand USE-INDEX DialType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST RatePref WHERE 
           RatePref.Brand = lcBrand USE-INDEX Prefix
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST RatePref WHERE 
           RatePref.Brand = lcBrand USE-INDEX DialType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT RatePref WHERE 
          RatePref.Brand = lcBrand USE-INDEX Prefix
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT RatePref WHERE 
          RatePref.Brand = lcBrand USE-INDEX DialType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV RatePref WHERE 
           RatePref.Brand = lcBrand USE-INDEX Prefix
        NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV RatePref  WHERE 
           RatePref.Brand = lcBrand USE-INDEX DialType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       RatePref.Brand
       RatePref.DialType
       RatePref.Prefix
       lcDTName
       RatePref.RatePref
       RatePref.CustRate
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND DialType NO-LOCK WHERE
        DialType.DialType = RatePref.DialType NO-ERROR.
   ASSIGN lcDTName = IF AVAILABLE DialType
                     THEN DialType.DTName
                     ELSE "". 
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP RatePref.Prefix
           RatePref.DialType
      WITH FRAME lis.
      UPDATE
          RatePref.RatePref
          RatePref.CustRate
      WITH FRAME lis
      EDITING:
         READKEY.
         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:

            PAUSE 0.
            IF FRAME-FIELD = "DialType" THEN DO:
                   FIND DialType WHERE DialType.DialType =
                   INPUT FRAME lis RatePref.DialType NO-LOCK NO-ERROR.
                   IF NOT AVAIL DialType THEN DO:
                      BELL.
                      MESSAGE "Unknown dialling type !".
                      NEXT.       
                   END.
                   DISP DialType.DTName.
            END.
         END.
         APPLY LASTKEY.
      END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.

