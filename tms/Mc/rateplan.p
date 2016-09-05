/* ----------------------------------------------------------------------
  MODULE .......: RatePlan.P
  TASK .........: Rating Plan
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.09.02
  CHANGED ......: 27.02.03 tk tokens
                  28.02.03/aam PNPRateplan removed
                  18.03.03/tk new memo
                  17.09.03/aam brand
  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RatePlan

{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'rateplan'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhRatePlan AS HANDLE NO-UNDO.
   lhRatePlan = BUFFER RatePlan:HANDLE.
   RUN StarEventInitialize(lhRatePlan).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhRatePlan).
   END.

END.


def /* new */ shared var siirto AS char.

DEF VAR RatePlan  like RatePlan.RatePlan  NO-UNDO.
DEF VAR RPName    like RatePlan.RPName NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcPNPName    AS CHAR                   NO-UNDO. 

DEF BUFFER bRatePlan FOR RatePlan.

form
    RatePlan.Brand  
    RatePlan.RatePlan      /* column-label format */
    RatePlan.RPName     /* column-label format */
WITH ROW FrmRow width 80 overlay FrmDown  down
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " Rating Plans "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    RatePlan.RatePlan     /* label format */
    RatePlan.RPName       /* label format */

WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) ac-hdr 
    side-labels 
    1 columns
    FRAME lis.

{Func/brand.i}

form /* seek RatePlan  by  RatePlan */
    "Brand:" lcBrand skip
    "Code :" RatePlan
    help "Enter Rating Plan's code"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) no-labels overlay FRAME f1.

form /* seek RatePlan  by RPName */
    "Brand:" lcBrand skip
    "Name :" DGName
    help "Enter Rating Plan's name"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND NAME "
    COLOR VALUE(cfc) no-labels overlay FRAME f2.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST RatePlan
 WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE RatePlan THEN ASSIGN
   memory       = recid(RatePlan)
   must-print   = true
   must-add     = false.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rating plans available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = false
      must-add     = true.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a RatePlan  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, leave ADD-ROW.
        PAUSE 0 no-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR RatePlan.RatePlan
           validate
              (RatePlan.RatePlan NOT ENTERED or
              NOT CAN-FIND(RatePlan using  RatePlan.RatePlan WHERE
                           RatePlan.Brand = lcBrand),
              "RatePlan " + string(INPUT RatePlan.RatePlan) +
              " already exists !").
           IF RatePlan.RatePlan NOT ENTERED THEN LEAVE add-row.
           create RatePlan.
           ASSIGN
           RatePlan.Brand    = lcBrand
           RatePlan.RatePlan = INPUT FRAME lis RatePlan.RatePlan.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRatePlan).

           ASSIGN
           memory = recid(RatePlan)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST RatePlan
       WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE RatePlan THEN leave LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND RatePlan WHERE recid(RatePlan) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE RatePlan THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(RatePlan).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN leave.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 no-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 35 ufk[2]= 30 ufk[3]= 876 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 814   
        ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row RatePlan.RatePlan {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RatePlan.RatePlan WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row RatePlan.RPName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) RatePlan.RPName WITH FRAME sel.
      END.

      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND RatePlan WHERE recid(RatePlan) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-prev.
           IF AVAILABLE RatePlan THEN
              ASSIGN FIRSTrow = i memory = recid(RatePlan).
           ELSE leave.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 no-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* previous row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-prev.
           IF NOT AVAILABLE RatePlan THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* previous was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(RatePlan)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE RatePlan THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(RatePlan).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND RatePlan WHERE recid(RatePlan) = memory NO-LOCK NO-ERROR.
        RUN local-find-prev.
        IF AVAILABLE RatePlan THEN DO:
           memory = recid(RatePlan).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-prev.
              IF AVAILABLE RatePlan THEN memory = recid(RatePlan).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND RatePlan WHERE recid(RatePlan) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              RatePlan WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF RatePlan > "" THEN DO:
          FIND FIRST RatePlan WHERE 
                     RatePlan.Brand     = lcBrand AND
                     RatePlan.RatePlan >= RatePlan
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = true.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              RPName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       IF RPName > "" THEN DO:
          FIND FIRST RatePlan WHERE 
                     RatePlan.Brand   = lcBrand AND
                     RatePlan.RPName >= RPName
          USE-INDEX RPName  NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* price list configuration  */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:

        run local-find-this(false).

        RUN Mc/plistconf.p(RatePlan.RatePlan).

        ASSIGN ufkey    = TRUE
               pr-order = 0.
        NEXT loop.
     END.

     /* Update Memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO: 
        run local-find-this(FALSE).

        RUN Mc/memo.p(INPUT 0,
                 INPUT "RatePlan",
                 INPUT STRING(RatePlan.RatePlan),
                 INPUT "Rating Plan").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       IF CAN-FIND(FIRST PListConf OF RatePlan) THEN DO:
          MESSAGE
          "There are one or more Price Lists"  SKIP
          "defined for this Rating Plan"
          VIEW-AS ALERT-BOX
          TITLE " DELETE NOT ALLOWED ".
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       RatePlan.RatePlan RatePlan.RPName .

       RUN local-find-NEXT.
       IF AVAILABLE RatePlan THEN memory = recid(RatePlan).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-prev.
          IF AVAILABLE RatePlan THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(RatePlan).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       RatePlan.RatePlan RatePlan.RPName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRatePlan).

           DELETE RatePlan.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST RatePlan WHERE RatePlan.Brand = lcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 no-MESSAGE.
              leave LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     /* translations */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  
        FIND RatePlan WHERE RECID(RatePlan) = rtab[FRAME-LINE] NO-LOCK.
        RUN Mc/invlang.p(11,RatePlan.RatePlan).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       {Syst/uright2.i}
       RUN local-find-this(FALSE).
       ASSIGN ac-hdr = " CHANGE " ufkey = true.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY 
          RatePlan.RatePlan
          RatePlan.RPName.

       IF lcRight = "RW" THEN DO:

          RUN local-update-record.                                  
          HIDE FRAME lis NO-PAUSE.

          /* If  User Wanted To Cancel this Change Transaction */
          IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
          KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       END.
       ELSE DO:
          PAUSE.
          HIDE FRAME lis NO-PAUSE.
       END. 

       RUN local-disp-row.
       xrecid = recid(RatePlan).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(RatePlan) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(RatePlan) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN leave LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo no-undo.

    if exlock then
      find RatePlan WHERE recid(RatePlan) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find RatePlan WHERE recid(RatePlan) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST RatePlan
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST RatePlan USE-INDEX RPName
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST RatePlan
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST RatePlan USE-INDEX RPName
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT RatePlan
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT RatePlan USE-INDEX RPName
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-prev:
       IF order = 1 THEN FIND prev RatePlan
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND prev RatePlan USE-INDEX RPName
        WHERE RatePlan.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       RatePlan.Brand
       RatePlan.RatePlan
       RatePlan.RPName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-update-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      DISP RatePlan.RPName WITH FRAME lis.

      ASSIGN
         ufk    = 0
         ufk[1] = 7 WHEN lcRight = "RW"
         ufk[8] = 8
         ehto   = 0.
      RUN Syst/ufkey.p.
         
      IF toimi = 1 THEN 
      ChangeType:
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         ehto = 9.
         RUN Syst/ufkey.p.
         
         PROMPT-FOR RatePlan.RPName WITH FRAME lis.

         FIND CURRENT RatePlan EXCLUSIVE-LOCK.
         
         IF CURRENT-CHANGED RatePlan THEN DO:
            
            FIND CURRENT RatePlan NO-LOCK.
            
            MESSAGE 
               "This record has been changed elsewhere while updating" 
            VIEW-AS ALERT-BOX TITLE " UPDATE CANCELLED ".

            UNDO ChangeType, LEAVE ChangeType.
         END. 
         
         ELSE DO:
            IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRatePlan).
        
            ASSIGN FRAME lis RatePlan.RPName.
 
            IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRatePlan).
            
            FIND CURRENT RatePlan NO-LOCK.   
         END. 
      
         LEAVE ChangeType.
      END.
      
      LEAVE.
   END.
 
END PROCEDURE.


