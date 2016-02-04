
/* common cui browse to manage FMItem records */

DEF VAR PriceList  LIKE FMItem.PriceList  NO-UNDO. 
DEF VAR BillCode   LIKE FMItem.BillCode   NO-UNDO.
DEF VAR lcmode     AS CHAR                NO-UNDO INITIAL "general".
lcmode = {1}.



FIND FeeModel WHERE 
    FeeModel.Brand    = "1"  AND 
    FeeModel.FeeModel = FeeModel NO-LOCK.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.

VIEW FRAME sel. 


FIND FIRST FMItem OF FeeModel 
   NO-LOCK NO-ERROR.
IF AVAILABLE FMItem THEN ASSIGN
   memory       = recid(FMItem)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing event items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.      

LOOP:

REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a FMItem  */
       
      RUN local-add-record.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST FMItem OF FeeModel
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FMItem THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FMItem WHERE recid(FMItem) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FMItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FMItem).
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
        ufk[1]= 886 
        ufk[2]=  (IF lcmode = "general" THEN 703 ELSE 0 ) 
        ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" AND lcmode <> "cc" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" AND lcmode <> "cc" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.

      RUN choose-row.

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
        FIND FMItem WHERE recid(FMItem) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE FMItem THEN
              ASSIGN FIRSTrow = i memory = recid(FMItem).
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
           IF NOT AVAILABLE FMItem THEN DO:
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
                rtab[1] = recid(FMItem)
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
           IF NOT AVAILABLE FMItem THEN DO:
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
              rtab[FRAME-DOWN] = recid(FMItem).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND FMItem WHERE recid(FMItem) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FMItem THEN DO:
           memory = recid(FMItem).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FMItem THEN memory = recid(FMItem).
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
           FIND FMItem WHERE recid(FMItem) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND lcmode = "general" THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET PriceList WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF PriceList ENTERED THEN DO:
          FIND FIRST FMItem OF FeeModel WHERE 
                     FMItem.PriceList >= PriceList 
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.


     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND lcmode = "general" THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME F2.
       SET BillCode WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF BillCode ENTERED THEN DO:
          FIND FIRST FMItem OF FeeModel WHERE 
                     FMItem.BillCode >= BillCode 
          USE-INDEX BillCode 
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" AND lcmode <> "cc"  THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" AND lcmode <> "cc" 
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).
      
       /* Highlight */
       RUN highlight-row.

      
       RUN local-find-NEXT.
       IF AVAILABLE FMItem THEN memory = recid(FMItem).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FMItem THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(FMItem).
          END.
       END.

       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       RUN highlight-row.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFMItem).

           DELETE FMItem.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST FMItem OF FeeModel)
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
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFMItem).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFMItem).

       RUN local-disp-row.
       xrecid = recid(FMItem).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(FMItem) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(FMItem) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


/* common procedures -----------------------------------------------------------*/
PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND FMItem WHERE recid(FMItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FMItem WHERE recid(FMItem) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST FMItem
       OF FeeModel NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST FMItem OF FeeModel 
          USE-INDEX BillCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST FMItem
       OF FeeModel NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST FMItem OF FeeModel 
          USE-INDEX BillCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT FMItem
       OF FeeModel NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT FMItem 
       OF FeeModel USE-INDEX BillCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV FMItem
       OF FeeModel NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV FMItem 
       OF FeeModel USE-INDEX BillCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND PriceList WHERE 
          PriceList.Brand     = FMItem.Brand AND
          PriceList.PriceList = FMItem.PriceList 
          NO-LOCK NO-ERROR. 
       FIND BillItem WHERE 
          BillItem.Brand     = FMItem.Brand AND 
          BillItem.BillCode  = FMItem.BillCode 
          NO-LOCK NO-ERROR.
       FIND first ServiceLimitGroup WHERE
                  ServiceLimitGroup.Brand = gcBrand AND  
                  ServiceLimitGroup.GroupCode  =FMItem.ServiceLimitGroup
       NO-LOCK NO-ERROR.
                                
       IF avail ServiceLimitGroup THEN
            lcServicelname = ServiceLimitGroup.GroupName.
       ELSE lcServicelname = "".

       IF Fmitem.Servicelimitgroup ne "" THEN llservlimit = FALSE.
       ELSE llservlimit = TRUE.

END PROCEDURE.


/* end common procedures ----------------------------------------------------------- */


