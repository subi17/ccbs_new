/*---------------------------------------------------------------------
  MODULE .......: FATGMember
  TASK .........: UPDATEs table FATGMember
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 28-12-99
  CHANGED ......: 09.09.2002 ccn/prod help menu
                  05.11.2002 jr Eventlog
                  15.01.03   jp Jippii fix
                  16.09.03   Brand 
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable FATGMember

{Syst/commali.i} 
{Syst/eventval.i} 

DEF  NEW  shared VAR siirto AS CHAR.

DEF VAR MemberType   LIKE FATGMember.FTGmember            NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR mtypename    AS C                      NO-UNDO.
DEF VAR mtypenames   AS C                      NO-UNDO.
DEF VAR membername   AS C  FORMAT "X(15)"                    NO-UNDO.
DEF BUFFER xxmember for FATGMember.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhFATGMember AS HANDLE NO-UNDO.
   lhFATGMember = BUFFER FATGMember:HANDLE.
   RUN StarEventInitialize(lhFATGMember).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhFATGMember).
   END.
END.

ASSIGN
mtypenames = "CCN,Product,Fat Group".

form
    FATGMember.Brand 
    FatGmember.ftgrp
    mtypename                       column-label "Target Type"
    FATGMember.FTGMember            column-label "Target "
    membername       FORMAT "X(25)" Column-label "Target Name"  

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) "All members "
    FRAME sel.

{Func/brand.i}

form
    "FAT Group .....:" Fatgmember.ftgrp no-label skip 
    "Target Type....:" FATGMember.MemberType  NO-LABEL
    HELP "0 = CCN, 1 = Product"   
        mtypename NO-LABEL   FORMAT "x(12)"    at 35         SKIP
    "Target ........:" FATGMember.FTGMember     No-LABEL
    Membername   no-label FORMAT "x(24)"  SKIP

WITH  OVERLAY ROW 3 WIDTH 60 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) ac-hdr
    SIDE-LABELS

    FRAME lis.

form /* seek  MemberType */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "FAT Target:" Membertype HELP "Enter TARGET Of Fatime Group "
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.CUICommon:ctc) " FIND TARGET "
    COLOR VALUE(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME f1.
 form
    FATGMember.Memo

    WITH OVERLAY ROW 3 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc)
    " Memo: " + FATGMember.FTGrp + "/" + FATGMember.FTGMember WITH NO-LABELS 1 columns
    FRAME f4.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

orders = "  By Type  ,  By Member  ,By 3, By 4".

FIND FIRST FATGMember 
WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE FATGMember THEN ASSIGN
   Memory       = recid(FATGMember)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a FATGMember  */
      ASSIGN Syst.CUICommon:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           PROMPT-FOR FATGMember.FTGrp.
           IF INPUT FRAME lis FATGMember.FTGrp = "" THEN 
           LEAVE add-row. 

           IF NOT CAN-FIND(FIRST fatgroup WHERE
                                 FatGroup.Brand = lcBrand AND   
                                 fatgroup.ftgrp = INPUT Fatgmember.FTgrp) 
           THEN DO:
              BELL.
              MESSAGE "Unknown Fatime Group '" + input fatgmember.ftgrp + "'".
              NEXT-PROMPT fatgmember.ftgrp. next.
           ENd.             

           CREATE FATGMember.
           ASSIGN
              FATGmember.Brand = gcBrand 
              FATGMember.FTGrp = INPUT frame lis fatgmember.ftgrp.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhFATGMember).
           ASSIGN
              Memory = recid(FATGMember)
              xrecid = Memory.
           LEAVE add-row.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST FATGMembe WHERE 
                 FATGMember.Brand = lcBrand 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FATGMember THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND FATGMember WHERE recid(FATGMember) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE FATGMember THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(FATGMember).
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
        ufk[1]= 1852  ufk[2]= 0 ufk[3]= 0  ufk[4]= 927
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.

      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW FATGMember.FTGMember {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) FATGMember.FTGrp WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND FATGMember WHERE recid(FATGMember) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAIL  FATGMember THEN
              ASSIGN FIRSTrow = i Memory = recid(FATGMember).
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
           IF NOT AVAILABLE FATGMember THEN DO:
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
                rtab[1] = recid(FATGMember)
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
           IF NOT AVAILABLE FATGMember THEN DO:
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
              rtab[FRAME-DOWN] = recid(FATGMember).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND FATGMember WHERE recid(FATGMember) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE FATGMember THEN DO:
           Memory = recid(FATGMember).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE FATGMember THEN Memory = recid(FATGMember).
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
           FIND FATGMember WHERE recid(FATGMember) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       Disp lcBrand With FRAME f1.
       SET lcBrand WHEN gcAllBrand = TRUE
           MemberType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF MemberType ENTERED THEN DO:
          FIND FIRST FATGMember WHERE 
                     FATGMember.FTGmember  >= Membertype   AND 
                     FATGMember.Brand       = lcBrand 
          NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO TRANS ON ENDKEY UNDO, NEXT LOOP:
        {Syst/uright2.i}.

        Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
        ehto = 9. 
        RUN Syst/ufkey.p. ufkey = TRUE.
        RUN local-find-this(TRUE).
        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATGMember).
        UPDATE FATGMember.Memo WITH FRAME f4.
        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATGMember).
        HIDE FRAME f4 NO-PAUSE.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
/*       {Syst/uright2.i} */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.CUICommon:ctc)
        FATGMember.FTGMember FATGmember.Brand  with frame sel.

       RUN local-find-NEXT.
       IF AVAILABLE FATGMember THEN Memory = recid(FATGMember).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE FATGMember THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(FATGMember).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       FATGMember.FTGMember with frame sel.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhFATGMember).
           DELETE FATGMember.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST FATGMember
           WHERE FATGMember.Brand = lcBrand) THEN DO:
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
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
      DISPLAY FATGMember.MemberType .  
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhFATGMember).
       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhFATGMember).
       RUN local-disp-row.
       xrecid = recid(FATGMember).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(FATGMember) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(FATGMember) must-print = TRUE.
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
      FIND FATGMember WHERE recid(FATGMember) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND FATGMember WHERE recid(FATGMember) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST FATGMember 

       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST FATGMember USE-INDEX FTGrp
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST FATGMember 
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST FATGMember USE-INDEX FTGrp
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT FATGMember 
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT FATGMember USE-INDEX FTGrp
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV FATGMember 
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV FATGMember USE-INDEX FTGrp
       WHERE FATGMember.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
         FATGmember.Brand 
         Fatgmember.ftgrp 
         FATGMember.FTGMember
         mtypename
         membername
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   ASSIGN
      mtypename =  ENTRY(FATGMember.MemberType + 1,mtypenames,",").

   IF FATGMember.MemberType = 0 THEN DO:
      FIND FIRST CCN where 
                 CCN.Brand = lcBrand AND 
          STRING(CCN.CCN)  = FATGMember.FTGMember NO-LOCK NO-ERROR.
      IF AVAIL CCN THEN
         membername = CCN.CCNname.
      IF NOT AVAIL CCN THEN membername = "Unknown " + mtypename.    
   END.
   ELSE IF FATGMember.MemberType = 1 THEN DO:
      FIND FIRST BillItem WHERE   
                 BillItem.Brand      = lcBrand AND 
                 BillItem.BillCode   = FATGMember.FTGMember NO-LOCK NO-ERROR.
      IF AVAIL  BillItem   THEN
          membername = BillItem.BIName.
      IF NOT AVAIL BillItem    THEN membername = "Unknown " + mtypename.    
   END.

   ELSE membername = "".

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
      membername WHEN NOT NEW fatgmember
      mtypename  WHEN NOt NEW fatgmember  
      fatgmember.ftgrp 
      WITH FRAME lis. 

      UPDATE
      FATGMember.MemberType
      FATGMember.FTGMember
      WITH FRAME lis
      EDITING:

             READKEY.

             IF FRAME-FIELD = "ftgmember" AND keylabel(lastkey) = "F9"
             THEN DO:

                IF      fatgmember.membertype = 0 THEN RUN Help/nnmase.p.
                ELSE IF fatgmember.membertype = 1 THEN RUN Help/nntuse.p.

                IF CAN-FIND (FIRST xxmember WHERE 
                             xxmember.Brand      = gcBrand               AND 
                             xxmember.membertype = fatgmember.membertype AND
                             xxmember.ftgrp      = FatGmember.ftgrp      AND
                             xxmember.ftgmember  = siirto                AND
                       recid(xxmember) NE recid(FatGmember)) THEN DO:
                    MESSAGE
                    "Fatime Member already exists!".         
                    NEXT-PROMPT fatgmember.ftgmember. NEXT.         
                END.             

                IF siirto ne ? THEN 
                ASSIGN 
                FatGmember.FTGmember = siirto.
                DISP Fatgmember.ftgmember with frame lis.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "MemberType" THEN DO:
                   ASSIGN INPUT FRAME lis fatgmember.membertype.
                   IF fatgmember.membertype > 2 
                   THEN DO:
                      MESSAGE "Unknown member type".
                      NEXT-PROMPT membertype. NEXT.
                   END.
                   Run local-find-others. 
                   DISP mtypename WITH FRAME lis. 

                END.
                ELSE IF FRAME-FIELD = "FTgMember" THEN DO:
                   IF CAN-FIND (FIRST xxmember WHERE
                             xxmember.Brand      = lcBrand               AND 
                             xxmember.membertype = fatgmember.membertype AND
                             xxmember.ftgrp      = FatGmember.ftgrp      AND
                             xxmember.ftgmember  = INPUT FRAME lis 
                             fatgmember.ftgmember                        AND
                             recid(xxmember) NE recid(FatGmember)) THEN DO:
                       MESSAGE 
                       "Fatime Member allready exists!".
                       NEXT-PROMPT fatgmember.ftgmember. NEXT.
                   END.

                   ASSIGN input frame lis fatgmember.ftgmember.

                   IF FATGMember.MemberType = 0 
                   THEN DO:
                      FIND FIRST CCN where 
                             CCN.Brand = lcBrand   AND 
                      STRING(CCN.CCN)  = FATGMember.FTGMember NO-LOCK NO-ERROR.

                      IF NOT AVAIL CCN THEN DO:
                         BELL.
                         MESSAGE 
                         "Unknown 'CCN' Member".
                         NEXT-PROMPT fatgmember.ftgmember. 
                         NEXT.
                      END.
                      DIsp CCN.CCNname @ membername with frame lis.
                   END.

                   ELSE IF  FATGMember.MemberType = 1 THEN DO:
                      FIND FIRST BillItem where
                                 BillItem.Brand    = gcBrand    AND 
                                 BillItem.BillCode = FATGMember.FTGMember 
                      NO-LOCK NO-ERROR.

                      IF NOT AVAIL BillItem THEN DO:
                         BELL.
                         MESSAGE 
                         "Unknown 'Product' Member".
                         NEXT-PROMPT fatgmember.ftgmember. 
                         NEXT.
                      END.                  
                      DIsp BillItem.BIName @ membername with frame lis.
                   END.
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */
      LEAVE.
   END.
END PROCEDURE.

