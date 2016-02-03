
/* ----------------------------------------------------------------------
  MODULE .......: ServPac.P
  TASK .........: Service Package
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 17-06-99
  CHANGED ......: 07-10-99 jp urights added
                  21.05.02/tk Event logging added
                  11.11.02/jp sprule
                  10.03.03 tk tokens
                  19.03.03/tk run memo
                  05.09.03 jp brand
                  30.09.03 jp feemodel
                  07.12.04/aam icCLIType,
                               use servel.p instead of servel1.p,
                               FeeModel removed
                  27.03.07/aam don't run ctservel from here
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable ServPac

{Syst/commali.i} 
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ServPac'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhServPac AS HANDLE NO-UNDO.
   lhServPac = BUFFER ServPac:HANDLE.
   RUN StarEventInitialize(lhServPac).

   DEFINE VARIABLE lhServEl AS HANDLE NO-UNDO.
   lhServEl = BUFFER ServEl:HANDLE.
   RUN StarEventInitialize(lhServEl).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhServPac).
   END.
END.

DEF INPUT PARAMETER icCLIType AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ServPac  LIKE ServPac.ServPac  NO-UNDO.
DEF VAR SPName   LIKE ServPac.SPName NO-UNDO.
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
DEF VAR ldtFromDate  AS DATE                   NO-UNDO.
DEF VAR liQty        AS INT                    NO-UNDO. 

form
    ServPac.Brand
    ServPac.ServPac      /* COLUMN-LABEL*/  FORMAT "X(12)"
    ServPac.SPName      /* COLUMN-LABEL FORMAT */
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Service Packages "
    + (IF icCLIType > "" THEN "of '" + icCLIType + "'  " ELSE "")
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i} 

form
    ServPac.ServPac     /* LABEL */  FORMAT "X(12)"
    ServPac.SPName     /* LABEL FORMAT */
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek ServPack  BY  ServPac */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP

    "ServPacket:" ServPac
    HELP "Enter Code of ServPack"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CODE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek ServPack  BY SPName */
    "Brand Code:" lcBrand  HELP "Enter Brand"
    VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
    "ServName..:" SPName
    HELP "Enter Name of ServPack"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

        
cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".


RUN local-find-first.
IF AVAILABLE ServPac THEN ASSIGN
   Memory       = recid(ServPac)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = (lcRight = "RW").

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a ServPac  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR ServPac.ServPac
           VALIDATE
              (ServPac.ServPac NOT ENTERED OR
              NOT CAN-FIND(ServPac using  ServPac.ServPac WHERE 
                           ServPac.Brand = lcBrand ),
              "ServPack " + string(INPUT ServPac.ServPac) +
              " already exists !").
           IF INPUT FRAME lis ServPac.ServPac NOT ENTERED THEN 
           LEAVE add-row.
           CREATE ServPac.
           ASSIGN
           ServPac.Brand   = lcBrand 
           ServPac.ServPac = INPUT FRAME lis ServPac.ServPac.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServPac).

           ASSIGN
           Memory = recid(ServPac)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ServPac
      WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServPac THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServPac WHERE recid(ServPac) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServPac THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServPac).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 250 ufk[4]= 927
        ufk[5]= (IF lcRight = "RW" AND icCLIType = "" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" AND icCLIType = "" THEN 4 ELSE 0)
        ufk[7]= 814
        ufk[8]= 8
        ehto = 3 ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServPac.ServPac ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServPac.ServPac WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ServPac.SPName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServPac.SPName WITH FRAME sel.
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
        FIND ServPac WHERE recid(ServPac) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServPac THEN
              ASSIGN FIRSTrow = i Memory = recid(ServPac).
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
           IF NOT AVAILABLE ServPac THEN DO:
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
                rtab[1] = recid(ServPac)
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
           IF NOT AVAILABLE ServPac THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServPac).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ServPac WHERE recid(ServPac) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServPac THEN DO:
           Memory = recid(ServPac).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServPac THEN Memory = recid(ServPac).
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
           FIND ServPac WHERE recid(ServPac) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       Disp lcBrand With FRAME f1.
       SET lcBrand WHEN gcAllBrand = TRUE
           ServPac WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ServPac ENTERED THEN DO:
          FIND FIRST ServPac WHERE 
                     ServPac.ServPac >= ServPac AND
                     ServPac.Brand    = lcBrand NO-LOCK NO-ERROR.

          IF NOT  fRecFound(1) THEN NEXT Browse.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       Disp lcBrand With frame f2.
       SET lcBrand WHEN gcAllBrand = TRUE
           SPName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF SPName ENTERED THEN DO:
          FIND FIRST ServPac WHERE 
                    ServPac.SPName >= SPName AND
                    ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
          IF NOT  fRecFound(2) THEN NEXT Browse.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:  /* Package Contains */
       RUN local-find-this(FALSE).                                        
       
       /* either general elements */
       IF icCLIType = "" 
       THEN RUN servel(ServPac.ServPac).
      
       /* or elements of a CLI type */
       ELSE DO:
          MESSAGE "Check elelements through packages of clitype"
          VIEW-AS ALERT-BOX INFORMATION.
       END.   

       ufkey = TRUE.
       NEXT LOOP.
     END.

     /* UPDATE memo */
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        RUN local-find-this(FALSE).
        RUN memo(INPUT 0,
                 INPUT "ServPac",
                 INPUT STRING(ServPac.ServPac),
                 INPUT "Service package").
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANSACTION:  /* DELETE */

       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST ServEl WHERE 
                         ServEl.Brand   = gcBrand AND 
                         ServEl.ServPac = ServPac.ServPac) THEN DO:
          MESSAGE
          "This package contains one or more" SKIP
          "Services !"
          VIEW-AS ALERT-BOX ERROR
          TITLE " DELETE IS NOT ALLOWED ".
          NEXT.
       END.

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       ServPac.ServPac ServPac.SPName.

       RUN local-find-NEXT.
       IF AVAILABLE ServPac THEN Memory = recid(ServPac).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServPac THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ServPac).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ServPac.ServPac ServPac.SPName ServPac.Brand .
       IF ok THEN DO:

           FOR EACH ServEl WHERE
                    ServEl.Brand   = gcBrand AND 
                    ServEl.ServPac = ServPac.ServPac.

              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServEl).

              DELETE ServEl.
           END.         

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServPac).

           DELETE ServPac.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServPac
           WHERE ServPac.Brand = lcBrand) THEN DO:
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
       {Syst/uright2.i}
       /* change */
       RUN local-find-this((lcRight = "RW" AND icCLIType = "")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServPac).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ServPac.ServPac.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServPac).

       RUN local-disp-row.
       xrecid = recid(ServPac).
       LEAVE.
     END.

     /* translations */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  
        FIND ServPac WHERE RECID(ServPac) = rtab[FRAME-LINE] NO-LOCK.
        RUN invlang(12,ServPac.ServPac).
          
        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ServPac) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ServPac) must-print = TRUE.
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
      FIND ServPac WHERE recid(ServPac) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServPac WHERE recid(ServPac) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ServPac
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ServPac USE-INDEX SPName
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ServPac
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ServPac USE-INDEX SPName
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ServPac
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ServPac USE-INDEX SPName
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ServPac
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ServPac USE-INDEX SPName
       WHERE ServPac.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServPac.Brand
       ServPac.ServPac
       ServPac.SPName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP ServPac.ServPac
           ServPac.SPName
      WITH FRAME lis.
      
      IF lcRight = "RW" AND icCLIType = "" THEN DO:
      
         ehto = 9.
         RUN ufkey.
         
         UPDATE
            ServPac.SPName
         WITH FRAME lis EDITING:

             READKEY.
             
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
               
             END.
             APPLY LASTKEY.
         END. /* EDITING */
      END.
          
      ELSE PAUSE MESSAGE "Press ENTER to continue".

      LEAVE.
   END.
END PROCEDURE.

