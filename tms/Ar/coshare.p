/* ----------------------------------------------------------------------
  MODULE .......: CoShare
  TASK .........: UPDATEs table CoShare
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 31.12.02
  CHANGED ......: 19.09.03/aam brand
                  13.10.03/aam tokens
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'coshare'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCoShare AS HANDLE NO-UNDO.
   lhCoShare = BUFFER CoShare:HANDLE.
   RUN StarEventInitialize(lhCoShare).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhCoShare).
   END.

END.

DEF INPUT PARAMETER iiTargId AS INT NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcTarg       LIKE CoShare.TargType      NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 6.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 8.
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
DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR lcTargName   AS CHAR                   NO-UNDO. 

form
    CoShare.TargType
    CoShare.CoTarg
    lcTargName    COLUMN-LABEL "Name" FORMAT "X(15)"
    CoShare.RsLevel
    CoShare.CoPerc
    CoShare.CoAmt
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    lcTitle
    FRAME sel.

form
    CoShare.TargType   COLON 20
       VALIDATE(LOOKUP(INPUT CoShare.TargType,",C,R,S") > 0,
                "Valid choices are C, R OR S")
    CoShare.CoTarg     COLON 20
       lcTargName NO-LABEL FORMAT "X(30)" 
    CoShare.RsLevel    COLON 20
    CoShare.CoPerc     COLON 20
    CoShare.CoAmt      COLON 20 
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  target */
    lcTarg
    HELP "Enter target type "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND target type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FIND CoTarg WHERE CoTarg.CoTargId = iiTargId NO-LOCK.
ASSIGN lcTitle = " COMM. SHARING FOR: " +
                 CoTarg.TargType + "/" +
                 CoTarg.CoTarg   + "/" +
                 STRING(CoTarg.RsLevel). 

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Target Type ,    ,   , By 4".

FIND FIRST CoShare NO-LOCK WHERE
           CoShare.CoTargId = iiTargId NO-ERROR.

IF AVAILABLE CoShare THEN ASSIGN
   Memory       = recid(CoShare)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CoShare  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR CoShare.TargType
                      CoShare.CoTarg
                      CoShare.RsLevel
           EDITING:
              READKEY.

              IF KEYLABEL(LASTKEY) = "F9" AND 
                 FRAME-FIELD = "CoTarg" 
              THEN DO WITH FRAME lis:

                 siirto = ?. 

                 CASE INPUT CoShare.TargType:
                 WHEN "R" THEN RUN Mc/nnrsse.
                 WHEN "S" THEN RUN Help/nnmyse.
                 WHEN "C" THEN RUN Mc/nnasel.
                 END CASE.

                 IF siirto NE ? THEN DISPLAY siirto ;& CoShare.CoTarg.

                 ehto = 9. 
                 RUN Syst/ufkey.

              END.

              ELSE APPLY LASTKEY.
           END.               


           IF INPUT FRAME lis CoShare.TargType = ""
           THEN LEAVE add-row.

           IF CAN-FIND(
              FIRST CoShare WHERE 
                    CoShare.CoTargID = iiTargId AND 
                    CoShare.TargType = INPUT FRAME lis CoShare.TargType AND
                    CoShare.CoTarg   = INPUT FRAME lis CoShare.CoTarg   AND
                    CoShare.RsLevel  = INPUT FRAME lis CoShare.RsLevel)
           THEN DO:
              MESSAGE 
              "Commission target for" 
              INPUT FRAME lis CoShare.TargType "/"
              INPUT FRAME lis CoShare.CoTarg   "/"
              INPUT FRAME lis CoShare.RsLevel 
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           IF INPUT FRAME lis CoShare.TargType = "C" THEN DO:
              IF NOT CAN-FIND(FIRST Customer WHERE 
                                    Customer.Brand  = gcBrand AND
                                    Customer.CustNum = 
                                INTEGER(INPUT FRAME lis CoShare.CoTarg))
              THEN DO:
                 MESSAGE "Unknown customer."
                 VIEW-AS ALERT-BOX
                 ERROR.
                 NEXT.
              END.
           END.

           ELSE IF INPUT FRAME lis CoShare.TargType = "R" THEN DO:
              IF NOT CAN-FIND(FIRST Reseller WHERE 
                                    Reseller.Brand   = gcBrand AND
                                    Reseller.Reseller = 
                                INPUT FRAME lis CoShare.CoTarg)
              THEN DO:
                 MESSAGE "Unknown reseller."
                 VIEW-AS ALERT-BOX
                 ERROR.
                 NEXT.
              END.

              IF INPUT FRAME lis CoShare.RsLevel = 0 THEN DO:
                 MESSAGE "Reseller level must be chosen."
                 VIEW-AS ALERT-BOX
                 ERROR.
                 NEXT.
              END. 
           END.

           ELSE IF INPUT FRAME lis CoShare.TargType = "S" THEN DO:
              IF NOT CAN-FIND(FIRST Salesman WHERE 
                                    Salesman.Brand   = gcBrand AND
                                    Salesman.Salesman = 
                                INPUT FRAME lis CoShare.CoTarg)
              THEN DO:
                 MESSAGE "Unknown Salesman."
                 VIEW-AS ALERT-BOX
                 ERROR.
                 NEXT.
              END.
           END.

           CREATE CoShare.             
           ASSIGN
           CoShare.CoTargID = iiTargId
           CoShare.TargType = INPUT FRAME lis CoShare.TargType
           CoShare.CoTarg   = INPUT FRAME lis CoShare.CoTarg
           CoShare.RsLevel  = INPUT FRAME lis CoShare.RsLevel.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCoShare).

           ASSIGN
           Memory = recid(CoShare)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CoShare
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CoShare THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CoShare WHERE recid(CoShare) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CoShare THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CoShare).
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
        ufk[1]= 35  ufk[2]= 0  ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CoShare.TargType {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoShare.TargType WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW CoShare.CoTarg {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoShare.CoTarg WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW CoShare.RsLevel {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CoShare.RsLevel WITH FRAME sel.
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
        FIND CoShare WHERE recid(CoShare) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CoShare THEN
              ASSIGN FIRSTrow = i Memory = recid(CoShare).
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
           IF NOT AVAILABLE CoShare THEN DO:
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
                rtab[1] = recid(CoShare)
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
           IF NOT AVAILABLE CoShare THEN DO:
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
              rtab[FRAME-DOWN] = recid(CoShare).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CoShare WHERE recid(CoShare) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CoShare THEN DO:
           Memory = recid(CoShare).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CoShare THEN Memory = recid(CoShare).
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
           FIND CoShare WHERE recid(CoShare) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lcTarg WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lcTarg ENTERED THEN DO:
          FIND FIRST CoShare WHERE 
           CoShare.CoTargId  = iiTargId AND
           CoShare.TargType >= lcTarg
           NO-LOCK NO-ERROR.

          IF NOT AVAILABLE CoShare THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some CoShare/CoShare was found */
          ASSIGN order = 1 Memory = recid(CoShare) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CoShare.TargType CoShare.CoTarg .

       RUN local-find-NEXT.
       IF AVAILABLE CoShare THEN Memory = recid(CoShare).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CoShare THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CoShare).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CoShare.TargType CoShare.CoTarg .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCoShare).

           DELETE CoShare.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CoShare) THEN DO:
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
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCoShare).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CoShare.TargType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCoShare).

       RUN local-disp-row.
       xrecid = recid(CoShare).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CoShare) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CoShare) must-print = TRUE.
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
      FIND CoShare WHERE recid(CoShare) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CoShare WHERE recid(CoShare) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CoShare
        WHERE CoShare.CoTargId = iiTargId
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CoShare
        WHERE CoShare.CoTargId = iiTargId
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CoShare
        WHERE CoShare.CoTargId = iiTargId
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CoShare
        WHERE CoShare.CoTargId = iiTargId
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CoShare.TargType 
       CoShare.CoTarg
       lcTargName 
       CoShare.RsLevel
       CoShare.CoPerc
       CoShare.CoAmt
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   lcTargName = "Unknown". 

   CASE CoShare.TargType:
   WHEN "C" THEN DO:
      FIND Customer WHERE Customer.CustNum = INTEGER(CoShare.CoTarg) 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Customer THEN lcTargName = Customer.CustName.
   END. 
   WHEN "R" THEN DO:
      FIND FIRST Reseller WHERE     
                 Reseller.Brand    = gcBrand AND
                 Reseller.Reseller = CoShare.CoTarg NO-LOCK NO-ERROR.
      IF AVAILABLE Reseller THEN lcTargName = Reseller.RsName.
   END.
   WHEN "S" THEN DO:
      FIND FIRST Salesman WHERE 
                 Salesman.Brand    = gcBrand AND
                 Salesman.Salesman = CoShare.CoTarg 
         NO-LOCK NO-ERROR.
      IF AVAILABLE Salesman THEN lcTargName = Salesman.SmName.
   END.
   OTHERWISE lcTargName = "". 

   END CASE.       

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP CoShare.TargType
           CoShare.CoTarg
           lcTargName 
           CoShare.RsLevel
           CoShare.CoPerc
           CoShare.CoAmt
      WITH FRAME lis.
    
      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN Syst/ufkey.
       
         UPDATE
         CoShare.CoPerc
         CoShare.CoAmt
         WITH FRAME lis.
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
          
      LEAVE.
   END.
END PROCEDURE.

