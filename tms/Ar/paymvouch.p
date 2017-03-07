/* ----------------------------------------------------------------------
  MODULE .......: paymvouch.p
  TASK .........: Update payment voucher number series
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 17.09.03
  CHANGED ......: 30.11.06/aam VoucherType
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'paymvouch'}
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPaymVouch AS HANDLE NO-UNDO.
   lhPaymVouch = BUFFER PaymVouch:HANDLE.
   RUN StarEventInitialize(lhPaymVouch).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPaymVouch).
   END.
END.

DEF TEMP-TABLE ttPaymVouch NO-UNDO
   FIELD Brand   AS CHAR
   FIELD Voucher AS INT
   FIELD VoucherType AS INT
   INDEX Brand Brand VoucherType Voucher.

DEF TEMP-TABLE ttVoucherType NO-UNDO
   FIELD VouchType AS INT
   FIELD TypeName  AS CHAR
   INDEX VouchType VouchType. 
   
DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR lcBrandName  AS CHAR                   NO-UNDO.
DEF VAR liOldVoucher AS INT                    NO-UNDO. 
DEF VAR lcType       AS CHAR                   NO-UNDO.

form
    ttPaymVouch.Brand    FORMAT "X(8)"  COLUMN-LABEL "Brand"
    lcBrandName          FORMAT "X(30)" COLUMN-LABEL "Name"
    ttPaymVouch.VoucherType FORMAT ">9" COLUMN-LABEL "Type"
    lcType               FORMAT "X(10)" COLUMN-LABEL "Name"
    ttPaymVouch.Voucher  FORMAT ">>>>>>>9" COLUMN-LABEL "Voucher"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " Voucher Number Series "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    ttPaymVouch.Brand    FORMAT "X(8)"  COLON 15
    ttPaymVouch.VoucherType FORMAT ">9" COLON 15 
       LABEL "Type" 
       lcType NO-LABEL FORMAT "X(30)" 
    ttPaymVouch.Voucher  FORMAT ">>>>>>>9" COLON 15 
       HELP "Last used payment voucher number" 
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

/* get all brands that user is entitled to */
find TmsUser where TmsUser.UserCode = katun no-lock no-error.

IF NOT AVAIL TMSUSER THEN DO:
   MESSAGE 
   "Unknown UserCode " katun SKIP
   VIEW-AS ALERT-BOX.
   NEXT.
END.

/* voucher types */
FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "Payment" AND
         TMSCodes.FieldName = "VoucherType":
   CREATE ttVoucherType.
   ASSIGN ttVoucherType.VouchType = INTEGER(TMSCodes.CodeValue)
          ttVoucherType.TypeName  = TMSCodes.CodeName.
END.            
         

FOR EACH Brand NO-LOCK WHERE 
         LOOKUP(Brand.Brand,TMSUser.Brand) > 0 OR 
         INDEX("*",TmsUser.Brand) > 0,
    EACH ttVoucherType:
           
   FIND LAST PaymVouch WHERE
             PaymVouch.Brand       = Brand.Brand AND
             PaymVouch.VoucherType = ttVoucherType.VouchType NO-LOCK NO-ERROR.
   CREATE ttPaymVouch.
   ASSIGN ttPaymVouch.Brand       = Brand.Brand
          ttPaymVouch.VoucherType = ttVoucherType.VouchType
          ttPaymVouch.Voucher     = IF AVAILABLE PaymVouch
                                    THEN PaymVouch.Voucher
                                    ELSE 0.
END.             

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code of Section,By Name of Section,By 3, By 4".


FIND FIRST ttPaymVouch
  NO-LOCK NO-ERROR.
IF AVAILABLE ttPaymVouch THEN ASSIGN
   Memory       = recid(ttPaymVouch)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE "No brands available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
   END.


   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttPaymVouch WHERE recid(ttPaymVouch) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttPaymVouch THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttPaymVouch).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttPaymVouch.Brand {Syst/uchoose.i} NO-ERROR
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttPaymVouch.Brand WITH FRAME sel.
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
        FIND ttPaymVouch WHERE recid(ttPaymVouch) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttPaymVouch THEN
              ASSIGN FIRSTrow = i Memory = recid(ttPaymVouch).
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
           IF NOT AVAILABLE ttPaymVouch THEN DO:
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
                rtab[1] = recid(ttPaymVouch)
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
           IF NOT AVAILABLE ttPaymVouch THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttPaymVouch).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttPaymVouch WHERE recid(ttPaymVouch) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttPaymVouch THEN DO:
           Memory = recid(ttPaymVouch).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttPaymVouch THEN Memory = recid(ttPaymVouch).
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
           FIND ttPaymVouch WHERE recid(ttPaymVouch) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(TRUE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY ttPaymVouch.Brand.

       liOldVoucher = ttPaymVouch.Voucher.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       /* moved backwards */
       IF liOldVoucher > ttPaymVouch.Voucher THEN DO:

          Ok = FALSE.

          MESSAGE "Voucher number series will be moved backwards." SKIP
                  "Are You sure that it is what You want to do ?"
          VIEW-AS ALERT-BOX
          QUESTION
          BUTTONS YES-NO
          TITLE " Move Series Backwards "
          UPDATE Ok.

          IF Ok THEN DO:

             FOR EACH PaymVouch WHERE
                      PaymVouch.Brand       = ttPaymVouch.Brand       AND
                      PaymVouch.VoucherType = ttPaymVouch.VoucherType AND
                      PaymVouch.Voucher     > ttPaymVouch.Voucher:

                IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPaymVouch).
                
                DELETE PaymVouch.
             END.
          END.

          ELSE ttPaymVouch.Voucher = liOldVoucher.

       END.

       IF liOldVoucher NE ttPaymVouch.Voucher AND
          NOT CAN-FIND(FIRST PaymVouch WHERE
                             PaymVouch.Brand   = ttPaymVouch.Brand AND
                            PaymVouch.VoucherType = ttPaymVouch.VoucherType AND
                             PaymVouch.Voucher = ttPaymVouch.Voucher)
       THEN DO:

          CREATE PaymVouch.
          ASSIGN PaymVouch.Brand       = ttPaymVouch.Brand
                 PaymVouch.VoucherType = ttPaymVouch.VoucherType 
                 PaymVouch.Voucher     = ttPaymVouch.Voucher.

          IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPaymVouch).

          RELEASE PaymVouch.
       END.

       RUN local-disp-row.
       xrecid = recid(ttPaymVouch).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttPaymVouch) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttPaymVouch) must-print = TRUE.
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
      FIND ttPaymVouch WHERE recid(ttPaymVouch) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttPaymVouch WHERE recid(ttPaymVouch) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ttPaymVouch
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ttPaymVouch
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ttPaymVouch
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ttPaymVouch
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ttPaymVouch.Brand
       lcBrandName 
       ttPaymVouch.VoucherType
       lcType
       ttPaymVouch.Voucher
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND Brand WHERE Brand.Brand = ttPaymVouch.Brand NO-LOCK NO-ERROR.
   lcBrandName = IF AVAILABLE Brand     
                 THEN Brand.BRName
                 ELSE "".
   lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                             "Payment",
                             "VoucherType",
                             STRING(ttPaymVouch.VoucherType)).
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          ttPaymVouch.Brand
          ttPaymVouch.VoucherType
          lcType 
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
             ttPaymVouch.Voucher
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

