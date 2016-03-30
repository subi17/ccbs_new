/* ----------------------------------------------------------------------
  MODULE .......: iprange.P
  TASK .........: IPRange CUI
  APPLICATION ..: TMS
  AUTHOR .......: anttis
  CREATED ......: 28.05.2012
  Version ......: Yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
/*
{Syst/commpaa.i}
katun = "anttis".
gcBrand = "1".
qupd = True.
*/
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'iprange'}
{Rate/iprange.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhiprange AS HANDLE NO-UNDO.
   lhiprange = BUFFER iprange:HANDLE.
   RUN StarEventInitialize(lhiprange).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhiprange).
   END.
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcIprange  LIKE iprange.NetworkAddress  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
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

form
    iprange.NetworkAddress  /* COLUMN-LABEL FORMAT */
    iprange.NetMask  /* COLUMN-LABEL FORMAT */
    iprange.PLMN     /* COLUMN-LABEL FORMAT */
    iprange.Opername COLUMN-LABEL "OperName" 
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " IP Ranges "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    iprange.NetworkAddress   /* LABEL FORMAT */
    iprange.Netmask   /* LABEL FORMAT */
    iprange.PLMN      /* LABEL FORMAT */
    iprange.OperName LABEL "OperName"

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek iprange  BY  iprange */
    lciprange
    HELP "Enter IP Address"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND IP "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Code,By Name,By 3, By 4".


FIND FIRST iprange
/* srule */ NO-LOCK NO-ERROR.
IF AVAILABLE iprange THEN ASSIGN
   Memory       = recid(iprange)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No IPRange records available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 36 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a iprange  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        
        ehto = 9. RUN Syst/ufkey.
          
        RUN local-UPDATE-record (TRUE).
         
        IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
        KEYLABEL(LASTKEY) EQ "F4" THEN 
        UNDO add-row, LEAVE add-row.

        IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhiprange).

        ASSIGN
        Memory = recid(iprange)
        xrecid = Memory.
        LEAVE.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST iprange
      /* srule */ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE iprange THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND iprange WHERE recid(iprange) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE iprange THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(iprange).
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
        ufk[1]= 35 /* ufk[2]= 30 */ ufk[2] = 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 1752 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW iprange.NetworkAddress {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) iprange.NetworkAddress WITH FRAME sel.
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
        FIND iprange WHERE recid(iprange) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE iprange THEN
              ASSIGN FIRSTrow = i Memory = recid(iprange).
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
           IF NOT AVAILABLE iprange THEN DO:
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
                rtab[1] = recid(iprange)
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
           IF NOT AVAILABLE iprange THEN DO:
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
              rtab[FRAME-DOWN] = recid(iprange).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND iprange WHERE recid(iprange) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE iprange THEN DO:
           Memory = recid(iprange).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE iprange THEN Memory = recid(iprange).
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
           FIND iprange WHERE recid(iprange) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET lciprange WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF lciprange ENTERED THEN DO:
          FIND FIRST iprange WHERE iprange.NetworkAddress BEGINS lciprange
          /* srule */ NO-LOCK NO-ERROR.
          IF NOT AVAILABLE iprange THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some iprange/iprange was found */
          ASSIGN order = 1 Memory = recid(iprange) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}.
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       iprange.NetworkAddress iprange.NetMask .

       RUN local-find-NEXT.
       IF AVAILABLE iprange THEN Memory = recid(iprange).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE iprange THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(iprange).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       iprange.NetworkAddress iprange.NetMask .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhiprange).

           DELETE iprange.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST iprange
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

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       {Syst/uright2.i}
       RUN local-find-this(FALSE).
       
       ASSIGN ac-hdr = " IPRange " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-view-record.                                  
       HIDE FRAME lis NO-PAUSE.
       ufkey = True.

       RUN local-disp-row.
       xrecid = recid(iprange).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(iprange) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(iprange) must-print = TRUE.
        NEXT LOOP.
     END.
      
     ELSE IF LOOKUP(nap,"f7") > 0 AND ufk[7] > 0 THEN DO: 
     
        RUN local-find-this(FALSE).
        
        RUN Mc/eventsel.p("IPRange","#BEGIN").

        ufkey = true.
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
      FIND iprange WHERE recid(iprange) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND iprange WHERE recid(iprange) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST iprange
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST iprange
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT iprange
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV iprange
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       iprange.NetworkAddress
       iprange.NetMask
       iprange.PLMN
       iprange.Opername

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
END PROCEDURE.

PROCEDURE local-VIEW-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      DISP 
         IPRange.NetworkAddress
         IPRange.NetMask
         IPRange.PLMN
         IPRange.OperName
      WITH FRAME lis.
      
      IF ufkey THEN DO:
         
         ASSIGN 
            ehto   = 0
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[7]= 1752
            ufk[8] = 8.
         RUN Syst/ufkey.
      END.
      
      IF toimi = 1 THEN DO:
       
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhiprange).

         ASSIGN ac-hdr = " IPRange " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
         cfc = "lis". RUN Syst/ufcolor.

         RUN local-UPDATE-record(FALSE).
         ufkey = True.
       
         /* IF  User Wanted TO Cancel this Change TRANSACTION */
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
         KEYLABEL(lastkey) = "F4" THEN UNDO, NEXT.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhiprange).
         
      END.   

      ELSE IF toimi = 7 THEN DO: 
     
        RUN local-find-this(FALSE).
        
        RUN Mc/eventsel.p("IPRange",STRING(IPRange.NetWorkAddress)).

        ufkey = true.
        NEXT.
      END.   
      
      ELSE IF toimi = 8 THEN LEAVE MaintMenu.
   END.

   HIDE FRAME lis NO-PAUSE.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   
   DEFINE INPUT PARAMETER plNew AS LOGICAL NO-UNDO.

   DEF VAR ldeLastIP AS DEC NO-UNDO. 
   DEF VAR ldeFirstIP AS DEC NO-UNDO. 
   DEF VAR lcError AS CHAR NO-UNDO. 
      
   IF plNew THEN CLEAR FRAME lis NO-PAUSE.
   
   UPDATE_LOOP:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, RETURN:

      IF NOT plNew THEN
         DISP iprange.NetworkAddress
              iprange.netmask
              iprange.PLMN
              iprange.Opername.
      
      PROMPT-FOR
         IPRange.NetworkAddress WHEN plNew
         IPRange.NetMask
         IPRange.PLMN
         IPRange.OperName.
      
      IF NOT fValidateIPRange
            (INPUT INPUT IPRange.NetworkAddress,
             INPUT INPUT IPRange.NetMask,
             plNew,
             OUTPUT ldeFirstIP,
             OUTPUT ldeLastIP,
             OUTPUT lcError) THEN DO:
         MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
         UNDO, NEXT UPDATE_LOOP.
      END.

      IF plNew THEN DO:
         CREATE iprange.
         ASSIGN
            iprange.NetworkAddress
            IPRange.NetMask
            IPRange.PLMN
            IPRange.OperName
            IPRange.FirstIP = ldeFirstIP
            IPRange.LastIP = ldeLastIP.
         FIND CURRENT IPRange NO-LOCK.
         RETURN.
      END.
      
      FIND CURRENT IPRange EXCLUSIVE-LOCK.
      
      IF CURRENT-CHANGED IPRange THEN DO:
         
         FIND CURRENT IPRange NO-LOCK.
         
         MESSAGE 
            "This record has been changed elsewhere while updating" 
         VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
            
         RETURN.

      END. 
      ELSE DO: 
         ASSIGN
            IPRange.NetMask
            IPRange.PLMN
            IPRange.OperName
            IPRange.LastIP = ldeLastIP
            IPRange.FirstI = ldeFirstIP.
      END.

      FIND CURRENT IPRange NO-LOCK.
      
      RETURN.
   END.

END PROCEDURE.

