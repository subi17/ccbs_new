/* ----------------------------------------------------------------------
  MODULE .......: DPBasis.p
  TASK .........: DPBasis
  APPLICATION ..: TMS
  AUTHOR .......: pt
  CREATED ......: 13.07.2001
  CHANGED ......: 20.08.02/aam tuning 
                  22.08.02/tk  index KeyField, find by keyfield
                               check ratebsub
                  20.09.02/aam ccn instead of bdest
                  28.02.03 tk  tokens
                  11.09.03/aam brand 
  Version ......: M15
  ---------------------------------------------------------------------- */

{commali.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'dpbasis'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhDPBasis AS HANDLE NO-UNDO.
   lhDPBasis = BUFFER DPBasis:HANDLE.
   RUN StarEventInitialize(lhDPBasis).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhDPBasis).
   END.

END.



DEF /* new */ SHARED VAR siirto AS CHAR.

DEF INPUT PARAMETER  DPConfNum LIKE DPConf.DPConfNum NO-UNDO . 

DEF VAR KeyField     LIKE DPBasis.KeyField  NO-UNDO.

DEF VAR TargName     AS C                   NO-UNDO.
DEF VAR fixed        AS LO                  NO-UNDO.
DEF VAR xrecid       AS RECID               INIT ?.
DEF VAR x            AS I                   NO-UNDO.
DEF VAR FIRSTrow     AS INT                 NO-UNDO  INIT 0.
DEF VAR FrmRow       AS INT                 NO-UNDO  INIT 3.
DEF VAR FrmDown      AS INT                 NO-UNDO  INIT 11.
DEF VAR order        AS INT                 NO-UNDO  INIT 1.
DEF VAR orders       AS CHAR                NO-UNDO.
DEF VAR maxOrder     AS INT                 NO-UNDO  INIT 1.
DEF VAR ufkey        AS LOG                 NO-UNDO  INIT true.
DEF VAR delrow       AS INT                 NO-UNDO  INIT 0.
DEF VAR pr-order     AS INT                 NO-UNDO.
DEF VAR memory       AS RECID               NO-UNDO.
DEF VAR RowNo        AS INT                 NO-UNDO.
DEF VAR must-print   AS LOG                 NO-UNDO.
DEF VAR must-add     AS LOG                 NO-UNDO.
DEF VAR ac-hdr       AS CHAR                NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24     NO-UNDO.
DEF VAR i            AS INT                 NO-UNDO.
DEF VAR ok           AS LOG FORMAT "Yes/No" NO-UNDO.

FORM
    DPBasis.BillCode   /* column-label format */
    DPBasis.CCN FORMAT ">>>9"     
    TargName          COLUMN-LABEL "Name of Target" FORMAT "x(30)"

WITH ROW FrmRow CENTERED OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " +
    DPConf.DPCName + " Contains: " 
    FRAME sel.

FORM
    "Product Code ...:" DPBasis.BillCode  "OR" at 36 SKIP
    "CCN ............:" DPBasis.CCN FORMAT ">>>9"    SKIP 
WITH 
    OVERLAY 
    ROW 2 
    CENTERED
    NO-LABEL
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FORM /* seek DPBasis  by KeyField */
    KeyField
    HELP "Enter Billing Code or CCN"             
    WITH ROW 4 COL 2 TITLE COLOR VALUE(ctc) " FIND DISCOUNT "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FIND DPConf WHERE DPConf.DPConfNum = DPConfNum NO-LOCK no-error.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.  
VIEW FRAME sel.                              

orders = "By Product,By BDestination,By 3, By 4".

FIND FIRST DPBasis USE-INDEX KeyField
   WHERE DPBasis.DPConfNum = DPConfNum NO-LOCK NO-ERROR.
IF AVAILABLE DPBasis THEN ASSIGN
   memory       = recid(DPBasis)
   must-print   = true
   must-add     = false.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No DPBasis records available !" VIEW-AS ALERT-BOX.
      HIDE FRAME SEL NO-PAUSE.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = false
      must-add     = false.
END.
LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 COL 36 
       " " + ENTRY(order,orders) + " ".
      END.

   IF must-add THEN DO:  /* Add a DPBasis  */
      ASSIGN 
      cfc = "lis" 
      ufkey = true 
      ac-hdr = " ADD (F4: RETURN) " 
      must-add = false.
      RUN ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. 
        RUN ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           /* Special case: new records are NOT allowed if there is
              already a GENERIC discount record */

           FIND FIRST DPBasis OF DPConf  WHERE DPBasis.BasisType = 0 
           EXCLUSIVE-LOCK NO-ERROR.
           IF AVAILABLE DPBasis    

           THEN DO:
              ok = FALSE.
              MESSAGE 
              "There is already a GENERIC discount record."  SKIP(1)
              "You must delete it before adding new records !" SKIP
              "Delete this GENERIC discount now ?"

              VIEW-AS ALERT-BOX
              BUTTONS YES-NO
              UPDATE ok.
              IF NOT ok THEN UNDO add-row, LEAVE add-row.               
              ELSE DELETE DPBasis.
              /* ... and continue with new record ... */
           END.

           /* Ask new Key values */
           PROMPT-FOR 
              DPBasis.BillCode
              DPBasis.CCN   
           WITH FRAME lis EDITING:
              READKEY.     
              IF KEYLABEL(LASTKEY) = "F4" THEN UNDO ADD-ROW, LEAVE ADD-ROW.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 HIDE MESSAGE.

                 IF FRAME-FIELD = "BillCode" AND 
                      INPUT DPBasis.BillCode NE ""                  
                 THEN DO:
                    IF CAN-FIND(FIRST DPBasis WHERE 
                                      DPBasis.DPConfNum = DPConfNum AND
                                      DPBasis.BillCode = input DPBasis.BillCode)
                    THEN DO:
                       MESSAGE "Product" INPUT DPBasis.BillCode 
                       "exists already !"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                    /* check if BillCode is valid */
                    IF NOT CAN-FIND(BillItem WHERE 
                            BillItem.Brand    = gcBrand AND
                            BillItem.BillCode = INPUT DPBasis.BillCode) 
                    THEN DO:
                       MESSAGE 
                       "Unknown BillCode !"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.
                    LEAVE.      
                 END. /* BillCode */

                 ELSE IF FRAME-FIELD = "ccn" AND INPUT DPBasis.CCN NE 0 
                 THEN DO:
                    /* check duplicates */
                    IF CAN-FIND(FIRST DPBasis WHERE 
                                      DPBasis.DPConfNum = DPConfNum AND
                                      DPBasis.CCN = input DPBasis.CCN)
                    THEN DO:
                      MESSAGE "CCN " INPUT DPBasis.CCN 
                      "exists already !"
                      VIEW-AS ALERT-BOX ERROR.
                      NEXT.
                    END.

                    /* is CCN valid ? */
                    IF NOT CAN-FIND(FIRST CCN WHERE 
                                    CCN.Brand = gcBrand AND
                                    CCN.CCN = INPUT DPBasis.CCN)
                    THEN DO:
                       MESSAGE 
                       "Unknown Country or Service !"
                       VIEW-AS ALERT-BOX ERROR.
                       NEXT.
                    END.  

                    LEAVE.
                 END.  /* CCN */  

              END. /* if poisnap */
              APPLY LASTKEY.

           END.  /* editing */

           /*****************************************
           * If CCN, BillCode and BDest all are blank *
           * we shall ask if user wants to create a *
           * GENERIC discount definition which      *
           * is valid for all ISValue                 *
           *****************************************/
           IF INPUT DPBasis.CCN      = 0    AND 
              INPUT DPBasis.BillCode  = ""  
              /* AND INPUT DPBasis.BDest     = "" */
           THEN DO:   
              ok = FALSE.
              MESSAGE
              "You did not enter any key values."      SKIP
              "Do You want to create a GENERIC" SKIP
              "discount that covers ALL calls ?"
              VIEW-AS ALERT-BOX
              BUTTONS YES-NO
              UPDATE ok.
              IF NOT ok THEN UNDO add-row, NEXT add-row.

              /* ok; user wants this. In this case it should be
                 the ONLY DPBasis record with this DPConfNum */
              IF CAN-FIND(FIRST DPBasis OF DPConf) THEN DO:
                 OK = false.
                 MESSAGE 
                 "If You now create a GENERIC discount, all other" SKIP
                 "discounts become unnecessary and they shall be"  SKIP
                 "deleted."                                        SKIP(1)
                 "Delete other discounts ?"
                 VIEW-AS ALERT-BOX
                 BUTTONS YES-NO
                 UPDATE ok.
                 IF NOT ok THEN UNDO add-row, NEXT add-row.
                 /* user wanted to delete ... */
                 FOR EACH DPBasis OF DPConf:
                    DELETE DPBasis.
                 END.   
                 /* ok, now all other discounts are deleted */
              END.   
           END.

           CREATE DPBasis.
           ASSIGN     
           DPBasis.DPConfNum = DPConf.DPConfNum.


           ASSIGN
           DPBasis.BillCode  = INPUT FRAME lis DPBasis.BillCode
           DPBasis.CCN       = INPUT FRAME lis DPBasis.CCN.  

           IF DPBasis.BillCode NE "" THEN 
              ASSIGN DPBasis.BasisType = 1
                     DPBasis.KeyField  = DPBasis.BillCode.  

           ELSE IF DPBasis.CCN     NE 0  THEN 
              ASSIGN DPBasis.BasisType = 2
                     DPBasis.keyfield  = STRING(DPBasis.CCN,"99999999").  

           ELSE  DPBasis.BasisType = 0.

           HIDE FRAME lis.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDPBasis).

           ASSIGN
           memory = recid(DPBasis)
           xrecid = memory.
           LEAVE.
        END.

        LEAVE.

      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST DPBasis
      WHERE DPBasis.DPConfNum = DPConfNum NO-LOCK NO-ERROR.
      IF NOT AVAILABLE DPBasis THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND DPBasis WHERE recid(DPBasis) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE DPBasis THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(DPBasis).
              RUN local-find-NEXT.

          END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           DOWN.
        END.
        UP FRAME-line - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN  
        ufk[1]= 816 ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
        ehto = 3  
        ufkey = false.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW targName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) targName WITH FRAME sel.
      END.

      nap = keylabel(LASTkey).

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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND DPBasis WHERE recid(DPBasis) = memory NO-LOCK.
        DO i = 1 TO FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE DPBasis THEN
              ASSIGN FIRSTrow = i memory = recid(DPBasis).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE DPBasis THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(DPBasis)
                memory  = rtab[1].
           END.
        END.
        ELSE UP 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE DPBasis THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(DPBasis).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND DPBasis WHERE recid(DPBasis) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE DPBasis THEN DO:
           memory = recid(DPBasis).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE DPBasis THEN memory = recid(DPBasis).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND DPBasis WHERE recid(DPBasis) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDkey undo, NEXT LOOP:

ASK-F1:
        REPEAT WITH FRAME F1 
        ON ENDKEY UNDO ask-f1, LEAVE ask-f1.

          cfc = "puyr". RUN ufcolor.
          ehto = 9. RUN ufkey. ufkey = true.
          CLEAR FRAME f1.
          SET KeyField WITH FRAME f1.
          LEAVE.
       END. /* ask-f1 */   

       HIDE FRAME f1 NO-PAUSE.
       IF KeyField ENTERED THEN DO:
          FIND FIRST DPBasis USE-INDEX KeyField WHERE 
                     DPBasis.DPConfNum = DPConfNum AND
                     DPBasis.KeyField  >= KeyField
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE DPBasis THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some DPBasis was found */
          ASSIGN order = 1 memory = recid(DPBasis) must-print = true.
       END.
       NEXT LOOP.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"  THEN DO:  /* ADD */
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-line.
       RUN local-find-this (false).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DPBasis.CCN DPBasis.BillCode .

       RUN local-find-NEXT.
       IF AVAILABLE DPBasis THEN memory = recid(DPBasis).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE DPBasis THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(DPBasis).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       DPBasis.CCN DPBasis.BillCode .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDPBasis).

           DELETE DPBasis.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST DPBasis
           WHERE DPBasis.DPConfNum = DPConfNum) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP. 

           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       MESSAGE
       "No changes are allowed." SKIP(1)
       "You should delete and re-create a row instead"
       VIEW-AS ALERT-BOX INFORMATION.
       NEXT loop.
/*
       /* change */
       {uright2.i}
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN ufkey.
       cfc = "lis". RUN ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY DPBasis.BillCode DPBasis.CCN.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDPBasis).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDPBasis).

       RUN local-disp-row.
       xrecid = recid(DPBasis).
       LEAVE. */
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(DPBasis) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(DPBasis) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:
    DEF INPUT PARAMETER exlock AS LO NO-undo.
    IF exlock THEN
       FIND DPBasis WHERE recid(DPBasis) = rtab[frame-line(sel)] 
       EXCLUSIVE-LOCK.
    ELSE
       FIND DPBasis WHERE recid(DPBasis) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST DPBasis USE-INDEX KeyField
       WHERE DPBasis.DPConfNum = DPConfNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST DPBasis USE-INDEX KeyField
       WHERE DPBasis.DPConfNum = DPConfNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT DPBasis USE-INDEX KeyField
       WHERE DPBasis.DPConfNum = DPConfNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV DPBasis USE-INDEX KeyField       
       WHERE DPBasis.DPConfNum = DPConfNum NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       DPBasis.BillCode
       DPBasis.CCN 
       TargName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   TargName = "ALL Calls".
   IF DPBasis.BillCode NE "" THEN DO:
      FIND BillItem WHERE 
         BillItem.Brand    = gcBrand AND
         BillItem.BillCode = DPBasis.BillCode NO-LOCK NO-ERROR.
      IF AVAIL BillItem THEN targName  = BillItem.BIName.
                     ELSE targName  = "! Unknown BillCode !".
   END.
   ELSE IF DPBasis.CCN NE 0 THEN DO:
      FIND FIRST CCN WHERE  
         CCN.Brand = gcBrand AND
         CCN.CCN = DPBasis.CCN NO-LOCK NO-ERROR.
      IF AVAIL CCN THEN TargName = CCN.CCNName.
                     ELSE targName = "! Unknown CCN !".
   END.   

END PROCEDURE.

PROCEDURE local-update-record:
END PROCEDURE.
