/* ----------------------------------------------------------------------
  MODULE .......: PaymConfTax
  TASK .........: UPDATEs table PaymConfTax
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 08.01.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'PaymConfTax'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhPaymConfTax AS HANDLE NO-UNDO.
   lhPaymConfTax = BUFFER PaymConfTax:HANDLE.
   RUN StarEventInitialize(lhPaymConfTax).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhPaymConfTax).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER iiPaymConfig AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 10.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 6.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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

DEF VAR lcTaxZoneName   AS CHAR NO-UNDO.
DEF VAR lcTaxAccName    AS CHAR NO-UNDO.

DEF BUFFER bConfig FOR PaymConfTax.

FORM
    PaymConfTax.TaxZone   COLUMN-LABEL "Tax Zone"
    lcTaxZoneName         FORMAT "X(25)" COLUMN-LABEL "Name" 
    PaymConfTax.TaxAccNum COLUMN-LABEL "Account"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " TAX POSTING RULES " 
    FRAME sel.

FORM
    PaymConfTax.PaymConfig COLON 18 SKIP
    PaymConfTax.TaxZone  COLON 18 
       lcTaxZoneName NO-LABEL FORMAT "X(30)" SKIP
    PaymConfTax.TaxAccNum COLON 18
       lcTaxAccName NO-LABEL FORMAT "X(30)" SKIP
WITH  OVERLAY ROW 12 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " TAX RULE "
    SIDE-LABELS 
    FRAME lis.


FUNCTION fTaxZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   FIND TaxZone WHERE TaxZone.TaxZone = icTaxZone NO-LOCK NO-ERROR.
   
   IF AVAILABLE TaxZone 
   THEN lcTaxZoneName = TaxZone.TZName.
   ELSE lcTaxZoneName = "".
   
END FUNCTION.

FUNCTION fDispTaxZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   fTaxZoneName(icTaxZone).
   
   DISP lcTaxZoneName WITH FRAME lis.
   
END FUNCTION.

FUNCTION fDispTaxAccName RETURNS LOGIC
   (iiTaxAcc AS INT):
   
   IF iiTaxAcc = 0 THEN lcTaxAccName = "".
   
   ELSE DO:
      FIND Account WHERE 
           Account.Brand  = gcBrand AND
           Account.AccNum = iiTaxAcc NO-LOCK NO-ERROR.
      IF AVAILABLE Account 
      THEN lcTaxAccName = Account.AccName.
      ELSE lcTaxAccName = ?.
   END.
   
   DISP lcTaxAccName WITH FRAME lis.
   
END FUNCTION.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE PaymConfTax THEN ASSIGN
   Memory       = recid(PaymConfTax)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No tax configurations available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a PaymConfTax  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY iiPaymConfig @ PaymConfTax.PaymConfig.
            
           CREATE PaymConfTax.
           ASSIGN 
              PaymConfTax.PaymConfig = iiPaymConfig.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              PaymConfTax.TaxZone = "" OR   
              PaymConfTax.TaxZone = ? THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPaymConfTax).

           ASSIGN
           Memory = recid(PaymConfTax)
           xrecid = Memory.  

           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE PaymConfTax THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND PaymConfTax WHERE recid(PaymConfTax) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE PaymConfTax THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(PaymConfTax).
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
        ufk   = 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW PaymConfTax.TaxZone ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) PaymConfTax.TaxZone WITH FRAME sel.
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
        FIND PaymConfTax WHERE recid(PaymConfTax) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE PaymConfTax THEN
              ASSIGN FIRSTrow = i Memory = recid(PaymConfTax).
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
           IF NOT AVAILABLE PaymConfTax THEN DO:
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
                rtab[1] = recid(PaymConfTax)
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
           IF NOT AVAILABLE PaymConfTax THEN DO:
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
              rtab[FRAME-DOWN] = recid(PaymConfTax).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND PaymConfTax WHERE recid(PaymConfTax) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE PaymConfTax THEN DO:
           Memory = recid(PaymConfTax).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE PaymConfTax THEN Memory = recid(PaymConfTax).
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
           FIND PaymConfTax WHERE recid(PaymConfTax) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

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
       PaymConfTax.TaxZone
       PaymConfTax.TaxAccNum.

       RUN local-find-NEXT.
       IF AVAILABLE PaymConfTax THEN Memory = recid(PaymConfTax).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE PaymConfTax THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(PaymConfTax).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       PaymConfTax.TaxZone PaymConfTax.TaxAccNum.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPaymConfTax).

           DELETE PaymConfTax.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE PaymConfTax THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPaymConfTax).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY PaymConfTax.TaxZone.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPaymConfTax).

       RUN local-disp-row.
       xrecid = recid(PaymConfTax).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(PaymConfTax) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(PaymConfTax) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND PaymConfTax WHERE recid(PaymConfTax) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND PaymConfTax WHERE recid(PaymConfTax) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST PaymConfTax 
          WHERE PaymConfTax.PaymConfig = iiPaymConfig
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST PaymConfTax
          WHERE PaymConfTax.PaymConfig = iiPaymConfig
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT PaymConfTax
          WHERE PaymConfTax.PaymConfig = iiPaymConfig
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV PaymConfTax
          WHERE PaymConfTax.PaymConfig = iiPaymConfig
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       PaymConfTax.TaxZone
       lcTaxZoneName
       PaymConfTax.TaxAccNum
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
   fTaxZoneName(PaymConfTax.TaxZone).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llNew AS LOGIC NO-UNDO.
    
   llNew = NEW PaymConfTax.
    
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP PaymConfTax.PaymConfig
           PaymConfTax.TaxZone
           PaymConfTax.TaxAccNum
      WITH FRAME lis.

      fDispTaxZoneName(PaymConfTax.TaxZone).
      fDispTaxAccName(PaymConfTax.TaxAccNum).
      
      IF NOT llNew THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 8 THEN LEAVE.
      END.

      ELSE toimi = 1.
      
      IF toimi = 1 THEN DO:
      
         ehto = 9.
         RUN ufkey.
      
         UPDATE
            PaymConfTax.TaxZone  WHEN NEW PaymConfTax
            PaymConfTax.TaxAccNum
         WITH FRAME lis EDITING:
 
            READKEY.

            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
               PAUSE 0.

               IF FRAME-FIELD = "TaxZone" THEN DO:
                  IF INPUT PaymConfTax.TaxZone = ? OR
                     INPUT PaymConfTax.TaxZone = ""
                  THEN LEAVE.
                  
                  fDispTaxZoneName(INPUT INPUT PaymConfTax.TaxZone).
                  
                  IF lcTaxZoneName = "" THEN DO:
                     MESSAGE "Unknown tax zone"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "TaxAccNum" THEN DO:
                  fDispTaxAccName(INPUT INPUT PaymConfTax.TaxAccNum).

                  IF lcTaxAccName = ? THEN DO:
                     MESSAGE "Unknown account"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

            END.
            
            APPLY LASTKEY.
         END.
 
         IF PaymConfTax.TaxZone = ? OR PaymConfTax.TaxZone = "" THEN LEAVE.   
              
         /* overlapping time period */
         IF CAN-FIND(FIRST bConfig WHERE 
                           bConfig.PaymConfig = iiPaymConfig AND
                           bConfig.TaxZone    = PaymConfTax.TaxZone AND
                           RECID(bConfig) NE RECID(PaymConfTax))      
         THEN DO:
            MESSAGE "Tax configuration has already been defined for"
                    "this tax zone"
            VIEW-AS ALERT-BOX ERROR. 
            NEXT. 
         END.

         IF llNew THEN LEAVE.
      END.

   END.
   
   HIDE FRAME fDesc NO-PAUSE.
   
END PROCEDURE.

