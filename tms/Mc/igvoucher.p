/* ----------------------------------------------------------------------
  MODULE .......: igvoucher
  TASK .........: UPDATEs table IgVoucher
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 15.11.06
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'IGVoucher'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhIGVoucher AS HANDLE NO-UNDO.
   lhIGVoucher = BUFFER IGVoucher:HANDLE.
   RUN StarEventInitialize(lhIGVoucher).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhIGVoucher).
   END.

END.

DEF INPUT PARAMETER icInvGroup AS CHAR NO-UNDO.

DEF VAR liPaymType   AS INT                    NO-UNDO.
DEF VAR lcSeqPrefix  AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
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
DEF VAR lcPaymType    AS CHAR                   NO-UNDO.
DEF VAR lcCode       AS CHAR                   NO-UNDO. 

DEF BUFFER bIGVoucher FOR IGVoucher.
    
form
    IGVoucher.PaymType   
    lcPaymType           FORMAT "X(30)" COLUMN-LABEL "Description"
    IGVoucher.FromDate
    IGVoucher.SeqPrefix 
    IGVoucher.Voucher               
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " " + icInvGroup + ": Voucher Number Sequences  "  
    FRAME sel.
    
form
    IGVoucher.PaymType  COLON 22
       lcPaymType FORMAT "X(30)" NO-LABEL 
    IGVoucher.FromDate  COLON 22 
    IGVoucher.SeqPrefix COLON 22 FORMAT "X(4)"
    IGVoucher.Voucher   COLON 22 
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  IGVoucher */
    "Type:" liPaymType FORMAT ">>9"
    HELP "Enter type"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Type "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fTypeName RETURNS CHARACTER
   (iiPaymType AS INT):

   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "Payment",
                           "PaymType",
                           STRING(iiPaymType)).
   
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE IGVoucher THEN ASSIGN
   Memory       = recid(IGVoucher)
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
   END.
    
   IF must-add THEN DO:  /* Add a IGVoucher  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR IGVoucher.PaymType 
                      IGVoucher.FromDate
           WITH FRAME lis EDITING:
           
              READKEY. 
              nap = KEYLABEL(LASTKEY).

              IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "PaymType"
              THEN DO:

                 RUN Help/h-tmscodes(INPUT "Payment",  /* TableName */
                                      "PaymType",  /* FieldName */
                                      "AccRec",   /* GroupCode */
                                OUTPUT lcCode).

                 IF lcCode ne "" AND lcCode NE ? THEN DO:
                    DISPLAY lcCode @ IGVoucher.PaymType WITH FRAME lis.   
                 END.   

                 ehto = 9.
                 RUN Syst/ufkey.
                 NEXT. 
              END.

              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
              THEN DO WITH FRAME lis:
             
                 PAUSE 0.
                
                 IF FRAME-FIELD = "PaymType" THEN DO:
                    lcPaymType = fTypeName(INPUT INPUT IGVoucher.PaymType).
                    DISPLAY lcPaymType WITH FRAME lis.
                 END.
              END.

              APPLY LASTKEY.
           END.

           IF INPUT FRAME lis IGVoucher.PaymType = "" OR
              INPUT FRAME lis IGVoucher.FromDate = ?
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST IGVoucher WHERE
                             IGVoucher.Brand    = gcBrand    AND
                             IGVoucher.InvGroup = icInvGroup 
                             USING IGVoucher.PaymType AND
                                   IGVoucher.FromDate) THEN DO:
              MESSAGE "Number sequence already exists with type"     
                      INPUT FRAME lis IGVoucher.PaymType "and date"
                      INPUT FRAME lis IGVoucher.FromDate
              VIEW-AS ALERT-BOX ERROR.
              NEXT. 
           END.
           
           
           CREATE IGVoucher.
           ASSIGN IGVoucher.Brand    = gcBrand
                  IGVoucher.InvGroup = icInvGroup
                  IGVoucher.PaymType = INPUT FRAME lis IGVoucher.PaymType
                  IGVoucher.FromDate = INPUT FRAME lis IGVoucher.FromDate.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhIGVoucher).

           ASSIGN
           Memory = recid(IGVoucher)
           xrecid = Memory.  
           LEAVE.
        END.
        
        IF KEYLABEL(LASTKEY) = "F4" THEN LEAVE. 
        
        
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE IGVoucher THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND IGVoucher WHERE recid(IGVoucher) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE IGVoucher THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(IGVoucher).
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
        ufk    = 0
        ufk[1] = 35
        ufk[2] = 0
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW IGVoucher.PaymType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) IGVoucher.PaymType WITH FRAME sel.
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
        FIND IGVoucher WHERE recid(IGVoucher) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE IGVoucher THEN
              ASSIGN FIRSTrow = i Memory = recid(IGVoucher).
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
           IF NOT AVAILABLE IGVoucher THEN DO:
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
                rtab[1] = recid(IGVoucher)
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
           IF NOT AVAILABLE IGVoucher THEN DO:
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
              rtab[FRAME-DOWN] = recid(IGVoucher).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND IGVoucher WHERE recid(IGVoucher) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE IGVoucher THEN DO:
           Memory = recid(IGVoucher).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE IGVoucher THEN Memory = recid(IGVoucher).
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
           FIND IGVoucher WHERE recid(IGVoucher) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE liPaymType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liPaymType > 0 THEN DO:
       
          FIND FIRST IGVoucher WHERE 
                     IGVoucher.Brand    = gcBrand    AND
                     IGVoucher.InvGroup = icInvGroup AND
                     IGVoucher.PaymType >= liPaymType
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE IGVoucher THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(IGVoucher) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(ctc)
       IGVoucher.PaymType IGVoucher.Voucher IGVoucher.SeqPrefix.

       RUN local-find-NEXT.
       IF AVAILABLE IGVoucher THEN Memory = recid(IGVoucher).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE IGVoucher THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(IGVoucher).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       IGVoucher.PaymType IGVoucher.Voucher IGVoucher.SeqPrefix.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhIGVoucher).

           DELETE IGVoucher.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE IGVoucher THEN DO:
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
       RUN local-find-this((lcRight = "RW")).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhIGVoucher).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhIGVoucher).

       RUN local-disp-row.
       xrecid = recid(IGVoucher).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(IGVoucher) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(IGVoucher) must-print = TRUE.
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
      FIND IGVoucher WHERE recid(IGVoucher) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND IGVoucher WHERE recid(IGVoucher) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN 
       FIND FIRST IGVoucher WHERE
                  IGVoucher.Brand    = gcBrand    AND
                  IGVoucher.InvGroup = icInvGroup NO-LOCK NO-ERROR.
       /*
       ELSE IF order = 2 THEN FIND FIRST IGVoucher USE-INDEX SeqPrefix 
          NO-LOCK NO-ERROR.
       */   
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN 
       FIND LAST IGVoucher WHERE
                 IGVoucher.Brand    = gcBrand    AND
                 IGVoucher.InvGroup = icInvGroup NO-LOCK NO-ERROR.
       
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN 
       FIND NEXT IGVoucher WHERE
                 IGVoucher.Brand    = gcBrand    AND
                 IGVoucher.InvGroup = icInvGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN 
       FIND PREV IGVoucher WHERE
                 IGVoucher.Brand    = gcBrand    AND
                 IGVoucher.InvGroup = icInvGroup NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       IGVoucher.PaymType
       IGVoucher.FromDate
       IGVoucher.SeqPrefix
       IGVoucher.Voucher
       lcPaymType
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   lcPaymType = fTypeName(IGVoucher.PaymType).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      IGVoucher.PaymType
      IGVoucher.FromDate
      IGVoucher.SeqPrefix
      IGVoucher.Voucher
      lcPaymType
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9. RUN Syst/ufkey.
      
         UPDATE
         IGVoucher.SeqPrefix 
         IGVoucher.Voucher    
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "PaymType" THEN DO:
                  lcPaymType = fTypeName(INPUT 
                                         INPUT FRAME lis IGVoucher.PaymType).
                  DISPLAY lcPaymType WITH FRAME lis.
               END.

            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */

         LEAVE. 
      END.
      
      ELSE DO:
         ehto = 5.
         RUN Syst/ufkey.
         PAUSE MESSAGE "Press ENTER to continue".
      END. 
      
      LEAVE.
   END.
   
END PROCEDURE.

