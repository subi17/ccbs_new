/* ----------------------------------------------------------------------
  MODULE .......: Offer
  TASK .........: UPDATEs table Offer
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 21.01.09
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable Offer

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Offer'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhOffer AS HANDLE NO-UNDO.
   lhOffer = BUFFER Offer:HANDLE.
   RUN StarEventInitialize(lhOffer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhOffer).
   END.

END.

DEF INPUT PARAMETER icOffer  AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilUpdate AS LOG  NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcOffer      AS CHAR                   NO-UNDO.
DEF VAR lcName       AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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


FORM
    Offer.Offer   FORMAT "X(16)"
    Offer.FromDate 
    Offer.ToDate 
    Offer.OfferAmount FORMAT "->>>>>>9.99"
    Offer.Priority    
    Offer.Active
    Offer.Description FORMAT "X(15)"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  OFFERS  " + "  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    Offer.Brand            COLON 18
    Offer.Offer            COLON 18 FORMAT "X(16)"
    Offer.Active           COLON 18 
    Offer.FromDate         COLON 18
    Offer.ToDate           COLON 18 SKIP(1)
    Offer.OfferAmount      COLON 18 
    Offer.VatIncl          COLON 18 LABEL "Tax"
    Offer.Priority         COLON 18 
    Offer.DispItemAmounts  COLON 18 SKIP(1)
    Offer.Description VIEW-AS EDITOR SIZE 60 BY 3
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand:" lcBrand skip
    "Offer:" lcOffer FORMAT "X(16)" 
    HELP "Enter offer ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Offer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM 
    "Brand:" lcBrand skip
    "Offer:" lcName FORMAT "X(20)" 
    HELP "Enter offer name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Offer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

IF icOffer > "" THEN ASSIGN
   FrmRow = 3
   FrmDown = 8.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE Offer THEN ASSIGN
   Memory       = recid(Offer)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No offers available" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a Offer  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ Offer.Brand.

           PROMPT-FOR Offer.Offer WITH FRAME lis.
           IF INPUT Offer.Offer = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST Offer WHERE 
                             Offer.Brand     = lcBrand AND
                             Offer.Offer = INPUT Offer.Offer)
           THEN DO:
              MESSAGE "Rule already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
            
           CREATE Offer.
           ASSIGN 
              Offer.Brand           = lcBrand
              Offer.Offer           = INPUT FRAME lis Offer.Offer
              Offer.FromDate        = TODAY
              Offer.ToDate          = 12/31/2049
              Offer.VatIncl         = FALSE
              Offer.Priority        = 10
              Offer.DispItemAmounts = 1
              Offer.Active          = TRUE.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOffer).

           ASSIGN
           Memory = recid(Offer)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE Offer THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND Offer WHERE recid(Offer) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Offer THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Offer).
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
        ufk[1]= 135
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0.

        ELSE IF NOT ilUpdate THEN ASSIGN
           ufk[5] = 0
           ufk[6] = 0.
           
        IF icOffer > "" THEN ASSIGN
           ufk[1] = 0
           ufk[5] = 0.
           
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Offer.Offer ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Offer.Offer WITH FRAME sel.
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
        FIND Offer WHERE recid(Offer) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Offer THEN
              ASSIGN FIRSTrow = i Memory = recid(Offer).
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
           IF NOT AVAILABLE Offer THEN DO:
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
                rtab[1] = recid(Offer)
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
           IF NOT AVAILABLE Offer THEN DO:
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
              rtab[FRAME-DOWN] = recid(Offer).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Offer WHERE recid(Offer) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Offer THEN DO:
           Memory = recid(Offer).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Offer THEN Memory = recid(Offer).
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
           FIND Offer WHERE recid(Offer) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       SET lcBrand WHEN gcAllBrand 
           lcOffer WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcOffer > "" THEN DO:
          FIND FIRST Offer USE-INDEX Offer WHERE 
                     Offer.Brand = lcBrand AND
                     Offer.Offer >= lcOffer
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST OfferCriteria OF Offer) THEN DO:
          MESSAGE "Criteria definitions exist, deletion not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.

       IF CAN-FIND(FIRST OfferItem OF Offer) THEN DO:
          MESSAGE "Item definitions exist, deletion not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
        
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       Offer.Offer 
       Offer.FromDate Offer.ToDate Offer.OfferAmount .

       RUN local-find-NEXT.
       IF AVAILABLE Offer THEN Memory = recid(Offer).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Offer THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Offer).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Offer.Offer 
       Offer.FromDate Offer.ToDate Offer.OfferAmount.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhOffer).

           DELETE Offer.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE Offer THEN DO:
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
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOffer).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY Offer.Offer.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOffer).

       RUN local-disp-row.
       xrecid = recid(Offer).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Offer) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Offer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN Syst/ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND Offer WHERE recid(Offer) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Offer WHERE recid(Offer) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icOffer > "" THEN 
      FIND FIRST Offer USE-INDEX Offer WHERE 
                 Offer.Brand = lcBrand AND
                 Offer.Offer = icOffer NO-LOCK NO-ERROR.
   
   ELSE IF order = 1 THEN 
      FIND FIRST Offer USE-INDEX Offer WHERE 
                 Offer.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF icOffer > "" THEN 
      FIND LAST Offer USE-INDEX Offer WHERE 
                Offer.Brand = lcBrand AND
                Offer.Offer = icOffer NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND LAST Offer USE-INDEX Offer WHERE 
                Offer.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF icOffer > "" THEN 
      FIND NEXT Offer USE-INDEX Offer WHERE 
                Offer.Brand = lcBrand AND
                Offer.Offer = icOffer NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN 
      FIND NEXT Offer USE-INDEX Offer WHERE 
                Offer.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF icOffer > "" THEN 
      FIND PREV Offer USE-INDEX Offer WHERE 
                Offer.Brand = lcBrand AND
                Offer.Offer = icOffer NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN 
      FIND PREV Offer USE-INDEX Offer WHERE 
                Offer.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       Offer.Offer 
       Offer.FromDate
       Offer.ToDate
       Offer.OfferAmount
       Offer.Priority
       Offer.Active
       Offer.Description
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llDispMenu   AS LOG  NO-UNDO.
   DEF VAR lcNewValue   AS CHAR NO-UNDO.
   DEF VAR ldtNewDate   AS DATE NO-UNDO.

   llDispMenu = NOT NEW Offer.

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         Offer.Brand          
         Offer.Offer 
         Offer.Active
         Offer.FromDate        
         Offer.ToDate           
         Offer.OfferAmount          
         Offer.VatIncl        
         Offer.Priority          
         Offer.DispItemAmounts        
         Offer.Description
      WITH FRAME lis.

      IF llDispMenu THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW" AND ilUpdate
            ufk[3] = 1998
            ufk[4] = 1997
            ufk[4] = 1997
            ufk[6] = 1752
            ufk[7] = 1522 WHEN ilUpdate
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
      ELSE ASSIGN toimi      = 1
                  llDispMenu = TRUE.
                  
      IF toimi = 1 THEN DO TRANS:
         RUN pUpdate.
      END.
            
      ELSE IF toimi = 3 THEN RUN Mc/offercriteria (Offer.Offer,
                                                ilUpdate).
      
      ELSE IF toimi = 4 THEN RUN Mc/offeritem.p (Offer.Offer,
                                            ilUpdate).
                         
      ELSE IF toimi = 6 THEN DO: 
         RUN Mc/eventsel.p("offer", "#BEGIN" + chr(255) + Offer.Brand + chr(255) + Offer.Offer).
      END.   

      /* functions */
      ELSE IF toimi = 7 THEN do:
      end.
      
      ELSE IF toimi = 8 THEN LEAVE.
   END.

END PROCEDURE.

PROCEDURE pUpdate:

   DEF VAR llUpdateAmount AS LOG  NO-UNDO.

   FIND CURRENT Offer EXCLUSIVE-LOCK.
      
   llUpdateAmount = (NEW Offer OR
                     NOT CAN-FIND(FIRST OfferItem OF Offer)).
   ehto = 9.
   RUN Syst/ufkey.
   
   REPEAT ON ENDKEY UNDO, LEAVE:
   
      UPDATE
         Offer.Active
         Offer.FromDate        
         Offer.ToDate  
         Offer.OfferAmount 
         Offer.VatIncl 
         Offer.Priority          
         Offer.DispItemAmounts        
         Offer.Description
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.

      Offer.Description = TRIM(Offer.Description).
   
      LEAVE.
   END.

END PROCEDURE.

