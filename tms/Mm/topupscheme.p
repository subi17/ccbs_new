/* ----------------------------------------------------------------------
  MODULE .......: TopupScheme
  TASK .........: UPDATEs table TopupScheme
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 20.10.08
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable TopupScheme

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TopupScheme'}
{Syst/eventval.i}

DEF BUFFER bItemValue FOR TMRItemValue.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTopupScheme AS HANDLE NO-UNDO.
   lhTopupScheme = BUFFER TopupScheme:HANDLE.
   RUN StarEventInitialize(lhTopupScheme).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhTopupScheme).
   END.

END.

DEF shared VAR siirto AS CHAR.

DEF VAR lcTopupScheme AS CHAR                   NO-UNDO.
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 1.
DEF VAR ufkey         AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow        AS INT                    NO-UNDO  init 0.
DEF VAR pr-order      AS INT                    NO-UNDO.
DEF VAR Memory        AS RECID                  NO-UNDO.
DEF VAR RowNo         AS INT                    NO-UNDO.
DEF VAR must-print    AS LOG                    NO-UNDO.
DEF VAR must-add      AS LOG                    NO-UNDO.
DEF VAR ac-hdr        AS CHAR                   NO-UNDO.
DEF VAR rtab          AS RECID EXTENT 24        NO-UNDO.
DEF VAR i             AS INT                    NO-UNDO.
DEF VAR ok            AS log format "Yes/No"    NO-UNDO.

DEF VAR lcFrameField    AS CHAR NO-UNDO. 
DEF VAR lcHelpField     AS CHAR NO-UNDO.
DEF VAR lcCode          AS CHAR NO-UNDO. 
DEF VAR lcPrefix        AS CHAR NO-UNDO.
DEF VAR lcSource        AS CHAR NO-UNDO.

FORM
    TopupScheme.TopupScheme   FORMAT "X(16)"
    TopupScheme.Description   FORMAT "X(18)"
    TopupScheme.PPReqPrefix   
    TopupScheme.TopupSource   
    TopupScheme.ToDate       COLUMN-LABEL "Valid To"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN 
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TOPUP SCHEMES  " + "  " +
       string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

FORM
    TopupScheme.Brand          COLON 20
    TopupScheme.TopupScheme    COLON 20 FORMAT "X(16)"
    TopupScheme.FromDate       COLON 20
    TopupScheme.ToDate         COLON 20
    TopupScheme.PPReqPrefix    COLON 20
       lcPrefix NO-LABEL FORMAT "X(30)" SKIP
    TopupScheme.TopupSource    COLON 20 
       lcSource NO-LABEL FORMAT "X(30)" SKIP
    TopupScheme.VatIncl        COLON 20 LABEL "Tax" SKIP(1)
    TopupScheme.Description VIEW-AS EDITOR SIZE 60 BY 3
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "Brand :" lcBrand skip
    "Scheme:" lcTopupScheme FORMAT "X(16)" 
    HELP "Enter scheme ID"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Scheme "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FUNCTION fPrefixName RETURNS LOGIC
   (icPrefix AS CHAR):
   
   lcPrefix = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "PrepaidRequest",
                               "PPReqPrefix",
                               icPrefix).
   DISPLAY lcPrefix WITH FRAME lis.
   
END FUNCTION.

FUNCTION fSourceName RETURNS LOGIC
   (icSource AS CHAR):
   
   lcSource = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                               "PrepaidRequest",
                               "Source",
                               icSource).
   DISPLAY lcSource WITH FRAME lis.
   
END FUNCTION.

IF gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.


RUN local-Find-First.

IF AVAILABLE TopupScheme THEN ASSIGN
   Memory       = recid(TopupScheme)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No rules available!" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a TopupScheme  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY lcBrand @ TopupScheme.Brand.

           PROMPT-FOR TopupScheme.TopupScheme WITH FRAME lis.
           IF INPUT TopupScheme.TopupScheme = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST TopupScheme WHERE 
                  TopupScheme.Brand = lcBrand AND
                  TopupScheme.TopupScheme = INPUT TopupScheme.TopupScheme)
           THEN DO:
              MESSAGE "Scheme already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
            
           CREATE TopupScheme.
           ASSIGN 
              TopupScheme.Brand       = lcBrand
              TopupScheme.TopupScheme = INPUT FRAME lis TopupScheme.TopupScheme
              TopupScheme.FromDate    = TODAY
              TopupScheme.ToDate      = 12/31/2049.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTopupScheme).

           ASSIGN
           Memory = recid(TopupScheme)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TopupScheme THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND TopupScheme WHERE recid(TopupScheme) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TopupScheme THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TopupScheme).
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
        ufk[1] = 816
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        ufk[7] = 0  
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TopupScheme.TopupScheme {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TopupScheme.TopupScheme WITH FRAME sel.
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
        FIND TopupScheme WHERE recid(TopupScheme) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TopupScheme THEN
              ASSIGN FIRSTrow = i Memory = recid(TopupScheme).
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
           IF NOT AVAILABLE TopupScheme THEN DO:
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
                rtab[1] = recid(TopupScheme)
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
           IF NOT AVAILABLE TopupScheme THEN DO:
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
              rtab[FRAME-DOWN] = recid(TopupScheme).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TopupScheme WHERE recid(TopupScheme) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TopupScheme THEN DO:
           Memory = recid(TopupScheme).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TopupScheme THEN Memory = recid(TopupScheme).
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
           FIND TopupScheme WHERE recid(TopupScheme) = Memory NO-LOCK.
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
           lcTopupScheme WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcTopupScheme > "" THEN DO:
          FIND FIRST TopupScheme WHERE 
                     TopupScheme.Brand = lcBrand AND
                     TopupScheme.TopupScheme >= lcTopupScheme
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           RUN local-find-this (FALSE).
           IF AVAILABLE TopupScheme THEN 
              siirto = TopupScheme.TopupScheme.
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

       IF CAN-FIND(FIRST OfferItem WHERE 
                         OfferItem.Brand    = gcBrand AND
                         OfferItem.ItemType = "Topup" AND
                         OfferItem.ItemKey = TopupScheme.TopupScheme)
       THEN DO:
          MESSAGE "Offer definitions exist. Delete not allowed."
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       TopupScheme.TopupScheme TopupScheme.Description
       TopupScheme.PPReqPrefix.
        
       RUN local-find-NEXT.
       IF AVAILABLE TopupScheme THEN Memory = recid(TopupScheme).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TopupScheme THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TopupScheme).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TopupScheme.TopupScheme TopupScheme.Description
       TopupScheme.PPReqPrefix.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTopupScheme).

           DELETE TopupScheme.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TopupScheme THEN DO:
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
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          RUN local-find-this (FALSE).
          IF AVAILABLE TopupScheme THEN 
             siirto = TopupScheme.TopupScheme.
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTopupScheme).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY TopupScheme.TopupScheme.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTopupScheme).

       RUN local-disp-row.
       xrecid = recid(TopupScheme).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TopupScheme) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TopupScheme) must-print = TRUE.
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
      FIND TopupScheme WHERE recid(TopupScheme) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TopupScheme WHERE recid(TopupScheme) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST TopupScheme WHERE 
                 TopupScheme.Brand = lcBrand 
      NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST TopupScheme WHERE 
                TopupScheme.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT TopupScheme WHERE 
                TopupScheme.Brand = lcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV TopupScheme WHERE 
                TopupScheme.Brand = lcBrand NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       TopupScheme.TopupScheme 
       TopupScheme.Description
       TopupScheme.PPReqPrefix
       TopupScheme.TopupSource
       TopupScheme.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR lcValue AS CHAR NO-UNDO.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP 
         TopupScheme.Brand          
         TopupScheme.TopupScheme        
         TopupScheme.FromDate
         TopupScheme.ToDate              
         TopupScheme.PPReqPrefix        
         TopupScheme.TopupSource          
         TopupScheme.VatIncl    
         TopupScheme.Description   
      WITH FRAME lis.

      fPrefixName(TopupScheme.PPReqPrefix).
      fSourceName(TopupScheme.TopupSource).
      
      IF NEW TopupScheme THEN toimi = 1.

      ELSE DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[4] = 1728
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.
      END.
                  
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT TopupScheme EXCLUSIVE-LOCK.
      
         ehto = 9.
         RUN Syst/ufkey.
   
         UPDATE
            TopupScheme.FromDate
            TopupScheme.ToDate              
            TopupScheme.PPReqPrefix        
            TopupScheme.TopupSource          
            TopupScheme.VatIncl    
            TopupScheme.Description   
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"PPReqPrefix,TopupSource") > 0 
            THEN DO:

               ASSIGN 
                  lcFrameField = FRAME-FIELD
                  lcHelpField  = IF FRAME-FIELD = "TopupSource"
                                 THEN "Source" 
                                 ELSE FRAME-FIELD.
                                 
               RUN Help/h-tmscodes.p(INPUT "PrepaidRequest", /* TableName */
                                    lcHelpField,      /* FieldName */
                                    "Prepaid",        /* GroupCode */
                              OUTPUT lcCode).
              
               IF lcCode ne "" AND lcCode NE ? THEN DO WITH FRAME lis:
                  IF lcFrameField = "PPReqPrefix"
                  THEN DISPLAY lcCode @ TopupScheme.PPReqPrefix.
                  ELSE DISPLAY lcCode @ TopupScheme.TopupSource.
               END.
 
               ehto = 9.
               RUN Syst/ufkey.

               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.

               IF FRAME-FIELD = "PPReqPrefix" THEN DO:
                  fPrefixName(INPUT INPUT TopupScheme.PPReqPrefix).
                  IF lcPrefix = "" THEN DO:
                     MESSAGE "Unknown prefix"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "TopupSource" THEN DO:
                  fSourceName(INPUT INPUT TopupScheme.TopupSource).
                  IF lcSource = "" THEN DO:
                     MESSAGE "Unknown source"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.

            END.
            
            APPLY LASTKEY.
         END.
   
         IF NEW TopupScheme THEN LEAVE MaintMenu.
         LEAVE.
      END.

      ELSE IF toimi = 4 THEN 
         RUN Mm/topupschemerow(TopupScheme.TopupScheme).

      ELSE IF toimi = 8 THEN LEAVE.  

   END.
   
END PROCEDURE.

