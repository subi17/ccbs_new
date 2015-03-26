/* ----------------------------------------------------------------------
  MODULE .......: Region
  TASK .........: UPDATEs table Region
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.11.06
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{commali.i}
{timestamp.i}

{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'Region'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRegion AS HANDLE NO-UNDO.
   lhRegion = BUFFER Region:HANDLE.
   RUN StarEventInitialize(lhRegion).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRegion).
   END.

END.

DEF VAR lcRegion     AS CHAR                   NO-UNDO.
DEF VAR lcRgName     AS CHAR                   NO-UNDO.
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
DEF VAR lcZoneName   AS CHAR                   NO-UNDO.

DEF BUFFER bRegion FOR Region.
    
form
    Region.Region   
    Region.RgName 
    Region.TaxZone                 COLUMN-LABEL "Tax Zone"
    lcZoneName      FORMAT "X(25)" COLUMN-LABEL "Zone Name"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  REGIONS  "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    Region.Region  COLON 22
    Region.RgName  COLON 22 
    Region.TaxZone COLON 22 
       VALIDATE(CAN-FIND(TaxZone WHERE TaxZone.TaxZone = INPUT Region.TaxZone),
                "Unknown tax zone")
    lcZoneName      
       FORMAT "X(30)" 
       NO-LABEL
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  Region */
    "Region:" lcRegion FORMAT "x(12)"
    HELP "Enter region"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Region "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  Region */
    "Name:" lcRgName FORMAT "x(30)"
    HELP "Enter name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.



FUNCTION fZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   lcZoneName = "".
   
   FIND TaxZone WHERE TaxZone.TaxZone = icTaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN lcZoneName = TaxZone.TZName. 
   
END FUNCTION.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE Region THEN ASSIGN
   Memory       = recid(Region)
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
    
   IF must-add THEN DO:  /* Add a Region  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR Region.Region WITH FRAME lis EDITING:
           
               READKEY. 
               nap = KEYLABEL(LASTKEY).
               APPLY LASTKEY.
           END.

           IF INPUT FRAME lis Region.Region = ""
           THEN LEAVE add-row.

           IF CAN-FIND(Region USING Region.Region) THEN DO:
              MESSAGE "Region already exists with code"     
                      INPUT FRAME lis Region.Region
              VIEW-AS ALERT-BOX ERROR.
              NEXT. 
           END.
           
           
           CREATE Region.
           Region.Region  = INPUT Region.Region.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhRegion).

           ASSIGN
           Memory = recid(Region)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE Region THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Region WHERE recid(Region) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Region THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Region).
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
        ufk[2] = 30
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Region.Region ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Region.Region WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Region.RgName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) Region.RgName WITH FRAME sel.
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
        FIND Region WHERE recid(Region) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Region THEN
              ASSIGN FIRSTrow = i Memory = recid(Region).
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
           IF NOT AVAILABLE Region THEN DO:
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
                rtab[1] = recid(Region)
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
           IF NOT AVAILABLE Region THEN DO:
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
              rtab[FRAME-DOWN] = recid(Region).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Region WHERE recid(Region) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Region THEN DO:
           Memory = recid(Region).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Region THEN Memory = recid(Region).
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
           FIND Region WHERE recid(Region) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE lcRegion WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF lcRegion > "" THEN DO:
       
          FIND FIRST Region WHERE 
                     Region.Region >= lcRegion
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE Region THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(Region) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       UPDATE lcRgName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
                         
       IF lcRgName > "" THEN DO:
       
          FIND FIRST Region WHERE 
                     Region.RgName >= lcRgName
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE Region THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(Region) 
                 must-print = TRUE
                 order      = 2.
          NEXT LOOP.
       END.

     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       COLOR DISPLAY VALUE(ctc)
       Region.Region Region.TaxZone Region.RgName.

       RUN local-find-NEXT.
       IF AVAILABLE Region THEN Memory = recid(Region).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Region THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Region).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       Region.Region Region.TaxZone Region.RgName.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhRegion).

           DELETE Region.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE Region THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhRegion).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhRegion).

       RUN local-disp-row.
       xrecid = recid(Region).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Region) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Region) must-print = TRUE.
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
      FIND Region WHERE recid(Region) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Region WHERE recid(Region) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST Region NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST Region USE-INDEX RgName 
          NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST Region NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST Region USE-INDEX RgName 
          NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT Region NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT Region USE-INDEX RgName 
          NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV Region NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV Region USE-INDEX RgName 
          NO-LOCK NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       Region.Region
       Region.RgName
       Region.TaxZone
       lcZoneName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   fZoneName(Region.TaxZone).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      Region.Region
      Region.RgName
      Region.TaxZone
      lcZoneName
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9. RUN ufkey.
      
         UPDATE
         Region.RgName 
         Region.TaxZone    
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "TaxZone" THEN DO:
                  fZoneName(INPUT INPUT FRAME lis Region.TaxZone).
                  DISPLAY lcZoneName WITH FRAME lis.
               END.

            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */

         LEAVE. 
      END.
      
      ELSE DO:
         ehto = 5.
         RUN ufkey.
         PAUSE MESSAGE "Press ENTER to continue".
      END. 
      
      LEAVE.
   END.
   
END PROCEDURE.

