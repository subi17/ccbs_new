/* ----------------------------------------------------------------------
  MODULE .......: TriggerItem
  TASK .........: UPDATEs table TriggerItem
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
{lib/tokenchk.i 'Mobsub'}

DEF INPUT PARAMETER     icTriggerConfID   AS CHAR NO-UNDO.
DEF INPUT PARAMETER     iiTriggerEventID  AS INT  NO-UNDO.


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTriggerItem AS HANDLE NO-UNDO.
   lhTriggerItem = BUFFER TriggerItem:HANDLE.
   RUN StarEventInitialize(lhTriggerItem).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhTriggerItem).
   END.
END.

DEF VAR lcTriggerItem  AS CHAR                 NO-UNDO.
DEF VAR icLeagueCode   AS CHAR                 NO-UNDO.
DEF VAR liCustNum      AS INT                  NO-UNDO FORMAT ">>>>>>>>>9".
DEF VAR lcRgName     AS CHAR                   NO-UNDO.
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
DEF VAR lcTriggerItemType AS CHAR FORMAT "X(40)"      NO-UNDO.
DEF VAR lcTriggerItemRule AS CHAR FORMAT "X(40)"      NO-UNDO.
DEF VAR lcLeagueName AS CHAR                   NO-UNDO.
DEF VAR lcCustNum    AS CHAr                   NO-UNDO.
DEF VAR lcCLI        AS CHAR                   NO-UNDO.

DEF BUFFER bTriggerItem FOR TriggerItem.

run local-find-others.
                                
form
    TriggerItem.TriggerEventID FORMAT ">>>>>>>9" COLUMN-LABEL "ID"
    TriggerItem.TriggerConfID  FORMAT "X(6)"     COLUMN-LABEL "ConfID"
    TriggerItem.InvCust                          COLUMN-LABEL "InvCust"
    TriggerItem.Cli            FORMAT "X(12)"
    TriggerItem.StatusCode     FORMAT ">9"       COLUMN-LABEL "ST"
    TriggerItem.Created        FORMAT "99-99-99"
    TriggerItem.Activated      FORMAT "99-99-99"
    TriggerItem.Handled        FORMAT "99-99-99" 

WITH ROW FrmRow width 72 CENTERED  OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TriggerItems for  " + icTriggerConfID +  string(pvm,"99-99-99") + " "
    FRAME sel.

form
    TriggerItem.TriggerEventID    COLON 22
    TriggerItem.TriggerConfID     COLON 22
    TriggerItem.InvCust           COLON 22 LABEL "Invcust" 
    TriggerItem.Cli               COLON 22 
    TriggerItem.Period            COLON 22 
    TriggerItem.Created           COLON 22
    TriggerItem.Activated         COLON 22 
    TriggerItem.Handled           COLON 22
    TriggerItem.StatusCode        COLON 22 
                    
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  TriggerItem */
    "Customer number:" liCustNum
    HELP "Enter Customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Customer "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  TriggerItem */
    "Name:" lcRgName FORMAT "x(30)"
    HELP "Enter name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FUNCTION fZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   /*ZoneName = "".
   
   FIND TaxZone WHERE TaxZone.TaxZone = icTaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN lcZoneName = TaxZone.TZName. 
     */
END FUNCTION.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE TriggerItem THEN ASSIGN
   Memory       = recid(TriggerItem)
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
    
   IF must-add THEN DO:  /* Add a TriggerItem  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:
           
           CREATE TriggerItem.
/*           ASSIGN
           TriggerItem.TriggerConfID   = iiTriggerConfID 
           TriggerItem.TriggerEventID  = NEXT-VALUE(TriggerItem).  */

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTriggerItem).

           ASSIGN
           Memory = recid(TriggerItem)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TriggerItem THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TriggerItem WHERE recid(TriggerItem) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TriggerItem THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TriggerItem).
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
        ufk[1] = 714
        ufk[2] = 30
        ufk[4] = 0
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
        UFK[7] = 0
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TriggerItem.TriggerConfID ;(uchoose.i;) NO-ERROR WITH 
        FRAME sel.
        COLOR DISPLAY VALUE(ccc) TriggerItem.TriggerConfID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TriggerItem.InvCust ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TriggerItem.InvCust WITH FRAME sel.
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
        FIND TriggerItem WHERE recid(TriggerItem) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TriggerItem THEN
              ASSIGN FIRSTrow = i Memory = recid(TriggerItem).
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
           IF NOT AVAILABLE TriggerItem THEN DO:
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
                rtab[1] = recid(TriggerItem)
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
           IF NOT AVAILABLE TriggerItem THEN DO:
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
              rtab[FRAME-DOWN] = recid(TriggerItem).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TriggerItem WHERE recid(TriggerItem) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TriggerItem THEN DO:
           Memory = recid(TriggerItem).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TriggerItem THEN Memory = recid(TriggerItem).
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
           FIND TriggerItem WHERE recid(TriggerItem) = Memory NO-LOCK.
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
       UPDATE liCustNum WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF liCustNum > 0 THEN DO:
       
          FIND FIRST TriggerItem WHERE 
                     TriggerItem.TriggerEventID      = iiTriggerEventID  AND 
                     TriggerItem.TriggerConfID       = icTriggerConfID   AND 
                     TriggerItem.InvCust             = liCustNum 
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE TriggerItem THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(TriggerItem) 
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
/*******                         
       IF lcRgName > "" THEN DO:
       
          FIND FIRST TriggerItem WHERE 
                     TriggerItem.TriggerConfID  = iiTriggerConfID AND 
                     TriggerItem.InvCust >= lcRgName
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE TriggerItem THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(TriggerItem) 
                 must-print = TRUE
                 order      = 2.
          NEXT LOOP.
       END.
**************/
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
    
        RUN local-find-this (FALSE).
            
        UFKEY = TRUE.
        run ufkey.
        NEXT LOOP.
     
     END.

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
    
     END.
 
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

       IF TriggerItem.StatusCode ne 0 THEN DO:
          MESSAGE
          "Delete not allowed " VIEW-AS ALERT-BOX.
          NEXT.
       END.

       COLOR DISPLAY VALUE(ctc)
       TriggerItem.TriggerEventID     
       TriggerItem.TriggerConfID
       TriggerItem.InvCust   
       TriggerItem.Cli
       TriggerItem.Created
       TriggerItem.Activated
       TriggerItem.Handled
       TriggerItem.StatusCode.

       RUN local-find-NEXT.
       IF AVAILABLE TriggerItem THEN Memory = recid(TriggerItem).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TriggerItem THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TriggerItem).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TriggerItem.TriggerEventID TriggerItem.InvCust TriggerItem.Created TriggerItem.Handled 
       TriggerItem.StatusCode TriggerItem.Activated TriggerItem.TriggerConfID .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTriggerItem).

           DELETE TriggerItem.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TriggerItem THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTriggerItem).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTriggerItem).

       RUN local-disp-row.
       xrecid = recid(TriggerItem).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TriggerItem) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TriggerItem) must-print = TRUE.
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
      FIND TriggerItem WHERE recid(TriggerItem) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TriggerItem WHERE recid(TriggerItem) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF   order = 1 THEN FIND FIRST TriggerItem WHERE 
                    TriggerItem.TriggerConfID = icTriggerConfID  AND 
                    TriggerItem.TriggerEventID = iiTriggerEventID
       NO-LOCK NO-ERROR.
      /* ELSE IF order = 2 THEN FIND FIRST TriggerItem USE-INDEX TriggerItemName            WHERE TriggerItem.TriggerConfID = iiTriggerConfID
          NO-LOCK NO-ERROR. */
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST TriggerItem WHERE TriggerItem.TriggerConfID = icTriggerConfID AND TriggerItem.TriggerEventID = iiTriggerEventID NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT TriggerItem WHERE TriggerItem.TriggerConfID = icTriggerConfID AND TriggerItem.TriggerEventID = iiTriggerEventID  NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV TriggerItem WHERE TriggerItem.TriggerConfID = icTriggerConfID and TriggerItem.TriggerEventID = iiTriggerEventID NO-LOCK NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       TriggerItem.TriggerEventID
       TriggerItem.TriggerConfID
       TriggerItem.InvCust
       TriggerItem.Cli
       TriggerItem.Created
       TriggerItem.Activated
       TriggerItem.Handled
       TriggerItem.StatusCode
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST TriggerConf WHERE 
              TriggerConf.TriggerConfID = icTriggerConfID  NO-LOCK NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      TriggerItem.TriggerEventID
      TriggerItem.TriggerConfID
      TriggerItem.InvCust
      TriggerItem.Cli
      TriggerItem.Period 
      TriggerItem.StatusCode      
      TriggerItem.Activated
      TriggerItem.Created  
      TriggerItem.Handled    
      WITH FRAME lis.

      MESSAGE "PRESS ENTER TO CONTINUE" . PAUSE NO-MESSAGE.
      /*******
      IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9. RUN ufkey.
      
         UPDATE
         TriggerItem.TriggerConfID
         TriggerItem.InvCust 
         TriggerItem.Cli
         TriggerItem.Created
         TriggerItem.Handled
         TriggerItem.StatusCode
          
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
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
*******/      
      LEAVE.
   END.
   
END PROCEDURE.

