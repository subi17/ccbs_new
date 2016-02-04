/* ----------------------------------------------------------------------
  MODULE .......: TriggerConf
  TASK .........: UPDATEs table TriggerConf
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 13.11.06
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Mobsub'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhTriggerConf AS HANDLE NO-UNDO.
   lhTriggerConf = BUFFER TriggerConf:HANDLE.
   RUN StarEventInitialize(lhTriggerConf).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhTriggerConf).
   END.

END.

DEF VAR lcTriggerConf     AS CHAR                    NO-UNDO.
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
DEF VAR lcTriggerConfType AS CHAR FORMAT "X(40)"    NO-UNDO.
DEF VAR lcTriggerConfRule AS CHAR FORMAT "X(40)"    NO-UNDO.
DEF VAR lcRuleName   AS CHAR      FORMAT "X(20)"    NO-UNDO.
DEF VAR siirto       AS CHAR                   NO-UNDO.

DEF BUFFER bTriggerConf FOR TriggerConf.
    
form
    TriggerConf.TriggerConfID      FORMAT "x(12)" COLUMN-LABEL "ID"
    TriggerConf.TCNAme             FORMAT "X(16)"
    TriggerConf.Prior              FORMAT ">>>9"  COLUMN-LABEL "Prio"
    TriggerConf.EventRule                         COLUMN-LABEL "ER"
    lcRuleName                                    COLUMN-LABEL "EventRule"
    TriggerConf.ValidFrom
    TriggerConf.ValidTo
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TriggerConfs  "  + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    TriggerConf.TriggerConfID           LABEL "ID......."                SKIP
    TriggerConf.TCNAme   FORMAT "X(30)" LABEL "Name....."                SKIP
    TriggerConf.Prior                   LABEL "Prior...."                SKIP
    TriggerConf.EventRule               LABEL "EventRule"  lcRuleName NO-LABEL   SKIP
    TriggerConf.ValidFrom               LABEL "From....."                SKIP
    TriggerConf.ValidTo                 LABEL "To......."                SKIP
                   
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  TriggerConf */
    "TriggerConf:" lcTriggerConf FORMAT ">9"
    HELP "Enter TriggerConf"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND TriggerConf "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  TriggerConf */
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

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE TriggerConf THEN ASSIGN
   Memory       = recid(TriggerConf)
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
    
   IF must-add THEN DO:  /* Add a TriggerConf  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:
           
           CREATE TriggerConf.
           update 
           TriggerConf.TriggerConfID  WITH FRAME lis.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTriggerConf).
           ASSIGN
           Memory = recid(TriggerConf)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TriggerConf THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TriggerConf WHERE recid(TriggerConf) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TriggerConf THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TriggerConf).
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
        ufk[3] = 4004 
        ufk[4] = 4000
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TriggerConf.TriggerConfID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TriggerConf.TriggerConfID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TriggerConf.TCNAme ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TriggerConf.TCNAme WITH FRAME sel.
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
        FIND TriggerConf WHERE recid(TriggerConf) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TriggerConf THEN
              ASSIGN FIRSTrow = i Memory = recid(TriggerConf).
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
           IF NOT AVAILABLE TriggerConf THEN DO:
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
                rtab[1] = recid(TriggerConf)
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
           IF NOT AVAILABLE TriggerConf THEN DO:
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
              rtab[FRAME-DOWN] = recid(TriggerConf).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TriggerConf WHERE recid(TriggerConf) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TriggerConf THEN DO:
           Memory = recid(TriggerConf).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TriggerConf THEN Memory = recid(TriggerConf).
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
           FIND TriggerConf WHERE recid(TriggerConf) = Memory NO-LOCK.
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
       UPDATE lcTriggerConf WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF lcTriggerConf > ""     THEN DO:
       
          FIND FIRST TriggerConf WHERE 
                     TriggerConf.TriggerConfID >= lcTriggerConf
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE TriggerConf THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(TriggerConf) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       UPDATE lcRgName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
                         
       IF lcRgName > "" THEN DO:
       
          FIND FIRST TriggerConf WHERE 
                     TriggerConf.TCNAme >= lcRgName
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE TriggerConf THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(TriggerConf) 
                 must-print = TRUE
                 order      = 2.
          NEXT LOOP.
       END.

     END. /* Search-2 */
     
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
        
        RUN local-find-this (FALSE).

        RUN Mm/triggerfield.p(TriggerConf.TriggerConfID).
        
        UFKEY = TRUE.
        RUN Syst/ufkey.
        NEXT LOOP.
     
     ENd.
     
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
        
        RUN local-find-this (FALSE).

        RUN Mm/triggerevent.p(TriggerConf.TriggerConfID).
        
        UFKEY = TRUE.
        RUN Syst/ufkey.
        NEXT LOOP.
     
     ENd.
     
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
       TriggerConf.TriggerConfID 
       TriggerConf.Prior 
       TriggerConf.EventRule
       lcRuleName
       TriggerConf.TCName  
       TriggerConf.ValidFrom  
       TriggerConf.ValidTo        
       TriggerConf.TriggerConfID.

       RUN local-find-NEXT.
       IF AVAILABLE TriggerConf THEN Memory = recid(TriggerConf).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TriggerConf THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TriggerConf).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TriggerConf.TriggerConfID TriggerConf.Prior TriggerConf.TCName 
       TriggerConf.ValidFrom  TriggerConf.ValidTo       TriggerConf.EventRule  lcRuleName
              .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTriggerConf).

           DELETE TriggerConf.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TriggerConf THEN DO:
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

       xrecid = recid(TriggerConf).
       
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTriggerConf).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  

       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTriggerConf).

       FIND FIRST TriggerConf where recid(triggerconf) = xrecid no-lock no-error.
       
       RUN local-disp-row.

       xrecid = recid(TriggerConf).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TriggerConf) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TriggerConf) must-print = TRUE.
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
      FIND TriggerConf WHERE recid(TriggerConf) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TriggerConf WHERE recid(TriggerConf) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST TriggerConf NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST TriggerConf USE-INDEX TCName 
          NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST TriggerConf NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST TriggerConf USE-INDEX TCName 
          NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT TriggerConf NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT TriggerConf USE-INDEX TCName 
          NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV TriggerConf NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV TriggerConf USE-INDEX TCName 
          NO-LOCK NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       TriggerConf.TriggerConfID
       TriggerConf.TCNAme
       TriggerConf.Prior
       TriggerConf.EventRule
       lcRuleName
       TriggerConf.ValidFrom  
       TriggerConf.ValidTo
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   /*fZoneName(TriggerConf.Prior).*/

   FIND FIRST TMSCOdes WHERE 
              TMSCodes.Tablename = "TriggerConf"        AND 
              TMSCodes.FieldName = "TriggerConfType"    AND 
              TMSCodes.CodeGroup = "TriggerConfType"    AND 
              TMSCodes.CodeValue = STRING(TriggerConf.Prior) NO-LOCK NO-ERROR.
   
   IF AVAIL TMSCodes THEN lcTriggerConftype = TMSCodes.CodeName.
   ELSE                   lcTriggerConftype = "".
   
   FIND FIRST TMSCOdes WHERE 
              TMSCodes.Tablename = "TriggerConf"        AND 
              TMSCodes.FieldName = "EventRule"    AND 
              TMSCodes.CodeGroup = "TriggerConfType"    AND 
              TMSCodes.CodeValue = STRING(TriggerConf.EventRule) NO-LOCK NO-ERROR.
   
   IF AVAIL TMSCodes THEN lcRuleName = TMSCodes.CodeName.
   ELSE                   lcRuleName = "".
                                  
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      disp 
      TriggerConf.TriggerConfID
      TriggerConf.TCNAme
      TriggerConf.Prior
      TriggerConf.EventRule
      lcRuleName
      TriggerConf.ValidFrom  
      TriggerConf.ValidTo    
      WITH FRAME lis.

      IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
      
         ehto = 9. RUN Syst/ufkey.
         PAUSE 0.
         UPDATE
         TriggerConf.TCNAme 
         TriggerConf.Prior
         TriggerConf.EventRule
         TriggerConf.ValidFrom
         TriggerConf.ValidTo
                                            
         WITH FRAME lis EDITING:
 
            READKEY.

            IF FRAME-FIELD = "EventRule" AND keylabel(lastkey) = "F9" THEN DO:
               RUN Help/h-tmscodes(INPUT  "TriggerConf"      ,
                                     "EventRule"  ,
                                     "TriggerConfType"  ,
                              OUTPUT Siirto).
                PAUSE 0.

                ASSIGN
                     TriggerConf.EventRule = INT(siirto) /*
                    lcRuleName            = Tmscodes.codename */ .
                PAUSE 0.         
                DISP TriggerConf.EventRule . 
                NEXT-PROMPT TriggerConf.EventRule. NEXT. 
            END.
            
            PAUSE 0.
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "EventRule" THEN DO:
                  FIND FIRST TMSCOdes WHERE
                             TMSCodes.Tablename = "TriggerConf"        AND
                             TMSCodes.FieldName = "EventRule"    AND
                             TMSCodes.CodeGroup = "TriggerConfType"    AND
                             TMSCodes.CodeValue = STRING(INPUT TriggerConf.EventRule) NO-LOCK NO-ERROR.

                  IF NOT AVAIL Tmscodes THEN DO:
                     BELL.
                     MESSAGE "Unknown Event rule".
                     NEXT-PROMPT EventRule.
                     NEXT.
                  END.
                  PAUSE 0.
                  Disp  TMSCodes.CodeName @ lcRuleName with frame lis.
                  
               END.
               ELSE IF FRAME-FIELD = "TriggerConfRule" THEN DO:

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

