/* ----------------------------------------------------------------------
  MODULE .......: TMSRelation
  TASK .........: TMSRelation table records
  APPLICATION ..: tms
  AUTHOR .......: susanjee
  CREATED ......: 05.03.2018
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable TMSRelation

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'TMSRelation'} 
{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTMSRelation AS HANDLE NO-UNDO.
   lhTMSRelation = BUFFER TMSRelation:HANDLE.
   RUN StarEventInitialize(lhTMSRelation).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhTMSRelation).
   END.

END.

DEFINE INPUT  PARAMETER icTableName   AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icKeyType     AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER icParentValue AS CHARACTER NO-UNDO.

DEF SHARED VAR siirto AS CHAR.

DEF VAR lcTableName   AS CHAR                   NO-UNDO.
DEF VAR lcKeyType     AS CHAR                   NO-UNDO. 
DEF VAR xrecid        AS RECID                           init ?.
DEF VAR FIRSTrow      AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow        AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown       AS INT                    NO-UNDO  init 15.
DEF VAR order         AS INT                    NO-UNDO  init 1.
DEF VAR maxOrder      AS INT                    NO-UNDO  init 2.
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

DEF VAR lcField         AS CHAR NO-UNDO. 
DEF VAR lcCode          AS CHAR NO-UNDO. 
DEF VAR ldDiscValue     AS DEC  NO-UNDO.
DEF VAR lcDPUnit        AS CHAR NO-UNDO.
DEF VAR lcSubject       AS CHAR NO-UNDO.
DEF VAR llSubjectType   AS LOG  NO-UNDO.
DEF VAR llTargetType    AS LOG  NO-UNDO.
DEF VAR llCCDisplay     AS LOG  NO-UNDO.

FORM TMSRelation.TMSRelationID FORMAT ">>>9"  COLUMN-LABEL "ID"
     TMSRelation.TableName     FORMAT "X(20)" COLUMN-LABEL "Table Name"
     TMSRelation.KeyType       FORMAT "X(20)" COLUMN-LABEL "Key Type"
     TMSRelation.ParentValue   FORMAT "X(12)" COLUMN-LABEL "Parent Value"
     TMSRelation.ChildValue    FORMAT "X(12)" COLUMN-LABEL "Child Value"
     WITH ROW FrmRow WIDTH 80 OVERLAY FrmDown DOWN 
     COLOR VALUE(Syst.Var:cfc)   
     TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi           +
                     "  TMS Relation  " + STRING(TODAY,"99-99-99") + " "
     FRAME sel. 

/* {Func/brand.i} */

FORM TMSRelation.TMSRelationID COLON 25 FORMAT ">>>9"  LABEL "Relation ID"
     TMSRelation.TableName     COLON 25 FORMAT "X(20)" LABEL "Table Name"
     TMSRelation.KeyType       COLON 25 FORMAT "X(20)" LABEL "Key Type"
     TMSRelation.ParentValue   COLON 25 FORMAT "X(20)" LABEL "Parent Value"
     TMSRelation.ChildValue    COLON 25 FORMAT "X(20)" LABEL "Child Value"
     TMSRelation.RelationType  COLON 25 FORMAT "X(20)" LABEL "Relation Type"
     TMSRelation.FromTime      COLON 25                LABEL "From Time"
     TMSRelation.ToTime        COLON 25                LABEL "End Time"
     WITH OVERLAY ROW 1 CENTERED
     COLOR VALUE(Syst.Var:cfc)
     TITLE COLOR VALUE(Syst.Var:ctc) ac-hdr 
     SIDE-LABELS 
     FRAME lis.

FORM 
    "TableName..:" lcTableName FORMAT "X(20)" SKIP 
    "KeyType....:" lcKeyType   FORMAT "X(20)"
    WITH row 4 col 2 TITLE COLOR VALUE(Syst.Var:ctc) " FIND TMS Relation "
    COLOR VALUE(Syst.Var:cfc) NO-LABELS OVERLAY FRAME f1.

IF Syst.Var:gcHelpParam > "" THEN ASSIGN
   FrmRow  = 3
   FrmDown = 11.

Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE TMSRelation THEN ASSIGN
   Memory       = recid(TMSRelation)
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

   IF must-add THEN DO: 
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           PROMPT-FOR 
              TMSRelation.TableName 
              TMSRelation.KeyType 
              TMSRelation.ParentValue 
              TMSRelation.ChildValue 
              TMSRelation.RelationType WITH FRAME lis.
           
           IF INPUT TMSRelation.TableName = "" THEN UNDO, LEAVE ADD-ROW.
           
           IF CAN-FIND(FIRST TMSRelation WHERE 
                             TMSRelation.TMSRelationID = INPUT TMSRelation.TMSRelationID)
           THEN DO:
              MESSAGE "Relation ID already exists"
              VIEW-AS ALERT-BOX ERROR.
              NEXT.
           END.
            
           FIND LAST TMSRelation USE-INDEX TMSRelationID NO-LOCK NO-ERROR.
           IF AVAILABLE TMSRelation
           THEN i = TMSRelation.TMSRelationId + 1.
           ELSE i = 1.

           CREATE TMSRelation.
           ASSIGN 
              TMSRelation.TMSRelationID = i 
              TMSRelation.TableName     = INPUT FRAME lis TMSRelation.TableName
              TMSRelation.KeyType       = INPUT FRAME lis TMSRelation.KeyType     
              TMSRelation.ParentValue   = INPUT FRAME lis TMSRelation.ParentValue  
              TMSRelation.ChildValue    = INPUT FRAME lis TMSRelation.ChildValue   
              TMSRelation.RelationType  = INPUT FRAME lis TMSRelation.RelationType 
              TMSRelation.FromTime      = DATETIME-TZ(TODAY, 0)    
              TMSRelation.ToTime        = DATETIME-TZ(12/31/49, 86399000).      

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTMSRelation).

           ASSIGN
           Memory = recid(TMSRelation)
           xrecid = Memory.  
           LEAVE ADD-ROW.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TMSRelation THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND TMSRelation WHERE recid(TMSRelation) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TMSRelation THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TMSRelation).
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
        Syst.Var:ufk    = 0
        Syst.Var:ufk[1] = 816
        Syst.Var:ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
        Syst.Var:ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)  
        Syst.Var:ufk[8] = 8 
        Syst.Var:ehto   = 3 
        ufkey  = FALSE.
        
        /* used as help */
        IF Syst.Var:gcHelpParam > "" THEN ASSIGN
           Syst.Var:ufk[5] = 11
           Syst.Var:ufk[6] = 0
           Syst.Var:ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TMSRelation.TMSRelationID {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) TMSRelation.TMSRelationID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TMSRelation.TMSRelationID {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) TMSRelation.TMSRelationID WITH FRAME sel.
      END.

      Syst.Var:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.Var:nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND TMSRelation WHERE recid(TMSRelation) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TMSRelation THEN
              ASSIGN FIRSTrow = i Memory = recid(TMSRelation).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE TMSRelation THEN DO:
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
                rtab[1] = recid(TMSRelation)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE TMSRelation THEN DO:
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
              rtab[FRAME-DOWN] = recid(TMSRelation).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TMSRelation WHERE recid(TMSRelation) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TMSRelation THEN DO:
           Memory = recid(TMSRelation).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TMSRelation THEN Memory = recid(TMSRelation).
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND TMSRelation WHERE recid(TMSRelation) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(Syst.Var:nap,"1,f1") > 0 AND Syst.Var:ufk[1] > 0 THEN 
     DO ON ENDKEY UNDO, NEXT LOOP:
       Syst.Var:cfc = "puyr". RUN Syst/ufcolor.p.
       Syst.Var:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET lcTableName lcKeyType WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcTableName ENTERED OR
          lcKeyType   ENTERED THEN DO:

          IF lcKeyType  > "" THEN
             FIND FIRST TMSRelation NO-LOCK WHERE 
                        TMSRelation.TableName = lcTableName AND 
                        TMSRelation.KeyType   = lcKeyType   NO-ERROR.
          ELSE 
             FIND FIRST TMSRelation NO-LOCK WHERE 
                        TMSRelation.TableName = lcTableName NO-ERROR.
          
          IF NOT AVAILABLE TMSRelation THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          
          ASSIGN order = 1 Memory = recid(TMSRelation) must-print = TRUE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0 THEN DO:  /* add */
        IF Syst.Var:gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0  
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       TMSRelation.TMSRelationID TMSRelation.TableName.
        
       RUN local-find-NEXT.
       IF AVAILABLE TMSRelation THEN Memory = recid(TMSRelation).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TMSRelation THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TMSRelation).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       TMSRelation.TMSRelationID TMSRelation.TableName.
       
       IF ok THEN DO:
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTMSRelation).

           DELETE TMSRelation.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TMSRelation THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis /*  TRANSACTION */
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       IF Syst.Var:gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSRelation).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.Var:ehto = 9. RUN Syst/ufkey.p.
       Syst.Var:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.
       DISPLAY TMSRelation.TMSRelationID.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSRelation).

       RUN local-disp-row.
       xrecid = recid(TMSRelation).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TMSRelation) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TMSRelation) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.

IF Syst.Var:gcHelpParam > "" THEN DO:
   IF xRecid NE ? THEN DO:
      FIND FIRST TMSRelation WHERE RECID(TMSRelation) = xRecid NO-LOCK.
      siirto = STRING(TMSRelation.TMSRelationID).
   END.   
END.
   
Syst.Var:ehto = 4.
RUN Syst/ufkey.p.

fCleanEventObjects().

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND TMSRelation WHERE recid(TMSRelation) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TMSRelation WHERE recid(TMSRelation) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST TMSRelation WHERE (IF icTableName   > "" THEN TMSRelation.TableName   = icTableName   ELSE TRUE) 
                                              AND (IF icKeyType     > "" THEN TMSRelation.KeyType     = icKeyType     ELSE TRUE)
                                              AND (IF icParentValue > "" THEN TMSRelation.ParentValue = icParentValue ELSE TRUE) USE-INDEX ParentValue NO-LOCK NO-ERROR.                 


END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN FIND LAST TMSRelation WHERE (IF icTableName   > "" THEN TMSRelation.TableName   = icTableName   ELSE TRUE) 
                                             AND (IF icKeyType     > "" THEN TMSRelation.KeyType     = icKeyType     ELSE TRUE)
                                             AND (IF icParentValue > "" THEN TMSRelation.ParentValue = icParentValue ELSE TRUE) USE-INDEX ParentValue NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN FIND NEXT TMSRelation WHERE (IF icTableName   > "" THEN TMSRelation.TableName   = icTableName   ELSE TRUE) 
                                             AND (IF icKeyType     > "" THEN TMSRelation.KeyType     = icKeyType     ELSE TRUE)
                                             AND (IF icParentValue > "" THEN TMSRelation.ParentValue = icParentValue ELSE TRUE) USE-INDEX ParentValue NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN FIND PREV TMSRelation WHERE (IF icTableName   > "" THEN TMSRelation.TableName   = icTableName   ELSE TRUE) 
                                             AND (IF icKeyType     > "" THEN TMSRelation.KeyType     = icKeyType     ELSE TRUE)
                                             AND (IF icParentValue > "" THEN TMSRelation.ParentValue = icParentValue ELSE TRUE) USE-INDEX ParentValue NO-LOCK NO-ERROR.


END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY TMSRelation.TMSRelationID 
           TMSRelation.TableName     
           TMSRelation.KeyType       
           TMSRelation.ParentValue   
           TMSRelation.ChildValue    
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   IF NEW TMSRelation THEN Syst.Var:toimi = -1.
   
   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      DISP TMSRelation.TMSRelationID
           TMSRelation.TableName    
           TMSRelation.KeyType      
           TMSRelation.ParentValue  
           TMSRelation.ChildValue   
           TMSRelation.RelationType 
           TMSRelation.FromTime      
           TMSRelation.ToTime        
      WITH FRAME lis.

      ASSIGN
         Syst.Var:ufk    = 0
         Syst.Var:ufk[1] = 7 WHEN lcRight = "RW"
         Syst.Var:ufk[8] = 8
         Syst.Var:ehto   = 0.
      RUN Syst/ufkey.p.

      IF Syst.Var:toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT TMSRelation EXCLUSIVE-LOCK.
      
         Syst.Var:ehto = 9.
         RUN Syst/ufkey.p.
   
         UPDATE 
            TMSRelation.TableName    
            TMSRelation.KeyType      
            TMSRelation.ParentValue  
            TMSRelation.ChildValue   
            TMSRelation.RelationType 
            TMSRelation.FromTime      
            TMSRelation.ToTime        
         WITH FRAME lis.
 
         LEAVE.

      END.

      ELSE IF Syst.Var:toimi = 8 THEN LEAVE.

   END.
   
END PROCEDURE.

