/* ----------------------------------------------------------------------
  MODULE .......: ScUpdRule
  TASK .........: UPDATEs table ScUpdRule
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 07.01.05
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable ScUpdRule

{Syst/commali.i}
{Func/timestamp.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'ScUpdRule'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhScUpdRule AS HANDLE NO-UNDO.
   lhScUpdRule = BUFFER ScUpdRule:HANDLE.
   RUN StarEventInitialize(lhScUpdRule).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhScUpdRule).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcServCom    AS CHAR                   NO-UNDO.
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
DEF VAR lcServName   AS CHAR                   NO-UNDO. 
DEF VAR lcUpdName    AS CHAR                   NO-UNDO. 

DEF BUFFER bScUpdRule FOR ScUpdRule.
    
form
    ScUpdRule.ServCom   
    lcServName COLUMN-LABEL "Name"     FORMAT "X(15)" 
    ScUpdRule.OldValue    
    ScUpdRule.NewValue    
    ScUpdRule.UpdServCom         
    ScUpdRule.UpdValue    
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " Service Update Rules "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    ScUpdRule.ServCom  COLON 18
      lcServName NO-LABEL FORMAT "X(30)" SKIP
    ScUpdRule.OldValue    COLON 18 
    ScUpdRule.NewValue    COLON 18 
    ScUpdRule.UpdServCom  COLON 18
      lcUpdName NO-LABEL FORMAT "X(30)" SKIP
    ScUpdRule.UpdValue    COLON 18 
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  ScUpdRule */
    "Brand .:" lcBrand skip
    "Service:" lcServCom FORMAT "x(12)"
    HELP "Enter service component"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Service "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  ScUpdRule */
    "Brand .:" lcBrand skip
    "Service:" lcServCom FORMAT "X(12)"
    HELP "Enter updated service component"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Updated Service "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.



FUNCTION fServName RETURNS CHARACTER
  (idServCom AS CHAR).

  FIND ServCom WHERE
       ServCom.Brand   = gcBrand AND
       ServCom.ServCom = idServCom NO-LOCK NO-ERROR.
  IF AVAILABLE ServCom
  THEN RETURN ServCom.SCName.
  ELSE RETURN "".
  
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Updated Service ," +
         "  By Linked Service  ,".

RUN local-find-first.

IF AVAILABLE ScUpdRule THEN ASSIGN
   Memory       = recid(ScUpdRule)
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
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ScUpdRule  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR ScUpdRule.ServCom.

           IF INPUT FRAME lis ScUpdRule.ServCom = ""
           THEN LEAVE add-row.

           IF NOT CAN-FIND(ServCom WHERE
                           ServCom.Brand = gcBrand AND
                           ServCom.ServCom = INPUT ScUpdRule.ServCom)
           THEN DO:
              MESSAGE "Unknown service" 
              VIEW-AS ALERT-BOX ERROR.
              NEXT. 
           END.
           
           CREATE ScUpdRule.
           ASSIGN
           ScUpdRule.Brand    = lcBrand
           ScUpdRule.ServCom  = INPUT ScUpdRule.ServCom.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhScUpdRule).

           ASSIGN
           Memory = recid(ScUpdRule)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ScUpdRule THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ScUpdRule WHERE recid(ScUpdRule) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ScUpdRule THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ScUpdRule).
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
        ufk[1]= 70  ufk[2]= 71 ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ScUpdRule.ServCom {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ScUpdRule.ServCom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ScUpdRule.UpdServCom {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ScUpdRule.UpdServCom WITH FRAME sel.
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
        FIND ScUpdRule WHERE recid(ScUpdRule) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ScUpdRule THEN
              ASSIGN FIRSTrow = i Memory = recid(ScUpdRule).
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
           IF NOT AVAILABLE ScUpdRule THEN DO:
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
                rtab[1] = recid(ScUpdRule)
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
           IF NOT AVAILABLE ScUpdRule THEN DO:
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
              rtab[FRAME-DOWN] = recid(ScUpdRule).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ScUpdRule WHERE recid(ScUpdRule) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ScUpdRule THEN DO:
           Memory = recid(ScUpdRule).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ScUpdRule THEN Memory = recid(ScUpdRule).
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
           FIND ScUpdRule WHERE recid(ScUpdRule) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              lcServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcServCom > "" THEN DO:
       
          FIND FIRST ScUpdRule WHERE 
                     ScUpdRule.Brand    = gcBrand AND
                     ScUpdRule.ServCom >= lcServCom
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       DISPLAY lcBrand WITH FRAME F2.
       UPDATE lcBrand WHEN gcAllBrand
              lcServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcServCom > "" THEN DO:
       
          FIND FIRST ScUpdRule WHERE 
                     ScUpdRule.Brand       = gcBrand AND
                     ScUpdRule.UpdServCom >= lcServCom
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */


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
       ScUpdRule.ServCom ScUpdRule.OldValue ScUpdRule.NewValue
       ScUpdRule.UpdServCom ScUpdRule.UpdValue.

       RUN local-find-NEXT.
       IF AVAILABLE ScUpdRule THEN Memory = recid(ScUpdRule).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ScUpdRule THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ScUpdRule).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       ScUpdRule.ServCom ScUpdRule.OldValue ScUpdRule.NewValue
       ScUpdRule.UpdServCom ScUpdRule.UpdValue.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhScUpdRule).

           DELETE ScUpdRule.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE ScUpdRule THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhScUpdRule).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhScUpdRule).

       RUN local-disp-row.
       xrecid = recid(ScUpdRule).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ScUpdRule) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ScUpdRule) must-print = TRUE.
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
      FIND ScUpdRule WHERE recid(ScUpdRule) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ScUpdRule WHERE recid(ScUpdRule) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST ScUpdRule 
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX ServCom NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST ScUpdRule
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX UpdServCom NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST ScUpdRule 
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX ServCom NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST ScUpdRule
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX UpdServCom NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT ScUpdRule 
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX ServCom NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT ScUpdRule
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX UpdServCom NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV ScUpdRule 
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX ServCom NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV ScUpdRule
          WHERE ScUpdRule.Brand   = lcBrand 
          USE-INDEX UpdServCom NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ScUpdRule.ServCom
       lcServName
       ScUpdRule.OldValue
       ScUpdRule.NewValue
       ScUpdRule.UpdServCom 
       ScUpdRule.UpdValue
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

    FIND ServCom OF ScUpdRule NO-LOCK NO-ERROR.
    
    lcServName = fServName(ScUpdRule.ServCom).
       
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      lcUpdName = fServName(ScUpdRule.UpdServCom).
      
      DISP 
      ScUpdRule.ServCom
      lcServName
      ScUpdRule.OldValue
      ScUpdRule.NewValue
      ScUpdRule.UpdServCom 
      lcUpdName
      ScUpdRule.UpdValue
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN Syst/ufkey.p.
      
         UPDATE
         ScUpdRule.OldValue    WHEN NEW ScUpdRule
         ScUpdRule.NewValue    WHEN NEW ScUpdRule
         ScUpdRule.UpdServCom 
         ScUpdRule.UpdValue
         WITH FRAME lis EDITING:
            
            READKEY.
         
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
               IF FRAME-FIELD = "UpdServCom" THEN DO:
                  lcUpdName = fServName(INPUT INPUT FRAME lis 
                                              ScUpdRule.UpdServCom).
                  IF lcUpdName = "" THEN DO:
                     BELL.
                     MESSAGE "Unknown service".
                     NEXT.
                  END. 
                     
                  IF CAN-FIND(FIRST bScUpdRule WHERE
                     bScUpdRule.Brand      = gcBrand                    AND 
                     bScUpdRule.ServCom    = ScUpdRule.ServCom          AND
                     bScUpdRule.OldValue   = INPUT ScUpdRule.OldValue   AND
                     bScUpdRule.NewValue   = INPUT ScUpdRule.NewValue   AND
                     bScUpdRule.UpdServCom = INPUT ScUpdRule.UpdServCom AND
                     RECID(bScUpdRule) NE RECID(ScUpdRule))
                  THEN DO:
                     BELL.
                     MESSAGE "A rule with given values already exists".
                     NEXT.
                  END.
                                  
                  DISPLAY lcUpdName WITH FRAME lis.
               END.

            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */
         
      END.
      
      ELSE DO:
         ehto = 5.
         RUN Syst/ufkey.p.
         PAUSE MESSAGE "Press ENTER to continue".
      END. 
      
      LEAVE.
   END.
   
END PROCEDURE.

