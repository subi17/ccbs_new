/* ----------------------------------------------------------------------
  MODULE .......: fieldselection.p
  TASK .........: mark and select fields into a list
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 08.05.08
  CHANGED ......: 
  Version ......: yoigo
  ---------------------------------------------------------------------- */
{Syst/commali.i}


DEF INPUT  PARAMETER icTable        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icTitle        AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icCurrentList  AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icExtraFields AS CHAR NO-UNDO.
DEF INPUT  PARAMETER icAllowed      AS CHAR NO-UNDO.
DEF INPUT  PARAMETER iiMaxSelected  AS INT  NO-UNDO.
DEF INPUT  PARAMETER icFunctions    AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER ocNewSelection AS CHAR NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
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

DEF VAR lcField      AS CHAR   NO-UNDO.
DEF VAR lhTable      AS HANDLE NO-UNDO.
DEF VAR lhField      AS HANDLE NO-UNDO.
DEF VAR liCnt        AS INT    NO-UNDO.
DEF VAR liTable      AS INT    NO-UNDO.
DEF VAR lcFieldData  AS CHAR   NO-UNDO.
DEF VAR llDispTable  AS LOG    NO-UNDO.

DEF TEMP-TABLE ttField NO-UNDO
   FIELD TableName     AS CHAR 
   FIELD FieldName     AS CHAR
   FIELD FieldLabel    AS CHAR
   FIELD FieldHelp     AS CHAR
   FIELD Selected      AS LOG 
   FIELD OrigSelection AS LOG
   INDEX FieldName FieldName.
   
FORM
    ttField.Selected    FORMAT "       */"    COLUMN-LABEL "Selected"
    ttField.FieldName   FORMAT "X(20)" COLUMN-LABEL "Field"
    ttField.FieldLabel  FORMAT "X(27)" COLUMN-LABEL "Label"   
    lcFieldData         FORMAT "X(20)" COLUMN-LABEL "Description"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " SELECT " + icTitle + " "
    FRAME sel.

form
    ttField.FieldName  COLON 13 LABEL "Field" FORMAT "X(20)"
    ttField.FieldLabel COLON 13 LABEL "Label" FORMAT "X(30)"
    ttField.FieldHelp  COLON 13 LABEL "Description" FORMAT "X(60)"
WITH  OVERLAY ROW 6 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form 
    "Field:" lcField FORMAT "X(20)"
    HELP "Enter field name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND FIELD "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


FUNCTION fSelectionResult RETURNS LOGIC:

   ocNewSelection = ttField.FieldName.
   IF INDEX(icFunctions,"l") > 0 THEN
      ocNewSelection = ocNewSelection + "|" + ttField.FieldLabel.
   IF INDEX(icFunctions,"t") > 0 THEN
      ocNewSelection = ocNewSelection + "|" + ttField.TableName.

END FUNCTION.


llDispTable = (NUM-ENTRIES(icTable) > 1).
IF llDispTable THEN lcFieldData:LABEL IN FRAME sel = "Table".

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

DO liTable = 1 TO NUM-ENTRIES(icTable):

   CREATE BUFFER lhTable FOR TABLE ENTRY(liTable,icTable).
 
   /* get possible field names, _file/_field could also be used, but this 
      way it doesn't matter in which db table is */
   DO liCnt = 1 TO lhTable:NUM-FIELDS:
      lhField = lhTable:BUFFER-FIELD(liCnt).

      IF icAllowed > "" AND
         LOOKUP(lhField:NAME,icAllowed) = 0 THEN NEXT.

      CREATE ttField.
      ASSIGN 
         ttField.TableName  = lhTable:NAME
         ttField.FieldName  = lhField:NAME
         ttField.FieldLabel = lhField:LABEL
         ttField.FieldHelp  = lhField:HELP
         ttField.Selected   = (LOOKUP(ttField.FieldName,icCurrentList) > 0).
      
      IF ttField.Selected THEN ttField.OrigSelection = TRUE.   
   END.
 
   DELETE OBJECT lhField.
   DELETE OBJECT lhTable.
END.

DEF VAR lcExtraField AS CHAR NO-UNDO. 

DO liCnt = 1 TO NUM-ENTRIES(icExtraFields):
   
   lcExtraField = ENTRY(liCnt, icExtraFields).
   CREATE ttField.
   ASSIGN 
      ttField.FieldName  = lcExtraField
      ttField.Selected   = (LOOKUP(lcExtraField,icCurrentList) > 0).
END.

RUN local-find-first.

IF AVAILABLE ttField THEN ASSIGN
   Memory       = recid(ttField)
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
    
   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ttField WHERE recid(ttField) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttField THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttField).
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
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        IF INDEX(icFunctions,"h") > 0 THEN ASSIGN
           ufk[5] = 11.
        ELSE ASSIGN    
           ufk[3] = 1516
           ufk[5] = 1517.

        RUN ufkey.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ttField.FieldName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ttField.FieldName WITH FRAME sel.
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
        FIND ttField WHERE recid(ttField) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttField THEN
              ASSIGN FIRSTrow = i Memory = recid(ttField).
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
           IF NOT AVAILABLE ttField THEN DO:
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
                rtab[1] = recid(ttField)
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
           IF NOT AVAILABLE ttField THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttField).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttField WHERE recid(ttField) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttField THEN DO:
           Memory = recid(ttField).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttField THEN Memory = recid(ttField).
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
           FIND ttField WHERE recid(ttField) = Memory NO-LOCK.
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
       UPDATE lcField WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF lcField > "" THEN DO:
       
          FIND FIRST ttField WHERE 
                     ttField.FieldName >= lcField
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttField THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN 
             order      = 1 
             Memory     = RECID(ttField) 
             must-print = TRUE.
             
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* select / unselect */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

        RUN local-find-this (FALSE).

        IF AVAILABLE ttField THEN DO:

           IF ttField.Selected THEN DO:
              /* is unselection allowed */ 
              IF ttField.OrigSelection AND INDEX(icFunctions,"d") = 0 THEN
              MESSAGE "Removing original items is not allowed"
              VIEW-AS ALERT-BOX INFORMATION.
   
              ELSE ttField.Selected = FALSE.
           END.
           
           ELSE DO:
              /* is selection allowed */
              IF INDEX(icFunctions,"a") = 0 THEN
              MESSAGE "Adding items is not allowed"
              VIEW-AS ALERT-BOX INFORMATION.
   
              ELSE ttField.Selected = TRUE.
           END.
              
           RUN local-disp-row.
        END.
     END. 
     
     /* selection list ok */
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  

        /* used as help */
        IF INDEX(icFunctions,"h") > 0 THEN DO:
           
           RUN local-find-this(FALSE).
           fSelectionResult().
           LEAVE LOOP.
        END.
        
        i = 0.

        /* always start with the current list -> order of items in old
           TMRItemValues stays the same */
        ocNewSelection = icCurrentList.
        
        FOR EACH ttField WHERE ttField.Selected:
        
           IF LOOKUP(ttField.FieldName,ocNewSelection) > 0 THEN NEXT.
            
           ocNewSelection = ocNewSelection + 
                            (IF ocNewSelection > "" THEN "," ELSE "") + 
                            ttField.FieldName.
           i = i + 1.                 
        END.

        IF i > iiMaxSelected THEN DO:
           MESSAGE "You can choose only" iiMaxSelected "item" + 
                   (IF iiMaxSelected > 1 THEN "s" ELSE "")
           VIEW-AS ALERT-BOX ERROR.
           ocNewSelection = "".
        END.

        ELSE LEAVE LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       /* used as help */
       IF INDEX(icFunctions,"h") > 0 THEN DO:
          fSelectionResult().
          LEAVE LOOP.
       END.
  
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(ttField).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttField) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttField) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
        ocNewSelection = icCurrentList.
        LEAVE LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND ttField WHERE recid(ttField) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttField WHERE recid(ttField) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF order = 1 THEN FIND FIRST ttField NO-LOCK NO-ERROR.
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST ttField NO-LOCK NO-ERROR.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT ttField NO-LOCK NO-ERROR.
          
END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV ttField NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       ttField.FieldName
       ttField.FieldLabel
       ttField.Selected
       lcFieldData
       WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   IF llDispTable 
   THEN lcFieldData = ttField.TableName.
   ELSE lcFieldData = ttField.FieldHelp.
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      ttField.FieldName
      ttField.FieldLabel
      ttField.FieldHelp
      WITH FRAME lis.
      
      PAUSE MESSAGE "Press ENTER to continue".

      LEAVE.
   END.
   
END PROCEDURE.

