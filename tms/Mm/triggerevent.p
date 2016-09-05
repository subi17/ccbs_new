/* ----------------------------------------------------------------------
  MODULE .......: TriggerEvent
  TASK .........: UPDATEs table TriggerEvent
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

DEF INPUT PARAMETER    icTriggerConfID AS CHAR NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhTriggerEvent AS HANDLE NO-UNDO.
   lhTriggerEvent = BUFFER TriggerEvent:HANDLE.
   RUN StarEventInitialize(lhTriggerEvent).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhTriggerEvent).
   END.

END.

DEF VAR lcTriggerEvent       AS CHAR                   NO-UNDO.
DEF VAR icLeagueCode AS CHAR                   NO-UNDO.
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
DEF VAR lcTriggerEventType AS CHAR FORMAT "X(40)"      NO-UNDO.
DEF VAR lcTriggerEventRule AS CHAR FORMAT "X(40)"      NO-UNDO.
DEF VAR lcLeagueName AS CHAR                   NO-UNDO.
DEF VAR lcCustNum    AS CHAr                   NO-UNDO.
DEF VAR lcCLI        AS CHAR                   NO-UNDO.
DEF VAR lcDuration   AS CHAR  FORMAT "X(25)"   NO-UNDO.
DEF VAR lcTriggerStatus AS CHAR FORMAT "X(35)" NO-UNDO.
DEF VAR lcTriggerItemsFile AS CHAR             NO-UNDO.
DEF VAR lcError            AS CHAR             NO-UNDO.

DEF BUFFER bTriggerEvent FOR TriggerEvent.

run local-find-others.
                                
form
    TriggerEvent.TriggerEventID      COLUMN-LABEL "ID"
    TriggerEvent.TriggerConfID
    TriggerEvent.EventSource    FORMAT "X(10)" COLUMN-LABEL "Source"
    TriggerEvent.StatusCode     FORMAT ">9"    COLUMN-LABEL "ST"
    TriggerEvent.Created        FORMAT "99-99-9999"
    TriggerEvent.Activated      FORMAT "99-99-9999"
    TriggerEvent.Handled        FORMAT "99-99-9999" 

WITH ROW FrmRow width 76 CENTERED  OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       "  TriggerEvents for  " + icTriggerConfID  + " " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    TriggerEvent.TriggerEventID    COLON 22
    TriggerEvent.TriggerConfID     COLON 22
    TriggerEvent.EventSource       COLON 22 LABEL "Source" FORMAT "X(24)"
    TriggerEvent.Created           COLON 22
    TriggerEvent.Activated         COLON 22 
    TriggerEvent.Handled           COLON 22
    TriggerEvent.StatusCode        COLON 22 lcTriggerStatus NO-LABEL
    TriggerEvent.TableName         COLON 22
    TriggerEvent.TableID           COLON 22
    TriggerEvent.Qty               COLON 22
    lcDuration                     COLON 22 LABEL "Duration"
    TRIGGEREvent.ChangedFields     COLON 22
    TriggerEvent.ChangedValues     COLON 22
    TriggerEvent.KeyValue          COLON 22
    TriggerEvent.Reason            COLON 22 FORMAT "X(40)"
                    

WITH  OVERLAY ROW 3 CENTERED
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  TriggerEvent */
    "TriggerEvent:" lcTriggerEvent FORMAT "X(16)"
    HELP "Enter TriggerEvent"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND TriggerEvent "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  TriggerEvent */
    "Name:" lcRgName FORMAT "x(30)"
    HELP "Enter name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FUNCTION fIsValidInputFile RETURN CHAR
   (INPUT icFileName AS CHAR ):


   IF SEARCH(icfilename) = ? THEN RETURN "Unknown Filename " + icFilename.

   RETURN "".
ENd.

DEF TEMP-TABLE ttList 
FIELD InvCust AS INT
FIELD CLI     AS CHAR.

FUNCTION fGenerateSpecialTrigger RETURN INT
   (INPUT icFileName AS CHAR ):

   DEF VAR lcline AS CHAR NO-UNDO.
   DEF VAR lii    AS INT  NO-UNDO.
   
   FIND FIRST TriggerConf WHERE
              TriggerConf.TriggerConfID = "SPECIAL_FILE"        AND
              TriggerConf.EventRule     > 0               AND
              TriggerConf.ValidTo       >= Today          AND
              TriggerConf.ValidFrom     <= Today NO-LOCK NO-ERROR.

   IF AVAIL TriggerConf THEN DO:
      
      CREATE TriggerEvent.
      ASSIGN
      TriggerEvent.TriggerEventID = NEXT-VALUE(TriggerEvent)
      TriggerEvent.TriggerConfID  = "SPECIAL_FILE"
      TriggerEvent.EventSource    = "CREATE"
      TriggerEvent.Created        = DateTime(Today,mtime)
      TriggerEvent.TableName      = "SPECIAL_FILE"
      TriggerEvent.Keyvalue       = STRING(icFileName) 
      TriggerEvent.ChangedValues  = icFileName.

      lii = 1.
      RELEASE TriggerEvent.

   END.

   RETURN lii.

END FUNCTION.



FUNCTION fZoneName RETURNS LOGIC
   (icTaxZone AS CHAR):
   
   /*ZoneName = "".
   
   FIND TaxZone WHERE TaxZone.TaxZone = icTaxZone NO-LOCK NO-ERROR.
   IF AVAILABLE TaxZone THEN lcZoneName = TaxZone.TZName. 
     */
END FUNCTION.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE TriggerEvent THEN ASSIGN
   Memory       = recid(TriggerEvent)
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
    
   IF must-add THEN DO:  /* Add a TriggerEvent  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           CREATE TriggerEvent.
           
           ASSIGN
           TriggerEvent.TriggerConfID   = icTriggerConfID 
           TriggerEvent.Created         = DateTime(Today,mtime)
           TriggerEvent.EventSource     = "TE-UI"
           TriggerEvent.TableName       = icTriggerConfID
           TriggerEvent.TriggerEventID  = NEXT-VALUE(TriggerEvent).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhTriggerEvent).

           ASSIGN
           Memory = recid(TriggerEvent)
           xrecid = Memory
           must-add = false.  
           LEAVE Add-row.
        END.

      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE TriggerEvent THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND TriggerEvent WHERE recid(TriggerEvent) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE TriggerEvent THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(TriggerEvent).
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
        ufk[4] = 4002 
        ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
        UFK[7] = 0
        ufk[8] = 8 
        ehto   = 3 
        ufkey  = FALSE.

        RUN Syst/ufkey.p.
        
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW TriggerEvent.TriggerConfID {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TriggerEvent.TriggerConfID WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW TriggerEvent.EventSource {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) TriggerEvent.EventSource WITH FRAME sel.
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
        FIND TriggerEvent WHERE recid(TriggerEvent) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE TriggerEvent THEN
              ASSIGN FIRSTrow = i Memory = recid(TriggerEvent).
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
           IF NOT AVAILABLE TriggerEvent THEN DO:
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
                rtab[1] = recid(TriggerEvent)
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
           IF NOT AVAILABLE TriggerEvent THEN DO:
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
              rtab[FRAME-DOWN] = recid(TriggerEvent).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND TriggerEvent WHERE recid(TriggerEvent) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE TriggerEvent THEN DO:
           Memory = recid(TriggerEvent).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE TriggerEvent THEN Memory = recid(TriggerEvent).
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
           FIND TriggerEvent WHERE recid(TriggerEvent) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
/******
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       UPDATE lcTriggerEvent WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
                         
       IF lcTriggerEvent > "" THEN DO:
       
          FIND FIRST TriggerEvent WHERE 
                     TriggerEvent.LEagueCode = icLeagueCode AND 
                     TriggerEvent.TriggerConfID >= lcTriggerEvent
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE TriggerEvent THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(TriggerEvent) 
                 must-print = TRUE
                 order      = 1.
          NEXT LOOP.

       END.
*************/
     END. /* Search-1 */

     /* Search BY column 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f2.
       UPDATE lcRgName WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
/*******                         
       IF lcRgName > "" THEN DO:
       
          FIND FIRST TriggerEvent WHERE 
                     TriggerEvent.TriggerConfID  = iiTriggerConfID AND 
                     TriggerEvent.EventSource >= lcRgName
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE TriggerEvent THEN DO:
             MESSAGE "Not found"
             VIEW-AS ALERT-BOX.
             NEXT BROWSE.
          END.

          ASSIGN Memory     = RECID(TriggerEvent) 
                 must-print = TRUE
                 order      = 2.
          NEXT LOOP.
       END.
**************/
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
    
        RUN local-find-this (FALSE).
            
        RUN Mm/triggeritem.p(TriggerEvent.TriggerConfID,
                          TriggerEvent.TriggerEventID).
                    
        UFKEY = TRUE.
        RUN Syst/ufkey.p.
        NEXT LOOP.
     
     END.

     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
    
     END.
 

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0  
     THEN DO:  /* add */
        {Syst/uright2.i}
     
        IF icTriggerConfID = "SPECIAL_FILE" THEN DO:
        
           UPDATE lcTriggerItemsFile FORMAT "X(50)" LABEL "INPUT FILENAME" WITH OVERLAY CENTERED FRAME File.

           IF INPUT lcTriggerItemsFile = "" THEN DO: hide frame file no-pause. NEXT LOOP. END.

           MESSAGE "Validating input file...".
           
           lcError = fIsValidInputFile(lcTriggerItemsFile).
           
           IF lcError NE "" THEN DO:
              MESSAGE lcError VIEW-AS ALERT-BOX ERROR.
              hide frame file no-pause.
              NEXT LOOP.
           END. 

           IF fGenerateSpecialTrigger(lcTriggerItemsFile) > 0 THEN 
              MESSAGE "TRIGGER EVENT CREATED SUCCESFULLY" VIEW-AS ALERT-BOX.
           ELSE MESSAGE "TRIGGER EVENT CREATION FAILED" VIEW-AS ALERT-BOX.  
        
           HIDE FRAME File NO-PAUSE .
           NEXT LOOP.
        END.
        ELSE 
        must-add = TRUE.
        NEXT LOOP.
     END.
     
     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF TriggerEvent.StatusCode ne 0 THEN DO:
          MESSAGE
          "Delete not allowed " VIEW-AS ALERT-BOX.
          NEXT.
       END.

       COLOR DISPLAY VALUE(ctc)
       TriggerEvent.TriggerEventID     
       TriggerEvent.TriggerConfID
       TriggerEvent.EventSource   
       TriggerEvent.Created
       TriggerEvent.Activated
       TriggerEvent.Handled
       TriggerEvent.StatusCode.

       RUN local-find-NEXT.
       IF AVAILABLE TriggerEvent THEN Memory = recid(TriggerEvent).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE TriggerEvent THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(TriggerEvent).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       TriggerEvent.TriggerEventID TriggerEvent.EventSource TriggerEvent.Created TriggerEvent.Handled 
       TriggerEvent.StatusCode  TriggerEvent.Activated TriggerEvent.TriggerConfID.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTriggerEvent).

           DELETE TriggerEvent.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE TriggerEvent THEN DO:
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

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTriggerEvent).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTriggerEvent).

       RUN local-disp-row.
       xrecid = recid(TriggerEvent).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(TriggerEvent) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(TriggerEvent) must-print = TRUE.
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
      FIND TriggerEvent WHERE recid(TriggerEvent) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND TriggerEvent WHERE recid(TriggerEvent) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

       IF   order = 1 THEN FIND FIRST TriggerEvent WHERE 
          TriggerEvent.TriggerConfID = icTriggerConfID 
       NO-LOCK NO-ERROR.
      /* ELSE IF order = 2 THEN FIND FIRST TriggerEvent USE-INDEX TriggerEventName  
         WHERE TriggerEvent.TriggerConfID = iiTriggerConfID
          NO-LOCK NO-ERROR. */
         
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 THEN FIND LAST TriggerEvent WHERE TriggerEvent.TriggerConfID = icTriggerConfID NO-LOCK NO-ERROR.
  
END PROCEDURE.

PROCEDURE local-find-NEXT:

       IF order = 1 THEN FIND NEXT TriggerEvent WHERE TriggerEvent.TriggerConfID = icTriggerConfID NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
 
       IF order = 1 THEN FIND PREV TriggerEvent WHERE TriggerEvent.TriggerConfID = icTriggerConfID NO-LOCK NO-ERROR.
   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       TriggerEvent.TriggerEventID
       TriggerEvent.TriggerConfID
       TriggerEvent.EventSource
       TriggerEvent.Created
       TriggerEvent.Activated
       TriggerEvent.Handled
       TriggerEvent.StatusCode
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   FIND FIRST TriggerConf WHERE 
              TriggerConf.TriggerConfID = icTriggerConfID  NO-LOCK NO-ERROR.
   
   IF TriggerEvent.Duration > 0 
   THEN lcDuration = STRING(TriggerEvent.Duration,"hh:mm:ss").
   ELSE lcDuration = "".

   FIND FIRST TMSCOdes WHERE
              TMSCodes.Tablename = "TriggerEvent"        AND
              TMSCodes.FieldName = "StatusCode"    AND
              TMSCodes.CodeGroup = "StatusCode"    AND
              TMSCodes.CodeValue = STRING(TriggerEvent.StatusCode) NO-LOCK NO-ERROR.

   IF AVAIL TMSCodes THEN lcTriggerStatus = TMSCodes.CodeName.
   ELSE                   lcTriggerStatus = "".
      

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.

      CLEAR FRAME lis NO-PAUSE.
      
      DISP 
      TriggerEvent.TriggerEventID
      TriggerEvent.TriggerConfID
      TriggerEvent.EventSource
      TriggerEvent.StatusCode      
      lcTriggerStatus
      TriggerEvent.Activated
      TriggerEvent.Created  
      TriggerEvent.Handled    
      TriggerEvent.TableName
      TriggerEvent.TableID
      TriggerEvent.ChangedFields
      TriggerEvent.ChangedValues     
      TriggerEvent.KeyValue          
      TriggerEvent.REason
      TriggerEvent.Qty
      lcDuration
      WITH FRAME lis.

      MESSAGE "PRESS ENTER TO CONTINUE" . PAUSE NO-MESSAGE.

      IF lcRight = "RW" THEN REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
  /********
         ehto = 9. RUN Syst/ufkey.p.
      
         UPDATE
         TriggerEvent.EventSource 
          
         WITH FRAME lis EDITING:
            
            READKEY.
 
            IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:
             
               PAUSE 0.
                
            END.
                             
            APPLY LASTKEY.
       
         END. /* EDITING */
*/
         LEAVE. 
      END.
      
      ELSE DO:
         ehto = 5.
         RUN Syst/ufkey.p.
         PAUSE MESSAGE "Press ENTER to continue".
      END. 
      LEAVE.
   END.
   
END PROCEDURE.

