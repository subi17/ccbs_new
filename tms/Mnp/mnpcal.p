/* ----------------------------------------------------------------------
  MODULE .......: MNPCAL.P
  TASK .........: Manage MNP calendar.
  APPLICATION ..: TMS
  AUTHOR .......: as
  CREATED ......: 06/2008
  CHANGED ......: 
  Version ......: xfera 
  ---------------------------------------------------------------------- */

{commali.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'MNPCal'}
{timestamp.i}
{xmlfunction.i}
{ftaxdata.i}
{timestamp.i}
{tmsconst.i}
{eventval.i}
{mnp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMNPCal AS HANDLE NO-UNDO.
   lhMNPCal = BUFFER MNPCal:HANDLE.
   RUN StarEventInitialize(lhMNPCal).

   DEFINE VARIABLE lhTMSParam AS HANDLE NO-UNDO.
   lhTMSParam = BUFFER TMSParam:HANDLE.
   RUN StarEventInitialize(lhTMSParam).

END.

DEF NEW shared VAR siirto AS CHAR.

DEFINE VARIABLE xrecid       AS RECID                   NO-UNDO  init ?.
DEFINE VARIABLE FIRSTrow     AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE FrmRow       AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE FrmDown      AS INTEGER                 NO-UNDO  init 15.
DEFINE VARIABLE order        AS INTEGER                 NO-UNDO  init 1.
DEFINE VARIABLE maxOrder     AS INTEGER                 NO-UNDO  init 3.
DEFINE VARIABLE ufkey        AS LOGICAL                 NO-UNDO  init TRUE.
DEFINE VARIABLE delrow       AS INTEGER                 NO-UNDO  init 0.
DEFINE VARIABLE pr-order     AS INTEGER                 NO-UNDO.
DEFINE VARIABLE Memory       AS RECID                   NO-UNDO.
DEFINE VARIABLE RowNo        AS INTEGER                 NO-UNDO.
DEFINE VARIABLE must-print   AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE must-add     AS LOGICAL                 NO-UNDO.
DEFINE VARIABLE ac-hdr       AS CHARACTER               NO-UNDO.
DEFINE VARIABLE rtab         AS RECID EXTENT 24         NO-UNDO.
DEFINE VARIABLE i            AS INTEGER                 NO-UNDO.
DEFINE VARIABLE ok           AS LOGICAL format "Yes/No" NO-UNDO.
DEF BUFFER MNPCalBuf FOR MNPCal.
DEFINE VARIABLE lcRegion     AS CHARACTER               NO-UNDO.
DEFINE VARIABLE llAdmin      AS LOGICAL NO-UNDO. 
DEFINE VARIABLE llSyst       AS LOGICAL NO-UNDO. 
DEFINE VARIABLE liPeriodSum AS INTEGER NO-UNDO.  

llAdmin = (getTMSRight("CCSUPER,SYST") EQ "RW").
/* YOT-1438 */
llSyst = (getTMSRight("SYST") EQ "RW").

FORM
    MNPCal.OrderChannel
    MNPCal.Region  
    MNPCal.MessageType FORMAT "x(10)"
    MNPCal.Periods  
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " MNP CALENDAR  "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

FORM
    "Channel ....:" MNPCal.OrderChannel FORMAT "x(16)" 
    HELP "Order Channel (F9)" SKIP
    "Region .....:" MNPCal.Region       FORMAT "x(2)"
    HELP "Region (F9)" 
    lcRegion FORMAT "x(20)" SKIP
    "Message Type:" MNPCal.MessageType  FORMAT "x(8)"
    HELP "Message Type (F9)" SKIP
    "Periods ....:" MNPCal.Periods      FORMAT ">9"
    HELP "Maximum amount of periods before making alarm"
WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    NO-LABELS 
    FRAME lis.

FORM
    "Min. Window:" TMSParam.IntVal    
    HELP "Minimum sum of periods"
WITH  OVERLAY ROW 4 centered
    TITLE "Min. Window" 
    NO-LABELS 
    FRAME fWindow.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.
IF AVAILABLE MNPCal THEN ASSIGN
   Memory       = recid(MNPCal)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   MESSAGE
      "No entries available!" SKIP
      "Do You want to add one?"
   VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO UPDATE ok.
   IF ok THEN must-add = TRUE.
   ELSE RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a MNPCal  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

ADD-ROW:

      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN ufkey.
        
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           
           CREATE MNPCal.

           RUN local-UPDATE-record(TRUE).

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhMNPCal). 

           ASSIGN
              Memory = recid(MNPCal)
              xrecid = Memory.
        
           LEAVE ADD-ROW.
      
        END.
      
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE MNPCal THEN LEAVE LOOP.
      NEXT LOOP.
      
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND MNPCal WHERE recid(MNPCal) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE MNPCal THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(MNPCal).
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
        ufk = 0
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0
        ufk[5]= 5  WHEN llAdmin 
        ufk[6]= 4  WHEN llAdmin 
        ufk[7]= 9020 WHEN llSyst ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
         RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW MNPCal.OrderChannel ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPCal.OrderChannel WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW MNPCal.Region ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPCal.Region WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
        CHOOSE ROW MNPCal.MessageType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) MNPCal.MessageType WITH FRAME sel.
      END.
      
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND MNPCal WHERE recid(MNPCal) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE MNPCal THEN
              ASSIGN FIRSTrow = i Memory = recid(MNPCal).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE MNPCal THEN DO:
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
                rtab[1] = recid(MNPCal)
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
           IF NOT AVAILABLE MNPCal THEN DO:
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
              rtab[FRAME-DOWN] = recid(MNPCal).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND MNPCal WHERE recid(MNPCal) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE MNPCal THEN DO:
           Memory = recid(MNPCal).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE MNPCal THEN Memory = recid(MNPCal).
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
           FIND MNPCal WHERE recid(MNPCal) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
      END. /* NEXT page */
     
     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND llAdmin
      THEN DO:
     
        must-add = TRUE.
        
        NEXT LOOP.
        
     END. /* ADD NEW */
         
     ELSE IF LOOKUP(nap,"6,f6") > 0  AND llAdmin 
      THEN DO TRANS:

        RUN local-find-this(FALSE).
        
        ok = FALSE. 
        
        RUN local-find-next.
        IF AVAIL MNPCal THEN
           memory = RECID(MNPCal).
        ELSE DO:
           RUN local-find-this(FALSE).
           RUN local-find-prev.
           IF AVAILABLE MNPCal THEN ASSIGN
              memory = RECID(MNPCal)
              delrow = delrow - 1.
        END.
        
        RUN local-find-this(TRUE).
        
        COLOR DISPLAY value(ctc)
           MNPCal.OrderChannel   
           MNPCal.Region
           MNPCal.MessageType
           MNPCal.Periods
           WITH FRAME sel.
        
        MESSAGE
         "ARE YOU SURE YOU WANT TO DELETE THIS ROW?" 
        UPDATE ok.
        
        IF MNPCal.Region = "99" AND
           MNPCal.OrderChannel = "" THEN DO:
           MESSAGE "Cannot delete default value!" VIEW-AS ALERT-BOX.
           ok = FALSE.
        END.
        
        COLOR DISPLAY value(ccc)
           MNPCal.OrderChannel
           MNPCal.Region
           MNPCal.MessageType
           MNPCal.Periods
           WITH FRAME sel.
       
        IF OK THEN DO:
       
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhMNPCal).
           DELETE MNPCal.

           RUN local-find-first.

        END.
        ELSE DO:
           
           delrow = 0.
           
           RUN local-find-this(false).
        
           memory = RECID(MNPCal).

        END.

        IF NOT AVAIL MNPCal THEN
           LEAVE LOOP.
        
        must-print = TRUE.
        
        NEXT LOOP.
        
     END. /* DELETE */
            
      ELSE IF LOOKUP(nap,"7,f7") > 0 AND llSyst
       THEN DO:
         
         ehto = 9.
         RUN ufkey.

         FIND FIRST TMSParam WHERE
            TMSParam.Brand = gcBrand AND
            TMSParam.ParamCode = "MNPMinWindow" AND
            TMSParam.ParamGroup = "MNP" NO-LOCK NO-ERROR.
         
         DISP TMSParam.IntVal WITH FRAME fWindow.
         
         UPDATE_LOOP:
         REPEAT WITH FRAME fWindow ON ENDKEY UNDO, LEAVE: 

            PROMPT-FOR TMSParam.IntVal.
            IF INPUT TMSParam.IntVal <= 0 THEN DO:
               MESSAGE ({&MSG_INCORRECT_VALUE}) VIEW-AS ALERT-BOX. 
               DISP TMSParam.IntVal.
               UNDO, NEXT UPDATE_LOOP. 
            END.

            /* IF  User Wanted TO Cancel this Change TRANSACTION */
            IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
            KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
            
            IF LOOKUP(KEYLABEL(LASTKEY),"f1,enter,return") > 0 THEN DO:
            
               FIND CURRENT TMSParam EXCLUSIVE-LOCK.
               
               IF CURRENT-CHANGED TMSParam THEN DO:
                  
                  FIND CURRENT TMSParam NO-LOCK.
                  
                  MESSAGE 
                     ({&MSG_RECORD_CHANGED}) 
                  VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".
                  LEAVE.
               END.
              
               ok = FALSE.
               
               MESSAGE
                  "ARE YOU SURE?" 
               UPDATE ok.

               IF ok THEN DO:
                  IF llDoEvent THEN RUN StarEventSetOldBuffer(lhTMSParam).
                  ASSIGN TMSParam.IntVal.
                  IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhTMSParam).
               END.

               LEAVE.
            END.
         END. 

         HIDE FRAME fWindow NO-PAUSE.
         ufkey = TRUE.
         NEXT LOOP. 
      END. 

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND llAdmin THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:

       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record(FALSE).
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.
       xrecid = recid(MNPCal).
     
       LEAVE.

     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(MNPCal) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(MNPCal) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().

PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND MNPCal WHERE recid(MNPCal) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND MNPCal WHERE recid(MNPCal) = rtab[frame-line(sel)] 
       NO-LOCK.

END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST MNPCal USE-INDEX OrderChannel NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST MNPCal USE-INDEX Region NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN
      FIND FIRST MNPCal USE-INDEX MessageType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST MNPCal USE-INDEX OrderChannel NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST MNPCal USE-INDEX Region NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN
      FIND LAST MNPCal USE-INDEX MessageType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT MNPCal USE-INDEX OrderChannel NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT MNPCal USE-INDEX Region NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN
      FIND NEXT MNPCal USE-INDEX MessageType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN
      FIND PREV MNPCal USE-INDEX OrderChannel NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
      FIND PREV MNPCal USE-INDEX Region NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN
      FIND PREV MNPCal USE-INDEX MessageType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

   RUN local-find-others.

   CLEAR FRAME sel NO-PAUSE.
   DISPLAY 
      MNPCal.OrderChannel
      MNPCal.Region
      MNPCal.MessageType
      MNPCal.Periods
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   
   DEFINE INPUT PARAMETER llNew AS LOGICAL NO-UNDO.
   DEFINE VARIABLE llDefault AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE liMinWindow AS INTEGER NO-UNDO. 

   liMinWindow = fGetMinMNPWindow().

   IF MNPCal.Region = "99" AND MNPCal.OrderChannel = "" 
      THEN llDefault = TRUE. ELSE llDefault = FALSE.
   
   lcRegion = "".
   FIND FIRST Region NO-LOCK WHERE
      Region.Region = MNPCal.Region NO-ERROR.
   IF AVAIL Region THEN lcRegion = Region.RgName.
   IF MNPCal.Region EQ "99" THEN lcRegion = "DEFAULT".
   
   DISP
      MNPCal.Region lcRegion
      MNPCal.OrderChannel
      MNPCal.MessageType 
      MNPCal.Periods WITH FRAME lis.

   MAIN:
   REPEAT ON ENDKEY UNDO, LEAVE:
   
   PROMPT
      MNPCal.OrderChannel WHEN NOT llDefault 
      MNPCal.Region WHEN NOT llDefault
      MNPCal.MessageType 
      MNPCal.Periods
   WITH FRAME lis EDITING:

      READKEY.
      
      nap = keylabel(lastkey).
      /* IF  User Wanted TO Cancel this Change TRANSACTION */
      IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
      KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
      
      IF KEYLABEL(lastkey) = "F2" THEN NEXT.
            
      if nap = "F9" THEN DO:
         CASE FRAME-FIELD:
            
            WHEN "OrderChannel" THEN DO:
               RUN h-tmscodes
                  ("MNPCal","OrderChannel","MNP", OUTPUT siirto).
               IF siirto ne "" THEN DO:
                  MNPCal.OrderChannel = siirto.
               END.
               disp MNPCal.OrderChannel WITH FRAME lis.
               ehto = 9.
               RUN ufkey.
               NEXT.
            END.
            
            WHEN "MessageType" THEN DO:
            RUN h-tmscodes
                  ("MNPCal","MessageType","MNP", OUTPUT siirto).
               IF siirto ne "" THEN DO:
                  MNPCal.MessageType = siirto.
               END.
               disp MNPCal.MessageType WITH FRAME lis.
               ehto = 9.
               RUN ufkey.
               NEXT.
            END.
         END.
      END.

      IF LOOKUP(nap,poisnap) > 0 THEN DO:
         
         IF FRAME-FIELD = "OrderChannel" THEN DO:
            FIND FIRST TMSCodes WHERE
               TMSCodes.TableName = "MNPCal"   AND
               TMSCodes.FieldName = "OrderChannel" AND
               TMSCodes.CodeGroup = "MNP"   AND
               TMSCodes.CodeValue = INPUT MNPCal.OrderChannel
            NO-LOCK NO-ERROR.
            IF NOT AVAIL TMSCodes AND INPUT MNPCal.OrderChannel NE ""  THEN DO:
               MESSAGE "Unknown Channel" INPUT MNPCal.OrderChannel
                  VIEW-AS ALERT-BOX.
               NEXT.
            END.
            IF INPUT MNPCal.OrderChannel NE "" THEN
            DISPLAY TMSCodes.CodeValue @ MNPCal.OrderChannel WITH FRAME lis.
         END.
         
         IF FRAME-FIELD = "Region" THEN DO:
            FIND FIRST Region NO-LOCK WHERE
               Region.Region = INPUT MNPCal.Region NO-ERROR.
            IF NOT AVAIL Region AND INPUT MNPCal.Region NE "99" THEN DO:
               MESSAGE "Unknown Region" INPUT MNPCal.Region VIEW-AS ALERT-BOX.
               lcRegion = "".
               DISPLAY lcRegion WITH FRAME lis.
               NEXT.
            END.
            
            IF INPUT MNPCal.Region = "99" THEN lcRegion = "DEFAULT".
            ELSE lcRegion = Region.RgName.
            DISPLAY lcRegion WITH FRAME lis.
         END.
         
         ELSE IF FRAME-FIELD = "MessageType" THEN DO:
            FIND FIRST TMSCodes WHERE
               TMSCodes.TableName = "MNPCal"   AND
               TMSCodes.FieldName = "MessageType" AND
               TMSCodes.CodeGroup = "MNP"   AND
               TMSCodes.CodeValue = INPUT MNPCal.MessageType
            NO-LOCK NO-ERROR.
            IF NOT AVAIL TMSCodes THEN DO:
               MESSAGE "Unknown MessageType" INPUT MNPCal.MessageType
                  VIEW-AS ALERT-BOX.
               NEXT.
            END.
            DISPLAY TMSCodes.CodeValue @ MNPCal.MessageType WITH FRAME lis.
         END.
         
      END.
      
      APPLY LASTKEY.

   END.
                    
   liPeriodSum = fMNPTotalPeriods(
      INPUT INPUT MNPCal.MessageType,
      INPUT INPUT MNPCal.Periods,
      INPUT INPUT MNPCal.OrderChannel,
      INPUT INPUT MNPCal.Region).
   
   IF liPeriodSum < liMinWindow THEN DO:
      MESSAGE "Total period sum cannot be less than minimum"
      VIEW-AS ALERT-BOX.
      NEXT MAIN.
   END.
   
   IF INPUT MNPCal.Region EQ "" OR INPUT MNPCal.MessageType = "" THEN DO:
      MESSAGE "Missing data!".
      NEXT MAIN.
   END.

   IF INPUT MNPCal.OrderChannel = "" AND
      INPUT MNPCal.Region NE "99" THEN DO:
      MESSAGE "Channel is missing a value" VIEW-AS ALERT-BOX. 
      NEXT MAIN.
   END.
  
   
   FIND FIRST MNPCalBuf WHERE
      MNPCalBuf.OrderChannel = INPUT MNPCal.OrderChannel AND
      MNPCalBuf.Region = INPUT MNPCal.Region AND
      MNPCalBuf.MessageType = INPUT MNPCal.MessageType AND
      ROWID(MNPCalBuf) NE ROWID(MNPCal) NO-LOCK NO-ERROR. 
   IF AVAIL MNPCalBuf THEN DO: 
      MESSAGE "Definition already exists!" VIEW-AS ALERT-BOX.        
      UNDO.
   END.

   FIND CURRENT MNPCal EXCLUSIVE-LOCK.
   
   IF CURRENT-CHANGED MNPCal THEN DO:
      
      FIND CURRENT MNPCal NO-LOCK.
      
      MESSAGE ({&MSG_RECORD_CHANGED})
      VIEW-AS ALERT-BOX TITLE "UPDATE CANCELLED".

   END. 
   ELSE DO: 
      
      IF NOT llNew AND llDoEvent THEN RUN StarEventSetOldBuffer ( lhMNPCal ).
      
      ASSIGN
         MNPCal.OrderChannel WHEN NOT llDefault 
         MNPCal.Region  WHEN NOT llDefault
         MNPCal.MessageType 
         MNPCal.Periods.
      
      IF NOT llNew AND llDoEvent THEN RUN StarEventMakeModifyEvent ( lhMNPCal ).
  
  END.
  
  FIND CURRENT MNPCal NO-LOCK.

  LEAVE. 
   
  END.  
END PROCEDURE.
