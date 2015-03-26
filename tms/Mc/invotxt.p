/* ----------------------------------------------------------------------
  MODULE .......: INVOTXT.P
  TASK .........: UPDATE ALL invoice texts 
  APPLICATION ..:   
  AUTHOR .......: JR
  CREATED ......: 25-09-02
  CHANGED ......: 05.03.03 tk  tokens
                  18.03.03/aam EPLForm,
                               new report type 2,
                               Attachments
                  07.04.03/jp  new report type order confirmation             
                  23.05.03/aam view send log (itsendlo)
                  28.05.03/aam run ufkey after f9,
                               title instead of language in browser
                  10.06.03/aam InfoType
                  30.07.03/jp  printcont&orderconf case
                  05.09.03/aam brand,
                               invotxt2.p merged to this
                  22.10.03/aam RemLevel                               
                  11.02.04/aam print (prininfo.p),
                               gcHelpParam
                  22.03.04/aam LetterClass, AddrTarget             
                  07.05.04/aam MainTitle
                  27.12.05/aam memo
                  02.01.06/aam wider format for text,
                               warning for exceeding 160 chrs on sms-texts
                  27.01.06/aam disp maintitle in browser if it exists
                  22.06.06/aam CStateList instead of RemLevel
  Version ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable InvText

{commali.i}
{invotxt.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'invtext'}
{tmsconst.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhInvText AS HANDLE NO-UNDO.
   lhInvText = BUFFER InvText:HANDLE.
   RUN StarEventInitialize(lhInvText).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhInvText).
   END.

END.

DEFINE INPUT PARAMETER  icTarget    LIKE InvText.Target   NO-UNDO.
DEFINE INPUT PARAMETER  icKeyValue  LIKE InvText.KeyValue NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR FromDate     LIKE InvText.FromDate     NO-UNDO.
DEF VAR lctarget     AS CHARACTER              NO-UNDO.
DEF VAR expl         LIKE t-target.expl        NO-UNDO FORMAT "x(25)".
DEF VAR targetname   AS CHARACTER              NO-UNDO.
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
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR tpos         AS C FORMAT "x(20)"       NO-UNDO.
DEF VAR lcReport     AS CHAR                   NO-UNDO.
DEF VAR lcLanguage   AS CHAR                   NO-UNDO.
DEF VAR lcprogram    AS cHAR                   NO-UNDO.
DEF VAR liMax        AS INT                    NO-UNDO.

DEF VAR lcType       AS CHAR  NO-UNDO.
DEF VAR lcAddress    AS CHAR  NO-UNDO. 
DEF VAR lcCode       AS CHAR  NO-UNDO. 
DEF VAR llMemo       AS LOG   NO-UNDO.
DEF VAR liLength     AS INT   NO-UNDO. 
DEF VAR lcTitle      AS CHAR  NO-UNDO.
DEF VAR lckeyvalue   AS CHAR  NO-UNDO.

DEF VAR llMore       AS LOGICAL                NO-UNDO.
DEF VAR llShowHistory AS LOGICAL               NO-UNDO init FALSE.


form
    InvText.FromDate   
    InvText.ToDate
    InvText.Target   format "x(9)"     
    InvText.KeyValue format "x(17)"
    InvText.Language FORMAT "9"  COLUMN-LABEL "L"
    lcReport    COLUMN-LABEL "Printed" format "x(7)"
    llMemo      COLUMN-LABEL "M"       FORMAT "M/"
    lcTitle     COLUMN-LABEL "Title"   FORMAT "X(20)"

WITH CENTERED  ROW FrmRow FrmDown DOWN WIDTH 80
    COLOR VALUE(cfc) OVERLAY
    TITLE COLOR VALUE(ctc) " " + ynimi + " Information Texts " 
        + string(pvm,"99-99-99") + " "
        FRAME sel.

form
    InvText.Target                              COLON 12       
       expl             NO-LABEL     
       InvText.ITNum  LABEL "ID" FORMAT ">>>>>>>>>" TO 78 SKIP
    
    InvText.KeyValue                  Format "x(25)" COLON 12 
       targetname       NO-LABEL      Format "x(35)"             SKIP
    
    InvText.FromDate LABEL "Valid"              COLON 12 "-"
       InvText.ToDate   NO-LABEL                                 
    InvText.Language   COLON 57 lcLanguage NO-LABEL              SKIP
 
    InvText.Position                            COLON 12
       tpos             NO-LABEL                                 
    InvText.InfoType   COLON 57 LABEL "Type" FORMAT "X(3)"
       lcType NO-LABEL FORMAT "X(14)" SKIP
    
    InvText.Report     COLON 12 lcReport NO-LABEL                
    InvText.SendRule COLON 57 LABEL "SendRule" SKIP

    InvText.Attachment COLON 12
    InvText.Category   COLON 57 LABEL "Process" SKIP
    
    InvText.AddrTarget COLON 12 LABEL "Address" FORMAT ">"
       lcAddress NO-LABEL FORMAT "X(26)"
    InvText.LetterClass COLON 57 SKIP
    
    InvText.MainTitle                           COLON 12 
       "L:" AT 71    
          liLength AT 73 NO-LABEL FORMAT ">>>>>9"     SKIP
    InvText.TxtTitle                            COLON 12     
       FORMAT "X(256)" VIEW-AS FILL-IN SIZE 60 BY 1
       HELP "Title, used in eMail texts and letters" SKIP
    
    InvText.InvText COLON 1
        NO-LABEL 
        VIEW-AS EDITOR SIZE 75 BY 8 

WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) 
       ac-hdr + "  Text ID: " + STRING(InvText.ITNum) + " "
    SIDE-LABELS
    FRAME lis.

{brand.i}

form /* seek InvText BY  Date */
    "Brand:" lcBrand skip
    "Date :" FromDate
    HELP "Enter FromDate"      
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND FromDate "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek InvText BY   */
    "Brand   :" lcBrand
    SKIP
    "Target  :" lctarget FORMAT "X(15)"
      HELP "Enter Target Code"
    SKIP
    "KeyValue:" lckeyvalue FORMAT "X(25)"
      HELP "Enter Key Value" 
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Target"
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fTypeName RETURNS CHARACTER
   (icType AS CHAR).

   IF icType = "" THEN RETURN "".

   ELSE RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "InvText","InfoType",icType).

END FUNCTION.

FUNCTION fAddrTarget RETURNS CHARACTER
   (iiAddress AS INT).

   IF iiAddress = 0 THEN RETURN "".

   ELSE RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "InvText","AddrTarget",STRING(iiAddress)).

END FUNCTION.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By FromDate, By Target ,By 3, By 4".

IF icTarget > "" THEN MaxOrder = 1.

RUN local-find-first.
IF AVAILABLE InvText THEN ASSIGN
   memory       = recid(InvText)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No information texts available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a InvText  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.

        PAUSE 0 NO-MESSAGE.

        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           ehto = 9. RUN ufkey.

           ASSIGN InvText.InvText:SCREEN-VALUE = "". 

           IF icTarget > "" 
           THEN DISPLAY icTarget @ InvText.Target
                        icKeyValue @ InvText.KeyValue.

           ELSE                         
           PROMPT-FOR
              InvText.Target
              InvText.KeyValue
              WITH FRAME lis EDITING:
              READKEY.
              IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "keyvalue" THEN
              DO:
                 IF INPUT InvText.Target NE "" THEN
                 lcProgram = fGetprogram(INPUT INPUT InvText.Target).
                 IF lcProgram NE "" THEN 
                 DO:
                    RUN VALUE(lcProgram).
                    IF siirto NE ? THEN DISP siirto @ InvText.KeyValue.        

                    ehto = 9.
                    RUN ufkey.

                 END.   
              END.

              IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 IF frame-field = "keyvalue" THEN 
                 DO:
                    IF INPUT InvText.KeyValue = "" AND 
                       INPUT InvText.Target NE "general"
                    THEN DO:
                       NEXT-PROMPT InvText.Target.
                       NEXT.
                    END.

                    /* VALIDATE ... */
                    targetname = fValKeyvalue(INPUT INPUT InvText.Target,
                                              INPUT INPUT InvText.KeyValue,
                                              FALSE /* non-silent */).
                    IF targetname = ? THEN NEXT.
                    ELSE DISP targetname.                
                 END.

                 if frame-field = "target" THEN 
                 DO:
                    if input InvText.Target = "" THEN LEAVE add-row.
                    FIND t-target where t-target.target = INPUT InvText.Target
                    no-lock no-error.
                    IF NOT AVAIL t-target THEN 
                    DO:
                       bell. message "Unknown Target !".
                       NEXT.
                    END.
                    DISP t-target.expl @ expl. 
                 END.  
              END.
              ELSE IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0
                 THEN UNDO add-row, LEAVE add-row.

              APPLY LASTKEY.    
           END.      

           CREATE InvText.
           ASSIGN
           InvText.Brand    = lcBrand
           InvText.ITNum    = NEXT-VALUE(it-seq)
           InvText.Target   = INPUT InvText.Target
           InvText.KeyValue = INPUT InvText.KeyValue.

           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhInvText).

           ASSIGN
           memory = recid(InvText)
           xrecid = memory.
           LEAVE add-row.
        END.
      END.  /* ADD-ROW */

      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE
             ufkey = TRUE. 

      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND InvText WHERE recid(InvText) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE InvText THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(InvText).
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

        IF NOT llMore THEN
        ASSIGN
        ufk[1]= 28  ufk[2]= (IF icTarget = "" THEN 183 ELSE 0)
        ufk[3]= (IF llShowHistory THEN 38 ELSE 37) 
        ufk[4]= 814 ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 555 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        
        ELSE ASSIGN
        ufk[1]= 927 ufk[2]= 938 ufk[3]= 1796 ufk[4]= 0
        ufk[5]= 0   ufk[6]= 0   ufk[7]= 0 ufk[8]= 8 
        ufk[9]= 1 ehto = 3 ufkey = FALSE.
        

        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
        
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW InvText.FromDate {uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvText.FromDate WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW InvText.Target ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) InvText.Target WITH FRAME sel.
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
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND InvText WHERE recid(InvText) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE InvText THEN
              ASSIGN FIRSTrow = i memory = recid(InvText).
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
           IF NOT AVAILABLE InvText THEN DO:
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
                rtab[1] = recid(InvText)
                memory  = rtab[1].
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
           IF NOT AVAILABLE InvText THEN DO:
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
              rtab[FRAME-DOWN] = recid(InvText).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND InvText WHERE recid(InvText) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE InvText THEN DO:
           memory = recid(InvText).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE InvText THEN memory = recid(InvText).
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
           memory = rtab[FRAME-DOWN].
           FIND InvText WHERE recid(InvText) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 AND NOT llMore AND ufk[1] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME f1.
       SET lcBrand WHEN gcAllBrand AND icTarget = ""
           FromDate WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF FromDate ENTERED THEN DO:
          IF icTarget = "" THEN 
          FIND FIRST InvText USE-INDEX FromDate
          WHERE InvText.Brand = lcBrand AND
                InvText.FromDate <= FromDate
          NO-LOCK NO-ERROR.

          ELSE
          FIND FIRST InvText USE-INDEX FromDate
          WHERE InvText.Brand    = lcBrand    AND
                InvText.Target   = icTarget   AND
                InvText.KeyValue = icKeyValue AND
                InvText.FromDate <= FromDate
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 AND NOT llMore AND ufk[2] > 0
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN ufcolor.

       ASSIGN
         lcTarget   = "SMS"
         lcKeyValue = "".

       ehto = 9. RUN ufkey. ufkey = TRUE.
       DISPLAY lcBrand WITH FRAME f2.
       UPDATE lcBrand WHEN gcAllBrand AND icTarget = ""
           lcTarget validate(lctarget > "", "Enter Target!")  
           lckeyvalue WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.

       FIND FIRST InvText WHERE
          InvText.Brand     = lcBrand AND
          InvText.Target    = lcTarget AND
          InvText.KeyValue >= lcKeyValue AND
          InvText.FromDate <= TODAY AND
          InvText.ToDate   >= TODAY
       No-LOCK NO-ERROR.

       IF NOT fRecFound(2) THEN NEXT BROWSE.
       NEXT LOOP.
                        
     END. /* Search-2 */

     /* view send log */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
       FIND InvText WHERE recid(InvText) = rtab[FRAME-LINE] NO-LOCK.
         IF InvText.Target NE "SMS" THEN
            MESSAGE "Function not supported for" InvText.Target VIEW-AS ALERT-BOX.
         ELSE
            RUN invlang(32,InvText.ITNum).

       ufkey = TRUE.
       NEXT LOOP.

     END.

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND NOT llMore AND ufk[3] > 0 THEN DO:
        llShowHistory = NOT llShowHistory.
        CLEAR FRAME sel no-pause.
        RUN local-find-FIRST.
        ASSIGN
            memory = recid(InvText)
            ufkey = true
            must-print = TRUE.
        NEXT LOOP.
        
     END.
    
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" AND ufk[5] > 0
     THEN DO:  /* add */
     
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" AND ufk[6] > 0
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       InvText.FromDate InvText.ToDate
       InvText.Target. 

       RUN local-find-NEXT.
       IF AVAILABLE InvText THEN memory = recid(InvText).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE InvText THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(InvText).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       InvText.FromDate InvText.ToDate
       InvText.Target. 
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhInvText).

           DELETE InvText.

           /* was LAST record DELETEd ? */

           IF NOT CAN-FIND(FIRST InvText USE-INDEX FromDate WHERE
                                 InvText.Brand    = lcBrand  AND
                                 InvText.Target   = Target   AND
                                 InvText.KeyValue = KeyValue)  
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     /* print */
     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0
     THEN DO:  
       llMore = TRUE.
       ufkey = TRUE.
       NEXT LOOP.

     END. 

     /********************/
     /* 2nd page options */
     /********************/

     /* MEMO in F1 */

     ELSE IF LOOKUP(nap,"1,f1") > 0 AND llMore AND ufk[1] > 0 THEN DO:

        /*RUN local-find-this (FALSE).*/
 
        RUN memo(INPUT 0,
                 INPUT "InvText",
                 INPUT STRING(InvText.ITNum),
                 INPUT "Information Text").

        RUN local-disp-row.
        
        ASSIGN ufkey = TRUE
               ehto  = 9.
        NEXT LOOP.
     END.

     /* PRINT LETTER in F2 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 AND llMore AND ufk[2] > 0 THEN DO:
         RUN local-find-this (TRUE).
         RUN prininfo(InvText.ITNum,
                    0,
                    "").
         ufkey = TRUE.
         NEXT LOOP.
     END.

     /* VIEW SEND LOG in F3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 AND llMore AND ufk[3] > 0 THEN DO:  
       RUN local-find-this (FALSE).
       RUN itsendlo(IF InvText.Target = "Customer" 
                    THEN INTEGER(InvText.KeyValue) ELSE 0,
                    0,
                    1,
                    InvText.ITNum).
       ufkey = TRUE.
       NEXT LOOP.

     END. 


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).
       ASSIGN 
             ac-hdr = " CHANGE " 
             ufkey  = TRUE 
             ehto = 9. 
       RUN ufkey.
       cfc = "lis". 
       RUN ufcolor. 
       CLEAR FRAME lis NO-PAUSE.
       DISPLAY InvText.KeyValue.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhInvText).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhInvText).

       RUN local-disp-row.
       xrecid = recid(InvText).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(InvText) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(InvText) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN DO:
         IF llMore THEN DO:
            llMore = FALSE.
            ufkey = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            xRecid = 0.
            LEAVE LOOP.
         END.
     END. 

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
ASSIGN si-recid    = xrecid        
       gcHelpParam = "".


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND InvText WHERE recid(InvText) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE FIND InvText WHERE recid(InvText) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icTarget = "" THEN DO:

       IF order = 1 THEN DO:
         IF llShowHistory THEN
            FIND FIRST InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
         ELSE FIND FIRST InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
         END.
       ELSE IF order = 2 THEN DO:
         IF llShowHistory THEN
            FIND FIRST InvText WHERE 
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX target 
               NO-LOCK NO-ERROR.
         ELSE FIND FIRST InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX target
               NO-LOCK NO-ERROR.
       END.
   END.
   ELSE DO:
       IF icKeyValue > "" 
       THEN FIND FIRST InvText 
          WHERE InvText.Brand    = lcBrand    AND
                InvText.Target   = icTarget   AND
                InvText.KeyValue = icKeyValue
          USE-INDEX Target
       NO-LOCK NO-ERROR.

       ELSE FIND FIRST InvText 
          WHERE InvText.Brand    = lcBrand AND
                InvText.Target   = icTarget
          USE-INDEX Target
       NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF icTarget = "" THEN DO:
       IF order = 1 THEN DO:
         IF llShowHistory THEN
            FIND LAST InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
         ELSE FIND LAST InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
         IF llShowHistory THEN
            FIND LAST InvText WHERE 
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX target 
               NO-LOCK NO-ERROR.
         ELSE FIND LAST InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX target
               NO-LOCK NO-ERROR.
       END.
   END.
   ELSE DO:
       IF icKeyValue > "" 
       THEN FIND LAST InvText 
          WHERE InvText.Brand    = lcBrand    AND
                InvText.Target   = icTarget   AND
                InvText.KeyValue = icKeyValue
          USE-INDEX Target
       NO-LOCK NO-ERROR.

       ELSE FIND LAST InvText 
          WHERE InvText.Brand    = lcBrand AND
                InvText.Target   = icTarget
          USE-INDEX Target
       NO-LOCK NO-ERROR.
   END.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF icTarget = "" THEN DO:
       IF order = 1 THEN DO:
         IF llShowHistory THEN
            FIND NEXT InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
         ELSE FIND NEXT InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
         IF llShowHistory THEN
            FIND NEXT InvText WHERE 
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX target 
               NO-LOCK NO-ERROR.
         ELSE FIND NEXT InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX target
               NO-LOCK NO-ERROR.
       END.
   END.       
   ELSE DO:
       IF icKeyValue > "" 
       THEN FIND NEXT InvText 
          WHERE InvText.Brand    = lcBrand    AND
                InvText.Target   = icTarget   AND
                InvText.KeyValue = icKeyValue
          USE-INDEX Target
       NO-LOCK NO-ERROR.

       ELSE FIND NEXT InvText 
          WHERE InvText.Brand    = lcBrand AND
                InvText.Target   = icTarget
          USE-INDEX Target
       NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF icTarget = "" THEN DO:
       IF order = 1 THEN DO:
         IF llShowHistory THEN
            FIND PREV InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
         ELSE FIND PREV InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX FromDate
               NO-LOCK NO-ERROR.
       END.
       ELSE IF order = 2 THEN DO:
         IF llShowHistory THEN
            FIND PREV InvText WHERE 
               InvText.Brand = lcBrand AND
               InvText.ToDate < TODAY
               USE-INDEX target 
               NO-LOCK NO-ERROR.
         ELSE FIND PREV InvText WHERE
               InvText.Brand = lcBrand AND
               InvText.ToDate >= TODAY
               USE-INDEX target
               NO-LOCK NO-ERROR.
       END.
   END.


   ELSE DO:
       IF icKeyValue > "" 
       THEN FIND PREV InvText 
          WHERE InvText.Brand    = lcBrand    AND
                InvText.Target   = icTarget   AND
                InvText.KeyValue = icKeyValue
          USE-INDEX Target
       NO-LOCK NO-ERROR.

       ELSE FIND PREV InvText 
          WHERE InvText.Brand    = lcBrand AND
                InvText.Target   = icTarget
          USE-INDEX Target
       NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.

       DISPLAY 
       InvText.FromDate
       InvText.ToDate
       InvText.Target
       InvText.KeyValue
       InvText.Language
       lcTitle
       lcReport
       llMemo
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND t-target WHERE t-target.target = InvText.Target NO-LOCK NO-ERROR.

   IF InvText.Position > 0 AND InvText.Position <= NUM-ENTRIES(ppos) THEN
      tpos = ENTRY(InvText.Position,ppos). 
   ELSE
      tpos = "!! INVALID VALUE !!".
   expl = t-target.expl.    
   targetname = fValKeyValue(InvText.Target,InvText.KeyValue,TRUE).   

   FIND Language WHERE Language.Language = Invtext.Language NO-LOCK NO-ERROR.
   IF AVAIL Language THEN lcLanguage = Language.LangName.
   ELSE lcLanguage = "".

   ASSIGN lcReport = fRepType(InvText.Report)
          lcTitle  = IF InvText.MainTitle > ""
                     THEN InvText.MainTitle
                     ELSE InvText.TxtTitle.

   llMemo = CAN-FIND(FIRST Memo WHERE
                           Memo.Brand     = gcBrand   AND
                           Memo.HostTable = "InvText" AND
                           Memo.KeyValue  = STRING(InvText.ITNum)).
END PROCEDURE.

PROCEDURE local-update-record:

   MaintMenu:
   REPEAT ON ENDKEY UNDO, LEAVE:

      CLEAR FRAME lis NO-PAUSE.

      RUN local-find-others.

      ASSIGN lcType    = fTypeName(InvText.InfoType)   
             lcAddress = fAddrTarget(InvText.AddrTarget)
             liLength  = LENGTH(InvText.InvText).

      DISP 
         InvText.Target
         InvText.Category
         InvText.ITNum
         expl
         InvText.KeyValue
         targetname
         InvText.FromDate 
         InvText.ToDate 
         InvText.TxtTitle 
         InvText.Position
         tpos
         InvText.Language
         lcLanguage
         InvText.Report
         InvText.SendRule
         InvText.Attachment
         InvText.AddrTarget
         lcAddress
         InvText.Category
         InvText.LetterClass
         lcReport
         InvText.MainTitle
         InvText.InvText 
         InvText.InfoType
         lcType
         liLength
      WITH FRAME lis.
      
      IF NEW InvText THEN toimi = 1.
      ELSE DO: 
         ASSIGN 
            ehto   = 0
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND gcHelpParam = ""
            ufk[8] = 8.
         RUN ufkey.
      END.
      
      IF toimi = 1 THEN 
      REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE MaintMenu:

         FIND CURRENT InvText EXCLUSIVE-LOCK.
            
         ehto = 9.
         RUN ufkey.
         
         CASE InvText.Target:
             WHEN "BillItem" THEN DO:
                  InvText.Position:HELP = 
                  "1:InvStart 2:InvEnd 5:InvRowStart 6:InvRowEnd".
                  LiMax = 6.
             END.
             WHEN "InvSect" THEN DO:
                  InvText.Position:HELP = 
                  "1:InvStart 2:InvEnd 3:SectStart 4:SectEnd".
                  LiMax = 4.
             END.

             WHEN "Printcont" OR WHEN "OrderConf" THEN DO:
                  InvText.Position:HELP = 
                  "Order of Print Sections".
                  LiMax = 4.
             END.

             OTHERWISE DO:
                  InvText.Position:HELP = "1:InvStart 2:InvEnd".
                  LiMax = 2.
             END.
         END CASE.


         UPDATE
            InvText.FromDate
            InvText.ToDate
            InvText.Position
            InvText.Report
            InvText.Attachment
            InvText.AddrTarget
            InvText.Language
            InvText.InfoType
            InvText.SendRule WHEN InvText.Target EQ "SMS"
            InvText.Category
            InvText.LetterClass
            InvText.MainTitle
            InvText.TxtTitle
            InvText.InvText
         WITH FRAME lis  EDITING:

            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,"InfoType,AddrTarget") > 0
            THEN DO:

               IF FRAME-FIELD = "InfoType" THEN DO:
                  RUN h-tmscodes(INPUT "InvText",    /* TableName */
                                       "InfoType", /* FieldName */
                                       "CustCare",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:

                     DISPLAY lcCode ;& InvText.InfoType WITH FRAME lis.   

                     lcType = fTypeName(INPUT FRAME lis InvText.InfoType).
                     DISPLAY lcType WITH FRAME lis. 
                  END.
               END.

               ELSE IF FRAME-FIELD = "AddrTarget" THEN DO:
                  RUN h-tmscodes(INPUT "InvText",    /* TableName */
                                       "AddrTarget", /* FieldName */
                                       "Printing",     /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:

                     DISPLAY INTEGER(lcCode) ;& InvText.AddrTarget  
                     WITH FRAME lis.   
                        
                     lcAddress = fAddrTarget(INTEGER(lcCode)).
                     DISPLAY lcAddress WITH FRAME lis. 
                  END.
               END.

               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 
            THEN DO WITH FRAME lis:

               PAUSE 0.

               IF frame-field = "FromDate" THEN
               DO:
                  IF INPUT InvText.FromDate = ? THEN
                  DO:
                     BELL.
                     MESSAGE "You must give beginning date !".
                     NEXT.
                  END.
               END.

               ELSE IF frame-field = "ToDate" THEN
               DO:
                  IF INPUT InvText.ToDate NE ? THEN
                  DO:
                     IF INPUT InvText.ToDate < INPUT InvText.FromDate THEN  
                     DO:
                        BELL.
                        MESSAGE "FromDate must NOT be later than ToDate !".
                        NEXT.
                     END.
                  END.
                  ELSE 
                  DO:
                     BELL.
                     MESSAGE "You must give ending date !".
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "Position" THEN DO:

                  IF INPUT InvText.Position = 0 OR INPUT InvText.Position >
                  LiMax THEN DO:
                     BELL.
                     MESSAGE "Invalid position code !".
                     NEXT.
                  END.
                  DISP ENTRY(INPUT InvText.Position,ppos) @ tpos.   
               END.

               ELSE IF FRAME-FIELD = "Language" THEN DO:
                  FIND Language WHERE Language.Language = INPUT InvText.Language
                  NO-LOCK NO-ERROR.
                  IF AVAIL LANGUAGE THEN DISP Language.LangName @ lcLanguage.
                  ELSE DO:
                     MESSAGE "Invalid language !".
                     NEXT.
                  END.
               END.

               ELSE IF FRAME-FIELD = "Report" THEN DO:
                  IF INPUT InvText.Report > 5 THEN DO:
                     MESSAGE "Invalid Code !".
                     NEXT.
                  END.
                  DISP fRepType(INPUT INPUT InvText.Report) @ lcReport.
               END.

               ELSE IF FRAME-FIELD = "InfoType" THEN DO:
                  IF INPUT InvText.InfoType = ""
                  THEN lcType = "".
                  ELSE DO:
                     lcType = fTypeName(INPUT FRAME lis InvText.InfoType).
                     IF lcType = "" THEN DO:
                        MESSAGE "Unknown type.".
                        NEXT.                        
                     END.
                  END.
                  DISPLAY lcType.
               END.

               ELSE IF FRAME-FIELD = "AddrTarget" THEN DO:
                  IF INPUT InvText.AddrTarget = 0
                  THEN lcAddress = "".
                  ELSE DO:
                     lcAddress = fAddrTarget(INPUT FRAME lis
                                             InvText.AddrTarget).
                     IF lcAddress = "" THEN DO:
                        MESSAGE "Unknown address target.".
                        NEXT.                        
                     END.
                  END.
                  DISPLAY lcAddress.
               END.

               ELSE IF FRAME-FIELD = "InvText" THEN DO:
                
                  IF InvText.Target = "SMS" AND 
                     LENGTH(INPUT FRAME lis InvText.InvText) > 160
                  THEN DO:
                     ok = FALSE.
                     MESSAGE "Length of text exceeds 160 characters." SKIP
                             "Do You still want to save this text ?"
                     VIEW-AS ALERT-BOX QUESTION
                     BUTTONS YES-NO
                     SET ok.
                     IF NOT ok THEN NEXT.
                  END.
               END.
               ELSE IF frame-field = "SendRule" THEN
               DO:
                  IF LOOKUP(INPUT InvText.SendRule,{&SMS_SENDRULES}) EQ 0
                  THEN DO:
                     BELL.
                     MESSAGE "Unknown value".
                     NEXT.
                  END.
               END.
            END.   

            APPLY LASTKEY.

         END. /* EDITING */   
         
         LEAVE.
         
      END.  /* toimi=1 */

      CLEAR FRAME lis NO-PAUSE.
      LEAVE.
   END.

END PROCEDURE.


