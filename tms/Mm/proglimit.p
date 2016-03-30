/* -----------------------------------------------
  MODULE .......: ProgLimit
  FUNCTION .....: Maintain Servicelimitgroup analyse
  APPLICATION ..: TMS
  AUTHOR .......: JP
  CREATED ......: 24-11-05
  MODIFIED .....: 
  VERSION ......: SCRUNKO3
  ------------------------------------------------------ */

{Syst/commali.i} 
{Syst/eventval.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {Func/lib/eventlog.i}
        
    DEF VAR lhProgLimit AS HANDLE NO-UNDO.
    lhProgLimit = BUFFER ProgLimit:HANDLE.
    RUN StarEventInitialize(lhProgLimit).
                    
    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhProgLimit).
    END.
END.

DEF INPUT PARAMETER  iislseq AS INT            NO-UNDO.

DEF   shared VAR siirto  AS CHAR.


DEF VAR llShowHistory        AS LOG            NO-UNDO.
DEF VAR lcEvent    LIKE ProgLimit.GroupCode    NO-UNDO.
DEF VAR xrecid     AS RECID                           init ?.
DEF VAR firstline  AS INT                    NO-UNDO  init 0.
DEF VAR order      AS INT                    NO-UNDO  init 1.
DEF VAR ordercount AS INT                    NO-UNDO  init 2.
DEF VAR ufkey      AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline    AS INT                    NO-UNDO  init 0.
DEF VAR ex-order   AS INT                    NO-UNDO.
DEF VAR memory     AS RECID                  NO-UNDO.
def var line       as int format "99"        NO-UNDO.
DEF VAR must-print AS LOG                    NO-UNDO.
DEF VAR must-add   AS LOG                    NO-UNDO.
DEF VAR fr-header  AS CHAR                   NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24        NO-UNDO.
DEF VAR i          AS INT                    NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.
DEF VAR lcWeekday  AS cHAR                   NO-UNDO FORMAT "X(40)" .
DEF VAR lcTypeName AS CHAR                   NO-UNDO FORMAT "X(25)" .
DEF VAR lcCode     AS CHAR                   NO-UNDO.
DEF VAR lcUnit     AS CHAR                   NO-UNDO.
DEF VAR lcClitype  AS CHAR                   NO-UNDO.
DEF VAR lcBillCode AS CHAR                   NO-UNDO.
DEF VAR lcBdest    AS CHAR                   NO-UNDO.
DEF VAR ldtValidF  AS DATE                   NO-UNDO FORMAT "99-99-9999".
DEF VAR ldtValidT  AS DATE                   NO-UNDO FORMAT "99-99-9999".
DEF VAR liPrior    AS INT                    NO-UNDO.
DEF VAR lcCCN      AS CHAR                   NO-UNDO.
DEF VAR lcServicel AS CHAR                   NO-UNDO  FORMAT "X(12)" .
DEF VAR llOK       AS LOG                    NO-UNDO.
DEF VAR lcTitle    AS CHAR                   NO-UNDO.

DEF BUFFER     xxProgLimit FOR ProgLimit.

 
form
      
   Servicelimit.SLName   FORMAT "X(19)" column-label  "ServLim"
   ProgLimit.LimitFrom 
   ProgLimit.LimitTo     
   ProgLimit.ValidFrom   FORMAT "99-99-99" COLUMN-LABEL "From"
   ProgLimit.ValidTo     FORMAT "99-99-99" COLUMN-LABEL "To"

WITH width 74 OVERLAY CENTERED scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi + lcTitle + " "  
   + string(pvm,"99-99-99") + " "  FRAME sel.

form          
   ProgLimit.GroupCode       COLON 23               SKIP
   ProgLimit.SLSeq     LABEL "SLSeq"      COLON 23 
       ServiceLimit.SLName AT 42    NO-LABEL 
   ProgLimit.ValidFrom     COLON 23 format 99-99-9999  SKIP
   ProgLimit.ValidTo       COLON 23 format 99-99-9999  SKIP 
   ProgLimit.Bdest         COLON 23 FORMAT "X(20)" Bdest.BDName NO-LABEL
   ServiceLimit.inclunit   COLON 23  lcUnit NO-LABEL   SKIP
   
   ProgLimit.LimitFrom     COLON 23                    SKIP
   ProgLimit.LimitTo       COLON 23                  
WITH OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH SIDE-LABELS FRAME lis.

form /*  search WITH FIELD ProgLimit */
    lcEvent
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND Event "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

FIND FIRST ServiceLimit WHERE 
           ServiceLimit.SLSeq = iiSLSEq NO-LOCK NO-ERROR.
            
IF AVAIL ServiceLimit then lcTitle = ServiceLimit.GroupCode.
ELSE DO:
   MESSAGE 
   "Unknown service limit!"
   VIEW-AS ALERT-BOX.
   RETURN.

END.
cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE ProgLimit THEN ASSIGN
   memory     = recid(ProgLimit)
   must-print = TRUE
   must-add   = FALSE.
ELSE ASSIGN
   memory     = ?
   must-print = FALSE
   must-add   = TRUE.

LOOP:
repeat WITH FRAME sel:

   IF order <> ex-order THEN DO:
      ex-order = order.
   END.

   IF must-add THEN DO:  /* ProgLimit -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:

           CREATE ProgLimit.
           ASSIGN
           ProgLimit.GroupCode = ServiceLimit.GroupCode
           ProgLimit.SLSeq     = iiSlseq .

           DISP ProgLimit.GroupCode ProgLimit.SLSeq Servicelimit.SLName 
           WITH FRAME lis.
           PAUSE 0.
           RUN LOCAL-UPDATE-RECORD(true).
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
           ProgLimit.GroupCode = ""  THEN
           UNDO add-new, LEAVE add-new.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhProgLimit).
           
           ASSIGN
              memory = recid(ProgLimit)
              xrecid = memory
              must-add = false.
           LEAVE add-new.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE ProgLimit THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:

        up FRAME-LINE - 1.
        FIND ProgLimit where recid(ProgLimit) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:

           IF AVAILABLE ProgLimit  THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(ProgLimit).
              RUN LOCAL-FIND-NEXT.
           END.
           ELSE DO:
              CLEAR no-pause.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.

        up FRAME-LINE - 1.
        DOWN firstline.
        ASSIGN firstline = 0
               must-print = FALSE
               .
        PAUSE 0 no-message.

        /* one page of data has been printed AND
        the cursor is in the upmost line FOR 'choose' */
      END. /* must-print = TRUE */
   END. /* print-line */

   /* IF lastly a line has been deleted */
   IF delline > 0 THEN DOWN delline - 1.
   ASSIGN delline = 0.

   BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:
         
      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF llShowHistory = TRUE THEN ufk[4]= 38.
        ELSE                         ufk[4]= 37.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.

      IF order = 1 THEN DO:
        CHOOSE ROW ProgLimit.LimitFrom {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) ProgLimit.validfrom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ProgLimit.LimitFrom {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) ProgLimit.validto WITH FRAME sel.
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
      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND ProgLimit where recid(ProgLimit) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND ProgLimit where recid(ProgLimit) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE ProgLimit THEN DO:
              message "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* a previous one was found */
              scroll DOWN.
              RUN LOCAL-DISP-ROW.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
              rtab[1] = recid(ProgLimit)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND ProgLimit where recid(ProgLimit) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE ProgLimit THEN DO:
              message "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* yet another record was found */
              scroll up.
              RUN LOCAL-DISP-ROW.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(ProgLimit).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ProgLimit where recid(ProgLimit) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE ProgLimit THEN DO:
           memory = recid(ProgLimit).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE ProgLimit THEN memory = recid(ProgLimit).
              ELSE line = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* this is the FIRST data page */
           message "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 no-message.
        END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* cursor TO the downmost line */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           message "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 no-message.
       END.
       ELSE DO: /* the downmost line wasn't empty */
           memory = rtab[FRAME-DOWN].
           FIND ProgLimit where recid(ProgLimit) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF lookup(nap,"4,f4") > 0 THEN DO:  

         IF llShowHistory = FALSE THEN llShowHistory = TRUE.
         ELSE                          llShowHistory = FALSE.

         run local-find-first.
         RUN Syst/ufkey.
         must-print = true.
         ufkey = true.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND ProgLimit where recid(ProgLimit) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          
          ProgLimit.ValidTo
          ProgLimit.ValidFrom
          ProgLimit.LimitFrom
          ProgLimit.LimitTo
          Servicelimit.SLName WHEN AVAIL ServiceLimit.

       RUN LOCAL-FIND-NEXT.
       
       IF AVAILABLE ProgLimit THEN memory = recid(ProgLimit).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND ProgLimit where recid(ProgLimit) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE ProgLimit THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(ProgLimit).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND ProgLimit where recid(ProgLimit) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.

       COLOR DISPLAY value(ccc)
          ProgLimit.ValidTo
          ProgLimit.ValidFrom
          ProgLimit.LimitFrom
          ProgLimit.LimitTo
          Servicelimit.SLName WHEN AVAIL ServiceLimit.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhProgLimit).

           DELETE ProgLimit.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST ProgLimit
           /* search condition */) THEN DO:
              CLEAR FRAME sel no-pause.
              PAUSE 0 no-message.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST ProgLimit where 
            recid(ProgLimit) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.

       cfc = "lis". RUN Syst/ufcolor.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhProgLimit).

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       xrecid = recid(ProgLimit).

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhProgLimit).

       run local-find-first.
       RUN Syst/ufkey.
       must-print = true.
       ufkey = true.
       NEXT LOOP.
                                                  
     
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(ProgLimit) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(ProgLimit) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   
   FIND FIRST ServiceLimit WHERE
              ServiceLimit.slseq = ProgLimit.SLSeq NO-LOCK NO-ERROR.

   DISPLAY
      ProgLimit.ValidTo
      ProgLimit.ValidFrom
      ProgLimit.LimitFrom
      ProgLimit.LimitTo
      Servicelimit.SLName WHEN AVAIL ServiceLimit
      
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 AND not llShowHistory  THEN 
      FIND NEXT ProgLimit WHERE 
                ProgLimit.SLSeq      = iislseq AND 
                ProgLimit.ValidFrom <= today   AND 
                ProgLimit.ValidTo   >= today
      NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN 
      FIND NEXT ProgLimit NO-LOCK NO-ERROR.

   ELSE IF order = 2 AND not llShowHistory  THEN 
      FIND NEXT ProgLimit  WHERE 
                ProgLimit.SLSeq      = iislseq AND
                ProgLimit.ValidFrom <= today   AND 
                ProgLimit.ValidTo   >= today
      NO-LOCK NO-ERROR.

   ELSE IF order = 2 THEN 
      FIND NEXT ProgLimit WHERE 
                ProgLimit.SLSeq      = iislseq 
      NO-LOCK NO-ERROR.
                  

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 AND not llShowHistory THEN 
      FIND PREV ProgLimit  WHERE 
                ProgLimit.ValidFrom <= today   AND
                ProgLimit.ValidTo   >= today   AND 
                ProgLimit.SLSeq      = iislseq 
      NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN 
      FIND Prev ProgLimit  NO-LOCK NO-ERROR.

   ELSE IF order = 2 AND not llShowHistory THEN 
      FIND PREV ProgLimit WHERE 
                ProgLimit.SLSeq      = iislseq AND
                ProgLimit.ValidFrom <= today   AND
                ProgLimit.ValidTo   >= today
      NO-LOCK NO-ERROR.

   ELSE IF order = 2 THEN 
      FIND Prev ProgLimit WHERE 
                ProgLimit.SLSeq      = iislseq 
      NO-LOCK NO-ERROR.
                   

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF order = 1 AND not llShowHistory THEN 
      FIND FIRST ProgLimit  WHERE 
                 ProgLimit.ValidFrom <= today   AND
                 ProgLimit.ValidTo   >= today   AND 
                 ProgLimit.SLSeq      = iislseq 
                   
      NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND FIRST ProgLimit NO-LOCK NO-ERROR.
   ELSE IF order = 2 AND not llShowHistory THEN 
      FIND FIRST ProgLimit WHERE 
                 ProgLimit.SLSeq      = iislseq AND
                 ProgLimit.ValidFrom <= today   AND
                 ProgLimit.ValidTo   >= today    
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST ProgLimit  WHERE 
                 ProgLimit.SLSeq      = iislseq 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 AND not llShowHistory  THEN 
      FIND LAST ProgLimit WHERE 
                ProgLimit.SLSeq      = iislseq AND
                ProgLimit.ValidFrom <= today   AND
                ProgLimit.ValidTo   >= today
      NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN
   FIND LAST ProgLimit  WHERE 
             ProgLimit.SLSeq      = iislseq 
   NO-LOCK NO-ERROR.
   ELSE IF order = 2 AND not llShowHistory  THEN 
      FIND LAST ProgLimit WHERE 
                ProgLimit.SLSeq      = iislseq AND
                ProgLimit.ValidFrom <= today   AND
                ProgLimit.ValidTo   >= today
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
   FIND LAST ProgLimit WHERE 
             ProgLimit.SLSeq      = iislseq 
   NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.
   

   FIND FIRST Bdest WHERE 
              Bdest.Brand = gcBrand AND 
              BDest.BDest = ProgLimit.BDest AND
              BDest.ToDate >= ProgLimit.ValidFrom AND
              BDest.FromDate <= ProgLimit.ValidTo NO-LOCK NO-ERROR.
            
   FIND FIRST ServiceLimit WHERE 
              ServiceLimit.slseq = ProgLimit.SLSeq NO-LOCK NO-ERROR.
      
   FIND FIRST TMSCodes WHERE
              TMSCodes.Tablename    = "Daycampaign"   AND
              TMSCodes.FieldName    = "InclUnit"      AND
              TMSCodes.CodeGroup    = "Unit"          AND
              TMSCodes.CodeValue    =  STRING(Servicelimit.inclunit)
   NO-LOCK NO-ERROR.

   IF Avail TMSCodes THEN lcUnit = TMSCodes.CodeName.
   ELSE                   lcUnit = "".

   PAUSE 0.
   DISP 
      ProgLimit.GroupCode
      ProgLimit.ValidTo
      ProgLimit.ValidFrom
      ProgLimit.LimitFrom
      ProgLimit.LimitTo
      ProgLimit.Bdest
      ProgLimit.slseq 
      Servicelimit.SLName   WHEN AVAIL ServiceLimit
      lcUnit
      Bdest.BDname          WHEN AVAIL Bdest 
   WITH FRAME lis.
   PAUSE 0.      
   UPDATE 
      ProgLimit.ValidFrom
      ProgLimit.ValidTo
      ProgLimit.Bdest
      ProgLimit.LimitFrom
      ProgLimit.LimitTo
   WITH FRAME lis EDITING: 
      
      READKEY. 
      
      IF keylabel(LASTKEY) = "F9" AND
         FRAME-FIELD = "SLGAType"  THEN DO:
      END.
      ELSE IF keylabel(LASTKEY) = "F9" AND
          FRAME-FIELD = "ServiceLimitGroup"  THEN DO:
      END.   
      
      nap = KEYLABEL(LASTKEY). 
      IF lookup(nap,poisnap) > 0 THEN DO:
         if keylabel(lastkey) = "F4" THEN LEAVE . 

         IF FRAME-FIELD = "Bdest" THEN DO:
            FIND FIRST Bdest WHERE 
                       Bdest.Brand = gcBrand AND
                       Bdest.Bdest = input frame lis ProgLimit.Bdest AND
                       BDest.ToDate >= INPUT FRAME lis ProgLimit.ValidFrom AND
                       BDest.FromDate <= INPUT FRAME lis ProgLimit.ValidTo
            NO-LOCK NO-ERROR.
 
            IF NOT AVAIL Bdest THEN DO:
               BELL.
               MESSAGE 
               "Unknown B-Destination" .
               NEXT-prompt ProgLimit.Bdest.
               NEXT.
            END.
            DISP Bdest.bdname  WITH FRAME lis. PAUSE 0.
         END.
         
         ELSE IF FRAME-FIELD = "ServiceLimitGroup" THEN DO:
            
         END.

         ELSE IF FRAME-FIELD = "validfrom" THEN DO:
            if input frame lis ProgLimit.ValidFrom  = ? THEN DO:
               BELL.
               MESSAGE 
               "from Date can't be empty!".
               NEXT-prompt ProgLimit.ValidFrom.
               NEXT.
            END.
         END.
         
         ELSE IF FRAME-FIELD = "validTo" THEN DO:
            if input frame lis ProgLimit.ValidTo  = ? THEN DO:
               BELL.
               MESSAGE 
               "to Date can't be empty!".
               NEXT-prompt ProgLimit.ValidTo.
               NEXT.
            END.
         END.

      END.
      
      APPLY LASTKEY. 
   
   END.
   HIDE FRAME lis.
   
END.

