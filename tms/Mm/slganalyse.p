/* -----------------------------------------------
  MODULE .......: SLGAnalyse
  FUNCTION .....: Maintain Servicelimitgroup analyse
  APPLICATION ..: TMS
  AUTHOR .......: JP
  CREATED ......: 24-11-05
  MODIFIED .....: 
  VERSION ......: SCRUNKO3
  ------------------------------------------------------ */

{commali.i} 
{eventval.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {lib/eventlog.i}
        
    DEF VAR lhSLGAnalyse AS HANDLE NO-UNDO.
    lhSLGAnalyse = BUFFER SLGAnalyse:HANDLE.
    RUN StarEventInitialize(lhSLGAnalyse).
                    
    ON F12 ANYWHERE DO:
        run eventview2.p(lhSLGAnalyse).
    END.
END.

/* not used, yet */ 
DEF var /* INPUT PARAMETER */ icSearchRule AS CHAR NO-UNDO.
DEF VAR llShowHistory                      AS LOG  NO-UNDO.
DEF  NEW  shared VAR siirto AS CHAR.
DEF VAR lcEvent    LIKE SLGAnalyse.CliType  NO-UNDO.
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
DEF VAR lcServicel AS CHAR                  NO-UNDO  FORMAT "X(12)" .
DEF VAR lcSLGAtype AS INT                   NO-UNDO  FORMAT "9". 
DEF VAR llOK       AS LOG                    NO-UNDO.
DEF VAR lidestType AS INT                   NO-UNDO.

DEF BUFFER xxSLGAnalyse FOR SLGAnalyse.
DEF TEMP-TABLE ttSLG LIKE slganalyse.
 
form
   SLGAnalyse.BelongTo   COLUMN-LABEL "B" 
   SLGAnalyse.CliType      
   SLGAnalyse.BillCode  FORMAT "X(15)"   
   SLGANAlyse.CCN       FORMAT ">>>9"
   SLGANalyse.Bdest     FORMAT "X(8)" 
   SLGAnalyse.ValidTo   
   SLGAnalyse.ServiceLimitGroup
   SLGAnalyse.Prior   FORMAT "99"

WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " SLG ANALYSE " 
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form          
   SLGAnalyse.BelongTo      COLON 23    SKIP                    
   SLGAnalyse.CliType       COLON 23  
      CliType.CliName NO-LABEL          SKIP
   SLGAnalyse.BillCode      COLON 23
      BillItem.BIName NO-LABEL          SKIP
   SLGAnalyse.CCN     LABEL "Rating CCN"      COLON 23 format ">>>9" 
       CCN.CCNName AT 42    NO-LABEL 
   SLGAnalyse.ValidFrom     COLON 23 format 99-99-9999  SKIP
   SLGAnalyse.ValidTo       COLON 23 format 99-99-9999  SKIP 
   SLGAnalyse.Bdest         COLON 23 Bdest.BDName   NO-LABEL 
   SLGAnalyse.SLGAType      COLON 23
     lcTypeName NO-LABEL                                SKIP  
   SLGAnalyse.ServiceLimitGroup  COLON 23 FORMAT "X(20)" SKIP
   SLGAnalyse.Prior              COLON 23 FORMAT "99"
WITH OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH SIDE-LABELS FRAME lis.

FORM
   SKIP
   "This program will generate a periodical contract rules for chosen criteria." SKIP(1)
   "  Substype.....:" lcClitype     Clitype.Cliname          SKIP
   "  BillCode.....:" lcBillCode    BillItem.Biname          SKIP
   "  RateCCN......:" lcCCN format ">>>9"  CCN.CCNNAme       SKIP
   "  Type of Bdest:" lidestType                             SKIP
   "  ValidFrom....:" ldtValidF                              SKIP
   "  ValidTo......:" ldtValidT                              SKIP
   "  BDest........:" lcBDest       BDest.BDName             SKIP
   "  Priority.....:" liPrior                                SKIP
   "  Target Type..:" lcSLGAType  lcTypeName                 SKIP
   "  Target.......:" lcServicel            

WITH OVERLAY ROW 4 centered
COLOR value(cfc) TITLE "GENERATE ANALYSE ROWS" 
side-label NO-LABEL
FRAME Generate.
                          

form /*  search WITH FIELD SLGAnalyse */
    lcEvent
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND Event "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

                     

RUN LOCAL-FIND-FIRST.

IF AVAILABLE SLGAnalyse THEN ASSIGN
   memory     = recid(SLGAnalyse)
   must-print = TRUE
   must-add   = FALSE.
ELSE ASSIGN
   memory     = ?
   must-print = FALSE
   must-add   = FALSE.


LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO:  /* SLGAnalyse -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
      
      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSACTION:

           CREATE SLGAnalyse.
           SLGAnalyse.Brand = gcBrand.

           RUN LOCAL-UPDATE-RECORD(true).
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
           SLGAnalyse.CliType = ""  THEN
           UNDO add-new, LEAVE add-new.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhSLGAnalyse).
           
           ASSIGN
              memory = recid(SLGAnalyse)
              xrecid = memory
              must-add = false.
           LEAVE add-new.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE SLGAnalyse THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:

        up FRAME-LINE - 1.
        FIND SLGAnalyse where recid(SLGAnalyse) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:

           IF AVAILABLE SLGAnalyse  THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(SLGAnalyse).
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
        ufk[1]= 847  ufk[2]= 0 ufk[3]= 2104 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.

        IF llShowHistory = TRUE THEN ufk[4]= 38.
        ELSE                         ufk[4]= 37.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW SLGAnalyse.CliType ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) SLGAnalyse.CliType WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SLGAnalyse.BillCode ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) SLGAnalyse.CliType WITH FRAME sel.
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
        FIND SLGAnalyse where recid(SLGAnalyse) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND SLGAnalyse where recid(SLGAnalyse) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE SLGAnalyse THEN DO:
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
              rtab[1] = recid(SLGAnalyse)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND SLGAnalyse where recid(SLGAnalyse) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE SLGAnalyse THEN DO:
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
              rtab[FRAME-DOWN] = recid(SLGAnalyse).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND SLGAnalyse where recid(SLGAnalyse) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE SLGAnalyse THEN DO:
           memory = recid(SLGAnalyse).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE SLGAnalyse THEN memory = recid(SLGAnalyse).
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
           FIND SLGAnalyse where recid(SLGAnalyse) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF lookup(nap,"1,f1") > 0 THEN DO:  
        RUN slgareport.
        NEXT LOOP.
     END.
     
     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF lookup(nap,"3,f3") > 0 THEN DO:  
        ufk = 0.
        run ufkey.
        RUN LOCAL-GENERATE-RECORD.
        run local-find-first.
        run ufkey.
        must-print = true.
        ufkey = true.
        NEXT LOOP.
     END.

     ELSE IF lookup(nap,"4,f4") > 0 THEN DO:  

         IF llShowHistory = FALSE THEN llShowHistory = TRUE.
         ELSE                          llShowHistory = FALSE.

         run local-find-first.
         run ufkey.
         must-print = true.
         ufkey = true.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND SLGAnalyse where recid(SLGAnalyse) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          SLGAnalyse.BelongTo
          SLGAnalyse.CliType 
          SLGAnalyse.ValidTo
          SLGAnalyse.CCN
          SLGANalyse.Bdest 
          SLGAnalyse.ServiceLimitGroup
          SLGAnalyse.Prior
          SLGAnalyse.BillCode.

       RUN LOCAL-FIND-NEXT.
       
       IF AVAILABLE SLGAnalyse THEN memory = recid(SLGAnalyse).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND SLGAnalyse where recid(SLGAnalyse) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE SLGAnalyse THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(SLGAnalyse).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND SLGAnalyse where recid(SLGAnalyse) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.

       COLOR DISPLAY value(ccc)
          SLGAnalyse.CliType 
          SLGAnalyse.ValidTo
          SLGAnalyse.CCN
          SLGANalyse.Bdest
          SLGAnalyse.ServiceLimitGroup
          SLGAnalyse.Prior
          SLGAnalyse.BelongTo
          SLGAnalyse.BillCode.

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhSLGAnalyse).

           DELETE SLGAnalyse.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST SLGAnalyse
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
     else if lookup(nap,"enter,return") > 0 
     THEN REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
     
       /* change */
       FIND FIRST SLGAnalyse where 
            recid(SLGAnalyse) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.

       cfc = "lis". RUN ufcolor.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhSlganalyse).

       RUN LOCAL-UPDATE-RECORD(FALSE).

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhSlganalyse).
       RUN local-disp-row.
       
       xrecid = recid(SLGAnalyse).
       LEAVE.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(SLGAnalyse) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(SLGAnalyse) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   
   FIND FIRST BillItem WHERE 
              BillItem.Brand    = SLGAnalyse.Brand     AND
              BillItem.BillCode = SLGAnalyse.BillCode 
   NO-LOCK NO-ERROR.

   DISPLAY
      SLGAnalyse.CliType  
      SLGAnalyse.BillCode   
      SLGAnalyse.ValidTo
      SLGAnalyse.CCN
      SLGANalyse.Bdest
      SLGAnalyse.Prior
      SLGAnalyse.ServiceLimitGroup
      SLGAnalyse.BelongTo
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 AND not llShowHistory  THEN 
      FIND NEXT SLGAnalyse use-index belongto WHERE 
                SLGAnalyse.Brand      = gcBrand AND 
                SLGAnalyse.ValidFrom <= today   AND 
                SLGAnalyse.ValidTo   >= today
      NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN 
      FIND NEXT SLGAnalyse use-index belongto WHERE
                  SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.

   ELSE IF order = 2 AND not llShowHistory  THEN 
      FIND NEXT SLGAnalyse use-index BillCode WHERE 
                SLGAnalyse.Brand      = gcBrand AND 
                SLGAnalyse.ValidFrom <= today   AND 
                SLGAnalyse.ValidTo   >= today
      NO-LOCK NO-ERROR.

   ELSE IF order = 2 THEN 
      FIND NEXT SLGAnalyse use-index BillCode WHERE
                SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.
                  

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 AND not llShowHistory THEN 
      FIND PREV SLGAnalyse use-index belongto WHERE 
                SLGAnalyse.Brand      = gcBrand AND
                SLGAnalyse.ValidFrom <= today   AND
                SLGAnalyse.ValidTo   >= today
                                                
      NO-LOCK NO-ERROR.

   ELSE IF order = 1 THEN 
      FIND Prev SLGAnalyse use-index belongto WHERE
                   SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.

   ELSE IF order = 2 AND not llShowHistory THEN 
      FIND PREV SLGAnalyse use-index BillCode WHERE 
                SLGAnalyse.Brand      = gcBrand AND
                SLGAnalyse.ValidFrom <= today   AND
                SLGAnalyse.ValidTo   >= today
      NO-LOCK NO-ERROR.

   ELSE IF order = 2 THEN 
      FIND Prev SLGAnalyse use-index billCode WHERE
                SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.
                   

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF order = 1 AND not llShowHistory THEN 
      FIND FIRST SLGAnalyse use-index belongto WHERE 
                 SLGAnalyse.Brand      = gcBrand AND
                 SLGAnalyse.ValidFrom <= today   AND
                 SLGAnalyse.ValidTo   >= today    
      NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN 
      FIND FIRST SLGAnalyse use-index belongto WHERE
                 SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 AND not llShowHistory THEN 
      FIND FIRST SLGAnalyse use-index BillCode WHERE 
                 SLGAnalyse.Brand      = gcBrand AND
                 SLGAnalyse.ValidFrom <= today   AND
                 SLGAnalyse.ValidTo   >= today    
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST SLGAnalyse use-index billCode WHERE
                 SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 AND not llShowHistory  THEN 
      FIND LAST SLGAnalyse use-index belongto WHERE 
                SLGAnalyse.Brand      = gcBrand AND
                SLGAnalyse.ValidFrom <= today   AND
                SLGAnalyse.ValidTo   >= today
      NO-LOCK NO-ERROR.
   ELSE IF order = 1 THEN
   FIND LAST SLGAnalyse use-index belongto WHERE
               SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 AND not llShowHistory  THEN 
      FIND LAST SLGAnalyse use-index billCode WHERE 
                SLGAnalyse.Brand      = gcBrand AND
                SLGAnalyse.ValidFrom <= today   AND
                SLGAnalyse.ValidTo   >= today
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
   FIND LAST SLGAnalyse use-index billCode WHERE
               SLGAnalyse.Brand      = gcBrand NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.
   
   DEF VAR liMatches AS INT NO-UNDO. 
   
   IF NOT bNew THEN DO:

      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand AND
                 BillItem.BillCode = SLGAnalyse.BillCode
      NO-LOCK NO-ERROR.

      FIND FIRST CliType WHERE
                 CliType.Brand    = gcBrand AND
                 Clitype.CliType  = SLGAnalyse.CliType
      NO-LOCK NO-ERROR.
    
      FIND FIRST TMSCodes WHERE
                 TMSCodes.Tablename    = "SLGAanalyse"   AND
                 TMSCodes.FieldName    = "SLGAType"      AND
                 TMSCodes.CodeGroup    = "Servicelimit"  AND
                 TMSCodes.CodeValue    = STRING(SLGAnalyse.slgaType)
      NO-LOCK NO-ERROR.

      FIND FIRST Bdest WHERE 
                 Bdest.Brand = gcBrand AND 
                 BDest.BDest = SLGAnalyse.BDest AND
                 BDest.ToDate >= SLGAnalyse.ValidFrom AND
                 BDest.FromDate <= SLGAnalyse.ValidTo NO-LOCK NO-ERROR.
            
      FIND FIRST CCN WHERE
                 CCN.Brand    = gcBrand AND
                 CCN.CCN      = SLGAnalyse.CCN
      NO-LOCK NO-ERROR.
      
      IF AVAIL TMSCodes THEN lcTypeName = TMSCodes.CodeName.
      ELSE lcTypeName = "".
      
      DISP 
         SLGAnalyse.CliType 
         BillItem.BIName       WHEN AVAIL BillItem
         SLGAnalyse.BillCode
         SLGAnalyse.SLGAType 
         lcTypeName
         Clitype.CliName       WHEN AVAIL Clitype
         Bdest.BDname          WHEN AVAIL Bdest 
         CCN.CCNName           WHEN AVAIL ccn
         SLGAnalyse.ValidFrom
         SLGAnalyse.ValidTo
         SLGAnalyse.ServiceLimitGroup
         SLGAnalyse.Prior
         SLGAnalyse.BelongTo
         
      WITH FRAME lis.
         
   END.
   
   UPDATE 
      SLGAnalyse.BelongTo WHEN bNew
      SLGAnalyse.CliType  WHEN bNew
      SLGAnalyse.BillCode 
      SLGanalyse.CCN
      SLGAnalyse.ValidFrom
      SLGAnalyse.ValidTo
      SLGAnalyse.Bdest
      SLGAnalyse.SLGAType
      SLGAnalyse.ServiceLimitGroup
      SLGAnalyse.Prior
   WITH FRAME lis EDITING: 
      
      READKEY. 
      
      IF keylabel(LASTKEY) = "F9" AND
         FRAME-FIELD = "SLGAType"  THEN DO:
 
         RUN h-tmscodes(INPUT "SLGAanalyse",    /* TableName */
                              "SLGAType",       /* FieldName */
                              "ServiceLimit",   /* GroupCode */
                              OUTPUT lcCode).
                               
         IF lcCode ne "" AND lcCode NE ? THEN DO:
            DISPLAY INTEGER(lcCode) @ SLGAnalyse.SLGAType
            WITH FRAME lis.
         END.
         ehto = 9.
         RUN ufkey.
         NEXT.
      END.
      ELSE IF keylabel(LASTKEY) = "F9" AND
          FRAME-FIELD = "ServiceLimitGroup"  THEN DO:

         IF      INPUT SLGAnalyse.SLGAType = 1 THEN run h-servlimitgrp.
         ELSE IF INPUT SLGAnalyse.SLGAType = 2 THEN run  h-daycamp.p.

         ASSIGN SLGAnalyse.ServiceLimitGroup = siirto.
         disp SLGAnalyse.ServiceLimitGroup WITH FRAME lis.
      END.   
      
      nap = KEYLABEL(LASTKEY). 
      IF lookup(nap,poisnap) > 0 THEN DO:
         if keylabel(lastkey) = "F4" THEN LEAVE . 

         IF FRAME-FIELD = "CliType" THEN DO:
            FIND FIRST CliType WHERE 
                       Clitype.Brand   = gcBrand AND
                       CliType.CliType = input frame lis SLGAnalyse.CliType
            NO-LOCK NO-ERROR.
            IF NOT AVAIL CliType THEN DO:
               BELL.
               MESSAGE 
               "Unknown Clitype Item" .
               NEXT-prompt CliType.CliType.
               NEXT.
            END.
            DISP CliType.CliName WITH FRAME lis. PAUSE 0.
         END.

         ELSE IF FRAME-FIELD = "BillItem" THEN DO:
            FIND FIRST BillItem WHERE 
                       BillItem.Brand    = gcBrand AND
                       BillItem.BillCode = input frame lis SLGAnalyse.BillCode
            NO-LOCK NO-ERROR.
            IF NOT AVAIL BillItem THEN DO:
               BELL.
               MESSAGE 
               "Unknown Billing Item" .
               NEXT-prompt SLGAnalyse.BillCode.
               NEXT.
            END.
            DISP BillItem.BIName WITH FRAME lis. PAUSE 0.
         END.

         ELSE IF FRAME-FIELD = "CCN" THEN DO:
            IF INPUT SLGAnalyse.CCN > 0 THEN DO:
               FIND FIRST CCN WHERE 
                          CCN.Brand    = gcBrand AND
                          CCN.CCN      = input frame lis SLGAnalyse.CCN
               NO-LOCK NO-ERROR.
               IF NOT AVAIL CCN THEN DO:
                  BELL.
                  MESSAGE 
                  "Unknown CCN Item" .
                  NEXT-prompt SLGAnalyse.CCN.
                  NEXT.
               END.
               DISP CCN.CCNName WITH FRAME lis. PAUSE 0.
            END.
            ELSE DISP "" @ CCN.CCNName WITH FRAME lis.
         END.
         ELSE IF FRAME-FIELD = "Bdest" THEN DO:
            IF INPUT SLGAnalyse.Bdest > "" THEN DO:
               FIND FIRST Bdest WHERE 
                     Bdest.Brand = gcBrand AND
                     Bdest.Bdest = input frame lis SLGAnalyse.Bdest AND
                     BDest.ToDate >= INPUT FRAME lis SLGAnalyse.ValidFrom AND
                     BDest.FromDate <= INPUT FRAME lis SLGAnalyse.ValidTo 
               NO-LOCK NO-ERROR.
 
               IF NOT AVAIL Bdest THEN DO:
            
                  liMatches = 0.
                  FOR EACH Bdest NO-LOCK WHERE 
                        Bdest.Brand = gcBrand AND
                        Bdest.Bdest MATCHES input frame lis SLGAnalyse.Bdest:
                     liMatches = liMatches + 1.
                  END.

                  IF liMatches <= 0 THEN DO:
                     BELL.
                     MESSAGE 
                     "Unknown B-Destination" .
                     NEXT-prompt SLGAnalyse.Bdest.
                     NEXT.
                  END.
                  ELSE DO:
                     DISP STRING(liMatches) + " matches found"
                        @ Bdest.bdname WITH FRAME lis.
                     PAUSE 0.
                  END.
               END.
               ELSE DO:
                  DISP Bdest.bdname WITH FRAME lis.
                  PAUSE 0.
               END.
            END.
            ELSE DISP "" @ Bdest.bdname WITH FRAME lis.
         END.

         ELSE IF FRAME-FIELD = "slgatype" THEN DO:
            FIND FIRST TMSCodes WHERE 
                       TMSCodes.Tablename    = "SLGAanalyse"   AND 
                       TMSCodes.FieldName    = "SLGAType"      AND 
                       TMSCodes.CodeGroup    = "Servicelimit"  AND 
                       TMSCodes.CodeValue    = INPUT slgaType 
            NO-LOCK NO-ERROR.
            IF NOT AVAIL TMSCodes THEN DO:
               BELL.
               MESSAGE 
               "Unknown type" .
               NEXT-prompt SLGAnalyse.slgatype.
               NEXT.
            END.
            DISP  TMSCodes.codename @ lctypename  WITH FRAME lis. PAUSE 0.
         END.
         
         ELSE IF FRAME-FIELD = "ServiceLimitGroup" THEN DO:
            
            IF INPUT SLGANalyse.slgatype = 1 THEN DO:
               FIND FIRST ServiceLimitGroup WHERE 
                          ServiceLimitGroup.Brand = gcBrand ANd
                          ServiceLimitGroup.GroupCode  = 
                          input frame lis SLGAnalyse.ServiceLimitGroup
               NO-LOCK NO-ERROR.
               IF NOT AVAIL ServiceLimitGroup THEN DO:
                  BELL.
                  MESSAGE 
                  "Unknown ServiceLimit Group" .
                  NEXT-prompt SLGAnalyse.serviceLimitGroup.
                  NEXT.
               END.
            END.
            ELSE IF  INPUT SLGANalyse.slgatype = 2 THEN DO:
               FIND FIRST DayCampaign WHERE
                          DayCampaign.Brand = gcBrand AND
                          DayCampaign.DCEvent = 
                          input frame lis SLGAnalyse.ServiceLimitGroup 
               NO-LOCK NO-ERROR.
               IF NOT AVAIL DayCampaign THEN DO:
                  BELL.
                  MESSAGE
                  "Unknown Periodical contract" .
                  NEXT-prompt SLGAnalyse.serviceLimitGroup.
                  NEXT.
               END.
            END.
         END.

         ELSE IF FRAME-FIELD = "validfrom" THEN DO:
            if input frame lis SLGAnalyse.ValidFrom  = ? THEN DO:
               BELL.
               MESSAGE 
               "from Date can't be empty!".
               NEXT-prompt SLGAnalyse.ValidFrom.
               NEXT.
            END.
         END.
         
         ELSE IF FRAME-FIELD = "validTo" THEN DO:
            if input frame lis SLGAnalyse.ValidTo  = ? THEN DO:
               BELL.
               MESSAGE 
               "to Date can't be empty!".
               NEXT-prompt SLGAnalyse.ValidTo.
               NEXT.
            END.
         END.

      END.
      
      APPLY LASTKEY. 
   
   END.
   HIDE FRAME lis.
   
END.


PROCEDURE LOCAL-GENERATE-RECORD. 

   PAUSE 0.
   ASSIGN 
      ldtValidF = Today
      ldtValidT = 12/31/2050
      liPrior   = 20
      lcTypeName = "".
      
   DISP 
      lcCliType 
      lcBillCode
      lcccn
      lidestType
      lcSLGAType 
      lcTypeName
      ldtValidF  
      ldtValidT
      lcbdest
      liPrior
      lcServiceL
   WITH FRAME Generate.

   UPDATE 
      lcCliType 
      lcBillCode 
      lcCCN
      lidestType
      ldtValidF
      ldtValidT
      lcbdest
      liPrior
      lcSLGAType
      lcServiceL
   WITH FRAME generate EDITING: 
      
      READKEY. 
      
      IF keylabel(LASTKEY) = "F9" AND
         FRAME-FIELD = "lcSLGAType"  THEN DO:
 
         RUN h-tmscodes(INPUT "SLGAanalyse",    /* TableName */
                              "SLGAType",       /* FieldName */
                              "ServiceLimit",   /* GroupCode */
                              OUTPUT lcCode).
                               
         IF lcCode ne "" AND lcCode NE ? THEN DO:
            DISPLAY INTEGER(lcCode) @ lcSLGAType
            WITH FRAME generate.
         END.
         ehto = 9.
         RUN ufkey.
         NEXT.
      END.
      ELSE IF keylabel(LASTKEY) = "F9" AND
          FRAME-FIELD = "lcServiceL"  THEN DO:

         IF      INPUT lcSLGAType = "1" THEN run h-servlimitgrp.
         ELSE IF INPUT lcSLGAType = "2" THEN run  h-daycamp.p.

         ASSIGN lcServiceL = siirto.
         disp lcServiceL WITH FRAME Generate.
      END.   
      ELSE IF keylabel(LASTKEY) = "F9" AND
         FRAME-FIELD = "lidesttype"  THEN DO:

         RUN h-tmscodes(INPUT "SLGAnalyse", /* TableName */
                              "bdest",       /* FieldName */
                              "bdest",       /* GroupCode */
                              OUTPUT lcCode).
                               
         IF lcCode ne "" AND lcCode NE ? THEN DO:
            DISPLAY INTEGER(lcCode) @ lidesttype
            WITH FRAME generate.
         END.
         ehto = 9.
         RUN ufkey.
         NEXT.
      END.
      ELSE IF keylabel(LASTKEY) = "F9" AND
         FRAME-FIELD = "lcbdest"       AND 
         INPUT lidesttype  = 2 THEN DO:
 
         RUN h-tmscodes(INPUT "Bdest", /* TableName */
                              "bDestClass",       /* FieldName */
                              "analysis",       /* GroupCode */
                              OUTPUT lcCode).

         IF lcCode ne "" AND lcCode NE ? THEN DO:
            DISPLAY lcCode @ lcbdest
            WITH FRAME generate.
         END.
         ehto = 9.
         RUN ufkey.
         NEXT.
      END.
      
      nap = KEYLABEL(LASTKEY). 
      IF lookup(nap,poisnap) > 0 THEN DO:
         if keylabel(lastkey) = "F4" THEN LEAVE . 

         IF FRAME-FIELD = "lcCliType" THEN DO:
            FIND FIRST CliType WHERE 
                       Clitype.Brand   = gcBrand AND
                       CliType.CliType = input frame generate lcCliType
            NO-LOCK NO-ERROR.
            IF NOT AVAIL CliType THEN DO:
               BELL.
               MESSAGE 
               "Unknown Clitype " lcclitype .
               NEXT-prompt lcCliType.
               NEXT.
            END.
            DISP CliType.CliName WITH FRAME generate. PAUSE 0.
         END.

         ELSE IF FRAME-FIELD = "lcBillCode" THEN DO:
            FIND FIRST BillItem WHERE 
                       BillItem.Brand    = gcBrand AND
                       BillItem.BillCode = input frame generate lcBillCode
            NO-LOCK NO-ERROR.
            IF NOT AVAIL BillItem AND 
               INPUT lcbillCode ne "*" THEN DO:
               BELL.
               MESSAGE 
               "Unknown Billing Item" .
               NEXT-prompt lcBillCode.
               NEXT.
            END.
            IF AVAIL BillItem then 
               DISP BillItem.BIName WITH FRAME generate. 
            ELSE  DISP "ALL" @ BillItem.BIName WITH FRAME generate.   
               PAUSE 0.
         END.
         
         ELSE IF FRAME-FIELD = "lcCCN" THEN DO:
            
            IF INT(INPUT lcCCN) > 0  THEN 
            FIND FIRST CCN WHERE 
                       CCN.Brand    = gcBrand AND
                       CCN.CCN = INT(input frame generate lcCCN)
            NO-LOCK NO-ERROR.
            IF NOT AVAIL CCN AND 
               INPUT lcCCN ne "*" THEN DO:
               BELL.
               MESSAGE 
               "Unknown CCN Code" .
               NEXT-prompt lcCCN.
               NEXT.
            END.
            IF AVAIL CCN then 
               DISP CCN.CCNName WITH FRAME generate. 
            ELSE  DISP "ALL" @ CCN.CCNName WITH FRAME generate.   
              PAUSE 0.
         END.

         ELSE IF FRAME-FIELD = "lcBDest" THEN DO:
         
            IF  INPUT FRAME generate liDesttype = 1 THEN DO:
               FIND FIRST Bdest WHERE 
                  Bdest.Brand = gcBrand AND
                  Bdest.Bdest = input frame generate lcBdest AND
                  BDest.ToDate >= INPUT FRAME generate ldtValidF AND
                  BDest.FromDate <= INPUT FRAME generate ldtValidT
               NO-LOCK NO-ERROR.
            
               IF NOT AVAIL BDest AND 
                  INPUT lcbdest ne "*" THEN DO:
                  BELL.
                  MESSAGE 
                  "Unknown b-destination" .
                  NEXT-prompt lcBdest.
                  NEXT.
               END.
      
               IF AVAIL Bdest then 
                  DISP Bdest.bdName WITH FRAME generate. 
               ELSE  DISP "ALL" @ Bdest.bdname WITH FRAME generate.   
               PAUSE 0.
            END.
            ELSE IF INPUT FRAME generate liDesttype = 2 THEN DO:
               
               FIND FIRST TMSCodes WHERE
                          TMSCodes.Tablename    = "bDEst"   AND
                          TMSCodes.FieldName    = "bDestClass"      AND
                          TMSCodes.CodeGroup    = "analysis"  AND
                          TMSCodes.CodeValue    =  
                       INPUT FRAME generate  lcbdest 
               NO-LOCK NO-ERROR.
                                   
               IF NOT AVAIL TMSCodes AND 
               INPUT lcbdest ne "*" THEN DO:
                  BELL.
                  MESSAGE 
                  "Unknown b-type" .
                  NEXT-prompt lcbdest.
                  NEXT.
               END.
      
               DISP tmscodes.codename  @ Bdest.bdname WITH FRAME generate.   
               PAUSE 0.
            END.
         END.
       
         ELSE IF FRAME-FIELD = "lcslgatype" THEN DO:
            FIND FIRST TMSCodes WHERE 
                       TMSCodes.Tablename    = "SLGAanalyse"   AND 
                       TMSCodes.FieldName    = "SLGAType"      AND 
                       TMSCodes.CodeGroup    = "Servicelimit"  AND 
                       TMSCodes.CodeValue    = INPUT FRAME generate lcslgaType 
            NO-LOCK NO-ERROR.
            
            IF NOT AVAIL TMSCodes THEN DO:
               BELL.
               MESSAGE 
               "Unknown type" .
               NEXT-prompt lcslgatype.
               NEXT.
            END.
            DISP  TMSCodes.codename @ lctypename  WITH FRAME Generate. PAUSE 0.
         END.
       
         ELSE IF FRAME-FIELD = "lidesttype" THEN DO:
            FIND FIRST TMSCodes WHERE 
                       TMSCodes.Tablename    = "SLGAnalyse"   AND 
                       TMSCodes.FieldName    = "bdest"      AND 
                       TMSCodes.CodeGroup    = "bdest"  AND 
                       TMSCodes.CodeValue    = 
                          STRING(INPUT FRAME generate lidestType)
            NO-LOCK NO-ERROR.
            
            IF NOT AVAIL TMSCodes THEN DO:
               BELL.
               MESSAGE 
               "Unknown B-destination type" .
               NEXT-prompt lidestType.
               NEXT.
            END.
         END.
         
         ELSE IF FRAME-FIELD = "ServiceL" THEN DO:
            
            IF INPUT lcslgatype = "1" THEN DO:
               FIND FIRST ServiceLimitGroup WHERE 
                          ServiceLimitGroup.GroupCode  = 
                          input frame Generate lcServiceL
               NO-LOCK NO-ERROR.
               IF NOT AVAIL ServiceLimitGroup THEN DO:
                  BELL.
                  MESSAGE 
                  "Unknown ServiceLimit Group" .
                  NEXT-prompt lcserviceL.
                  NEXT.
               END.
            END.
            ELSE IF  INPUT lcslgatype = "2" THEN DO:
               
               FIND FIRST DayCampaign WHERE
                          DayCampaign.DCEvent = 
                          input frame Generate lcServiceL 
               NO-LOCK NO-ERROR.
               IF NOT AVAIL DayCampaign THEN DO:
                  BELL.
                  MESSAGE
                  "Unknown Periodical contract" .
                  NEXT-prompt lcserviceL.
                  NEXT.
               END.
            END.
         END.

         ELSE IF FRAME-FIELD = "validfrom" THEN DO:
            if input frame lis SLGAnalyse.ValidFrom  = ? THEN DO:
               BELL.
               MESSAGE 
               "from Date can't be empty!".
               NEXT-prompt SLGAnalyse.ValidFrom.
               NEXT.
            END.
         END.
         
         ELSE IF FRAME-FIELD = "validTo" THEN DO:
            if input frame lis SLGAnalyse.ValidTo  = ? THEN DO:
               BELL.
               MESSAGE 
               "from Date can't be empty!".
               NEXT-prompt SLGAnalyse.ValidTo.
               NEXT.
            END.
         END.

      END.
      APPLY LASTKEY. 
   END.

   FOR EACH Clitype NO-LOCK  WHERE 
            Clitype.Brand   = gcBrand AND 
            CliType.Clitype = lcClitype,
       EACH RatePlan NO-LOCK WHERE 
            RatePlan.Brand  = gcBrand AND 
            RatePlan.RatePlan = Clitype.Priceplan,
       EACH plistconf OF RatePlan,
       EACH Pricelist of PlistConf,
       EACH Tariff WHERE 
            Tariff.Brand      = gcBrand             AND 
            Tariff.PriceList  = PriceList.Pricelist AND
            Tariff.ValidFrom <= ldtValidT NO-LOCK.
      
      IF lcBillCode      NE "*" AND 
         Tariff.BillCode NE lcBillCode THEN NEXT.

      IF lcCCN      NE "*" AND
         Tariff.CCN NE INT(lcCCN)  THEN NEXT.
               
      IF lcBDest = "*"  THEN DO:
      
         FOR EACH Bdest WHERE 
                  BDest.Brand = gcBrand AND
                  BDest.ToDate >= ldtValidF AND
                  BDest.FromDate <= ldtValidT  NO-LOCK:
                  
            FIND FIRST ttSLG WHERE 
                       ttSLG.Brand     = gcBrand         AND 
                       ttSlg.Belongto  = TRUE            AND 
                       ttSlg.CliType   = lcClitype       AND
                       ttSLG.BillCode  = Tariff.BillCode AND 
                       ttSLG.CCN       = Tariff.CCN      AND 
                       ttSLG.Bdest     = Bdest.Bdest NO-LOCK NO-ERROR.
                 
            IF AVAIL ttSLG THEN NEXT.
            
            CREATE ttSLG.
            ASSIGN 
               ttSLG.Brand     = gcBrand 
               ttSlg.Belongto  = TRUE
               ttSlg.CliType   = lcClitype
               ttSLG.BillCode  = Tariff.BillCode
               ttSLG.CCN       = Tariff.CCN
               ttSLG.Bdest     = Bdest.Bdest
               ttSLG.VAlidFrom = ldtValidF
               ttSLG.ValidTo   = ldtValidT
               ttSLG.ServiceL  = lcServiceL
               ttSlg.SlgaType  = INT(lcSLGAType)
               ttSLG.Prior     = liPrior.
         END.
      END.
      ELSE IF liDesttype = 1 THEN DO:
         FIND FIRST ttSLG WHERE 
                    ttSLG.Brand     = gcBrand         AND 
                    ttSlg.Belongto  = TRUE            AND 
                    ttSlg.CliType   = lcClitype       AND
                    ttSLG.BillCode  = Tariff.BillCode AND 
                    ttSLG.CCN       = Tariff.CCN      AND 
                    ttSLG.Bdest     = lcBdest NO-LOCK NO-ERROR.
                 
         CREATE ttSLG.
         ASSIGN 
            ttSLG.Brand     = gcBrand 
            ttSlg.Belongto  = TRUE
            ttSlg.CliType   = lcClitype
            ttSLG.BillCode  = Tariff.BillCode
            ttSLG.CCN       = Tariff.CCN
            ttSLG.Bdest     = lcBdest
            ttSLG.VAlidFrom = ldtValidF
            ttSLG.ValidTo   = ldtValidT
            ttSLG.ServiceL  = lcServiceL
            ttSlg.SlgaType  = INT(lcSLGAType)
            ttSLG.Prior     = liPrior.
      END.
      
      ELSE IF liDestType = 2 THEN DO:   
         
         FOR EACH Bdest WHERE 
                  BDest.Brand = gcBrand  AND 
                  Bdest.DestType = INT(lcBdest) AND
                  BDest.ToDate >= ldtValidF AND
                  BDest.FromDate <= ldtValidT NO-LOCK:
                  
            FIND FIRST ttSLG WHERE 
                       ttSLG.Brand     = gcBrand         AND 
                       ttSlg.Belongto  = TRUE            AND 
                       ttSlg.CliType   = lcClitype       AND
                       ttSLG.BillCode  = Tariff.BillCode AND 
                       ttSLG.CCN       = Tariff.CCN      AND 
                       ttSLG.Bdest     = Bdest.Bdest NO-LOCK NO-ERROR.
                 
            IF AVAIL ttSLG THEN NEXT.
            
            CREATE ttSLG.
            ASSIGN 
               ttSLG.Brand     = gcBrand 
               ttSlg.Belongto  = TRUE
               ttSlg.CliType   = lcClitype
               ttSLG.BillCode  = Tariff.BillCode
               ttSLG.CCN       = Tariff.CCN
               ttSLG.Bdest     = Bdest.Bdest
               ttSLG.VAlidFrom = ldtValidF
               ttSLG.ValidTo   = ldtValidT
               ttSLG.ServiceL  = lcServiceL
               ttSlg.SlgaType  = INT(lcSLGAType)
               ttSLG.Prior     = liPrior.
         END.
      END.
   END. 

   
   DEF VAR liQty AS I NO-UNDO.
   
   FOR EACH ttSlg NO-LOCK.
     liQty = liQty + 1.
   END.

   MESSAGE 
    "New records " liQty
   view-as alert-box.
   
   run ttslganalyse(INPUT-OUTPUT TABLE ttslg).

   liQty = 0.
   
   FOR EACH ttSlg NO-LOCK.
       liQty = liQty + 1.
   END.

   message
     "Add " liqty " records to database?"  
   view-as alert-box BUTTONS YES-NO UPDATE llOK .
              
   IF llok THEN DO TRANS:
      FOR EACH ttSLG.
         CREATE SLGAnalyse.
         BUFFER-COPY ttslg to SLGANalyse.

      END.
   END.

   HIDE FRAME lis.
   LEAVE.
END.



