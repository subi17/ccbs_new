/* -----------------------------------------------
  MODULE .......: pnplist
  FUNCTION .....: Maintain rating pnplist numbers
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 25-02-01
  MODIFIED .....: 05.02.03 jp update pnpgroup 
                           if pnplist begins earlier than 
                  01.03.03 jp modify eventlog         
                  pnpgroup
  VERSION ......: SCRUNKO3, (23.10.96)
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhpnplist AS HANDLE NO-UNDO.
   lhPnplist = BUFFER pnplist:HANDLE.
   RUN StarEventInitialize(lhPnplist).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPnplist).
   END.

END.





DEF INPUT  PARAMETER  ipPnpSeq   AS INT  NO-UNDO .
DEF INPUT  PARAMETER  icpnpgroup AS CHAR NO-UNDO.

   FIND FIRST pnpgroup WHERE
              pnpgroup.pnpseq = ipPnpSeq
   NO-LOCK NO-ERROR.

DEF buffer xxPnpgroup FOR pnpgroup.

DEF VAR haku-pnplist LIKE pnplist.pnpSeq  NO-UNDO.

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
DEF VAR lFileName  AS CHAR                   NO-UNDO.
def var ok         as log format "Yes/No"    NO-UNDO.

form
   pnplist.cli
   pnplist.DialTypeUsed[1] column-label "DE"
   pnplist.DialTypeUsed[2] column-label "FV"
   pnplist.DialTypeUsed[3] column-label "MV"
   pnplist.DialTypeUsed[4] column-label "SMS"
   pnplist.DialTypeUsed[5] column-label "DA"
   pnplist.fromdate
   pnplist.toDate
WITH ROW 3 WIDTH 55 CENTERED OVERLAY scroll 1 12 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + pnpgroup.name
   FRAME sel.

form
   "CLI number:" pnplist.cli 
HELP "CLI number (a-number), CLI Series (045*) WHEN PNPgroup"  SKIP

   "Default...:" pnplist.DialTypeUsed[1]    FORMAT "X/-" SKIP
   "FixedVoice:" pnplist.DialTypeUsed[2]    FORMAT "X/-" SKIP
   "MobilVoice:" pnplist.DialTypeUsed[3]    FORMAT "X/-" SKIP
   "SMS.......:" pnplist.DialTypeUsed[4]    FORMAT "X/-" SKIP
   "DATA .....:" pnplist.DialTypeUsed[5]    FORMAT "X/-" SKIP
   "Valid time:" pnplist.fromdate pnplist.todate     
WITH OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header  WITH no-labels
   FRAME lis.

form /*  search WITH FIELD pnplist */
   haku-pnplist help "Give ...."
with row 4 col 2 title color value(ctc) " FIND xxxxxxx "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

FORM
   "Give filename :" lFileName  FORMAT "x(40)"
WITH
   ROW 8 OVERLAY CENTERED TITLE " Import CLI list from file " 
   NO-LABELS FRAME frmImport.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.
IF AVAILABLE pnplist THEN ASSIGN
   memory       = recid(pnplist)
   must-print = TRUE
   must-add    = FALSE.
ELSE ASSIGN
   memory       = ?
   must-print = FALSE
   must-add    = TRUE.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by 11111 ".
       if order = 2 then put screen row 19 col 30 " Order by 22222 ".
    END.

   IF must-add THEN DO:  /* pnplist -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD "
      
      must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.
        DO TRANSACTION:
           CLEAR frame lis.

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhpnplist).
           
           RUN LOCAL-UPDATE-RECORD(TRUE).
           
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhpnplist).
           
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
                   pnplist.cli = "" THEN
           UNDO add-new, LEAVE add-new.
              
              ASSIGN
              memory   = recid(pnplist)
              xrecid   = memory
              must-add = false.

        END.

      END.  /* add-new */

      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE pnplist THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND pnplist where recid(pnplist) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE pnplist THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(pnplist).
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
               must-print = FALSE.
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW pnplist.cli {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) pnplist.cli WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW pnplist.cli {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) pnplist.cli WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > ordercount THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = ordercount.
      END.

      IF order <> ex-order THEN DO:
        ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
        FIND pnplist where recid(pnplist) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        message "You are on a empty row, move upwards !".
        PAUSE 1 no-message.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           FIND pnplist where recid(pnplist) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE pnplist THEN DO:
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
              rtab[1] = recid(pnplist)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND pnplist where recid(pnplist) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE pnplist THEN DO:
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
              rtab[FRAME-DOWN] = recid(pnplist).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND pnplist where recid(pnplist) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE pnplist THEN DO:
           memory = recid(pnplist).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE pnplist THEN memory = recid(pnplist).
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
           FIND pnplist where recid(pnplist) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       haku-pnplist = 0.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       UPDATE haku-pnplist WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if haku-pnplist <> 0 THEN DO:
          FIND FIRST pnplist where pnplist.pnpSeq >= haku-pnplist
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE pnplist THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  pnplist/pnplist was found */
          ASSIGN order = 1 memory = recid(pnplist) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND pnplist where recid(pnplist) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          pnplist.cli 
          pnplist.DialTypeUsed[1]
          pnplist.DialTypeUsed[2] 
          pnplist.DialTypeUsed[3] 
          pnplist.DialTypeUsed[4] 
          pnplist.DialTypeUsed[5] 
          
          
          pnplist.fromdate
          pnplist.todate.


       RUN LOCAL-FIND-NEXT.
       
       IF AVAILABLE pnplist THEN memory = recid(pnplist).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND pnplist where recid(pnplist) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE pnplist THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(pnplist).
          END.
       END.
       
       /* 'find' back TO the ROW TO be deleted */
       FIND pnplist where recid(pnplist) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          pnplist.cli pnplist.FromDate pnplist.ToDate.
       IF ok THEN DO:
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhpnplist).
           DELETE pnplist.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST pnplist
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
     
     ELSE IF LOOKUP(nap,"7,F7") > 0 THEN DO:
        PAUSE 0.
        UPDATE 
           lFileName 
        WITH FRAME frmImport EDITING:
           READKEY.
           nap = KEYLABEL(LASTKEY).
           IF LOOKUP(nap,poisnap) > 0 THEN DO:
              ASSIGN lFileName.
              IF INPUT lFileName = "" THEN LEAVE.
              ELSE DO:

                 IF SEARCH(lFileName) = ? THEN DO:
                    MESSAGE
                       "File does not exist !"
                    VIEW-AS ALERT-BOX.
                    NEXT.
                 END.
                 ELSE DO:
                    INPUT FROM lFileName.
                    REPEAT:
                       IMPORT UNFORMATTED xls.
                       CREATE pnplist.
                       ASSIGN
                          pnplist.Brand    = gcBrand 
                          pnplist.pnpSeq   = ipPnpSeq
                          pnplist.cli      = TRIM(xls).
                    END.
                 END.
              END.
           END.
           APPLY LASTKEY.
        END.
        HIDE FRAME frmImport.
     END.
     
     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST pnplist where 
            recid(pnplist) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.

       cfc = "lis". RUN Syst/ufcolor.

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       xrecid = recid(pnplist).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(pnplist) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(pnplist) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   
   FIND FIRST pnpgroup WHERE
              pnpgroup.pnpseq = pnplist.pnpSeq 
   NO-LOCK NO-ERROR.
   
   DISPLAY
      pnplist.cli 
      pnplist.DialTypeUsed[1] 
      pnplist.DialTypeUsed[2] 
      pnplist.DialTypeUsed[3] 
      pnplist.DialTypeUsed[4] 
      pnplist.DialTypeUsed[5] 


      pnplist.FromDate
      pnplist.ToDate 
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 THEN 
      FIND NEXT pnplist WHERE
                pnplist.pnpSeq = ipPnpSeq and
                pnplist.cli   ne icpnpgroup
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 THEN 
      FIND PREV pnplist WHERE
                pnplist.pnpSeq = ipPnpSeq and
                pnplist.cli   ne icpnpgroup
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.
   IF order = 1 THEN 
      FIND FIRST pnplist WHERE
                 pnplist.pnpSeq = ipPnpSeq and
                 pnplist.cli   ne icpnpgroup
                 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 THEN 
      FIND LAST pnplist WHERE
                pnplist.pnpSeq = ipPnpSeq   AND                                            pnplist.cli   ne icpnpgroup
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.

   IF bNew THEN DO:
      CLEAR FRAME lis.
      FIND FIRST pnplist WHERE 
                 pnplist.pnpseq = ippnpseq AND 
                 pnplist.cli    = icpnpgroup  NO-ERROR.
      IF NOT AVAIL pnplist THEN DO:
         CREATE pnplist.
            pnplist.pnpSeq = ipPnpseq.
         ASSIGN
           pnplist.pnpSeq      = ipPnpseq       
           pnplist.cli         = ""
           pnplist.DialTypeUsed    = true
           pnplist.FromDate       = today
           pnplist.ToDate         = 12/31/2050.
      END.           
   END.
         
   FIND FIRST pnpgroup WHERE
              pnpgroup.pnpseq = ipPnpSeq
   NO-LOCK NO-ERROR.

         
   UPDATE 
      pnplist.cli 
      pnplist.DialTypeUsed[1] 
      pnplist.DialTypeUsed[2] 
      pnplist.DialTypeUsed[3] 
      pnplist.DialTypeUsed[4] 
      pnplist.DialTypeUsed[5] 
      
      pnplist.FromDate
      pnplist.ToDate
   WITH FRAME lis EDITING: 
      
      READKEY. 
      nap = KEYLABEL(LASTKEY). 
      IF lookup(nap,poisnap) > 0 THEN DO:
         if keylabel(lastkey) = "F4" THEN LEAVE . 


         IF FRAME-FIELD = "cli" THEN DO:
            /* Calculate length of cli, if wc */
            IF bnew AND input cli = "" then DO:
               leave.
            ENd.

            IF INDEX(input pnplist.cli,"*") > 0 AND 
            pnpgroup.pnpgroup begins "0" THEN DO:
               BELL.
               MESSAGE
               "Number series are not allowed for " + pnpgroup.pnpgroup
               VIEW-aS ALERT-BOX.
               NEXT-PROMPT pnplist.cli. NEXT.
            END.
         
         END. 
         ELSE IF FRAME-FIELD = "FromDate" THEN DO:
            if input frame lis pnplist.FromDate  = ? THEN DO:
               BELL.
               MESSAGE 
               "from Date can't be empty!".
               NEXT-prompt pnplist.FromDate.
               NEXT.
            END.

            IF pnpgroup.Dfrom > INPUT frame lis pnplist.FromDate THEN DO:
               MESSAGE
               "You have changed validity period for PNP B-number."  SKIP
               "Do you want to update PNP Group validity period accordingly?"
               VIEW-AS ALERT-BOX
               BUTTONS OK-CANCEL
               TITLE " Update validity period "
               SET ok.
            
               IF OK THEN DO:
                  FIND FIRST xxpnpgroup WHERE
                             RECID(xxpnpgroup) = RECID(pnpgroup)
                  EXCLUSIVE-LOCK.
                  ASSIGN
                  xxpnpgroup.DFrom = INPUT frame lis pnplist.FromDate.
               ENd.
            ENd.

         END.

         ELSE IF FRAME-FIELD = "ToDate" THEN DO:
            if input frame lis pnplist.ToDate  = ? THEN DO:
               BELL.
               MESSAGE 
               "To Date can't be empty!".
               NEXT-prompt pnplist.ToDate.
               NEXT.
            END.
         END.
      END.
      
      APPLY LASTKEY. 
   
   END.
END.
