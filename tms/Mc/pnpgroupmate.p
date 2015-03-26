/* -----------------------------------------------
  MODULE .......: pnpgroup
  FUNCTION .....: Maintain rating pnpgroup numbers
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 25-02-01
  MODIFIED .....: JP 12.02.04 brand etc.
  VERSION ......: M15
  ------------------------------------------------------ */

{testpaa.i}

DEF VAR haku-pnpgroup    LIKE pnpgroup.pnpgroup  NO-UNDO.
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
DEF VAR lcRateccnname  AS CHAR                   NO-UNDO FORMAT "X(30)" .

DEF BUFFER xxpnpgroup FOR pnpgroup.
DEF BUFFER xxrateccn  FOR ccn.
form
   pnpgroup.pnpgroup    
   pnpgroup.name     format "x(10)"
   pnpgroup.BillCode  format "x(8)"
      BillItem.BIName  format "x(10)"
   pnpgroup.ccn     
      CCN.CCNName  format "x(10)"
   pnpgroup.rateccn     column-label "RateCCN"
      
   pnpgroup.dto

WITH width 80 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
   " Maintain PNP groups "
   + string(pvm,"99-99-99") + " "
   FRAME sel.

form
   "Group code :" pnpgroup.pnpgroup              SKIP
   "Group name :" pnpgroup.name                  SKIP
   "BillItem ...:" pnpgroup.BillCode  
   BillItem.BIName                               SKIP
   "CCN .......:" pnpgroup.ccn 
   CCN.CCNName                                   SKIP
   "Rate CCN. .:" pnpgroup.RateCCN                 
   lcrateccnname                                 SKIP  
   "Valid From.:" pnpgroup.dfrom                 SKIP
   "Valid To...:" pnpgroup.dto      
WITH OVERLAY ROW 4 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header WITH no-labels
   FRAME lis.

form /*  search WITH FIELD pnpgroup */
    haku-pnpgroup
    help "Give ...."
    with row 4 col 2 title color value(ctc) " FIND xxxxxxx "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE pnpgroup THEN ASSIGN
   memory       = recid(pnpgroup)
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

   IF must-add THEN DO:  /* pnpgroup -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN ufkey.
        DO TRANSACTION:

           CREATE pnpgroup.
           ASSIGN
           pnpgroup.pnpseq = NEXT-VALUE(pnpseq)
           pnpgroup.Brand  = gcBrand 
           pnpgroup.pnpgrouptupe = 1.

           RUN LOCAL-UPDATE-RECORD(true).
           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
           pnpgroup.pnpgroup = "" THEN
           UNDO add-new, LEAVE add-new.
           ASSIGN
              memory = recid(pnpgroup)
              xrecid = memory
              must-add = false.
           LEAVE add-new.
        END.
      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE pnpgroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND pnpgroup where recid(pnpgroup) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE pnpgroup THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(pnpgroup).
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
        ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 1761
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 1762 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW pnpgroup.pnpgroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) pnpgroup.pnpgroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW pnpgroup.pnpgroup ;(uchoose.i;) no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) pnpgroup.pnpgroup WITH FRAME sel.
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
        FIND pnpgroup where recid(pnpgroup) = memory.
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
           FIND pnpgroup where recid(pnpgroup) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE pnpgroup THEN DO:
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
              rtab[1] = recid(pnpgroup)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND pnpgroup where recid(pnpgroup) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE pnpgroup THEN DO:
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
              rtab[FRAME-DOWN] = recid(pnpgroup).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND pnpgroup where recid(pnpgroup) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE pnpgroup THEN DO:
           memory = recid(pnpgroup).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE pnpgroup THEN memory = recid(pnpgroup).
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
           FIND pnpgroup where recid(pnpgroup) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN ufcolor.
       haku-pnpgroup = "".
       ehto = 9. RUN ufkey. ufkey = TRUE.
       UPDATE haku-pnpgroup WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if haku-pnpgroup <> "" THEN DO:
          FIND FIRST pnpgroup where 
                     Pnpgroup.Brand     = gcBrand AND 
                     pnpgroup.pnpgroup >= haku-pnpgroup
          /* search condition */ no-lock no-error.
          IF NOT AVAILABLE pnpgroup THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  pnpgroup/pnpgroup was found */
          ASSIGN order = 1 memory = recid(pnpgroup) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     if lookup(nap,"5,f5") > 0 THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND pnpgroup where recid(pnpgroup) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          pnpgroup.pnpgroup 
          pnpgroup.pnpgroup.

       RUN LOCAL-FIND-NEXT.
       
       IF AVAILABLE pnpgroup THEN memory = recid(pnpgroup).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND pnpgroup where recid(pnpgroup) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE pnpgroup THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(pnpgroup).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND pnpgroup where recid(pnpgroup) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc)
          pnpgroup.pnpgroup 
          pnpgroup.pnpgroup.
       IF ok THEN DO:

           DELETE pnpgroup.

           /* in the LAST record was deleted ? */
           IF NOT can-find(FIRST pnpgroup WHERE pnpgroup.Brand = gcBrand 
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

     ELSE IF LOOKUP(nap,"4,F4") > 0 THEN DO:
       FIND FIRST pnpgroup WHERE 
            RECID(pnpgroup) = rtab[FRAME-LINE] 
       NO-LOCK NO-ERROR.

       RUN pnplist.p(INPUT pnpgroup.pnpSeq, INPUT pnpgroup.pnpgroup).
       ufkey = true.
       run ufkey.
       PAUSE 0.
     END.

     else if lookup(nap,"7,f7") > 0 THEN DO:  /* hinnasto */

        FIND FIRST pnpgroup WHERE 
             RECID(pnpgroup) = rtab[FRAME-LINE] 
        NO-LOCK NO-ERROR.
           ufkey = TRUE.

        RUN nntuhi(0,pnpgroup.RateCCN).
     END.
     
     else if lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANSACTION:
       /* change */
       FIND FIRST pnpgroup where 
            recid(pnpgroup) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN ufkey.

       cfc = "lis". RUN ufcolor.

       RUN LOCAL-UPDATE-RECORD(FALSE).
       
       xrecid = recid(pnpgroup).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(pnpgroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(pnpgroup) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 
   
   FIND FIRST BillItem WHERE
              BillItem.Brand    = gcBrand AND 
              BillItem.BillCode = pnpgroup.BillCode
   NO-LOCK NO-ERROR.
 
   FIND FIRST CCN WHERE
              CCN.Brand = gcBRand AND 
              CCN.CCN = pnpgroup.ccn
   NO-LOCK NO-ERROR.
  
   DISPLAY

      pnpgroup.pnpgroup  
      pnpgroup.name
      pnpgroup.BillCode BillItem.BIName when AVAIL BillItem 
      pnpgroup.ccn     CCN.CCNName
      pnpgroup.RateCCN   
      pnpgroup.dto
   WITH FRAME sel.
   
END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 THEN 
      FIND NEXT pnpgroup WHERE pnpgroup.Brand = gcBRand 
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 THEN 
      FIND PREV pnpgroup WHERE pnpgroup.Brand = gcBRand
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF order = 1 THEN 
      FIND FIRST pnpgroup WHERE pnpgroup.Brand = gcBRand
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 THEN 
      FIND LAST pnpgroup WHERE pnpgroup.Brand = gcBRand
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.
   
   IF NOT bNew THEN DO:

      FIND FIRST BillItem WHERE
                 BillItem.Brand    = gcBrand   AND 
                 BillItem.BillCode = pnpgroup.BillCode
      NO-LOCK NO-ERROR.
 
      FIND FIRST CCN WHERE
                 CCN.Brand = gcBrand AND 
                 CCN.CCN   = pnpgroup.ccn
      NO-LOCK NO-ERROR.


      FIND FIRST xxRateccn WHERE 
                 xxRateccn.Brand = gcBrand AND 
                 xxRateccn.ccn   = pnpgroup.rateccn 
      no-lock no-error.

      IF AVAIL xxrateccn THEN 
           lcrateccnname = xxrateccn.ccnname.
      ELSE lcrateccnname = "".
 


      DISP 
         pnpgroup.pnpgroup 
         BillItem.BIName   WHEN AVAIL BillItem
         lcrateccnname 
         CCN.CCNName   WHEN AVAIL CCN
      WITH FRAME lis.
         
   END.
   
   UPDATE 
      pnpgroup.pnpgroup WHEN bNew
      pnpgroup.name 

      pnpgroup.BillCode 
      pnpgroup.ccn 
      pnpgroup.RateCCN
      pnpgroup.dfrom
      pnpgroup.dto
   WITH FRAME lis EDITING: 
      
      READKEY. 
      nap = KEYLABEL(LASTKEY). 
      IF lookup(nap,poisnap) > 0 THEN DO:
         if keylabel(lastkey) = "F4" THEN LEAVE . 


         IF FRAME-FIELD = "pnpgroup" THEN DO:
            if input frame lis pnpgroup.pnpgroup  = "" THEN DO:
               LEAVE .
            END.
         END.
         
         ELSE IF FRAME-FIELD = "RateCCN" THEN DO:
            if input frame lis pnpgroup.RateCCN = "" OR 
            NOT CAN-FIND(FIRST Rateccn WHERE 
                               Rateccn.Brand = gcBrand AND 
                               Rateccn.ccn = INPUT FRAME lis PnpGroup.rateccn) 
            THEN DO:
               BELL.
               MESSAGE "Unknown Rateccn!".
               NEXT-PROMPT pnpgroup.RateCCN.
               NEXT.
            END.
            FIND FIRST xxRateccn WHERE 
                       xxRateccn.Brand = gcBrand and 
                    xxRateccn.ccn = INPUT FRAME lis PnpGroup.rateccn no-lock.
        
            DISP xxrateccn.ccnname @ lcrateccnname with frame lis.
         END.
         
         ELSE IF FRAME-FIELD = "dfrom" THEN DO:
            if input frame lis pnpgroup.dfrom  = ? THEN DO:
               BELL.
               MESSAGE 
               "from Date can't be empty!".
               NEXT-prompt dfrom.
               NEXT.
            END.
         END.

         ELSE IF FRAME-FIELD = "dto" THEN DO:
            if input frame lis pnpgroup.dto  = ? THEN DO:
               BELL.
               MESSAGE 
               "To Date can't be empty!".
               NEXT-prompt dto.
               NEXT.
            END.
            
            IF CAN-FIND(FIRST xxpnpgroup WHERE
                              xxPnpGroup.Brand    = gcBrand                 AND                               xxpnpgroup.pnpgroup = input pnpgroup.pnpgroup AND
                              xxpnpgroup.dto      = input pnpgroup.dto      AND
                              RECID(xxpnpgroup)   ne RECID(pnpgroup))
            THEN DO:
               BELL.
               MESSAGE "PNP Group allready exist!".
               NEXT-PROMPT pnpgroup.dto.
               NEXT.               
            END.                  
         END.
         
      END.
      
      APPLY LASTKEY. 
   
   END.
   HIDE FRAME lis.
   
END.
