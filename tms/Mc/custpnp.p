/* -----------------------------------------------
  MODULE .......: CUSTPNP
  FUNCTION .....: Maintain Customers PNPList numbers
  APPLICATION ..: TMS
  AUTHOR .......: KL
  CREATED ......: 21.02.2003
  MODIFIED .....: 02.03.03 tk tokens
                  24.03.03/aam format x(13) for BDestFrom & BDestTo
                  27.03.03 kl find pnpgroup automatically
                  27.03.03 tk don't display plname and pnpgroup name when bNew
                              fixed checking if last one was deleted 
                  17.09.03 jp Brand             
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/excel.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'pnplist'}

DEF input PARAMETer ipCustNum AS INT NO-UNDO .

FIND FIRST Customer WHERE
           Customer.CustNum = ipCustNum
NO-LOCK NO-ERROR.

DEF VAR haku-PNPList LIKE PNPList.pnpSeq  NO-UNDO.

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
DEF VAR ok         AS LOG FORMAT "Yes/No"    NO-UNDO.
DEF VAR liMaxLen   AS INT                    NO-UNDO.
DEF VAR liLoop     AS INT                    NO-UNDO.

DEF BUFFER bList FOR PNPList.

FOR EACH PNPGroup NO-LOCK:
   liMaxLen = MAX(liMaxLen,LENGTH(PNPGroup.BDest)).
END.

FORM
   PNPList.PNPGroup
   PNPGroup.Name
   PNPList.PriceList
   PNPList.CLI
   PNPList.BDestFrom FORMAT "X(13)"
   PNPList.BDestTo   FORMAT "X(13)" 
WITH ROW 3 CENTERED OVERLAY scroll 1 12 DOWN
   COLOR value(cfc)
   title color value(ctc) " PNP numbers: " + STRING(Customer.CustNum) + " - " +
   Customer.CustName + " "
FRAME sel.

FORM
   "BNumberFrom:" PNPList.BDestFrom 
      FORMAT "X(13)"
      HELP "FROM B-number for PNP series" SKIP
   "BNumberTo .:" PNPList.BDestTo
      FORMAT "X(13)" 
      HELP "TO B-number for PNP series"   SKIP
   "PNP Group .:" PNPList.PNPGroup
      HELP "PNP Group code"
      PNPGroup.Name                       SKIP
   "Price List :" PNPList.PriceList
      PriceList.PLName                    SKIP
   "CLI .......:" PNPList.CLI             SKIP
WITH OVERLAY ROW 6 centered
   COLOR value(cfc)
   TITLE COLOR value(ctc)
   fr-header  WITH no-labels
FRAME lis.

form /*  search WITH FIELD PNPList */
   haku-PNPList help "Give ...."
with row 4 col 2 title color value(ctc) " FIND xxxxxxx "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

FORM
   "Give filename :" lFileName  FORMAT "x(40)"
WITH
   ROW 8 OVERLAY CENTERED TITLE " Import CLI list from file " 
   NO-LABELS FRAME frmImport.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

RUN LOCAL-FIND-FIRST.

IF AVAILABLE PNPList THEN ASSIGN
   memory       = recid(PNPList)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No PNPList records available !" VIEW-AS ALERT-BOX.
      HIDE FRAME sel NO-PAUSE.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " Order by PNP group  ".
       if order = 2 then put screen row 19 col 30 " Order by FROM dest. ".
    END.

   IF must-add THEN DO:  /* PNPList -ADD  */
      HIDE FRAME lis.
      assign cfc = "lis" ufkey = true fr-header = " ADD "

      must-add = FALSE.
      RUN Syst/ufcolor.p.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
        PAUSE 0 no-message.
        CLEAR FRAME lis no-pause.
        ehto = 9. RUN Syst/ufkey.p.
        DO TRANSACTION: 

           RUN LOCAL-UPDATE-RECORD(TRUE).

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
                   PNPList.BDestFrom = "" THEN
           UNDO add-new, LEAVE add-new.

              ASSIGN
              memory   = recid(PNPList)
              xrecid   = memory
              must-add = false.

        END. 

      END.  /* add-new */

      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE PNPList THEN LEAVE LOOP.
      NEXT LOOP.

   END.

print-line:
   DO :
      IF must-print THEN DO:
        up FRAME-LINE - 1.
        FIND PNPList where recid(PNPList) = memory no-lock no-error.

        /* print 1 page data on the screen
        beginning from the record whose keyvalue = memory
        beginning from line 'delline' */

        /* IF a line has just been deleted, THEN ... */
        IF delline > 0 THEN DOWN delline - 1.

        repeat WITH FRAME sel:
           IF AVAILABLE PNPList THEN DO:
              RUN LOCAL-DISP-ROW.
              rtab[FRAME-LINE] = recid(PNPList).
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
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
        CHOOSE ROW PNPList.PNPGroup {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) PNPList.PNPGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW PNPList.BDestFrom {Syst/uchoose.i} no-error WITH FRAME sel.
        COLOR DISPLAY value(ccc) PNPList.BDestFrom WITH FRAME sel.
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
        ASSIGN
           firstline = 0
           memory    = rtab[FRAME-LINE].
        FIND PNPList where recid(PNPList) = memory.
        DO i = 1 TO FRAME-LINE - 1:
           RUN LOCAL-FIND-PREV.
           IF AVAILABLE PNPList THEN ASSIGN
              firstline = i
              memory    = recid(PNPList).
           ELSE LEAVE.
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
           FIND PNPList where recid(PNPList) = rtab[1] no-lock.
           RUN LOCAL-FIND-PREV.
           IF NOT AVAILABLE PNPList THEN DO:
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
              rtab[1] = recid(PNPList)
              memory = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           FIND PNPList where recid(PNPList) = rtab[FRAME-DOWN] no-lock .
           RUN LOCAL-FIND-NEXT.
           IF NOT AVAILABLE PNPList THEN DO:
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
              rtab[FRAME-DOWN] = recid(PNPList).
              /* finally LAST line's keyvalue is saved */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND PNPList where recid(PNPList) = memory no-lock no-error.
        RUN LOCAL-FIND-PREV.
        IF AVAILABLE PNPList THEN DO:
           memory = recid(PNPList).

           /* go back one page */
           DO line = 1 TO (FRAME-DOWN - 1):
              RUN LOCAL-FIND-PREV.
              IF AVAILABLE PNPList THEN memory = recid(PNPList).
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
           FIND PNPList where recid(PNPList) = memory no-lock.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       haku-PNPList = 0.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       UPDATE haku-PNPList WITH FRAME haku-f1.
       HIDE FRAME haku-f1 no-pause.
       if haku-PNPList <> 0 THEN DO:
          FIND FIRST PNPList where 
                     PNPList.pnpSeq >= haku-PNPList
         AND PnpList.Brand = gcBrand no-lock no-error.
          IF NOT AVAILABLE PNPList THEN DO:
             BELL.
             message "NONE FOUND !".
             PAUSE 1 no-message.
             NEXT BROWSE.
          END.
          /*  PNPList/PNPList was found */
          ASSIGN order = 1 memory = recid(PNPList) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Haku sar. 1 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
        must-add = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* removal */
       delline = FRAME-LINE.
       FIND PNPList where recid(PNPList) = rtab[FRAME-LINE] no-lock.

       /* line TO be deleted is lightened */
       COLOR DISPLAY value(ctc)
          PNPList.BDestFrom 
       WITH FRAME sel.

       RUN LOCAL-FIND-NEXT.

       IF AVAILABLE PNPList THEN memory = recid(PNPList).
       ELSE DO:
          /* the one TO be deleted is rereaden */
          FIND PNPList where recid(PNPList) = rtab[FRAME-LINE] no-lock.
          /* AND THEN the previous one */
          RUN LOCAL-FIND-PREV.
          IF AVAILABLE PNPList THEN DO:
             ASSIGN
             delline = delline - 1  /* cause the LAST one is TO be deleted */
             memory = recid(PNPList).
          END.
       END.

       /* 'find' back TO the ROW TO be deleted */
       FIND PNPList where recid(PNPList) = rtab[FRAME-LINE]
       exclusive-lock.

       ASSIGN ok = FALSE.
       message "ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY value(ccc) PNPList.BDestFrom WITH FRAME sel.
       IF ok THEN DO:

           DELETE PNPList.

           /* in the LAST record was deleted ? */

           IF NOT CAN-FIND(FIRST PNPList WHERE
                                 PNPList.CustNum = ipCustNum) 
           THEN DO:
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
                       CREATE PNPList.
                       ASSIGN
                          PNPList.Brand  = gcBrand 
                          PNPList.pnpSeq = ipCustNum
                          PNPList.BDestFrom  = TRIM(xls).
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
       FIND FIRST PNPList where 
            recid(PNPList) = rtab[frame-line(sel)]
       exclusive-lock.
       assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
       RUN Syst/ufkey.p.

       cfc = "lis". RUN Syst/ufcolor.p.

       RUN LOCAL-UPDATE-RECORD(FALSE).

       xrecid = recid(PNPList).

     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
       RUN LOCAL-FIND-FIRST.
       ASSIGN memory = recid(PNPList) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
       RUN LOCAL-FIND-LAST.
       ASSIGN memory = recid(PNPList) must-print = TRUE.
       NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE LOCAL-DISP-ROW: 

   FIND FIRST pnpgroup WHERE
              pnpgroup.pnpseq = PNPList.pnpSeq
   NO-LOCK NO-ERROR.

   DISPLAY
      PNPList.PNPGroup
      PNPGroup.Name
      PNPList.PriceList
      PNPList.CLI 
      PNPList.BDestFrom
      PNPList.BDestTo
   WITH FRAME sel.

END PROCEDURE.

PROCEDURE LOCAL-FIND-NEXT.

   IF order = 1 THEN 
      FIND NEXT PNPList USE-INDEX CustNum WHERE
                PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
      FIND NEXT PNPList USE-INDEX PNPSeq WHERE
                PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-PREV.

   IF order = 1 THEN 
      FIND PREV PNPList USE-INDEX CustNum WHERE
                PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
      FIND PREV PNPList USE-INDEX PNPSeq WHERE
                PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-FIRST.

   IF order = 1 THEN 
      FIND FIRST PNPList USE-INDEX CustNum WHERE
                 PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
      FIND FIRST PNPList USE-INDEX PNPSeq WHERE
                 PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-FIND-LAST.

   IF order = 1 THEN 
      FIND LAST PNPList USE-INDEX CustNum WHERE
                PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN
      FIND LAST PNPList USE-INDEX PNPSeq WHERE
                PNPList.CustNum = ipCustNum
      NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE LOCAL-UPDATE-RECORD. 

   DEF INPUT PARAMETE bNew AS LO NO-UNDO.

   IF bNew THEN DO:
      CREATE PNPList.
      PNPList.CustNum = ipCustNum.
      PNPList.Brand   = gcBrand.
   END.
   ELSE DO:
      FIND FIRST PNPGroup WHERE
                 PNPGroup.Brand    = gcBrand AND 
                 PNPGroup.PNPGroup = PNPList.PNPGroup
      NO-LOCK NO-ERROR.
      FIND FIRST PriceList WHERE
                 PriceList.PriceList = PNPList.PriceList
      NO-LOCK NO-ERROR.
   END.

   DISPLAY
      PNPList.PNPGroup
      PNPGroup.Name     WHEN AVAIL PNPGroup AND NOT bNew
      PNPList.PriceList
      PriceList.PLName  WHEN AVAIL PriceList AND NOT bNew
      PNPList.CLI
      PNPList.BDestFrom
      PNPList.BDestTo
   WITH FRAME lis.

   IF lcRight = "RW" THEN DO:      
      UPDATE 
         PNPList.BDestFrom 
         PNPList.BDestTo
         PNPList.PNPGroup
         PNPList.PriceList
         PNPList.CLI
      WITH FRAME lis EDITING: 

         READKEY. 
         nap = KEYLABEL(LASTKEY). 
         IF lookup(nap,poisnap) > 0 THEN DO:
            if keylabel(lastkey) = "F4" THEN LEAVE . 

            IF FRAME-FIELD = "BDestFrom" THEN DO:

               ASSIGN PNPList.BDestFrom.
               IF bnew AND INPUT PNPList.BDestFrom = "" THEN LEAVE.

               DEC(PNPList.BDestFrom) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE
                     "Field contains invalid characters !"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.

               FIND FIRST bList WHERE
                          bList.Brand     = PNPList.Brand           AND
                          bList.CustNum   = PNPList.CustNum         AND 
                          bList.BDestFrom = INPUT PNPList.BDestFrom AND
                          RECID(bList) NE RECID(PNPList)
               NO-LOCK NO-ERROR.
               IF AVAILABLE bList THEN DO:
                  BELL.
                  MESSAGE "PNP number has already been defined for customer"
                          bList.CustNum.
                  NEXT.                          
               END.
  
               HIDE MESSAGE NO-PAUSE.  
               
               IF bnew THEN DO:
                  PNPList.BDestTo = PNPList.BDestFrom.
                  DISP PNPList.BDestTo WITH FRAME lis.
               END.

            END. 

            IF FRAME-FIELD = "BDestTo" THEN DO:

               ASSIGN PNPList.BDestTo.
               IF bnew AND INPUT PNPList.BDestTo = "" THEN LEAVE.

               DEC(PNPList.BDestTo) NO-ERROR.
               IF ERROR-STATUS:ERROR THEN DO:
                  MESSAGE
                     "Field contains invalid characters !"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               IF DEC(PNPList.BDestFrom) > DEC(PNPList.BDestTo) THEN DO:
                  MESSAGE
                     "Series are in wrong order !"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               PNPList.PNPGroup = DYNAMIC-FUNCTION("fGetPNPGroup" IN ghFunc1,
                                                    PNPList.BDestFrom).
               IF PNPList.PNPGroup NE "" THEN DO:
                  FIND FIRST PNPGroup WHERE
                             PNPGroup.Brand    = gcBrand AND
                             PNPGroup.PNPGroup = PNPList.PNPGroup
                  NO-LOCK NO-ERROR.
                  PNPList.PNPSeq = PNPGroup.PNPSeq.
                  DISP
                     PNPList.PNPGroup
                     PNPGroup.Name
                  WITH FRAME lis.
               END.
            END. 

            IF FRAME-FIELD = "PNPGroup" THEN DO:

               ASSIGN PNPList.PNPGroup.

               IF bnew AND PNPList.PNPGroup = "" THEN LEAVE.

               FIND FIRST PNPGroup WHERE
                          PNPGroup.PNPGroup = PNPList.PNPGroup
               NO-LOCK NO-ERROR.
               IF NOT AVAIL PNPGroup THEN DO:
                  MESSAGE
                     "PNP Group does not exist !"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               ELSE DO:
                  PNPList.PNPSeq = PNPGroup.PNPSeq.
                  DISP PNPGroup.Name WITH FRAME lis.
               END.
            END.

            IF FRAME-FIELD = "PriceList" THEN DO:
               ASSIGN PNPList.PriceList.
               FIND FIRST PriceList WHERE
                          PriceList.PriceList = PNPList.PriceList
               NO-LOCK NO-ERROR.
               IF NOT AVAIL PriceList THEN DO:
                  MESSAGE
                     "Price list does not exist !"
                  VIEW-AS ALERT-BOX ERROR.
                  NEXT.
               END.
               ELSE DISP PriceList.PLName WITH FRAME lis.
            END.

         END.
         APPLY LASTKEY. 
      END.
   END.
   ELSE PAUSE.

END.
