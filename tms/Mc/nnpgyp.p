/* -----------------------------------------------
  MODULE .......: NNPGYP.P
  FUNCTION .....: BItemGroup maintenance
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 12-08-96
  MODIFIED .....: 24.02.1997 pt tilinumero AccNum
                  11.02.1998 kl UPDATE BItemGroup.tu-mtili
                  25.06.1998 kl vast => ok
                  18.05.1999 jp uright1 & uright2 added  
                  20.07.2001 kl tunimi table removed
                  26.11.01 lp RUN Mc/memo added
                  26.04.02 tk eventlogging added
                  22.07.02 tk show full page on "end"
                  29.07.02 lp F4 is empty
                  12.11.02 jr "prgrp" => "bitemgroup" in memo
                  26.02.03 tk tokens
                  05.09.03 aam brand 
                  06.02.04 jp custnum for memo
                  14.12.06 aam translations (invlang)
  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable BItemGroup

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'bitemgroup'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBItemGroup AS HANDLE NO-UNDO.
   lhBItemGroup = BUFFER BItemGroup:HANDLE.
   RUN StarEventInitialize(lhBItemGroup).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhBItemGroup). 
   END.

END.


DEF NEW shared VAR siirto AS CHAR.

def var ok         as lo format "Yes/No" NO-UNDO.
DEF VAR haku       LIKE BItemGroup.BIGroup    NO-UNDO.
DEF VAR haku2      LIKE BItemGroup.BIGName    NO-UNDO.
DEF VAR firstline  AS INT                NO-UNDO.
DEF VAR order      AS INT                NO-UNDO.
DEF VAR ex-order   AS INT                NO-UNDO.
DEF VAR memory     AS RECID              NO-UNDO.
def var line       as int format "99"    NO-UNDO.
DEF VAR delline    AS INT                NO-UNDO.
DEF VAR must-print AS LOG                NO-UNDO.
DEF VAR must-add   AS LOG                NO-UNDO.
DEF VAR ufkey      AS LOG                NO-UNDO.
DEF VAR fr-header  AS CHAR.
DEF VAR rtab       AS RECID EXTENT 24    NO-UNDO.
DEF VAR i          AS INT NO-UNDO.
DEF VAR xrecid     AS RECID.
def var endloop as i no-undo.

DEF VAR lcDoc1      AS CHAR NO-UNDO.
DEF VAR lcGraph     AS CHAR NO-UNDO.
DEF VAR lcDoc1Name  AS CHAR NO-UNDO.
DEF VAR lcGraphName AS CHAR NO-UNDO.
DEF VAR lcCode      AS CHAR NO-UNDO. 

DEF BUFFER bGroup FOR BItemGroup.

form
    BItemGroup.Brand 
    BItemGroup.BIGroup   /* column-label "Gr. code"   */
    BItemGroup.BIGName   /* column-label "Group name" */
    lcDoc1              FORMAT "X(8)" COLUMN-LABEL "Doc1"
    lcGraph             FORMAT "X(8)" COLUMN-LABEL "Graph"
    BItemGroup.InvoiceOrder
WITH width 80 OVERLAY /* scroll 1 */ 15 DOWN COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Billing Item Groups " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    BItemGroup.Brand   COLON 20
    BItemGroup.BIGroup COLON 20  
    BItemGroup.BIGName COLON 20
    lcDoc1  COLON 20
       FORMAT "X(8)"
       LABEL "Doc1 Group"
       HELP "Doc1 group code (XFERAxxxx)"
    lcDoc1Name
       NO-LABEL
       FORMAT "X(30)"
       SKIP
    lcGraph COLON 20
       FORMAT "X(8)"
       LABEL "Graph Group"
       HELP "Graph group in Doc1" 
    lcGraphName
       NO-LABEL
       FORMAT "X(30)"
       SKIP
    BItemGroup.InvoiceOrder COLON 20
WITH  OVERLAY ROW 7 col 5 COLOR value(cfc) TITLE COLOR value(ctc)
    fr-header WITH side-labels FRAME lis.

{Func/brand.i}

form /* produkt :n tunnuksella hakua varten */
    "Brand:" lcBrand skip
    "Code :" haku
    help "Give a code or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* produkt :n nimella hakua varten */
    "Brand:" lcBrand skip
    "Name :" haku2
    help "Give a Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* memo */
WITH
    OVERLAY ROW 7 centered NO-LABEL
    color value(cfc) title color value(cfc) " Update memo "
    FRAME memo.             

FUNCTION fDispDoc1 RETURNS LOGIC
   (icDoc1 AS CHAR):
   
   lcDoc1Name = "".
   IF icDoc1 > "" THEN DO:
      lcDoc1Name = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                    "Doc1",
                                    "SummaryGroup",
                                    icDoc1).
   END.
   
   DISPLAY lcDoc1Name WITH FRAME lis.
END.


FUNCTION fDispGraph RETURNS LOGIC
   (icGraph AS CHAR):
   
   lcGraphName = "".
   IF icGraph > "" THEN DO:
      FIND FIRST bGroup WHERE
                 bGroup.Brand = gcBrand AND
                 bGroup.BIGroup = icGraph NO-LOCK NO-ERROR.
      IF AVAILABLE bGroup THEN lcGraphName = bGroup.BIGName.
   END.
   
   DISPLAY lcGraphName WITH FRAME lis.
END.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST BItemGroup WHERE BItemGroup.Brand = lcBrand
no-lock no-error.

IF AVAILABLE BItemGroup THEN 
ASSIGN 
   memory     = recid(BItemGroup)
   must-print = TRUE 
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No BillCode groups available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN 
      memory     = ? 
      must-print = FALSE 
      must-add   = TRUE.
END.

ASSIGN 
   xrecid = ? 
   delline = 0 
   ufkey = TRUE 
   order = 1 
   firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO:  /* BItemGroup -ADD  */
      assign 
         cfc = "lis" 
         ufkey = true 
         fr-header = " ADD" 
         must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.
         DO TRANSAction:
            DISPLAY lcBrand @ BItemGroup.Brand.

            PROMPT-FOR BItemGroup.BIGroup
            VALIDATE
               (BItemGroup.BIGroup = "" OR
               NOT can-find(BItemGroup WHERE
                            BItemGroup.Brand = lcBrand AND
                            BItemGroup.BIGroup = INPUT BItemGroup.BIGroup),
               "Group " + string(INPUT BItemGroup.BIGroup) +
               " already exists !").
            if input BItemGroup.BIGroup = "" THEN LEAVE add-new.
            CREATE BItemGroup.
            ASSIGN
            BItemGroup.Brand   = lcBrand
            BItemGroup.BIGroup = INPUT FRAME lis BItemGroup.BIGroup.
            UPDATE BItemGroup.BIGName.
            ASSIGN
            memory = recid(BItemGroup)
            xrecid = memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBItemGroup).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST BItemGroup WHERE BItemGroup.Brand = lcBrand no-lock no-error.
      IF NOT AVAILABLE BItemGroup THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND BItemGroup where recid(BItemGroup) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:

            IF AVAILABLE BItemGroup THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(BItemGroup).
               IF order = 1 THEN FIND NEXT BItemGroup 
                  WHERE BItemGroup.Brand = lcBrand no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT BItemGroup USE-INDEX BIGName
                  WHERE BItemGroup.Brand = lcBrand no-lock no-error.
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.

            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.

            DOWN.

         END.
         IF endloop = 0 then up FRAME-LINE - 1.
         DOWN firstline.
         ASSIGN firstline = 0
                must-print = FALSE
                endloop = 0.
         PAUSE 0 no-message.

         /* one page of data has been Printed AND
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
         ufk[1] = 35 ufk[2] = 30 ufk[3] = 927 ufk[4] = 814
         ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7] = 0   ufk[8] = 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW BItemGroup.BIGroup ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BItemGroup.BIGroup WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW BItemGroup.BIGName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BItemGroup.BIGName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT. 

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 3 THEN order = 1.
      END.

      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 2.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND BItemGroup where recid(BItemGroup) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev BItemGroup
               WHERE BItemGroup.Brand = lcBrand no-lock no-error.
            ELSE IF order = 2 THEN FIND prev BItemGroup USE-INDEX BIGName
               WHERE BItemGroup.Brand = lcBrand no-lock no-error.
            IF AVAILABLE BItemGroup THEN
               ASSIGN firstline = i memory = recid(BItemGroup).
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
            FIND BItemGroup where recid(BItemGroup) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev BItemGroup 
               WHERE BItemGroup.Brand = lcBrand no-lock no-error.
            ELSE IF order = 2 THEN FIND prev BItemGroup USE-INDEX BIGName
               WHERE BItemGroup.Brand = lcBrand no-lock no-error.
            IF NOT AVAILABLE BItemGroup THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               RUN local-disp-row.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(BItemGroup)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */


      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:  
            FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-LINE] no-lock.             IF order = 1 THEN FIND NEXT BItemGroup
               WHERE BItemGroup.Brand = lcBrand no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT BItemGroup USE-INDEX BIGName
               WHERE BItemGroup.Brand = lcBrand no-lock no-error.

            IF NOT AVAILABLE BItemGroup THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(BItemGroup).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .  
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND BItemGroup where recid(BItemGroup) = memory no-lock no-error.
         IF order = 1 THEN FIND prev BItemGroup
            WHERE BItemGroup.Brand = lcBrand no-lock no-error.
         ELSE IF order = 2 THEN FIND prev BItemGroup USE-INDEX BIGName
            WHERE BItemGroup.Brand = lcBrand no-lock no-error.
         IF AVAILABLE BItemGroup THEN DO:
            memory = recid(BItemGroup).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev BItemGroup 
                  WHERE BItemGroup.Brand = lcBrand no-lock no-error.
               ELSE IF order = 2 THEN FIND prev BItemGroup USE-INDEX BIGName
                  WHERE BItemGroup.Brand = lcBrand no-lock no-error.
               IF AVAILABLE BItemGroup THEN memory = recid(BItemGroup).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            message "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-message.
         END.
     END. /* previous page */

     /* NEXT page */
     else if lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND BItemGroup where recid(BItemGroup) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN Syst/ufcolor.
        haku = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr.
        UPDATE lcBrand WHEN gcAllBrand
               haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.
        if haku <> "" THEN DO:
           FIND FIRST BItemGroup where 
              BItemGroup.Brand = lcBrand AND
              BItemGroup.BIGroup >= INPUT haku no-lock no-error.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN Syst/ufcolor.
        haku2 = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr2.
        UPDATE lcBrand WHEN gcAllBrand
               haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if haku2 <> "" THEN DO:
           FIND FIRST BItemGroup 
              WHERE BItemGroup.Brand = lcBrand and
                    BItemGroup.BIGName >= INPUT haku2
              no-lock no-error.

           IF NOT fRecFound(2) THEN NEXT. 

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     if lookup(nap,"3,f3") > 0 
     THEN DO TRANS: /* memo */
        FIND BItemGroup where recid(BItemGroup) = rtab[frame-line(sel)]
        NO-LOCK NO-ERROR.

        RUN Mc/memo(INPUT 0,
                 INPUT "bitemgroup",
                 INPUT STRING(BItemGroup.BIGroup),
                 INPUT "BillItem Group code").
        ufkey = TRUE.
        NEXT LOOP.

     END.

     /* translations */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
         FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/invlang(6,BItemGroup.BIGroup).
         
         ufkey = TRUE.
         NEXT LOOP.
     END.


     ELSE if  lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */

        {Syst/uright2.i}

        delline = FRAME-LINE.
        FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) BItemGroup.BIGroup BItemGroup.BIGName.

        IF order = 1 THEN FIND NEXT BItemGroup 
           WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND NEXT BItemGroup USE-INDEX BIGName
           WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        IF AVAILABLE BItemGroup THEN memory = recid(BItemGroup).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev BItemGroup 
              WHERE BItemGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev BItemGroup USE-INDEX BIGName
              WHERE BItemGroup.Brand = lcBrand no-lock no-error.
           IF AVAILABLE BItemGroup THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(BItemGroup).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND BItemGroup where recid(BItemGroup) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N) ? " UPDATE ok.
        COLOR DISPLAY value(ccc) BItemGroup.BIGroup BItemGroup.BIGName.
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhBItemGroup).

            DELETE BItemGroup.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST BItemGroup) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 THEN
     DO WITH FRAME lis TRANSAction:
        /* change */
        FIND BItemGroup where recid(BItemGroup) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN Syst/ufkey.
        cfc = "lis". RUN Syst/ufcolor.

        RUN local-find-others.
        DISPLAY 
          BItemGroup.Brand
          BItemGroup.BIGroup
          BItemGroup.BIGName
          lcDoc1
          lcGraph
          BItemGroup.InvoiceOrder.
        fDispDoc1(lcDoc1).
        fDispGraph(lcGraph).

        IF lcRight = "RW" THEN DO:  
           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBItemGroup).

           REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:
              UPDATE 
                 BItemGroup.BIGName
                 lcDoc1
                 lcGraph
                 BItemGroup.InvoiceOrder
              EDITING:

                 READKEY.

                 IF KEYLABEL(LASTKEY) = "F9" AND 
                    FRAME-FIELD = "lcDoc1"
                 THEN DO:

                    RUN Help/h-tmscodes(INPUT "Doc1",    /* TableName */
                                         "SummaryGroup",   /* FieldName */
                                         "Printing",  /* GroupCode */
                                   OUTPUT lcCode).

                    IF lcCode ne "" AND lcCode NE ? THEN DO:
                       DISPLAY lcCode ;& lcDoc1 WITH FRAME lis.   
                    END.   

                    ehto = 9.
                    RUN Syst/ufkey.
                    NEXT. 
                 END.

                 ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
                 DO WITH FRAME lis:
                    PAUSE 0.
      
                    IF FRAME-FIELD = "lcDoc1" THEN DO:
                       fDispDoc1(INPUT INPUT lcDoc1).
                      
                       IF INPUT lcDoc1 > "" AND lcDoc1Name = "" THEN DO:
                          MESSAGE "Unknown Doc1 group"
                          VIEW-AS ALERT-BOX ERROR.
                          NEXT.
                       END.
                    END.

                    ELSE IF FRAME-FIELD = "lcGraph" THEN DO:
                       fDispGraph(INPUT INPUT lcGraph).
                      
                       IF INPUT lcGraph > "" AND lcGraphName = "" THEN DO:
                          MESSAGE "Unknown graph group"
                          VIEW-AS ALERT-BOX ERROR.
                          NEXT.
                       END.
                    END.
                 END.
                 
                 APPLY LASTKEY.
              END. /* EDITING */
       
              BItemGroup.ReportCode = lcDoc1 + "," + lcGraph.
              LEAVE.
           END.
           
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBItemGroup).

        END.
        ELSE PAUSE.

        HIDE FRAME lis no-pause.
        RUN local-disp-row.

        xrecid = recid(BItemGroup).                
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST BItemGroup
           WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST BItemGroup USE-INDEX BIGName
           WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        ASSIGN memory = recid(BItemGroup) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */

        IF      order = 1 THEN FIND LAST BItemGroup 
           WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST BItemGroup USE-INDEX BIGName
           WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        do endloop = 1 to frame-down - 1:
           IF      order = 1 then find prev BItemGroup 
             WHERE BItemGroup.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND PREV BItemGroup USE-INDEX BIGName
              WHERE BItemGroup.Brand = lcBrand no-lock no-error.
        end.
        ASSIGN 
           memory = recid(BItemGroup) 
           must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */

END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.


PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
          BitemGroup.Brand 
          BItemGroup.BIGroup 
          BItemGroup.BIGName
          lcDoc1
          lcGraph
          BItemGroup.InvoiceOrder
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   lcDoc1 = ENTRY(1,BItemGroup.ReportCode).
   IF NUM-ENTRIES(BItemGroup.ReportCode) > 1 THEN
      lcGraph = ENTRY(2,BItemGroup.ReportCode).
   ELSE lcGraph = "".   

END PROCEDURE.


