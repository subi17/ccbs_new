/* ----------------------------------------------------------------------------
  MODULE .......: ROAMTARIFFLIST.P
  FUNCTION .....: Lists roamtariffs.
  APPLICATION ..: TMS
  AUTHOR .......: anttis 
  CREATED ......: 02-07-07
  MODIFIED .....:
  Version ......: xfera 
--------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable RoamTariff

{commali.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'RoamTariff'}
{cparam2.i}

DEF BUFFER xxtariff FOR RoamTariff.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhRoamTariff AS HANDLE NO-UNDO.
   lhRoamTariff = BUFFER RoamTariff:HANDLE.
   RUN StarEventInitialize ( lhRoamTariff ).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhRoamTariff).
   END.
   
   DEFINE VARIABLE lhRoamTariff2 AS HANDLE NO-UNDO.
   lhRoamTariff2 = BUFFER xxtariff:HANDLE.
   RUN StarEventInitialize ( lhRoamTariff2 ).

   DEFINE VARIABLE lhRTItem AS HANDLE NO-UNDO.
   lhRTItem = BUFFER RTItem:HANDLE.
   RUN StarEventInitialize ( lhRTItem ).

   DEFINE VARIABLE lhRoamBDest AS HANDLE NO-UNDO.
   lhRoamBDest = BUFFER RoamBDest:HANDLE.
   RUN StarEventInitialize ( lhRoamBDest ).

END.

DEF INPUT PARAMETER icPlist LIKE RoamTariff.PriceList NO-UNDO init "".

DEF VAR lcSortTime AS CHAR INIT "".
DEF VAR iiRateType AS INT NO-UNDO INIT 0.
DEF VAR iiTimeType AS INT NO-UNDO INIT 0.
DEF VAR irRowId AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR plseek     LIKE RoamTariff.PriceList NO-UNDO.

DEF VAR firstline  AS INT                NO-UNDO.
DEF VAR order      AS INT                NO-UNDO.
DEF VAR ex-order   AS INT                NO-UNDO.
DEF VAR maxOrder   AS INT                NO-UNDO  init 2.
DEF VAR memory     AS RECID              NO-UNDO.
def var line       as int format "99"    NO-UNDO.
DEF VAR delline    AS INT                NO-UNDO.
DEF VAR must-print AS LOG                NO-UNDO.
DEF VAR must-add   AS LOG                NO-UNDO.
DEF VAR ufkey      AS LOG                NO-UNDO.
DEF VAR fr-header  AS CHAR               NO-UNDO.
DEF VAR rtab       AS RECID EXTENT 24    NO-UNDO.
DEF VAR i          AS INT                NO-UNDO.
DEF VAR j          AS INT                NO-UNDO.

DEF VAR xrecid     AS RECID              NO-UNDO.
def var ok         as lo format "Yes/No" NO-UNDO.
DEF VAR lBrHdr     AS CHAR               NO-UNDO.

DEF VAR lcRateType AS CHAR FORMAT "x(16)" NO-UNDO.
DEF VAR lcSortRateType AS CHAR FORMAT "x(16)" NO-UNDO.
DEF VAR lcSortPLMN AS CHAR FORMAT "x(16)" NO-UNDO.

lBrHdr = " ROAMING TARIFF MAINTENANCE " + string(pvm,"99-99-99") + " " .

form
   RoamTariff.ValidFrom   column-label "From"    
   RoamTariff.ValidTo     column-label "To"     
   RoamTariff.PriceList   column-label "PLMN"
   lcRateType             column-label "Type"
   RoamTariff.RoamingType column-label "Org./Term."
   RoamTariff.Service     column-label "Service/Zone"
WITH 
   width 80 OVERLAY ROW 1 scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi + lBrHdr
FRAME sel.

form /* search plmn */
   "Code :" plseek
   help "Give a plmn code"
WITH 
   row 4 col 2 title color value(ctc) " PLMN code "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr1.

cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

ASSIGN
   xrecid    = ?
   delline   = 0
   ufkey     = TRUE
   order     = 1
   firstline = 0.

RUN local-find-first.
IF AVAILABLE RoamTariff THEN DO: 
   ASSIGN
   memory     = recid(RoamTariff)
   must-print = TRUE 
   must-add   = FALSE.
END.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No tariffs available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN 
      memory     = ? 
      must-print = FALSE 
      must-add   = FALSE.
END.

RUN pDispSortInfo.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
   END.

   IF must-add THEN DO ON ENDKEY UNDO, LEAVE: /* RoamTariff -ADD  */
      must-add = false.
      RUN roamtariff.p(0, icPList, iiRateType).
      RUN pDispSortInfo.
      
      ASSIGN
         must-print = TRUE
         ufkey = TRUE.
      /* is there ANY record ? */
      clear frame sel. 
      RUN local-find-first.
      IF NOT AVAILABLE RoamTariff THEN DO:
         memory = ?.
         NEXT LOOP.
      END.
      UP line.
      ASSIGN
         Memory = recid(RoamTariff)
         xrecid = Memory.
      NEXT LOOP.
   END.

   print-line:
   DO :
      IF must-print THEN DO:

         up FRAME-LINE - 1.

         FIND FIRST RoamTariff where 
              recid(RoamTariff) = memory
         NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:

            IF AVAILABLE RoamTariff THEN DO:

               RUN local-find-others.

               RUN local-disp-row.

               rtab[FRAME-LINE] = recid(RoamTariff).

               RUN local-find-next.

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
         ASSIGN firstline = 0 must-print = FALSE.
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
         ASSIGN ufk = 0
         ufk[1] = 0 ufk[2]= 9005 ufk[3]= 9004  ufk[4] = 9003
         ufk[5] = (IF lcRight = "RW" THEN 5 ELSE 0)  
         ufk[6] = (IF lcRight = "RW" THEN 4 ELSE 0)   
         ufk[7] = 0 ufk[8] = 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         {uright1.i '"5,6"'}

         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW RoamTariff.PriceList ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) RoamTariff.PriceList WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW lcRateType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) lcRateType WITH FRAME sel.
      END.
      
      nap = KEYLABEL(LASTKEY).

      IF nap = "F9" THEN NEXT.

      IF rtab[FRAME-LINE] = ? AND
         LOOKUP(nap,"2,f2,3,f3,4,f4,5,f5,8,f8") = 0
      THEN DO:
         BELL.
         message "Cursor is on a empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      if LOOKUP(nap,"cursor-right") > 0 THEN DO:
         if maxOrder > 1 then do:
            if maxOrder = 2 and order = 1 then order = 2.
            ELSE if order + 1 > maxOrder then order = 1.
            ELSE order = order + 1. 
         end. 
      END.
      if LOOKUP(nap,"cursor-left") > 0 THEN DO:
         if maxorder > 1 then do:
           if order - 1 < 1 then do:
              if maxOrder = 2 then order = 2.
              ELSE order = maxOrder.
           end.
           ELSE do:
              if maxOrder = 2 then order = 1.
              ELSE order = order - 1. 
           end. 
         end.
      END.                                              

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND RoamTariff where recid(RoamTariff) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE RoamTariff THEN
               ASSIGN firstline = i memory = recid(RoamTariff).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      /* previous line */
      if LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND RoamTariff where recid(RoamTariff) = rtab[1] NO-LOCK.

            RUN local-find-prev.

            IF NOT AVAILABLE RoamTariff THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.

               RUN local-find-others.

               RUN local-disp-row.

               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(RoamTariff)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      ELSE if LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND RoamTariff where recid(RoamTariff) = rtab[FRAME-DOWN] NO-LOCK .

            RUN local-find-next.

            IF NOT AVAILABLE RoamTariff THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               RUN local-find-others.

               RUN local-disp-row.

               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(RoamTariff).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DO:
            DOWN 1.
         END.   
      END. /* NEXT line */

      /* previous page */
      ELSE if LOOKUP(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND RoamTariff where recid(RoamTariff) = memory NO-LOCK NO-ERROR.

         RUN local-find-prev.

         IF AVAILABLE RoamTariff THEN DO:
            memory = recid(RoamTariff).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE roamTariff THEN memory = recid(RoamTariff).
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
     ELSE if LOOKUP(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND RoamTariff where recid(RoamTariff) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku sarakk. 1 */
     ELSE if LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 THEN DO:  /* haku sar. 1 */
        cfc = "puyr". RUN ufcolor.
        plseek = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        
        UPDATE
               plseek WITH FRAME hayr1.
        HIDE FRAME hayr1 no-pause.

        if plseek <> "" THEN DO:

            
           FIND FIRST RoamTariff where
                      RoamTariff.PriceList = plseek
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE RoamTariff THEN DO:
                 FIND FIRST RoamTariff where
                            RoamTariff.PriceList BEGINS plseek
                 NO-LOCK NO-ERROR.
           END.

           IF NOT AVAILABLE RoamTariff THEN DO:

                 FIND FIRST RoamTariff where
                            RoamTariff.PriceList MATCHES "*" + plseek + "*"
                 NO-LOCK NO-ERROR.

           END.

           NEXT LOOP.

           END.
        END. /* Haku sar. 1 */

      ELSE if LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 THEN DO:
        cfc = "puyr". RUN ufcolor.
        plseek = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        
        UPDATE
               plseek WITH FRAME hayr1.
        HIDE FRAME hayr1 no-pause.


        IF plseek <> "" THEN DO: 
           FIND FIRST RoamOper where
                      RoamOper.PLMN = plseek
           NO-LOCK NO-ERROR.
           IF NOT AVAILABLE RoamOper THEN DO:
                 FIND FIRST RoamGroup where
                            RoamGroup.RoamGroup = plseek
                 NO-LOCK NO-ERROR.
              IF NOT AVAILABLE RoamGroup THEN DO:
                 IF plseek ne "DEFAULT" THEN DO:
                    MESSAGE "Unknown PLMN" VIEW-AS ALERT-BOX.
                    NEXT LOOP.
                 END.   
              END.
           END.
        END.
        
         icPList = plseek.
         RUN pDispSortInfo.
         CLEAR FRAME sel no-pause.
         must-print = TRUE.
         ufkey = true.
         HIDE MESSAGE.
         RUN pDispSortInfo.         
         RUN local-find-first.
         IF AVAILABLE RoamTariff THEN DO: 
            ASSIGN
            memory     = recid(RoamTariff)
            must-add   = FALSE.
         END.
         ELSE memory = ?. 
        
        NEXT LOOP.

      END.
      
      ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 THEN DO:
         DEF VAR menuc AS C EXTENT 10 NO-UNDO.
         DO WHILE TRUE:
            ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN ufkey. 
          DISPLAY
          "A) Ad-Hoc Discount                      "  @ menuc[1] SKIP
          "B) Connection Charge                    "  @ menuc[2] SKIP 
          "C) Voice Charging                       "  @ menuc[3] SKIP
          "D) SMS MO/MT                            "  @ menuc[4] SKIP
          "E) GPRS                                 "  @ menuc[5] SKIP
          "F) Value Added Numbers                  "  @ menuc[6] SKIP
          "H) Zone Pricing                         "  @ menuc[7] SKIP
          "I) Country Code Pricing                 "  @ menuc[8] SKIP
          "J) MTC Pricing                          "  @ menuc[9] SKIP
          "K) ALL TYPES                            "  @ menuc[10] SKIP

            WITH OVERLAY WIDTH 45 FRAME choices NO-LABELS.
            CHOOSE FIELD menuc AUTO-RETURN go-on (F8) WITH FRAME choices
            TITLE " " + "Filter by Type" + " " 
            CENTERED WITH COL 1 ROW 5.
            HIDE FRAME choices. 

            IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.
            
            DEFINE VARIABLE lcRateTypes AS CHARACTER NO-UNDO.
            lcRateTypes = "1,2,3,4,5,7,8,9,10,0".

            IF FRAME-INDEX = 0 THEN LEAVE.
            
            IF FRAME-INDEX <= 10 THEN DO:
               iiRateType = INT(ENTRY(FRAME-INDEX,lcRateTypes)).
               LEAVE.
            END.

      END. /* DO WHILE */

         CLEAR FRAME sel no-pause.
         must-print = TRUE.
         ufkey = true.
         HIDE FRAME choices NO-PAUSE.
         HIDE MESSAGE.
         RUN pDispSortInfo.         
         RUN local-find-first.
         IF AVAILABLE RoamTariff THEN DO: 
            ASSIGN
            memory     = recid(RoamTariff)
            must-print = TRUE 
            must-add   = FALSE.
         END.
         ELSE DO:
            IF lcRight NE "RW" THEN DO:
               MESSAGE "No tariffs available !" VIEW-AS ALERT-BOX.
               RETURN.
            END.
            ASSIGN 
               memory     = ? 
               must-print = TRUE 
               must-add   = FALSE.
         END.
         NEXT LOOP.
      END.


      ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:
         DEF VAR menud AS C EXTENT 4 NO-UNDO.
         DO WHILE TRUE:
            ASSIGN ufk = 0 ufk[8] = 8 ehto = 3. RUN ufkey. 
          DISPLAY
          "A) ALL                                  "  @ menud[1] SKIP
          "B) CURRENT                              "  @ menud[2] SKIP 
          "C) FUTURE                               "  @ menud[3] SKIP
          "D) HISTORY                              "  @ menud[4] SKIP
          
            WITH OVERLAY WIDTH 45 FRAME timechoices NO-LABELS.
            CHOOSE FIELD menud AUTO-RETURN go-on (F8) WITH FRAME timechoices
            TITLE " " + "Filter by Date" + " " 
            CENTERED WITH COL 1 ROW 5.
            HIDE FRAME timechoices.

            IF LOOKUP(KEYLABEL(LASTKEY),"x,F8") > 0  THEN LEAVE.

            IF FRAME-INDEX EQ 1 THEN DO:
               iiTimeType = 0.
               LEAVE.
            END.

            ELSE IF FRAME-INDEX  = 2 THEN DO :
               iiTimeType = 1. 
               LEAVE.
            END.
                    
            ELSE IF FRAME-INDEX = 3 THEN  DO:
               iiTimeType = 2. 
               LEAVE.
            END.

            ELSE IF FRAME-INDEX = 4 THEN DO:
               iiTimeType = 3. 
               LEAVE.
            END.
                     
            ELSE IF FRAME-INDEX = 0 THEN LEAVE.

      END. /* DO WHILE */

         CLEAR FRAME sel no-pause.
         must-print = TRUE.
         ufkey = true.
         HIDE FRAME timechoices NO-PAUSE.
         HIDE MESSAGE.
         RUN pDispSortInfo. 
         RUN local-find-first.
         IF AVAILABLE RoamTariff THEN DO: 
            ASSIGN
            memory     = recid(RoamTariff)
            must-print = TRUE 
            must-add   = FALSE.
         END.
         ELSE DO:
            IF lcRight NE "RW" THEN DO:
               MESSAGE "No tariffs available !" VIEW-AS ALERT-BOX.
               RETURN.
            END.
            ASSIGN 
               memory     = ? 
               must-print = TRUE 
               must-add   = FALSE.
         END.
         NEXT LOOP.
      END.
     

      ELSE if LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN 
         DO ON ENDKEY UNDO, LEAVE:  /* lisays */
         must-add = TRUE.
         NEXT LOOP.
      END.

     ELSE if LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" THEN DO TRANSAction:  
     /* removal */

        delline = FRAME-LINE.
        FIND RoamTariff where recid(RoamTariff) = rtab[FRAME-LINE] NO-LOCK.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) 
           RoamTariff.ValidFrom
           RoamTariff.ValidTo
           RoamTariff.PriceList
           lcRateType
           RoamTariff.RoamingType
           RoamTariff.Service
        WITH FRAME sel.

        RUN local-find-next.   
        IF AVAILABLE RoamTariff THEN memory = recid(RoamTariff).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND RoamTariff where recid(RoamTariff) = rtab[FRAME-LINE] NO-LOCK.
           
           /* AND THEN the previous one */
           RUN local-find-prev.
           IF AVAILABLE RoamTariff THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(RoamTariff).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND FIRST RoamTariff where 
             recid(RoamTariff) = rtab[FRAME-LINE]
        EXCLUSIVE-LOCK NO-ERROR.
        
        ASSIGN ok = FALSE.
        
        IF RoamTariff.ValidFrom <= TODAY THEN DO:
            MESSAGE "Tariff is currently active or history."
            + " . Cannot make any changes." VIEW-AS ALERT-BOX.
        END.
        
        ELSE DO:
           message " ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
        END.
        
        COLOR DISPLAY value(ccc) 
           RoamTariff.ValidFrom
           RoamTariff.ValidTo
           RoamTariff.PriceList
           lcRateType
           RoamTariff.RoamingType
           RoamTariff.Service
        WITH FRAME sel.

        IF ok THEN DO:
            
            FOR EACH RTItem WHERE RTItem.TariffNum = RoamTariff.TariffNum
               EXCLUSIVE-LOCK:
               IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhRTItem).
               DELETE RTItem.            
            END.

            FOR EACH RoamBDest WHERE RoamBDest.TariffNum = RoamTariff.TariffNum
               EXCLUSIVE-LOCK:
               IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhRoamBDest).
               DELETE RoamBDest.            
            END.
            
            /* no gaps between tariffs allowed */ 
            IF RoamTariff.RateType NE 1 AND 
               RoamTariff.RateType NE 2 AND 
               RoamTariff.RateType NE 7 THEN DO:
               FIND FIRST xxTariff WHERE 
                  xxTariff.PriceList = RoamTariff.PriceList AND
                  xxTariff.RateType  = RoamTariff.RateType AND
                  xxTariff.Service   = RoamTariff.Service AND
                  xxTariff.ValidTo   = RoamTariff.ValidFrom - 1 AND
                  xxTariff.RoamingType = RoamTariff.RoamingType AND
                  RECID(xxtariff) NE RECID(RoamTariff)
                  EXCLUSIVE-LOCK NO-ERROR.
               IF AVAIL xxTariff THEN DO:
                  IF llDoEvent THEN RUN StarEventSetOldBuffer (lhRoamTariff2).
                  xxTariff.ValidTo = RoamTariff.ValidTo.               
                  IF llDoEvent 
                     THEN RUN StarEventMakeModifyEvent (lhRoamTariff2).
               END.
            END.   

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhRoamTariff). 
            DELETE RoamTariff.

            /* in the LAST record was deleted ? */
            RUN local-find-first.
            IF NOT AVAILABLE RoamTariff THEN DO:
               CLEAR FRAME sel no-pause.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     ELSE if LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW"  THEN DO:

        FIND FIRST RoamTariff where 
             recid(RoamTariff) = rtab[frame-line(sel)]
        NO-LOCK.
        
        IF RoamTariff.ValidFrom <= TODAY THEN DO:
            run roamtariff.p(RoamTariff.TariffNum, "", 0).
            RUN pDispSortInfo. 
            ufkey = true.
            ex-order = -1.
            RUN local-disp-row.
            xrecid = recid(RoamTariff).
            NEXT LOOP.
        END.
        run roamtariff.p(RoamTariff.TariffNum, "", 0).
        must-print = true.
        RUN pDispSortInfo.
        ufkey = true.

        RUN local-disp-row.
        xrecid = recid(RoamTariff).
  
  END.

     ELSE if LOOKUP(nap,"home,h") > 0 THEN DO:
        RUN local-find-first.
        ASSIGN
           memory     = recid(RoamTariff)
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE if LOOKUP(nap,"end,e") > 0 THEN DO : /* LAST record */
        RUN local-find-last.
        ASSIGN
           memory     = recid(RoamTariff)
           must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE if LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

PROCEDURE local-find-FIRST:
   
   IF order EQ 1 THEN
      FIND FIRST RoamTariff WHERE
         (icPlist = "" OR
         (RoamTariff.PriceList = icPlist)) AND
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         USE-INDEX PriceList NO-LOCK NO-ERROR.
   IF order EQ 2 THEN
      FIND FIRST RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR (RoamTariff.PriceList = icPlist))
         USE-INDEX RateType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:
   
   IF order EQ 1 THEN
      FIND NEXT RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR
         (RoamTariff.PriceList = icPlist))
         USE-INDEX PriceList NO-LOCK NO-ERROR.
   IF order EQ 2 THEN
      FIND NEXT RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR
         (RoamTariff.PriceList = icPlist))
         USE-INDEX RateType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:
   
   IF order EQ 1 THEN
      FIND PREV RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR
         (RoamTariff.PriceList = icPlist))
         USE-INDEX PriceList NO-LOCK NO-ERROR.
   IF order EQ 2 THEN
      FIND PREV RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR
         (RoamTariff.PriceList = icPlist))
         USE-INDEX RateType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:
   
   IF order EQ 1 THEN
      FIND LAST RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR
         (RoamTariff.PriceList = icPlist))
         USE-INDEX PriceList NO-LOCK NO-ERROR.
   IF order EQ 2 THEN
      FIND LAST RoamTariff WHERE
         (iiRateType = 0 OR  RoamTariff.RateType = iiRateType)
         AND (iiTimeType = 0 OR (iiTimeType = 3 AND ValidTo < TODAY)
         OR (iiTimeType = 2 AND ValidFrom > TODAY)
         OR (iiTimeType = 1 AND ValidFrom <= TODAY AND ValidTo >= TODAY))
         AND (icPlist = "" OR
         (RoamTariff.PriceList = icPlist))
         USE-INDEX RateType NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-others:

END PROCEDURE.

PROCEDURE local-disp-row:

   CLEAR FRAME sel NO-PAUSE.
   FIND FIRST TMSCodes WHERE
      TMSCodes.TableName = "RoamTariff"   AND
      TMSCodes.FieldName = "RateType" AND
      TMSCodes.CodeGroup = "Roaming"   AND
      TMSCodes.CodeValue = STRING(RoamTariff.RateType)
   NO-LOCK NO-ERROR.
   IF AVAIL TMSCodes THEN DO:
      lcRateType = TMSCodes.CodeName.
   END.
   DISPLAY 
      RoamTariff.ValidFrom
      RoamTariff.ValidTo WHEN RoamTariff.ValidTo NE DATE(12,31,9999)
      RoamTariff.PriceList
      lcRateType
      RoamTariff.Service
      RoamTariff.RoamingType
   WITH FRAME sel.

END PROCEDURE. 

PROCEDURE pDispSortInfo:
   
   IF iiRateType <> 0 THEN DO:
      FIND FIRST TMSCodes WHERE
         TMSCodes.TableName = "RoamTariff"   AND
         TMSCodes.FieldName = "RateType" AND
         TMSCodes.CodeGroup = "Roaming"   AND
         TMSCodes.CodeValue = STRING(iiRateType)
      NO-LOCK NO-ERROR.
      IF AVAIL TMSCodes THEN DO:
         lcSortRateType = TMSCodes.CodeName.
      END.
   END.
   ELSE DO:
      lcSortRateType = "".   
   END.

   CASE iiTimeType:
      WHEN 1 THEN DO: lcSortTime = "CURRENT". END.

      WHEN 2 THEN DO: lcSortTime = "FUTURE". END.

      WHEN 3 THEN DO: lcSortTime = "HISTORY". END.

      WHEN 0 THEN DO: lcSortTime = "". END.
   END.
   
   put screen row 19 col 2
"                                                                              "
.
   put screen row 19 col 10 "PLMN: " + icPList  +
   "    RateType: " + lcSortRateType +
   "    DATE: " + lcSortTime.
END.

