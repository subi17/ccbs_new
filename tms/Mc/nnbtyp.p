/* ----------------------------------------------------------------------------
  MODULE .......: NNBTYP.P
  FUNCTION .....: BDest maintenance
  APPLICATION ..: TMS
  AUTHOR .......: PT
  CREATED ......: 21-01-96
  MODIFIED .....: 15.11.01 kl: CTRL-B - copy BDest
                  07.12.01 kl: DISPLAY FORMATs
                  30.05.02 kl: BDestHist replaces BDest.CustNum
                  14.05.02 kl: Customer rates into MORE INFO
                  20.08.02/tk: Cubio version + Eventlogging
                  05.09.02/jp: validation
                  23.09.02/jr: class F9 & validate
                  23.09.02/aam ccn and rbdest removed,
                               rateccn and bdesthist from here 
                  26.02.03/tk: tokens
                  08.09.03/aam brand 
                  16.10.03/aam use bdest.ccn as reporting ccn
                  30.10.03/jp  display bdest.brand
                  02.12.06/jt  some ui tuning... browsing should be updated
                  14.12.06/aam translations (invlang)
  Version ......: M15
  --------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable BDest

{Syst/commali.i} 
{Func/tmsparam2.i}
{Func/timestamp.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'bdest'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhBDest AS HANDLE NO-UNDO.
   lhBDest = BUFFER BDest:HANDLE.
   RUN StarEventInitialize( lhBDest ).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhBDest).
   END.

END.

DEF shared VAR siirto AS CHAR.

DEF BUFFER bBDest FOR BDest.
DEF BUFFER bTariff FOR Tariff.

DEF VAR haku         LIKE BDest.BDest       NO-UNDO.
DEF VAR hakunimi     LIKE BDest.BDName      NO-UNDO.
DEF VAR liCCN        LIKE BDest.CCN         NO-UNDO. 
DEF VAR firstline    AS INT                 NO-UNDO.
DEF VAR order        AS INT                 NO-UNDO.
DEF VAR ex-order     AS INT                 NO-UNDO.
DEF VAR memory       AS RECID               NO-UNDO.
DEF VAR line         as int FORMAT "99"     NO-UNDO.
DEF VAR delline      AS INT                 NO-UNDO.
DEF VAR must-print   AS LOG                 NO-UNDO.
DEF VAR must-add     AS LOG                 NO-UNDO.
DEF VAR ufkey        AS LOG                 NO-UNDO.
DEF VAR fr-header    AS CHAR                NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24     NO-UNDO.
DEF VAR i            AS INT                 NO-UNDO.
DEF VAR xrecid       AS RECID               NO-UNDO.
DEF VAR ok           AS LOG                 NO-UNDO FORMAT "Yes/No".
DEF VAR lcDestType   AS CHAR                NO-UNDO.
DEF VAR lcClass      AS CHAR                NO-UNDO. 
DEF VAR copyto       LIKE BDest.BDest       NO-UNDO.
DEF VAR lcCode       AS CHAR                NO-UNDO.

FORM
   BDest.Brand     FORMAT "x(2)"     column-LABEL "Br"
   BDest.BDest     FORMAT "x(15)" 
   BDest.BDName    FORMAT "x(34)"
   BDest.CCN       FORMAT ">>>9"      COLUMN-LABEL "CCN"
   lcDestType      FORMAT "x(10)"    COLUMN-LABEL "DestType"
   BDest.ToDate   
WITH 
   width 80 ROW 1 OVERLAY scroll 1 15 DOWN
   COLOR value(cfc)
   title color value(ctc) " " + ynimi +
      " B-DESTINATIONS " + 
      string(pvm,"99-99-99") + " " FRAME sel.

FORM
   BDest.Brand   LABEL "Brand" COLON 20 SKIP
   BDest.BDestID COLON 20 
   BDest.BDest   LABEL "B-Destination" COLON 20 SKIP
   BDest.BDName  LABEL "Name/description" COLON 20 
      FORMAT "X(40)" SKIP
   BDest.DestType  LABEL "Destination Type" COLON 20
      FORMAT ">>9"
      help "Destination type"   
      lcDestType NO-LABEL FORMAT "X(30)" SKIP
   BDest.CCN     LABEL "Reporting CCN" COLON 20
      FORMAT ">>>9"
      HELP "Reporting CCN" 
      CCN.CCNName NO-LABEL SKIP
   Bdest.Class   LABEL "Class" COLON 20 
      FORMAT ">>9" 
      lcClass NO-LABEL FORMAT "X(30)" SKIP
   BDest.FromDate LABEL "Valid From" COLON 20 SKIP
   BDest.ToDate LABEL "Valid To" COLON 20
WITH  
   OVERLAY ROW 4 centered COLOR value(cfc) TITLE COLOR value(ctc)
   fr-header WITH side-LABELs FRAME lis.

{Func/brand.i}


FORM 
   "Brand:" lcBrand skip
   "BDest:" haku
   help "Give B-destination."                         
WITH 
   row 4 col 2 title color value(ctc) " FIND B-DESTINATION "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

FORM 
   "Brand:" lcBrand skip
   "Name :" hakunimi
   help "Give name of B-Destination"             
WITH 
   row 4 col 2 title color value(ctc) " FIND NAME "
   COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr3.


FORM
   " Copy from :" BDest.BDest SKIP
   " Copy to ..:" copyto
WITH 
   NO-LABEL centered OVERLAY ROW 6 
   title " Copy B-Destination " FRAME fDestcopy.

FORM
   " Copy from :" BDest.BDest SKIP
   " Copy to ..:" copyto
WITH 
   NO-LABEL centered OVERLAY ROW 6 
   title " Copy tariffs of a B-Destination " FRAME prcopy.


FUNCTION fDestType RETURNS CHAR
   (iiDestType AS INT):
   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "BDest",
                           "BDestType",
                           STRING(iiDestType)).
END.

FUNCTION fBDestClass RETURNS CHAR
  (iiClass AS INT):

   RETURN DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                           "BDest",
                           "BDestClass",
                           STRING(iiClass)).
END.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST BDest where 
   BDest.Brand = lcBrand  NO-LOCK NO-ERROR.
IF AVAILABLE BDest THEN ASSIGN 
   memory     = recid(BDest)
   must-print = TRUE 
   must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No B-Destinations available" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN 
      memory = ?
      must-print = FALSE
      must-add    = TRUE.
END.

ASSIGN 
   xrecid    = ? 
   delline   = 0 
   ufkey     = TRUE 
   order     = 1 
   firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.

   IF must-add THEN DO:  /* BDest -ADD  */
      ASSIGN ufkey = TRUE must-add = FALSE.
      
      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-MESSAGE.
         assign cfc = "lis" ehto = 9. RUN Syst/ufkey.p. RUN Syst/ufcolor.p.
         fr-header = " ADD ".

         DO TRANSAction ON ENDKEY UNDO add-new, LEAVE add-new:
            CLEAR FRAME lis no-pause.

            DISPLAY lcBrand @ BDest.Brand.

            PROMPT-FOR 
               BDest.BDest 
               BDest.BDName
               BDest.DestType WITH FRAME lis.

            IF INPUT FRAME lis BDest.BDest = "" THEN 
               UNDO add-new, LEAVE add-new.
            
            /* ToDate not used (yet) in this check */
            IF CAN-FIND(FIRST bBDest where
                        bBDest.Brand = lcBrand AND
                        bBDest.BDest = INPUT FRAME lis BDest.BDest AND
                        bBDest.DestType = INPUT FRAME lis BDest.DestType)
            THEN DO:        
               MESSAGE "B-destination"
                       INPUT FRAME lis BDest.BDest
                       "already exists"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            if TRIM(INPUT FRAME lis BDest.BDest) NE 
               INPUT FRAME lis BDest.BDest
            THEN DO:
               MESSAGE "B-destination contains invalid characters"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
            END.

            FIND LAST BDest USE-INDEX BDestID NO-LOCK NO-ERROR.
            IF AVAILABLE BDest THEN i = BDest.BDestID + 1.
            ELSE i = 1.
            CREATE BDest.
            ASSIGN 
               FRAME lis BDest.BDest
               FRAME lis BDest.DestType
               FRAME lis BDest.BDName
               BDest.Brand = lcBrand
               BDest.BDestID = i
               BDest.FromDate = TODAY
               BDest.ToDate = 12/31/2049.

            RUN local-update-record.
  
            IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
               UNDO add-new, LEAVE add-new.

            IF llDoEvent THEN RUN StarEventMakeCreateEvent ( lhBDest ).

            CLEAR FRAME lis no-pause.
            ASSIGN memory = recid(BDest) xrecid = memory .
         END.
      END.  /* add-new */
      
      CLEAR FRAME lis no-pause.
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST BDest where 
         BDest.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BDest THEN LEAVE LOOP.
      HIDE FRAME lis no-pause.
      NEXT LOOP.

   END.

   print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND BDest where recid(BDest) = memory NO-LOCK NO-ERROR.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE BDest THEN DO:

               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(BDest).
               IF order = 1 THEN FIND NEXT BDest where
                  BDest.Brand = lcBrand 
                  NO-LOCK NO-ERROR.
               ELSE IF order = 2 THEN FIND NEXT BDest where
                  BDest.Brand = lcBrand    
                  USE-INDEX BDName NO-LOCK NO-ERROR.
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
         PAUSE 0 no-MESSAGE.

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
         ufk[1]= 704 ufk[2]= 717 ufk[3]= 0    ufk[4]= 814
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)  
         ufk[7]= 1162 ufk[8]= 8   ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.

      END.
      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW BDest.BDest {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) BDest.BDest WITH FRAME sel.
      END.
      IF order = 2 THEN DO:
         CHOOSE ROW BDest.BDName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY value(ccc) BDest.BDName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = KEYLABEL(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 5 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 5.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND BDest where recid(BDest) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev BDest where
               BDest.Brand = lcBrand 
               NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND prev BDest where
               BDest.Brand = lcBrand 
               USE-INDEX BDName NO-LOCK NO-ERROR.
            IF AVAILABLE BDest THEN
               ASSIGN firstline = i memory = recid(BDest).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      if lookup(nap,"CTRL-B") > 0 THEN DO WITH FRAME prcopy TRANS:
         PAUSE 0.
         FIND BDest where recid(BDest) = rtab[frame-line(sel)].

         DISP BDest.BDest WITH FRAME fDestcopy.
         UPDATE copyto WITH FRAME fDestcopy.
         ASSIGN copyto.

         if copyto NE "" THEN DO:
            IF CAN-FIND(FIRST bBDest WHERE
                              bBDest.Brand = gcBrand AND
                              bBDest.BDest = copyto AND
                              bBDest.DestType = BDest.DestType AND
                              bBDest.ToDate = 12/31/49)
            THEN DO:
               MESSAGE "B-destination already exists"
               VIEW-AS ALERT-BOX ERROR.
            END.
            ELSE DO:
               CREATE bBDest.
               ASSIGN bBDest.BDest = copyto.
               buffer-copy BDest except BDest.BDest 
                  BDest.FromDate BDest.ToDate 
                  TO bBDest.
               ASSIGN  
                  bBDest.FromDate = TODAY
                  bBDest.ToDate   = 12/31/49.
            END.      
         END.

         HIDE FRAME fDestcopy.
         must-print = TRUE.
      END.

      ELSE IF lookup(nap,"CTRL-Z") > 0 THEN DO WITH FRAME prcopy:

         PAUSE 0.
         FIND BDest where recid(BDest) = rtab[frame-line(sel)].

         DISP BDest.BDest WITH FRAME prcopy.
         UPDATE copyto WITH FRAME prcopy.

         if copyto NE "" THEN DO:

            i = 0.
            IF CAN-FIND(FIRST bBDest WHERE 
                              bBDest.Brand = gcBrand AND
                              bBDest.BDest = copyto) THEN 
            FOR EACH Tariff NO-LOCK where
                     Tariff.Brand = gcBrand AND
                     Tariff.BDest = BDest.BDest AND
                     Tariff.ValidTo >= TODAY:

               IF CAN-FIND(FIRST bTariff where
                                 bTariff.Brand    = gcBrand AND
                                 bTariff.BDest    = copyto AND
                                 bTariff.CustNum  = Tariff.CustNum  AND
                                 bTariff.PriceList = Tariff.PriceList AND
                                 bTariff.ValidFrom = Tariff.ValidFrom)   
               THEN NEXT.

               CREATE bTariff.
               BUFFER-COPY Tariff EXCEPT TariffNum BDest TO bTariff.
               ASSIGN 
                  bTariff.TariffNum = NEXT-VALUE(Tariff)
                  bTariff.BDest = copyto
                  i = i + 1.
            END.

            HIDE FRAME prcopy NO-PAUSE.
            MESSAGE i "tariffs were copied"
            VIEW-AS ALERT-BOX TITLE " DONE ".
         END.

      END.

      /* previous line */
      ELSE IF lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND BDest where recid(BDest) = rtab[1] NO-LOCK.
            IF order = 1 THEN FIND prev BDest where
                BDest.Brand = lcBrand 
                NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND prev BDest where
                BDest.Brand = lcBrand 
                USE-INDEX BDName NO-LOCK NO-ERROR.

            IF NOT AVAILABLE BDest THEN DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-MESSAGE.
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
               rtab[1] = recid(BDest)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      ELSE IF lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND BDest where recid(BDest) = rtab[FRAME-DOWN] NO-LOCK .
            IF order = 1 THEN FIND NEXT BDest where
               BDest.Brand = lcBrand 
               NO-LOCK NO-ERROR.
            ELSE IF order = 2 THEN FIND NEXT BDest  where
               BDest.Brand = lcBrand 
               USE-INDEX BDName NO-LOCK NO-ERROR.

            IF NOT AVAILABLE BDest THEN DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(BDest).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      ELSE IF lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND BDest where recid(BDest) = memory NO-LOCK NO-ERROR.
         IF order = 1 THEN FIND prev BDest where
             BDest.Brand = lcBrand 
             NO-LOCK NO-ERROR.
         ELSE IF order = 2 THEN FIND prev BDest where
             BDest.Brand = lcBrand 
             USE-INDEX BDName NO-LOCK NO-ERROR.

         IF AVAILABLE BDest THEN DO:
            memory = recid(BDest).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev BDest where
                  BDest.Brand = lcBrand 
                  NO-LOCK NO-ERROR.
               ELSE IF order = 2 THEN FIND prev BDest where
                  BDest.Brand = lcBrand 
                  USE-INDEX BDName NO-LOCK NO-ERROR.

               IF AVAILABLE BDest THEN memory = recid(BDest).
               ELSE line = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* this is the FIRST data page */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL.
            PAUSE 1 no-MESSAGE.
         END.
      END. /* previous page */

      /* NEXT page */
      ELSE IF lookup(nap,"next-page,page-down,+") > 0 THEN DO WITH FRAME sel:
         /* cursor TO the downmost line */
         IF rtab[FRAME-DOWN] = ? THEN DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-MESSAGE.
         END.
         ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND BDest where recid(BDest) = memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */

      /* Haku 1 */
      ELSE IF lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
         cfc = "puyr". RUN Syst/ufcolor.p.
         haku = "".
         ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.

         DISPLAY lcBrand WITH FRAME hayr.
         UPDATE lcBrand WHEN gcAllBrand
                haku WITH FRAME hayr.
         HIDE FRAME hayr no-pause.

         if haku <> "" THEN DO:
            FIND FIRST BDest USE-INDEX BDest where
                       BDest.Brand = lcBrand AND
                       BDest.BDest = INPUT haku
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE BDest THEN 
              FIND FIRST BDest where
                         BDest.Brand = lcBrand AND
                         BDest.BDest ge INPUT haku
              NO-LOCK NO-ERROR.
            IF NOT AVAILABLE BDest THEN 
              FIND FIRST BDest where
                         BDest.Brand = lcBrand AND
                         BDest.BDest le INPUT haku
              NO-LOCK NO-ERROR.
            IF NOT fRecFound(1) THEN NEXT BROWSE.      
          
            NEXT LOOP.
         END.
      END. /* Haku sar. 1 */

      ELSE IF lookup(nap,"2,f2") > 0 THEN DO:  /* haku nimellA */
         cfc = "puyr". RUN Syst/ufcolor.p.
         hakunimi = "".
         ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
         DISPLAY lcBrand WITH FRAME hayr3.
         UPDATE lcBrand WHEN gcAllBrand
                hakunimi WITH FRAME hayr3.
         HIDE FRAME hayr no-pause.

         if hakunimi <> "" THEN DO:
            FIND FIRST BDest USE-INDEX BDName where
                       BDest.Brand      = lcBrand AND
                       BDest.BDName >= hakunimi
            NO-LOCK NO-ERROR.

            IF NOT fRecFound(2) THEN NEXT BROWSE.

            NEXT LOOP.
         END.
      END. /* Haku sar. 2 */

      ELSE IF lookup(nap,"7,f7") > 0 THEN DO:  /* rating */

         FIND FIRST BDest WHERE
              RECID(BDest) = rtab[frame-line(sel)] NO-LOCK NO-ERROR.

         RUN Mc/rateccn.p(BDest.BDestID).

         ASSIGN 
            memory = recid(BDest)
            ufkey  = TRUE. 
      END.

      /* translations */
      ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
         FIND BDest where recid(BDest) = rtab[FRAME-LINE] NO-LOCK.
         RUN Mc/invlang.p(2,STRING(BDest.BDestID)).
         ufkey = TRUE.
         NEXT LOOP.
      END.


      ELSE IF lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
         must-add = TRUE.
         NEXT LOOP.
      END.

      ELSE IF lookup(nap,"6,f6") > 0 AND lcRight = "RW"
      THEN DO TRANSAction:  /* removal */

        delline = FRAME-LINE.
        FIND BDest where recid(BDest) = rtab[FRAME-LINE] NO-LOCK.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) 
           BDest.BDest BDest.BDName    BDest.Brand.

        IF order = 1 THEN FIND NEXT BDest where
           BDest.Brand = lcBrand NO-LOCK NO-ERROR.
        ELSE IF order = 2 THEN FIND NEXT BDest where
           BDest.Brand = lcBrand 
           USE-INDEX BDName NO-LOCK NO-ERROR.

        IF AVAILABLE BDest THEN memory = recid(BDest).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND BDest where recid(BDest) = rtab[FRAME-LINE] NO-LOCK.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev BDest where
              BDest.Brand = lcBrand NO-LOCK NO-ERROR.
           ELSE IF order = 2 THEN FIND prev BDest where
              BDest.Brand = lcBrand 
              USE-INDEX BDName NO-LOCK NO-ERROR.

           IF AVAILABLE BDest THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(BDest).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND BDest where recid(BDest) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        MESSAGE "ARE YOU SURE YOU WANT TO REMOVE (Y/N)" UPDATE ok.
        COLOR DISPLAY value(ccc) 
           BDest.BDest BDest.BDName  BDest.Brand.
        IF ok THEN DO:

            FOR EACH RateCCN EXCLUSIVE-LOCK WHERE
                     RateCCN.BDestID = BDest.BDestID:
               DELETE RateCCN.
            END. 

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent ( lhBDest ).

            DELETE BDest.

            /* if the LAST record was deleted */
            IF NOT can-find(FIRST BDest WHERE 
                BDest.Brand = lcBrand) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-MESSAGE.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
      END. /* removal */

      ELSE IF lookup(nap,"enter,return") > 0 THEN DO WITH FRAME lis TRANS:
         cfc = "lis". RUN Syst/ufcolor.p.
         /* change */
         fr-header = " CHANGE ".
         FIND BDest where recid(BDest) = rtab[frame-line(sel)] NO-lock.

         FIND CCN NO-LOCK WHERE 
              CCN.Brand = BDest.Brand AND
              CCN.CCN   = BDest.CCN NO-ERROR.
         IF AVAILABLE CCN THEN DISPLAY CCN.CCNName WITH FRAME lis.
         ELSE DISPLAY "" @ CCN.CCNName WITH FRAME lis.
                    
         IF llDoEvent THEN RUN StarEventSetOldBuffer (lhBDest).

         RUN local-update-record.
           
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
            KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

         IF llDoEvent THEN RUN StarEventMakeModifyEvent (lhBDest).

         xrecid = recid(BDest).
         HIDE FRAME lis no-pause.

         DISPLAY
            BDest.BDName 
            BDest.CCN
            BDest.ToDate
         WITH FRAME sel.
      END.

      ELSE IF lookup(nap,"home,h") > 0 THEN DO:
         IF order = 1 THEN FIND FIRST BDest  where
            BDest.Brand = lcBrand NO-LOCK NO-ERROR.
         ELSE IF order = 2 THEN FIND FIRST BDest where
            BDest.Brand = lcBrand       
            USE-INDEX BDName NO-LOCK NO-ERROR.
         ASSIGN memory = recid(BDest) must-print = TRUE.
         NEXT LOOP.
      END.

      ELSE IF lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
         IF order = 1 THEN FIND LAST BDest 
         where BDest.Brand = lcBrand NO-LOCK NO-ERROR.
         ELSE IF order = 2 THEN FIND LAST BDest 
         where BDest.Brand = lcBrand USE-INDEX BDName
         NO-LOCK NO-ERROR.
         ASSIGN memory = recid(BDest) must-print = TRUE.
         NEXT LOOP.
      END.
      ELSE IF lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

   END.  /* BROWSE */
   
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

fCleanEventObjects().


PROCEDURE local-disp-row:

   lcDestType = fDestType(BDest.DestType).
   
   DISP 
      BDest.Brand
      BDest.BDest 
      BDest.BDName 
      BDest.CCN
      BDest.ToDate
      lcDestType 
   WITH FRAME sel.
 
END PROCEDURE.

PROCEDURE local-update-record:

   DEF VAR llBDestTrans AS LOG NO-UNDO. 

   llBDestTrans = CAN-FIND(FIRST BDestTrans WHERE
                                 BDestTrans.BDestId = BDest.BDestId).

   BDestUpdate:
   REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

      lcDestType = fDestType(BDest.DestType).
      lcClass = fBDestClass(BDest.Class).
      DISP 
         BDest.Brand
         BDest.BDestID 
         BDest.BDest
         BDest.BDName 
         BDest.CCN 
         BDest.DestType lcDestType 
         BDest.Class lcClass 
         BDest.FromDate
         BDest.ToDate
         WITH FRAME lis.
 
      IF NOT NEW BDest THEN DO:
         ASSIGN 
            ufk    = 0 
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[4] = 9843 WHEN llBDestTrans
            ufk[8] = 8
            ehto   = 0
            ufkey  = TRUE.
           
         RUN Syst/ufkey.p.
      END.
      ELSE toimi = 1.
        
      IF toimi = 1 THEN DO:
        
         FIND CURRENT BDest EXCLUSIVE-LOCK.

         ASSIGN ufkey = TRUE ehto = 9.
         RUN Syst/ufkey.p.
   
         UPDATE
            BDest.BDName WHEN NOT NEW BDest
            BDest.DestType WHEN NOT NEW BDest
            BDest.CCN 
            BDest.Class
            BDest.FromDate
            BDest.ToDate
         WITH FRAME lis EDITING:
               
            READKEY. nap = KEYLABEL(LASTKEY).
               
            IF nap = "F9" AND FRAME-FIELD = "Class" THEN DO: 

               RUN Help/h-tmscodes.p(INPUT "BDest",  /* TableName*/
                                    "BDestClass", /* FieldName */
                                    ?, /* GroupCode */
                              OUTPUT lcCode).

               IF lcCode ne "" AND lcCode NE ? THEN 
                  DISPLAY INTEGER(lcCode) ;& BDest.Class WITH FRAME lis.

               ehto = 9.
               RUN Syst/ufkey.p.
               NEXT. 
            END.
            
            ELSE IF LOOKUP(nap,poisnap) > 0 THEN DO:
   
               PAUSE 0 no-MESSAGE.

               IF FRAME-FIELD = "DestType" THEN DO:
                  lcDestType = fDestType(INPUT INPUT FRAME lis BDest.DestType).
                  IF lcDestType = "" THEN DO:
                     MESSAGE "Unknown destination type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  DISP lcDestType.
               END.
               
               ELSE IF FRAME-FIELD = "Class" THEN DO:
                  lcClass = fBDestClass(INPUT INPUT FRAME lis BDest.Class).
                  IF lcClass = "" THEN DO:
                     MESSAGE "Unknown class"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  DISP lcClass.
               END.

               ELSE IF FRAME-FIELD = "CCN" THEN DO:
                  FIND CCN NO-LOCK WHERE 
                       CCN.Brand = BDest.Brand AND
                       CCN.CCN   = INPUT BDest.CCN NO-ERROR.
                  IF NOT AVAILABLE CCN THEN DO:
                     MESSAGE "Unknown CCN" 
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
                  DISPLAY CCN.CCNName WITH FRAME lis.
               END.
            END.
         
            APPLY LASTKEY.
         END. /* EDITING */
      
         IF NEW BDest THEN LEAVE BDestUpdate.
      END.
      ELSE IF toimi = 4 THEN DO:
         RUN Mc/bdesttrans.p(Bdest.BdestId).
      END.

      ELSE IF toimi = 8 THEN LEAVE.
   END.
 
END PROCEDURE.


