/* -----------------------------------------------
  MODULE .......: NNPLYP.P
  FUNCTION .....: UPDATE of Price List Headers
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 05-10-97
  changePVM ....: 30.04.98 kl added Currency ( Currency code )
                  13.05.98 pt F4: copy contents of other p.l.
                              F6: DELETE Tariff records
                  03.02.98 kl UPDATE ROWID VALUE when copying
                  11.03.99 kl UPDATE FromDate when copying
                  08.04.99 kl CHOOSE valid from Date + BillCode
                  27.04.99 kl because of dates Price list can be copied
                              into itself using different dates !
                  18.05.99 jp uright1 & uright2 added  
                  08.07.99 kl DECIMAL PARAMETER
                  27.09.99 pt more digits FOR ROWID FIELD in copy section
                  08.02.00 kl sequence PriceId
                  13.07.01 kl PriceList.InclVAT
                  30.04.02 tk eventlogging added
                  21.05.02 tk invoice texts
                  10.09.02 jp validation....
                  23.09.02/aam Tariff.CCN instead of Tariff.BDest
                  24.09.02/jr  removed invtext 
                  18.10.02/aam show rates (F4)
                  21.02.03/aam Prefix and DedicList 
                  26.02.03/tk  tokens
                  18.03.03/tk  frame title changed
                  20.03.03/aam one parameter added for tariff.p
                  04.04.03 kl  RUN Mc/tariff, new parameter
                  08.09.03/aam brand
                  22.10.03/jp  f3 - copy rates  added validto replace billcode 
                  04.04.06 jp  f3 - fcopypricelist
                  04.04.06 jp  frame sel new layout

  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable PriceList

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'pricelist'}

{Syst/eventval.i}
{Func/fpricelistcopy.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhPriceList AS HANDLE NO-UNDO.
   lhPriceList = BUFFER PriceList:HANDLE.   
   RUN StarEventInitialize(lhPriceList).

   DEFINE VARIABLE lhTariff AS HANDLE NO-UNDO.
   lhTariff = BUFFER Tariff:HANDLE.
   RUN StarEventInitialize(lhTariff).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhPriceList).
   END.

END.


DEF BUFFER fplist FOR PriceList.
DEF BUFFER copy-rate  FOR Tariff.


IF llDoEvent THEN DO:
  DEFINE VARIABLE lhCopy-rate AS HANDLE NO-UNDO.
  lhCopy-rate = BUFFER copy-rate:HANDLE.
  RUN StarEventInitialize(lhCopy-rate).
END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR PriceList LIKE PriceList.PriceList NO-UNDO FORMAT "X(18)".
DEF VAR PLName    LIKE PriceList.PLName    NO-UNDO.
DEF VAR FromDate  LIKE Tariff.ValidFrom    NO-UNDO.
DEF VAR ToDate    LIKE tariff.ValidTo      NO-UNDO.
DEF VAR BillCode  LIKE BillItem.BillCode   NO-UNDO.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR firstline    AS INT                    NO-UNDO  init 0.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR ordercount   AS INT                    NO-UNDO  init 2.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delline      AS INT                    NO-UNDO  init 0.
DEF VAR ex-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
def var line         as int format "99"        NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR fr-header    AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.
def var vdate        as da  format "99-99-99"  NO-UNDO.
DEF VAR llCopyPriceList AS LOG                 NO-UNDO.

form
    PriceList.PriceList    FORMAT "X(18)" 
    PriceList.PLName       format "x(30)"
    PriceList.Currency     column-label "Curr"
    PriceList.CurrUnit     column-label "Unit"
    PriceList.InclVAT      column-label "VAT"
    PriceList.Rounding     column-label "D"
    PriceList.Memo     format "x(9)"
WITH width 80 OVERLAY scroll 1 15 DOWN
    color value(cfc) title color value(ctc) " " + ynimi +
    " PRICE LISTS " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    PriceList.Brand       LABEL "Brand ........." SKIP
    PriceList.PriceList   LABEL "Price List ...." SKIP
    PriceList.PLName      LABEL "Name of List .." 
       VALIDATE(PriceList.PLName ne "","Missing Pricelist name")  SKIP
    PriceList.Prefix      LABEL "Prefix ........"
       VALIDATE(INPUT PriceList.Prefix = "" OR 
                CAN-FIND(FIRST RatePref WHERE
                               RatePref.Brand  = gcBrand AND 
                               RatePref.RatePref = INPUT PriceList.Prefix),
                "Unknown prefix") 
       SKIP
    PriceList.DedicList   LABEL "Dedic./General " SKIP
    PriceList.Currency    LABEL "Currency code ." 
       VALIDATE(pricelist.currency ne "","Missing currency code!") 
       Currency.Currname     NO-LABEL                SKIP
    PriceList.CurrUnit    LABEL "Currency unit ." SKIP
    PriceList.InclVAT     LABEL "VAT Usage ....." 
       FORMAT "Including/Excluding" SKIP
    PriceList.Rounding    LABEL "No.of Decimals " SKIP
    PriceList.Memo        LABEL "Memo .........." SKIP

    WITH  OVERLAY ROW 3 centered
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 
    FRAME lis.

{Func/brand.i}

form /* Price List search WITH FIELD PriceList */
    "Brand:" lcBrand skip
    "Code :" PriceList
    help "Give pricelist's code"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f1.

form /* Price List search WITH FIELD PLName */
    "Brand:" lcBrand skip
    "Name :" PLName
    help "Give pricelist's name"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME f2.

form /* FOR copying */
    skip(1)
    "Instruction:  Now we copy entries from an existing Price list" SKIP
    "              into this Price list."                              skip(1)
    "              FROM Price list:" PriceList NO-LABEL
    help "Code of Price list FROM where entries are to be copied"
    fpList.PLName no-label format "x(22)"                              SKIP
    "              Dates ..........:" FromDate NO-LABEL
    help "Prices from which VALID FROM Date ?"          
       " - " ToDate No-label 
    HELP " Prices To which VALID From Date?"                             SKIP(1)
    "              INTO Price list:" PriceList.PriceList NO-LABEL
    PriceList.PLName no-label format "x(22)"                           SKIP
    "              Valid FROM ....:" vdate NO-LABEL
    help "From which Date are the prices valid ?" skip(1)
    "              Duplicate entries (that already exist) will"        SKIP
    "              be skipped."                                     skip(1)

with centered overlay row 3 title " COPY ENTRIES INTO " + PriceList.PriceList +
    " FROM OTHER Price LIST " FRAME plcopy.


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

FIND FIRST PriceList
WHERE PriceList.Brand = lcBrand no-lock no-error.
IF AVAILABLE PriceList THEN ASSIGN
   memory       = recid(PriceList)
   must-print = TRUE
   must-add    = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No pricelists available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN
      memory       = ?
      must-print = FALSE
      must-add    = TRUE.
END.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 34 "   By Code   ".
       if order = 2 then put screen row 19 col 34 "   By Name   ".
    END.

   IF must-add THEN DO:  /* PriceList -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.

         DO TRANSAction:
            DISPLAY lcBrand @ PriceList.Brand.
            PROMPT-FOR PriceList.PriceList FORMAT "X(18)"
            VALIDATE
               (PriceList.PriceList = "" OR
               NOT can-find(PriceList WHERE
                            PriceList.Brand = lcBrand AND
                            PriceList.PriceList = INPUT PriceList.PriceList),
               "Price List " + string(INPUT PriceList.PriceList) +
               " already exists !").
            if input frame lis PriceList.PriceList = "" THEN LEAVE add-new.
            CREATE PriceList.
            ASSIGN
            PriceList.Brand     = lcBrand
            PriceList.PriceList = INPUT FRAME lis PriceList.PriceList.

            UPDATE PriceList.PLName
                   PriceList.Prefix
                   PriceList.DedicList
                   PriceList.Currency
                   PriceList.CurrUnit
                   PriceList.InclVAT
                   PriceList.Rounding           
                   PriceList.Memo

            WITH FRAME LIS
            EDITING:
               READKEY.
               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME LIS:
                  HIDE MESSAGE.
                  IF FRAME-FIELD = "Currency" THEN DO:
                     FIND Currency WHERE 
                          currency.currency = input pricelist.currency
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL Currency THEN DO:
                        BELL.
                        MESSAGE "Unknown currency code !".
                        NEXT.
                     END.
                     DISP Currency.CurrName.
                  END.

                  ELSE IF FRAME-FIELD = "Prefix" AND
                     INPUT FRAME lis PriceList.Prefix NE "" 
                  THEN DO:
                     FIND FIRST RatePref WHERE 
                          RatePref.Brand    = lcBrand AND 
                          RatePref.RatePref = INPUT PriceList.Prefix
                     NO-LOCK NO-ERROR.
                     IF NOT AVAIL RatePref THEN DO:
                        BELL.
                        MESSAGE "Unknown prefix !".
                        NEXT.
                     END.
                  END.
                END.

                APPLY LASTKEY.
            END.

            ASSIGN
            memory = recid(PriceList)
            xrecid = memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhPriceList).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST PriceList
      WHERE PriceList.Brand = lcBrand no-lock no-error.
      IF NOT AVAILABLE PriceList THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND PriceList where recid(PriceList) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE PriceList THEN DO:

               DISPLAY 
                  PriceList.PriceList FORMAT "X(20)"
                  PriceList.PLName
                  PriceList.Currency
                  PriceList.CurrUnit
                  PriceList.InclVAT
                  PriceList.Rounding
                  PriceList.Memo.
               rtab[FRAME-LINE] = recid(PriceList).
               IF order = 1 THEN FIND NEXT PriceList
               WHERE PriceList.Brand = lcBrand no-lock no-error.
               ELSE IF order = 2 THEN FIND NEXT PriceList USE-INDEX PLName
               WHERE PriceList.Brand = lcBrand no-lock no-error.
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
         ufk[1]= 35  ufk[2]= 30 
         ufk[3]= (IF lcRight = "RW" THEN 955 ELSE 0)
         ufk[4]= 878
         ufk[5]= (IF lcRight = "RW" THEN 5   ELSE 0)
         ufk[6]= (IF lcRight = "RW" THEN 4   ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW PriceList.PriceList ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) PriceList.PriceList WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW PriceList.PLName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) PriceList.PLName WITH FRAME sel.
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
         FIND PriceList where recid(PriceList) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev PriceList
            WHERE PriceList.Brand = lcBrand no-lock no-error.
            ELSE IF order = 2 THEN FIND prev PriceList USE-INDEX PLName
            WHERE PriceList.Brand = lcBrand no-lock no-error.
            IF AVAILABLE PriceList THEN
               ASSIGN firstline = i memory = recid(PriceList).
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
            FIND PriceList where recid(PriceList) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev PriceList
            WHERE PriceList.Brand = lcBrand no-lock no-error.
            ELSE IF order = 2 THEN FIND prev PriceList USE-INDEX PLName
            WHERE PriceList.Brand = lcBrand no-lock no-error.
            IF NOT AVAILABLE PriceList THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY 
                  PriceList.PriceList FORMAT "X(18)"
                  PriceList.PLName
                  PriceList.Currency
                  PriceList.CurrUnit
                  PriceList.InclVAT
                  PriceList.Rounding
                  PriceList.Memo.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(PriceList)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND PriceList where recid(PriceList) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT PriceList
            WHERE PriceList.Brand = lcBrand no-lock no-error.
            ELSE IF order = 2 THEN FIND NEXT PriceList USE-INDEX PLName
            WHERE PriceList.Brand = lcBrand no-lock no-error.
            IF NOT AVAILABLE PriceList THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY 
               
                  PriceList.PriceList FORMAT "X(18)"

                  PriceList.PLName

                  PriceList.Currency
                  PriceList.CurrUnit
                  PriceList.InclVAT
                  PriceList.Rounding
                  PriceList.Memo.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(PriceList).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND PriceList where recid(PriceList) = memory no-lock no-error.
         IF order = 1 THEN FIND prev PriceList
         WHERE PriceList.Brand = lcBrand no-lock no-error.
         ELSE IF order = 2 THEN FIND prev PriceList USE-INDEX PLName
         WHERE PriceList.Brand = lcBrand no-lock no-error.
         IF AVAILABLE PriceList THEN DO:
            memory = recid(PriceList).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev PriceList
               WHERE PriceList.Brand = lcBrand no-lock no-error.
               ELSE IF order = 2 THEN FIND prev PriceList USE-INDEX PLName
               WHERE PriceList.Brand = lcBrand no-lock no-error.
               IF AVAILABLE PriceList THEN memory = recid(PriceList).
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
            FIND PriceList where recid(PriceList) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.
        PriceList = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME F1.
        UPDATE lcBrand WHEN gcAllBrand
               PriceList WITH FRAME f1.
        HIDE FRAME f1 no-pause.

        if PriceList <> "" THEN DO:

           FIND FIRST PriceList WHERE 
              PriceList.Brand = lcBrand AND
              PriceList.PriceList >= PriceList
              no-lock no-error.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     else if lookup(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". RUN Syst/ufcolor.
        PLName = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME F1.
        UPDATE lcBrand WHEN gcAllBrand
               PLName WITH FRAME f2.
        HIDE FRAME f2 no-pause.

        if PLName <> "" THEN DO:
           FIND FIRST PriceList USE-INDEX PLName WHERE 
              PriceList.Brand = lcBrand AND
              PriceList.PLName >= PLName
           no-lock no-error.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     if lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"3,f3") > 0 AND lcRight = "RW" 
     THEN DO WITH FRAME plcopy:

        PAUSE 0.
        assign ufkey = true ehto = 9 PriceList = "". RUN Syst/ufkey.

        FIND PriceList where recid(PriceList) = rtab;<frame-line(sel);> no-lock.
        
        DISP PriceList.PriceList PriceList.PLName.

        MESSAGE 
        "DO you want to copy pricelist " pricelist.pricelist "->"                                                       pricelist.pricelist
                                       + "_" + STRING(year(Today),"9999") 
                                             + STRING(Month(Today),"99") 
                                             + STRING(Day(Today),"99")
        VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE llCopyPriceList      .
        
        IF llCopyPriceList THEN DO:
           fCopyPriceList(INPUT pricelist.pricelist, INPUT FALSE).
           LEAVE.
        END.
        
        IF CAN-FIND(FIRST Tariff where 
                          Tariff.Brand     = PriceList.Brand AND
                          Tariff.PriceList = PriceList.PriceList) 
        THEN DO:                          
           BELL. 
           message "Warning: there are already entries in Price list"
           PriceList.PriceList "!".
        END.
        else message "No entries found yet in Price list" PriceList.PriceList.

        vdate = pvm.
        update PriceList validate(input PriceList = "" OR
                                can-find(fplist where 
                                         fplist.PriceList = INPUT PriceList),
                                         "Unknown Price list code !").

        if input PriceList = "" THEN DO:
           HIDE FRAME plcopy.
           NEXT LOOP.                                
        END.
        if PriceList ne "" THEN DO:
           FIND fPList where 
                fPList.Brand     = lcBrand AND
                fpList.PriceList = PriceList 
           no-lock.
           DISP fPList.PLName.

        UPDATE
           FromDate
           ToDate
           vdate
        WITH FRAME plcopy EDITING:
           READKEY.
           IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 THEN DO:
              HIDE FRAME plcopy.
              UNDO, LEAVE.
           END.
           APPLY LASTKEY.
        END.

           BELL.
           ok = FALSE.
           message "Are You SURE You want to copy" PriceList "INTO" 
           PriceList.PriceList VIEW-AS ALERT-BOX BUTTONS YES-NO UPDATE ok.
           IF ok THEN DO:
              i = 0.

              COPY:
              FOR EACH Tariff no-lock where
                       Tariff.Brand      = lcBrand   AND
                       Tariff.PriceList  = PriceList AND
                       Tariff.ValidFrom >= FromDate  AND
                       Tariff.ValidFrom <= ToDate    AND 
                       Tariff.CustNum   = 0.

                 /* SKIP entries that already exist */         
                 IF can-find(FIRST copy-rate where
                                   copy-rate.Brand     = lcBrand AND 
                                   copy-rate.CCN       = Tariff.CCN AND
                                   copy-rate.PriceList = PriceList.PriceList AND
                                   copy-rate.ValidFrom = vdate               AND
                                   copy-rate.CustNum   = 0)
                 THEN NEXT.

                 CREATE copy-rate.
                 BUFFER-COPY tariff TO copy-rate
                    ASSIGN
                       copy-rate.ValidFrom = vdate
                       copy-rate.PriceList = PriceList.PriceList
                       copy-rate.TariffNum = next-value(Tariff)
                       i                   = i + 1.

              END.
              message "Totally" i "entries copied from" PriceList "into"
              PriceList.PriceList "with StartDate"
              STRING(vdate,"99.99.9999")
              VIEW-AS ALERT-BOX TITLE " COPY COMPLETE ".
              PAUSE no-message.
           END. /* OK */   
        END.
        CLEAR FRAME plcopy no-pause.
        HIDE FRAME plcopy no-pause.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO:  /* tariffs */
        FIND PriceList where recid(PriceList) = rtab;<frame-line(sel);> 
            no-lock no-error.

        IF AVAILABLE PriceList THEN DO:
           RUN Mc/tariff(0,0,PriceList.PriceList,0,"",0). 
           UFKEY = TRUE.
        END.
     end. 


     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* removal */

        delline = FRAME-LINE.
        FIND PriceList where recid(PriceList) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc)
           PriceList.PriceList FORMAT "X(18)" 

           PriceList.PLName 

           PriceList.Currency
           PriceList.CurrUnit
           PriceList.InclVAT
           PriceList.Rounding
           PriceList.Memo.

        IF order = 1 THEN FIND NEXT PriceList
        WHERE PriceList.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND NEXT PriceList USE-INDEX PLName
        WHERE PriceList.Brand = lcBrand no-lock no-error.
        IF AVAILABLE PriceList THEN memory = recid(PriceList).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND PriceList where recid(PriceList) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           IF order = 1 THEN FIND prev PriceList
           WHERE PriceList.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev PriceList USE-INDEX PLName
           WHERE PriceList.Brand = lcBrand no-lock no-error.
           IF AVAILABLE PriceList THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(PriceList).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND PriceList where recid(PriceList) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        MESSAGE 
           "Are you SURE You want to delete Price list and its entries (Y/N) ?"
           UPDATE ok.
        COLOR DISPLAY value(ccc)
           PriceList.PriceList FORMAT "X(18)"
           PriceList.PLName 
           PriceList.Currency
           PriceList.CurrUnit
           PriceList.InclVAT
           PriceList.Rounding
           PriceList.Memo.
        IF ok THEN DO:
            i = 0.
            FOR EACH Tariff where 
               Tariff.Brand     = PriceList.Brand AND
               Tariff.PriceList = PriceList.PriceList.

               IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhTariff).

               DELETE Tariff.
               i = i + 1.
            END.   
            IF i > 0 THEN DO:
               message "Price list" PriceList.PriceList "with" i
               "entries was deleted - press ENTER !".
               PAUSE no-message.
            END.

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhPriceList).

            DELETE PriceList.
            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST PriceList
            WHERE PriceList.Brand = lcBrand) THEN DO:
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

        FIND PriceList where recid(PriceList) = rtab[frame-line(sel)]
        exclusive-lock.
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN Syst/ufkey.
        cfc = "lis". RUN Syst/ufcolor.
        DISPLAY 
           PriceList.Brand
           PriceList.PriceList FORMAT "X(18)"
           PriceList.PLName

           PriceList.DedicList
           PriceList.Currency
           PriceList.CurrUnit
           PriceList.InclVAT
           PriceList.Rounding
           PriceList.Memo.

        IF lcRight = "RW" THEN DO:   

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhPriceList).

           FIND Currency WHERE Currency.Currency = PriceList.Currency 
              NO-LOCK NO-ERROR.
           IF AVAILABLE Currency THEN DISPLAY Currency.CurrName.
           ELSE DISPLAY "" @ Currency.CurrName.

           UPDATE 
              PriceList.PLName
              PriceList.Prefix
              PriceList.DedicList
              PriceList.Currency
              PriceList.CurrUnit
              PriceList.InclVAT
              PriceList.Rounding
              PriceList.Memo 
           EDITING:

              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME LIS:
                 HIDE MESSAGE.
                 IF FRAME-FIELD = "Currency" THEN DO:
                    FIND Currency WHERE 
                         currency.currency = input pricelist.currency
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL Currency THEN DO:
                       BELL.
                       MESSAGE "Unknown currency code !".
                       NEXT.
                    END.
                    DISP Currency.CurrName.
                 END.

                 ELSE IF FRAME-FIELD = "Prefix" AND
                    INPUT FRAME lis PriceList.Prefix NE "" 
                 THEN DO:
                    FIND FIRST RatePref WHERE 
                         RatePref.Brand    = PriceList.Brand AND
                         RatePref.RatePref = input pricelist.Prefix
                    NO-LOCK NO-ERROR.
                    IF NOT AVAIL RatePref THEN DO:
                       BELL.
                       MESSAGE "Unknown prefix !".
                       NEXT.
                    END.
                 END.
              END.

              APPLY LASTKEY.

           END. 

           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhPriceList).

        END.
        ELSE PAUSE.
        HIDE FRAME lis no-pause.
        DISPLAY 
           PriceList.PLName

           PriceList.Currency
           PriceList.InclVAT
           PriceList.Rounding
           PriceList.Memo
        WITH FRAME sel.
        xrecid = recid(PriceList).


     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST PriceList
        WHERE PriceList.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST PriceList USE-INDEX PLName
        WHERE PriceList.Brand = lcBrand no-lock no-error.
        ASSIGN memory = recid(PriceList) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST PriceList
        WHERE PriceList.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST PriceList USE-INDEX PLName
        WHERE PriceList.Brand = lcBrand no-lock no-error.
        ASSIGN memory = recid(PriceList) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

