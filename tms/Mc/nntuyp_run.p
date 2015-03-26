/* --------------------------------------------------------------------------
  MODULE .......: NNTUYP.P
  FUNCTION .....: Maintain BillCode data
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 12-08-96
  MODIFIED .....: 20.07.01 kl: RepText instead of tunimi
                  13.02.02 jp: BillCode FORMAT x(16)
                  16.05.02 aam InvSect AND ISOrder added 
                  16.05.02/tk Event logging added
                  20.05.02/tk invoice texts
                  22.07.02/tk show full page on "end"
                  29.07.02 lp F2 -> Find Name(was find name)
                  23.08.02/tk use TB2AccNum for epl-form
                              -> dont update cost accounts
                              update VatCode
                  05.09.02 jp validation
                  26.02.03 tk tokens
                  28.02.03 aam epl-form removed 
                  05.09.03 aam brand
                  08.01.04 aam EUAccNum, FSAccNum, EUVatCode added,
                               sections removed
                  12.02.04/aam Accounts to 6 digits
                  28.09.04/aam AltAccNum
                  04.05.05/aam use DispMPM for "Disp MPM"
                  30.11.05/aam longer format for name
                  13.11.06/aam TaxClass instead of VatCode
                  14.11.06/aam account with 8 digits
                  21.11.06/aam SAPRid, VipAccNum,
                               translations (invlang)
                  
  Version ......: yoigo
  -------------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable BillItem

{commali.i}
{eventval.i}
{lib/tokenlib.i}
{lib/tokenchk.i 'billitem'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhBillItem AS HANDLE NO-UNDO.
   lhBillItem = BUFFER BillItem:HANDLE.
   RUN StarEventInitialize(lhBillItem).

   ON F12 ANYWHERE DO:
      RUN eventview2.p(lhBillItem). 
   END.

END.

DEFINE INPUT PARAMETER icBGroup AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER icUpdateListMode AS CHAR NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

def var ok         as lo format "Yes/No" NO-UNDO.
DEF VAR haku  LIKE BillItem.BillCode  NO-UNDO.
DEF VAR haku2 LIKE BillItem.BIName NO-UNDO.
DEF VAR BIGroup LIKE BItemGroup.BIGroup NO-UNDO.
DEF VAR firstline  AS i  NO-UNDO.
DEF VAR AccName    AS c  NO-UNDO.
DEF VAR BIGName    AS c  NO-UNDO.
DEF VAR order      AS i  NO-UNDO.
DEF VAR ex-order   AS i  NO-UNDO.
DEF VAR memory     AS re NO-UNDO.
def var line       as i  format "99"    NO-UNDO.
DEF VAR delline    AS i         NO-UNDO.
DEF VAR must-print AS lo        NO-UNDO.
DEF VAR must-add   AS lo        NO-UNDO.
DEF VAR ufkey      AS lo        NO-UNDO.
DEF VAR fr-header  AS c.
DEF VAR rtab       AS RECID EXTENT 24      NO-UNDO.
DEF VAR i          AS i  NO-UNDO.
DEF VAR xrecid     AS re.
DEF VAR endloop    AS I NO-UNDO.

DEF VAR lcAltName   AS CHAR NO-UNDO.
DEF VAR lcVIPName   AS CHAR NO-UNDO.
DEF VAR lcEUConName AS CHAR NO-UNDO.
DEF VAR lcEUAccName AS CHAR NO-UNDO.
DEF VAR lcFSAccName AS CHAR NO-UNDO.
DEF VAR lcEuVATName AS CHAR NO-UNDO.
DEF VAR llCanDelete AS LOG  NO-UNDO INITIAL TRUE.

/* variables used in case of CCAdminTool */
DEF VAR liccAcount AS INT NO-UNDO.
DEF VAR lcCCSAPRId AS CHAR NO-UNDO.
IF icUpdateListMode = "update-mode-cc" THEN DO:

  llCanDelete = FALSE.

  FIND TMSParam WHERE TMSParam.Brand = gcBrand AND
                      TMSParam.ParamGroup = "CCAdminTool" AND
                      TMSParam.ParamCode = "BillItemAccount" NO-LOCK NO-ERROR.
  IF AVAIL TMSParam THEN liccAcount = TMSParam.IntVal.
 
  FIND TMSParam WHERE TMSParam.Brand = gcBrand AND
                      TMSParam.ParamGroup = "CCAdminTool" AND
                      TMSParam.ParamCode = "BillItemSAPRId" NO-LOCK NO-ERROR.
  IF AVAIL TMSParam THEN lcCCSAPRId = TMSParam.CharVal.

END.


form
    BillItem.Brand      FORMAT "X(4)" COLUMN-LABEL "Bran"
    BillItem.BillCode   format "x(16)"
    BillItem.BIName     format "x(17)"
    BillItem.BIGroup    column-label "Group"
          BIGName       column-label "GroupName" format "x(11)"
    BillItem.AccNum     column-label "Acct"   help "Account number"
                        FORMAT ">>>>>>>9"
    BillItem.TaxClass   COLUMN-LABEL "TaxClass"
WITH width 80 OVERLAY ROW 1 scroll 1 15 DOWN COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " Billing Items " + string(pvm,"99-99-99") + " "
    FRAME sel.

form
    BillItem.Brand     label "Brand ....." SKIP
    BillItem.BillCode  label "BillItem .." format "x(16)"               SKIP
    BillItem.BIName    label "Name ......"                                   
           FORMAT "X(45)" 
           VALIDATE(input billitem.biname ne "","Missing Billing Item Name!")
           SKIP
    BillItem.BIGroup   label "Group ....." 
       BItemGroup.BIGName AT 23 NO-LABEL  SKIP
    BillItem.AccNum    label "Account ..." 
           FORMAT ">>>>>>>9"
           Account.AccName    AT 23 NO-LABEL  SKIP
    BillItem.AltAccNum label "Own Use ..." 
           FORMAT ">>>>>>>9"
           HELP "Account for own use"   
           lcAltName   AT 23 NO-LABEL FORMAT "X(30)" SKIP
    BillItem.VIPAccNum label "VIP Use ..." 
           FORMAT ">>>>>>>9"
           HELP "Account for VIP customer use"   
           lcVIPName   AT 23 NO-LABEL FORMAT "X(30)" SKIP
    BillItem.EUConAccNum LABEL "EU Consum. "
           FORMAT ">>>>>>>9"
           lcEUConName AT 23 NO-LABEL FORMAT "X(30)" SKIP
    BillItem.EUAccNum  label "EU Account " 
           FORMAT ">>>>>>>9"
           lcEUAccName AT 23 NO-LABEL FORMAT "X(30)" SKIP
    BillItem.FSAccNum  label "FS Account " 
           FORMAT ">>>>>>>9"
           lcFSAccName AT 23 NO-LABEL FORMAT "X(30)" SKIP
    BillItem.InvSect   label "Section ..." 
       InvSect.ISName  AT 23 NO-LABEL  SKIP
    BillItem.TaxClass label "Tax Class ." 
       HELP "Tax class"
       TaxClass.TCName AT 23 NO-LABEL SKIP
    BillItem.SAPRid 
       LABEL "SAP Data .."
       FORMAT "X(3)" 
       SKIP
    BillItem.DispMPM  LABEL "Display MPM"       
       HELP "Display MPM on specification (if not then display '----')"
       FORMAT "Yes/No" SKIP
    BillItem.CostCentre LABEL "Cost Centre" SKIP
WITH  OVERLAY ROW 3 CENTERED COLOR value(cfc) TITLE COLOR value(ctc)
    fr-header WITH side-labels FRAME lis.

{brand.i}

form /* produkt :n tunnuksella hakua varten */
    "Brand:" lcBrand skip
    "Code :" haku
    help "Give Code or its first characters"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* produkt :n nimella hakua varten */
    "Brand:" lcBrand skip
    "Name :" haku2
    help "Give Name or its first characters"
    with row 4 col 2 title color value(ctc) " FIND NAME "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

form /* BillCode group - haku */
    "Brand:" lcBrand skip
    "Group:" BIGroup
    help "Give GroupCode or its first characters"
    with row 4 col 2 title color value(ctc) " FIND GROUP "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku3.

form /* cost accounts */
   "   TB1 Account: " BillItem.TB1AccNum SKIP
   "   TB2 Account: " BillItem.TB2AccNum
WITH
   centered row 7 width 40 no-labels title " Cost accounts: " + 
   BillItem.BillCode + " - " + BillItem.BIName OVERLAY FRAME tbacc.


cfc = "sel". RUN ufcolor. ASSIGN ccc = cfc.
view FRAME sel.

RUN local-find-first.


IF AVAILABLE BillItem THEN 
   ASSIGN 
      memory     = recid(BillItem)
      must-print = TRUE 
      must-add   = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No BillCodes available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ELSE ASSIGN 
      memory     = ? 
      must-print = FALSE 
      must-add   = TRUE.
END.
ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 30 " By Code  ".
       if order = 2 then put screen row 19 col 30 " By Name  ".
       if order = 3 then put screen row 19 col 30 " By Group ".

    END.

   IF must-add THEN DO:  /* BillItem -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD" must-add = FALSE.
      RUN ufcolor.

      add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN ufkey.
         DO TRANSAction:
            DISPLAY lcBrand @ BillItem.Brand.

            PROMPT-FOR BillItem.BillCode
            VALIDATE
               (BillItem.BillCode = "" OR
               NOT can-find(BillItem WHERE 
                    BillItem.Brand = lcBrand AND
                    BillItem.BillCode = INPUT FRAME lis BillItem.BillCode),
               "Item " + string(INPUT BillItem.BillCode) +
               " already exists !").
            if input BillItem.BillCode = "" THEN LEAVE add-new.

            CREATE BillItem.

            ASSIGN
            BillItem.Brand    = lcBrand
            BillItem.BillCode = INPUT FRAME lis BillItem.BillCode
            BillItem.DispMPM = TRUE. 

            RUN VALUE(icUpdateListMode).
            
            ASSIGN
            memory = recid(BillItem)
            xrecid = memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhBillItem).

         LEAVE.

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      FIND FIRST BillItem WHERE BillItem.Brand = lcBrand no-lock no-error.
      IF NOT AVAILABLE BillItem THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND BillItem where recid(BillItem) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE BillItem THEN DO:

               RUN local-find-others.

               DISPLAY BillItem.Brand BillItem.BillCode BillItem.BIName 
                       BillItem.AccNum BillItem.BIGroup
                       BIGName BillItem.TaxClass.
               rtab[FRAME-LINE] = recid(BillItem).
               
               RUN local-find-next.

            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         IF endloop = 0 THEN up FRAME-LINE - 1.
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
         ufk[1]= 35  ufk[2]= 30
         ufk[3]= (IF icBGroup = ? THEN 973 ELSE 0 )  
         ufk[4]= 814
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6]= (IF lcRIght = "RW" AND llCanDelete THEN 4 ELSE 0)
         ufk[7]= 1760 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW BillItem.BillCode ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BillItem.BillCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW BillItem.BIName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BillItem.BIName WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW BillItem.BIGroup ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) BillItem.BIGroup WITH FRAME sel.
      END.


      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 4 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 3.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND BillItem where recid(BillItem) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE BillItem THEN
            ASSIGN firstline = i memory = recid(BillItem).
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
            FIND BillItem where recid(BillItem) = rtab[1] no-lock.
            RUN local-find-prev.

            IF NOT AVAILABLE BillItem THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               RUN local-find-others.


               DISPLAY BillItem.Brand BillItem.BillCode BillItem.BIName    
                       BillItem.AccNum 
                       BillItem.BIGroup
                       BIGName BillItem.TaxClass.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(BillItem)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND BillItem where recid(BillItem) = rtab[FRAME-DOWN] no-lock .
            RUN local-find-next. 
            IF NOT AVAILABLE BillItem THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.

               RUN local-find-others.

               DISPLAY BillItem.Brand BillItem.BillCode BillItem.BIName
                       BillItem.AccNum BillItem.BIGroup
                       BIGName BillItem.TaxClass .
               DO i = 1 TO FRAME-DOWN - 1: rtab[i] = rtab[i + 1]. END.
               rtab[FRAME-DOWN] = recid(BillItem).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND BillItem where recid(BillItem) = memory no-lock no-error.
         RUN local-find-prev.
 
         IF AVAILABLE BillItem THEN DO:
            memory = recid(BillItem).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               
               RUN local-find-prev.

               IF AVAILABLE BillItem THEN memory = recid(BillItem).
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
            FIND BillItem where recid(BillItem) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN ufcolor.
        haku = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr.
        UPDATE lcBrand WHEN gcAllBrand 
               haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.
        if haku <> "" THEN DO:
           FIND FIRST BillItem WHERE 
               BillItem.Brand = lcBrand AND
               BillItem.BillCode >=  haku no-lock no-error.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN ufcolor.
        haku2 = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME hayr2.
        UPDATE lcBrand WHEN gcAllBrand 
               haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if haku2 <> "" THEN DO:
           FIND FIRST BillItem where 
              BillItem.Brand  = lcBrand AND
              BillItem.BIName >= haku2 no-lock no-error.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */

     /* Haku 3 */
     if lookup(nap,"3,f3") > 0 AND icBGroup  = ? THEN DO:  /* haku sarakk. 3 */
        cfc = "puyr". RUN ufcolor.
        BIGroup = "".
        ehto = 9. RUN ufkey. ufkey = TRUE.
        DISPLAY lcBrand WITH FRAME haku3.
        UPDATE lcBrand WHEN gcAllBrand 
               BIGroup WITH FRAME haku3.

        HIDE FRAME haku3 no-pause.
        if BIGroup <> "" THEN DO:
           FIND FIRST BillItem where 
              BillItem.Brand   =  lcBrand AND
              BillItem.BIGroup >= BIGroup no-lock no-error.

           IF NOT fRecFound(3) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 3 */

     /* translations */
     ELSE IF LOOKUP(nap,"4,f4") > 0 AND ufk[4] > 0 THEN DO:  
         FIND BillItem where recid(BillItem) = rtab[FRAME-LINE] NO-LOCK.
         RUN invlang(1,BillItem.BillCode).
         
         ufkey = TRUE.
         NEXT LOOP.
     END.

     ELSE if  lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */
         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" AND llCanDelete
     THEN DO TRANSAction:  /* removal */
        delline = FRAME-LINE.
        FIND BillItem where recid(BillItem) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) 
              BillItem.Brand BillItem.BillCode BillItem.BIName BillItem.AccNum
              BIGName BillItem.TaxClass BillItem.BIGroup.

        RUN local-find-next.

       
        IF AVAILABLE BillItem THEN memory = recid(BillItem).
        ELSE DO:
           /* the one TO be deleted is reread */
           FIND BillItem where recid(BillItem) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           RUN local-find-prev.
           IF AVAILABLE BillItem THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(BillItem).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND BillItem where recid(BillItem) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        MESSAGE 
        "WARNING: YOU SHOULD NEVER DELETE A BillCode THAT EXISTS ON CALLS/INVOICES !".
        message " DO YOU REALLY WANT TO ERASE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc) 
           BillItem.Brand BillItem.BillCode BillItem.BIName BillItem.AccNum
           BIGName BillItem.TaxClass BillItem.BIGroup.
        IF ok THEN DO:

            FOR EACH RepText where 
                     RepText.Brand     = BillItem.Brand AND
                     RepText.Language  = 1 AND
                     RepText.TextType  = 1 AND
                     RepText.LinkCode  = BillItem.BillCode.

               DELETE RepText.

            END.

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent (lhBillItem).

            DELETE BillItem.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST BillItem) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"7,F7") > 0 THEN DO:
        PAUSE 0.
        DO TRANS:
           FIND BillItem where recid(BillItem) = rtab[FRAME-LINE] no-lock.
           RUN invotxt("BillItem",BillItem.BillCode).
        END.
        ufkey = TRUE.
        NEXT LOOP.
     END.


     else if lookup(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSAction:

        /* change */
        FIND BillItem where recid(BillItem) = rtab[frame-line(sel)]
        exclusive-lock.

        ASSIGN lcAltName   = ""
               lcVIPName   = "" 
               lcEUAccName = ""
               lcEUConName = ""
               lcFSAccName = ""
               lcEUVATName = "".
        IF BillItem.AltAccNum > 0 THEN DO:
           FIND Account WHERE 
                Account.Brand  = lcBrand AND
                Account.AccNum = BillItem.AltAccNum 
           NO-LOCK NO-ERROR.
           IF AVAILABLE Account THEN lcAltName = Account.AccName.
        END.
        IF BillItem.VipAccNum > 0 THEN DO:
           FIND Account WHERE 
                Account.Brand  = lcBrand AND
                Account.AccNum = BillItem.VipAccNum 
           NO-LOCK NO-ERROR.
           IF AVAILABLE Account THEN lcVipName = Account.AccName.
        END.
        IF BillItem.EUConAccNum > 0 THEN DO:
           FIND Account WHERE 
                Account.Brand  = lcBrand AND
                Account.AccNum = BillItem.EUConAccNum 
           NO-LOCK NO-ERROR.
           IF AVAILABLE Account THEN lcEuConName = Account.AccName.
        END.
        IF BillItem.EUAccNum > 0 THEN DO:
           FIND Account WHERE 
                Account.Brand  = lcBrand AND
                Account.AccNum = BillItem.EUAccNum 
           NO-LOCK NO-ERROR.
           IF AVAILABLE Account THEN lcEuAccName = Account.AccName.
        END.
        IF BillItem.FSAccNum > 0 THEN DO:
           FIND Account WHERE 
                Account.Brand  = lcBrand AND
                Account.AccNum = BillItem.FSAccNum 
           NO-LOCK NO-ERROR.
           IF AVAILABLE Account THEN lcFSAccName = Account.AccName.
        END.
 
        RUN local-find-others.

        FIND InvSect where 
           InvSect.Brand   = BillItem.Brand AND
           InvSect.InvSect = BillItem.InvSect no-lock no-error.

        FIND TaxClass WHERE
             TaxClass.TaxClass = BillItem.TaxClass NO-LOCK NO-ERROR.
        
        assign fr-header = " CHANGE " ufkey = TRUE ehto = 9.
        RUN ufkey.
        cfc = "lis". RUN ufcolor.

        DISPLAY 
        BillItem.Brand
        BillItem.BillCode
        Billitem.Biname
        BIGName @ BItemGroup.BIGName
        InvSect.ISName when AVAIL InvSect 
        ""  when NOT AVAIL InvSect ;& InvSect.ISName
        BillItem.InvSect
        AccName @ Account.AccName
        BillItem.BIGroup 
        BillItem.AccNum 
        BillItem.AltAccNum   lcAltName
        BillItem.VIPAccNum   lcVipName
        BillItem.EuConAccNum lcEuConName
        BillItem.EUAccNum    lcEuAccName
        BillItem.FSAccNum    lcFsAccName
        BillItem.TaxClass
        BillItem.SAPRid
        TaxClass.TCName WHEN AVAILABLE TaxClass
        "" WHEN NOT AVAILABLE TaxClass @ TaxClass.TCName.

        IF lcRight = "RW" THEN DO:

           IF llDoEvent THEN RUN StarEventSetOldBuffer(lhBillItem).

           RUN VALUE(icUpdateListMode). 

           IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
           KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.
                           
           IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhBillItem).

        END.
        ELSE PAUSE.

        HIDE FRAME lis no-pause.
        
        RUN local-find-others.
        DISPLAY BillItem.Brand 
                BillItem.BIName BillItem.AccNum BillItem.BIGroup 
                BillItem.TaxClass  BIGName
        WITH FRAME sel.
        xrecid = recid(BillItem).

        LEAVE.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST BillItem 
           WHERE BillItem.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND FIRST BillItem USE-INDEX BIName
           WHERE BillItem.Brand = lcBrand no-lock no-error.
        ELSE IF order = 3 THEN FIND FIRST BillItem USE-INDEX BIGroup
           WHERE BillItem.Brand = lcBrand no-lock no-error.

        ASSIGN memory = recid(BillItem) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST BillItem 
           WHERE BillItem.Brand = lcBrand no-lock no-error.
        ELSE IF order = 2 THEN FIND LAST BillItem USE-INDEX BIName
           WHERE BillItem.Brand = lcBrand no-lock no-error.
        ELSE IF order = 3 THEN FIND LAST BillItem USE-INDEX BIGroup
           WHERE BillItem.Brand = lcBrand no-lock no-error.

        do endloop = 1 to frame-down - 1:
           IF order = 1 THEN find prev BillItem 
              WHERE BillItem.Brand = lcBrand no-lock no-error.
           ELSE IF order = 2 THEN FIND prev BillItem USE-INDEX BIName
              WHERE BillItem.Brand = lcBrand no-lock no-error.
           ELSE IF order = 3 THEN FIND prev BillItem USE-INDEX BIGroup
              WHERE BillItem.Brand = lcBrand no-lock no-error.
        end.

        ASSIGN memory = recid(BillItem) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.


PROCEDURE local-find-first: 

 IF icBGroup <> ? THEN 
   FIND FIRST BillItem WHERE BillItem.Brand = lcBrand
                       AND LOOKUP( BillItem.BIGroup, icBGroup) > 0 NO-LOCK NO-ERROR.
 ELSE
   FIND FIRST BillItem WHERE BillItem.Brand = lcBrand no-lock no-error.

END PROCEDURE.

PROCEDURE local-find-next:

  IF icBGroup <> ? THEN DO:
    IF order = 1 OR order = 3 THEN FIND NEXT BillItem 
        WHERE BillItem.Brand = lcBrand AND LOOKUP( BillItem.BIGroup, icBGroup) > 0 no-lock no-error.
    ELSE IF order = 2 THEN FIND NEXT BillItem USE-INDEX BIName
        WHERE BillItem.Brand = lcBrand  AND LOOKUP( BillItem.BIGroup, icBGroup) > 0  no-lock no-error.
  END.
  ELSE DO:
    IF order = 1 THEN FIND NEXT BillItem 
        WHERE BillItem.Brand = lcBrand no-lock no-error.
    ELSE IF order = 2 THEN FIND NEXT BillItem USE-INDEX BIName
        WHERE BillItem.Brand = lcBrand no-lock no-error.
    ELSE IF order = 3 THEN FIND NEXT BillItem USE-INDEX BIGroup
        WHERE BillItem.Brand = lcBrand no-lock no-error.
  END.

END PROCEDURE.

PROCEDURE local-find-prev:

  IF icBGroup <> ? THEN DO:
    IF order = 1 OR order = 3 THEN FIND PREV BillItem 
        WHERE BillItem.Brand = lcBrand AND LOOKUP( BillItem.BIGroup, icBGroup) > 0  no-lock no-error.
    ELSE IF order = 2 THEN FIND PREV BillItem USE-INDEX BIName
        WHERE BillItem.Brand = lcBrand  AND LOOKUP( BillItem.BIGroup, icBGroup) > 0  no-lock no-error.
  END.
  ELSE DO:
    IF order = 1 THEN FIND PREV BillItem 
        WHERE BillItem.Brand = lcBrand no-lock no-error.
    ELSE IF order = 2 THEN FIND PREV BillItem USE-INDEX BIName
        WHERE BillItem.Brand = lcBrand no-lock no-error.
    ELSE IF order = 3 THEN FIND PREV BillItem USE-INDEX BIGroup
        WHERE BillItem.Brand = lcBrand no-lock no-error.
  END.

END PROCEDURE.

PROCEDURE local-find-last:

  IF icBGroup <> ? THEN DO:
    IF order = 1 OR order = 3 THEN FIND LAST BillItem 
        WHERE BillItem.Brand = lcBrand AND LOOKUP( BillItem.BIGroup, icBGroup) > 0  no-lock no-error.
    ELSE IF order = 2 THEN FIND LAST BillItem USE-INDEX BIName
        WHERE BillItem.Brand = lcBrand  AND LOOKUP( BillItem.BIGroup, icBGroup) > 0  no-lock no-error.
  END.
  ELSE DO:
    IF order = 1 THEN FIND LAST BillItem 
        WHERE BillItem.Brand = lcBrand no-lock no-error.
    ELSE IF order = 2 THEN FIND LAST BillItem USE-INDEX BIName
        WHERE BillItem.Brand = lcBrand no-lock no-error.
    ELSE IF order = 3 THEN FIND LAST BillItem USE-INDEX BIGroup
        WHERE BillItem.Brand = lcBrand no-lock no-error.
  END.

END PROCEDURE.


PROCEDURE local-find-others:

   FIND BItemGroup where   
        BItemGroup.Brand   = BillItem.Brand AND
        BItemGroup.BIGroup = BillItem.BIGroup
   no-lock no-error.
   IF AVAIL BItemGroup 
   THEN BIGName = BItemGroup.BIGName.  
   ELSE BIGName = "!! UNKNOWN !!".

   FIND Account where 
        Account.Brand  = BillItem.Brand AND
        Account.AccNum = BillItem.AccNum 
   no-lock no-error.                     
   IF AVAIL Account 
   THEN AccName = Account.AccName.  
   ELSE AccName = "!! UNKNOWN !!".

END PROCEDURE.

PROCEDURE update-mode-general:

     UPDATE BillItem.BIName 
            BillItem.BIGroup 
            BillItem.AccNum 
            BillItem.AltAccNum
            BillItem.VIpAccNum
            BillItem.EUConAccNum 
            BillItem.EUAccNum 
            BillItem.FSAccNum 
            BillItem.FSAccNum 
            BillItem.InvSect 
            BillItem.TaxClass  
            BillItem.SAPRid
            BillItem.DispMPM
            BillItem.CostCentre
     WITH FRAME lis EDITING:
            
               READKEY.
               
               IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                  HIDE MESSAGE.
                  if frame-field = "BIGroup" THEN DO:
                     FIND BItemGroup where 
                          BItemGroup.Brand = lcBrand AND
                          BItemGroup.BIGroup = INPUT FRAME lis BillItem.BIGroup 
                             no-lock no-error.
                     IF NOT AVAIL BItemGroup THEN DO:
                        bell. message "Unknown BillCode group !".
                        NEXT.
                     END.
                     DISP BItemGroup.BIGName.
                  END.

                  else if frame-field = "AccNum" THEN DO:
                     FIND Account where 
                          Account.Brand = lcBrand AND
                          Account.AccNum = INPUT FRAME lis BillItem.AccNum 
                     no-lock no-error.
                     IF NOT AVAIL Account THEN DO:
                        bell. message "Unknown Account !".
                        NEXT.
                     END.
                     DISP Account.AccName.
                  END.

                  else if frame-field = "AltAccNum" THEN DO:
                     IF INPUT FRAME lis BillItem.AltAccNum = 0
                     THEN DISPLAY "" @ lcAltName.
                     ELSE DO:
                        FIND Account where 
                             Account.Brand = lcBrand AND
                             Account.AccNum = INPUT FRAME lis
                                              BillItem.AltAccNum 
                        no-lock no-error.
                        IF NOT AVAIL Account THEN DO:
                           bell. message "Unknown Account !".
                           NEXT.
                        END.
                        DISP Account.AccName @ lcAltName.
                     END.
                  END.

                  else if frame-field = "VipAccNum" THEN DO:
                     IF INPUT FRAME lis BillItem.VipAccNum = 0
                     THEN DISPLAY "" @ lcVipName.
                     ELSE DO:
                        FIND Account where 
                             Account.Brand = lcBrand AND
                             Account.AccNum = INPUT FRAME lis
                                              BillItem.VipAccNum 
                        no-lock no-error.
                        IF NOT AVAIL Account THEN DO:
                           bell. message "Unknown Account !".
                           NEXT.
                        END.
                        DISP Account.AccName @ lcVipName.
                     END.
                  END.

                   else if frame-field = "EUConAccNum" THEN DO:
                     IF INPUT FRAME lis BillItem.EuConAccNum = 0
                     THEN DISPLAY "" @ lcEuConName.
                     ELSE DO:
                        FIND Account where 
                             Account.Brand = lcBrand AND
                             Account.AccNum = INPUT FRAME lis
                                              BillItem.EUConAccNum 
                        no-lock no-error.
                        IF NOT AVAIL Account THEN DO:
                           bell. message "Unknown Account !".
                           NEXT.
                        END.
                        DISP Account.AccName @ lcEUConName.
                     END.
                  END.

                  else if frame-field = "EUAccNum" THEN DO:
                     IF INPUT FRAME lis BillItem.EuAccNum = 0
                     THEN DISPLAY "" @ lcEuAccName.
                     ELSE DO:
                        FIND Account where 
                             Account.Brand = lcBrand AND
                             Account.AccNum = INPUT FRAME lis BillItem.EUAccNum 
                        no-lock no-error.
                        IF NOT AVAIL Account THEN DO:
                           bell. message "Unknown Account !".
                           NEXT.
                        END.
                        DISP Account.AccName @ lcEUAccName.
                     END.   
                  END.

                  else if frame-field = "FSAccNum" THEN DO:
                     IF INPUT FRAME lis BillItem.FSAccNum = 0
                     THEN DISPLAY "" @ lcFSAccName.
                     ELSE DO:
                        FIND Account where 
                             Account.Brand = lcBrand AND
                             Account.AccNum = INPUT FRAME lis BillItem.FSAccNum 
                        no-lock no-error.
                        IF NOT AVAIL Account THEN DO:
                           bell. message "Unknown Account !".
                           NEXT.
                        END.
                        DISP Account.AccName @ lcFSAccName.
                     END.
                  END.

                  else if frame-field = "InvSect" 
                  THEN DO:
                     if input frame lis BillItem.InvSect = "" THEN DO:
                        display "" ;& InvSect.ISName.
                     END.
                     ELSE DO:
                        FIND InvSect where 
                           InvSect.Brand   = lcBrand AND
                           InvSect.InvSect = INPUT FRAME lis BillItem.InvSect
                           no-lock no-error.
                        IF NOT AVAIL InvSect THEN DO:
                           bell. message "Unknown section !".
                           NEXT.
                        END.
                        DISP InvSect.ISName.
                     END.
                  END.
                  
                  else if frame-field = "TaxClass" THEN DO:
                     IF INPUT FRAME lis BillItem.TaxClass = "" THEN
                        DISPLAY "" @ TaxClass.TCName.
                     ELSE DO:
                        FIND TaxClass where 
                           TaxClass.TaxClass = INPUT FRAME lis 
                           BillItem.TaxClass no-lock no-error.
                        IF NOT AVAIL TaxClass THEN DO:
                           bell. message "Unknown Tax class !".
                           NEXT.
                        END.
                        DISP TaxClass.TCName.
                     END. 
                  END.

               END.
               APPLY LASTKEY.
            END. /* EDITING */   

END PROCEDURE.


PROCEDURE update-mode-cc:

   

   IF NEW BillItem THEN DO:

            ASSIGN
            BillItem.Brand    = lcBrand
            BillItem.BillCode = INPUT FRAME lis BillItem.BillCode
            BillItem.DispMPM = FALSE
            BillItem.AccNum = liccAcount
            BillItem.AltAccNum = liccAcount
            BillItem.VipAccNum = liccAcount
            BillItem.EUConAccNum = liccAcount
            BillItem.EUAccNum = liccAcount
            BillItem.FSAccNum = liccAcount
            BillItem.TaxClass = "1"
            BillItem.SAPRid = lcCCSAPRId. 
             
            /* go and find the rest of information to display */
                                
            RUN local-find-others.

            FIND InvSect where 
                 InvSect.Brand   = BillItem.Brand AND
                 InvSect.InvSect = BillItem.InvSect no-lock no-error.

            FIND TaxClass WHERE
                 TaxClass.TaxClass = BillItem.TaxClass NO-LOCK NO-ERROR.
       
          
            DISPLAY 
                 BillItem.Brand
                 BillItem.BillCode
                 Billitem.Biname
                 BIGName @ BItemGroup.BIGName
                 InvSect.ISName when AVAIL InvSect 
                 ""  when NOT AVAIL InvSect ;& InvSect.ISName
                 BillItem.InvSect
                 AccName @ Account.AccName
                 BillItem.BIGroup 
                 BillItem.AccNum 
                 BillItem.AltAccNum
                 BillItem.VIpAccNum
                 BillItem.EUConAccNum 
                 BillItem.EUAccNum 
                 BillItem.FSAccNum 
                 BillItem.FSAccNum 
                 BillItem.InvSect 
                 BillItem.TaxClass
                 BillItem.SAPRid
                 TaxClass.TCName WHEN AVAILABLE TaxClass
                 "" WHEN NOT AVAILABLE TaxClass @ TaxClass.TCName
            WITH FRAME lis.
          
     END.

     UPDATE BillItem.BIName 
            BillItem.BIGroup
     WITH FRAME lis EDITING:         
               READKEY.
               IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                  HIDE MESSAGE.
                  if frame-field = "BIGroup" THEN DO:
                     IF LOOKUP(INPUT FRAME lis BillItem.BIGroup , icBGroup  ) = 0 THEN DO:
                         bell. message " Incorrect BillItem Group !, possible values are: " icBGroup .
                         NEXT.
                     END.
                     FIND BItemGroup where 
                          BItemGroup.Brand = lcBrand AND
                          BItemGroup.BIGroup = INPUT FRAME lis BillItem.BIGroup 
                          no-lock no-error.
                     DISP BItemGroup.BIGName.
                  END.
               END.
              APPLY LASTKEY.
     END. /* EDITING */  

END PROCEDURE.

