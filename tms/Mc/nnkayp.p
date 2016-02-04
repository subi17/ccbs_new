/* -----------------------------------------------
  MODULE .......: NNKAYP.P
  FUNCTION .....: Asiakkaiden kategorioiden yllApito
  APPLICATION ..: NN/TELE1
  AUTHOR .......: PT
  CREATED ......: 29.09.96 pt
  changePVM ....:
  MODIFIED......: 17.05.99 jp - uright1 & uright2 added
                  07.03.01 pt - Account
                  20.03.01 aam  PerAccNum, UnbillAccNum
                  22.02.02 ht - IntType added
                  27.02.02 HT - previous continued
                  26.04.02 tk - eventlogging added
                  09.09.02 jp - frame-field fixed
                  18.10.02 aam  credit limit removed
                  27.02.03 tk - tokens 
                  11.09.03 tk - brand
                  07.10.03 aam  PaymTerm
                  14.11.06 aam  CustIDType,
                                accounts with 8 digits
                  03.01.07 aam  accounts removed              
  Version ......: M15
  ------------------------------------------------------ */

&GLOBAL-DEFINE BrTable CustCat

{Syst/commali.i}
{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'custcat'}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCustCat AS HANDLE NO-UNDO.
   lhCustCat = BUFFER CustCat:HANDLE.
   RUN StarEventInitialize(lhCustCat).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCustCat).
   END.

END.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR haku        LIKE CustCat.Category  NO-UNDO.
DEF VAR haku2       LIKE CustCat.CatName NO-UNDO.
DEF VAR firstline   AS INT NO-UNDO.
DEF VAR order       AS INT NO-UNDO.
DEF VAR ex-order    AS INT NO-UNDO.
DEF VAR memory      AS RECID           NO-UNDO.
def var line        as int format "99" NO-UNDO.
DEF VAR delline     AS INT             NO-UNDO.
DEF VAR must-print  AS LOG             NO-UNDO.
DEF VAR must-add    AS LOG             NO-UNDO.
DEF VAR ufkey       AS LOG             NO-UNDO.
DEF VAR fr-header   AS CHAR.
DEF VAR rtab        AS RECID EXTENT 24 NO-UNDO.
DEF VAR i           AS INT             NO-UNDO.
DEF VAR xrecid      AS RECID.
DEF VAR ok          AS LO FORMAT "Yes/No" NO-UNDO.

DEF VAR lcPerName     AS CHAR NO-UNDO.
DEF VAR lcUbName      AS CHAR NO-UNDO.
DEF VAR IntTypeName   AS C    NO-UNDO.
DEF VAR TypeNames     AS C    NO-UNDO.
DEF VAR lcUnknown     AS CHAR NO-UNDO INIT "Not found".           
DEF VAR lcAccName     AS CHAR NO-UNDO. 
DEF VAR lcIDType      AS CHAR NO-UNDO.
DEF VAR lcCode        AS CHAR NO-UNDO. 

TypeNames = "General,Fixed,Added to reference rate,Unknown !".


FUNCTION fCustIDType RETURNS LOGICAL
   (icCustIDType AS CHAR):

   DEF VAR liCnt AS INT NO-UNDO.
   
   IF icCustIDType = "" THEN RETURN TRUE.
   
   DO liCnt = 1 TO NUM-ENTRIES(icCustIDType):
      IF NOT DYNAMIC-FUNCTION("fTMSCodeChk" IN ghFunc1,
                              "Customer",
                              "CustIDType",
                              ENTRY(liCnt,icCustIDType))
      THEN RETURN FALSE.
   END.
   
   RETURN TRUE.
   
END FUNCTION.   
   

form
    CustCat.Brand   COLUMN-LABEL "Bran" FORMAT "X(4)"
    CustCat.Category
    CustCat.CatName FORMAT "X(23)"
    CustCat.IntType COLUMN-LABEL "Int"
       HELP "0=General, 1=Fixed, 2=Added to confirmed reference rate"
    CustCat.PaymTerm COLUMN-LABEL "PaymT"
    CustCat.SelfEmployed COLUMN-LABEL "SelfEmp"
    CustCat.MobSubLimit COLUMN-LABEL "SubLimit"
    CustCat.ActivationLimit COLUMN-LABEL "ActLimit"

    WITH width 80 OVERLAY scroll 1 15 DOWN
    COLOR value(cfc)
    title color value(ctc) " " + ynimi +
    " CUSTOMER CATEGORIES "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    CustCat.Category    LABEL "Category Code" COLON 24 SKIP
    CustCat.CatName   LABEL "Category Name" COLON 24
    VALIDATE(INPUT CustCat.CatName ne "","Missing Category Name!") SKIP

    CustCat.IntType   LABEL "Interest type" COLON 24
       HELP "0=General, 1=Fixed, 2=Added to confirmed reference rate"
       IntTypeName              NO-LABEL FORMAT "x(25)"    SKIP
    CustCat.PaymTerm   LABEL "Payment Term" COLON 24 SKIP
    CustCat.CustIDType LABEL "Cust. ID Type" COLON 24
       HELP "Customers with these ID types will be assigned to this category"
       FORMAT "X(20)" 
       SKIP
    CustCat.MobSubLimit LABEL "Mobsub limit" COLON 24
      HELP "How many active subscriptions this category can have by default"
         SKIP 
    CustCat.ActivationLimit LABEL "MobSub Activation limit" COLON 24
      HELP "How many subscription activation request can have by default" SKIP
    CustCat.SelfEmployed LABEL "Self Employed" COLON 24
      HELP "Is this category self employed or not" SKIP 
    
 WITH  OVERLAY ROW 7 col 5
    COLOR value(cfc)
    TITLE COLOR value(ctc)
    fr-header WITH side-labels 
    FRAME lis.

form /* kategorian tunnuksella hakua varten */
    "Brand:" lcBrand skip
    "Code :" haku
    help "Give a code or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND CODE "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr.

form /* kategorian nimella hakua varten */
    "Brand:" lcBrand skip
    "Name :" haku2
    help "Give a Name or beginning of it"
    with row 4 col 2 title color value(ctc) " FIND Name "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME hayr2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
view FRAME sel.
order = 1.

   run pFindFirst.

   IF AVAILABLE CustCat THEN ASSIGN memory = recid(CustCat)
      must-print = TRUE must-add    = FALSE.
   ELSE DO:
      IF lcRight NE "RW" THEN DO:
         MESSAGE "No customer categories available !" VIEW-AS ALERT-BOX.
         RETURN.
      END.
      ASSIGN 
         memory     = ? 
         must-print = FALSE 
         must-add   = TRUE.
   END.

ASSIGN xrecid = ? delline = 0 ufkey = TRUE order = 1 firstline = 0.

LOOP:
repeat WITH FRAME sel:

    IF order <> ex-order THEN DO:
       ex-order = order.
       if order = 1 then put screen row 19 col 35 "  By Code  ".
       if order = 2 then put screen row 19 col 35 "  By Name  ".
    END.

   IF must-add THEN DO:  /* CustCat -ADD  */
      assign cfc = "lis" ufkey = true fr-header = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.
add-new:
      repeat WITH FRAME lis ON ENDKEY UNDO add-new, LEAVE add-new.
         PAUSE 0 no-message.
         CLEAR FRAME lis no-pause.
         ehto = 9. RUN Syst/ufkey.
         DO TRANSAction:
            PROMPT-FOR CustCat.Category
            VALIDATE
               (CustCat.Category = "" OR
               NOT can-find(CustCat using  CustCat.Category WHERE
                            CustCat.Brand = lcBrand),
               "Product " + string(INPUT CustCat.Category) +
               " already exists !").
            if input CustCat.Category = "" THEN LEAVE add-new.
            CREATE CustCat.
            ASSIGN
            CustCat.Brand    = lcBrand
            CustCat.Category = INPUT FRAME lis CustCat.Category.
            UPDATE 
               CustCat.CatName 
               CustCat.IntType 
               CustCat.PaymTerm
               CustCat.CustIDType
               CustCat.MobSubLimit
               CustCat.ActivationLimit
               CustCat.SelfEmployed
            EDITING:
               READKEY.
               
               IF KEYLABEL(LASTKEY) = "F9" AND 
                  FRAME-FIELD = "CustIDType"
               THEN DO:

                  RUN Help/h-tmscodes(INPUT "Customer",    /* TableName */
                                       "CustIDType",  /* FieldName */
                                       "CustCare",  /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     IF LOOKUP(lcCode,INPUT FRAME lis CustCat.CustIDType) = 0
                     THEN DO:
                        lcCode = INPUT FRAME lis CustCat.CustIDType +
                                 (IF INPUT FRAME lis CustCat.CustIDType > ""
                                  THEN "," ELSE "") +
                                 lcCode. 
                        DISPLAY lcCode @ CustCat.CustIDType
                        WITH FRAME lis.   
                     END.   
                  END.   

                  ehto = 9.
                  RUN Syst/ufkey.
                  NEXT. 
               END.

               IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
                  PAUSE 0.
                  IF FRAME-FIELD = "IntType" THEN DO:
                     IF      INPUT FRAME lis IntType = 0 THEN 
                        IntTypeName = ENTRY(1,TypeNames).
                     ELSE IF INPUT FRAME lis IntType = 1 THEN 
                        IntTypeName = ENTRY(2,TypeNames).
                     ELSE IF INPUT FRAME lis IntType = 2 THEN 
                        IntTypeName = ENTRY(3,TypeNames).
                     ELSE DO:
                        BELL.
                        IntTypeName = ENTRY(4,TypeNames).
                        DISP IntTypeName.
                        NEXT.
                     END.
                     DISP IntTypeName WITH FRAME lis.
                  END.

                  ELSE IF FRAME-FIELD = "CustIDType" THEN DO:
                  
                     IF INPUT FRAME lis CustCat.CustIDType > "" THEN DO:
                        IF NOT 
                        fCustIDType(INPUT INPUT FRAME lis CustCat.CustIDType)
                        THEN DO:
                           BELL.
                           MESSAGE "Unknown ID type(s) !".
                           NEXT.
                        END.
                     END.
                  END.   
                  ELSE IF FRAME-FIELD = "ActivationLimit" THEN DO:
                     IF INPUT FRAME lis CustCat.ActivationLimit <
                        INPUT FRAME lis CustCat.MobSubLimit THEN DO:
                        BELL.
                        MESSAGE "Activation limit can not be less than Mobsub limit !".
                        NEXT.
                     END. /* IF CustCat.ActivationLimit */
                  END. /* ELSE IF FRAME-FIELD = "ActivationLimit" THEN DO: */
               END.
               APPLY LASTKEY.
            END.   
            ASSIGN
            memory = recid(CustCat)
            xrecid = memory.
         END.

         IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustCat).

      END.  /* add-new */
      HIDE FRAME lis no-pause.
      ASSIGN must-print = TRUE.

      /* any records AVAILABLE ? */
      run pFindFirst.
      IF NOT AVAILABLE CustCat THEN LEAVE LOOP.
      NEXT LOOP.
   END.

print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND CustCat where recid(CustCat) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE CustCat THEN DO:
               DISPLAY 
               CustCat.Brand
               CustCat.Category CustCat.CatName 
               CustCat.IntType 
               CustCat.PaymTerm
               CustCat.MobSubLimit
               CustCat.ActivationLimit
               CustCat.SelfEmployed.

               rtab[FRAME-LINE] = recid(CustCat).
               run pFindNext.
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
         ufk[1]= 35  ufk[2]= 30 ufk[3]= 0 ufk[4]= 0
         ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
         ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
         ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.

         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW CustCat.Category ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CustCat.Category WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW CustCat.CatName ;(uchoose.i;) no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CustCat.CatName WITH FRAME sel.
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
         FIND CustCat where recid(CustCat) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            run pFindPrev.
            IF AVAILABLE CustCat THEN
               ASSIGN firstline = i memory = recid(CustCat).
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
            FIND CustCat where recid(CustCat) = rtab[1] no-lock.
            run pFindPrev.
            IF NOT AVAILABLE CustCat THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY CustCat.Brand CustCat.Category CustCat.CatName
                 CustCat.IntType  CustCat.PaymTerm
                       .
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(CustCat)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */




      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND CustCat where recid(CustCat) = rtab[FRAME-DOWN] no-lock .
            run pFindNext.
            IF NOT AVAILABLE CustCat THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY 
               CustCat.Brand CustCat.Category CustCat.CatName 
               CustCat.IntType
               CustCat.PaymTerm.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(CustCat).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up,-") > 0 THEN DO:
         memory = rtab[1].
         FIND CustCat where recid(CustCat) = memory no-lock no-error.
         run pFindPrev.
         IF AVAILABLE CustCat THEN DO:
            memory = recid(CustCat).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               run pFindPrev.
               IF AVAILABLE CustCat THEN memory = recid(CustCat).
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
            FIND CustCat where recid(CustCat) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     /* Haku 1 */
     if lookup(nap,"1,f1") > 0 THEN DO:  /* haku sarakk. 1 */
        cfc = "puyr". RUN Syst/ufcolor.
        haku = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISP lcBrand with frame hayr.
        UPDATE 
           lcBrand WHEN gcAllBrand
           haku WITH FRAME hayr.
        HIDE FRAME hayr no-pause.
        if haku <> "" THEN DO:
           FIND FIRST CustCat where 
                      CustCat.Category >= INPUT haku AND
                      CustCat.Brand = lcBrand
           no-lock no-error.

           IF NOT fRecFound(1) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     /* Haku sarakk. 2 */
     if lookup(nap,"2,f2") > 0 THEN DO:  /* haku sar. 2 */
        cfc = "puyr". RUN Syst/ufcolor.
        haku2 = "".
        ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
        DISP lcBrand WITH FRAME hayr2.
        UPDATE 
           lcBrand WHEN gcAllBrand
           haku2 WITH FRAME hayr2.
        HIDE FRAME hayr2 no-pause.
        if haku2 <> "" THEN DO:
           FIND FIRST CustCat where CustCat.CatName >= INPUT haku2 AND
                      CustCat.Brand = lcBrand
           no-lock no-error.

           IF NOT fRecFound(2) THEN NEXT BROWSE.

           NEXT LOOP.
        END.
     END. /* Haku sar. 2 */


     ELSE if  lookup(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* lisays */

         must-add = TRUE.
         NEXT LOOP.
     END.

     else if lookup(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSAction:  /* removal */

        delline = FRAME-LINE.
        FIND CustCat where recid(CustCat) = rtab[FRAME-LINE] no-lock.

        /* line TO be deleted is lightened */
        COLOR DISPLAY value(ctc) 
        CustCat.Category CustCat.CatName .

        run pFindNext.
        IF AVAILABLE CustCat THEN memory = recid(CustCat).
        ELSE DO:
           /* the one TO be deleted is rereaden */
           FIND CustCat where recid(CustCat) = rtab[FRAME-LINE] no-lock.
           /* AND THEN the previous one */
           run pFindPrev.
           IF AVAILABLE CustCat THEN DO:
              ASSIGN
              delline = delline - 1  /* cause the LAST one is TO be deleted */
              memory = recid(CustCat).
           END.
        END.

        /* 'find' back TO the ROW TO be deleted */
        FIND CustCat where recid(CustCat) = rtab[FRAME-LINE]
        exclusive-lock.

        ASSIGN ok = FALSE.
        message " ARE YOU SURE YOU WANT TO REMOVE (Y/N)? " UPDATE ok.
        COLOR DISPLAY value(ccc)
        CustCat.Category CustCat.CatName 
        CustCat.IntType .
        IF ok THEN DO:

            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustCat).

            DELETE CustCat.

            /* in the LAST record was deleted ? */
            IF NOT can-find(FIRST CustCat WHERE
                                  CustCat.Brand = lcBrand) THEN DO:
               CLEAR FRAME sel no-pause.
               PAUSE 0 no-message.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
        END.
        ELSE delline = 0. /* wasn't the LAST one */
     END. /* removal */

     else if lookup(nap,"enter,return") > 0 AND lcRight = "RW"
     THEN DO:
     
     DO TRANSAction WITH FRAME lis ON ENDKEY UNDO, LEAVE:
        /* change */
        FIND CustCat where recid(CustCat) = rtab[frame-line(sel)]
        exclusive-lock.
        ASSIGN ufkey = TRUE ehto = 9. RUN Syst/ufkey.

        PAUSE 0.
        DISPLAY CustCat.Category WITH FRAME lis.

        IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCustCat).

        UPDATE CustCat.CatName 
               CustCat.IntType 
               CustCat.PaymTerm
               CustCat.CustIDType
               CustCat.MobSubLimit
               CustCat.ActivationLimit
               CustCat.SelfEmployed
        WITH FRAME lis

        EDITING:
           READKEY.

           IF KEYLABEL(LASTKEY) = "F9" AND 
              FRAME-FIELD = "CustIDType"
           THEN DO:

                  RUN Help/h-tmscodes(INPUT "Customer",    /* TableName */
                                       "CustIDType",  /* FieldName */
                                       "CustCare",  /* GroupCode */
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN DO:
                     IF LOOKUP(lcCode,INPUT FRAME lis CustCat.CustIDType) = 0
                     THEN DO:
                        lcCode = INPUT FRAME lis CustCat.CustIDType +
                                 (IF INPUT FRAME lis CustCat.CustIDType > ""
                                  THEN "," ELSE "") +
                                 lcCode. 
                        DISPLAY lcCode @ CustCat.CustIDType
                        WITH FRAME lis.   
                     END.   
                  END.   

                  ehto = 9.
                  RUN Syst/ufkey.
                  NEXT. 
           END.
           
           ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
              PAUSE 0.

              IF FRAME-FIELD = "IntType" THEN DO:
                 IF INPUT FRAME lis IntType < 0 OR
                    INPUT FRAME lis IntType > 2 THEN DO:
                   BELL.
                   MESSAGE "Unknown RepType !".
                   NEXT.
                 END.
              END.

              ELSE IF FRAME-FIELD = "CustIDType" THEN DO:
                IF INPUT FRAME lis CustCat.CustIDType > "" THEN DO:
                   IF NOT fCustIDType(INPUT INPUT FRAME lis CustCat.CustIDType)
                   THEN DO:
                      BELL.
                      MESSAGE "Unknown ID type(s) !".
                      NEXT.
                   END.
                END.
              END.   
              ELSE IF FRAME-FIELD = "ActivationLimit" THEN DO:
                 IF INPUT FRAME lis CustCat.ActivationLimit <
                    INPUT FRAME lis CustCat.MobSubLimit THEN DO:
                    BELL.
                    MESSAGE "Activation limit can not be less than Mobsub limit !".
                    NEXT.
                 END. /* IF CustCat.ActivationLimit */
              END. /* ELSE IF FRAME-FIELD = "ActivationLimit" THEN DO: */
           END.
           APPLY LASTKEY.
        END.   

        IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCustCat).

        xrecid = recid(CustCat).
        
        DISPLAY 
               CustCat.Brand CustCat.Category CustCat.CatName 
               CustCat.IntType
               CustCat.PaymTerm 
               CustCat.SelfEmployed 
               CustCat.MobsubLimit
               CustCat.ActivationLimit
               WITH FRAME sel.
     END. /* TRANSACTION */
     HIDE FRAME lis NO-PAUSE.
     END.

     else if lookup(nap,"home,h") > 0 THEN DO:
        run pFindFirst.
        ASSIGN memory = recid(CustCat) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end,e") > 0 THEN DO : /* LAST record */
        run pFindLast.
        ASSIGN memory = recid(CustCat) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.

procedure pFindFirst:
  case order:
     when 1 THEN 
        FIND FIRST CustCat WHERE 
                   CustCat.Brand = lcBrand
        no-lock no-error.
     when 2 THEN 
        FIND FIRST CustCat USE-INDEX CatName where
                   CustCat.Brand = lcBrand
        no-lock no-error.
  end.      
end.

procedure pFindLast:
  case order:
     when 1 THEN 
        FIND Last CustCat WHERE 
                   CustCat.Brand = lcBrand
        no-lock no-error.
     when 2 THEN 
        FIND Last CustCat USE-INDEX CatName where
                   CustCat.Brand = lcBrand
        no-lock no-error.
  end.      
end.

procedure pFindNext:
  case order:
     when 1 THEN 
        FIND Next CustCat WHERE 
                   CustCat.Brand = lcBrand
        no-lock no-error.
     when 2 THEN 
        FIND Next CustCat USE-INDEX CatName where
                   CustCat.Brand = lcBrand
        no-lock no-error.
  end.      
end.

procedure pFindPrev:
  case order:
     when 1 THEN 
        FIND Prev CustCat WHERE 
                   CustCat.Brand = lcBrand
        no-lock no-error.
     when 2 THEN 
        FIND Prev CustCat USE-INDEX CatName where
                   CustCat.Brand = lcBrand
        no-lock no-error.
  end.      
end.


