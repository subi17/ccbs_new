/* -----------------------------------------------
  MODULE .......: NNASEL.P
  FUNCTION .....: AsiakasBROWSE / HELP
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 21-01-96
  CHANGED ......: 03.12.98 pt in ENglish (almost ..)
                  07.05.99 pt search OrgId
                  15.09.03/aam brand
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF  shared VAR siirto AS CHAR.

DEF VAR haku  LIKE Customer.CustNum  NO-UNDO.
DEF VAR haku2 LIKE Customer.CustName NO-UNDO.
DEF VAR haku3 LIKE Customer.SearchName NO-UNDO.
DEF VAR firstline AS INT NO-UNDO.
DEF VAR order AS INT NO-UNDO.
DEF VAR ex-order AS INT NO-UNDO.
DEF VAR memory   AS RECID              NO-UNDO.
def var line     as int format "99"    NO-UNDO.
DEF VAR delline    AS INT                NO-UNDO.
DEF VAR must-print AS LOG            NO-UNDO.
DEF VAR ufkey        AS LOG            NO-UNDO.
DEF VAR fr-header AS CHAR.

def var ashaku as char format "x(30)"  NO-UNDO.
DEF VAR aakhaku AS LOG NO-UNDO.
DEF VAR orghaku AS LOG NO-UNDO.
DEF VAR haettava AS LOG init TRUE      NO-UNDO.
DEF VAR numero AS LOG NO-UNDO.
DEF VAR rtab AS RECID EXTENT 24      NO-UNDO.
DEF VAR i AS INT NO-UNDO.

DEF VAR xrecid AS RECID.

form
    Customer.CustNum                  column-label "Cust.No"
    Customer.InvCust                 column-label "Inv.Cno"
    Customer.SearchName        format "x(5)"  column-label "Abbrev."
    Customer.CustName  format "x(21)" column-label "Customer's name"
    Customer.COName   format "x(10)" column-label "Add'l name"
    Customer.OrgId   format "x(10)" column-label "Org Code"
    Customer.PostOffice   format "x(6)"  column-label "City"
 WITH centered OVERLAY scroll 1 13 DOWN ROW 3
    COLOR value(cfc)
    title color value(ctc) 
       " SEEK CUSTOMERS (" + gcBrand + ") '" + ashaku + "' " FRAME sel.


form
    ashaku NO-LABEL
    help "Type Cust.Name +ENTER, Cust. No +ENTER or 'Abbreviation' + HOME"
    SKIP
    "What are you using as a search Key:" SKIP
    "  - Customer's   Name   (ENTER)" SKIP
    "  - Abbreviated  Name   (HOME)"  SKIP
    "  - Customer     number (ENTER)" SKIP
    "  - Organization CODE   (END)"   SKIP
with row 1 centered overlay title " SEEK CUSTOMER " FRAME alku.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
   FIND FIRST Customer  USE-INDEX CustNum WHERE 
      Customer.Brand = gcBrand NO-LOCK no-error.
   IF NOT AVAIL Customer THEN DO:
      BELL.
      message "No customers in File - press ENTER !".
      PAUSE no-message.
      RETURN.
   END.


   ASSIGN
   haettava = TRUE xrecid = ? delline = 0 ufkey = TRUE firstline = 0 siirto = ?.

LOOP:
repeat WITH FRAME sel:

    IF haettava THEN DO:

       ASSIGN haettava = FALSE aakhaku = FALSE orghaku = FALSE.
       PAUSE 0 no-message.
alku:  repeat WITH FRAME alku ON ENDKEY UNDO, RETURN:
          ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
          UPDATE ashaku WITH FRAME alku EDITING:
             READKEY. nap = keylabel(LASTKEY).
             /* onko painettu home */
             if nap = "home" then assign aakhaku = true nap = "enter".
             if nap = "end"  then assign orghaku = true nap = "enter".
             APPLY keycode(nap).
          END.

          if ashaku = "" THEN LEAVE LOOP.


          IF aakhaku THEN DO:
             FIND FIRST Customer where 
                Customer.Brand       = gcBrand AND
                Customer.SearchName >= INPUT ashaku
             no-lock no-error.
             order = 2.
          END.
          ELSE IF orghaku THEN DO:
             FIND FIRST Customer where 
                Customer.Brand  = gcBrand AND
                Customer.OrgId >= INPUT ashaku
             no-lock no-error.
             order = 4.
          END.



          ELSE DO:
             /* onko numero */
             numero = TRUE.
             DO i = 1 TO length(ashaku).
                if index("0123456789",substring(ashaku,i,1)) = 0 THEN
                ASSIGN numero = FALSE i = 999.
             END.
             IF numero AND length(ashaku) le 8 THEN DO:
                FIND FIRST Customer where 
                   Customer.Brand = gcBrand AND
                   CustNum ge integer(ashaku) no-lock no-error.
                order = 1.
             END.
             ELSE DO:
                FIND FIRST Customer where 
                   Customer.Brand  = gcBrand AND
                   CustName ge ashaku no-lock no-error.
                order = 3.
             END.
          END.
          IF NOT AVAIL Customer THEN DO:
             BELL.
             message "CAN'T FIND !".
             NEXT alku.
          END.
          ASSIGN memory = recid(Customer) must-print = TRUE.
          view FRAME sel.
          LEAVE.
       END. /* repeat */
    END.

    IF order <> ex-order THEN DO:
       ex-order = order.
    END.


print-line:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND Customer where recid(Customer) = memory no-lock no-error.

         /* print 1 page data on the screen
         beginning from the record whose KeyValue = memory
         beginning from line 'delline' */

         /* IF a line has just been deleted, THEN ... */
         IF delline > 0 THEN DOWN delline - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE Customer THEN DO:
               DISPLAY Customer.CustNum Customer.CustName Customer.OrgId
                       Customer.COName Customer.PostOffice SearchName 
                       Customer.InvCust.
               rtab[FRAME-LINE] = recid(Customer).
               IF order = 1 THEN FIND NEXT Customer
               USE-INDEX CustNum WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 2 THEN FIND NEXT Customer
               USE-INDEX SearchName WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 3 THEN FIND NEXT Customer
               USE-INDEX CustName WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 4 THEN FIND NEXT Customer
               USE-INDEX OrgId  WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
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
         ufk[1]= 0   ufk[2]= 0   ufk[3]= 0 ufk[4]= 0
         ufk[5]= 11 ufk[6]= 0 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF order = 1 THEN DO:
         CHOOSE ROW Customer.CustNum {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Customer.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW Customer.SearchName {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Customer.SearchName WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW Customer.CustName {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Customer.CustName WITH FRAME sel.
      END.
      ELSE IF order = 4 THEN DO:
         CHOOSE ROW Customer.OrgId {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) Customer.OrgId WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         order = order + 1. IF order = 5 THEN order = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         order = order - 1. IF order = 0 THEN order = 4.
      END.

      IF order <> ex-order THEN DO:
         ASSIGN firstline = 0 memory = rtab[FRAME-LINE].
         FIND Customer where recid(Customer) = memory.
         DO i = 1 TO FRAME-LINE - 1:
            IF order = 1 THEN FIND prev Customer
            USE-INDEX CustNum WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND prev Customer
            USE-INDEX SearchName WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND prev Customer
            USE-INDEX CustName WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 4 THEN FIND prev Customer
            USE-INDEX OrgId  WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            IF AVAILABLE Customer THEN
               ASSIGN firstline = i memory = recid(Customer).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? THEN DO:
         BELL.
         message "You are on an empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous line */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND Customer where recid(Customer) = rtab[1] no-lock.
            IF order = 1 THEN FIND prev Customer
            USE-INDEX CustNum WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND prev Customer
            USE-INDEX SearchName WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND prev Customer
            USE-INDEX CustName WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 4 THEN FIND prev Customer
            USE-INDEX OrgId  WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            IF NOT AVAILABLE Customer THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* a previous one was found */
               scroll DOWN.
               DISPLAY Customer.CustNum Customer.CustName Customer.OrgId
                       Customer.COName Customer.PostOffice SearchName 
                       Customer.InvCust.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(Customer)
               memory = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous line */

      /* NEXT line */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND Customer where recid(Customer) = rtab[FRAME-DOWN] no-lock .
            IF order = 1 THEN FIND NEXT Customer
            USE-INDEX CustNum WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 2 THEN FIND NEXT Customer
            USE-INDEX SearchName WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 3 THEN FIND NEXT Customer
            USE-INDEX CustName WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            ELSE IF order = 4 THEN FIND NEXT Customer
            USE-INDEX OrgId  WHERE 
               Customer.Brand = gcBrand NO-LOCK no-error.
            IF NOT AVAILABLE Customer THEN DO:
               message "YOU ARE ON THE LAST ROW A !".
               BELL.
               PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* yet another record was found */
               scroll up.
               DISPLAY Customer.CustNum Customer.CustName Customer.OrgId
                       Customer.COName Customer.PostOffice SearchName 
                       Customer.InvCust.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(Customer).
               /* finally LAST line's KeyValue is saved */
               memory = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* NEXT line */

      /* previous page */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         memory = rtab[1].
         FIND Customer where recid(Customer) = memory no-lock no-error.
         IF order = 1 THEN FIND prev Customer
         USE-INDEX CustNum WHERE 
            Customer.Brand = gcBrand NO-LOCK no-error.
         ELSE IF order = 2 THEN FIND prev Customer
         USE-INDEX SearchName WHERE 
            Customer.Brand = gcBrand NO-LOCK no-error.
         ELSE IF order = 3 THEN FIND prev Customer
         USE-INDEX CustName  WHERE 
            Customer.Brand = gcBrand NO-LOCK no-error.
         ELSE IF order = 4 THEN FIND prev Customer
         USE-INDEX OrgId  WHERE 
            Customer.Brand = gcBrand NO-LOCK no-error.
         IF AVAILABLE Customer THEN DO:
            memory = recid(Customer).

            /* go back one page */
            DO line = 1 TO (FRAME-DOWN - 1):
               IF order = 1 THEN FIND prev Customer
               USE-INDEX CustNum WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 2 THEN FIND prev Customer
               USE-INDEX SearchName WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 3 THEN FIND prev Customer
               USE-INDEX CustName WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               ELSE IF order = 4 THEN FIND prev Customer
               USE-INDEX OrgId  WHERE 
                  Customer.Brand = gcBrand NO-LOCK no-error.
               IF AVAILABLE Customer THEN memory = recid(Customer).
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
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
        /* cursor TO the downmost line */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "YOU ARE ON THE LAST PAGE !".
            BELL.
            PAUSE 1 no-message.
        END.
        ELSE DO: /* the downmost line wasn't empty */
            memory = rtab[FRAME-DOWN].
            FIND Customer where recid(Customer) = memory no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* NEXT page */

     else if lookup(nap,"5,f5,enter,return") > 0 THEN DO: /* valinta */
        FIND Customer where recid(Customer) = rtab[FRAME-LINE] no-lock.
        siirto = string(CustNum).
        LEAVE LOOP.
     END.


     else if lookup(nap,"home") > 0 THEN DO:
        IF order = 1 THEN FIND FIRST Customer
        USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND FIRST Customer
        USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND FIRST Customer
        USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 4 THEN FIND FIRST Customer
        USE-INDEX OrgId  WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(Customer) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* LAST record */
        IF order = 1 THEN FIND LAST Customer
        USE-INDEX CustNum WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 2 THEN FIND LAST Customer
        USE-INDEX SearchName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 3 THEN FIND LAST Customer
        USE-INDEX CustName WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ELSE IF order = 4 THEN FIND LAST Customer
        USE-INDEX OrgId  WHERE Customer.Brand = gcBrand NO-LOCK no-error.
        ASSIGN memory = recid(Customer) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        haettava = TRUE.
        HIDE FRAME sel no-pause.
        NEXT LOOP.
     END.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
HIDE FRAME alku no-pause.
si-recid = xrecid.

