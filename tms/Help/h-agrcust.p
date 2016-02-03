/* -----------------------------------------------
  MODULE .......: h-agrcust
  FUNCTION .....: agreement customer help
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.02.06
  CHANGED ......: 
  Version ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}
{Func/fcustdata.i}

DEF INPUT PARAMETER icOrgID AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCustName  AS CHAR NO-UNDO.
DEF VAR lcLastName  AS CHAR NO-UNDO.
DEF VAR lcFirstName AS CHAR NO-UNDO.
DEF VAR liCustNum   AS INT  NO-UNDO.
DEF VAR lcOrgID     AS CHAR NO-UNDO. 

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 3.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR Memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.

form
    Customer.CustNum    FORMAT ">>>>>>>9" COLUMN-LABEL "CustNbr"
    lcCustName          FORMAT "X(20)"    COLUMN-LABEL "Name"
    Customer.Address    FORMAT "X(20)"    COLUMN-LABEL "Address"
    Customer.PostOffice FORMAT "X(14)"    COLUMN-LABEL "Post."
    Customer.OrgID      FORMAT "X(11)"    COLUMN-LABEL "PersonID"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " AGREEMENT CUSTOMERS " 
    FRAME sel.

form /* seek  Customer */
    "Customer Nbr:" liCustNum
       HELP "Enter customer number"
       FORMAT ">>>>>>>9"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND number "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  */
   "LastName/Company:" lcLastName    
       FORMAT "X(30)"
       HELP "Last name or company name" SKIP
   "FirstName ......:" lcFirstName 
       FORMAT "X(20)"
       HELP "First name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

form /* seek  */
    "PersonID/CompanyID:" lcOrgID
       HELP "Enter person id / company id"
       FORMAT "X(11)" 
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND person ID "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f3.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

IF icOrgID > "" THEN ASSIGN 
   MaxOrder = 1
   order    = 3.
ELSE order = 2. 

RUN local-find-first.

IF AVAILABLE Customer THEN ASSIGN
   Memory       = recid(Customer)
   must-print   = TRUE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
   END.

   PrintPage:
   DO :
   
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND Customer WHERE recid(Customer) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE Customer THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(Customer).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        up FRAME-LINE - 1.
        DOWN FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = FALSE.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed AND the cursor is on the
        upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
      
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

   BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
        ASSIGN
           ufk    = 0
           ufk[5] = 11
           ufk[8] = 8 
           ehto   = 3 
           ufkey  = FALSE.

        IF icOrgID = "" THEN ASSIGN 
           ufk[1] = 702
           ufk[2] = 717
           ufk[3] = 812.
           
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW Customer.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Customer.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW lcCustName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) lcCustName WITH FRAME sel.
      END.
      ELSE IF order = 3 THEN DO:
         CHOOSE ROW Customer.OrgID ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) Customer.OrgID WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND Customer WHERE recid(Customer) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE Customer THEN
              ASSIGN FIRSTrow = i Memory = recid(Customer).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE Customer THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-DOWN TO 2 BY -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(Customer)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE Customer THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT ROW was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 TO FRAME-DOWN - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-DOWN] = recid(Customer).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND Customer WHERE recid(Customer) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE Customer THEN DO:
           Memory = recid(Customer).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE Customer THEN Memory = recid(Customer).
              ELSE RowNo = FRAME-DOWN.
           END.
           must-print = TRUE.
           NEXT LOOP.
        END.
        ELSE DO:
           /* is this the very FIRST record of the table ?  */
           MESSAGE "YOU ARE ON THE FIRST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
        END.
     END. /* PREVious page */

     /* NEXT page */
     ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND Customer WHERE recid(Customer) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 AND ufk[1] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        CLEAR FRAME f1.
        liCustNum = 0.
        UPDATE liCustNum WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF liCustNum > 0 THEN DO:
           FIND FIRST Customer USE-INDEX CustNum WHERE 
                      Customer.Brand    = gcBrand   AND
                      Customer.CustNum >= liCustNum AND
                      Customer.CustNum  = Customer.AgrCust
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE Customer THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(Customer)
              must-print = TRUE
              order      = 1.
              
           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 AND ufk[2] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        CLEAR FRAME f2.
        ASSIGN lcLastName   = ""
               lcFirstName = "".
               
        UPDATE lcLastName lcFirstName WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.

        IF lcLastName > "" THEN DO:

            IF lcFirstName = "" THEN 
               FIND FIRST Customer USE-INDEX CustName WHERE 
                          Customer.Brand    = gcBrand     AND
                          Customer.CustName >= lcLastName AND
                          Customer.CustNum  = Customer.AgrCust
               NO-LOCK NO-ERROR.

            ELSE DO:
               
               FIND FIRST Customer USE-INDEX CustName WHERE 
                          Customer.Brand     = gcBrand     AND
                          Customer.CustName  = lcLastName  AND
                          Customer.FirstName = lcFirstName AND
                          Customer.CustNum   = Customer.AgrCust
               NO-LOCK NO-ERROR.

               IF NOT AVAILABLE Customer THEN 
               FIND FIRST Customer USE-INDEX CustName WHERE 
                          Customer.Brand     = gcBrand          AND
                          Customer.CustName  = lcLastName       AND
                          Customer.FirstName BEGINS lcFirstName AND
                          Customer.CustNum   = Customer.AgrCust
               NO-LOCK NO-ERROR.

               IF NOT AVAILABLE Customer THEN 
               FIND FIRST Customer USE-INDEX CustName WHERE 
                          Customer.Brand     = gcBrand      AND
                          Customer.CustName  = lcLastName   AND
                          Customer.FirstName >= lcFirstName AND
                          Customer.CustNum   = Customer.AgrCust
               NO-LOCK NO-ERROR.

               IF NOT AVAILABLE Customer THEN 
               FIND FIRST Customer USE-INDEX CustName WHERE 
                          Customer.Brand     = gcBrand      AND
                          Customer.CustName  >= lcLastName  AND
                          Customer.FirstName >= lcFirstName AND
                          Customer.CustNum   = Customer.AgrCust
               NO-LOCK NO-ERROR.
           END.
           
           IF NOT AVAILABLE Customer THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(Customer)
              must-print = TRUE
              order      = 2.
              
           NEXT LOOP.
        END.
     END. /* Search-2 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 AND ufk[3] > 0 
     THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        CLEAR FRAME f3.
        lcOrgID = "".
        UPDATE lcOrgID WITH FRAME f3.
        HIDE FRAME f3 NO-PAUSE.

        IF lcOrgID > "" THEN DO:
           FIND FIRST Customer USE-INDEX OrgID WHERE 
                      Customer.Brand  = gcBrand AND
                      Customer.OrgID >= lcOrgID AND
                      Customer.CustNum = Customer.AgrCust
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE Customer THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(Customer)
              must-print = TRUE
              order      = 3.
              
           NEXT LOOP.
        END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: 
        RUN local-find-this(FALSE).

        IF AVAILABLE Customer THEN DO:
           siirto = string(Customer.CustNum).
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Customer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Customer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND Customer WHERE recid(Customer) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND Customer WHERE recid(Customer) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST Customer USE-INDEX CustNum WHERE
                 Customer.Brand   = gcBrand AND
                 Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST Customer USE-INDEX CustName WHERE
                 Customer.Brand   = gcBrand AND
                 Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN DO:
      IF icOrgID > "" THEN 
      FIND FIRST Customer USE-INDEX OrgID WHERE
                 Customer.Brand   = gcBrand AND
                 Customer.OrgID   = icOrgID NO-LOCK NO-ERROR.

      ELSE    
      FIND FIRST Customer USE-INDEX OrgID WHERE
                 Customer.Brand   = gcBrand AND
                 Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   END.
   
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN 
      FIND LAST Customer USE-INDEX CustNum WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST Customer USE-INDEX CustName WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN DO:
      IF icOrgID > "" THEN 
      FIND LAST Customer USE-INDEX OrgID WHERE
                Customer.Brand   = gcBrand AND
                Customer.OrgID   = icOrgID NO-LOCK NO-ERROR.

      ELSE    
      FIND LAST Customer USE-INDEX OrgID WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT Customer USE-INDEX CustNum WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT Customer USE-INDEX CustName WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN DO:
      IF icOrgID > "" THEN 
      FIND NEXT Customer USE-INDEX OrgID WHERE
                Customer.Brand   = gcBrand AND
                Customer.OrgID   = icOrgID NO-LOCK NO-ERROR.

      ELSE    
      FIND NEXT Customer USE-INDEX OrgID WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN 
      FIND PREV Customer USE-INDEX CustNum WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV Customer USE-INDEX CustName WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   ELSE IF order = 3 THEN DO:
      IF icOrgID > "" THEN 
      FIND PREV Customer USE-INDEX OrgID WHERE
                Customer.Brand   = gcBrand AND
                Customer.OrgID   = icOrgID NO-LOCK NO-ERROR.

      ELSE    
      FIND PREV Customer USE-INDEX OrgID WHERE
                Customer.Brand   = gcBrand AND
                Customer.CustNum = Customer.AgrCust NO-LOCK NO-ERROR.
   END.
 
END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       Customer.CustNum    
       lcCustName
       Customer.Address
       Customer.PostOffice
       Customer.OrgID
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

   lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                 BUFFER Customer).
END PROCEDURE.


