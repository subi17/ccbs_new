/* -----------------------------------------------
  MODULE .......: h-customer
  FUNCTION .....: customer help
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 10.01.06
  CHANGED ......: 
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{fcustdata.i}

DEF INPUT PARAMETER iiMainCust AS INT  NO-UNDO.
DEF INPUT PARAMETER icMainType AS CHAR NO-UNDO.
DEF INPUT PARAMETER icCustType AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcCustName  AS CHAR NO-UNDO.
DEF VAR liCustNum   AS INT  NO-UNDO.

DEF VAR xrecid       AS RECID                  NO-UNDO  init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 13.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 2.
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

DEF TEMP-TABLE ttCustomer NO-UNDO
   FIELD CustNum    AS INT
   FIELD CustName   AS CHAR
   FIELD InvCust    AS INT
   FIELD ICName     AS CHAR 
   FIELD Address    AS CHAR
   FIELD PostOffice AS CHAR
   INDEX CustNum  CustNum
   INDEX CustName CustName.
   
DEF BUFFER bInvCust FOR Customer.

form
    ttCustomer.CustNum    FORMAT ">>>>>>>9" COLUMN-LABEL "CustNbr"
    ttCustomer.CustName   FORMAT "X(20)"    COLUMN-LABEL "Name"
    ttCustomer.Address    FORMAT "X(16)"    COLUMN-LABEL "Address"
    ttCustomer.PostOffice FORMAT "X(8)"     COLUMN-LABEL "Post."
    ttCustomer.InvCust    FORMAT ">>>>>>>9" COLUMN-LABEL "InvCust"
    ttCustomer.ICName     FORMAT "X(12)"    COLUMN-LABEL "ICust Name"
WITH ROW FrmRow width 80 OVERLAY FrmDown DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ac-hdr + " " 
    FRAME sel.

form /* seek  ttCustomer */
    "Customer Nbr:" liCustNum
    HELP "Enter customer number"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND number "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek  */
    "Name:" lcCustName
    HELP "Enter customer name"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND name "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.

FUNCTION fPickCustomer RETURNS LOGICAL
   (icType AS CHAR).
   
   DEF VAR llTake AS LOG  NO-UNDO.
   DEF VAR lcRole AS CHAR NO-UNDO.
   
   ASSIGN llTake = FALSE
          lcRole = fCustRoles(BUFFER Customer).  

   /* invoicing customers or users */
   CASE icCustType:
   WHEN "invcust" THEN llTake = (SUBSTRING(lcRole,2,1) = "1").
   WHEN "user"    THEN llTake = (SUBSTRING(lcRole,3,1) = "1").
   WHEN "all"     THEN llTake = TRUE.
   END CASE.

   IF llTake THEN DO:
   
      CREATE ttCustomer.          
      ASSIGN ttCustomer.CustNum    = Customer.CustNum
             ttCustomer.Address    = Customer.Address
             ttCustomer.PostOffice = Customer.PostOffice
             ttCustomer.InvCust    = Customer.InvCust.
             ttCustomer.CustName   = DYNAMIC-FUNCTION("fDispCustName" 
                                                         IN ghFunc1,
                                                      BUFFER Customer).
      IF Customer.InvCust NE Customer.CustNum THEN DO:
         FIND bInvCust WHERE bInvCust.CustNum = Customer.InvCust 
            NO-LOCK NO-ERROR.
         IF AVAILABLE bInvCust THEN 
            ttCustomer.ICName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                                 BUFFER bInvCust).
      END.           
      ELSE ttCustomer.ICName = ttCustomer.CustName.
   END.

END FUNCTION.

CASE icCustType:
WHEN "invcust" THEN ac-hdr = "INV.CUSTOMERS OF ".
WHEN "user"    THEN ac-hdr = "USERS OF ".
WHEN "all"     THEN ac-hdr = " ".
END CASE.

IF icMainType = "AgrCust" THEN DO:

   ac-hdr = ac-hdr + "AGR.CUSTOMER " + STRING(iiMainCust).
   
   FOR EACH Customer NO-LOCK WHERE
            Customer.AgrCust = iiMainCust:
            
      fPickCustomer(icCustType).
   END. 
END.

ELSE IF icMainType = "InvCust" THEN DO:

   ac-hdr = ac-hdr + "INV.CUSTOMER " + STRING(iiMainCust).
   
   FOR EACH Customer NO-LOCK WHERE
            Customer.InvCust = iiMainCust:
            
      fPickCustomer(icCustType).
   END. 

END. 

ELSE RETURN.

cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE ttCustomer THEN ASSIGN
   Memory       = recid(ttCustomer)
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
        FIND ttCustomer WHERE recid(ttCustomer) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttCustomer THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttCustomer).
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

        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
         CHOOSE ROW ttCustomer.CustNum ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttCustomer.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
         CHOOSE ROW ttCustomer.CustName ;(uchoose.i;) NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) ttCustomer.CustName WITH FRAME sel.
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
        FIND ttCustomer WHERE recid(ttCustomer) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttCustomer THEN
              ASSIGN FIRSTrow = i Memory = recid(ttCustomer).
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
           IF NOT AVAILABLE ttCustomer THEN DO:
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
                rtab[1] = recid(ttCustomer)
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
           IF NOT AVAILABLE ttCustomer THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttCustomer).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttCustomer WHERE recid(ttCustomer) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttCustomer THEN DO:
           Memory = recid(ttCustomer).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttCustomer THEN Memory = recid(ttCustomer).
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
           FIND ttCustomer WHERE recid(ttCustomer) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        CLEAR FRAME f1.
        liCustNum = 0.
        UPDATE liCustNum WITH FRAME f1.
        HIDE FRAME f1 NO-PAUSE.

        IF liCustNum > 0 THEN DO:
           FIND FIRST ttCustomer WHERE 
                      ttCustomer.CustNum >= liCustNum
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE ttCustomer THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(ttCustomer)
              must-print = TRUE
              order      = 1.
              
           NEXT LOOP.
        END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

        cfc = "puyr". run ufcolor.
        ehto = 9. RUN ufkey. ufkey = TRUE.
        CLEAR FRAME f2.
        lcCustName = "".
        UPDATE lcCustName WITH FRAME f2.
        HIDE FRAME f2 NO-PAUSE.

        IF lcCustName > "" THEN DO:
           FIND FIRST ttCustomer WHERE 
                      ttCustomer.CustName >= lcCustName
           NO-LOCK NO-ERROR.

           IF NOT AVAILABLE ttCustomer THEN DO:
              MESSAGE "None found !".    
              PAUSE 2 NO-MESSAGE.
              NEXT BROWSE.
           END.

           ASSIGN
              memory     = RECID(ttCustomer)
              must-print = TRUE
              order      = 2.
              
           NEXT LOOP.
        END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO: 
        RUN local-find-this(FALSE).

        IF AVAILABLE ttCustomer THEN DO:
           siirto = string(ttCustomer.CustNum).
           LEAVE LOOP.
        END.
     END.

     ELSE IF LOOKUP(nap,"HOME,H") > 0 THEN DO : /* FIRST record */
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttCustomer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttCustomer) must-print = TRUE.
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
      FIND ttCustomer WHERE recid(ttCustomer) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ttCustomer WHERE recid(ttCustomer) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN 
      FIND FIRST ttCustomer USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST ttCustomer USE-INDEX CustName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-LAST:

   IF order = 1 THEN 
      FIND LAST ttCustomer USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST ttCustomer USE-INDEX CustName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 THEN 
      FIND NEXT ttCustomer USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT ttCustomer USE-INDEX CustName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 THEN 
      FIND PREV ttCustomer USE-INDEX CustNum NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND PREV ttCustomer USE-INDEX CustName NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:

    RUN local-find-others.
       
    CLEAR FRAME sel NO-PAUSE.
    DISPLAY 
       ttCustomer.CustNum    
       ttCustomer.CustName
       ttCustomer.Address
       ttCustomer.PostOffice
       ttCustomer.InvCust
       ttCustomer.ICName
    WITH FRAME sel.

END PROCEDURE.

PROCEDURE local-find-others.

END PROCEDURE.


