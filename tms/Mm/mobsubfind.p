/* ----------------------------------------------------------------------
  MODULE .......: mobsubfind.p
  TASK .........: find mobsub with customer 
  APPLICATION ..: nn
  AUTHOR .......: 
  CREATED ......: 
  CHANGED ......: 
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'Customer'}
{Syst/eventval.i}
{Func/lib/accesslog.i}


DEFINE INPUT PARAMETER  icCriteria AS C NO-UNDO.
DEFINE INPUT PARAMETER  icValue    AS C NO-UNDO.

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.Var:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
   lhCustomer = BUFFER Customer:HANDLE.
   RUN StarEventInitialize(lhCustomer).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhCustomer).
   END.

END.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Customer      LIKE Customer.CustNum     NO-UNDO.
DEF VAR CustName      LIKE Customer.CustName      NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR liQty        AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcSurname1   AS CHAR                   NO-UNDO.
DEF VAR lcFirstName  AS CHAR                   NO-UNDO.
DEF VAR lcSurName2   AS CHAR                   NO-UNDO.
DEF VAR lcCompany    AS CHAR                   NO-UNDO.
DEF VAR lcProgram    AS CHAR                   NO-UNDO.

lcProgram = PROGRAM-NAME(1).

form
    Customer.CustNum     /* COLUMN-LABEL FORMAT */
    Customer.CustName     /* COLUMN-LABEL FORMAT */
    Customer.OrgID
    liQty   COLUMN-LABEL "Amount Of Subscriptions"

WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.Var:cfc)   
    TITLE COLOR VALUE(Syst.Var:ctc) " " + Syst.Var:ynimi +
    "  SUBSCRIPTION MENU  "
    + string(TODAY,"99-99-99") + " "
    FRAME sel.


Syst.Var:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.Var:ccc = Syst.Var:cfc.
VIEW FRAME sel.

orders = "  By Code  ,  By Name  ,By 3, By 4".

IF icValue ne "ID" THEN DO:
   IF NUM-ENTRIES(icValue,"|") > 1 THEN DO:
      ASSIGN
         lcSurName1  = ENTRY(1,icValue,"|")
         lcSurName2  = ENTRY(2,icValue,"|")
         lcCompany   = ENTRY(3,icValue,"|").
   END.
   ELSE lcSurname1 = icvalue.
END.
   
  

run LOCAL-FIND-FIRST.

IF AVAILABLE Customer THEN DO: 
   ASSIGN
   Memory       = recid(Customer)
   must-print   = TRUE
   must-add     = FALSE.
END.    
ELSE DO:
   MESSAGE 
   "Cannot find any Customers!"
   VIEW-AS ALERT-BOX .
   
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a Customer  */
      ASSIGN Syst.Var:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        Syst.Var:ehto = 9. RUN Syst/ufkey.p.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           PROMPT-FOR Customer.CustNum
           VALIDATE
              (Customer.CustNum NOT ENTERED OR
              NOT CAN-FIND(Customer using  Customer.CustNum),
              "Billing RepType " + string(INPUT Customer.CustNum) +
              " already exists !").
           IF INPUT FRAME lis Customer.CustNum = "" THEN 
           LEAVE add-row.
           CREATE Customer.
           ASSIGN
           Customer.CustNum = INPUT FRAME lis Customer.CustNum.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCustomer).

           ASSIGN
           Memory = recid(Customer)
           xrecid = Memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN LOCAL-FIND-FIRST.
      IF NOT AVAILABLE Customer THEN LEAVE LOOP.
      NEXT LOOP.
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
        Syst.Var:ufk[1] = 0  
        Syst.Var:ufk[2] = 2245 
        Syst.Var:ufk[3] = 0
        Syst.Var:ufk[4] = 0
        Syst.Var:ufk[5] = 0
        Syst.Var:ufk[6] = 0
        Syst.Var:ufk[7] = 0
        Syst.Var:ufk[8]= 8 Syst.Var:ufk[9]= 1
        Syst.Var:ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.

      
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW Customer.CustNum {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Customer.CustNum WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW Customer.CustName {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.Var:ccc) Customer.CustName WITH FRAME sel.
      END.

      IF rtab[FRAME-LINE] = ? THEN NEXT.

      Syst.Var:nap = keylabel(LASTKEY).

      IF LOOKUP(Syst.Var:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.Var:nap,"cursor-left") > 0 THEN DO:
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

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN Syst.Var:nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(Syst.Var:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
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
      ELSE IF LOOKUP(Syst.Var:nap,"cursor-down") > 0 THEN DO
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
      ELSE IF LOOKUP(Syst.Var:nap,"PREV-page,page-up,-") > 0 THEN DO:
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
     ELSE IF LOOKUP(Syst.Var:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
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

     ELSE IF (LOOKUP(Syst.Var:nap,"2,f2") > 0 OR LOOKUP(Syst.Var:nap, "enter,return") > 0)
          AND lcRight = "RW" THEN DO:  
        RUN local-find-this (FALSE).
        RUN CreateReadAccess("Customer", Syst.Var:katun, Customer.CustNum, lcProgram, "CustNum" ).
        IF icCriteria      = "ID" OR 
           icCriteria      = "AGRNAME" THEN
           RUN Mm/mobsub.p(Customer.CustNum, "AGREEMENT").
        ELSE IF icCriteria = "UserName" THEN
           RUN Mm/mobsub.p(Customer.CustNum, "USER").
       
        ufkey = true.
                              
     END.
     
     ELSE IF LOOKUP(Syst.Var:nap,"5,f5") > 0 AND Syst.Var:ufk[5] > 0 AND lcRight = "RW" THEN DO:
     /* add */
        must-add= TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"6,f6") > 0 AND Syst.Var:ufk[6] > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(Syst.Var:ctc)
       Customer.CustNum Customer.CustName .

       RUN local-find-NEXT.
       IF AVAILABLE Customer THEN Memory = recid(Customer).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE Customer THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(Customer).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(Syst.Var:ccc)
       Customer.CustNum Customer.CustName .
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCustomer).

           DELETE Customer.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST Customer
           /* srule */) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.Var:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(Customer) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(Customer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.Var:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
Syst.Var:si-recid = xrecid.



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
       
   IF order = 1 AND icCriteria = "ID" THEN FIND FIRST Customer
       WHERE Customer.Brand = Syst.Var:gcBrand AND 
             Customer.OrgID = icValue NO-LOCK NO-ERROR.
   ELSE IF order = 1 AND icCriteria = "AGRNAME" THEN 
       FIND FIRST Customer  WHERE 
                  Customer.Brand    = Syst.Var:gcBrand                  AND 
                  
                  IF lcSurname1 > "" THEN
                     Customer.CustName  BEGINS  lcSurname1        
                  ELSE 
                     TRUE 
                  
                  AND   

                  IF lcSurName2 > "" THEN
                     Customer.SurName2  BEGINS  lcSurname2        
                  ELSE
                     TRUE
                  
                  AND   
                  
                  IF lcSurName1 EQ "" AND lcSurname2 EQ "" THEN   
                     Customer.CompanyName BEGINS lcCompany        
                  ELSE
                    TRUE
                  
                  AND
                  CAN-FIND(FIRST MobSub WHERE
                                   MobSub.AgrCust = Customer.Custnum)
                  
                  NO-LOCK NO-ERROR.
   ELSE IF order = 1 AND icCriteria = "USERNAME" THEN
          FIND FIRST Customer  WHERE
                     Customer.Brand    = Syst.Var:gcBrand               AND
                     Customer.CustName  BEGINS  lcSurname1     AND
                     Customer.FirstName BEGINS  lcFirstName    AND
                     Customer.SurName2  BEGINS lcSurname2    AND
                     Customer.CompanyName BEGINS lcCompany     AND
                     CAN-FIND(FIRST MobSub WHERE 
                                    MobSub.CustNum = Customer.CustNum)
                     NO-LOCK NO-ERROR.
                       
END PROCEDURE.

PROCEDURE local-find-LAST:

       IF order = 1 AND icCriteria = "ID" THEN FIND LAST Customer
              WHERE Customer.Brand = Syst.Var:gcBrand  AND 
                    Customer.OrgID = icValue NO-LOCK NO-ERROR.
       ELSE IF order = 1 AND icCriteria = "AGRNAME" THEN
          FIND LAST Customer  WHERE
                    Customer.Brand    = Syst.Var:gcBrand             AND
                    Customer.CustName  BEGINS  lcSurname1   AND
                    Customer.SurName2  BEGINS  lcSurname2        AND
                    Customer.CompanyName BEGINS lcCompany        AND
                    CAN-FIND(FIRST MobSub WHERE
                                   MobSub.AgrCust = Customer.Custnum)
                    NO-LOCK NO-ERROR.
       ELSE IF order = 1 AND icCriteria = "USERNAME" THEN
          FIND LAST Customer  WHERE
                    Customer.Brand    = Syst.Var:gcBrand             AND
                    Customer.CustName    BEGINS  lcSurname1   AND
                    Customer.FirstName   BEGINS  lcFirstName  AND
                    Customer.SurName2    BEGINS  lcSurname2    AND
                    Customer.CompanyName BEGINS  lcCompany     AND
                    CAN-FIND(FIRST MobSub WHERE 
                                   MobSub.CustNum = Customer.CustNum)
                    NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:

   IF order = 1 AND icCriteria = "ID" THEN 
   FIND NEXT Customer
       WHERE Customer.Brand = Syst.Var:gcBrand AND 
            Customer.OrgID = icValue NO-LOCK NO-ERROR.
   ELSE IF order = 1 AND icCriteria = "AGRNAME" THEN
   FIND NEXT Customer  WHERE
             Customer.Brand    = Syst.Var:gcBrand  AND
             Customer.CustName  BEGINS  lcSurname1  AND
             Customer.SurName2  BEGINS  lcSurname2        AND
             Customer.CompanyName BEGINS lcCompany        AND
             CAN-FIND(FIRST MobSub WHERE
                            MobSub.AgrCust = Customer.Custnum)
             NO-LOCK NO-ERROR.
   ELSE IF order = 1 AND icCriteria = "USERNAME" THEN
   FIND NEXT Customer  WHERE
             Customer.Brand    = Syst.Var:gcBrand            AND
             Customer.CustName  BEGINS  lcSurname1  AND
             Customer.FirstName BEGINS  lcFirstName AND
             Customer.SurName2  BEGINS lcSurname2   AND
             Customer.CompanyName BEGINS lcCompany  AND
             CAN-FIND(FIRST MobSub WHERE 
                            MobSub.CustNum = Customer.CustNum)
             NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:

   IF order = 1 AND icCriteria = "ID" THEN FIND PREV Customer
      WHERE Customer.brand = Syst.Var:gcBrand AND 
            Customer.OrgID = icValue NO-LOCK NO-ERROR.
   ELSE IF order = 1 AND icCriteria = "AGRNAME" THEN
     FIND PREV Customer  WHERE
             Customer.Brand    = Syst.Var:gcBrand  AND
             Customer.CustName  BEGINS  lcSurname1  AND
             Customer.SurName2  BEGINS  lcSurname2        AND
             Customer.CompanyName BEGINS lcCompany        AND
             CAN-FIND(FIRST MobSub WHERE
                            MobSub.AgrCust = Customer.Custnum)
             NO-LOCK NO-ERROR.
   ELSE IF order = 1 AND icCriteria = "USERNAME" THEN
     FIND PREV Customer  WHERE
               Customer.Brand    = Syst.Var:gcBrand  AND
               Customer.CustName    BEGINS  lcSurname1   AND
               Customer.FirstName   BEGINS  lcFirstName  AND
               Customer.SurName2    BEGINS lcSurname2    AND
               Customer.CompanyName BEGINS lcCompany     AND
               CAN-FIND(FIRST MobSub WHERE 
                              MobSub.CustNum = Customer.CustNum)
               NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       Customer.CustNum 
       Func.Common:mDispCustName(BUFFER Customer) @
                                 Customer.CustName
       Customer.OrgID
       Liqty

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   liqty = 0 .

   if icCriteria = "ID" OR icCriteria = "AGRNAME" THEN 
   FOR EACH Mobsub NO-LOCK WHERE
            Mobsub.Brand    = Syst.Var:gcBrand    AND 
            Mobsub.AgrCust  = Customer.AgrCust.
         
       liqty = liqty + 1.     
   END.
   ELSE IF LOOKUP(icCriteria,"CUSTNAME,USERNAME") > 0 THEN  
   FOR EACH Mobsub NO-LOCK WHERE
            Mobsub.Brand    = Syst.Var:gcBrand    AND
            Mobsub.CustNum = Customer.CustNum.
     liqty = liqty + 1.
  END.
                                                

END PROCEDURE.

PROCEDURE local-UPDATE-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP
          Customer.CustName
      WITH FRAME lis.
      IF lcRight = "RW" THEN DO:
         UPDATE
            Customer.CustName
         WITH FRAME lis.
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.

