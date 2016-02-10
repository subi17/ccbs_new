/* ----------------------------------------------------------------------
  MODULE .......: CallLimit
  TASK .........: Updates table CallLimit
  APPLICATION ..: nn
  AUTHOR .......: jpo
  CREATED ......: 21-01-03
  CHANGED ......: 08.04.03 tk  input parameter credit type
                  19.09.03 tk  converted from old version, brand
                  31.12.04/aam CreditType from SubSer

  VERSION ......: M15
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable CallLimit

{Syst/commali.i}
{Func/fsubser.i}
{Syst/eventval.i}
{Func/func.i}

if llDoEvent THEN DO:
    &GLOBAL-DEFINE STAR_EVENT_USER katun
    {Func/lib/eventlog.i}

    DEF VAR lhCallLimit AS HANDLE NO-UNDO.
    lhCallLimit = BUFFER CallLimit:HANDLE.
    RUN StarEventInitialize(lhCallLimit).

    ON F12 ANYWHERE DO:
        RUN Mc/eventview2.p(lhCallLimit).
    END.
END.

DEF INPUT PARAMETER iCreditType AS INT NO-UNDO init 1.


def  new  shared var siirto AS char.

DEF VAR CustNo      like CallLimit.CustNo        NO-UNDO.
DEF VAR CLI      like CallLimit.CLI        NO-UNDO.
DEF VAR xrecid       AS recid                           init ?.
DEF VAR FIRSTrow     AS int                    NO-UNDO  init 0.
DEF VAR FrmRow       AS int                    NO-UNDO  init 1.
DEF VAR FrmDown      AS int                    NO-UNDO  init 15.
DEF VAR order        AS int                    NO-UNDO  init 1.
DEF VAR orders       AS char                   NO-UNDO.
DEF VAR maxOrder     AS int                    NO-UNDO  init 2.
DEF VAR ufkey        AS log                    NO-UNDO  init true.
DEF VAR delrow       AS int                    NO-UNDO  init 0.
DEF VAR pr-order     AS int                    NO-UNDO.
DEF VAR memory       AS recid                  NO-UNDO.
DEF VAR RowNo        AS int                    NO-UNDO.
DEF VAR must-print   AS log                    NO-UNDO.
DEF VAR must-add     AS log                    NO-UNDO.
DEF VAR ac-hdr       AS char                   NO-UNDO.
DEF VAR rtab         AS recid extent 24        NO-UNDO.
DEF VAR i            AS int                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR username     AS CHAR  FORMAT "X(20)"   NO-UNDO.
DEF VAR CustName     AS CHAR  FORMAT "X(20)"   NO-UNDO.
DEF VAR lcMsg        AS CHAR  EXTENT 5         NO-UNDO.
DEF VAR lcType       AS CHAR                   NO-UNDO.
DEF VAR llfind       AS LOG                    NO-UNDO.
DEF VAR lcdelitype   AS CHAR                   NO-UNDO.
DEF VAR liCreditType AS INT                    NO-UNDO. 

DEF BUFFER xCallLimit FOR CallLimit.

FIND FIRST TMSCodes WHERE
           TMSCodes.TableName = "CreditType" AND
           TMSCodes.FieldName = "CreditType" AND
           TMSCodes.CodeGroup = "CreditType" AND
           TMSCodes.CodeValue = STRING(iCreditType)
NO-LOCK NO-ERROR.

lcType = TMSCodes.CodeName.

form
    CallLimit.Brand      format "x(5)"
    CallLimit.CustNo     /* column-label format */
    CustName             FORMAT "X(12)"   
    CallLimit.CLI        /* column-label*/ format "x(10)"
    CallLimit.Limit   Column-label "Limit"
    CallLimit.dfrom
    CallLimit.dto
    CallLimit.Delitype 
    CallLimit.Delipara format "x(11)" COLUMN-LABEL "Param"


WITH ROW FrmRow width 80 overlay FrmDown down
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " " + lcType + " "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    "Cust num ......" CallLimit.CustNo   SKIP
    "Cust name ....." CustName           SKIP
    "CLI ..........." CallLimit.CLI      SKIP
    "User name ....." username           SKIP
    "Limit ........." CallLimit.Limit    SKIP
    "Valid from ...." CallLimit.dfrom    SKIP
    "Valid to ......" CallLimit.dto      SKIP
    "Deliver type .." CallLimit.Delitype  lcDeliType
    HELP "F9 = Deliver types"            SKIP
    "Deliver params " CallLimit.Delipara                            
    HELP "E-mail addresses/phone number ('[C,'-separated list)" SKIP


WITH  overlay row 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS NO-LABEL
    FRAME lis.

form /* seek  CustNo */
    "Brand :" lcBrand skip
    "CustNo:" CustNo
    HELP "Enter Customer Number of the Call Limit "
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CUSTOMER "
    COLOR VALUE(cfc) NO-labels overlay FRAME f1.

form /* seek  CLI */
    "Brand:" lcBrand skip
    "CLI .:" CLI
    HELP "Enter CLI of the Call Limit"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND CLI "
    COLOR VALUE(cfc) NO-labels overlay FRAME f2.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By CustNum  ,   By CLI   ,By 3, By 4".


RUN local-find-first.
IF AVAILABLE CallLimit THEN ASSIGN
   memory       = recid(CallLimit)
   must-print   = true
   must-add     = false.
ELSE ASSIGN
   memory       = ?
   must-print   = false
   must-add     = true.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder ne 1 THEN DO:
       pr-order = order.
       PUT SCREEN row FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CallLimit  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = false.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis on ENDkey undo ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.
           CREATE CallLimit.
           ASSIGN
           calllimit.brand = lcBrand
           calllimit.dto   = 12/31/2050
           calllimit.clseq = NEXT-VALUE(CallLimit)
           CallLimit.CreditType = iCreditType.           
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCallLimit).
           ASSIGN
           Memory = recid(CallLimit)
           xrecid = Memory.
           LEAVE.

        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = true.

      /* is there ANY record ? */
      FIND FIRST CallLimit WHERE 
                 CallLimit.CreditType = iCreditType AND  
                 CallLimit.Brand = lcBrand 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CallLimit THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   do :
      IF must-print THEN DO:
        UP FRAME-line - 1.
        FIND CallLimit WHERE recid(CallLimit) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose recid is saved into 'memory'.
        starting from row 'delrow' */

        /* IF a row was recently DELETEd ... */
        IF delrow > 0 THEN DOwn delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CallLimit THEN DO:
              RUN local-disp-row.
              rtab[FRAME-line] = recid(CallLimit).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-line] = ?.
           END.
           IF FRAME-line = FRAME-down THEN LEAVE.
           down.
        END.
        up FRAME-line - 1.
        down FIRSTrow.
        ASSIGN FIRSTrow = 0
               must-print = false.
        PAUSE 0 NO-MESSAGE.

        /* Now there is one page DISPLAYed and the cursor is on the
        upermost row, waiting for a 'choose' */
      END. /* must-print = true */
   END. /* PrintPage */

   /* IF a row was recently DELETEd: */
   IF delrow > 0 THEN DOwn delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel on ENDkey undo, return:

      IF ufkey THEN DO:
        ASSIGN
        ufk[1]= 702  ufk[2]= 653 ufk[3]= 0  ufk[4]= 0
        ufk[5]= 5  ufk[6]= 4 ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = false.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        choose row CallLimit.CustNo ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CallLimit.CustNo WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        choose row CallLimit.CLI ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CallLimit.CLI WITH FRAME sel.
      END.

      IF rtab[FRAME-line] = ? THEN NEXT.

      nap = keylabel(LASTkey).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-line].
        FIND CallLimit WHERE recid(CallLimit) = memory NO-LOCK.
        do i = 1 to FRAME-line - 1:
           RUN local-find-PREV.
           IF AVAILABLE CallLimit THEN
              ASSIGN FIRSTrow = i memory = recid(CallLimit).
           ELSE LEAVE.
        END.
        must-print = true.
        NEXT LOOP.
      END.

      IF rtab[FRAME-line] = ? and NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTkey).

      /* PREVious row */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-line = 1 THEN DO:
           RUN local-find-this(false).
           RUN local-find-PREV.
           IF NOT AVAILABLE CallLimit THEN DO:
              MESSAGE "YOU ARE ON THE FIRST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* PREVious was found */
              SCROLL DOWN.
              RUN local-disp-row.
              DO i = FRAME-down to 2 by -1:
                 rtab[i] = rtab[i - 1].
              END.
              ASSIGN
                rtab[1] = recid(CallLimit)
                memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious row */

      /* NEXT row */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-line = FRAME-down THEN DO:
           RUN local-find-this(false).
           RUN local-find-NEXT.
           IF NOT AVAILABLE CallLimit THEN DO:
              MESSAGE "YOU ARE ON THE LAST ROW !".
              BELL. PAUSE 1 NO-MESSAGE.
              NEXT BROWSE.
           END.
           ELSE DO:
              /* NEXT row was found */
              SCROLL UP.
              RUN local-disp-row.
              DO i = 1 to FRAME-down - 1:
                 rtab[i] = rtab[i + 1].
              END.
              rtab[FRAME-down] = recid(CallLimit).
              /* save recid of uppermost row */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT row */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND CallLimit WHERE recid(CallLimit) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CallLimit THEN DO:
           memory = recid(CallLimit).

           /* reverse 1 page */
           DO RowNo = 1 to (FRAME-down - 1):
              RUN local-find-PREV.
              IF AVAILABLE CallLimit THEN memory = recid(CallLimit).
              ELSE RowNo = FRAME-down.
           END.
           must-print = true.
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
       /* Put Cursor on downmost Row */
       IF rtab[FRAME-down] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost row was NOT empty*/
           memory = rtab[FRAME-down].
           FIND CallLimit WHERE recid(CallLimit) = memory NO-LOCK.
           must-print = true.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search by column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO on ENDkey undo, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME f1.
       SET CustNo WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF CustNo ENTERED THEN DO:
          FIND FIRST CallLimit WHERE 
                     CallLimit.CustNo >= CustNo AND 
                     CallLimit.CreditType = iCreditType AND
                     CallLimit.Brand = lcBrand
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search by col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO on ENDkey undo, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = true.
       CLEAR FRAME F2.
       SET CLI WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF CLI ENTERED THEN DO:
          FIND FIRST CallLimit WHERE 
                     CallLimit.CLI >= CLI AND 
                     CallLimit.CreditType = iCreditType AND
                     CallLimit.Brand = lcBrand
          USE-INDEX CLI NO-LOCK NO-ERROR.

          IF NOT fRecFound(2) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-2 */
     ELSE IF LOOKUP(nap,"5,f5") > 0 THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-line.
       RUN local-find-this (false).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CallLimit.Brand   
       CallLimit.CustNo  
       CustName          
       CallLimit.CLI     
       CallLimit.Limit   
       CallLimit.dfrom
       CallLimit.dto
       CallLimit.Delitype
       CallLimit.Delipara.

       RUN local-find-NEXT.
       IF AVAILABLE CallLimit THEN memory = recid(CallLimit).
       ELSE DO:
          /* read back the record that is to be  removed */
          RUN local-find-this (false).                     

          RUN local-find-PREV.
          IF AVAILABLE CallLimit THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(CallLimit).
          END.
       END.

       /* FIND back the row that is to be removed */
       RUN local-find-this(true).

       ASSIGN ok = false.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          CallLimit.Brand   
          CallLimit.CustNo  
          CustName          
          CallLimit.CLI     
          CallLimit.Limit   
          CallLimit.dfrom
          CallLimit.dto
          CallLimit.Delitype
          CallLimit.Delipara.
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCallLimit).

           DELETE CallLimit.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CallLimit
           WHERE CallLimit.CreditType = iCreditType AND 
                 CallLimit.Brand = lcBrand) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = true.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* undo DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(true).
       ASSIGN ac-hdr = " CHANGE " ufkey = true ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CallLimit.CustNo.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCallLimit).

       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted To Cancel this Change Transaction */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCallLimit).

       RUN local-disp-row.
       xrecid = recid(CallLimit).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(CallLimit) must-print = true.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(CallLimit) must-print = true.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.



PROCEDURE local-find-this:

    def INPUT parameter exlock as lo NO-undo.

    IF exlock THEN
      find CallLimit WHERE recid(CallLimit) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       find CallLimit WHERE recid(CallLimit) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN 
          FIND FIRST CallLimit WHERE  
                     CallLimit.CreditType = iCreditType AND 
                     CallLimit.Brand = lcBrand  
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND FIRST CallLimit USE-INDEX CLI WHERE 
                     CallLimit.CreditType = iCreditType AND 
                     CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN 
          FIND LAST CallLimit WHERE 
                    CallLimit.CreditType = iCreditType AND 
                    CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND LAST CallLimit USE-INDEX CLI WHERE 
                    CallLimit.CreditType = iCreditType AND 
                    CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN 
          FIND NEXT CallLimit WHERE 
                    CallLimit.CreditType = iCreditType AND 
                    CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND NEXT CallLimit USE-INDEX CLI WHERE 
                    CallLimit.CreditType = iCreditType AND 
                    CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN 
          FIND PREV CallLimit WHERE  
                    CallLimit.CreditType = iCreditType AND 
                    CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN 
          FIND PREV CallLimit USE-INDEX CLI WHERE 
                    CallLimit.CreditType = iCreditType AND 
                    CallLimit.Brand = lcBrand 
          NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.

       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CallLimit.Brand
       CallLimit.CustNo  
       CustName
       CallLimit.CLI     
       CallLimit.Limit
       CallLimit.dfrom
       CallLimit.dto
       CallLimit.Delitype 
       CallLimit.Delipara 
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   Find Customer where Customer.CustNum = CallLimit.CustNo NO-LOCK NO-ERROR.

   IF avail Customer then custname = Customer.CustName.
   ELSE custname = "".

   Find Mobsub WHERE
        mobsub.CLI = CallLimit.CLI NO-LOCK NO-ERROR.

   IF avail mobsub then 
   Username = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER Customer).

FIND FIRST TMSCodes WHERE
           TMSCodes.TableName = "CallLimit" AND
           TMSCodes.FieldName = "delitype" AND
           TMSCodes.CodeGroup = "delitype" AND
           TMSCodes.CodeValue = STRING(calllimit.delitype)
NO-LOCK NO-ERROR.

IF avail tmscodes then lcDeliType = TMSCodes.CodeName.


END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others.
      DISP

      CallLimit.CustNo   
      CallLimit.CLI     
      CallLimit.Limit
      CallLimit.dfrom
      CallLimit.dto
      CallLimit.Delitype 
      CallLimit.Delipara
      username
      custname
      lcdelitype
      WITH FRAME lis.
      UPDATE
      CallLimit.CustNo    WHEN  NEW CallLimit
      CallLimit.CLI       WHEN  NEW CallLimit AND iCreditType NE 1
      CallLimit.Limit
      CallLimit.dfrom
      CallLimit.dto
      CallLimit.Delitype 
      CallLimit.Delipara 

      WITH FRAME lis
      EDITING:
             READKEY.
             IF FRAME-FIELD = "delitype" AND keylabel(lastkey) = "F9" 
             THEN DO:
                RUN Help/h-tmscodes(INPUT "CallLimit",  /* TableName*/
                                     "DeliType", /* FieldName */
                                     "Delitype", /* GroupCode */
                               OUTPUT siirto).

                ASSIGN
                   callLimit.Delitype = INT(siirto) WHEN siirto NE ?.
                   disp CallLimit.DeliType with frame lis.
                NEXT.
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.
                IF FRAME-FIELD = "CustNo" THEN DO:
                   FIND Customer WHERE Customer.CustNum =
                   INPUT FRAME lis CallLimit.CustNo NO-LOCK NO-ERROR.
                   IF NOT AVAIL Customer THEN DO:
                      BELL.
                      MESSAGE "Unknown Customer !".
                      NEXT.
                   END.
                   DISP Customer.CustName @ CustName WITH FRAME lis.
                END.

                ELSE IF FRAME-FIELD = "CLI" THEN DO:
                   FIND MobSub WHERE 
                        mobsub.CLI = INPUT FRAME lis CallLimit.CLI
                   NO-LOCK NO-ERROR.

                   IF NOT AVAIL mobsub AND
                      CallLimit.CreditType >= 2  AND
                      custno  > 1000 THEN DO:
                      BELL.
                      MESSAGE 
                      "Unknown Mobile Number".
                      NEXT-PROMPT cli. NEXT.

                   END.   

                   IF AVAIL mobsub THEN DO:
                      liCreditType = fCreditTypeValue(MobSub.MsSeq,
                                                      OUTPUT i).
                      IF mobsub.Custnum ne Customer.CustNum THEN DO:
                         MESSAGE
                         "Mismatched Customer/Mobile Subscription numbers!".
                         NEXT.
                      END.
                      IF liCreditType NE iCreditType THEN DO:
                         MESSAGE
                         "Wrong Credit Type for Mobile Subscription !".
                         NEXT.
                      END.
                   END.   
                   ELSE IF FRAME lis CallLimit.CLI = "" THEN DO:
                      username = "".
                      disp username with frame lis.
                   END.
                   ELSE IF NOT AVAIL Mobsub THEN DO:
                      BELL.
                      MESSAGE
                      "Unknown Mobile subscription!".
                      NEXT.
                   END.
                   ELSE DO:
                      DISP 
                      DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1, BUFFER                       Customer) @  username
                      with frame lis.
                   END.   
                END.

                ELSE IF FRAME-FIELD = "delitype" THEN DO:
                   RUN Syst/v-tmscodes(INPUT "CallLimit",    /* TableName */
                                        "Delitype", /* FieldName */
                                        "delitype",     /* GroupCode */
                                  INPUT INPUT delitype,
                                  OUTPUT llFind).
                   IF input delitype > 2 AND iCreditType = 2 THEN DO:
                      MESSAGE "Type" input delitype "not allowed !".
                      NEXT.
                   END.

                   IF NOT llFind THEN  DO:
                        NEXT-PROMPT delitype.
                        NEXT.
                   END.

                END.
                ELSE IF FRAME-FIELD = "limit" THEN DO:
                   IF INPUT FRAME lis CallLimit.limit > 100 OR 
                      INPUT FRAME lis CallLimit.limit < 0 THEN DO:
                       BELL.
                       MESSAGE
                       "Limit must be between 0 - 100 !".
                       NEXT.
                   END.
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */

      LEAVE.
   END.
END PROCEDURE.
