~/* ----------------------------------------------------------------------
  MODULE .......: ServiceLimit.P
  TASK .........: UPDATE Billing Event Items
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 28-09-99
  CHANGED ......: 04-10-99 jp  urights added
                  10-01-00 jpo Added BillType
                  28-12-01 jpo NEW FRAME FORMAT AND BillCycle FIELD
                  11.03.03 tk  tokens
                  24.06.03 aam FromDate, ToDate, InclAmt etc.,
                               eventlog
                  12.10.05 kl update ServiceLimit.SLName
                  12.01.06 jp  FromDate,ValidTo
                  
  Version ......: Cubio
  ---------------------------------------------------------------------- */

{Syst/commali.i}  
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'mobsub'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhServiceLimit AS HANDLE NO-UNDO.
   lhServiceLimit = BUFFER ServiceLimit:HANDLE.
   RUN StarEventInitialize(lhServiceLimit).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhServiceLimit).
   END.

END.

DEF  INPUT PARAMETER     Groupcode LIKE ServiceLimit.Groupcode NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR SLCode LIKE ServiceLimit.SLCode NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 2.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 14.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  init 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcCode       AS CHAR                   NO-UNDO. 
DEF VAR lcUnit       AS CHAR                   NO-UNDO. 
DEF VAR lctype       AS CHAR                   NO-UNDO.
DEF VAR servicelimit AS CHAR                   NO-UNDO.
DEF VAR llFirstMonthCalc AS LOG                NO-UNDO.
DEF VAR llLastMonthCalc  AS LOG                NO-UNDO.

form
    ServiceLimit.GroupCode      /* COLUMN-LABEL FORMAT */
    ServiceLimit.SLCode       
    ServiceLimit.SLName        FORMAT "X(40)"

WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    " ServiceLimit Groups " + groupcode    
    FRAME sel.

form
    ServiceLimit.GroupCode    COLON 25 FORMAT "X(20)"
    ServiceLimitGroup.GroupName NO-LABEL SKIP                              
    ServiceLimit.SLCode     COLON 25 FORMAT "x(16)"                           
    ServiceLimit.SLSeq      COLON 25 LABEL "Sequence number" SKIP
    ServiceLimit.SLName     COLON 25 LABEL "Servicelimit name" 
    ServiceLimit.DialType   COLON 25 
      lctype  FORMAT  "x(30)" NO-LABEL         SKIP
    ServiceLimit.InclAmt      COLON 25 SKIP
    ServiceLimit.InclUnit     COLON 25 
       lcUnit FORMAT "X(30)" NO-LABEL SKIP
    ServiceLimit.BDestLimit   COLON 25 
    ServiceLimit.ValidFrom    COLON 25   SKIP
    ServiceLimit.ValidTo      COLON 25   SKIP   
    llFirstMonthCalc COLON 25
      FORMAT "Relative/Full"
      LABEL "1. month service limit"
      HELP "First month calculation method, (F)ull or (R)elative" SKIP
    llLastMonthCalc COLON 25
      FORMAT "Relative/Full"
      LABEL "Last month service limit"
      HELP "Last month calculation method, (F)ull or (R)elative" SKIP
    Servicelimit.web          COLON 25
WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek Billing Event Item  BY  GroupCode */
    GroupCode
    HELP "Enter Price List Code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND P-LIST "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Billing Event Item  BY SLCode */
    SLCode
    HELP "Enter SLCode"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SLCode "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FUNCTION fDispUnit RETURNS LOGICAL
   (iiUnit AS INT).
   
   IF iiUnit > 0 THEN 
      lcUnit = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "Servicelimit","InclUnit",STRING(iiUnit)).
   ELSE lcUnit = "".
    
   DISPLAY lcUnit WITH FRAME lis.
   
   RETURN (iiUnit = 0 OR (iiUnit > 0 AND lcUnit > "")). 
   
END FUNCTION.

FUNCTION fDispType RETURNS LOGICAL
   (iiUnit AS INT).
   
   
   FIND dialtype WHERE 
        dialtype.dialtype = iiunit NO-LOCK NO-ERROR.

   IF AVAIL dialtype THEN lctype = dialtype.dtname. 
   ELSE lctype = "".
   
   DISPLAY lcType WITH FRAME lis.
      
         
END FUNCTION.
         


cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Price List,By SLCode  ,By 3, By 4".


FIND FIRST ServiceLimit
WHERE ServiceLimit.GroupCode = GroupCode NO-LOCK NO-ERROR.
IF AVAILABLE ServiceLimit THEN ASSIGN
   memory       = recid(ServiceLimit)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No billing event items available !" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   ASSIGN
      memory       = ?
      must-print   = FALSE
      must-add     = TRUE.
END.      

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a ServiceLimit  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        ehto = 9. RUN Syst/ufkey.
        REPEAT TRANSACTION WITH FRAME lis:
           CLEAR FRAME lis NO-PAUSE.

           DISP Groupcode @ servicelimit.groupcode 
           WITH FRAME lis. pause 0.
            PROMPT-FOR 
              ServiceLimit.SLCode
              ServiceLimit.SLName
           EDITING:
              READKEY.
              IF LOOKUP(KEYLABEL(LASTKEY),"f4") > 0 THEN DO:
                 UNDO add-row, leave add-row.
              
              END.
              
              
              IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                 PAUSE 0.
                 
                 IF FRAME-FIELD = "GroupCode" THEN DO:
                    IF INPUT ServiceLimit.GroupCode = "" THEN 
                    UNDO add-row, LEAVE add-row.

                    FIND ServiceLimitGroup WHERE 
                         ServiceLimitGroup.Brand = gcBrand AND 
                         ServiceLimitGroup.GroupCode = 
                       INPUT  servicelimit.GroupCode
                    NO-LOCK NO-ERROR.

                   IF NOT AVAIL ServiceLimitGroup THEN DO:
                       BELL.
                       MESSAGE "Unknown ServiceLimit Group !".
                       NEXT.
                    END.
                    DISP ServiceLimitGroup.GroupName.
                 END.   

                 ELSE IF FRAME-FIELD = "SLCode" THEN DO:
                    IF INPUT ServiceLimit.SLCode = "" THEN DO:
                       NEXT-PROMPT ServiceLimit.GroupCode.
                       NEXT.
                    END.
                 END.   
              END.
              APPLY LASTKEY.
           END.   
           DEF VAR laskuri as i no-undo.

           find last servicelimit no-lock use-index slseq no-error.
           
           if not avail servicelimit then laskuri = 1.
           else laskuri = servicelimit.slseq + 1.

           CREATE ServiceLimit.
           ASSIGN
           Servicelimit.SLSEQ     = Laskuri
           ServiceLimit.GroupCode = GroupCode
           ServiceLimit.SLCode  = INPUT FRAME lis ServiceLimit.SLCode
           ServiceLimit.SLName  = INPUT FRAME lis ServiceLimit.SLName.
           RUN local-update-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhServiceLimit).
           
           ASSIGN
           memory = recid(ServiceLimit)
           xrecid = memory.
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST ServiceLimit
      WHERE ServiceLimit.GroupCode = GroupCode NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ServiceLimit THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND ServiceLimit WHERE recid(ServiceLimit) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ServiceLimit THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ServiceLimit).
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
        ufk[1]= 0  ufk[2]= 0 ufk[3]= 2601 ufk[4]= 2602
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0) 
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0 ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW ServiceLimit.GroupCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServiceLimit.GroupCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW ServiceLimit.SLCode ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) ServiceLimit.SLCode WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND ServiceLimit WHERE recid(ServiceLimit) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ServiceLimit THEN
              ASSIGN FIRSTrow = i memory = recid(ServiceLimit).
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

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ServiceLimit THEN DO:
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
                rtab[1] = recid(ServiceLimit)
                memory  = rtab[1].
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
           IF NOT AVAILABLE ServiceLimit THEN DO:
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
              rtab[FRAME-DOWN] = recid(ServiceLimit).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND ServiceLimit WHERE recid(ServiceLimit) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ServiceLimit THEN DO:
           memory = recid(ServiceLimit).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ServiceLimit THEN memory = recid(ServiceLimit).
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
           memory = rtab[FRAME-DOWN].
           FIND ServiceLimit WHERE recid(ServiceLimit) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */
     /* Search BY column 1 */

     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO:
         
        RUN local-find-this(FALSE).

        FIND FIRST DayCampaign WHERE
                   DayCampaign.Brand      = gcBrand AND 
                   DayCampaign.DCevent    = ServiceLimit.GroupCode AND
                   DayCampaign.ValidTo   >= Today NO-LOCK NO-ERROR.
        IF AVAIL DayCampaign AND
          LOOKUP(DayCampaign.dcType,"4,8") > 0
        THEN RUN Mm/proglimit(INPUT ServiceLimit.slseq).
        ELSE RUN Mm/servicelimittarget(INPUT ServiceLimit.slseq).
        ufkey = TRUE.
        NEXT loop.
     END.
                    
     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:
     
        RUN local-find-this(FALSE).
             
        RUN Mm/mservicelimit.p(INPUT 0 , 
                                  ServiceLimit.dialtype,
                                  Servicelimit.slseq ).
        ufkey = TRUE.
        NEXT loop.
     END.
                                          
     
     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSAction:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          ServiceLimit.GroupCode    
          ServiceLimit.SLCode     
          
          ServiceLimit.slname.

       RUN local-find-NEXT.
       IF AVAILABLE ServiceLimit THEN memory = recid(ServiceLimit).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ServiceLimit THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             memory = recid(ServiceLimit).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          ServiceLimit.GroupCode    
          ServiceLimit.SLCode     
          
          ServiceLimit.slname.

       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhServiceLimit).
           
           DELETE ServiceLimit.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST ServiceLimit
           WHERE ServiceLimit.GroupCode = GroupCode) THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(TRUE).
       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY
          ServiceLimit.SLCode
          ServiceLimit.GroupCode
          ServiceLimit.SLName.

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhServiceLimit).
       
       RUN local-update-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhServiceLimit).
       
       RUN local-disp-row.
       xrecid = recid(ServiceLimit).
       LEAVE.
     END.
           
     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(ServiceLimit) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(ServiceLimit) must-print = TRUE.
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
      FIND ServiceLimit WHERE recid(ServiceLimit) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND ServiceLimit WHERE recid(ServiceLimit) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST ServiceLimit
       WHERE ServiceLimit.GroupCode = GroupCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST ServiceLimit
       WHERE ServiceLimit.GroupCode = GroupCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT ServiceLimit
       WHERE ServiceLimit.GroupCode = GroupCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV ServiceLimit
       WHERE ServiceLimit.GroupCode = GroupCode NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:

       RUN local-find-others.
       
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       ServiceLimit.GroupCode
       ServiceLimit.SLCode
       ServiceLimit.SLName

       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
       FIND ServicelimitGroup  WHERE 
            ServiceLimitGroup.Brand = gcBrand AND
            ServicelimitGroup.GroupCode = ServiceLimit.GroupCode 
        NO-LOCK NO-ERROR. 
END PROCEDURE.

PROCEDURE local-update-record:
   REPEAT ON ENDKEY UNDO, LEAVE:
      RUN local-find-others. 
      ASSIGN
         llFirstMonthCalc = (ServiceLimit.FirstMonthCalc = 1)
         llLastMonthCalc  = (ServiceLimit.LastMonthCalc = 1).
      
      DISP
          ServiceLimitGroup.GroupName   WHEN AVAIL ServiceLimitGroup
          ServiceLimit.GroupCode
          ServiceLimit.SLSeq
          ServiceLimit.DialType
          ServiceLimit.InclAmt
          ServiceLimit.BDestLimit
          ServiceLimit.InclUnit
          ServiceLimit.ValidFrom
          ServiceLimit.ValidTo
          llFirstMonthCalc
          llLastMonthCalc
          Servicelimit.web
      WITH FRAME lis.

      fDispUnit(ServiceLimit.InclUnit).
      fDispType(ServiceLimit.dialtype).
      
      IF lcRight = "RW" THEN DO:
      
         UPDATE
             ServiceLimit.SLName
             ServiceLimit.DialType
             ServiceLimit.InclAmt
             ServiceLimit.InclUnit
             ServiceLimit.BDestLimit
             ServiceLimit.ValidFrom
             ServiceLimit.Validto
             llFirstMonthCalc
             llLastMonthCalc
             Servicelimit.web
              
         WITH FRAME lis EDITING:
             READKEY.
             IF keylabel(LASTKEY) = "F9" AND 
                FRAME-FIELD = "InclUnit" 
             THEN DO:
                RUN Help/h-tmscodes(INPUT "ServiceLimit", /* TableName */
                                     "InclUnit", /* FieldName */
                                     "ServiceLimit", /* GroupCode */
                               OUTPUT lcCode).
              
                IF lcCode ne "" AND lcCode NE ? THEN DO:
                   fDispUnit(INTEGER(lcCode)).
                   DISPLAY INTEGER(lcCode) ;& ServiceLimit.InclUnit 
                   WITH FRAME lis.
                END.
                ehto = 9.
                RUN Syst/ufkey.
                NEXT. 
             END.

             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
             
                PAUSE 0.


                IF FRAME-FIELD = "InclUnit" THEN DO:
                   IF NOT fDispUnit(INPUT INPUT ServiceLimit.InclUnit)
                   THEN DO:
                      BELL.
                      MESSAGE "Unknown unit".
                      NEXT.
                   END.
                END.
                ELSE IF FRAME-FIELD = "Dialtype" THEN DO:
                   
                   FIND FIRST dialtype WHERE 
                       dialtype.dialtype = input frame lis servicelimit.dialtype
                   no-lock NO-ERROR.
                   IF NOT AVAIL dialtype 
                   THEN DO:
                      BELL.
                      MESSAGE "Unknown type".
                      NEXT.
                   END.
                   DISP dialtype.dtname @ lctype WITH FRAME lis.
                END.
                ELSE IF FRAME-FIELD = "web" THEN DO:
                    IF INPUT FRAME lis web > 1 THEN DO:
                       BELL.
                       MESSAGE "0=dont show,1=show".
                       NEXT-PROMPT web. NEXT.
                    END.
                END.
             END.
             APPLY LASTKEY.
          END. /* EDITING */

          ASSIGN 
             ServiceLimit.FirstMonthCalc = INTEGER(llFirstMonthCalc)
             ServiceLimit.LastMonthCalc = INTEGER(llLastMonthCalc).
             
      END.
      ELSE PAUSE.
      LEAVE.
   END.
END PROCEDURE.
