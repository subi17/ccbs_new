/* ----------------------------------------------------------------------
  MODULE .......: CampRow
  TASK .........: UPDATEs table CampRow
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 22.01.04
  CHANGED ......: 29.11.06/aam type 5 TopUp
  Version ......: M15
  ---------------------------------------------------------------------- */

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CampRow'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhCampRow AS HANDLE NO-UNDO.
   lhCampRow = BUFFER CampRow:HANDLE.
   RUN StarEventInitialize(lhCampRow).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhCampRow).
   END.

END.

DEF INPUT PARAMETER icCampaign AS CHAR NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  init 1.
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
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcTitle      AS CHAR                   NO-UNDO. 
DEF VAR lcItem       AS CHAR                   NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO.
DEF VAR lcType       AS CHAR                   NO-UNDO.
DEF VAR lcTypeLst    AS CHAR                   NO-UNDO.
DEF VAR lcCLIType    AS CHAR                   NO-UNDO.
DEF VAR lcBillItem   AS CHAR                   NO-UNDO. 
DEF VAR lcHelpProg   AS CHAR                   NO-UNDO.

form
    CampRow.CRowType
    lcType    COLUMN-LABEL "Type" FORMAT "X(10)"
    CampRow.CRowItem 
    lcItem    COLUMN-LABEL "Item Name" FORMAT "X(16)"
    CampRow.CLIType
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
    lcTitle
    FRAME sel.

form
    CampRow.CRowType   COLON 20
       lcType NO-LABEL FORMAT "X(30)"
    CampRow.CRowItem   COLON 20
       lcItem NO-LABEL FORMAT "X(30)" 
    CampRow.CLIType    COLON 20
       lcCLIType NO-LABEL FORMAT "X(30)" 
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FUNCTION fRowItem RETURNS LOGICAL
   (iiType AS INT,
    icItem AS CHAR,
    ilDisp AS LOG).

   lcItem = "".
   
   CASE iiType:
   WHEN 1 THEN DO:
      FIND PriceList WHERE 
           PriceList.Brand     = gcBrand AND
           PriceList.PriceList = icItem 
         NO-LOCK NO-ERROR.
      IF AVAILABLE PriceList THEN lcItem = PriceList.PLName.
   END. 
   WHEN 2 THEN DO:
      FIND DiscPlan WHERE 
           DiscPlan.Brand    = gcBrand AND
           DiscPlan.DiscPlan = icItem 
         NO-LOCK NO-ERROR.
      IF AVAILABLE DiscPlan THEN lcItem = DiscPlan.DPName.
   END.
   WHEN 3 THEN DO:
      FIND FatGroup WHERE
           FatGroup.Brand = gcBrand AND
           FatGroup.FtGrp = icItem 
         NO-LOCK NO-ERROR.
      IF AVAILABLE FatGroup THEN lcItem = FatGroup.FTGName.
   END.
   WHEN 4 OR WHEN 5 THEN DO:
      FIND FeeModel WHERE
           FeeModel.Brand    = gcBrand AND
           FeeModel.FeeModel = icItem 
         NO-LOCK NO-ERROR.
      IF AVAILABLE FeeModel THEN lcItem = FeeModel.FeeName.
   END.
   
   OTHERWISE lcItem = "". 

   END CASE.       
   
   IF ilDisp THEN 
   DISPLAY lcItem WITH FRAME lis.  
    
END FUNCTION.

FUNCTION fRowType RETURNS LOGICAL
   (iiType AS INT,
    ilDisp AS LOG).
   
      IF iiType >= 1 AND iiType <= 5
      THEN lcType = ENTRY(iiType,lcTypeLst).
      ELSE lcType = "".
            
      IF ilDisp THEN                
      DISPLAY lcType WITH FRAME lis.
               
      RETURN TRUE.
   
END FUNCTION.

DO i = 1 TO 5:
   lcTypeLst = lcTypeLst +
               DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "CampRow","CRowType",STRING(i)) + ",".
END.

lcHelpProg = "nnplse,h-dplan,h-fatgroup,h-bevent,h-bevent".

FIND Campaign WHERE 
     Campaign.Brand    = gcBrand AND
     Campaign.Campaign = icCampaign NO-LOCK.
ASSIGN lcTitle = " ROWS FOR CAMPAIGN: " +
                 STRING(Campaign.CaName) + " ".

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Row Type ,    ,   , By 4".

RUN local-find-first.

IF AVAILABLE CampRow THEN ASSIGN
   Memory       = recid(CampRow)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = FALSE.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

   IF must-add THEN DO:  /* Add a CampRow  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           PROMPT-FOR CampRow.CRowType
           EDITING:
             READKEY.

             IF KEYLABEL(LASTKEY) = "F9" THEN DO:
                    
                IF FRAME-FIELD = "CRowType" THEN DO:
                                   
                   RUN Help/h-tmscodes(INPUT "CampRow",     /* TableName */
                                        "CRowType",    /* FieldName */
                                        "Campaign",    /* GroupCode */
                                 OUTPUT lcCode).
                                 
                   IF lcCode ne "" AND lcCode NE ? THEN DO:
                      fRowType(INTEGER(lcCode),TRUE).
                      DISPLAY INTEGER(lcCode) ;& CampRow.CRowType
                      WITH FRAME lis.
                   END.
                END.

                ehto = 9.
                RUN Syst/ufkey.
                NEXT.
                
             END.   
                                                      
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "CRowType" THEN DO:
                   IF INPUT FRAME lis CampRow.CRowType = 0
                   THEN LEAVE. 
                   
                   fRowType(INPUT INPUT FRAME lis CampRow.CRowType,TRUE).
                   IF lcType = "" THEN DO:
                      BELL.
                      MESSAGE "Unknown type.".
                      NEXT.
                   END.
                END.

             END.
          
             APPLY LASTKEY.
            
           END.

           IF INPUT FRAME lis CampRow.CRowType = 0
           THEN LEAVE add-row.

           CREATE CampRow.
           ASSIGN
           CampRow.Brand    = gcBrand 
           CampRow.Campaign = icCampaign
           CampRow.CRowType = INPUT FRAME lis CampRow.CRowType.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCampRow).

           ASSIGN
           Memory = recid(CampRow)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CampRow OF Campaign
       NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CampRow THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CampRow WHERE recid(CampRow) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CampRow THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CampRow).
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
        ufk[1]= 0   ufk[2]= 0  ufk[3]= 0  ufk[4]= 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 0  ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CampRow.CRowType ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CampRow.CRowType WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,8,f8") = 0 THEN DO:
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
        FIND CampRow WHERE recid(CampRow) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CampRow THEN
              ASSIGN FIRSTrow = i Memory = recid(CampRow).
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
           IF NOT AVAILABLE CampRow THEN DO:
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
                rtab[1] = recid(CampRow)
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
           IF NOT AVAILABLE CampRow THEN DO:
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
              rtab[FRAME-DOWN] = recid(CampRow).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CampRow WHERE recid(CampRow) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CampRow THEN DO:
           Memory = recid(CampRow).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CampRow THEN Memory = recid(CampRow).
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
           FIND CampRow WHERE recid(CampRow) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW"
     THEN DO:  /* add */
        {Syst/uright2.i}
        must-add = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW"
     THEN DO TRANSACTION:  /* DELETE */
       {Syst/uright2.i}
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       CampRow.CRowType CampRow.CRowItem .

       RUN local-find-NEXT.
       IF AVAILABLE CampRow THEN Memory = recid(CampRow).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CampRow THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CampRow).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CampRow.CRowType CampRow.CRowItem .

       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCampRow).

           DELETE CampRow.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CampRow OF Campaign) THEN DO:
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
       RUN local-find-this(IF lcRight = "RW" THEN TRUE ELSE FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCampRow).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CampRow.CRowType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCampRow).

       RUN local-disp-row.
       xrecid = recid(CampRow).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CampRow) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CampRow) must-print = TRUE.
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
      FIND CampRow WHERE recid(CampRow) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CampRow WHERE recid(CampRow) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CampRow 
          OF Campaign USE-INDEX Campaign
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CampRow
          OF Campaign USE-INDEX Campaign
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CampRow
          OF Campaign USE-INDEX Campaign
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CampRow
          OF Campaign USE-INDEX Campaign
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CampRow.CRowType 
       lcType
       CampRow.CRowItem
       lcItem 
       CampRow.CLIType
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

   fRowItem(CampRow.CRowType,CampRow.CRowItem,FALSE).
   
   fRowType(CampRow.CRowType,FALSE).
   
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:
   
      RUN local-find-others.
      
      lcCLIType = "".
      IF CampRow.CLIType > "" THEN DO:
         FIND CLIType WHERE
              CLIType.Brand   = CampRow.Brand AND
              CLIType.CLIType = CampRow.CLIType NO-LOCK NO-ERROR.
         IF AVAILABLE CLIType THEN lcCLIType = CLIType.CLIName.
      END.

      lcBillItem = "".
      IF CampRow.BillCode > "" THEN DO:
         FIND BillItem WHERE
              BillItem.Brand    = CampRow.Brand AND
              BillItem.BillCode = CampRow.BillCode NO-LOCK NO-ERROR.
         IF AVAILABLE BillItem THEN lcBillItem = BillItem.BIName.
      END.

      fRowType(CampRow.CRowType,TRUE).
      
      DISP CampRow.CRowType
           CampRow.CRowItem
           lcItem 
           CampRow.CLIType
           lcCLIType
      WITH FRAME lis.
      
      IF lcRight = "RW" THEN DO:
      
         ehto = 9. RUN Syst/ufkey.
       
         UPDATE
         CampRow.CRowItem
         CampRow.CLIType
         WITH FRAME lis EDITING:
         
             READKEY.

             IF KEYLABEL(LASTKEY) = "F9" AND
                LOOKUP(FRAME-FIELD,"CRowItem") > 0
             THEN DO:
                    
                IF FRAME-FIELD = "CRowItem" THEN DO:
                   ASSIGN siirto = ?
                          i = INTEGER(INPUT FRAME lis CampRow.CRowType).
                   
                   IF i >= 1 AND i <= 5
                   THEN RUN VALUE(ENTRY(i,lcHelpProg)).
                   
                   IF siirto NE ? THEN 
                   DISPLAY siirto @ CampRow.CRowItem WITH FRAME lis.
                END.

                ehto = 9.
                RUN Syst/ufkey.
                NEXT.
                
             END.   
                                                      
             IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
                PAUSE 0.

                IF FRAME-FIELD = "CRowItem" THEN DO:
                
                   fRowItem(INPUT INPUT FRAME lis CampRow.CRowType,
                            INPUT INPUT FRAME lis CampRow.CRowItem,
                            TRUE).
                   IF lcItem = "" THEN DO:
                      BELL.
                      MESSAGE "Unknown Item.".
                      NEXT.
                   END.
                END.
                
                ELSE IF FRAME-FIELD = "CLIType" THEN DO:
                   IF INPUT FRAME lis CampRow.CLIType = "" 
                   THEN DISPLAY "" @ lcCLIType.
                   ELSE DO:
                   
                      FIND CLIType WHERE 
                           CLIType.Brand   = CampRow.Brand AND
                           CLIType.CLIType = INPUT FRAME lis CampRow.CLIType 
                      NO-LOCK NO-ERROR.
                      IF NOT AVAIL CLIType THEN DO:
                         BELL.
                         MESSAGE "Unknown CLIType !".
                         NEXT.
                      END.
                      DISP CLIType.CLIName @ lcCLIType.
                   END.   
                END.
             END.
          
             APPLY LASTKEY.
          
          END. /* EDITING */
          
      END.
      
      ELSE PAUSE MESSAGE "Press ENTER to continue".
           
      LEAVE.
   END.
END PROCEDURE.

