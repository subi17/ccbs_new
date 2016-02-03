/* ----------------------------------------------------------------------
  MODULE .......: CTServPac
  TASK .........: UPDATEs table CTServPac
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 03.12.04
  CHANGED ......: 30.11.06/aam ServType
                  27.03.07/aam dates to ctservel,
                               inform user from elements if dates are changed
  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable CTServPac

{Syst/commali.i}

{Syst/eventval.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'CTServPac'}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhCTServPac AS HANDLE NO-UNDO.
   lhCTServPac = BUFFER CTServPac:HANDLE.
   RUN StarEventInitialize(lhCTServPac).

   DEFINE VARIABLE lhCTServEl AS HANDLE NO-UNDO.
   lhCTServEl = BUFFER CTServEl:HANDLE.
   RUN StarEventInitialize(lhCTServEl).

   DEFINE VARIABLE lhCTServAttr AS HANDLE NO-UNDO.
   lhCTServAttr = BUFFER CTServAttr:HANDLE.
   RUN StarEventInitialize(lhCTServAttr).


   ON F12 ANYWHERE DO:
      RUN eventview2(lhCTServPac).
   END.

END.

DEF INPUT PARAMETER icCLIType AS CHAR NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR lcServPac    AS CHAR                   NO-UNDO. 
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 15.
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
DEF VAR lcCLIName    AS CHAR                   NO-UNDO.
DEF VAR lcSPName     AS CHAR                   NO-UNDO.
DEF VAR ldtFromDate  AS DATE                   NO-UNDO.
DEF VAR liQty        AS INT                    NO-UNDO. 
DEF VAR lcType       AS CHAR                   NO-UNDO. 
DEF VAR lcCode       AS CHAR                   NO-UNDO. 

form
    CTServPac.CLIType    
    lcCLIName           COLUMN-LABEL "Name" FORMAT "X(15)"
    CTServPac.ServPac   FORMAT "X(12)"  
    lcSPName            COLUMN-LABEL "Name" FORMAT "X(16)"
    CTServPac.ServType  
    CTServPac.FromDate
    CTServPac.ToDate      
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) " " + ynimi +
       " CLIType Services "  + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}

form
    CTServPac.CLIType     COLON 20
       VALIDATE(INPUT CTServPac.CLIType = "" OR
                CAN-FIND(CLIType WHERE 
                         CLIType.Brand   = gcBrand AND
                         CLIType.CLIType = INPUT CTServPac.CLIType),
                "Unknown CLI Type")       
       lcCLIName NO-LABEL FORMAT "X(30)" SKIP
    CTServPac.ServPac     COLON 20 
       FORMAT "X(12)"
       VALIDATE(INPUT CTServPac.ServPac = "" OR
                CAN-FIND(ServPac WHERE 
                         ServPac.Brand   = gcBrand AND
                         ServPac.ServPac = INPUT CTServPac.ServPac),
                "Unknown service package")
       lcSPName NO-LABEL FORMAT "X(30)" SKIP
    CTServPac.ServType    COLON 20
       lcType NO-LABEL FORMAT "x(30)"          
    CTServPac.FromDate    COLON 20
       VALIDATE(INPUT CTServPac.FromDate NE ?,
                "Date is mandatory")
    CTServPac.ToDate      COLON 20 
       VALIDATE(INPUT CTServPac.ToDate >= INPUT CTServPac.FromDate,
                "End date cannot be before beginning date")
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

form /* seek  ServPac */
    "Brand ......:" lcBrand skip
    "Service Pack:" lcServPac
    HELP "Enter service package"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Service Package "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

FORM
   " All service components that are not yet copied to this CLI type " SKIP
   " with date given below will now be copied." SKIP(1)
   ldtFromDate AT 2 
      LABEL "Valid Date"
      HELP "Valid from date that will be marked to components"
      FORMAT "99-99-9999"
      SKIP(1)
   WITH OVERLAY ROW 10 CENTERED SIDE-LABELS TITLE " COPY COMPONENTS " 
        FRAME fCopyComp.

 
FUNCTION fServTypeName RETURNS LOGIC
   (iiServType AS INT):
   
   lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                             "CTServPac",
                             "ServType", 
                             STRING(iiServType)).
END FUNCTION.
 
cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-find-first.

IF AVAILABLE CTServPac THEN ASSIGN
   Memory       = recid(CTServPac)
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
    END.

   IF must-add THEN DO:  /* Add a CTServPac  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANSACTION WITH FRAME lis ON ENDKEY UNDO, LEAVE:

           DISPLAY icCLIType @ CTServPac.CLIType WITH FRAME lis.
           PROMPT-FOR CTServPac.ServPac.

           IF INPUT FRAME lis CTServPac.ServPac = ""
           THEN LEAVE add-row.

           IF CAN-FIND(FIRST CTServPac WHERE 
                     CTServPac.Brand   = lcBrand   AND
                     CTServPac.CLIType = icCLIType AND
                     CTServPac.ServPac = INPUT FRAME lis CTServPac.ServPac)
           THEN DO:
              MESSAGE 
              "Service Package" 
              icCLIType "/"
              INPUT FRAME lis CTServPac.ServPac
              "already exists !"
              VIEW-AS ALERT-BOX
              ERROR.
              NEXT.
           END.

           CREATE CTServPac.
           ASSIGN
           CTServPac.Brand   = lcBrand
           CTServPac.CLIType = icCLIType
           CTServPac.ServPac = INPUT FRAME lis CTServPac.ServPac.

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhCTServPac).

           /* copy also service components */
           RUN pCopyComponents (CTServPac.ServPac,
                                CTServPac.FromDate).
           
           ASSIGN
           Memory = recid(CTServPac)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST CTServPac WHERE CTServPac.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE CTServPac THEN LEAVE LOOP.
      NEXT LOOP.
   END.

   PrintPage:
   DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND CTServPac WHERE recid(CTServPac) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE CTServPac THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(CTServPac).
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
        ufk[1]= 30  ufk[2]= 0 ufk[3]= 28  ufk[4]= 250
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0)
        ufk[7]= 73  
        ufk[8]= 8 
        ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW CTServPac.ServPac ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) CTServPac.ServPac WITH FRAME sel.
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
        FIND CTServPac WHERE recid(CTServPac) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE CTServPac THEN
              ASSIGN FIRSTrow = i Memory = recid(CTServPac).
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
           IF NOT AVAILABLE CTServPac THEN DO:
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
                rtab[1] = recid(CTServPac)
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
           IF NOT AVAILABLE CTServPac THEN DO:
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
              rtab[FRAME-DOWN] = recid(CTServPac).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND CTServPac WHERE recid(CTServPac) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE CTServPac THEN DO:
           Memory = recid(CTServPac).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE CTServPac THEN Memory = recid(CTServPac).
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
           FIND CTServPac WHERE recid(CTServPac) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". run ufcolor.
       ehto = 9. RUN ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       DISPLAY lcBrand WITH FRAME F1.
       UPDATE lcBrand WHEN gcAllBrand
              lcServPac WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.

       IF lcServPac > "" THEN DO:
          FIND FIRST CTServPac WHERE 
                     CTServPac.Brand    = lcBrand   AND
                     CTServPac.CLIType  = icCLIType AND
                     CTServPac.ServPac >= lcServPac
          NO-LOCK NO-ERROR.

          IF NOT fRecFound(1) THEN NEXT BROWSE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* Package Contains */
       RUN local-find-this(FALSE).                                        
       
       /*  elements of a CLI type */
       RUN ctservel(icCLIType,
                    CTServPac.ServPac,
                    CTServPac.FromDate,
                    CTServPac.ToDate).

       ufkey = TRUE.
       NEXT LOOP.
     END.

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
       CTServPac.CLIType CTServPac.ServPac 
       CTServPac.FromDate CTServPac.ToDate.

       RUN local-find-NEXT.
       IF AVAILABLE CTServPac THEN Memory = recid(CTServPac).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE CTServPac THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(CTServPac).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
       CTServPac.CLIType CTServPac.ServPac
       CTServPac.FromDate CTServPac.ToDate.

       IF ok THEN DO:

           FOR EACH CTServEl OF CTServPac EXCLUSIVE-LOCK:
              FOR EACH CTServAttr OF CTServEl EXCLUSIVE-LOCK:
                 IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCTServAttr).
                 DELETE CTServAttr.
              END.
              
              IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCTServEl).
              DELETE CTServEl.
           END.
           
           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhCTServPac).

           DELETE CTServPac.

           /* was LAST record DELETEd ? */
           IF NOT CAN-FIND(FIRST CTServPac WHERE CTServPac.Brand = lcBrand) 
           THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"7,f7") > 0 AND ufk[7] > 0 THEN DO:  
       RUN local-find-this(FALSE).                                        

       PAUSE 0.
       VIEW FRAME fCopyComp.
       
       /* copy all components and their attributes from the general package 
          to cli type */
       REPEAT ON ENDKEY UNDO, LEAVE:
                
          ehto = 9.
          RUN ufkey.
          
          PAUSE 0.
          UPDATE ldtFromDate WITH FRAME fCopyComp.
          
          ASSIGN ufk    = 0
                 ufk[1] = 7
                 ufk[5] = 795
                 ufk[8] = 8
                 ehto   = 0.
          RUN ufkey.        
          
          IF toimi = 5 AND ldtFromDate NE ? THEN DO:
          
             RUN pCopyComponents (CTServPac.ServPac,
                                  ldtFromDate).
            
             LEAVE.
          END.

          ELSE IF toimi = 8 THEN LEAVE.

       END.

       HIDE FRAME fCopyComp NO-PAUSE.
          
       ufkey = TRUE.
       NEXT LOOP.
     END.


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhCTServPac).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY CTServPac.CLIType.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhCTServPac).

       RUN local-disp-row.
       xrecid = recid(CTServPac).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(CTServPac) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(CTServPac) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

fCleanEventObjects().


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND CTServPac WHERE recid(CTServPac) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND CTServPac WHERE recid(CTServPac) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST CTServPac 
          WHERE CTServPac.Brand   = lcBrand AND
                CTServPac.CLIType = icCLIType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST CTServPac 
          WHERE CTServPac.Brand   = lcBrand AND
                CTServPac.CLIType = icCLIType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT CTServPac 
          WHERE CTServPac.Brand   = lcBrand AND
                CTServPac.CLIType = icCLIType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV CTServPac 
          WHERE CTServPac.Brand   = lcBrand AND
                CTServPac.CLIType = icCLIType
        NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       CTServPac.CLIType 
       lcCLIName
       CTServPac.ServPac
       lcSPName
       CTServPac.ServType
       CTServPac.FromDate
       CTServPac.ToDate
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.

      ASSIGN lcCLIName = ""
             lcSPName  = "".
             
      FIND CLIType WHERE
           CLIType.Brand   = CTServPac.Brand AND
           CLIType.CLIType = CTServPac.CLIType NO-LOCK NO-ERROR.
      IF AVAILABLE CLIType THEN lcCLIName = CLIType.CLIName.
           
      FIND ServPac WHERE
           ServPac.Brand   = CTServPac.Brand AND
           ServPac.ServPac = CTServPac.ServPac NO-LOCK NO-ERROR.
      IF AVAILABLE ServPac THEN lcSPName = ServPac.SPName.
           
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.

      fServTypeName(CTServPac.ServType).
          
      DISP CTServPac.CLIType
           lcCLIName
           CTServPac.ServPac
           lcSPName
           CTServPac.FromDate
           CTServPac.ToDate
           CTServPac.ServType
           lcType
      WITH FRAME lis.
      
      ASSIGN 
         ufk = 0
         ufk[1] = 7 WHEN lcRight = "RW"
         ufk[8] = 8
         ehto   = 0.
      RUN ufkey.
         
      IF toimi = 1 THEN DO:
      
         ehto = 9. RUN ufkey.
      
         FIND CURRENT CTServPac EXCLUSIVE-LOCK.
         
         REPEAT WITH FRAME lis ON ENDKEY UNDO, LEAVE:

          UPDATE
          CTServPac.ServType
          CTServPac.FromDate
          CTServPac.ToDate
          WITH FRAME lis EDITING:
         
             READKEY.

             IF KEYLABEL(LASTKEY) = "F9" AND 
                  FRAME-FIELD = "ServType"
             THEN DO:

                RUN h-tmscodes(INPUT "CTServPac",    /* TableName */
                                     "ServType",  /* FieldName */
                                     "MobSub",  /* GroupCode */
                               OUTPUT lcCode).

                IF lcCode ne "" AND lcCode NE ? THEN DO:
                   DISPLAY lcCode @ CTServPac.ServType WITH FRAME lis.   
                END.   
                  
                ehto = 9.
                RUN ufkey.
                NEXT. 
             END.
    
             ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
             DO WITH FRAME lis:
             
                PAUSE 0.
                
                IF FRAME-FIELD = "ServType" THEN DO:
                   fServTypeName(INPUT INPUT FRAME lis CTServPac.ServType).
                   DISPLAY lcType WITH FRAME lis.
                   
                   IF lcType = "" THEN DO:
                      MESSAGE "Unknown package type".
                      NEXT.
                   END.
                END.
             END.

             APPLY LASTKEY.
          END. /* EDITING */
          
          IF CTServPac.FromDate ENTERED OR
             CTServPac.ToDate ENTERED 
          THEN DO:
             MESSAGE "Dates have been changed. Make sure that beginning"
                     "dates of elements have been assigned accordingly."
             VIEW-AS ALERT-BOX INFORMATION.
          END.
         
          LEAVE.
          
         END.
      END.
      
      LEAVE.
   END.
   
END PROCEDURE.

/* copy service components and their attributes to clitype */
PROCEDURE pCopyComponents:

   DEF INPUT PARAMETER icServPac   AS CHAR NO-UNDO.
   DEF INPUT PARAMETER idtFromdate AS DATE NO-UNDO.
                                   
   ASSIGN i     = 0
          liQty = 0.
             
   FOR EACH ServEl NO-LOCK WHERE
            ServEl.Brand   = gcBrand AND
            ServEl.ServPac = icServPac,
      FIRST ServCom NO-LOCK WHERE
            ServCom.Brand   = gcBrand AND
            ServCom.ServCom = ServEl.ServCom:
                      
      FIND CTServEl WHERE
           CTServEl.Brand    = gcBrand         AND
           CTServEl.CLIType  = icCLIType       AND
           CTServEl.ServPac  = icServPac       AND
           CTServEl.ServCom  = ServEl.ServCom  AND
           CTServEl.FromDate = idtFromDate NO-LOCK NO-ERROR.
    
      IF NOT AVAILABLE CTServEl THEN DO:                
         CREATE CTServEl.
         ASSIGN CTServEl.Brand      = gcBrand
                CTServEl.CTServEl   = NEXT-VALUE(CTServEl)
                CTServEl.CLIType    = icCLIType
                CTServEl.ServPac    = icServPac
                CTServEl.ServCom    = ServEl.ServCom
                CTServEl.FromDate   = idtFromDate
                CTServEl.ChgAllowed = ServCom.SCChgable
                CTServEl.ServType   = ServCom.ServType
                CTServEl.DefValue   = ServEl.SeValue
                i                   = i + 1. 
      END.

      FOR EACH ServAttr NO-LOCK WHERE
               ServAttr.Brand   = gcBrand AND
               ServAttr.ServCom = ServEl.ServCom:
                         
         IF CAN-FIND(CTServAttr WHERE
                     CTServAttr.CTServEl = CTServEl.CTServEl AND
                     CTServAttr.ServAttr = ServAttr.ServAttr AND
                     CTServAttr.FromDate = idtFromDate)
         THEN NEXT.
                   
         CREATE CTServAttr.
         ASSIGN CTServAttr.CTServEl   = CTServEl.CTServEl
                CTServAttr.ServAttr   = ServAttr.ServAttr
                CTServAttr.FromDate   = idtFromDate
                CTServAttr.DefValue   = ServAttr.DefValue
                CTServAttr.ChgAllowed = ServAttr.ScChgable
                liQty                 = liQty + 1.
      END.
                          
   END.
          
   MESSAGE i "components and" liQty "attributes"
             "were copied to package" icServPac
   VIEW-AS ALERT-BOX
   TITLE " DONE ".
 

END PROCEDURE.

