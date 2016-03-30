/* ----------------------------------------------------------------------
  MODULE .......: DCServicePackage
  TASK .........: UPDATEs table DCServicePackage
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.11.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

&GLOBAL-DEFINE BrTable DCServicePackage

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DCServicePackage'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDCServicePackage AS HANDLE NO-UNDO.
   lhDCServicePackage = BUFFER DCServicePackage:HANDLE.
   RUN StarEventInitialize(lhDCServicePackage).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2(lhDCServicePackage).
   END.

END.

DEF INPUT PARAMETER icDCEvent  AS CHAR  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR lcServPac    AS CHAR                   NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 12.
DEF VAR order        AS INT                    NO-UNDO  init 1.
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
DEF VAR liPackageID   AS INT                    NO-UNDO.

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR llActive     AS LOG  NO-UNDO INIT TRUE. 

DEF TEMP-TABLE ttPackage NO-UNDO
   FIELD DCServicePackageID AS INT
   FIELD ServPac   AS CHAR
   FIELD ToDate    AS DATE
   INDEX ServPac ServPac ToDate DESC.
   
FORM
    DCServicePackage.ServPac  FORMAT "X(16)" COLUMN-LABEL "Service Package"
    ServPac.SPName            FORMAT "X(30)" COLUMN-LABEL "Name" 
    DCServicePackage.FromDate
    DCServicePackage.ToDate
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       "  SERVICE PACKAGES OF " + STRING(icDCEvent) + " (Active) "
    FRAME sel.

{Func/brand.i}

FORM
    DCServicePackage.Brand        COLON 20
    DCServicePackage.DCEvent      COLON 20
       DayCampaign.DCName NO-LABEL FORMAT "X(35)" SKIP
    DCServicePackage.DCServicePackageID COLON 20 
       LABEL "Row ID"
    DCServicePackage.ServPac      COLON 20 
       ServPac.SPName NO-LABEL FORMAT "X(35)" SKIP
    DCServicePackage.FromDate    COLON 20
    DCServicePackage.ToDate      COLON 20    
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FORM 
    "ServPac:" lcServPac FORMAT "X(16)"
    HELP "Enter service package"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Service Pacage "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


RUN pInitTempTable.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN local-Find-First.

IF AVAILABLE ttPackage THEN ASSIGN
   Memory       = recid(ttPackage)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No service packages available" VIEW-AS ALERT-BOX.
      RETURN.
   END.

   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a DCServicePackage  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.

        REPEAT TRANSACTION WITH FRAME lis:

           DISPLAY gcBrand @ DCServicePackage.Brand
                   icDCEvent @ DCServicePackage.DCEvent.

           PROMPT-FOR 
              DCServicePackage.ServPac WITH FRAME lis
           EDITING:
              READKEY.

              IF KEYLABEL(LASTKEY) = "F9" AND FRAME-FIELD = "PayType" THEN DO:

                 ehto = 9.
                 RUN Syst/ufkey.
                 NEXT. 
              END.
                 
              APPLY LASTKEY.
           END.
            
           IF INPUT DCServicePackage.ServPac = "" THEN UNDO, LEAVE ADD-ROW.

           IF  NOT CAN-FIND(FIRST ServPac WHERE
                      ServPac.Brand = gcBrand AND
                      ServPac.ServPac = INPUT DCServicePackage.ServPac) 
           THEN DO:
               MESSAGE "Unknown service package"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
           END.
           
           FIND LAST DCServicePackage USE-INDEX DCServicePackageID 
              NO-LOCK NO-ERROR.
           IF AVAILABLE DCServicePackage THEN 
              liPackageID = DCServicePackage.DCServicePackageID + 1.
           ELSE liPackageID = 1.
           
           CREATE DCServicePackage.
           ASSIGN 
              DCServicePackage.Brand   = gcBrand
              DCServicePackage.DCServicePackageID = liPackageID
              DCServicePackage.DCEvent = icDCEvent
              DCServicePackage.ServPac = INPUT FRAME lis 
                                         DCServicePackage.ServPac.
                                        
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           CREATE ttPackage.
           BUFFER-COPY DCServicePackage TO ttPackage.
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDCServicePackage).

           ASSIGN
           Memory = recid(ttPackage)
           xrecid = RECID(DCServicePackage).  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttPackage THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttPackage WHERE recid(ttPackage) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttPackage THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttPackage).
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
        ufk[1]= 739 
        ufk[2]= 0  
        ufk[3]= 0  
        ufk[4]= 1827 WHEN NOT llActive
        ufk[4]= 1828 WHEN llActive
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7]= 0  
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[4] = 0
           ufk[5] = 11
           ufk[6] = 0
           ufk[7] = 0.
         
        RUN Syst/ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DCServicePackage.ServPac {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DCServicePackage.ServPac WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"4,f4,5,f5,8,f8") = 0 THEN DO:
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
        FIND ttPackage WHERE recid(ttPackage) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttPackage THEN
              ASSIGN FIRSTrow = i Memory = recid(ttPackage).
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
           IF NOT AVAILABLE ttPackage THEN DO:
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
                rtab[1] = recid(ttPackage)
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
           IF NOT AVAILABLE ttPackage THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttPackage).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttPackage WHERE recid(ttPackage) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttPackage THEN DO:
           Memory = recid(ttPackage).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttPackage THEN Memory = recid(ttPackage).
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
           FIND ttPackage WHERE recid(ttPackage) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       PAUSE 0.
       CLEAR FRAME f1.
       SET lcServPac WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       
       IF lcServPac > "" THEN DO:
          FIND FIRST ttPackage USE-INDEX ServPac WHERE 
                     ttPackage.ServPac >= lcServPac
          NO-LOCK NO-ERROR.

          IF NOT AVAILABLE ttPackage THEN NEXT BROWSE.

          ASSIGN
             order      = 1 
             memory     = RECID(ttPackage) 
             must-print = TRUE.

          NEXT LOOP.
       END.
     END. /* Search-1 */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* display filter */
        llActive = NOT llActive.
        RUN pInitTempTable.
        RUN local-find-first.
        ASSIGN 
           Memory = recid(ttPackage) 
           must-print = TRUE
           ufkey = TRUE.
        IF llActive THEN    
           FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"All","Active").
        ELSE FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"Active","All").
   
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           RUN local-find-this(FALSE).
           xRecid = RECID(DCServicePackage).
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST DCServiceComponent OF DCServicePackage) THEN DO:
          MESSAGE "Package has components. Delete not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
 
       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DCServicePackage.ServPac
       DCServicePackage.FromDate
       DCServicePackage.ToDate.

       RUN local-find-NEXT.
       IF AVAILABLE ttPackage THEN Memory = recid(ttPackage).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttPackage THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttPackage).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          DCServicePackage.ServPac
          DCServicePackage.FromDate
          DCServicePackage.ToDate.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDCServicePackage).

           DELETE DCServicePackage.
           DELETE ttPackage.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DCServicePackage THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = RECID(DCServicePackage).
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDCServicePackage).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDCServicePackage).

       RUN local-disp-row.
       xrecid = recid(DCServicePackage).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttPackage) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttPackage) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

FINALLY:

   HIDE FRAME sel NO-PAUSE.
   si-recid = xrecid.

   ehto = 4.
   RUN Syst/ufkey.

   fCleanEventObjects().
END.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN DO:
       FIND ttPackage WHERE recid(ttPackage) = rtab[frame-line(sel)].
       FIND FIRST DCServicePackage WHERE 
          DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID
       EXCLUSIVE-LOCK.
    END.  
    ELSE DO:
       FIND ttPackage WHERE recid(ttPackage) = rtab[frame-line(sel)] NO-LOCK.
       FIND FIRST DCServicePackage WHERE 
          DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID 
          NO-LOCK.
    END.
    
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF icDCEvent ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND FIRST ttPackage NO-LOCK NO-ERROR.
         IF AVAILABLE ttPackage THEN 
            FIND FIRST DCServicePackage WHERE 
            DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID 
            NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF icDCEvent ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND LAST ttPackage NO-LOCK NO-ERROR.
         IF AVAILABLE ttPackage THEN 
            FIND FIRST DCServicePackage WHERE 
            DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID
            NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF icDCEvent ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND NEXT ttPackage NO-LOCK NO-ERROR.
         IF AVAILABLE ttPackage THEN 
            FIND FIRST DCServicePackage WHERE 
            DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID 
            NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF icDCEvent ne ?  THEN DO:
      IF order = 1 THEN DO:
         FIND PREV ttPackage NO-LOCK NO-ERROR.
         IF AVAILABLE ttPackage THEN 
            FIND FIRST DCServicePackage WHERE 
            DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID 
            NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-disp-row:

       FIND FIRST DCServicePackage WHERE 
            DCServicePackage.DCServicePackageID = ttPackage.DCServicePackageID 
            NO-LOCK.
       
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DCServicePackage.ServPac
       ServPac.SPName WHEN AVAILABLE ServPac
       "" WHEN NOT AVAILABLE ServPac @ ServPac.SPName 
       DCServicePackage.FromDate
       DCServicePackage.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
    FIND FIRST ServPac WHERE 
               ServPac.Brand = gcBrand AND
               ServPac.ServPac = DCServicePackage.ServPac NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bDCPackage FOR DCServicePackage.
    
   ActionDetails:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      FIND FIRST DayCampaign WHERE 
           DayCampaign.Brand   = gcBrand AND
           DayCampaign.DCEvent = DCServicePackage.DCEvent NO-LOCK NO-ERROR.
           
      DISP 
         DCServicePackage.Brand          
         DCServicePackage.DCServicePackageID
         DCServicePackage.DCEvent        
         DayCampaign.DCName WHEN AVAILABLE DayCampaign
         DCServicePackage.ServPac
         ServPac.SPName WHEN AVAILABLE ServPac
         DCServicePackage.FromDate
         DCServicePackage.ToDate
       WITH FRAME lis.

      
      IF NOT NEW DCServicePackage THEN REPEAT:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW"
            ufk[4] = 252
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.

         IF toimi = 1 THEN LEAVE.
         
         ELSE IF toimi = 4 THEN 
            RUN Mm/dcservicecomponent.p(DCServicePackage.DCServicePackageID).
            
         ELSE IF toimi = 8 THEN LEAVE ActionDetails.
      END.

      FIND CURRENT DCServicePackage EXCLUSIVE-LOCK.
      
      UPDATE
         DCServicePackage.FromDate
         DCServicePackage.ToDate      
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.

      IF CAN-FIND(FIRST bDCPackage WHERE
                        bDCPackage.Brand      = gcBrand AND
                        bDCPackage.DCEvent    = DCServicePackage.DCEvent  AND
                        bDCPackage.ServPac    = DCServicePackage.ServPac  AND
                        bDCPackage.FromDate <= DCServicePackage.ToDate    AND
                        bDCPackage.ToDate   >= DCServicePackage.FromDate  AND
                        RECID(bDCPackage) NE RECID(DCServicePackage))
      THEN DO:
         MESSAGE "Check dates. Another configuration row already exists"
                 "with overlapping effective period."
         VIEW-AS ALERT-BOX ERROR.
         NEXT. 
      END.
      
      IF NEW DCServicePackage THEN LEAVE.
   
   END.
   
END PROCEDURE.

PROCEDURE pInitTempTable:

   EMPTY TEMP-TABLE ttPackage.
   
   FOR EACH DCServicePackage NO-LOCK WHERE
            DCServicePackage.Brand   = gcBrand AND
            DCServicePackage.DCEvent = icDCEvent:

      IF llActive AND DCServicePackage.ToDate < TODAY THEN NEXT.
            
      CREATE ttPackage.
      BUFFER-COPY DCServicePackage TO ttPackage.
   END.

END PROCEDURE.

