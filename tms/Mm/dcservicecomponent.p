/* ----------------------------------------------------------------------
  MODULE .......: DCServiceComponent
  TASK .........: UPDATEs table DCServiceComponent
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.11.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DCServiceComponent'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER Syst.CUICommon:katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDCServiceComponent AS HANDLE NO-UNDO.
   lhDCServiceComponent = BUFFER DCServiceComponent:HANDLE.
   RUN StarEventInitialize(lhDCServiceComponent).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDCServiceComponent).
   END.

END.

DEF INPUT PARAMETER iiDCPackageID  AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 5.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 10.
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
DEF VAR liComponentID   AS INT                    NO-UNDO.

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR llActive     AS LOG  NO-UNDO INIT TRUE. 
DEF VAR lcServPac    AS CHAR NO-UNDO.

DEF TEMP-TABLE ttComponent NO-UNDO
   FIELD DCServiceComponentID AS INT
   FIELD ServCom   AS CHAR
   FIELD ToDate    AS DATE
   INDEX ServCom ServCom ToDate DESC.
   
FORM
    DCServiceComponent.ServCom  FORMAT "X(12)" COLUMN-LABEL "Component"
    ServCom.SCName              FORMAT "X(30)" COLUMN-LABEL "Name" 
    DCServiceComponent.DefValue COLUMN-LABEL "Setting"
    DCServiceComponent.DefParam FORMAT "X(12)" 
    DCServiceComponent.ToDate
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(Syst.CUICommon:cfc)   
    TITLE COLOR VALUE(Syst.CUICommon:ctc) 
       "  COMPONENTS OF " + DCServicePackage.ServPac + " (Active) "
    FRAME sel.

FORM
    DCServicePackage.DCEvent COLON 20 
       DayCampaign.DCName NO-LABEL FORMAT "X(35)" SKIP
    DCServicePackage.ServPac COLON 20 
       ServPac.SPName NO-LABEL FORMAT "X(35)" SKIP
    DCServiceComponent.DCServiceComponentID COLON 20 
       LABEL "Row ID"
    DCServiceComponent.ServCom  COLON 20 
       ServCom.SCName NO-LABEL FORMAT "X(35)" SKIP
    DCServiceComponent.DefValue COLON 20
    DCServiceComponent.DefParam COLON 20 FORMAT "X(50)"
    DCServiceComponent.FromDate COLON 20
    DCServiceComponent.ToDate   COLON 20    
WITH  OVERLAY ROW 6 centered
    COLOR VALUE(Syst.CUICommon:cfc)
    TITLE COLOR VALUE(Syst.CUICommon:ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.

FIND FIRST DCServicePackage WHERE 
   DCServicePackage.DCServicePackageID = iiDCPackageID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DCServicePackage THEN DO:
   MESSAGE "Service package not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.
VIEW FRAME sel.

RUN pInitTempTable.

RUN local-Find-First.

IF AVAILABLE ttComponent THEN ASSIGN
   Memory       = recid(ttComponent)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No service components available" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a DCServiceComponent  */
      ASSIGN Syst.CUICommon:cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           PROMPT-FOR 
              DCServiceComponent.ServCom WITH FRAME lis
           EDITING:
              READKEY.

              IF KEYLABEL(LASTKEY) = "F9" THEN DO:

                 Syst.CUICommon:gcHelpParam = DCServicePackage.ServPac.
                 RUN Help/h-service_element.p.
                 IF siirto > "" THEN 
                    DISPLAY siirto @ DCServiceComponent.ServCom WITH FRAME lis.
                 Syst.CUICommon:ehto = 9.
                 RUN Syst/ufkey.p.
                 NEXT. 
              END.
                 
              APPLY LASTKEY.
           END.
            
           IF INPUT DCServiceComponent.ServCom = "" THEN UNDO, LEAVE ADD-ROW.

           IF  NOT CAN-FIND(FIRST ServEl WHERE
                      ServEl.Brand = Syst.CUICommon:gcBrand AND
                      ServEl.ServPac = DCServicePackage.ServPac AND
                      ServEl.ServCom = INPUT DCServiceComponent.ServCom) 
           THEN DO:
               MESSAGE "Unknown service component"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
           END.
           
           FIND LAST DCServiceComponent USE-INDEX DCServiceComponentID 
              NO-LOCK NO-ERROR.
           IF AVAILABLE DCServiceComponent THEN 
              liComponentID = DCServiceComponent.DCServiceComponentID + 1.
           ELSE liComponentID = 1.
           
           CREATE DCServiceComponent.
           ASSIGN 
              DCServiceComponent.DCServiceComponentID = liComponentID
              DCServiceComponent.DCServicePackageID = iiDCPackageID
              DCServiceComponent.ServCom = INPUT FRAME lis 
                                         DCServiceComponent.ServCom
              DCServiceComponent.FromDate = TODAY
              DCServiceComponent.ToDate   = DCServicePackage.ToDate.
                                        
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           CREATE ttComponent.
           BUFFER-COPY DCServiceComponent TO ttComponent.
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDCServiceComponent).

           ASSIGN
           Memory = recid(ttComponent)
           xrecid = RECID(DCServiceComponent).  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttComponent THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttComponent WHERE recid(ttComponent) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttComponent THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttComponent).
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
        Syst.CUICommon:ufk[1]= 0 
        Syst.CUICommon:ufk[2]= 0  
        Syst.CUICommon:ufk[3]= 0  
        Syst.CUICommon:ufk[4]= 1827 WHEN NOT llActive
        Syst.CUICommon:ufk[4]= 1828 WHEN llActive
        Syst.CUICommon:ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        Syst.CUICommon:ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        Syst.CUICommon:ufk[7]= 0  
        Syst.CUICommon:ufk[8]= 8 
        Syst.CUICommon:ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF Syst.CUICommon:gcHelpParam > "" THEN ASSIGN
           Syst.CUICommon:ufk[4] = 0
           Syst.CUICommon:ufk[5] = 11
           Syst.CUICommon:ufk[6] = 0
           Syst.CUICommon:ufk[7] = 0.
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DCServiceComponent.ServCom {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(Syst.CUICommon:ccc) DCServiceComponent.ServCom WITH FRAME sel.
      END.

      Syst.CUICommon:nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(Syst.CUICommon:nap,"4,f4,5,f5,8,f8") = 0 THEN DO:
            BELL.
            MESSAGE "You are on an empty row, move upwards !".
            PAUSE 1 NO-MESSAGE.
            NEXT.
         END.
      END.


      IF LOOKUP(Syst.CUICommon:nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(Syst.CUICommon:nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
        FIND ttComponent WHERE recid(ttComponent) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttComponent THEN
              ASSIGN FIRSTrow = i Memory = recid(ttComponent).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      /* PREVious ROW */
      IF LOOKUP(Syst.CUICommon:nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE ttComponent THEN DO:
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
                rtab[1] = recid(ttComponent)
                Memory  = rtab[1].
           END.
        END.
        ELSE up 1.
      END. /* PREVious ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(Syst.CUICommon:nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
        IF FRAME-LINE = FRAME-DOWN THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-NEXT.
           IF NOT AVAILABLE ttComponent THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttComponent).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(Syst.CUICommon:nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttComponent WHERE recid(ttComponent) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttComponent THEN DO:
           Memory = recid(ttComponent).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttComponent THEN Memory = recid(ttComponent).
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
     ELSE IF LOOKUP(Syst.CUICommon:nap,"NEXT-page,page-down,+") > 0 THEN DO WITH FRAME sel:
       /* PUT Cursor on downmost ROW */
       IF rtab[FRAME-DOWN] = ? THEN DO:
           MESSAGE "YOU ARE ON THE LAST PAGE !".
           BELL. PAUSE 1 NO-MESSAGE.
       END.
       ELSE DO: /* downmost ROW was NOT empty*/
           Memory = rtab[FRAME-DOWN].
           FIND ttComponent WHERE recid(ttComponent) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(Syst.CUICommon:nap,"4,f4") > 0 THEN DO:  /* display filter */
        llActive = NOT llActive.
        RUN pInitTempTable.
        RUN local-find-first.
        ASSIGN 
           Memory = recid(ttComponent) 
           must-print = TRUE
           ufkey = TRUE.
        IF llActive THEN    
           FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"All","Active").
        ELSE FRAME sel:TITLE = REPLACE(FRAME sel:TITLE,"Active","All").
   
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"5,f5") > 0 AND lcRight = "RW" THEN DO:  /* add */
        IF Syst.CUICommon:gcHelpParam > "" THEN DO:
           RUN local-find-this(FALSE).
           xRecid = RECID(DCServiceComponent).
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"6,f6") > 0 AND lcRight = "RW" 
     THEN DO TRANSACTION:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       IF CAN-FIND(FIRST DCServiceAttribute OF DCServiceComponent) THEN DO:
          MESSAGE "Component has attributes. Delete not allowed"
          VIEW-AS ALERT-BOX INFORMATION.
          NEXT.
       END.
                         
       /* Highlight */
       COLOR DISPLAY VALUE(Syst.CUICommon:ctc)
       DCServiceComponent.ServCom
       DCServiceComponent.DefParam
       DCServiceComponent.ToDate.

       RUN local-find-NEXT.
       IF AVAILABLE ttComponent THEN Memory = recid(ttComponent).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttComponent THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttComponent).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(Syst.CUICommon:ccc)
          DCServiceComponent.ServCom
          DCServiceComponent.DefParam
          DCServiceComponent.ToDate.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDCServiceComponent).

           DELETE DCServiceComponent.
           DELETE ttComponent.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DCServiceComponent THEN DO:
              CLEAR FRAME sel NO-PAUSE.
              PAUSE 0 NO-MESSAGE.
              LEAVE LOOP.
           END.
           must-print = TRUE.
           NEXT LOOP.
       END.
       ELSE delrow = 0. /* UNDO DELETE */
     END. /* DELETE */

     ELSE IF LOOKUP(Syst.CUICommon:nap,"enter,return") > 0 AND lcRight = "RW" THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF Syst.CUICommon:gcHelpParam > "" THEN DO:
          xRecid = RECID(DCServiceComponent).
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDCServiceComponent).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p.
       Syst.CUICommon:cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDCServiceComponent).

       RUN local-disp-row.
       xrecid = recid(DCServiceComponent).
       LEAVE.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttComponent) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttComponent) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(Syst.CUICommon:nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

FINALLY:
   HIDE FRAME sel NO-PAUSE.
   Syst.CUICommon:si-recid = xrecid.

   Syst.CUICommon:ehto = 4.
   RUN Syst/ufkey.p.

   fCleanEventObjects().
END.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN DO:
       FIND ttComponent WHERE recid(ttComponent) = rtab[frame-line(sel)].
       FIND FIRST DCServiceComponent WHERE 
          DCServiceComponent.DCServiceComponentID = 
             ttComponent.DCServiceComponentID EXCLUSIVE-LOCK.
    END.  
    ELSE DO:
       FIND ttComponent WHERE recid(ttComponent) = rtab[frame-line(sel)] 
          NO-LOCK.
       FIND FIRST DCServiceComponent WHERE 
          DCServiceComponent.DCServiceComponentID = 
             ttComponent.DCServiceComponentID NO-LOCK.
    END.
    
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiDCPackageID ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND FIRST ttComponent NO-LOCK NO-ERROR.
         IF AVAILABLE ttComponent THEN 
            FIND FIRST DCServiceComponent WHERE 
               DCServiceComponent.DCServiceComponentID = 
                  ttComponent.DCServiceComponentID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiDCPackageID ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND LAST ttComponent NO-LOCK NO-ERROR.
         IF AVAILABLE ttComponent THEN 
            FIND FIRST DCServiceComponent WHERE 
               DCServiceComponent.DCServiceComponentID = 
                  ttComponent.DCServiceComponentID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiDCPackageID ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND NEXT ttComponent NO-LOCK NO-ERROR.
         IF AVAILABLE ttComponent THEN 
            FIND FIRST DCServiceComponent WHERE 
               DCServiceComponent.DCServiceComponentID = 
                  ttComponent.DCServiceComponentID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiDCPackageID ne ?  THEN DO:
      IF order = 1 THEN DO:
         FIND PREV ttComponent NO-LOCK NO-ERROR.
         IF AVAILABLE ttComponent THEN 
            FIND FIRST DCServiceComponent WHERE 
               DCServiceComponent.DCServiceComponentID =
                  ttComponent.DCServiceComponentID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-disp-row:

       FIND FIRST DCServiceComponent WHERE 
            DCServiceComponent.DCServiceComponentID =
               ttComponent.DCServiceComponentID NO-LOCK.
       
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DCServiceComponent.ServCom
       ServCom.SCName WHEN AVAILABLE ServCom
       DCServiceComponent.DefValue
       DCServiceComponent.DefParam
       DCServiceComponent.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
    FIND FIRST ServCom WHERE 
               ServCom.Brand = Syst.CUICommon:gcBrand AND
               ServCom.ServCom = DCServiceComponent.ServCom NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF VAR llAttributes AS LOG  NO-UNDO.
   

   DEF BUFFER bDCComponent FOR DCServiceComponent.
    
   ActionDetails:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
   
      llAttributes = (AVAILABLE ServCom AND ServCom.ServAttr).
      
      FIND FIRST DayCampaign WHERE 
           DayCampaign.Brand = Syst.CUICommon:gcBrand AND
           DayCampaign.DCEvent = DCServicePackage.DCEvent 
           NO-LOCK NO-ERROR.

      FIND FIRST ServPac WHERE
                 ServPac.Brand = Syst.CUICommon:gcBrand AND
                 ServPac.ServPac = DCServicePackage.ServPac NO-LOCK NO-ERROR.
           
      DISP 
         DCServicePackage.DCEvent
         DayCampaign.DCName WHEN AVAILABLE DayCampaign
         DCServicePackage.ServPac
         ServPac.SPName WHEN AVAILABLE ServPac
         DCServiceComponent.DCServiceComponentID
         DCServiceComponent.ServCom
         ServCom.SCName WHEN AVAILABLE ServCom
         DCServiceComponent.DefValue
         DCServiceComponent.DefParam
         DCServiceComponent.FromDate
         DCServiceComponent.ToDate
       WITH FRAME lis.

      
      IF NOT NEW DCServiceComponent THEN REPEAT:
         ASSIGN 
            Syst.CUICommon:ufk    = 0
            Syst.CUICommon:ufk[1] = 7    WHEN lcRight = "RW"
            Syst.CUICommon:ufk[4] = 2350 WHEN llAttributes
            Syst.CUICommon:ufk[8] = 8
            Syst.CUICommon:ehto   = 0.
         
         RUN Syst/ufkey.p.

         IF Syst.CUICommon:toimi = 1 THEN LEAVE.
         
         ELSE IF Syst.CUICommon:toimi = 4 THEN 
            RUN Mm/dcserviceattribute.p(DCServiceComponent.DCServiceComponentID).
            
         ELSE IF Syst.CUICommon:toimi = 8 THEN LEAVE ActionDetails.
      END.

      FIND CURRENT DCServiceComponent EXCLUSIVE-LOCK.
      
      UPDATE
         DCServiceComponent.DefValue
         DCServiceComponent.DefParam
         DCServiceComponent.FromDate
         DCServiceComponent.ToDate      
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),Syst.CUICommon:poisnap) > 0 THEN DO WITH FRAME lis:
            
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.

      IF CAN-FIND(FIRST bDCComponent WHERE
                    bDCComponent.DCServicePackageID = 
                       DCServiceComponent.DCServicePackageID             AND
                    bDCComponent.ServCom  = DCServiceComponent.ServCom   AND
                    bDCComponent.FromDate <= DCServiceComponent.ToDate   AND
                    bDCComponent.ToDate   >= DCServiceComponent.FromDate AND
                    RECID(bDCComponent) NE RECID(DCServiceComponent))
      THEN DO:
         MESSAGE "Check dates. Another configuration row already exists"
                 "with overlapping effective period."
         VIEW-AS ALERT-BOX ERROR.
         NEXT. 
      END.
      
      IF NEW DCServiceComponent THEN LEAVE.
   
   END.
   
END PROCEDURE.

PROCEDURE pInitTempTable:

   EMPTY TEMP-TABLE ttComponent.
   
   FOR EACH DCServiceComponent NO-LOCK WHERE
            DCServiceComponent.DCServicePackageID = iiDCPackageID:

      IF llActive AND DCServiceComponent.ToDate < TODAY THEN NEXT.
            
      CREATE ttComponent.
      BUFFER-COPY DCServiceComponent TO ttComponent.
   END.

END PROCEDURE.

