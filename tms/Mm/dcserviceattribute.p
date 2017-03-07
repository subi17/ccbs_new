/* ----------------------------------------------------------------------
  MODULE .......: DCServiceAttribute
  TASK .........: UPDATEs table DCServiceAttribute
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 09.11.10
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{Syst/commali.i} 
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'DCServiceAttribute'}

{Syst/eventval.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhDCServiceAttribute AS HANDLE NO-UNDO.
   lhDCServiceAttribute = BUFFER DCServiceAttribute:HANDLE.
   RUN StarEventInitialize(lhDCServiceAttribute).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhDCServiceAttribute).
   END.

END.

DEF INPUT PARAMETER iiDCComponentID  AS INT  NO-UNDO.

DEF NEW shared VAR siirto AS CHAR.

DEF VAR xrecid       AS RECID                           init ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  init 7.
DEF VAR FrmDown      AS INT                    NO-UNDO  init 8.
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
DEF VAR liAttributeID   AS INT                    NO-UNDO.

DEF VAR lcStatus     AS CHAR NO-UNDO.
DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR llActive     AS LOG  NO-UNDO INIT TRUE. 
DEF VAR lcServPac    AS CHAR NO-UNDO.

DEF TEMP-TABLE ttAttribute NO-UNDO
   FIELD DCServiceAttributeID AS INT
   FIELD ServAttr   AS CHAR
   FIELD ToDate    AS DATE
   INDEX ServAttr ServAttr ToDate DESC.
   
FORM
    DCServiceAttribute.ServAttr FORMAT "X(14)" COLUMN-LABEL "Attribute"
    ServAttr.SAName             FORMAT "X(30)" COLUMN-LABEL "Name" 
    DCServiceAttribute.DefParam FORMAT "X(12)" 
    DCServiceAttribute.ToDate
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       "  ATTRIBUTES OF " + DCServiceComponent.ServCom + " (Active) "
    FRAME sel.

FORM
    DCServicePackage.DCEvent COLON 20 
       DayCampaign.DCName NO-LABEL FORMAT "X(35)" SKIP
    DCServicePackage.ServPac COLON 20 
       ServPac.SPName NO-LABEL FORMAT "X(35)" SKIP
    DCServiceComponent.ServCom  COLON 20 
       ServCom.SCName NO-LABEL FORMAT "X(35)" SKIP
    DCServiceAttribute.DCServiceAttributeID COLON 20 
       LABEL "Row ID"
    DCServiceAttribute.ServAttr COLON 20
       ServAttr.SAName NO-LABEL FORMAT "X(30)" SKIP
    DCServiceAttribute.DefParam COLON 20 
    DCServiceAttribute.FromDate COLON 20
    DCServiceAttribute.ToDate   COLON 20    
WITH  OVERLAY ROW 9 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FIND FIRST DCServiceComponent WHERE 
   DCServiceComponent.DCServiceComponentID = iiDCComponentID NO-LOCK NO-ERROR.
IF NOT AVAILABLE DCServiceComponent THEN DO:
   MESSAGE "Service component not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

FIND FIRST DCServicePackage WHERE 
   DCServicePackage.DCServicePackageID = DCServiceComponent.DCServicePackageID
   NO-LOCK NO-ERROR.
IF NOT AVAILABLE DCServicePackage THEN DO:
   MESSAGE "Service package not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

RUN pInitTempTable.

RUN local-Find-First.

IF AVAILABLE ttAttribute THEN ASSIGN
   Memory       = recid(ttAttribute)
   must-print   = TRUE
   must-add     = FALSE.
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No service Attributes available" VIEW-AS ALERT-BOX.
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

   IF must-add THEN DO:  /* Add a DCServiceAttribute  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis NO-PAUSE.
        ehto = 9. RUN Syst/ufkey.p.

        REPEAT TRANSACTION WITH FRAME lis:

           PROMPT-FOR 
              DCServiceAttribute.ServAttr WITH FRAME lis
           EDITING:
              READKEY.

              IF KEYLABEL(LASTKEY) = "F9" THEN DO:

                 gcHelpParam = DCServiceComponent.ServCom.
                 RUN Help/h-service_attribute.p.
                 IF siirto > "" THEN 
                    DISPLAY siirto @ DCServiceAttribute.ServAttr 
                    WITH FRAME lis.
                 ehto = 9.
                 RUN Syst/ufkey.p.
                 NEXT. 
              END.
                 
              APPLY LASTKEY.
           END.
            
           IF INPUT DCServiceAttribute.ServAttr = "" THEN UNDO, LEAVE ADD-ROW.

           IF  NOT CAN-FIND(FIRST ServAttr WHERE
                      ServAttr.Brand = gcBrand AND
                      ServAttr.ServCom = DCServiceComponent.ServCom AND
                      ServAttr.ServAttr = INPUT DCServiceAttribute.ServAttr) 
           THEN DO:
               MESSAGE "Unknown service Attribute"
               VIEW-AS ALERT-BOX ERROR.
               NEXT.
           END.
           
           FIND LAST DCServiceAttribute USE-INDEX DCServiceAttributeID 
              NO-LOCK NO-ERROR.
           IF AVAILABLE DCServiceAttribute THEN 
              liAttributeID = DCServiceAttribute.DCServiceAttributeID + 1.
           ELSE liAttributeID = 1.
           
           CREATE DCServiceAttribute.
           ASSIGN 
              DCServiceAttribute.DCServiceAttributeID = liAttributeID
              DCServiceAttribute.DCServiceComponentID = iiDCComponentID
              DCServiceAttribute.ServAttr = INPUT FRAME lis 
                                            DCServiceAttribute.ServAttr
              DCServiceAttribute.FromDate = TODAY
              DCServiceAttribute.ToDate   = DCServicePackage.ToDate.
                                        
           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 THEN
           UNDO add-row, LEAVE add-row.

           CREATE ttAttribute.
           BUFFER-COPY DCServiceAttribute TO ttAttribute.
           
           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhDCServiceAttribute).

           ASSIGN
           Memory = recid(ttAttribute)
           xrecid = RECID(DCServiceAttribute).  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE ttAttribute THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND ttAttribute WHERE recid(ttAttribute) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE ttAttribute THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(ttAttribute).
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
        ufk[1]= 0 
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
         
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW DCServiceAttribute.ServAttr {Syst/uchoose.i} NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) DCServiceAttribute.ServAttr WITH FRAME sel.
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
        FIND ttAttribute WHERE recid(ttAttribute) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE ttAttribute THEN
              ASSIGN FIRSTrow = i Memory = recid(ttAttribute).
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
           IF NOT AVAILABLE ttAttribute THEN DO:
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
                rtab[1] = recid(ttAttribute)
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
           IF NOT AVAILABLE ttAttribute THEN DO:
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
              rtab[FRAME-DOWN] = recid(ttAttribute).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND ttAttribute WHERE recid(ttAttribute) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE ttAttribute THEN DO:
           Memory = recid(ttAttribute).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE ttAttribute THEN Memory = recid(ttAttribute).
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
           FIND ttAttribute WHERE recid(ttAttribute) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"4,f4") > 0 THEN DO:  /* display filter */
        llActive = NOT llActive.
        RUN pInitTempTable.
        RUN local-find-first.
        ASSIGN 
           Memory = recid(ttAttribute) 
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
           xRecid = RECID(DCServiceAttribute).
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

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
       DCServiceAttribute.ServAttr
       DCServiceAttribute.DefParam
       DCServiceAttribute.ToDate.

       RUN local-find-NEXT.
       IF AVAILABLE ttAttribute THEN Memory = recid(ttAttribute).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE ttAttribute THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(ttAttribute).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          DCServiceAttribute.ServAttr
          DCServiceAttribute.DefParam
          DCServiceAttribute.ToDate.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhDCServiceAttribute).

           DELETE DCServiceAttribute.
           DELETE ttAttribute.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE DCServiceAttribute THEN DO:
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
          xRecid = RECID(DCServiceAttribute).
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhDCServiceAttribute).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN Syst/ufkey.p.
       cfc = "lis". RUN Syst/ufcolor.p. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhDCServiceAttribute).

       RUN local-disp-row.
       xrecid = recid(DCServiceAttribute).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(ttAttribute) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(ttAttribute) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

FINALLY:
   HIDE FRAME sel NO-PAUSE.
   si-recid = xrecid.

   ehto = 4.
   RUN Syst/ufkey.p.

   fCleanEventObjects().
END.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN DO:
       FIND ttAttribute WHERE recid(ttAttribute) = rtab[frame-line(sel)].
       FIND FIRST DCServiceAttribute WHERE 
          DCServiceAttribute.DCServiceAttributeID = 
             ttAttribute.DCServiceAttributeID EXCLUSIVE-LOCK.
    END.  
    ELSE DO:
       FIND ttAttribute WHERE recid(ttAttribute) = rtab[frame-line(sel)] 
          NO-LOCK.
       FIND FIRST DCServiceAttribute WHERE 
          DCServiceAttribute.DCServiceAttributeID = 
             ttAttribute.DCServiceAttributeID NO-LOCK.
    END.
    
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF iiDCComponentID ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND FIRST ttAttribute NO-LOCK NO-ERROR.
         IF AVAILABLE ttAttribute THEN 
            FIND FIRST DCServiceAttribute WHERE 
               DCServiceAttribute.DCServiceAttributeID = 
                  ttAttribute.DCServiceAttributeID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF iiDCComponentID ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND LAST ttAttribute NO-LOCK NO-ERROR.
         IF AVAILABLE ttAttribute THEN 
            FIND FIRST DCServiceAttribute WHERE 
               DCServiceAttribute.DCServiceAttributeID = 
                  ttAttribute.DCServiceAttributeID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF iiDCComponentID ne ? THEN DO:
      IF order = 1 THEN DO:
         FIND NEXT ttAttribute NO-LOCK NO-ERROR.
         IF AVAILABLE ttAttribute THEN 
            FIND FIRST DCServiceAttribute WHERE 
               DCServiceAttribute.DCServiceAttributeID = 
                  ttAttribute.DCServiceAttributeID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF iiDCComponentID ne ?  THEN DO:
      IF order = 1 THEN DO:
         FIND PREV ttAttribute NO-LOCK NO-ERROR.
         IF AVAILABLE ttAttribute THEN 
            FIND FIRST DCServiceAttribute WHERE 
               DCServiceAttribute.DCServiceAttributeID =
                  ttAttribute.DCServiceAttributeID NO-LOCK NO-ERROR.
      END.      
   END.
END PROCEDURE.

PROCEDURE local-disp-row:

       FIND FIRST DCServiceAttribute WHERE 
            DCServiceAttribute.DCServiceAttributeID =
               ttAttribute.DCServiceAttributeID NO-LOCK.
       
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       
       DISPLAY 
       DCServiceAttribute.ServAttr
       ServAttr.SAName WHEN AVAILABLE ServAttr
       DCServiceAttribute.DefParam
       DCServiceAttribute.ToDate
       WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.
   
    FIND FIRST ServAttr WHERE 
               ServAttr.Brand = gcBrand AND
               ServAttr.ServCom = DCServiceComponent.ServCom AND
               ServAttr.ServAttr = DCServiceAttribute.ServAttr 
       NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   DEF BUFFER bDCAttribute FOR DCServiceAttribute.
    
   ActionDetails:
   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
   
      FIND FIRST DayCampaign WHERE 
           DayCampaign.Brand = gcBrand AND
           DayCampaign.DCEvent = DCServicePackage.DCEvent 
           NO-LOCK NO-ERROR.

      FIND FIRST ServPac WHERE
                 ServPac.Brand = gcBrand AND
                 ServPac.ServPac = DCServicePackage.ServPac NO-LOCK NO-ERROR.
           
      FIND FIRST ServCom WHERE
                 ServCom.Brand = gcBrand AND
                 ServCom.ServCom = DCServiceComponent.ServCom NO-LOCK NO-ERROR.
                 
      DISP 
         DCServicePackage.DCEvent
         DayCampaign.DCName WHEN AVAILABLE DayCampaign
         DCServicePackage.ServPac
         ServPac.SPName WHEN AVAILABLE ServPac
         DCServiceComponent.ServCom
         ServCom.SCName WHEN AVAILABLE ServCom
         DCServiceAttribute.DCServiceAttributeID
         DCServiceAttribute.ServAttr
         ServAttr.SAName WHEN AVAILABLE ServAttr
         DCServiceAttribute.DefParam
         DCServiceAttribute.FromDate
         DCServiceAttribute.ToDate
       WITH FRAME lis.

      
      IF NOT NEW DCServiceAttribute THEN REPEAT:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7    WHEN lcRight = "RW"
            ufk[8] = 8
            ehto   = 0.
         
         RUN Syst/ufkey.p.

         IF toimi = 1 THEN LEAVE.
         
         ELSE IF toimi = 8 THEN LEAVE ActionDetails.
      END.

      FIND CURRENT DCServiceAttribute EXCLUSIVE-LOCK.
      
      UPDATE
         DCServiceAttribute.DefParam
         DCServiceAttribute.FromDate
         DCServiceAttribute.ToDate      
      WITH FRAME lis EDITING:
 
         READKEY.

         IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lis:
            
            PAUSE 0.
         END.
            
         APPLY LASTKEY.
      END.

      IF CAN-FIND(FIRST bDCAttribute WHERE
                    bDCAttribute.DCServiceComponentID = 
                       DCServiceAttribute.DCServiceComponentID           AND
                    bDCAttribute.ServAttr  = DCServiceAttribute.ServAttr AND
                    bDCAttribute.FromDate <= DCServiceAttribute.ToDate   AND
                    bDCAttribute.ToDate   >= DCServiceAttribute.FromDate AND
                    RECID(bDCAttribute) NE RECID(DCServiceAttribute))
      THEN DO:
         MESSAGE "Check dates. Another configuration row already exists"
                 "with overlapping effective period."
         VIEW-AS ALERT-BOX ERROR.
         NEXT. 
      END.
      
      IF NEW DCServiceAttribute THEN LEAVE.
   
   END.
   
END PROCEDURE.

PROCEDURE pInitTempTable:

   EMPTY TEMP-TABLE ttAttribute.
   
   FOR EACH DCServiceAttribute NO-LOCK WHERE
            DCServiceAttribute.DCServiceComponentID = iiDCComponentID:

      IF llActive AND DCServiceAttribute.ToDate < TODAY THEN NEXT.
            
      CREATE ttAttribute.
      BUFFER-COPY DCServiceAttribute TO ttAttribute.
   END.

END PROCEDURE.

