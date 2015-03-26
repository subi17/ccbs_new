/* ----------------------------------------------------------------------
  MODULE .......: OfferCriteria.p
  TASK .........: UPDATEs table OfferCriteria
  APPLICATION ..: tms
  AUTHOR .......: aam
  CREATED ......: 21.01.09
  CHANGED ......: 
  Version ......: Yoigo
  ---------------------------------------------------------------------- */

{commali.i} 
{lib/tokenlib.i}
{lib/tokenchk.i 'OfferCriteria'}
{eventval.i}
{timestamp.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhOfferCriteria AS HANDLE NO-UNDO.
   lhOfferCriteria = BUFFER OfferCriteria:HANDLE.
   RUN StarEventInitialize(lhOfferCriteria).

   ON F12 ANYWHERE DO:
      RUN eventview2(lhOfferCriteria).
   END.

END.

DEF INPUT PARAMETER icOffer  AS CHAR NO-UNDO.
DEF INPUT PARAMETER ilUpdate AS LOG  NO-UNDO. 

DEF NEW shared VAR siirto AS CHAR.

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

DEF VAR lcField      AS CHAR NO-UNDO. 
DEF VAR lcCode       AS CHAR NO-UNDO. 
DEF VAR lcType       AS CHAR NO-UNDO.
DEF VAR lcBegin      AS CHAR NO-UNDO.
DEF VAR lcEnd        AS CHAR NO-UNDO.
DEF VAR ldtFromDate  AS DATE NO-UNDO.
DEF VAR ldtToDate    AS DATE NO-UNDO. 
DEF VAR lcFrameField AS CHAR NO-UNDO. 
DEF VAR lcCodeTable  AS CHAR NO-UNDO.
DEF VAR ldDefFrom    AS DEC  NO-UNDO.


FORM
    OfferCriteria.CriteriaType   FORMAT "X(20)"
    OfferCriteria.IncludedValue  FORMAT "X(20)"
    OfferCriteria.ExcludedValue  FORMAT "X(15)"
    ldtFromDate                  FORMAT "99-99-99" COLUMN-LABEL "From"
    ldtToDate                    FORMAT "99-99-99" COLUMN-LABEL "To"
WITH ROW FrmRow CENTERED OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)   
    TITLE COLOR VALUE(ctc) 
       " CRITERIA FOR OFFER " + STRING(icOffer) + " "
    FRAME sel.

FORM
    OfferCriteria.Offer       COLON 15 FORMAT "X(16)"
       Offer.Description NO-LABEL FORMAT "X(40)" SKIP
    OfferCriteria.OfferCriteriaID COLON 15 SKIP(1)
    OfferCriteria.CriteriaType  COLON 15 
       lcType FORMAT "X(30)" NO-LABEL SKIP
    OfferCriteria.IncludedValue COLON 15
    OfferCriteria.ExcludedValue COLON 15
    OfferCriteria.BeginStamp    COLON 15
       lcBegin FORMAT "X(20)" NO-LABEL SKIP
    OfferCriteria.EndStamp      COLON 15
       lcEnd FORMAT "X(20)" NO-LABEL SKIP
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    FRAME lis.


FUNCTION fCriteriaType RETURNS LOGIC
   (icCriteriaType AS CHAR):

   IF icCriteriaType > "" THEN 
      lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                "OfferCriteria",
                                "CriteriaType",
                                icCriteriaType).
   ELSE lcType = "".
                                
   DISP lcType WITH FRAME lis.
   
END FUNCTION.


cfc = "sel". run ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

FIND FIRST Offer WHERE 
           Offer.Brand = gcBrand AND
           Offer.Offer = icOffer NO-LOCK NO-ERROR.
IF NOT AVAILABLE Offer THEN DO:
   MESSAGE "Offer not available"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

RUN local-Find-First.

IF AVAILABLE OfferCriteria THEN ASSIGN
   Memory       = recid(OfferCriteria)
   must-print   = TRUE
   must-add     = FALSE
   ldDefFrom    = fMake2DT(TODAY,0).
ELSE DO:
   IF lcRight NE "RW" THEN DO:
      MESSAGE "No criteria available!" VIEW-AS ALERT-BOX INFORMATION.
      RETURN.
   END.
   ASSIGN
      Memory       = ?
      must-print   = FALSE
      must-add     = FALSE
      ldDefFrom    = fMake2DT(Offer.FromDate,0).
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
    END.

   IF must-add THEN DO:  /* Add a OfferCriteria  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      run ufcolor.

      ADD-ROW:
      REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      
        PAUSE 0 NO-MESSAGE.
        VIEW FRAME lis. 
        CLEAR FRAME lis ALL NO-PAUSE.
        ehto = 9. RUN ufkey.

        REPEAT TRANS WITH FRAME lis:

           DISPLAY icOffer @ OfferCriteria.Offer.

           i = 1. 
           FOR EACH OfferCriteria NO-LOCK 
           BY OfferCriteria.OfferCriteriaId DESC:
              i = OfferCriteria.OfferCriteriaID + 1.
              LEAVE.
           END.
           
           CREATE OfferCriteria.
           ASSIGN 
              OfferCriteria.Brand = gcBrand 
              OfferCriteria.OfferCriteriaID = i
              OfferCriteria.Offer   = icOffer
              OfferCriteria.BeginStamp = ldDefFrom.
              OfferCriteria.EndStamp   = fMake2DT(12/31/2049,86399).

           RUN local-UPDATE-record.

           IF LOOKUP(KEYFUNCTION(LASTKEY),"ENDKEY,END-ERROR") > 0 OR
              OfferCriteria.CriteriaType = "" THEN 
           UNDO add-row, LEAVE add-row.

           IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhOfferCriteria).

           ASSIGN
           Memory = recid(OfferCriteria)
           xrecid = Memory.  
           LEAVE.
        END.
      END.  /* ADD-ROW */
      
      HIDE FRAME lis NO-PAUSE.

      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      RUN local-find-first.
      IF NOT AVAILABLE OfferCriteria THEN LEAVE LOOP.
      
      NEXT LOOP.
   END.

   PrintPage:
   DO :

      IF must-print THEN DO:

        UP FRAME-LINE - 1.
        FIND OfferCriteria WHERE recid(OfferCriteria) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE OfferCriteria THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(OfferCriteria).
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
        ufk   = 0
        ufk[5]= (IF lcRight = "RW" THEN 5 ELSE 0)  
        ufk[6]= (IF lcRight = "RW" THEN 4 ELSE 0) 
        ufk[7] = 1752
        ufk[8]= 8 
        ehto  = 3 
        ufkey = FALSE.
        
        /* used as help */
        IF gcHelpParam > "" THEN ASSIGN
           ufk[5] = 11
           ufk[6] = 0.
 
        ELSE IF NOT ilUpdate THEN ASSIGN
           ufk[5] = 0
           ufk[6] = 0.
          
        RUN ufkey.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW OfferCriteria.CriteriaType ;(uchoose.i;) NO-ERROR 
           WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) OfferCriteria.CriteriaType WITH FRAME sel.
      END.

      nap = keylabel(LASTKEY).

      IF rtab[FRAME-line] = ? THEN DO:
         IF LOOKUP(nap,"5,f5,7,f7,8,f8") = 0 THEN DO:
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
        FIND OfferCriteria WHERE recid(OfferCriteria) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE OfferCriteria THEN
              ASSIGN FIRSTrow = i Memory = recid(OfferCriteria).
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
           IF NOT AVAILABLE OfferCriteria THEN DO:
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
                rtab[1] = recid(OfferCriteria)
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
           IF NOT AVAILABLE OfferCriteria THEN DO:
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
              rtab[FRAME-DOWN] = recid(OfferCriteria).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND OfferCriteria WHERE recid(OfferCriteria) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE OfferCriteria THEN DO:
           Memory = recid(OfferCriteria).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE OfferCriteria THEN Memory = recid(OfferCriteria).
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
           FIND OfferCriteria WHERE recid(OfferCriteria) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     ELSE IF LOOKUP(nap,"5,f5") > 0 AND ufk[5] > 0 THEN DO:  /* add */
        IF gcHelpParam > "" THEN DO:
           xRecid = rtab[FRAME-LINE].
           LEAVE LOOP.
        END.
        
        ELSE DO:
           must-add = TRUE.
           NEXT LOOP.
        END.    
     END.

     ELSE IF LOOKUP(nap,"6,f6") > 0 AND ufk[6] > 0 
     THEN DO TRANS:  /* DELETE */
       delrow = FRAME-LINE.
       RUN local-find-this (FALSE).

       /* Highlight */
       COLOR DISPLAY VALUE(ctc)
          OfferCriteria.CriteriaType
          OfferCriteria.IncludedValue
          OfferCriteria.ExcludedValue.

       RUN local-find-NEXT.
       IF AVAILABLE OfferCriteria THEN Memory = recid(OfferCriteria).
       ELSE DO:
          /* read back the record that is TO be  removed */
          RUN local-find-this (FALSE).                     

          RUN local-find-PREV.
          IF AVAILABLE OfferCriteria THEN DO:
             ASSIGN
             delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
             Memory = recid(OfferCriteria).
          END.
       END.

       /* FIND back the ROW that is TO be removed */
       RUN local-find-this(TRUE).

       ASSIGN ok = FALSE.
       MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N)?" UPDATE ok.
       COLOR DISPLAY VALUE(ccc)
          OfferCriteria.CriteriaType
          OfferCriteria.IncludedValue
          OfferCriteria.ExcludedValue.
       
       IF ok THEN DO:

           IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhOfferCriteria).

           DELETE OfferCriteria.

           /* was LAST record DELETEd ? */
           RUN local-find-first.
           IF NOT AVAILABLE OfferCriteria THEN DO:
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
     REPEAT WITH FRAME lis TRANS
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       IF gcHelpParam > "" THEN DO:
          xRecid = rtab[FRAME-LINE (sel)].
          LEAVE LOOP.
       END.
 
       IF llDoEvent THEN RUN StarEventSetOldBuffer(lhOfferCriteria).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9. RUN ufkey.
       cfc = "lis". run ufcolor. CLEAR FRAME lis NO-PAUSE.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSEndStamp */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhOfferCriteria).

       RUN local-disp-row.
       xrecid = recid(OfferCriteria).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(OfferCriteria) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(OfferCriteria) must-print = TRUE.
        NEXT LOOP.
     END.
         
     ELSE IF LOOKUP(nap,"7,f7") > 0 THEN DO:
        RUN eventsel.p("offercriteria", "#BEGIN" + chr(255) 
           + gcBrand + chr(255) + icOffer).
        ufkey = TRUE.
        NEXT.
     END.   

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.

ehto = 4.
RUN ufkey.

fCleanEventObjects().



PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND OfferCriteria WHERE recid(OfferCriteria) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND OfferCriteria WHERE recid(OfferCriteria) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:

   IF order = 1 THEN FIND FIRST OfferCriteria WHERE 
      OfferCriteria.Brand = gcBrand AND
      OfferCriteria.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN FIND LAST OfferCriteria WHERE 
      OfferCriteria.Brand = gcBrand AND
      OfferCriteria.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN FIND NEXT OfferCriteria WHERE 
      OfferCriteria.Brand = gcBrand AND
      OfferCriteria.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
   IF order = 1 THEN FIND PREV OfferCriteria WHERE 
      OfferCriteria.Brand = gcBrand AND
      OfferCriteria.Offer = icOffer NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   CLEAR FRAME sel NO-PAUSE.
       
   DISPLAY 
      OfferCriteria.CriteriaType
      OfferCriteria.IncludedValue
      OfferCriteria.ExcludedValue
      ldtFromDate
      ldtToDate
   WITH FRAME sel.
       
END PROCEDURE.

PROCEDURE local-find-others.

   DEF VAR liTime AS INT NO-UNDO.
   
   fSplitTS(OfferCriteria.BeginStamp,
             OUTPUT ldtFromDate,
             OUTPUT liTime).
   fSplitTS(OfferCriteria.EndStamp,
            OUTPUT ldtToDate,
            OUTPUT liTime).
            
END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      ASSIGN
         lcBegin = fTS2HMS(OfferCriteria.BeginStamp)
         lcEnd   = fTS2HMS(OfferCriteria.EndStamp).
     
      fCriteriaType(OfferCriteria.CriteriaType).
         
      DISP 
         OfferCriteria.Offer        
         Offer.Description
         OfferCriteria.OfferCriteriaID
         OfferCriteria.CriteriaType
         OfferCriteria.IncludedValue        
         OfferCriteria.ExcludedValue
         OfferCriteria.BeginStamp
         lcBegin
         OfferCriteria.EndStamp
         lcEnd
      WITH FRAME lis.

      IF NOT NEW OfferCriteria THEN DO:
         ASSIGN 
            ufk    = 0
            ufk[1] = 7 WHEN lcRight = "RW" AND ilUpdate
            ufk[6] = 1752
            ufk[8] = 8
            ehto   = 0.
         
         RUN ufkey.
         
         IF toimi = 6 THEN DO: 
            RUN eventsel.p("offercriteria", "#BEGIN" + chr(255) 
               + OfferCriteria.Brand + chr(255) + OfferCriteria.Offer + 
               chr(255) + STRING(OfferCriteria.OfferCriteriaId)).
            LEAVE.
         END.   
         
         IF toimi = 8 THEN LEAVE.
      END.

      UpdateField:
      REPEAT TRANS WITH FRAME lis ON ENDKEY UNDO, LEAVE:
                
         FIND CURRENT OfferCriteria EXCLUSIVE-LOCK.
         ehto = 9.
         RUN ufkey.
         
         UPDATE
            OfferCriteria.CriteriaType   WHEN NEW OfferCriteria
            OfferCriteria.IncludedValue 
            OfferCriteria.BeginStamp
            OfferCriteria.EndStamp
         WITH FRAME lis EDITING:
 
            READKEY.

            IF KEYLABEL(LASTKEY) = "F9" AND 
               LOOKUP(FRAME-FIELD,
                      "CriteriaType,IncludedValue,ExcludedValue") > 0 
            THEN DO:

               IF FRAME-FIELD = "CriteriaType" THEN DO:
                  RUN h-tmscodes(INPUT "OfferCriteria", 
                                       "CriteriaType", 
                                       "Offer", 
                                 OUTPUT lcCode).

                  IF lcCode ne "" AND lcCode NE ? THEN 
                     DISP lcCode @ OfferCriteria.CriteriaType WITH FRAME lis.
               END.
             
               ELSE IF LOOKUP(FRAME-FIELD,"IncludedValue,ExcludedValue") > 0
               THEN DO:
                  ASSIGN
                     lcFrameField = FRAME-FIELD
                     lcCodeTable  = ""
                     lcCode       = "".
                  
                  CASE INPUT FRAME lis OfferCriteria.CriteriaType:
                  WHEN "OrderType" OR 
                  WHEN "OrderChannel" OR 
                  WHEN "NumberType" THEN lcCodeTable = "Order".
                  WHEN "PayType"    THEN lcCodeTable = "CLIType".
                  WHEN "CLIType" THEN DO:
                     siirto = ?.
                     RUN h-mobtype.
                     lcCode = siirto.
                  END. 
                  END CASE.
                  
                  IF lcCodeTable > "" THEN 
                     RUN h-tmscodes(INPUT lcCodeTable, 
                                    INPUT INPUT OfferCriteria.CriteriaType, 
                                    INPUT ?, 
                                    OUTPUT lcCode).

                   IF lcCode NE "" AND lcCode NE ? THEN DO WITH FRAME lis:
                     IF lcFrameField = "IncludedValue" THEN DO:
                        IF INPUT OfferCriteria.IncludedValue > ""
                        THEN lcCode = INPUT OfferCriteria.IncludedValue +
                                      "," + lcCode.
                        DISPLAY lcCode @ OfferCriteria.IncludedValue.
                     END.
                     ELSE DO:
                        IF INPUT OfferCriteria.ExcludedValue > ""
                        THEN lcCode = INPUT OfferCriteria.ExcludedValue +
                                      "," + lcCode.
                        DISPLAY lcCode @ OfferCriteria.ExcludedValue.
                     END.
                  END.
               END.
               
               ehto = 9.
               RUN ufkey.
               NEXT. 
            END.

            ELSE IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
 
               IF FRAME-FIELD = "CriteriaType" THEN DO:
                  IF INPUT FRAME lis OfferCriteria.CriteriaType = ""
                  THEN LEAVE. 
 
                  fCriteriaType(INPUT 
                         INPUT FRAME lis OfferCriteria.CriteriaType).
                  IF lcType = "" THEN DO:
                     MESSAGE "Unknown criteria type"
                     VIEW-AS ALERT-BOX ERROR.
                     NEXT.
                  END.
               END.
             END.
            
            APPLY LASTKEY.
         END.

         LEAVE UpdateField.
         
      END.
      
      IF NEW OfferCriteria THEN DO:
         ldDefFrom = OfferCriteria.BeginStamp.
         LEAVE.
      END.
   END.

END PROCEDURE.

