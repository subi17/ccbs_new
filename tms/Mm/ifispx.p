/* ----------------------------------------------------------------------
  MODULE .......: IFiSpx.P
  TASK .........: UPDATE IMSI PaymFile Spex.
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 03-06-99
  CHANGED ......: 29.06.99 pt IFiSpx.Isc1
                  05.10.99 jp urights added
                  05.11.02 jr Eventlog  
                  09.09.03 jp Brand
                  16.03.07 kl RUN Syst/filebrowser.p
                  19.04.07 kl search fixed, path added to SIMFile

  Version ......: M15
  ---------------------------------------------------------------------- */
&GLOBAL-DEFINE BrTable ifispx

{Syst/commali.i}
{Syst/eventval.i} 
{Func/cparam2.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'IFISpx'}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR Mancode LIKE IFiSpx.ManCode NO-UNDO.
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
DEF VAR llIsAdmin    AS LOG NO-UNDO INIT FALSE. 

/* check admin user rights */
IF getTMSRight("SYST") EQ "RW" THEN llIsAdmin = TRUE.

IF llDoEvent THEN 
DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhIFisPX AS HANDLE NO-UNDO.
   lhIFisPX = BUFFER IFisPX:HANDLE.
   RUN StarEventInitialize(lhIFisPX).

   ON F12 ANYWHERE 
   DO:
      RUN Mc/eventview2.p(lhIFisPX).
   END.
END.

form
    IFISpx.brand 
    IFiSpx.ManCode      /* COLUMN-LABEL FORMAT */
    IFiSpx.ManCode     /* COLUMN-LABEL FORMAT */
    SimMan.ManName
    IFiSpx.SimArt
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    title COLOR VALUE(ctc) " " + ynimi +
    " IMSI PaymFile Specifications "
    + string(pvm,"99-99-99") + " "
    FRAME sel.

{Func/brand.i}
form
    IFiSpx.ManCode   label "Manufacturer ..." SimMan.ManName NO-LABEL AT 38 SKIP
    IFiSpx.Version   label "Version ........"                               SKIP
    IFiSpx.SimArt   label "SIM Type ........" SimArt.SAName NO-LABEL AT 38 
    skip(1) 
    IFiSpx.Hrowd  label "No. of Hdr rows "                        skip(1)

    "Field      From Position   Length"                           SKIP
    "----------------------------"                           SKIP
    "IccId ........:" IFiSpx.ICC[1]   TO 19 NO-LABEL
                      IFiSpx.ICC[2]   TO 28 NO-LABEL SKIP

    "IMSI .........:" IFiSpx.IMSI[1]  TO 19 NO-LABEL
                      IFiSpx.IMSI[2]  TO 28 NO-LABEL SKIP

    "PIN Code 1 ...:" IFiSpx.IsCode1[1] TO 19 NO-LABEL
                      IFiSpx.IsCode1[2] TO 28 NO-LABEL SKIP

    "PIN Code 2 ...:" IFiSpx.IsCode2[1] TO 19 NO-LABEL
                      IFiSpx.IsCode2[2] TO 28 NO-LABEL SKIP

    "PUK Code 1 ...:" IFiSpx.IsUnb1[1]  TO 19 NO-LABEL
                      IFiSpx.IsUnb1[2]  TO 28 NO-LABEL SKIP

    "PUK Code 2 ...:" IFiSpx.IsUnb2[1]  TO 19 NO-LABEL
                      IFiSpx.IsUnb2[2]  TO 28 NO-LABEL SKIP

    "Key Identifier:" IFiSpx.Ki  [1]  TO 19 NO-LABEL
                      IFiSpx.Ki  [2]  TO 28 NO-LABEL SKIP

    "ISC1 .........:" IFiSpx.Isc1[1]  TO 19 NO-LABEL
                      IFiSpx.Isc1[2]  TO 28 NO-LABEL SKIP


WITH  OVERLAY ROW 1 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)
    ac-hdr WITH side-labels 
    FRAME lis.

form /* seek Spex  BY  Mancode */
     "Brand Code:" lcBrand  HELP "Enter Brand"
     VALIDATE(CAN-FIND(Brand WHERE Brand.Brand = lcBrand),"Unknown brand") SKIP
     "Man.Code..:"   mancode
    help "Enter Manufacturer"
    WITH row 4 col 2 title COLOR VALUE(ctc) " FIND MANUFACTURER "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

orders = "By 1,By 2,By 3, By 4".


FIND FIRST IFiSpx
WHERE ifispx.Brand = lcBrand NO-LOCK NO-ERROR.
IF AVAILABLE IFiSpx THEN ASSIGN
   Memory       = recid(IFiSpx)
   must-print   = TRUE
   must-add     = FALSE.
ELSE ASSIGN
   Memory       = ?
   must-print   = FALSE
   must-add     = TRUE.

LOOP:
REPEAT WITH FRAME sel:

   IF order <> pr-order AND MaxOrder NE 1 THEN DO:
      pr-order = order.
      PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 entry(order,orders).
   END.

   IF must-add THEN DO:  /* Add a IFiSpx  */
      ASSIGN cfc = "lis" ufkey = true ac-hdr = " ADD " must-add = FALSE.
      RUN Syst/ufcolor.p.

ADD-ROW:
   REPEAT WITH FRAME lis ON ENDKEY UNDO ADD-ROW, LEAVE ADD-ROW.
      PAUSE 0 no-MESSAGE.
      ehto = 9. RUN Syst/ufkey.p.
      DO TRANSACTION:
         CLEAR FRAME lis NO-PAUSE.
         PROMPT-FOR 
            IFiSpx.ManCode 
            IFiSpx.Version     
         EDITING:
            READKEY.
            IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN 
            DO WITH FRAME lis:
               PAUSE 0.
               if  frame-field = "Mancode" THEN 
               DO:
                  if input IFiSpx.ManCode = "" THEN 
                     UNDO ADD-row, LEAVE add-row.
                     FIND SimMan where 
                        Simman.Brand   = gcBrand AND 
                        SimMan.Mancode = INPUT IFiSpx.ManCode
                        no-lock no-error.
                     IF NOT AVAIL SimMan THEN 
                     DO:
                        bell. message "Unknown manufacturer !".
                        NEXT.
                     END.
                     DISP SimMan.ManName.
                  END. 
                  else if frame-field = "Version" THEN 
                  DO:
                     FIND IFiSpx where 
                        ifispx.Brand   = gcBrand AND 
                        IFiSpx.ManCode = INPUT IFiSpx.ManCode AND 
                        IFiSpx.Version = INPUT IFiSpx.Version
                        no-lock no-error.
                     IF AVAIL IFiSpx THEN 
                     DO:     
                        BELL.
                        message "Such a Spex record exists already !".
                        NEXT.
                     END.
                  END.      
               END.
               APPLY LASTKEY.
            END.      
            CREATE IFiSpx.
            ASSIGN IFIspx.Brand   = gcBrand 
                   IFiSpx.ManCode = INPUT FRAME lis IFiSpx.ManCode 
                   IFiSpx.Version = INPUT FRAME lis IFiSpx.Version.

            RUN local-UPDATE-record.
            if lookup(keyfunction(lastkey),"endkey,end-error") > 0 THEN
               UNDO add-row, LEAVE add-row.

            IF llDoEvent THEN RUN StarEventMakeCreateEvent(lhIFisPX).
            ASSIGN Memory = recid(IFiSpx)
                   xrecid = Memory.
         END.
      END.  /* ADD-ROW */
      HIDE FRAME lis NO-PAUSE.
      ASSIGN must-print = TRUE.

      /* is there ANY record ? */
      FIND FIRST IFiSpx
      WHERE ifispx.Brand = lcBrand NO-LOCK NO-ERROR.
      IF NOT AVAILABLE IFiSpx THEN LEAVE LOOP.
      NEXT LOOP.
   END.

PrintPage:
   DO :
      IF must-print THEN DO:
         UP FRAME-LINE - 1.
         FIND IFiSpx WHERE recid(IFiSpx) = Memory NO-LOCK NO-ERROR.

         /* DISPLAY one page beginning the record
           whose RECID is saved into 'Memory'.
           starting from ROW 'delrow' */

         /* IF a ROW was recently DELETEd ... */
         IF delrow > 0 THEN DOWN delrow - 1.

         REPEAT WITH FRAME sel:
            IF AVAILABLE IFiSpx THEN DO:
               RUN local-disp-row.
               rtab[FRAME-LINE] = recid(IFiSpx).
               RUN local-find-NEXT.
            END.
            ELSE 
            DO:
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
         PAUSE 0 no-MESSAGE.

         /* Now there is one page DISPLAYed AND the cursor is on the
         upermost ROW, waiting FOR a 'choose' */
      END. /* must-print = TRUE */
   END. /* PrintPage */

   /* IF a ROW was recently DELETEd: */
   IF delrow > 0 THEN DOWN delrow - 1.
   ASSIGN delrow = 0.

BROWSE:
   REPEAT WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN 
      DO:
         ASSIGN
            ufk[1]= 215  ufk[2]= 0 ufk[3]= 0   ufk[4] = 0
            ufk[5]= 5    ufk[6]= 4 ufk[7]= 0   ufk[8]= 8 ufk[9]= 1
            ehto = 3 ufkey = FALSE.
            {Syst/uright1.i '"5,6"'}  
         IF NOT llIsAdmin THEN ASSIGN  ufk[5]= 0    ufk[6]= 0. 
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN 
      DO:
         CHOOSE ROW IFiSpx.ManCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) IFiSpx.ManCode WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN 
      DO:
         CHOOSE ROW IFiSpx.ManCode {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
         COLOR DISPLAY VALUE(ccc) IFiSpx.ManCode WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN 
      DO:
         order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN 
      DO:
         order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN 
      DO:
         ASSIGN FIRSTrow = 0 Memory = rtab[FRAME-LINE].
         FIND IFiSpx WHERE recid(IFiSpx) = Memory NO-LOCK.
         DO i = 1 TO FRAME-LINE - 1:
            RUN local-find-prev.
            IF AVAILABLE IFiSpx THEN
               ASSIGN FIRSTrow = i Memory = recid(IFiSpx).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN 
      DO:
         BELL.
         MESSAGE "You are on an empty row, move upwards !".
         PAUSE 1 no-MESSAGE.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* previous ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN 
      DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            RUN local-find-this(FALSE).
            RUN local-find-prev.
            IF NOT AVAILABLE IFiSpx THEN 
            DO:
               MESSAGE "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE 
            DO:
               /* previous was found */
               SCROLL DOWN.
               RUN local-disp-row.
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
                  rtab[1] = recid(IFiSpx)
                  Memory  = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* previous ROW */

      /* NEXT ROW */
      ELSE IF LOOKUP(nap,"cursor-down") > 0 THEN 
      DO WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN 
         DO:
            RUN local-find-this(FALSE).
            RUN local-find-NEXT.
            IF NOT AVAILABLE IFiSpx THEN 
            DO:
               MESSAGE "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-MESSAGE.
               NEXT BROWSE.
            END.
            ELSE 
            DO:
               /* NEXT ROW was found */
               SCROLL UP.
               RUN local-disp-row.
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(IFiSpx).
               /* save RECID of uppermost ROW */
               Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* prev page */
      ELSE IF LOOKUP(nap,"prev-page,page-up,-") > 0 THEN 
      DO:
         Memory = rtab[1].
         FIND IFiSpx WHERE recid(IFiSpx) = Memory NO-LOCK NO-ERROR.
         RUN local-find-prev.
         IF AVAILABLE IFiSpx THEN 
         DO:
            Memory = recid(IFiSpx).

            /* reverse 1 page */
            DO RowNo = 1 TO (FRAME-DOWN - 1):
               RUN local-find-prev.
               IF AVAILABLE IFiSpx THEN Memory = recid(IFiSpx).
               ELSE RowNo = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE 
         DO:
            /* is this the very FIRST record of the table ?  */
            MESSAGE "YOU ARE ON THE FIRST PAGE !".
            BELL. PAUSE 1 no-MESSAGE.
         END.
      END. /* previous page */

      /* NEXT page */
      ELSE IF LOOKUP(nap,"NEXT-page,page-down,+") > 0 THEN 
      DO WITH FRAME sel:
         /* PUT Cursor on downmost ROW */
         IF rtab[FRAME-DOWN] = ? THEN 
         DO:
            MESSAGE "YOU ARE ON THE LAST PAGE !".
            BELL. PAUSE 1 no-MESSAGE.
         END.
         ELSE 
         DO: /* downmost ROW was NOT empty*/
            Memory = rtab[FRAME-DOWN].
            FIND IFiSpx WHERE recid(IFiSpx) = Memory NO-LOCK.
            must-print = TRUE.
            NEXT LOOP.
         END.
      END. /* NEXT page */

      /* Search BY column 1 */
      ELSE IF LOOKUP(nap,"1,f1") > 0 THEN 
      DO ON ENDKEY UNDO, NEXT LOOP:
         cfc = "puyr". RUN Syst/ufcolor.p.
         Mancode = "".
         ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
         Disp lcBrand With FRAME f1.
         UPDATE lcBrand WHEN gcAllBrand = TRUE
            Mancode WITH FRAME f1.
         HIDE FRAME f1 NO-PAUSE.
         IF Mancode <> "" THEN 
         DO:
            FIND FIRST IFiSpx WHERE 
               IFiSpx.ManCode >= mancode AND
               ifispx.Brand = lcBrand NO-LOCK NO-ERROR.

            IF NOT  fRecFound(1) THEN NEXT Browse.
 
            NEXT LOOP.
         END.
      END. /* Search-1 */

      ELSE IF LOOKUP(nap,"5,f5") > 0 AND llIsAdmin THEN 
      DO:  /* add */
         {Syst/uright2.i}
         must-add = TRUE.
         NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"6,f6") > 0 AND llIsAdmin THEN 
      DO TRANSACTION:  /* DELETE */
         {Syst/uright2.i}
         delrow = FRAME-LINE.
         RUN local-find-this (FALSE).

         /* Highlight */
         COLOR DISPLAY VALUE(ctc)
         IFISpx.Brand IFiSpx.ManCode IFiSpx.ManCode .

         RUN local-find-NEXT.
         IF AVAILABLE IFiSpx THEN Memory = recid(IFiSpx). ELSE 
         DO:
            /* read back the record that is TO be  removed */
            RUN local-find-this (FALSE).                     

            RUN local-find-prev.
            IF AVAILABLE IFiSpx THEN 
            DO:
               ASSIGN
               delrow = delrow - 1  /* 'cause the LAST record is DELETEd */
               Memory = recid(IFiSpx).
            END.
         END.

         /* FIND back the ROW that is TO be removed */
         RUN local-find-this(TRUE).

         ASSIGN ok = FALSE.
         MESSAGE "ARE YOU SURE YOU WANT TO ERASE (Y/N) ? " UPDATE ok.
         COLOR DISPLAY VALUE(ccc)
         IFiSpx.ManCode IFiSpx.ManCode .
         IF ok THEN 
         DO:
            IF llDoEvent THEN RUN StarEventMakeDeleteEvent(lhIFisPX).
            DELETE IFiSpx.

            /* was LAST record DELETEd ? */
            IF NOT CAN-FIND(FIRST IFiSpx WHERE ifispx.Brand = lcBrand) THEN 
            DO:
               CLEAR FRAME sel NO-PAUSE.
               PAUSE 0 no-MESSAGE.
               LEAVE LOOP.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE delrow = 0. /* UNDO DELETE */
      END. /* DELETE */

      ELSE IF LOOKUP(nap,"enter,return") > 0  AND llIsAdmin THEN
      REPEAT WITH FRAME lis TRANSACTION ON ENDKEY UNDO, LEAVE:
         {Syst/uright2.i}
         /* change */
         RUN local-find-this(TRUE).
         RUN local-find-others.

         ASSIGN ac-hdr = " CHANGE " ufkey = TRUE ehto = 9.
         RUN Syst/ufkey.p.
         cfc = "lis". RUN Syst/ufcolor.p.
         CLEAR FRAME lis NO-PAUSE.
         DISPLAY IFiSpx.ManCode.
         IF llDoEvent THEN RUN StarEventSetOldBuffer(lhIFisPX).
         RUN local-UPDATE-record.

         HIDE FRAME lis NO-PAUSE.
         IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
                   KEYLABEL(LASTKEY) = "F4" THEN UNDO, LEAVE.
         IF llDoEvent THEN RUN StarEventMakeModifyEvent(lhIFisPX).
         RUN local-disp-row.
         xrecid = recid(IFiSpx).
         LEAVE.
      END.

      ELSE IF LOOKUP(nap,"home,h") > 0 THEN 
      DO:
         RUN local-find-FIRST.
         ASSIGN Memory = recid(IFiSpx) must-print = TRUE.
        NEXT LOOP.
      END.

      ELSE IF LOOKUP(nap,"END,e") > 0 THEN 
      DO : /* LAST record */
         RUN local-find-LAST.
         ASSIGN Memory = recid(IFiSpx) must-print = TRUE.
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
      FIND IFiSpx WHERE recid(IFiSpx) = rtab[frame-line(sel)] EXCLUSIVE-LOCK.
   ELSE
      FIND IFiSpx WHERE recid(IFiSpx) = rtab[frame-line(sel)] NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
   IF order = 1 THEN 
      FIND FIRST IFiSpx WHERE ifispx.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND FIRST IFiSpx USE-INDEX Mancode WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.
   /*    
   ELSE IF order = 3 THEN 
      FIND FIRST IFiSpx USE-INDEX index-3 WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.
   ELSE IF order = 4 THEN 
      FIND FIRST IFiSpx USE-INDEX index-4 WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.   
   */
END PROCEDURE.

PROCEDURE local-find-LAST:
   IF order = 1 THEN 
      FIND LAST IFiSpx WHERE ifispx.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND LAST IFiSpx USE-INDEX Mancode WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.
   /*   
   ELSE IF order = 3 THEN 
      FIND LAST IFiSpx USE-INDEX index-3 WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.
   ELSE IF order = 4 THEN 
      FIND LAST IFiSpx USE-INDEX index-4 WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.   
   */
END PROCEDURE.

PROCEDURE local-find-NEXT:
   IF order = 1 THEN 
      FIND NEXT IFiSpx WHERE ifispx.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND NEXT IFiSpx USE-INDEX Mancode WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.
  /*    
  ELSE IF order = 3 THEN 
     FIND NEXT IFiSpx USE-INDEX index-3 WHERE ifispx.Brand = lcBrand 
        NO-LOCK NO-ERROR.
  ELSE IF order = 4 THEN 
     FIND NEXT IFiSpx USE-INDEX index-4 WHERE ifispx.Brand = lcBrand 
        NO-LOCK NO-ERROR.   */
END PROCEDURE.

PROCEDURE local-find-prev:
   IF order = 1 THEN 
      FIND prev IFiSpx WHERE ifispx.Brand = lcBrand NO-LOCK NO-ERROR.
   ELSE IF order = 2 THEN 
      FIND prev IFiSpx USE-INDEX Mancode WHERE ifispx.Brand = lcBrand 
         NO-LOCK NO-ERROR.
  /*    
  ELSE IF order = 3 THEN 
     FIND prev IFiSpx USE-INDEX index-3 WHERE ifispx.Brand = lcBrand 
        NO-LOCK NO-ERROR.
  ELSE IF order = 4 THEN 
     FIND prev IFiSpx USE-INDEX index-4 WHERE ifispx.Brand = lcBrand 
        NO-LOCK NO-ERROR.   
  */
END PROCEDURE.

PROCEDURE local-disp-row:
   RUN local-find-others.
   DISPLAY
      IFISpx.Brand 
      IFiSpx.ManCode
      IFiSpx.SimArt
      SimMan.ManName when AVAIL SimMan
      "" when NOT AVAIL SimMan @ SimMan.ManName
      IFiSpx.Version
      WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND SimMan of IFiSpx no-lock no-error.
   FIND SimArt WHERE 
      SimArt.Brand  = lcBrand    AND 
      SimArt.SimArt = IFiSpx.SimArt NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-UPDATE-record:
   RUN local-find-others.
   DISP        
      IFiSpx.Version
      SimMan.ManName
      SimArt.SAName WHEN AVAIL SimArt
   WITH FRAME lis.
   UPDATE
      IFiSpx.SimArt
      IFiSpx.Hrowd
      IFiSpx.ICC
      IFiSpx.IMSI
      IFiSpx.IsCode1
      IFiSpx.IsCode2
      IFiSpx.IsUnb1
      IFiSpx.IsUnb2
      IFiSpx.Ki 
      IFiSpx.Isc1
   WITH FRAME lis EDITING:
      READKEY.
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN 
      DO WITH FRAME lis:
         PAUSE 0.
         IF FRAME-FIELD = "SimArt" THEN 
         DO:
            FIND SimArt WHERE SimArt.SimArt =
            INPUT FRAME lis IFiSpx.SimArt NO-LOCK NO-ERROR.
               IF NOT AVAIL SimArt THEN 
               DO:
                  BELL.
                  MESSAGE "Unknown SIM Article !".
                  NEXT.
               END.
               DISP SimArt.SAName WITH FRAME lis.
          END.
      END.
      APPLY LASTKEY.
   END. /* EDITING */
END PROCEDURE.

