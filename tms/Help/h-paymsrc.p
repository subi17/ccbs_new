/*------------------------------------------------------
  MODULE .......: H-PaymSrc.P
  PARENT .......: APPLHELP.P
  FUNCTION .....: HELP browser of payment sources
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 14.06.2002
  MODIFIED .....: 21.12.2005 values from TMSCodes
  VERSION ......: M15
  ------------------------------------------------------ */

{Syst/commali.i}

DEF shared VAR siirto AS CHAR.

DEF VAR lcPaymSrc   AS CHAR                 NO-UNDO.
DEF VAR rtab        AS RECID EXTENT 11      NO-UNDO.
DEF VAR ufkey       AS LOG init TRUE        NO-UNDO.
DEF VAR i           AS INT                  NO-UNDO.
DEF VAR memory      AS RECID                NO-UNDO.
DEF VAR must-print  AS logic                NO-UNDO.
DEF VAR must-add    AS logic                NO-UNDO.
DEF VAR lcSource    AS CHAR                 NO-UNDO.

DEF TEMP-TABLE ttPaymSrc NO-UNDO
    FIELD PaymSrc  AS CHAR
    FIELD SrcName  AS CHAR
    INDEX PaymSrc is unique PaymSrc.

form
    ttPaymSrc.PaymSrc  format "x(8)"  label "Source"
    ttPaymSrc.SrcName  format "x(40)" Label "Name"
    WITH scroll 1 11 DOWN  ROW 4 centered COLOR value(Syst.CUICommon:cfc)
    title color value(Syst.CUICommon:ctc) " Payment Sources " OVERLAY FRAME sel.

form /* SEEK code */
    lcPaymSrc
    help "Enter Name of a PaymSrc"
    with row 4 col 2 title color value(Syst.CUICommon:ctc) " FIND PaymSrc "
    COLOR value(Syst.CUICommon:cfc) NO-LABELS OVERLAY FRAME hayr.

Syst.CUICommon:cfc = "sel". RUN Syst/ufcolor.p. ASSIGN Syst.CUICommon:ccc = Syst.CUICommon:cfc.

lcPaymSrc = Func.Common:mTMSCodeList("Payment",
                             "PaymSrc").
                                 
DO i = 1 TO NUM-ENTRIES(lcPaymSrc,CHR(1)):

    lcSource = ENTRY(i,lcPaymSrc,CHR(1)).
    
    CREATE ttPaymSrc.
    ASSIGN ttPaymSrc.PaymSrc = entry(1,lcSource,CHR(9))
           ttPaymSrc.SrcName = entry(2,lcSource,CHR(9)).
END.

MAIN:
repeat:

   FIND FIRST ttPaymSrc no-lock no-error.
   IF NOT AVAILABLE ttPaymSrc THEN DO:
      must-print = FALSE.
      must-add = TRUE.
   END.
   ELSE DO:
      memory = recid(ttPaymSrc).
      must-print = TRUE.
   END.

   PAUSE 0.
   view FRAME sel.


   LOOP:
   Repeat WITH FRAME sel:

      print-line:
      DO :
         IF must-print THEN DO:
            CLEAR FRAME sel ALL no-pause.
            FIND ttPaymSrc where recid(ttPaymSrc) = memory no-lock no-error.

            /* Print TO screen */
            rtab = ?.
            DO WHILE frame-line<= FRAME-DOWN AND AVAILABLE ttPaymSrc:
               DISPLAY
               ttPaymSrc.PaymSrc
               ttPaymSrc.SrcName
               WITH FRAME sel.
               
               rtab[FRAME-LINE] = recid(ttPaymSrc).
               DOWN WITH FRAME sel.
               FIND NEXT ttPaymSrc no-lock no-error.
            END.
            must-print = FALSE.
            up frame-line(sel) - 1 WITH FRAME sel.
         END. /* must-print */

         IF ufkey THEN DO:
            ASSIGN
            Syst.CUICommon:ufk = 0 Syst.CUICommon:ufk[1] = 35 Syst.CUICommon:ufk[5] = 11
            Syst.CUICommon:ufk[6] = 0 Syst.CUICommon:ufk[8] = 8  Syst.CUICommon:ufk[9] = 1
            siirto = ? Syst.CUICommon:ehto = 3 ufkey = FALSE.
            
            /* not called from applhelp */    
            IF NOT gcHelpParam = "ahelp" THEN Syst.CUICommon:ufk[5] = 0.
            
            RUN Syst/ufkey.p.
         END.
     END. /* print-line */

     BROWSE:
     repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

         HIDE MESSAGE no-pause.
         CHOOSE ROW ttPaymSrc.PaymSrc {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(Syst.CUICommon:ccc) ttPaymSrc.PaymSrc WITH FRAME sel.

         if frame-value = "" AND rtab[FRAME-LINE] = ? THEN NEXT.
         Syst.CUICommon:nap = keylabel(LASTKEY).

         /* previous line */
         if lookup(Syst.CUICommon:nap,"cursor-up") > 0 THEN DO
         WITH FRAME sel:
            IF FRAME-LINE = 1 THEN DO:
               FIND ttPaymSrc where recid(ttPaymSrc) = rtab[FRAME-LINE] no-lock.
               FIND prev ttPaymSrc no-lock no-error.
               IF NOT AVAILABLE ttPaymSrc THEN DO:
                  BELL.
                  message "You are on 1st row !".              
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* a previous one was found */
                  scroll DOWN.
                  DO i = 11 TO 2 BY -1:
                     rtab[i] = rtab[i - 1].
                  END.
                  DISPLAY ttPaymSrc.PaymSrc ttPaymSrc.SrcName.
                  rtab[FRAME-LINE] = recid(ttPaymSrc).
                  memory = recid(ttPaymSrc).
               END.
            END.
            ELSE up 1.
         END. /* previous line */

         /* NEXT line */
         if lookup(Syst.CUICommon:nap,"cursor-down") > 0 THEN DO WITH FRAME sel:
            IF FRAME-LINE = FRAME-DOWN THEN DO:
               FIND ttPaymSrc where recid(ttPaymSrc) = rtab[FRAME-LINE] no-lock .
               FIND NEXT ttPaymSrc no-lock no-error.
               IF NOT AVAILABLE ttPaymSrc THEN DO:
                  BELL.
                  message "You are on last row !".
                  PAUSE 1 no-message.
                  NEXT BROWSE.
               END.
               ELSE DO:
                  /* yet another record was found */
                  scroll up.
                  DO i = 1 TO 10:
                     rtab[i] = rtab[i + 1].
                  END.
                  DISPLAY ttPaymSrc.PaymSrc ttPaymSrc.SrcName.
                  rtab[FRAME-LINE] = recid(ttPaymSrc).
                  /* finally LAST line's keyvalue is saved */
                  memory = rtab[1].
               END.
            END.
            ELSE DOWN 1 .
         END. /* NEXT line */

         /* previous page */
         else if lookup(Syst.CUICommon:nap,"page-up,prev-page") > 0 THEN DO WITH FRAME sel:
            FIND ttPaymSrc where recid(ttPaymSrc) = memory no-lock no-error.
            FIND prev ttPaymSrc no-lock no-error.
            IF AVAILABLE ttPaymSrc THEN DO:

               DO i = 1 TO (FRAME-DOWN - 1):
                  FIND prev ttPaymSrc no-lock no-error.
                  IF AVAILABLE ttPaymSrc THEN memory = recid(ttPaymSrc).
                  ELSE i = FRAME-DOWN.
               END.
               must-print = TRUE.
               NEXT LOOP.
            END.
            ELSE DO:
               /* this is the FIRST data page */
               BELL.
               message "This is the 1st page !".          
               PAUSE 1 no-message.
            END.
        END. /* previous page */

        /* NEXT page */
        else if lookup(Syst.CUICommon:nap,"page-down,next-page") > 0 THEN DO WITH FRAME sel:
           IF rtab[FRAME-DOWN] = ? THEN DO:
               BELL.
               message "This is the last page !".
               PAUSE 1 no-message.
           END.
           ELSE DO: /* the downmost line wasn't empty */
               memory = rtab[FRAME-DOWN].
               must-print = TRUE.
               NEXT LOOP.
           END.
        END. /* NEXT page */

        /* Seek */
        if lookup(Syst.CUICommon:nap,"1,f1") > 0 THEN DO:  /* PaymSrc */
           Syst.CUICommon:cfc = "puyr". RUN Syst/ufcolor.p.
           Syst.CUICommon:ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
           lcPaymSrc = "".
           set lcPaymSrc WITH FRAME hayr.
           HIDE FRAME hayr no-pause.
           IF lcPaymSrc ENTERED THEN DO:
              FIND FIRST ttPaymSrc where ttPaymSrc.PaymSrc >= lcPaymSrc
              no-lock no-error.
              IF NOT AVAILABLE ttPaymSrc THEN DO:
                 BELL.
                 message "None found !".    
                 PAUSE 1 no-message.
                 NEXT BROWSE.
              END.
              /*  PaymSrc was found */
              ASSIGN
                memory = recid(ttPaymSrc)
                must-print = TRUE.
           END.
           NEXT LOOP.
        END. /* Seek */

        /* CHOOSE */
        else if lookup(Syst.CUICommon:nap,"return,enter,5,f5") > 0 AND Syst.CUICommon:ufk[5] > 0 THEN DO:
           FIND ttPaymSrc where recid(ttPaymSrc) = rtab[FRAME-LINE] no-lock.
           siirto = string(ttPaymSrc.PaymSrc).
           LEAVE MAIN.
        END. /* CHOOSE */

        /* FIRST record */
        else if lookup(Syst.CUICommon:nap,"home,h") > 0 THEN DO:
           FIND FIRST ttPaymSrc no-lock.
           memory = recid(ttPaymSrc).
           must-print = TRUE.
           NEXT LOOP.
        END. /* FIRST record */

        /* LAST record */
        else if lookup(Syst.CUICommon:nap,"end,e") > 0 THEN DO :
           FIND LAST ttPaymSrc no-lock.
           memory = recid(ttPaymSrc).
           must-print = TRUE.
           NEXT LOOP.
        END. /* LAST record */

        else if Syst.CUICommon:nap = "8" or Syst.CUICommon:nap = "f8" THEN LEAVE MAIN. /* RETURN */

     END.  /* BROWSE */
   END.  /* LOOP */
END. /* MAIN */

HIDE FRAME sel no-pause.

