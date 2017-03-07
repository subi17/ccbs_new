/* ----------------------------------------------------------------------
  MODULE .......: PREASOW.P
  TASK .. ......: Browse AND UPDATE preselects WITH A-SubNo
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 15.03.2000
  MUUTOSPVM ....: 07.07.2000 ht dpstype = 0 prevented
                  26.07.2000 ht WHITE LIST MESSAGE
  VERSIO .......: SCRUNKO3, (23.10.96)
  --------------------------------------------------------------------- */
{Syst/commali.i}
{Func/timestamp.i}

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF INPUT PARAMETER an1 AS c NO-UNDO.
DEF INPUT PARAMETER an2 AS c NO-UNDO.

DEF VAR haku-CLI  LIKE CLI.CLI  NO-UNDO.
DEF VAR xrecid       AS RECID                           init ?.
DEF VAR ekarivi      AS INT                    NO-UNDO  init 0.
DEF VAR jarj         AS INT                    NO-UNDO  init 1.
DEF VAR jarjlkm      AS INT                    NO-UNDO  init 1.
DEF VAR ufkey        AS LOG                    NO-UNDO  init TRUE.
DEF VAR privi        AS INT                    NO-UNDO  init 0.
DEF VAR ed-jarj      AS INT                    NO-UNDO.
DEF VAR muisti       AS RECID                  NO-UNDO.
def var rivi         as int format "99"        NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR lm-ots       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
def var ok           as log format "Yes/No"    NO-UNDO.
def var pre          as lo  format "Yes/"      NO-UNDO.
def var chang        as lo  format "*/"        NO-UNDO   init FALSE.
DEF VAR pstyp   LIKE Presel.PsType   NO-UNDO.
DEF VAR pstyp2  LIKE Presel.PsType   NO-UNDO.
DEF VAR ordere  LIKE Presel.Orderer  NO-UNDO.
DEF VAR authn   LIKE Presel.AuthNo   NO-UNDO.
DEF VAR authdat LIKE Presel.AuthDate NO-UNDO.
DEF VAR dpstype LIKE Presel.PsType   NO-UNDO.
def var pecode       as c   format "x(2)"      NO-UNDO.
DEF VAR sav          AS lo                     NO-UNDO   init FALSE.
def var numb         as c   format "x(30)"     NO-UNDO.
DEF VAR pstypes      AS c                      NO-UNDO.
def var pstypet      as c   format "x(20)"     NO-UNDO.
def var asnimi       as c   format "x(30)"     NO-UNDO.


DEF WORKFILE psel
    FIELD CLI LIKE CLI.CLI
    field pres as lo format "Yes/".


/******
form
    "CustNo ..." Customer.CustNum                       SKIP
    "CustName ." Customer.CustName    format "x(35)"    SKIP
    "OrgCode .." Customer.OrgId     format "x(35)"    skip(1)
    "Subs.No .." numb                                 SKIP

    "Type ....." Presel.PsType pstypet                SKIP
    "Orderer .." Presel.Orderer     format "x(35)"    skip(1)

    "Author ..." Presel.AuthNo                        SKIP
    "AuthDate ." Presel.AuthDate                      skip(1)

    "ConfDate ." Presel.ConfDate                      SKIP
    "OutFSeq .." Presel.FileSeq1                      SKIP
    "InFSeq ..." Presel.FileSeq2                      SKIP
    "RetCode .." Presel.ReturnCode PreselErr.PSEName         SKIP
    "          " Presel.ErrText                        skip(1)

WITH  OVERLAY ROW 2 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " PRESELECTION "
    NO-LABELS 
  /*  1 columns */
    FRAME lis.
*******/

form
    "CustNo ..." Customer.CustNum                       SKIP
    "CustName ." asnimi
                 /*Customer.CustName*/    format "x(35)"    SKIP
    "OrgCode .." Customer.OrgId     format "x(35)"    skip(1)
    "Subs.No .." numb                                 SKIP

    "Type ....." dpstype HELP "Type of Presel. 1)Nat 2)Intn'l 3)Both"
    pstypet                      SKIP
    "Orderer .." Ordere  HELP "Orderer of preselection " 
                                    format "x(35)"    skip(1)

    /*"Author ..." AuthN                                skip*/
    "AuthDate ." AuthDat HELP "Date of Authorisation"
                                                      skip(1)

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " PRESELECTION "
    NO-LABELS 
  /*  1 columns */
    FRAME lispres.

form
    pre                       COLUMN-LABEL "Preselected"
    CLI.CLI    
    Presel.AuthDate
    Presel.SentDate
    Presel.ConfDate
    Presel.PsType         
    /*Presel.ReturnCode*/            
    pecode                    COLUMN-LABEL "Rc"
    chang                     COLUMN-LABEL "Ch"
WITH centered OVERLAY ROW 2 13 DOWN
    COLOR value(cfc) TITLE COLOR value(ctc) 
    " PRESELECT. OF CLI SERIES " + an1 + "-" + an2 + " " 
    FRAME sel.


form /* Numeron haku kentällä CustNum */
    haku-CLI 
    help "Enter A-sub no. or its first digits"         
    with row 4 col 2 title color value(ctc) " FIND A-SUB. NO "
    COLOR value(cfc) NO-LABELS OVERLAY FRAME haku-f1.

form 
SKIP(1)
"   You have still some marked CLI left!        " SKIP
"   Do you wish to continue anyway? (Y/N)"     
    ok help "Continue without saving?"       SKIP(1)
    with row 6 centered title color value(ctc) " NOT SAVED "
    NO-LABELS OVERLAY FRAME saved.


cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
view FRAME sel.

pstypes = "NATIONAL,INTERNATIONAL,NAT & INT".


FIND FIRST CLI
where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
IF AVAILABLE CLI THEN 
 ASSIGN
   muisti       = recid(CLI)
   must-print   = TRUE
   must-add     = FALSE.

ELSE DO:
  MESSAGE 
  " FATAL ERROR ! RUN FOR YOUR LIFE.." SKIP
  " BUT CONTACT SYSTEM VENDOR FIRST ..."
  VIEW-AS ALERT-BOX ERROR.
  LEAVE.
END.

FIND Customer where Customer.CustNum = CLI.CustNum no-lock no-error.
/* set default VALUE FOR 'orderer' */
ordere = Customer.Contact.

/*
ASSIGN
   muisti       = ?
   must-print = FALSE
   must-add    = TRUE.
*/
LOOP:
repeat WITH FRAME sel:

    IF jarj <> ed-jarj THEN DO:
       ed-jarj = jarj.
/*
       if jarj = 1 then put screen row 19 col 30 " By number  ".
       if jarj = 2 then put screen row 19 col 30 " By name    ".
*/
    END.

tulostus:
   DO :
      IF must-print THEN DO:
         up FRAME-LINE - 1.
         FIND CLI where recid(CLI) = muisti no-lock no-error.

         /* tulostetaan 1 sivullinen tietoa ruudulle
         alkaen tietueesta, jonka avainarvo = muisti.
         alkaen rivilta privi */

         /* jos on juuri poistettu rivi, niin ... */
         IF privi > 0 THEN DOWN privi - 1.

         repeat WITH FRAME sel:
            IF AVAILABLE CLI THEN DO:

               RUN local-disp-row.

               rtab[FRAME-LINE] = recid(CLI).
               IF jarj = 1 THEN FIND NEXT CLI
               where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
          /*   ELSE IF jarj = 2 THEN FIND NEXT CLI USE-INDEX CLI
               where CLI >= an1 AND CLI <= an2 no-lock no-error.
               ELSE IF jarj = 3 THEN FIND NEXT CLI USE-INDEX index-3
               where CLI >= an1 AND CLI <= an2 no-lock no-error.
               ELSE IF jarj = 4 THEN FIND NEXT CLI USE-INDEX index-4
               where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
            END.
            ELSE DO:
               CLEAR no-pause.
               rtab[FRAME-LINE] = ?.
            END.
            IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
            DOWN.
         END.
         up FRAME-LINE - 1.
         DOWN ekarivi.
         ASSIGN ekarivi = 0
                must-print = FALSE.
         PAUSE 0 no-message.

         /* nyt on tulostettu 1 ruudullinen tavaraa ja kursori on ylim- */
      END. /* must-print = TRUE */
   END. /* tulostus */

   /* jos on viimeksi poistettu rivi: */
   IF privi > 0 THEN DOWN privi - 1.
   ASSIGN privi = 0.

BROWSE:
   repeat WITH FRAME sel ON ENDKEY UNDO, RETURN:

      IF ufkey THEN DO:
         ASSIGN
         ufk[1]= 36  ufk[2]= 0 ufk[3]= /*1500*/ 0 ufk[4]= 1501
         ufk[5]= 1500   ufk[6]= 15 ufk[7]= 1503 ufk[8]= 8 ufk[9]= 1
         ehto = 3 ufkey = FALSE.
         RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE no-pause.
      IF jarj = 1 THEN DO:
         CHOOSE ROW CLI.CLI {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.CLI WITH FRAME sel.
      END.
/*    ELSE IF jarj = 2 THEN DO:
         CHOOSE ROW CLI.CLI {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.CLI WITH FRAME sel.
      END.
      IF jarj = 3 THEN DO:
         CHOOSE ROW CLI.?? {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.?? WITH FRAME sel.
      END.
      ELSE IF jarj = 4 THEN DO:
         CHOOSE ROW CLI.??  {Syst/uchoose.i} no-error WITH FRAME sel.
         COLOR DISPLAY value(ccc) CLI.? WITH FRAME sel.
      END.
*/
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      if lookup(nap,"cursor-right") > 0 THEN DO:
         jarj = jarj + 1. IF jarj > jarjlkm THEN jarj = 1.
      END.
      if lookup(nap,"cursor-left") > 0 THEN DO:
         jarj = jarj - 1. IF jarj = 0 THEN jarj = jarjlkm.
      END.

      IF jarj <> ed-jarj THEN DO:
         ASSIGN ekarivi = 0 muisti = rtab[FRAME-LINE].
         FIND CLI where recid(CLI) = muisti.
         DO i = 1 TO FRAME-LINE - 1:
            IF jarj = 1 THEN FIND prev CLI
            where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
       /*   ELSE IF jarj = 2 THEN FIND prev CLI USE-INDEX CLI
            where CLI >= an1 AND CLI <= an2 no-lock no-error.
            ELSE IF jarj = 3 THEN FIND prev CLI USE-INDEX index-3
            where CLI >= an1 AND CLI <= an2 no-lock no-error.
            ELSE IF jarj = 4 THEN FIND prev CLI USE-INDEX index-4
            where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
            IF AVAILABLE CLI THEN
               ASSIGN ekarivi = i muisti = recid(CLI).
            ELSE LEAVE.
         END.
         must-print = TRUE.
         NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
         BELL.
         message "You are on empty row, move upwards !".
         PAUSE 1 no-message.
         NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* edellinen rivi */
      if lookup(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
         IF FRAME-LINE = 1 THEN DO:
            FIND CLI where recid(CLI) = rtab[1] no-lock.
            IF jarj = 1 THEN FIND prev CLI
            where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
      /*    ELSE IF jarj = 2 THEN FIND prev CLI USE-INDEX CLI
            where CLI >= an1 AND CLI <= an2 no-lock no-error.
            ELSE IF jarj = 3 THEN FIND prev CLI USE-INDEX index-3
            where CLI >= an1 AND CLI <= an2 no-lock no-error.
            ELSE IF jarj = 4 THEN FIND prev CLI USE-INDEX index-4
            where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
            IF NOT AVAILABLE CLI THEN DO:
               message "YOU ARE ON THE FIRST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* edellinen loytyi */
               scroll DOWN.
               RUN local-disp-row.
               /*display CLI.CLI pre chang.*/
               DO i = FRAME-DOWN TO 2 BY -1:
                  rtab[i] = rtab[i - 1].
               END.
               ASSIGN
               rtab[1] = recid(CLI)
               muisti = rtab[1].
            END.
         END.
         ELSE up 1.
      END. /* edellinen rivi */

      /* seuraava rivi */
      else if lookup(nap,"cursor-down") > 0 THEN DO
      WITH FRAME sel:
         IF FRAME-LINE = FRAME-DOWN THEN DO:
            FIND CLI where recid(CLI) = rtab[FRAME-DOWN] no-lock .
            IF jarj = 1 THEN FIND NEXT CLI
            where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
      /*    ELSE IF jarj = 2 THEN FIND NEXT CLI USE-INDEX CLI
            where CLI >= an1 AND CLI <= an2 no-lock no-error.
            ELSE IF jarj = 3 THEN FIND NEXT CLI USE-INDEX index-3
            where CLI >= an1 AND CLI <= an2 no-lock no-error.
            ELSE IF jarj = 4 THEN FIND NEXT CLI USE-INDEX index-4
            where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
            IF NOT AVAILABLE CLI THEN DO:
               message "YOU ARE ON THE LAST ROW !".
               BELL. PAUSE 1 no-message.
               NEXT BROWSE.
            END.
            ELSE DO:
               /* loytyi viela seuraava tietue */
               scroll up.
               RUN local-disp-row.
               /*display CLI.CLI pre chang.*/
               DO i = 1 TO FRAME-DOWN - 1:
                  rtab[i] = rtab[i + 1].
               END.
               rtab[FRAME-DOWN] = recid(CLI).
               /* ja lopuksi pannaan muistiin ylimman rivin avain */
               muisti = rtab[1].
            END.
         END.
         ELSE DOWN 1 .
      END. /* seuraava rivi */

      /* edellinen sivu */
      else if lookup(nap,"prev-page,page-up") > 0 THEN DO:
         muisti = rtab[1].
         FIND CLI where recid(CLI) = muisti no-lock no-error.
         IF jarj = 1 THEN FIND prev CLI
         where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
    /*   ELSE IF jarj = 2 THEN FIND prev CLI USE-INDEX CLI
         where CLI >= an1 AND CLI <= an2 no-lock no-error.
         ELSE IF jarj = 3 THEN FIND prev CLI USE-INDEX index-3
         where CLI >= an1 AND CLI <= an2 no-lock no-error.
         ELSE IF jarj = 4 THEN FIND prev CLI USE-INDEX index-4
         where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
         IF AVAILABLE CLI THEN DO:
            muisti = recid(CLI).

            /* mennään tiedostoa taaksepäin 1 sivun verran */
            DO rivi = 1 TO (FRAME-DOWN - 1):
               IF jarj = 1 THEN FIND prev CLI
               where CLI >= an1 AND CLI <= an2 USE-INDEX CLI no-lock no-error.
        /*     ELSE IF jarj = 2 THEN FIND prev CLI USE-INDEX CLI
               where CLI >= an1 AND CLI <= an2 no-lock no-error.
               ELSE IF jarj = 3 THEN FIND prev CLI USE-INDEX index-3
               where CLI >= an1 AND CLI <= an2 no-lock no-error.
               ELSE IF jarj = 4 THEN FIND prev CLI USE-INDEX index-4
               where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
               IF AVAILABLE CLI THEN muisti = recid(CLI).
               ELSE rivi = FRAME-DOWN.
            END.
            must-print = TRUE.
            NEXT LOOP.
         END.
         ELSE DO:
            /* sivun eka oli myos tiedoston eka */
            message "THIS IS THE FIRST LINE !".
            BELL. PAUSE 1 no-message.
         END.
     END. /* edellinen sivu */

     /* seuraava sivu */
     else if lookup(nap,"next-page,page-down") > 0 THEN DO WITH FRAME sel:
        /* kohdistin alimmalle riville */
        IF rtab[FRAME-DOWN] = ? THEN DO:
            message "THIS IS THE LAST PAGE !".
            BELL. PAUSE 1 no-message.
        END.
        ELSE DO: /* alin rivi ei ollut tyhja */
            muisti = rtab[FRAME-DOWN].
            FIND CLI where recid(CLI) = muisti no-lock.
            must-print = TRUE.
            NEXT LOOP.
        END.
     END. /* seuraava sivu */

     /* Haku 1 */
     else if lookup(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
        cfc = "puyr". RUN Syst/ufcolor.p.
        haku-CLI = "".
        ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
        UPDATE haku-CLI WITH FRAME haku-f1.
        HIDE FRAME haku-f1 no-pause.
        if haku-CLI <> "" THEN DO:
           FIND FIRST CLI where CLI.CLI >= haku-CLI
           AND CLI >= an1 AND CLI <= an2 no-lock no-error.
           IF NOT AVAILABLE CLI THEN DO:
              BELL.
              message "NONE FOUND !".      
              PAUSE 1 no-message.
              NEXT BROWSE.
           END.
           /* joku nnatno/an-asno loytyi */
           ASSIGN jarj = 1 muisti = recid(CLI) must-print = TRUE.
           NEXT LOOP.
        END.
     END. /* Haku sar. 1 */

     else if lookup(nap,"5,f5") > 0 THEN DO:
       FIND CLI where recid(CLI) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.
       RUN Mf/viewpres.p(CLI.CLI).
       ufkey = TRUE.
     END.

     else if lookup(nap,"4,f4") > 0 THEN DO: /* mark */
       FIND CLI where recid(CLI) = rtab[FRAME-LINE] NO-LOCK.
          /* is this CLI marked FOR Presel */
          FIND FIRST Presel WHERE Presel.CLI = CLI.CLI AND
                            Presel.PsType NE 0 NO-LOCK NO-ERROR.
          IF AVAIL Presel THEN DO:

             /* is the Presel record already Sent TO TELIA ?*/
             IF Presel.SentDate NE ? THEN 
             MESSAGE
             "This A-sub. no. has already been reported to Telia" SKIP
             "as a Carrier Preselect number." SKIP(1)
             "You should only change the RepType CODE "
             VIEW-AS ALERT-BOX INFORMATION.
          END.


          IF NOT AVAIL Presel THEN pre = FALSE.
          ELSE pre = TRUE.

          /* look FOR a TEMP-TABLE record */   
          FIND FIRST psel WHERE psel.CLI = CLI.CLI NO-LOCK NO-ERROR.

          IF AVAIL psel THEN DO: /* unmark */
             DELETE psel.
             chang = FALSE.
          END.
          ELSE DO:              /* mark this CLI into TEMP-TABLE */
             chang = TRUE.
             CREATE psel.
             ASSIGN
             psel.CLI = CLI.CLI.
          END.


       DISPLAY
         pre
         chang
       WITH FRAME sel.

     END.

     else if lookup(nap,"7,f7") > 0 THEN DO:

        ASSIGN
        ehto  = 9
        ufkey = TRUE.
        RUN Syst/ufkey.p.

        MESSAGE
        "All customer's A-sub. nos. between/including"    SKIP
        an1 "-" an2                                       SKIP
        "which have NOT yet been marked for Preselection" SKIP
        "are now being marked for Preselection."          SKIP
        VIEW-AS ALERT-BOX INFORMATION.

        RUN presel(TRUE).

        must-print = TRUE.
        HIDE FRAME lispres.
        LEAVE.

     END. 

     else if lookup(nap,"6,f6") > 0 THEN DO:
       FIND FIRST psel NO-LOCK NO-ERROR.
       IF NOT AVAIL psel THEN DO:
          MESSAGE 
          "   No changes were done !"
          VIEW-AS ALERT-BOX INFORMATION.
       END.
       ELSE DO:
         ASSIGN
         ehto  = 9
         ufkey = TRUE.
         RUN Syst/ufkey.p.

         RUN presel(FALSE).

         must-print = TRUE.
         HIDE FRAME lispres.
         LEAVE.
       END.

     END.

     else if lookup(nap,"enter,return") > 0 THEN
     DO:
       FIND CLI where recid(CLI) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.
       RUN Mf/viewpres.p(CLI.CLI).
       ufkey = TRUE.
     END.

     /**********
     else if lookup(nap,"enter,return") > 0 THEN
     DO:
       FIND CLI where recid(CLI) = rtab[FRAME-LINE] NO-LOCK NO-ERROR.
       RUN presel2.
       HIDE FRAME lis.
     END.
     ***********/

     else if lookup(nap,"home") > 0 THEN DO:
        IF jarj = 1 THEN FIND FIRST CLI
        where CLI.CLI >= an1 AND CLI.CLI <= an2 USE-INDEX CLI no-lock no-error.
   /*   ELSE IF jarj = 2 THEN FIND FIRST CLI USE-INDEX CLI
        where CLI >= an1 AND CLI <= an2 no-lock no-error.
        ELSE IF jarj = 3 THEN FIND FIRST CLI USE-INDEX index-3
        where CLI >= an1 AND CLI <= an2 no-lock no-error.
        ELSE IF jarj = 4 THEN FIND FIRST CLI USE-INDEX index-4
        where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
        ASSIGN muisti = recid(CLI) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"end") > 0 THEN DO : /* viimeinen tietue */
        IF jarj = 1 THEN FIND LAST CLI
        where CLI.CLI >= an1 AND CLI.CLI <= an2 USE-INDEX CLI no-lock no-error.
  /*    ELSE IF jarj = 2 THEN FIND LAST CLI USE-INDEX CLI
        where CLI >= an1 AND CLI <= an2 no-lock no-error.
        ELSE IF jarj = 3 THEN FIND LAST CLI USE-INDEX index-3
        where CLI >= an1 AND CLI <= an2 no-lock no-error.
        ELSE IF jarj = 4 THEN FIND LAST CLI USE-INDEX index-4
        where CLI >= an1 AND CLI <= an2 no-lock no-error.   */
        ASSIGN muisti = recid(CLI) must-print = TRUE.
        NEXT LOOP.
     END.

     else if lookup(nap,"8,f8") > 0 THEN DO:
        FIND FIRST psel NO-LOCK NO-ERROR.
        PAUSE 0.
        IF AVAIL psel THEN DO:
           UPDATE
             ok
           WITH FRAME saved.
           HIDE FRAME saved no-pause.
           IF NOT ok THEN NEXT LOOP.
        END.   

        LEAVE LOOP.
     END.
  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel no-pause.
si-recid = xrecid.



PROCEDURE local-disp-row:
       /* FIND additional information from other tables FOR DISPLAY */
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
         CLI.CLI
         pre
         Presel.PsType   when AVAIL Presel
         /*Presel.ReturnCode*/ pecode  when AVAIL Presel
         Presel.AuthDate when AVAIL Presel
         Presel.ConfDate when AVAIL Presel
         Presel.SentDate when AVAIL Presel
         chang
       WITH FRAME sel.
END PROCEDURE.


PROCEDURE local-find-others.

       FIND FIRST psel WHERE psel.CLI = CLI.CLI NO-LOCK NO-ERROR.
       IF AVAIL psel THEN DO:
          chang = TRUE.
          IF sav THEN DO:
             FIND FIRST Presel WHERE Presel.CLI  =  CLI.CLI AND
                            Presel.PsType NE 0 NO-LOCK NO-ERROR.


             IF NOT AVAIL Presel THEN pre = TRUE. 
             ELSE pre = FALSE. 
          END.
       END.
       ELSE DO:
          chang = FALSE.
          FIND Presel WHERE Presel.CLI = CLI.CLI AND 
                            Presel.PsType NE 0 NO-LOCK NO-ERROR.

          IF NOT AVAIL Presel THEN pre = FALSE.
          ELSE pre = TRUE.
       END.

       FIND Presel WHERE Presel.CLI = CLI.CLI NO-LOCK NO-ERROR.

       IF AVAIL Presel THEN DO:
          IF Presel.FileSeq2 = 0 THEN DO:
             pecode = "".

          END.
          ELSE DO:
             pecode = STRING(Presel.ReturnCode).
          END.
       END.

END PROCEDURE.




PROCEDURE Presel:

DEF INPUT PARAMETER doall AS LO NO-UNDO.


DEF VAR e  AS I  NO-UNDO.
DEF VAR ft AS LO NO-UNDO.


IF doall THEN DO:
   numb = TRIM(STRING(an1) + " - " + STRING(an2)).
END.
ELSE numb = "SELECTED ONES".


DO TRANSAction:
   ASSIGN
   authdat = TODAY
   asnimi  = Customer.CustName
   dpstype = 3.
   pstypet = ENTRY(dpstype,pstypes).
   PAUSE 0. 
   DISPLAY
      Customer.CustNum
      /*Customer.CustName*/
      Customer.OrgId
      numb
      pstypet
      authdat
   WITH FRAME lispres.

   UPDATE 
     asnimi
     dpstype
     /*pstyp*/
     ordere
     /*authn*/
     authdat

   WITH FRAME lispres 
   EDITING:
      READKEY.

      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO WITH FRAME lispres:
      PAUSE 0.
            IF FRAME-FIELD = "dpstype" THEN DO:
               IF INPUT FRAME lispres dpstype > 3  OR
                  INPUT FRAME lispres dpstype < 1  THEN DO:
                    disp "" @ pstypet WITH FRAME lispres.
                    MESSAGE "You have to choose a RepType between 1-3 !".
                    NEXT.
               END.
               pstypet = ENTRY(INPUT dpstype,pstypes).
               DISP pstypet WITH FRAME lispres.
            END.
            /*
            IF FRAME-FIELD = "Ordere" THEN DO:
               IF INPUT FRAME lispres Ordere = "" THEN DO:
                  MESSAGE "You have to enter Orderer of Transaction!".
                  NEXT.
               END.
            END.
            IF FRAME-FIELD = "AuthN" THEN DO:
               IF INPUT FRAME lispres AuthN = "" THEN DO:
                  MESSAGE "You have to enter Code of Author".
                  NEXT.
               END.
            END.
            */ 
          END.
         APPLY LASTKEY.
   END. /* EDITING */

   sav = TRUE.

   IF doall THEN DO:

      /******************************************* 
      * CREATE a Presel record FOR every NOT yet *
      * marked a-sub no.                         *
      *******************************************/
      FOR 
      EACH CLI NO-LOCK WHERE 
           CLI.CLI >= an1 AND
           CLI.CLI <= an2:

         FIND       Presel WHERE 
                    Presel.CLI = CLI.CLI NO-LOCK NO-ERROR.

         /* SKIP clis WITH existing Presel recs. */
         IF AVAIL Presel THEN NEXT.

         CREATE Presel.
         ASSIGN
            Presel.CustNum   = CLI.CustNum
          /*Presel.CustName  = asnimi*/  
            Presel.CLI    = CLI.CLI
            Presel.PsType   = dpstype
            Presel.Orderer  = ordere
            Presel.AuthNo   = authn
            Presel.AuthDate = authdat.
            Presel.CrStamp = fMakeTS().
            Presel.ChStamp = fMakeTS().

      END. /* FOR EACH */
   END.  /* IF DO ALL */ 

   ELSE DO:   

   /* Make changes TO the database */
   FOR EACH psel: /* go THRU runtime TEMP-TABLE */

       FIND Presel WHERE 
            Presel.CLI = psel.CLI NO-ERROR.

       /* DELETE previous Presel */
       IF AVAIL Presel THEN DO:
          IF Presel.ChStamp = 0 THEN DO:
             MESSAGE
             "This A-sub. no. has already been deleted from WHITE LIST." SKIP
             "After this change Carrier Preselect is going to Telia."    SKIP
             "Do you really want to do this?"                            SKIP(1)
             VIEW-AS ALERT-BOX QUESTION BUTTONS OK-CANCEL 

             UPDATE choice AS LOGICAL.
             CASE choice:
                WHEN TRUE THEN DELETE Presel.
                WHEN FALSE THEN DO: 
                   UNDO. 
                END.
             END.
          END.
       END.

       FIND CLI WHERE CLI.CLI = psel.CLI NO-LOCK NO-ERROR.

       CREATE Presel.
       ASSIGN
       Presel.CustNum   = CLI.CustNum
     /*Presel.CustName  = asnimi*/  
       Presel.CLI    = CLI.CLI
       Presel.PsType   = dpstype
       Presel.Orderer  = ordere
       Presel.AuthNo   = authn
       Presel.AuthDate = authdat.
       Presel.CrStamp = fMakeTS().
       Presel.ChStamp = fMakeTS().

   END. /* FOR EACH psel */
 END. /* ELSE */
 FOR EACH psel. DELETE psel. END.

 END. 
 LEAVE.

END. /* Presel */




