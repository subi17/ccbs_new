/*--------------------------------------------------------------
  MODULE .......: NNACCT.P
  FUNCTION .....: Accounting report > Excel
  APPLICATION ..: TELE1
  AUTHOR .......: PT
  CREATED ......: 12.06.1997 pt
  changePVM ....: 20.07.1997 pt  PARAMETER files xxxxxxx.act
                  07.11.1997 pt  MESSAGE ennen act-tied. dir:iA
  Version ......: M15
  -------------------------------------------------------------- */

{Syst/commali.i}


DEF STREAM ticket.
DEF NEW shared STREAM excel.

DEF VAR cnum     AS i  NO-UNDO init 1.

DEF VAR i        AS i  NO-UNDO.
DEF VAR mm       AS i  NO-UNDO.
DEF VAR dd       AS i  NO-UNDO.
DEF VAR yy       AS i  NO-UNDO.
def var ok       as lo no-undo format "Yes/No".
DEF VAR amt1     AS i  NO-UNDO.
DEF VAR amt2     AS i  NO-UNDO.
DEF VAR t        AS c  NO-UNDO.
DEF VAR okay     AS lo NO-UNDO.
DEF VAR tab      AS c  NO-UNDO.
DEF VAR xdate    AS DA NO-UNDO.
DEF VAR fsize    AS i  NO-UNDO.
DEF VAR debug    AS lo NO-UNDO.

DEF VAR excdir   AS c  NO-UNDO.
DEF VAR excfile  AS c  NO-UNDO.
DEF VAR ticfile  AS c  NO-UNDO.

DEF VAR tic_date AS c  NO-UNDO.
DEF VAR tic_sday AS c  NO-UNDO.
DEF VAR tic_time AS c  NO-UNDO.
DEF VAR tic_stim AS i  NO-UNDO.
DEF VAR tic_ttim AS i  NO-UNDO.
DEF VAR tic_bsub AS c  NO-UNDO.

DEF VAR pksec    AS i NO-UNDO.
DEF VAR opsec    AS i NO-UNDO.

DEF VAR bsubs    AS c NO-UNDO.

DEF VAR cname    AS c NO-UNDO.
DEF VAR bnfile   AS c NO-UNDO.



DEF WORKFILE wstat
   field wdate   as da   format "99.99.9999"
   field wamtall as i    format "zz,zzz,zz9"
   field wamtok  as i    format "zz,zzz,zz9"
   field wsec    as i    format "zzz,zzz,zz9" EXTENT 2.

form
  skip(1)
  "   NOTE!  This program will read in an ASCII-file which is converted "
  "          from one or several CH... -files with command 'call'."  skip(1)
  "          Summaru contains" SKIP
  "          - total sum of calls according to customer's B-numbers" SKIP
  "          - total sum of calls with curation more than 0 seconds"
  "          - total sum of sec/min of these calls, peak/off-peak" SKIP
  "          for each day." skip(1)
  "          Summary is written to a .txt -file which cal later be"
  "          opened in Ms Excel." skip(1)
  "          File to be read in ......." ticfile no-label format "x(40)"
  help "Enter file name to be read in/analyzed"  SKIP
  "          File for output .........." excfile no-label format "x(40)"
  help "Enter the file name for the Excel-format summary file"
  skip(1)
  "          Customers B-number list .." bnfile no-label format "x(20)"

WITH
   color value(cfc) title color value(ctc) " Accounting Report to Excel "
   OVERLAY width 80 side-labels FRAME krit.

form
   bnfile format "x(16)" NO-LABEL
WITH
   color value(cfc) title color value(ctc) " Seekt customer/file "
   fsize DOWN OVERLAY ROW 3 col 60 FRAME act.


ticfile = "<Enter name of ascii-file>".
excfile = "<Enter name of excel-file.txt>".


if opsys ="msdos" THEN /*test purposes! */
   ASSIGN
   ticfile = "/home/nn/acct/testfil.txt"  excfile = "/tele1/acct.txt".

message "FINDer .act -filer, vAnta...".
/* seek ALL customers' PARAM files WITH extension '.act' */
if opsys = "msdos" THEN dos SILENT dir/b *.act > nnacct.tmp.
ELSE
if opsys = "unix"  THEN UNIX SILENT ls -1 *.act > nnacct.tmp.

INPUT from nnacct.tmp no-echo.
repeat.
   IMPORT bnfile.
   if substr(bnfile,length(bnfile)) = "*" THEN ASSIGN
   bnfile = substr(bnfile,1,length(bnfile) - 1).

   if cname ne "" then cname = cname + ",".
   cname = cname + bnfile.
END.
INPUT CLOSE.

if opsys = "msdos" THEN cname = CAPS(cname).
fsize = minimum(12,num-entries(cname)).
tab = chr(9).

krit:
repeat WITH FRAME krit:
   cfc = "sel". RUN ufcolor.
   ehto = 9. RUN ufkey.
   disp "<" + entry(cnum,cname) + ">" @ bnfile.

   UPDATE ticfile excfile WITH FRAME krit
   EDITING:

      READKEY.
      IF lookup(keylabel(LASTKEY),poisnap) > 0 THEN DO:
    PAUSE 0.
    if frame-field = "ticfile" THEN DO:
       ASSIGN FRAME krit ticfile.
       if ticfile = "" THEN LEAVE krit.
       IF search(ticfile) = ? THEN DO:
          BELL.
          message "File not found !".
          NEXT.
       END.
    END.
    else if frame-field = "excfile" THEN DO:
       ASSIGN FRAME krit excfile.
       if excfile = "" THEN DO:
          NEXT-PROMPT ticfile.
          NEXT.
       END.
       if index(excfile,".txt") = 0 or index(excfile," ") > 0 THEN DO:
          BELL.
          MESSAGE
          "Invalid filename ! Filename must end with '.txt' !".
          NEXT.
       END.
       excfile = lc(excfile). DISP excfile.
    END.
      END.
      APPLY LASTKEY.
   END.

toimi:
   repeat:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[4] = 856 ufk[5] = 63 Ufk[8] = 8 ehto = 0.

      if search("./" + entry(cnum,cname)) = ? THEN DO:
    BELL. MESSAGE
    "Parameterfile"  entry(cnum,cname) "missing ! - hit ENTER !".
    ufk[5] = 0.
    PAUSE no-message.
      END.

      RUN ufkey.
      IF toimi = 8 THEN LEAVE krit.
      IF toimi = 1 THEN NEXT  krit.
      IF toimi = 4 THEN DO WITH FRAME act.
    cfc = "lis". RUN ufcolor.
    CLEAR FRAME act ALL no-pause.
    DO i = 1 TO fsize WITH FRAME act.
       bnfile = entry(i,cname).
       DISP bnfile.
       IF FRAME-LINE < FRAME-DOWN THEN DOWN. ELSE up FRAME-LINE - 1.
    END.
    DOWN cnum - 1.
    CHOOSE ROW bnfile
    help "Choose customer and press ENTER !" {Syst/uchoose.i} no-error
    WITH FRAME act.
    COLOR DISPLAY value(ccc) bnfile.

    cnum = frame-line(act).
    bnfile = entry(cnum,cname).
    HIDE FRAME act no-pause.
    disp "<" + bnfile + ">" @ bnfile WITH FRAME krit.
    NEXT toimi.
      END.
      IF toimi = 5 THEN LEAVE toimi.
   END.


   ok = FALSE.
   message "Do you want to start reading in now (Y/N) ?"  UPDATE ok.
   IF ok THEN DO:

      bsubs = "".
      INPUT STREAM ticket from value(bnfile).
      repeat:
    IMPORT STREAM ticket t.
    bsubs = bsubs + substr(t,2) + ",".
      END.
      if bsubs ne "" THEN bsubs = substr(bsubs,1,length(bsubs) - 1).
      INPUT STREAM ticket CLOSE.

      INPUT STREAM ticket from VALUE (ticfile).

      amt1 = 0. amt2 = 0.

      message "Reading in calls ....".

TICKET:
      repeat.
    IMPORT STREAM ticket t.


    /* total amount of ALL read calls */
    amt1 = amt1 + 1.
    PUT SCREEN ROW 23 col 60 string(amt1).

    ASSIGN
    tic_bsub  = trim(substr(t,39,20))
    tic_date  = trim(substr(t,71,6))

    tic_time  = trim(substr(t,77,6))   /* hhmmss */


    tic_stim  = 3600 * integer(substr(tic_time,1,2)) +
           60   * integer(substr(tic_time,3,2)) +
             integer(substr(tic_time,5,2))

    tic_time  = trim(substr(t,109,10)) /* 1/100 -sec */
    tic_ttim  = (integer(tic_time) / 100) + tic_stim.


    IF debug THEN DO:
       disp tic_date string(tic_stim,"hh:mm:ss")  tic_time
       string(tic_ttim,"hh:mm:ss")  tic_bsub.
       PAUSE 0.
    END.

    /* does the B-subscriber number belong TO this customer ? */
    okay = FALSE.
    DO i = 1 TO num-entries(bsubs).
       IF tic_bsub BEGINS entry(i,bsubs) THEN ASSIGN okay = TRUE i = 99999.
    END.
    IF NOT okay THEN NEXT TICKET.

    /* total amount of ALL calls TO customer's numbers */
    amt2 = amt2 + 1.
    PUT SCREEN ROW 23 col 70 string(amt2).

    /* is this the FIRST call on the DAY 'tic-date' ? */

    /* convert char/yymmdd -date into Progress' Date FORMAT */
    ASSIGN
    yy = integer(substr(tic_date,1,2))
    mm = integer(substr(tic_date,3,2))
    dd = integer(substr(tic_date,5,2)).
    IF yy > 95 THEN yy = 1900 + yy.  ELSE yy = 2000 + yy.
    xdate = date(mm,dd,yy).

    FIND FIRST wstat where wstat.wdate = xdate exclusive-lock no-error.
    IF NOT AVAIL wstat THEN DO:
       CREATE wstat.
       ASSIGN wstat.wdate = xdate.
    END.


    IF tic_stim =tic_ttim THEN ASSIGN pksec = 0 opsec = 0.
    ELSE DO:

       /* calculate call's seconds on peak AND off-peak InstDuePeriod */
       RUN nncapo(
         INPUT  xdate,
         INPUT  tic_stim,
         INPUT  tic_ttim,

         OUTPUT pksec,
         OUTPUT opsec).
    END.
    ASSIGN
       wstat.wamtall = wstat.wamtall + 1
       wstat.wsec[1] = wstat.wsec[1] + pksec.
       wstat.wsec[2] = wstat.wsec[2] + opsec.

    IF pksec NE 0 OR opsec NE 0 THEN ASSIGN
       wstat.wamtok  = wstat.wamtok  + 1.

      END. /* TICKET */
      INPUT  STREAM ticket CLOSE.

      FIND FIRST wstat no-error.
      IF NOT AVAIL wstat THEN DO:
    BELL.
    PAUSE 0.
    message "The are no calls for customers B-nummer - not printed !".
    message "press ENTER !".
    PAUSE no-message.
    LEAVE krit.
      END.

      /* print out results TO an ascii PaymFile */
      OUTPUT STREAM excel TO   VALUE (excfile).

      PUT STREAM excel UNFORMATTED
   "Date"         tab
   "tot. Amount"  tab
   "Ant lyckade"  tab
   "Min/peak"     tab
   "Min/off-peak".

      RUN uexskip(2).

      FOR EACH  wstat BY wstat.wdate:

    PUT STREAM excel
    wstat.wdate                format "99.99.9999" tab
    wstat.wamtall              format "zzzzzzz9"   tab
    wstat.wamtok               format "zzzzzzz9"   tab
    wstat.wsec[1] / 60         format "zzzzzzz9"   tab
    wstat.wsec[2] / 60         format "zzzzzzz9".

    RUN uexskip(1).
      END.
      OUTPUT STREAM excel  CLOSE.
      PAUSE 0 no-message.
      message "File '" + excfile + "'" SKIP
              "is now ready to be read into Excel !" 
      view-as alert-box.
   END.
   LEAVE krit.
END. /* krit */
PAUSE 0 no-message.
HIDE FRAME krit no-pause.

