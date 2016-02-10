/* ----------------------------------------------------------------------
  MODULE .......: SubSerhist.P
  TASK .........: browse subscription's Service history
  APPLICATION ..: TMS
  AUTHOR .......: aam (from subser.p)
  CREATED ......: 13.12.04
  CHANGED ......: 
  Version ......: M15
 ---------------------------------------------------------------------- */

{Syst/commali.i}
{Func/sername.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'SubSer'}
{Syst/eventval.i}
{Func/msisdn.i}


IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {Func/lib/eventlog.i}

   DEFINE VARIABLE lhSubSer AS HANDLE NO-UNDO.
   lhSubSer = BUFFER SubSer:HANDLE.
   RUN StarEventInitialize(lhSubSer).

   DEFINE VARIABLE lhFixedFee AS HANDLE NO-UNDO.
   lhFixedFee = BUFFER FixedFee:HANDLE.
   RUN StarEventInitialize(lhFixedFee).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhSubSer).
   END.

END.

DEF INPUT PARAMETER   MsSeq LIKE MobSub.MsSeq NO-UNDO .

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR ServCom  LIKE SubSer.ServCom  NO-UNDO.
DEF VAR ServPac LIKE SubSer.ServPac NO-UNDO.
DEF VAR xrecid         AS RECID                           init ?.
DEF VAR FIRSTrow       AS INT                    NO-UNDO  init 0.
DEF VAR FrmRow         AS INT                    NO-UNDO  init 3.
DEF VAR FrmDown        AS INT                    NO-UNDO  init 11.
DEF VAR order          AS INT                    NO-UNDO  init 1.
DEF VAR orders         AS CHAR                   NO-UNDO.
DEF VAR maxOrder       AS INT                    NO-UNDO  init 2.
DEF VAR lcinfo         AS CHAR                   NO-UNDO.


DEF VAR ufkey          AS LOG                    NO-UNDO  init TRUE.
DEF VAR delrow         AS INT                    NO-UNDO  init 0.
DEF VAR pr-order       AS INT                    NO-UNDO.
DEF VAR Memory         AS RECID                  NO-UNDO.
DEF VAR RowNo          AS INT                    NO-UNDO.
DEF VAR must-print     AS LOG                    NO-UNDO.
DEF VAR must-add       AS LOG                    NO-UNDO.

DEF VAR ac-hdr         AS CHAR                   NO-UNDO.
DEF VAR rtab           AS RECID EXTENT 24        NO-UNDO.
DEF VAR i              AS INT                    NO-UNDO.
DEF VAR ok             AS log format "Yes/No"    NO-UNDO.
DEF VAR repl           AS log format "Yes/No"    NO-UNDO.
DEF VAR newsub         AS log format "Yes/No"    NO-UNDO.
DEF VAR defprof        AS CHAR                   NO-UNDO.
DEF VAR rc             AS INT                    NO-UNDO.

DEF VAR prev-stat      AS I                      NO-UNDO.
DEF VAR prev-param     AS C                      NO-UNDO.
DEF VAR data_found     AS LO                     NO-UNDO INIT FALSE.
DEF VAR delay          AS CHAR                   NO-UNDO.
DEF VAR xStartPeriod   AS INT                    NO-UNDO.

/* components of barring international (FOR Billing Event BARRINT) */
DEF VAR intbarrcode    AS C  INIT "BOIC,OBR"     NO-UNDO.
DEF VAR old-intbarr    AS I  EXTENT 2            NO-UNDO.
DEF VAR new-intbarr    AS I  EXTENT 2            NO-UNDO.

form
    SubSer.ServPac  format "x(3)" column-label "SP"
    SubSer.ServCom  
    SubSer.SSStat   format ">>>9" column-label "St"
    SubSer.SSDate   format "99-99-99" column-label "Date"
    ServCom.ScName  format "x(30)" 
    SubSer.SSParam  format "x(6)"
WITH ROW FrmRow centered OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc)  
    " Service History of MobSub " + MobSub.CLI + " "
    FRAME sel.

form
    SubSer.ServPac
    ServPac.SPName
    skip(1)
    SubSer.ServCom  
    ServCom.ScName 
    ServCom.ScLocalName
    SKIP(1)
    SubSer.SSStat
    SubSer.SSParam
    SubSer.SSDate

WITH  OVERLAY ROW 4 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    1 columns
    FRAME lis.

form /* seek Subscriber's Service  BY  ServCom */
    ServCom
    HELP "Enter service component code"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SERVICE COMPONENT "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f1.

form /* seek Subscriber's Service  BY ServPac */
    subser.ServPac
    HELP "Enter Service Package"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND SERVICE PACKAGE "
    COLOR VALUE(cfc) NO-LABELS OVERLAY FRAME f2.


FIND MobSub WHERE MobSub.MsSeq = MsSeq NO-LOCK.

cfc = "sel". RUN Syst/ufcolor. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "By Service,By ServPac,By 3, By 4".

{Func/tmsparam.i MobSubDefPack return}. defprof = TMSParam.CharVal.


RUN local-find-first.

IF AVAILABLE SubSer THEN ASSIGN
   Memory       = recid(SubSer)
   must-print   = TRUE
   must-add     = FALSE
   newsub       = FALSE.
ELSE DO:
   MESSAGE "No service history available !" VIEW-AS ALERT-BOX.
   RETURN.
END.

LOOP:
REPEAT WITH FRAME sel:

    IF order <> pr-order AND MaxOrder NE 1 THEN DO:
       pr-order = order.
       PUT SCREEN ROW FrmRow + FrmDown + 3 col 30 
       " " + ENTRY(order,orders) + " ".
    END.

    PrintPage:
    DO :
      IF must-print THEN DO:
        UP FRAME-LINE - 1.
        FIND SubSer WHERE recid(SubSer) = Memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'Memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE SubSer THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(SubSer).
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
        ufk[1]= 245  ufk[2]= 246 ufk[3]= 0 
        ufk[4]= 0
        ufk[5]= 0
        ufk[6]= 0
        ufk[7]= 0 ufk[8]= 8 
        ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW SubSer.ServCom ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SubSer.ServCom WITH FRAME sel.
      END.
      ELSE IF order = 2 THEN DO:
        CHOOSE ROW SubSer.ServPac ;(uchoose.i;) NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) SubSer.ServPac WITH FRAME sel.
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
        FIND SubSer WHERE recid(SubSer) = Memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE SubSer THEN
              ASSIGN FIRSTrow = i Memory = recid(SubSer).
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
           IF NOT AVAILABLE SubSer THEN DO:
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
                rtab[1] = recid(SubSer)
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
           IF NOT AVAILABLE SubSer THEN DO:
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
              rtab[FRAME-DOWN] = recid(SubSer).
              /* save RECID of uppermost ROW */
              Memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        Memory = rtab[1].
        FIND SubSer WHERE recid(SubSer) = Memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE SubSer THEN DO:
           Memory = recid(SubSer).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE SubSer THEN Memory = recid(SubSer).
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
           FIND SubSer WHERE recid(SubSer) = Memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 2 BUT ORDER IS STILL 1 !!!! */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f1.
       SET ServCom WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF ServCom ENTERED THEN DO:
          FIND FIRST SubSer WHERE 
                      SubSer.ServCom >= ServCom AND
                      SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
          IF NOT AVAILABLE SubSer THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SubSer/ServCom was found */
          ASSIGN order = 1 Memory = recid(SubSer) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 1 BUT ORDER IS 2 !!!! */
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.
       ehto = 9. RUN Syst/ufkey. ufkey = TRUE.
       CLEAR FRAME f2.
       SET ServPac WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF ServPac ENTERED THEN DO:
          FIND FIRST SubSer WHERE 
                     SubSer.ServPac >= ServPac AND
                     SubSer.MsSeq    = MsSeq 
          USE-INDEX ServPac  NO-LOCK NO-ERROR.
          IF NOT AVAILABLE SubSer THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some SubSer/ServPac was found */
          ASSIGN order = 2 Memory = recid(SubSer) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */


     ELSE IF LOOKUP(nap,"enter,return") > 0 THEN
     REPEAT WITH FRAME lis TRANSACTION
     ON ENDKEY UNDO, LEAVE:
       /* change */
       RUN local-find-this(FALSE).

       ASSIGN ac-hdr = " CHANGE " ufkey = TRUE.
       cfc = "lis". RUN Syst/ufcolor. CLEAR FRAME lis NO-PAUSE.
       DISPLAY SubSer.ServCom.

       RUN local-UPDATE-record.                                  
       HIDE FRAME lis NO-PAUSE.

       /* IF  User Wanted TO Cancel this Change TRANSACTION */
       IF LOOKUP(KEYFUNCTION(LASTKEY),"endkey,end-error") > 0 OR
       KEYLABEL(lastkey) = "F4" THEN UNDO, LEAVE.

       RUN local-disp-row.

       xrecid = recid(SubSer).
       LEAVE.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN Memory = recid(SubSer) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN Memory = recid(SubSer) must-print = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"8,f8") > 0 THEN  LEAVE LOOP.

  END.  /* BROWSE */
END.  /* LOOP */

HIDE FRAME sel NO-PAUSE.
si-recid = xrecid.


PROCEDURE local-find-this:

    DEF INPUT PARAMETER exlock AS lo NO-UNDO.

    IF exlock THEN
      FIND SubSer WHERE recid(SubSer) = rtab[frame-line(sel)] 
      EXCLUSIVE-LOCK.
    ELSE
       FIND SubSer WHERE recid(SubSer) = rtab[frame-line(sel)] 
       NO-LOCK.
    FIND ServCom WHERE 
         ServCom.Brand   = gcBrand AND 
         ServCom.ServCom = ENTRY(1,SubSer.ServCom,".") 
    NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST SubSer
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND FIRST SubSer USE-INDEX ServPac
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST SubSer
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND LAST SubSer USE-INDEX ServPac
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT SubSer
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND NEXT SubSer USE-INDEX ServPac
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV SubSer
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
       ELSE IF order = 2 THEN FIND PREV SubSer USE-INDEX ServPac
       WHERE SubSer.MsSeq = MsSeq NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       RUN local-find-others.
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY
       SubSer.ServCom
       SubSer.SSDate
       SubSer.ServPac
       SubSer.SSStat
       SubSer.SSParam
       fGetServiceName(SubSer.ServCom) @ ServCom.ScName
       WITH FRAME sel.
END PROCEDURE.

PROCEDURE local-find-others.
   FIND ServPac WHERE 
        ServPac.Brand   = gcBrand   AND 
        ServPac.ServPac = SubSer.ServPac NO-LOCK NO-ERROR.
   
   FIND ServCom WHERE 
        ServCom.Brand   = gcBrand  AND 
        ServCom.ServCom = ENTRY(1,SubSer.ServCom,".") NO-LOCK NO-ERROR.

END PROCEDURE.

PROCEDURE local-UPDATE-record:

   REPEAT ON ENDKEY UNDO, LEAVE:

      RUN local-find-others.
      
      DISP
         SubSer.ServPac
         ServPac.SPName  WHEN AVAIL ServPac
         fGetServiceName(SubSer.ServCom) 
            WHEN AVAIL servcom @ ServCom.ScName 
         fGetLocalName(SubSer.ServCom) 
            WHEN AVAIL ServCom  @ ServCom.ScLocalName
         SubSer.SSStat
         SubSer.SSDate
         SubSer.SSParam when ServCom.SCParameter
      WITH FRAME lis. 
          
      PAUSE MESSAGE "Press ENTER to continue".
     
      LEAVE.
   
   END.
   
END PROCEDURE.



