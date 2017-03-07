/* ----------------------------------------------------------------------------
  MODULE .......: evbrcust.p
  TASK .........: Browse Eventlog for one Customer
  APPLICATION ..: TMS
  AUTHOR .......: jr
  CREATED ......: 23-04-02
  CHANGED ......: 17.09.02/jr Viewer frame change and functionkeys
                  23.09.02/jr Find for table & key
                  23.09.02/jr Enter & F5 use same viewer
                  23.09.02/jr removed unused parts and cleaning
                  24.09.02/jr added indexes with date and table
                  25.09.02/aam changes to finds (f2 & f3),
                               bigger frame,
                               show " / " instead of chr(255) in Key
                  06.11.02/jr  Added Date to User search               
                  11.11.02 lp  modified from eventsel.p
                  30.12.02/aam different logic for retrieving related logs,
                               fCrTemp() etc.
                  03.03.03/tk  tokens             
                  13.09.04/aam PaymPlan added
                  26.10.04/aam parameters for eventview changed,
                               CallSpec4 added,
                               EventLogStatus to browser (Type)
                  15.04.05/aam PNPGroup AND PNPList added             
                  02.11.05/aam DDAuth added
                  10.01.06/aam Fatime added
  Version ......: M15
  --------------------------------------------------------------------------- */
{Syst/commali.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'eventlog'}

DEF INPUT PARAMETER xxkey LIKE Eventlog.Key NO-UNDO.
DEF VAR xxvalues LIKE Eventlog.DataValues   NO-UNDO.
DEF VAR valuecount AS INTEGER NO-UNDO.

DEF /* NEW */ shared VAR siirto AS CHAR.

DEF VAR evdate       LIKE Eventlog.EventDate   NO-UNDO. 
DEF VAR f1date       LIKE Eventlog.EventDate   NO-UNDO.
DEF VAR UserCode     LIKE Eventlog.UserCode    NO-UNDO.
DEF VAR xrecid       AS RECID                           INIT ?.
DEF VAR FIRSTrow     AS INT                    NO-UNDO  INIT 0.
DEF VAR FrmRow       AS INT                    NO-UNDO  INIT 1.
DEF VAR FrmDown      AS INT                    NO-UNDO  INIT 15.
DEF VAR order        AS INT                    NO-UNDO  INIT 1.
DEF VAR orders       AS CHAR                   NO-UNDO.
DEF VAR maxOrder     AS INT                    NO-UNDO  INIT 3.
DEF VAR ufkey        AS LOG                    NO-UNDO  INIT TRUE.
DEF VAR delrow       AS INT                    NO-UNDO  INIT 0.
DEF VAR pr-order     AS INT                    NO-UNDO.
DEF VAR memory       AS RECID                  NO-UNDO.
DEF VAR RowNo        AS INT                    NO-UNDO.
DEF VAR must-print   AS LOG                    NO-UNDO.
DEF VAR must-add     AS LOG                    NO-UNDO.
DEF VAR ac-hdr       AS CHAR                   NO-UNDO.
DEF VAR rtab         AS RECID EXTENT 24        NO-UNDO.
DEF VAR i            AS INT                    NO-UNDO.
DEF VAR ok           AS log format "Yes/No"    NO-UNDO.
DEF VAR lcTable      AS CHAR FORMAT "X(20)"    NO-UNDO.
DEF VAR lcKey        AS CHAR FORMAT "X(20)"    NO-UNDO.
DEF VAR lcevtime     AS CHAR FORMAT "XX:XX:XX" NO-UNDO.
DEF VAR liCount      AS INT                    NO-UNDO. 

DEFINE VARIABLE muutokset AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE temp-event 
   FIELD ttEventLog   AS RECID 
   FIELD ttdate       LIKE Eventlog.eventdate
   FIELD tttime       LIKE Eventlog.eventtime
   FIELD ttuser       LIKE Eventlog.UserCode 
   FIELD tttable      LIKE Eventlog.tablename
   FIELD ttkey        LIKE Eventlog.key
   FIELD ttaction     LIKE Eventlog.action
   FIELD ttDatavalues LIKE Eventlog.datavalues
   FIELD ttType       LIKE EventLog.EventLogStatus

INDEX ttdate IS PRIMARY ttdate DESC tttime DESC
INDEX ttuser            ttuser ttdate DESC tttime DESC
INDEX tttable           tttable ttkey ttdate DESC.

DEF TEMP-TABLE ttCodes NO-UNDO
   FIELD IntCode AS INT
   FIELD ChrCode AS CHAR.

DEF TEMP-TABLE ttPNP NO-UNDO
   FIELD PNPSeq AS INT
   INDEX PNPSeq PNPSeq.
 

FORM 
   liCount AT 2 LABEL "Collecting" FORMAT ">>>>>>>>9"
   WITH OVERLAY ROW 10 CENTERED TITLE " Customer's events " 
        FRAME fPassTime.

FUNCTION fCrTemp RETURNS LOGICAL.

   CREATE temp-event.
   ASSIGN
   temp-event.ttEventLog   = RECID(EventLog)
   temp-event.ttdate       = Eventlog.eventdate
   temp-event.tttime       = Eventlog.eventtime
   temp-event.ttuser       = Eventlog.usercode
   temp-event.tttable      = Eventlog.TableName
   temp-event.ttkey        = Eventlog.Key
   temp-event.ttaction     = Eventlog.action
   temp-event.ttDatavalues = Eventlog.DataValues
   temp-event.ttType       = EventLog.EventLogStatus.

   liCount = liCount + 1.
   PAUSE 0.
   DISPLAY liCount WITH FRAME fPassTime. 

   RETURN TRUE. 

END FUNCTION.    


FIND Customer NO-LOCK WHERE 
     Customer.CustNum = INTEGER(xxKey) NO-ERROR.
IF NOT AVAILABLE Customer THEN RETURN.      

FOR EACH Eventlog NO-LOCK  WHERE 
         Eventlog.TableName = "Customer" AND
         Eventlog.Key       = STRING(Customer.CustNum):

   fCrTemp().

END.

/* related tables and their logs;
   invoice, payment, custintevent, bdesthist, cgmember, commrule,
   custpnpgroup, custpnpser, fixedfee, ffitem, singlefee, invtext, memo,
   cli, cliser, presel,
   billtarg, mobsub, msowner, subser, msisdn, imsi, sim

*/

FOR EACH Invoice NO-LOCK WHERE
         Invoice.Brand   = gcBrand AND
         Invoice.CustNum = Customer.Custnum,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "Invoice" AND
         EventLog.Key       = STRING(Invoice.InvNum):

   fCrTemp().

END.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "Payment" AND
         EventLog.Key BEGINS STRING(Customer.CustNum) + CHR(255):

   fCrTemp().

END.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CustIntEvent" AND
         EventLog.Key  BEGINS STRING(Customer.CustNum) + CHR(255):

   fCrTemp().

END.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "DDAuth" AND
         EventLog.Key BEGINS STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END.                                  

FOR EACH BDestHist NO-LOCK WHERE
         BDestHist.CustNum = Customer.CustNum,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "BDestHist" AND
         /* this may give also other customers' bdesthists, but at the moment
            this is the only way */
         EventLog.Key BEGINS  BDestHist.BDest  + CHR(255):

   fCrTemp().
END. 

FOR EACH CgMember OF Customer NO-LOCK,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CgMember" AND
         EventLog.Key       = CgMember.CustGroup + CHR(255) +
                              STRING(CgMember.CustNum):

   fCrTemp().
END.                                

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CommRule" AND
         EventLog.Key BEGINS  STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END. 

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CustPNPGroup" AND
         EventLog.Key BEGINS  STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END. 

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "Fatime" AND
         EventLog.Key BEGINS  STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END. 


FOR EACH CustPNPSer OF Customer NO-LOCK,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CustPNPSer" AND
         EventLog.Key       = CustPNPSer.FromBDest + CHR(255) +
                              CustPNPSer.ToBDest   + CHR(255) +
                              STRING(Customer.CustNum):

   fCrTemp().

END. 

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "FixedFee" AND
         EventLog.Key BEGINS STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END.


FOR EACH FixedFee OF Customer NO-LOCK,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "FFItem" AND
         EventLog.Key BEGINS STRING(FixedFee.FFNum) + CHR(255):

   fCrTemp().
END.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "InvText" AND
         EventLog.Key BEGINS "Customer" + CHR(255) + 
                             STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "Memo" AND
         EventLog.Key BEGINS "Customer" + CHR(255) + 
                             STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END.

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "SingleFee" AND
         EventLog.Key       BEGINS(STRING(Customer.CustNum) + CHR(255)):
   fCrTemp().
END.

FOR EACH PaymPlan NO-LOCK WHERE
         PaymPlan.Brand = gcBrand AND
         PaymPlan.CustNum = Customer.CustNum:

   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "PaymPlan" AND
            EventLog.Key       = STRING(PaymPlan.PPlanID):
      fCrTemp().
   END.
       
   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "PPInv" AND
            EventLog.Key   BEGINS STRING(PaymPlan.PPlanID) + CHR(255):
      fCrTemp().
   END.
   
   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "PPBatch" AND
            EventLog.Key   BEGINS STRING(PaymPlan.PPlanID) + CHR(255):
      fCrTemp().
   END.
   
END.

/* printing of report 4 */
FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CallSpec4" AND
         EventLog.Key  BEGINS STRING(Customer.CustNum) + CHR(255):

   fCrTemp().

END.

FOR EACH CLI NO-LOCK WHERE 
         CLI.CustNum = Customer.CustNum,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CLI" AND
         /* this doesn't consider effective dates 
           (same cli for different customers) */
         EventLog.Key       = CLI.CLI + CHR(255) + 
                              STRING(Customer.CustNum):

   fCrTemp().
END.                             

FOR EACH CLISer NO-LOCK WHERE
         CLISer.CustNum = Customer.CustNum,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "CLISer" AND
         EventLog.Key       = CLISer.CLIFrom + CHR(255) +
                              CLISer.CLITo   + CHR(255) +
                              STRING(Customer.CustNum):

   fCrTemp().
END.

FOR EACH Presel NO-LOCK WHERE
         Presel.CustNum = Customer.CustNum,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "Presel" AND
         EventLog.Key       = Presel.CLI:

   fCrTemp().
END. 

FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "BillTarget" AND
         EventLog.Key BEGINS  STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END.

FOR EACH MobSub OF Customer NO-LOCK:

   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "MobSub" AND
            EventLog.Key       = STRING(MobSub.MsSeq):

      fCrTemp().
   END.

   FOR EACH SubSer OF MobSub NO-LOCK,
       EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "SubSer" AND
            EventLog.Key       = STRING(SubSer.MsSeq) + CHR(255) +
                                 SubSer.ServCom:
      fCrTemp().
   END.

END.

/* pnp group types */
FOR EACH TMSCodes NO-LOCK WHERE
         TMSCodes.TableName = "PNPGroup" AND
         TMSCodes.FieldName = "GroupType":
         
   CREATE ttCodes.
   ttCodes.IntCode = INTEGER(TMSCodes.CodeValue) NO-ERROR.
END.
 
EMPTY TEMP-TABLE ttPNP.

FOR EACH MsOwner OF Customer NO-LOCK:

   FOR EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "MsOwner" AND
            EventLog.Key       = MsOwner.CLI + CHR(255) + 
                                 STRING(MsOwner.TsEnd):

      fCrTemp().
   END.
   
   /* pnp groups */
   FOR EACH ttCodes,
       EACH PNPGroup NO-LOCK WHERE
            PNPGroup.Brand     = gcBrand         AND
            PNPGroup.GroupType = ttCodes.IntCode AND
            PNPGroup.PNPGroup  = MsOwner.CLI:
            
       IF CAN-FIND(FIRST ttPNP WHERE ttPNP.PNPSeq = PNPGroup.PNPSeq)
       THEN NEXT.
       
       CREATE ttPNP.
       ttPNP.PNPSeq = PNPGroup.PNPSeq.
       
       FOR EACH EventLog NO-LOCK WHERE
                EventLog.TableName = "PNPGroup" AND
                EventLog.Key       = gcBrand + CHR(255) + 
                                     STRING(PNPGroup.PNPSeq):
          fCrTemp().      
       END.
   END.

   /* logs created with new index 22.8.05 -> */
   FOR EACH ttCodes,
       EACH EventLog NO-LOCK WHERE
            EventLog.TableName = "PNPGroup" AND
            EventLog.Key       = gcBrand + CHR(255) + 
                                 STRING(ttCodes.IntCode) + CHR(255) +
                                 STRING(MsOwner.CLI):
          fCrTemp().      
   END.

END.

FOR EACH MSISDN NO-LOCK WHERE
         MSISDN.Brand   = Customer.Brand AND
         MSISDN.CustNum = Customer.CustNum,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "MSISDN" AND
         EventLog.Key       = MSISDN.CLI:

   fCrTemp().
END.

FOR EACH IMSI OF Customer NO-LOCK,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "IMSI" AND
         EventLog.Key       = IMSI.IMSI:

   fCrTemp().
END.

FOR EACH SIM OF Customer NO-LOCK,
    EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "SIM" AND
         EventLog.Key       = SIM.ICC:

   fCrTemp().
END.

/* pnp numbers */
FOR EACH EventLog NO-LOCK WHERE
         EventLog.TableName = "PNPList" AND
         EventLog.Key BEGINS gcBrand + CHR(255) + 
                             STRING(Customer.CustNum) + CHR(255):

   fCrTemp().
END.                                  

form
    temp-event.ttdate     FORMAT "99.99.99" COLUMN-LABEL "Date"
    temp-event.tttime                       COLUMN-LABEL "Time"
    temp-event.ttaction   FORMAT "X(8)"     COLUMN-LABEL "Action"
    temp-event.ttuser     FORMAT "X(8)"     COLUMN-LABEL "User"
    temp-event.tttable    FORMAT "X(13)"    COLUMN-LABEL "Table"
    temp-event.ttkey      FORMAT "X(23)"    COLUMN-LABEL "Key"
    temp-event.ttType     FORMAT ">9"       COLUMN-LABEL "Type"
WITH ROW FrmRow width 80 OVERLAY FrmDown  DOWN
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) " " + ynimi +
    " Eventlog BROWSER for Customer " + xxkey + "  "
    + string(pvm,"99.99.99") + " "
FRAME sel.

form
    temp-event.ttdate
    temp-event.tttime  
    temp-event.ttaction    /* LABEL FORMAT */
    temp-event.ttuser
    temp-event.tttable     /* LABEL FORMAT */
    temp-event.ttkey      
    temp-event.ttType
    muutokset VIEW-AS EDITOR size-chars 60 BY 10
WITH  OVERLAY ROW 3 centered
    COLOR VALUE(cfc)
    TITLE COLOR VALUE(ctc) ac-hdr 
    SIDE-LABELS 
    2 columns
FRAME lis.

form /* seek Eventlog  BY  Date */
    "Date..:" f1date
    HELP "Enter Date" SKIP
    "Time..:"
    lcevtime
    HELP "Enter Time 99:99:99"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Date & Time"
    COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME f1.

form /* seek Eventlog  BY UserCode */
    "User.:" UserCode
    HELP "Enter Usercode" SKIP
    "Date.:" evdate
    HELP "Enter Date"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND User "
    COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME f2.

form /* seek Eventlog  BY  TableName and keyvalue */
    "Table..:" lcTable HELP "Enter TableName or beginning of it " SKIP
    "Key....:" lcKey   HELP "Enter KeyValue or beginning of it"
    WITH row 4 col 2 TITLE COLOR VALUE(ctc) " FIND Table & Key "
    COLOR VALUE(cfc) NO-LABELS OVERLAY 
FRAME f3.

FIND FIRST temp-event NO-LOCK NO-ERROR.
IF AVAILABLE temp-event THEN ASSIGN
   memory       = recid(temp-event)
   must-print   = TRUE
   must-add     = FALSE.

ELSE DO:
   MESSAGE "NO Event history available for this Customer"
   VIEW-AS ALERT-BOX ERROR.
   RETURN.
END.

cfc = "sel". RUN Syst/ufcolor.p. ASSIGN ccc = cfc.
VIEW FRAME sel.

orders = "  By Date  ,  By User  ,  By Table ".

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
        FIND temp-event WHERE recid(temp-event) = memory NO-LOCK NO-ERROR.

        /* DISPLAY one page beginning the record 
        whose RECID is saved into 'memory'.
        starting from ROW 'delrow' */

        /* IF a ROW was recently DELETEd ... */
        IF delrow > 0 THEN DOWN delrow - 1.

        REPEAT WITH FRAME sel:
           IF AVAILABLE temp-event THEN DO:
              RUN local-disp-row.
              rtab[FRAME-LINE] = recid(temp-event).
              RUN local-find-NEXT.
           END.
           ELSE DO:
              CLEAR NO-PAUSE.
              rtab[FRAME-LINE] = ?.
           END.
           IF FRAME-LINE = FRAME-DOWN THEN LEAVE.
           DOWN.
        END.
        UP FRAME-LINE - 1.
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
        ufk[1]= 28  ufk[2]= 542 ufk[3]= 2121 ufk[4]= 0
        ufk[5]=265  ufk[6]=0    ufk[7]= 0    ufk[8]= 8 ufk[9]= 1
        ehto = 3 ufkey = FALSE.
        RUN Syst/ufkey.p.
      END.

      HIDE MESSAGE NO-PAUSE.
      IF order = 1 THEN DO:
        CHOOSE ROW temp-event.ttdate {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) temp-event.ttdate WITH FRAME sel.
      END.

      ELSE  IF order = 2 THEN DO:
        CHOOSE ROW temp-event.ttuser {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) temp-event.ttuser WITH FRAME sel.
      END.
      IF order = 3 THEN DO:
        CHOOSE ROW temp-event.tttable {Syst/uchoose.i} NO-ERROR WITH FRAME sel.
        COLOR DISPLAY VALUE(ccc) temp-event.tttable WITH FRAME sel.
      END.
      IF rtab[FRAME-LINE] = ? THEN NEXT.

      nap = keylabel(LASTKEY).

      IF LOOKUP(nap,"cursor-right") > 0 THEN DO:
        order = order + 1. IF order > maxOrder THEN order = 1.
      END.
      IF LOOKUP(nap,"cursor-left") > 0 THEN DO:
        order = order - 1. IF order = 0 THEN order = maxOrder.
      END.

      IF order <> pr-order AND MaxOrder > 1 THEN DO:
        ASSIGN FIRSTrow = 0 memory = rtab[FRAME-LINE].
        FIND temp-event WHERE recid(temp-event) = memory NO-LOCK.
        DO i = 1 TO FRAME-LINE - 1:
           RUN local-find-PREV.
           IF AVAILABLE temp-event THEN
              ASSIGN FIRSTrow = i memory = recid(temp-event).
           ELSE LEAVE.
        END.
        must-print = TRUE.
        NEXT LOOP.
      END.

      IF rtab[FRAME-LINE] = ? AND NOT must-add THEN DO:
        BELL.
        MESSAGE "You are on an empty row, move upwards !".
        PAUSE 1 NO-MESSAGE.
        NEXT.
      END.

      ASSIGN nap = keylabel(LASTKEY).

      /* PREVious ROW */
      IF LOOKUP(nap,"cursor-up") > 0 THEN DO WITH FRAME sel:
        IF FRAME-LINE = 1 THEN DO:
           RUN local-find-this(FALSE).
           RUN local-find-PREV.
           IF NOT AVAILABLE temp-event THEN DO:
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
                rtab[1] = recid(temp-event)
                memory  = rtab[1].
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
           IF NOT AVAILABLE temp-event THEN DO:
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
              rtab[FRAME-DOWN] = recid(temp-event).
              /* save RECID of uppermost ROW */
              memory = rtab[1].
           END.
        END.
        ELSE DOWN 1 .
      END. /* NEXT ROW */

      /* PREV page */
      ELSE IF LOOKUP(nap,"PREV-page,page-up,-") > 0 THEN DO:
        memory = rtab[1].
        FIND temp-event WHERE recid(temp-event) = memory NO-LOCK NO-ERROR.
        RUN local-find-PREV.
        IF AVAILABLE temp-event THEN DO:
           memory = recid(temp-event).

           /* reverse 1 page */
           DO RowNo = 1 TO (FRAME-DOWN - 1):
              RUN local-find-PREV.
              IF AVAILABLE temp-event THEN memory = recid(temp-event).
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
           memory = rtab[FRAME-DOWN].
           FIND temp-event WHERE recid(temp-event) = memory NO-LOCK.
           must-print = TRUE.
           NEXT LOOP.
       END.
     END. /* NEXT page */

     /* Search BY column 1 */                          
     ELSE IF LOOKUP(nap,"1,f1") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f1.
       ASSIGN
             lcevtime = "".
       SET f1date 
           lcevtime 
       WITH FRAME f1.
       HIDE FRAME f1 NO-PAUSE.
       IF f1date ENTERED THEN DO: 
          IF lcevtime = "" 
          THEN DO:
              FIND FIRST temp-event NO-LOCK WHERE
                         temp-event.ttdate = f1date NO-ERROR.
              IF NOT AVAILABLE temp-event THEN 
                 FIND LAST temp-event NO-LOCK WHERE
                           temp-event.ttdate >= f1date NO-ERROR.
          END. 

          ELSE DO: 
             FIND LAST temp-event NO-LOCK WHERE 
                       temp-event.ttdate = f1date AND
                       temp-event.tttime >= STRING(lcevtime,"99:99:99")
                NO-ERROR.
             IF NOT AVAILABLE temp-event THEN 
                FIND FIRST temp-event NO-LOCK WHERE 
                           temp-event.ttdate = f1date NO-ERROR.
             IF NOT AVAILABLE temp-event THEN
                FIND LAST temp-event NO-LOCK WHERE
                          temp-event.ttdate >= f1date NO-ERROR.
          END. 

          IF NOT AVAILABLE temp-event THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some eventlog/tablename was found */
          ASSIGN order = 1 memory = recid(temp-event) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-1 */

     /* Search BY col 2 */
     ELSE IF LOOKUP(nap,"2,f2") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:

       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME F2.
       SET UserCode 
           evdate
       WITH FRAME f2.
       HIDE FRAME f2 NO-PAUSE.
       IF UserCode ENTERED THEN DO: 
          FIND FIRST temp-event
             WHERE temp-event.ttuser >= UserCode
             AND (IF evdate entered THEN  
                  temp-event.ttdate = evdate
                  ELSE TRUE)
          NO-LOCK NO-ERROR.
          IF NOT AVAILABLE temp-event THEN DO:
             BELL. MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some eventlog/usercode was found */
          ASSIGN order = 2 memory = recid(temp-event) must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-2 */

     /* Search BY column 3 */
     ELSE IF LOOKUP(nap,"3,f3") > 0 THEN DO ON ENDKEY UNDO, NEXT LOOP:
       cfc = "puyr". RUN Syst/ufcolor.p.
       ehto = 9. RUN Syst/ufkey.p. ufkey = TRUE.
       CLEAR FRAME f3.
       ASSIGN
             lcTable = ""
             lcKey   = "".

       SET lcTable lcKey WITH FRAME f3.
       HIDE FRAME f3 NO-PAUSE.
       IF lcTable ENTERED OR lcKey ENTERED THEN
       DO:
          FIND FIRST temp-event NO-LOCK WHERE 
          temp-event.tttable BEGINS lcTable AND
            (IF lcKey NE "" 
             THEN temp-event.ttkey BEGINS lcKey
             ELSE TRUE) NO-ERROR.
          IF NOT AVAILABLE temp-event THEN DO:
             BELL.
             MESSAGE "NOT FOUND !".
             PAUSE 1 NO-MESSAGE.
             NEXT BROWSE.
          END.
          /* some eventlog/tablename was found */
          ASSIGN 
                order      = 3 
                memory     = recid(temp-event) 
                must-print = TRUE.
          NEXT LOOP.
       END.
     END. /* Search-3 */

     ELSE IF LOOKUP(nap,"5,f5,enter,return") > 0 THEN DO:

        RUN local-find-this (FALSE).
        RUN Mc/eventview.p (temp-event.ttEventLog).

        ufkey = TRUE.
        NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"home,H") > 0 THEN DO:
        RUN local-find-FIRST.
        ASSIGN memory = recid(temp-event) must-print = TRUE.
       NEXT LOOP.
     END.

     ELSE IF LOOKUP(nap,"END,E") > 0 THEN DO : /* LAST record */
        RUN local-find-LAST.
        ASSIGN memory = recid(temp-event) must-print = TRUE.
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
       FIND temp-event WHERE recid(temp-event) = rtab[frame-line(sel)] 
       EXCLUSIVE-LOCK.
    ELSE
       FIND temp-event WHERE recid(temp-event) = rtab[frame-line(sel)] 
       NO-LOCK.
END PROCEDURE.

PROCEDURE local-find-FIRST:
       IF order = 1 THEN FIND FIRST temp-event
       USE-INDEX ttdate NO-LOCK NO-ERROR.
       IF order = 2 THEN FIND FIRST temp-event
       USE-INDEX ttuser NO-LOCK NO-ERROR.
       IF order = 3 THEN FIND FIRST temp-event
       USE-INDEX tttable NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-LAST:
       IF order = 1 THEN FIND LAST temp-event
       USE-INDEX ttdate NO-LOCK NO-ERROR.
       IF order = 2 THEN FIND LAST temp-event
       USE-INDEX ttuser NO-LOCK NO-ERROR.
       IF order = 3 THEN FIND LAST temp-event
       USE-INDEX tttable NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-NEXT:
       IF order = 1 THEN FIND NEXT temp-event
       USE-INDEX ttdate NO-LOCK NO-ERROR.
       IF order = 2 THEN FIND NEXT temp-event
       USE-INDEX ttuser NO-LOCK NO-ERROR.
       IF order = 3 THEN FIND NEXT temp-event
       USE-INDEX tttable NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-find-PREV:
       IF order = 1 THEN FIND PREV temp-event
       USE-INDEX ttdate NO-LOCK NO-ERROR.
       IF order = 2 THEN FIND PREV temp-event
       USE-INDEX ttuser NO-LOCK NO-ERROR.
       IF order = 3 THEN FIND PREV temp-event
       USE-INDEX tttable NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE local-disp-row:
       CLEAR FRAME sel NO-PAUSE.
       DISPLAY 
       temp-event.ttdate
       temp-event.tttime    
       temp-event.ttaction
       temp-event.ttuser
       temp-event.tttable  
       REPLACE(temp-event.ttkey,CHR(255)," / ") ;& temp-event.ttkey
       temp-event.ttType
       WITH FRAME sel.
END PROCEDURE.

