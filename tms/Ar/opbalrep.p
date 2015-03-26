/* ------------------------------------------------------
  MODULE .......: OPBALREP
  FUNCTION .....: List overpayment balance data
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 06.03.03
  MODIFIED .....: 12.09.03/aam brand
                  02.01.06/aam values from TMSCodes  
                  24.01.06/DYNAMIC-FUNCTION("fDispCustName"
  VERSION ......: M15
  ------------------------------------------------------ */

{commali.i}
{fcustbal.i}

/* print-linemuuttujat */
{utumaa.i}

DEF INPUT  PARAMETER idtDate     AS DATE  NO-UNDO.
DEF OUTPUT PARAMETER oiCount     AS INT   NO-UNDO.

DEF VAR viiva1 AS CHAR FORMAT "x(80)" NO-UNDO.
DEF VAR viiva2 LIKE viiva1.
DEF VAR viiva3 LIKE viiva1.
DEF VAR viiva4 LIKE viiva1.
DEF VAR jar    AS CHAR FORMAT "x(24)" NO-UNDO.
DEF VAR order  AS INT                 NO-UNDO.
DEF VAR sl     AS INT                 NO-UNDO.
DEF VAR rl     AS INT                 NO-UNDO.
DEF VAR rlx    AS INT                 NO-UNDO.
DEF VAR lev    AS INT  INIT 78        NO-UNDO.
DEF VAR otsi   AS CHAR EXTENT 39      NO-UNDO.

DEF VAR ldAmt   AS DEC                NO-UNDO.
DEF VAR lcType  AS CHAR               NO-UNDO.
DEF VAR liOrd   AS INT                NO-UNDO. 
DEF VAR ldtDate AS DATE               NO-UNDO. 
DEF VAR ldOpAmt AS DEC                NO-UNDO. 
DEF VAR lcCustName AS CHAR            NO-UNDO.

DEF TEMP-TABLE ttBal NO-UNDO
   FIELD OPLog AS INT
   FIELD Order   AS INT
   INDEX Order Order.

ASSIGN 
    viiva1 = FILL("=",lev)
    viiva2 = FILL("=",lev)
    viiva3 = FILL("-",lev)
    viiva4 = FILL("-",lev).


FORM HEADER
   viiva1 AT 1 SKIP
   ynimi at 1 FORMAT "x(30)" 
      "OVERPAYMENT BALANCES" AT 35
      "Page" AT 68             
      sl FORMAT "ZZZZ9" SKIP
   "Posted before " + STRING(idtDate,"99.99.9999") AT 35 FORMAT "X(30)"
      pvm FORMAT "99.99.9999" AT 68 SKIP
   viiva2 AT 1 SKIP
   "Customer"    AT 1
      "Name"        AT 10
      "OverPayment" TO 55 SKIP
   viiva3 AT 1 SKIP
   "Posted" AT 6
      "Invoice" TO 22
      "Voucher" TO 31
      "Amount"  TO 45
      "Type"    AT 47
      SKIP
   viiva4 AT 1 SKIP
   WITH WIDTH 85 NO-LABEL NO-BOX FRAME sivuotsi.


FUNCTION fCheckPage RETURNS LOGIC 
    (iAddLine AS INT).

    IF rl + iAddLine >= skayt1 THEN DO:
        {uprfeed.i rl}
        ASSIGN rlx = 0
               sl = sl + 1.
        view STREAM tul FRAME sivuotsi.  
        ASSIGN rl = 8.
    END.

    RETURN TRUE.
END.

ASSIGN sl = 1. 
VIEW STREAM tul FRAME sivuotsi.
ASSIGN rl = 8.

CustCollect:
FOR EACH Customer WHERE
         Customer.Brand = gcBrand USE-INDEX CustNum NO-LOCK:
    lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                                   BUFFER Customer).
                                               
    ldOPAmt = fGetCustBal(Customer.CustNum,"TOTAL","OP").
    IF ldOpAmt = 0 THEN NEXT. 

    /* first go through events and leave out unnecessary ones (events
       that sum up as zero) */

    ASSIGN ldAmt   = 0
           liOrd   = 0
           ldtDate = ?.
    EMPTY TEMP-TABLE ttBal.

    FOR EACH OPLog NO-LOCK WHERE
             OPLog.CustNum = Customer.CustNum
    BY OPLog.EventDate
    BY OPLog.Amt DESC:

       ldAmt = ldAmt + OPLog.Amt.

       IF ldAmt = 0 THEN DO:
          EMPTY TEMP-TABLE ttBal.
          ldtDate = ?.
       END.

       ELSE DO:
          CREATE ttBal.
          ASSIGN ttBal.OPLog = RECID(OPLog)
                 liOrd         = liOrd + 1
                 ttBal.Order   = liOrd.

          IF ldtDate = ? THEN ldtDate = OPLog.EventDate.

       END.

    END.

    /* first event is later than given date */
    IF ldtDate > idtDate THEN NEXT CustCollect.

    oiCount = oiCount + 1.

    fCheckPage(2).

    PUT STREAM tul 
       Customer.CustNum     AT 1  FORMAT ">>>>>>>9" 
       lcCustName           AT 10 FORMAT "X(30)"
       ldOpAmt              TO 55 FORMAT "->>>>>>>9.99" 
       SKIP.
    rl = rl + 1.

    FOR EACH ttBal,
    FIRST OPLog NO-LOCK WHERE
          RECID(OPLog) = ttBal.OPLog:

       lcType = DYNAMIC-FUNCTION("fTMSCodeName" IN ghFunc1,
                                 "OpLog",
                                 "EventType",
                                 STRING(OPLog.EventType)).

       fCheckPage(0).

       PUT STREAM tul 
          OPLog.EventDate AT 6  FORMAT "99-99-99"
          OPLog.InvNum    TO 22 FORMAT ">>>>>>>9"
          OPLog.Voucher   TO 31 FORMAT ">>>>>>>9"
          OPLog.Amt       TO 45 FORMAT "->>>>>>>>9.99"
          lcType          AT 47 FORMAT "X(32)"
          SKIP.
      rl = rl + 1.

    END. 

    PUT STREAM tul SKIP(1).
    rl = rl + 1.

END.

{uprfeed.i rl}


