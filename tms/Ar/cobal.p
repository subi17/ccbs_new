/* ------------------------------------------------------
  MODULE .......: cobal.p
  FUNCTION .....: update balances from commission events
  APPLICATION ..: TMS
  AUTHOR .......: aam
  CREATED ......: 20.02.04
  MODIFIED .....: 19.04.04/aam create fat instead of customer balance
                  30.08.04/aam brand
                  14.09.04/aam fCopyDefFatime()
                  29.11.04/aam new parameter to fcpfat-functions
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{cparam2.i}
{timestamp.i}
{eventval.i}
{fapvat.i}
{fcpfat.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun
   {lib/eventlog.i}

   DEFINE VARIABLE lhFatime AS HANDLE NO-UNDO.
   lhFatime = BUFFER Fatime:HANDLE.
   RUN StarEventInitialize(lhFatime).

END.


DEF INPUT  PARAMETER iiCoRule    AS INT  NO-UNDO. 
DEF INPUT  PARAMETER icReseller1 AS CHAR NO-UNDO. 
DEF INPUT  PARAMETER icReseller2 AS CHAR NO-UNDO.
DEF INPUT  PARAMETER idtDate1    AS DATE NO-UNDO.
DEF INPUT  PARAMETER idtDate2    AS DATE NO-UNDO.
DEF OUTPUT PARAMETER oiCount     AS INT  NO-UNDO.

DEF BUFFER bEvent FOR CoEvent.

DEF VAR liCount    AS INT  NO-UNDO. 
DEF VAR liCustNum  AS INT  NO-UNDO. 
DEF VAR liFromPer  AS INT  NO-UNDO.
DEF VAR liToPer    AS INT  NO-UNDO.
DEF VAR liMsSeq    AS INT  NO-UNDO.
DEF VAR lcCLI      AS CHAR NO-UNDO. 
DEF VAR ldAmtLimit AS DEC  NO-UNDO.

liFromPer = YEAR(TODAY) * 100 + MONTH(TODAY).

/* unpaid commission events */
FOR EACH CoEvent NO-LOCK WHERE
         CoEvent.Brand     = gcBrand  AND
         CoEvent.CalcDate >= idtDate1 AND
         CoEvent.CalcDate <= idtDate2 AND
         CoEvent.PaymDate  = ?,
   FIRST Salesman NO-LOCK WHERE
         Salesman.Brand     = CoEvent.Brand    AND
         Salesman.Salesman  = CoEvent.Salesman AND
         Salesman.Reseller >= icReseller1      AND
         Salesman.Reseller <= icReseller2:

   IF iiCoRule > 0 AND CoEvent.CoRuleID NE iiCoRule THEN NEXT.

   /* type of "fat" */ 
   FIND CoRule OF CoEvent NO-LOCK NO-ERROR.
   IF NOT AVAILABLE CoRule OR
      CoRule.FtGrp = "" 
   THEN NEXT.

   FIND FatGroup WHERE 
        FatGroup.Brand = CoEvent.Brand AND
        FatGroup.FtGrp = CoRule.FtGrp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE FatGroup THEN NEXT. 

   liCustNum = CoEvent.CustNum.
         
   IF liCustNum = 0 THEN liCustNum = INTEGER(Salesman.CustNum).
          
   IF liCustNum = 0 THEN NEXT. 
   
   FIND Customer WHERE Customer.CustNum = liCustNum NO-LOCK NO-ERROR.
   IF NOT AVAILABLE Customer THEN NEXT.

   ASSIGN liMsSeq = 0
          lcCLI   = ""
          liToPer = 205012.

   /* has target cli been defined */
   IF CoEvent.CoTargId > 0 THEN
   FOR FIRST CoTarg NO-LOCK WHERE
             CoTarg.Brand    = CoEvent.Brand    AND
             CoTarg.CoRuleID = CoEvent.CoRuleId AND
             CoTarg.CoTargID = CoEvent.CoTargID AND
             CoTarg.TargType = "M",
       FIRST MsOwner NO-LOCK WHERE
             MsOwner.MsSeq   = INTEGER(CoTarg.CoTarg) AND
             MsOwner.CustNum = liCustNum:
             
      ASSIGN liMsSeq = MsOwner.MsSeq
             lcCLI   = MsOwner.CLI.
   END. 

   /* if default rows have been defined to fatgroup then copy them */
   IF NOT fCopyDefFatime(FatGroup.FtGrp,
                         Customer.CustNum,
                         liMSSeq,
                         lcCLI,
                         CoEvent.CommAmt,
                         liFromPer,
                         liToPer,
                         CoEvent.HostKey)
   THEN DO:

      /* otherwise create one row with default values */
      IF NOT fCreateFatime(FatGroup.FtGrp,
                           Customer.CustNum,
                           liMsSeq,
                           lcCli,
                           CoEvent.CommAmt,
                           liFromPer,
                           CoEvent.HostKey)
      THEN NEXT. 
   END.
   
   /* mark event as paid */
   FIND bEvent WHERE RECID(bEvent) = RECID(CoEvent) EXCLUSIVE-LOCK.
   ASSIGN bEvent.PaymDate = TODAY
          oiCount         = oiCount + 1. 
   
END.   


