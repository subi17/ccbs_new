/* ----------------------------------------------------------------------
  MODULE .......: MSCHSTAT.P
  TASK .........: UPDATES MobSub.MSStat, only for superusers
  APPLICATION ..: TMS
  AUTHOR .......: tk
  CREATED ......: 30.07.03
  CHANGED ......: 11.09.03 jp Brand 
                  14.04.04 tk local-disp-lis cleaned up
                  30.12.04/aam Secret, RepCode, SaldoLimit from services
-------------------------------------------------------------------------- */

{Syst/commali.i}
{Syst/eventval.i}
{Func/fcustbal.i}
{Mc/lib/tokenlib.i}
{Mc/lib/tokenchk.i 'MobSub'}
{Func/fsubser.i}

IF llDoEvent THEN DO:
   &GLOBAL-DEFINE STAR_EVENT_USER katun

   {lib/eventlog.i}

   DEFINE VARIABLE lhMobsub AS HANDLE NO-UNDO.
   lhMobsub = BUFFER mobsub:HANDLE.
   RUN StarEventInitialize(lhMobsub).

   ON F12 ANYWHERE DO:
      RUN Mc/eventview2.p(lhMobsub).
   END.

END.

def var cli like mobsub.cli no-undo.

def var ac-hdr          as ch no-undo.
def var simdeliv        as lo no-undo.
def var stname          as ch no-undo.
def var unbilledbalance as de no-undo.
def var fatime          as lo no-undo.
def var local-mu-name   as ch no-undo.
def var portingdate     as ch no-undo format "x(33)".
def var prev-mu-seq     as i  no-undo.
def var ufkey           as lo no-undo.
def var ok              as lo no-undo.
DEF VAR def-sp-code     AS C  NO-UNDO.
DEF VAR llSecret        AS LO NO-UNDO.
DEF VAR lcRepCode       AS C  NO-UNDO.
DEF VAR liCreditType    AS I  NO-UNDO.
DEF VAR lcLine        AS CHAR                   NO-UNDO FORMAT "x(75)".
DEF VAR lcBarrStat    AS CHAR                   NO-UNDO FORMAT "x(24)".
DEF VAR lcBarrCmd     AS CHAR                   NO-UNDO.
DEF VAR llAllowed     AS LOGICAL                NO-UNDO.
DEF VAR lcPCLB        AS CHAR                   NO-UNDO.
DEF VAR lcDSSInfo     AS CHAR                   NO-UNDO.
DEF VAR lrCLBRec      AS RECID                  NO-UNDO.
DEF VAR lcMNP         AS CHARACTER NO-UNDO.



{Mm/mobsub1.i}

DEF BUFFER AgrCustomer FOR Customer.
DEF BUFFER InvCustomer FOR Customer.
DEF BUFFER UserCustomer FOR Customer.
{mobsub.frm}

form 
SKIP(3)
"       This program lets you change status of a mobile subscription " SKIP
SKIP(2)
"                      MSISDN ....:" cli SKIP(10)
with frame askcli no-labels width 80
title " CHANGE MOBSUB STATUS ".

ac-hdr = "CHANGE".

loop:
repeat:

   assign ufkey = true ehto = 9.
   RUN Syst/ufkey.

   update cli with frame askcli.

   if cli = "" then leave.

   find mobsub where mobsub.cli = cli no-error.
   if not avail mobsub then do:
      message "Unknown Subscription !" view-as alert-box error.
      next loop.
   end.
   run local-disp-lis.

   upd:
   repeat with frame lis trans:
      ASSIGN ufkey = TRUE ufk = 0 ehto = 1
      ufk[1] = 7
      ufk[8] = 8.
      RUN Syst/ufkey.

      if toimi = 8 then do:
         next loop.
      end.

      if toimi = 1 then do:
         assign ufkey = true ehto = 9.
         RUN Syst/ufkey.

         if llDoEvent THEN RUN StarEventSetOldBuffer(lhMobSub).
         update mobsub.msstat with frame lis.
         if llDoEvent THEN RUN StarEventMakeModifyEvent(lhMobSub).

         run local-disp-lis.
      end.

   end.

end.




PROCEDURE local-disp-lis:
      RUN local-find-others(false).
      
      ASSIGN llSecret     = fSecretValue(MobSub.MsSeq)
             lcRepCode    = STRING(fCallSpecReport(MobSub.MsSeq))
             liCreditType = fCreditTypeValue(MobSub.MsSeq,
                                             OUTPUT liSaldoLimit).
      
      /* special method ... */
      CLEAR FRAME lis NO-PAUSE.

      DISP
      MobSub.CustNum 
      Customer.CustName   WHEN AVAIL Customer 
      IMSI.PUK1           WHEN AVAIL imsi
      IMSI.PUK2           WHEN AVAIL imsi 
      IMSI.PIN2           WHEN AVAIL imsi
      IMSI.PIN1           WHEN AVAIL imsi
      IMSI.IMSI           WHEN AVAIL imsi 
      Mobsub.CliType
      CliType.cliname     WHEN AVAIL clitype


      Mobsub.BillTarget
      MobSub.ICC        
      SIM.Stock           WHEN AVAIL sim
      Stock.StoName       WHEN AVAIL stock
      MobSub.Reseller               
      MobSub.ActivationDate
      Reseller.RsName     WHEN AVAIL reseller
      MobSub.SalesMan
      Salesman.SmName     WHEN AVAIL salesman
      MobSub.MsStatus    FORMAT "99"
      stname
      MobSub.CLI
      lcRepCode
      liSaldoLimit 
      fatime
      llSecret
      liCreditType
      Customer.Salesman   WHEN AVAIL Customer  AND NEW mobsub @ MobSub.SalesMan
      Customer.Reseller   WHEN AVAIL Customer  AND NEW mobsub @ MobSub.Reseller 
      unbilledbalance
      portingdate

      WITH FRAME lis.

END PROCEDURE.

PROCEDURE local-find-others.

      DEF input parameter llsel AS LOG NO-UNDO.

      FIND Customer  OF mobsub  NO-LOCK NO-ERROR.
      FIND FIRST BillTarg  OF mobsub NO-LOCK NO-ERROR.

      FIND msisdn    WHERE MSISDN.CLI =  MobSub.CLI       NO-LOCK NO-ERROR.

      IF AVAIL msisdn AND msisdn.portingDate ne ? then DO:
         PortingDate = "Inporting Time " +  
            STRING(MSISDN.PortingTime,"99.99") + " on " + 
            String(msisdn.portingDate,"99-99-99").
      END.
      ELSE PortingDate = "".

      IF llsel = FALSE THEN DO:
         FIND killms   WHERE KillMs.CLI  = MobSub.CLI AND
                             KillMs.MsSeq = MobSub.MsSeq NO-LOCK NO-ERROR. 

         Fatime = can-find(FIRST fatime WHERE 
                                 fatime.cli    = mobsub.cli AND
                                 Fatime.MsSeq  = MobSub.MsSeq AND
                                 fatime.invnum = 0).
         FIND msstat    OF msisdn                            NO-LOCK NO-ERROR.
         FIND msclass   WHERE 
              MSClass.Brand = gcBrand AND
              MSClass.McCode  = MSISDN.McCode NO-LOCK NO-ERROR.
         FIND reseller    WHERE 
              ReSeller.Brand    = gcBrand   AND 
              Reseller.Reseller = MobSub.Reseller  
         NO-LOCK NO-ERROR.
         FIND salesman  WHERE 
              Salesman.Brand    = gcBrand AND 
              Salesman.Salesman = MobSub.SalesMan  
         NO-LOCK NO-ERROR.
         FIND sim       WHERE 
              SIM.Brand       = gcBrand AND 
              SIM.ICC         = MobSub.ICC     
         NO-LOCK NO-ERROR.
         FIND stock     WHERE 
              Stock.Brand     = gcBrand  AND 
              Stock.Stock     = SIM.Stock     NO-LOCK NO-ERROR.
         FIND mobcount  WHERE MobCount.MsSeq  = MobSub.MsSeq  NO-LOCK NO-ERROR.
         FIND CliType   WHERE 
              CliType.Brand   = gcBrand   AND 
              CliType.CliType = MobSub.CliType 
         NO-LOCK NO-ERROR.

         stname = entry(MobSub.MsStatus + 1, stnames).

         IF MobSub.SimDelStatus = 3 OR
            MobSub.SimDelStatus = 0 THEN simdeliv = TRUE. 
         ELSE simdeliv =  FALSE.  

         FIND imsi      WHERE IMSI.IMSI   = MobSub.IMSI     NO-LOCK NO-ERROR.
      END.
END PROCEDURE.


