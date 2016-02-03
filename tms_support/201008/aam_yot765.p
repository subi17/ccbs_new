{Syst/commpaa.i}
gcBrand = "1".
katun = "Qvantel".
{Func/timestamp.i}

&GLOBAL-DEFINE STAR_EVENT_USER katun

{lib/eventlog.i}

def var lccli as char no-undo.
def var i as int no-undo.
def var ldaevent as date no-undo.
def var ldatarj3 as date no-undo.
def var litime as int no-undo.

def buffer bowner for msowner.
def buffer bcust for customer.

DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
lhCustomer = BUFFER bCust:HANDLE.
RUN StarEventInitialize(lhCustomer).


for each customer no-lock where
    customer.custnum > 10000 and 
    customer.deltype = 0:

   ldaevent = ?.
   for first eventlog no-lock where
             eventlog.tablename = "customer" and
             eventlog.key = string(customer.custnum) and
             eventlog.action = "create" and
             eventlog.usercode = "request":
      ldaevent = eventlog.eventdate.       
   end.

   ldatarj3 = ?.
   for each msowner no-lock where
            msowner.agrcust = customer.custnum,
       each bowner no-lock where
            bowner.msseq = msowner.msseq and
            bowner.clitype = "tarj3"
   by bowner.tsend desc:
      fsplitts(bowner.tsend,
               output ldatarj3,
               output litime).
      leave.         
   end.
  
   find first bcust where recid(bcust) = recid(customer) exclusive-lock.
   RUN StarEventSetOldBuffer (lhCustomer).
   bcust.deltype = 1.
   RUN StarEventMakeModifyEvent (lhCustomer).
   
   i = i + 1.
   
   pause 0.
   disp i customer.custnum 
        ldaevent format "99-99-99"
        ldatarj3 format "99-99-99".
end.
