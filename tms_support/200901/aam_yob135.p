{Syst/testpaa.i}
{Func/func.i}

&GLOBAL-DEFINE STAR_EVENT_USER katun
{lib/eventlog.i}
DEFINE VARIABLE lhCustomer AS HANDLE NO-UNDO.
lhCustomer = BUFFER Customer:HANDLE.
RUN StarEventInitialize(lhCustomer).


def stream sread.
input stream sread from /apps/snet/200901/aam_yob135_cust.txt.

def stream slog.
output stream slog to /apps/snet/200901/aam_yob135.log append.

def var licust as int  no-undo.
def var lcbank as char no-undo.
def var lcname as char no-undo.
def var lcbname as char no-undo.
def var libcust as int no-undo.

def buffer bcust for customer.


repeat:
    import stream sread unformatted licust.

    find customer where customer.custnum = licust no-lock no-error.
    if not available customer then next.

    lcname = fdispcustname(buffer customer).
    
    disp 
       customer.custnum 
       lcname format "x(30)"
       customer.bankacc format "x(20)".                
    
    lcbank = "".
    lcbname = "".
    libcust = 0.
    
    for each bcust no-lock where
             bcust.brand = "1" and
             bcust.orgid = customer.orgid and
             bcust.roles = "inactive":
             
       if bcust.bankacc = "" then next.
       
       lcbank = lcbank + (if lcbank > "" then "," else "") + 
                bcust.bankacc.
                
       lcbname = fdispcustname(buffer bcust).         
       libcust = bcust.custnum.
    end.

    disp skip
         lcbank format "x(20)"
         lcbname format "x(30)"
         skip(1).

    if customer.bankacc = "" and lcbank > "" then do:
    
       find current customer exclusive-lock. 
       RUN StarEventSetOldBuffer (lhCustomer).
       customer.bankacc = lcbank.
       RUN StarEventMakeModifyEvent (lhCustomer).
       
       put stream slog unformatted
          customer.custnum chr(9)
          customer.bankacc chr(9)
          libcust skip.
          
    end.
end.

input stream sread close.
output stream slog close.

fcleaneventobjects().

