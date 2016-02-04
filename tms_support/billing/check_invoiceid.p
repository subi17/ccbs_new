{Syst/testpaa.i}
katun = "snet".

def var ldtInvDate as date no-undo.
def var liInvType  as int  no-undo init 1.
def var liaction   as int  no-undo.
def var lldone     as log  no-undo.
def var lcresult   as char no-undo.
def var lidone     as int  no-undo.

pause 0.
update 
ldtInvDate 
   format "99-99-99"
   label "Invoice Date"
   help "Invoice date that is checked" skip
liaction
   format "9"
   label "Action"
   help "0=just check, 1=check and renumber,2=set numbers (first time)"
   validate(input liaction <= 2,"Valid values are 0-2")
with overlay side-labels row 10 centered title " CHECK IDS " frame fcheck.
   
hide frame fcheck no-pause.

if ldtInvDate = ? then return.

if liaction = 1 then do:
   lldone = false.
   message "You have chosen to renumber invoices. Continue?"
   view-as alert-box question
   buttons yes-no
   set lldone.
   if not lldone then return.
end.

   
RUN Inv/invoice_extinvid.p(ldtInvDate,
                       liInvType,
                       liaction,
                       0,
                       0,
                       OUTPUT lidone).

lcresult = return-value.
if lcresult = "" then lcresult = "OK".

message "Result:" lcresult skip
        "(Re)numbered:" lidone 
view-as alert-box title " DONE ".

