&GLOBAL-DEFINE BrandVarDefined Yes
{Syst/commpaa.i}
gcBrand = "1".
{Func/timestamp.i}
{Func/func.p}
{Syst/tmsconst.i}
{Func/cparam2.i}

def stream slog.

def var ldainvdate as date no-undo.
def var lclogfile  as char no-undo.
def var lcclitype  as char no-undo.
def var liqty      as int  no-undo.
def var licustqty  as int  no-undo.
def var ldfromper  as dec  no-undo.
def var ldtoper    as dec  no-undo.
def var i          as int  no-undo.
def var j          as int  no-undo.
def var liInvType  as int  no-undo.

DEFINE VARIABLE lcFLATContracts   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcIPLContracts    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCONTSContracts  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCONTSFContracts  AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcFlatCont        AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcBundleBasedTypes AS CHARACTER NO-UNDO.

ASSIGN lcIPLContracts   = fCParamC("IPL_CONTRACTS")
       lcFLATContracts  = fCParamC("FLAT_CONTRACTS")
       lcCONTSContracts = fCParamC("CONTS_CONTRACTS")
       lcCONTSFContracts = fCParamC("CONTSF_CONTRACTS")
       lcBundleBasedTypes = fCParamC("BUNDLE_BASED_CLITYPES").

lcFlatCont = subst("&1,&2,&3,&4",
             lcIPLContracts,
             lcFLATContracts,
             lcCONTSContracts,
             lcCONTSFContracts).

def temp-table ttcli no-undo
   field msseq as int
   field clitype as char
   field cli as char
   field invnum as int
   field amt as dec
   index amt clitype amt desc.

def temp-table ttcust no-undo
   field custnum as int
   field extinvid as char
   field amt as dec
   index amt amt desc.

assign
   lclogfile = "/tmp/find_biggest_invoices_result.txt"
   liqty     = 5
   liInvType = 1
   ldainvdate = DATE(MONTH(TODAY),1,YEAR(TODAY)).

pause 0.
update ldainvdate label "Invoice Date" colon 15 format "99-99-99"
       liInvType  validate(input liInvType eq 1 or input liInvType eq 99,
                           "Allowed values: 1 (normal), 99 (test)")
                  label "Invoice Type" colon 15 format ">9"
       lcclitype  label "Subscr. Type" colon 15 format "x(16)"
          help "EMPTY = all"
       liqty      label "Pick"         colon 15 format ">>>>>>9"
          help "Qty per subscription type" skip(1)
       licustqty  label "Customers"    colon 15 format ">>>>>>9"
          help "Qty of biggest customers" skip(1)
       lclogfile  label "Result File"  colon 15 format "x(50)"
       go-on("f4")
with overlay side-labels row 9 centered title " FIND BIGGEST INVOICES "
     frame ffind.

hide frame ffind no-pause.

if ldainvdate = ? or lclogfile = "" or
   lookup(keylabel(last-key),"4,f4") > 0 then return.

do i = 1 to num-entries(lcBundleBasedTypes):
   lcclitype = replace(lcclitype,entry(i,lcBundleBasedTypes) + "/","").
end.
i = 0.

if lcclitype > "" then do:
   FIND FIRST clitype where
              clitype.brand = gcbrand and
              clitype.clitype = lcclitype NO-LOCK no-error.
   IF NOT AVAIL clitype then do:
      MESSAGE "Unknown Subscr. Type" lcclitype VIEW-AS ALERT-BOX.
      return.
   end.
end.

output stream slog to value(lclogfile).

for each invoice no-lock use-index invType where
         invoice.brand   = "1" and
         invoice.invdate = ldainvdate and
         invoice.invtype = liInvType:

    i = i + 1.
/*    if i > 10000 then leave. */

    if i mod 100 = 0 then do:
       pause 0.
       disp i label "Browsing"
       with overlay row 10 centered frame fqty.
    end.

    assign
       ldfromper = fmake2dt(invoice.fromdate,1)
       ldtoper   = fmake2dt(invoice.todate,86399).

    if licustqty > 0 then do:
       create ttcust.
       assign
          ttcust.custnum = invoice.custnum
          ttcust.extinvid = invoice.extinvid
          ttcust.amt     = invoice.invamt.
    end.

    for each subinvoice of invoice no-lock:

       find first msowner where
                  msowner.cli = subinvoice.cli and
                  msowner.invcust = invoice.custnum and
                  msowner.tsend > ldfromper and
                  msowner.tsbeg < ldfromper and
                  msowner.paytype = false no-lock no-error.
       if not available msowner then
       find first msowner where
                  msowner.cli = subinvoice.cli and
                  msowner.invcust = invoice.custnum and
                  msowner.tsend > ldfromper and
                  msowner.tsbeg < ldtoper and
                  msowner.paytype = false no-lock no-error.
       if not available msowner then
       find first msowner where
                  msowner.cli = subinvoice.cli and
                  msowner.invcust = invoice.custnum and
                  msowner.tsbeg <= ldtoper and
                  msowner.paytype = false no-lock no-error.
       if not available msowner then next.

       if lcclitype > "" and
          msowner.clitype ne lcclitype and
          msowner.tariffbundle ne lcclitype then next.

       create ttcli.
       assign
          ttcli.msseq = subinvoice.msseq
          ttcli.cli = subinvoice.cli
          ttcli.clitype = msowner.clitype + (if msowner.tariffbundle > "" then
                                           "/" + msowner.tariffbundle else "")
          ttcli.invnum = invoice.invnum
          ttcli.amt  = subinvoice.amt.
    end.
end.

put stream slog unformatted
   "Invoice"  chr(9)
   "Inv.Date" chr(9)
   "Cust"     chr(9)
   "MSISDN"   chr(9)
   "Subs.ID"  chr(9)
   "Subscr. Type" chr(9)
   "Amount"   skip.

for each ttcli
break by ttcli.clitype
      by ttcli.amt desc:

   if first-of(ttcli.clitype) then j = 0.

   j = j + 1.

   if j > liqty then next.

   find first invoice no-lock where
        invoice.invnum = ttcli.invnum.

   put stream slog unformatted
      invoice.extinvid chr(9)
      invoice.invdate  chr(9)
      invoice.custnum  chr(9)
      ttcli.cli        chr(9)
      ttcli.msseq      chr(9)
      ttcli.clitype    chr(9)
      trim(string(ttcli.amt,"->>>>>>>9.99")) skip.

end.

if licustqty > 0 then
put stream slog unformatted
   skip(1)
   "BIGGEST CUSTOMERS" skip
   "Customer"  chr(9)
   "Name"      chr(9)
   "Invoice"   chr(9)
   "Invoice Amount" skip.

j = 0.
for each ttcust use-index amt,
   first customer no-lock where
         customer.custnum = ttcust.custnum:

   j = j + 1.
   if j > licustqty then leave.

   put stream slog unformatted
      ttcust.custnum  chr(9)
      fDispCustName(BUFFER Customer) chr(9)
      ttcust.extinvid chr(9)
      trim(string(ttcust.amt,"->>>>>>>9.99")) skip.
end.

hide frame fqty no-pause.

output stream slog close.
