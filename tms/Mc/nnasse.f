/* nnasse.f

   changed:       26.09.02/aam customer balances in table CustBal,
                               PriceList and RateCust removed 
                  07.03.03/aam customer.balance[2] -> CreditLimit

*/


  Customer.CustNum     label "Customer nr."
  Customer.StartCharge    label "St.ch."               AT 24
  Customer.VolDisc   format "A/P" label "VDisc."  AT 34
  Customer.OrgId     label "Com/soc. nr.."        AT 48  
  help "Company / social security number"    SKIP
  Customer.SearchName    label "Search cons." Customer.Size label "Size" AT 24
  Customer.ConnType    label "Conn."  format "Dir/Indir"
  Customer.Category    label "Category ...." AT 48 dkatnimi NO-LABEL SKIP
  Customer.CustName    label "Name ......."
  ldCustAP             label "Advance paym."         AT 48
    FORMAT "z,zzz,zz9-"
    help "Customer's advance payment"                SKIP

  Customer.COName     label " - 2. row .."
  help "Street address"
  Customer.CreditLimit label "Credit Limit "         AT 48
  help "Customer CreditInvNum limitation"                            SKIP


  Customer.Contact   label "Contactpers."
  tot1               label "* Tot Limit...." at 46                SKIP

  Customer.Phone     label "Tel ........"
  kaytalv            label "Spend ......." at 48                SKIP

  Customer.Fax     label "Fax ........"
  ldCustBal  FORMAT "z,zzz,zz9-"
                   label "  Ledg. balance" at 46              SKIP
  Customer.Address     label "Address ...."
  ldCustInt  FORMAT "z,zzz,zz9-"
                   label "  Interest debt" at 46              SKIP
  Customer.ZipCode label "Postal addr."
  Customer.PostOffice     format "x(21)" NO-LABEL
  tot2 label "* Outstanding ." at 46              SKIP
  Customer.Country     label "Country ...."
  Customer.PaymTerm   label "Paymemt terms" at 48 "days"         SKIP
  Customer.Email   label "Email ......"                           SKIP
  Customer.InvGroup    label "Invoice gr.." igname NO-LABEL AT 24
  Customer.Salesman    label "Salesman" AT 48 mynimi NO-LABEL  SKIP
  Customer.Reseller    label "Reseller" AT 48 rsname NO-LABEL  SKIP
  Customer.InvCust    label "Inv. custnr." aslanimi NO-LABEL AT 24
  Customer.Language    label "Language" AT 48                     SKIP
  Customer.ContrBeg    label "Contract started" AT 48 SKIP
  Customer.RepCust   label "Rep. custnr." asranimi NO-LABEL AT 24
  Customer.ContrEnd    label "Contract ended ." AT 48 SKIP
  Customer.PaymCust    label "Paym. resp.." asrenimi NO-LABEL AT 24
  Customer.RepCodes   label "Report codes ..." format "x(8)" AT 48 SKIP 
