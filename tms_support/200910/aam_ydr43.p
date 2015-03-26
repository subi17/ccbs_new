{testpaa.i}
katun = "ari".

def var limonth     as int  no-undo.
def var licnt       as int  no-undo.
def var llinterrupt as log  no-undo.
def var lcdir       as char no-undo.

lcdir = "/store/riftp/ifs/outgoing/monthly/".

def stream slog.
output stream slog to /apps/snet/200910/aam_ydr43.log append.

put stream slog unformatted
   "Service invoices" skip.
   
/* service invoices */
do limonth = 200901 to 200907:

   pause 0.
   disp limonth format "999999" with 1 down.
   
   run /home/ari/work/ifs_invoice_monthly.p(32, 
                                            lcdir + 
                                            "INVOICES_SERVICE_" + 
                                               STRING(liMonth) + ".DAT",
                                            "Full",
                                            0,
                                            "",
                                            "",
                                            limonth,
                                            output licnt,
                                            output llinterrupt).
   put stream slog unformatted
      string(limonth,"999999") "|"
      licnt skip.
end.

put stream slog unformatted
   skip(1)
   "Sales invoices" skip.
 
/* sales invoices */
do limonth = 200901 to 200909:

   pause 0.
   disp limonth format "999999" with 1 down.
   
   run /home/ari/work/ifs_invoice_monthly.p(35, 
                                            lcdir + 
                                            "INVOICES_SALES_" + 
                                               STRING(liMonth) + ".DAT",
                                            "Full",
                                            0,
                                            "",
                                            "",
                                            limonth,
                                            output licnt,
                                            output llinterrupt).
   put stream slog unformatted
      string(limonth,"999999") "|"
      licnt skip.

end.


