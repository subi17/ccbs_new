{Syst/commpaa.i}
katun = "vikasagr".
gcbrand = "1".
{Func/timestamp.i}

define variable ldenow    as decimal no-undo.
define variable llheader  as logical no-undo init true.

define buffer b_msisdn    for msisdn.

define stream slog.
define stream sreport.
output stream slog to "/apps/yoigo/tms_support/201102/yot_1186.log".
output stream sreport to "/apps/yoigo/tms_support/201102/yot_1186.xls".

ldenow = fmakets().

each_loop:
for each msisdnnumber where
         msisdnnumber.cli >= "633010000" and
         msisdnnumber.cli <= "633099999" no-lock,
    first msisdn where
          msisdn.brand = gcbrand and
          msisdn.cli   = msisdnnumber.cli and
          msisdn.validto > ldenow no-lock:

    /* check that no duplicate timestamps exists */
    if can-find(first b_msisdn where
                      b_msisdn.cli = msisdn.cli and
                      b_msisdn.validto > ldenow and
                      rowid(b_msisdn) <> rowid(msisdn) no-lock) then do:
        put stream slog unformatted msisdn.cli "|error:duplicate timestamp found" skip.
        next each_loop.
    end. /* if can-find(first b_msisdn where */

    if msisdn.brand <> "1" then do:
        put stream slog unformatted msisdn.cli "error:wrong brand" skip.
        next each_loop.
    end. /* if msisdn.brand <> "1" then do: */

    /* check that no active subscription exists */
    if can-find(first mobsub where
                      mobsub.cli = msisdn.cli no-lock) then do:
        put stream slog unformatted msisdn.cli "|error:active subscription found" skip.
        next each_loop.
    end. /* if can-find(first mobsub where */

    /* Report file header */
    if llheader then do:
        put stream sreport unformatted "MSISDN"    CHR(9)
                                       "Status"    CHR(9)
                                       "Stock"     CHR(9)
                                       "Rank"      skip.
        llheader = false.
    end. /* if llheader then do: */

    put stream sreport unformatted msisdn.cli         CHR(9)
                                   msisdn.StatusCode  CHR(9)
                                   msisdn.pos         CHR(9)
                                   msisdnnumber.rank  skip.

end. /* for each msisdnnumber where */

output stream slog close.
output stream sreport close.