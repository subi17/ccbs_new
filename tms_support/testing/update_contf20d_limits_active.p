{Syst/commpaa.i}
gcbrand = "1".
katun = "Qvantel".
{Func/timestamp.i}
{Func/fdss.i}

def var licount as int no-undo.
def var llAvail as log no-undo.
def var lldss   as log no-undo.
def var ldetotallimit as dec no-undo.

output to "/apps/yoigo/tms_support/testing/update_contf20d_limits_active.txt".

for each mobsub where
         mobsub.brand = "1" and
         mobsub.clitype = "contf" no-lock:

    if licount >= 2 then leave.

    assign llAvail = true
           lldss   = false.

    if fIsDSSActive(mobsub.custnum,fmakets()) then
       lldss = true.

    for each mservicelimit where
             mservicelimit.msseq  = mobsub.msseq and
            (mservicelimit.slseq = 42 or mservicelimit.slseq = 43) and
             mservicelimit.endts >= 99999999.99999 exclusive-lock:

       if llAvail then do:
          put unformatted mobsub.cli + "|" + string(mobsub.msseq) + "|".
          llAvail = false.
       end.

       put unformatted string(mservicelimit.dialtype) + "|" +
                       string(mservicelimit.inclamt)  + "|".

       if mservicelimit.dialtype = 4 then
          mservicelimit.inclamt = 300.
       else if mservicelimit.dialtype = 7 then do:
          mservicelimit.inclamt = 1024.
          if lldss then
             RUN pUpdateDSSLimit(mobsub.custnum,"update",874,0,
                                 fmakets(),output ldetotallimit).
       end.

       put unformatted string(mservicelimit.inclamt) + "|".
    end.

    if llAvail = false then do:
       licount = licount + 1.
       if lldss then
          put unformatted string(lldss) + "|" + string(ldetotallimit) skip.
       else put unformatted string(lldss) skip.
    end.
end.

output close.
