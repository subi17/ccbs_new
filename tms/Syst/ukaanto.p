/* ------------------------------------------------------
  MODULE .......: ukaanto.p
  FUNCTION .....: Compile all found p modules found in the ppath variable
  APPLICATION ..: TS
  AUTHOR .......: PT
  CREATED ......:
  MODIFIED .....: 08.03.91
                  26.06.98 kl swedish version
                  19.07.99 pt MOBILE version
                  23.10.99 kl TMS version
                  18.10.00 kl common version, add row 0 transactions to log
                  11.10.06 kl use propath 
                  07.12.06 aam compile.log to /tmp
                  06.09.07 kl compile *.cls

  VERSION ......: Yoigo
------------------------------------------------------ */

{Syst/country.i}
{Func/excel.i}
{Func/fcreatedir.i}

def var line      as char format "x(78)".
def var moduli    as char format "x(15)".
def var i         as int.
def var x         as int.
def var bOk       as lo.
def var aloitus   as int.
def var alku      as int.
def var loppu     as int.
def var kesto     as int.
def var yht       as int.
def var rpath      as char no-undo format "x(9)".
def var shortname as c format "x(12)".
def var prefix    as c.
def var errmsg    as c.
def var row0      as c    no-undo.
def var host      as c    no-undo.
def var llwarn    as log  no-undo.
def var lltakei   as log  no-undo.
def var lclogfile as char no-undo.

assign
   llwarn    = false
   lltakei   = false
   lclogfile = "/tmp/compile.log".
      
pause 0.
update
   skip(1) 
   lclogfile
      colon 15
      label "Log File"
      format "x(50)"
      help "Name of the log file"
      skip
   llwarn 
      colon 15
      label "Log Warnings"
      format "Yes/No"
      help "Write warnings to log file"
      skip(1)
with side-labels row 10 centered title " COMPILE " frame fwarn.
hide frame fwarn.
   
input through value("hostname").
import unformatted host.
input close.

def temp-table wmodule no-undo
    field wsname  as c   /* name without path */
    field wlname  as c
    FIELD wdir    AS c
    index wsname as primary wsname
    index wdir wdir wsname.

case {&country}:
   when {&FIN} then assign
      rpath = "/frontapps"
      rpath = "/tmsapps" when host = "pallas".
end.

form
   wmodule.wsname label "Module"   format "x(12)"
   wmodule.wlname label "Path"     format "x(18)"
   alku           label "Started"
   loppu          label "Ended" 
   kesto          label "CompTime" 
   yht            label "TOT Time" 
   bOk            label "!"        format "/x"
   
with title " COMPILER BATCH PROCESS (.r files into " + rpath + ") " 
   row 1 centered 17 down frame log.

view frame log.
message "Making Module List, wait ...".
pause 0.

DEFINE VARIABLE lcTMSDirectories AS CHARACTER NO-UNDO.

lcTMSDirectories = "Ar,Func,Class,Gwy,Help,HPD,Inv,Macro,Mailconf,Mc,Mc/lib,Mf,Mnp,Mm,Rate,Syst,triggers,templates".

if opsys = "unix" then do:
   
   my-nl = chr(10).
   
   do i = 1 to num-entries(lcTMSDirectories).
      
      prefix = "/apps/tms/" + entry(i,lcTMSDirectories) + "/".

      if index(prefix,"/apps/tms/") = 0 then next.
      if index(prefix,"Dev")        > 0 then next.
      if index(prefix,"Work")       > 0 then next.

      input thru value
        ("ls -1 " + prefix + "*.p" + " " +
        (if lltakei then prefix + "*.i* " else "") +
         prefix + "*.cls").

      repeat:
         
         import unformatted line.
         
         if index(line,"no such file or directory") > 0 then next.
         
         x = index(line,"*").
         if x > 0 then substr(line,x,1) = "".
         
         shortname = substr(line,length(prefix) + 1).

         find first wmodule where 
                    wmodule.wsname = shortname
         no-error.
         
         if not avail wmodule then do:
            create wmodule.
            assign 
               wmodule.wsname = shortname
               wmodule.wlname = line
               wmodule.wdir   = entry(i,lcTMSDirectories).
         end.

      end.
   
   end.

end.
else return.

create wmodule.
assign
   wmodule.wsname = "applhelp.p"
   wmodule.wlname = "/apps/tms/applhelp.p"
   wmodule.wdir   = "".

assign yht = 0.
output stream excel to value(lclogfile).

for each wmodule BREAK BY wdir BY wsname with frame log:

  IF FIRST-OF(wmodule.wdir)
  THEN fCreateDir(rpath + if wmodule.wdir > "" then "/" + wmodule.wdir ELSE "").

  assign
     alku   = time
     moduli = wmodule.wlname.
  
  display 
     wmodule.wsname         
     substr(wmodule.wlname,1,index(wmodule.wlname,wmodule.wsname) - 1)
     @ wmodule.wlname
     string(alku,"hh:mm:ss") @ alku.
  pause 0.   
  
  bOk = true.

  compile value((if wmodule.wdir > "" then wmodule.wdir + "/" ELSE "") + wmodule.wsname) save into value(rpath)
    listing value("/tmp/compile.tmp") no-error.
  
  input through value("grep ~"0 Procedure~" " + "/tmp/compile.tmp").
  repeat:
     import unformatted row0.
  end.
  if llwarn and index(row0,"Yes") > 0 then 
     put stream excel unformatted 
        "WARNING: " substr(row0,1,40) my-nl.
   
  do i = 1 to error-status:num-messages:
     errmsg = error-status:get-message(i).
     if substr(errmsg,1,7) ne "Warning" then do:
        put stream excel unformatted errmsg my-nl.
        bOk = false.
     end.
  end.
  
  if NOT bOk then do:
     bell.
     put stream excel unformatted my-nl.
  end.
  
  assign
     loppu = time
     kesto = loppu - alku.
     yht   = yht   + kesto.
  
  display  
     string(loppu,"hh:mm:ss") @ loppu
     string(kesto,"hh:mm:ss") @ kesto
     string(yht,  "hh:mm:ss") @ yht
     bOk.
  down.
  pause 0 no-message.

end.
input close.

os-delete value("/tmp/compile.tmp").

message "Possible error messages are in:" skip
        lclogfile
view-as alert-box title " COMPILER ".

