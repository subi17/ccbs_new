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
                  25.10.99 pt again MOBILE version
  VERSION ......: M15
------------------------------------------------------ */


{excel.i}

def var line      as char format "x(78)".
def var source_mod    as char format "x(15)".
def var i         as int.
def var x         as int.
def var bOk       as lo.
def var aloitus   as int.
def var alku      as int.
def var loppu     as int.
def var kesto     as int.
def var yht       as int.
def var path      as char no-undo init "../W_r".
def var shortname as c format "x(12)".
def var prefix    as c.
def var ppath     as c.
def var errmsg    as c.
def var compile   as i format "9".
def var err       as lo.
def var amterr    as i.
def var xlist     as c init "common,fixed,mobile".

ppath = "../Mc,../Mf,../Mm".

def temp-table wmodule no-undo
    field wsname  as c   /* name without path */
    field wlname  as c

    index wsname as primary
       wsname.

form
   "    COMPILE: " skip
   "  1: Common"   skip
   "  2: Fixed"    skip
   "  3: Mobile"   skip
   " " compile
with centered row 7 width 20 no-label frame frm.

update compile with frame frm.
if compile < 1 OR compile > 3 then return.

form
   wmodule.wsname label "Module" format "x(12)"
   wmodule.wlname label "Path"   format "x(19)"
   alku           label "Started"
   loppu          label "Ended"
   kesto          label "CompTime"
   yht            label "TOT Time"
   err            label "!"
with title " COMPILER BATCH PROCESS (.r files into " + path + ") " 
   row 1 centered 17 down frame log.

view frame log.
message "Making Module List, wait ...".     
pause 0.

if opsys = "unix" then do:

   my-nl = chr(10).

   do i = 1 to num-entries(ppath).

      if i NE compile then NEXT.

      prefix = entry(i,ppath).

      input thru  value("ls -1 " + prefix + "/*.p " +
                                   prefix + "/func.i").
      repeat:

         import unformatted line.

         shortname = entry(num-entries(line,"/"),line,"/").

         find first wmodule where 
                    wmodule.wsname = shortname
         no-error.

         if not avail wmodule then do:
            create wmodule.
            assign 
               wmodule.wsname = shortname
               wmodule.wlname = line.
         end.

      end.

   end.

end.
else return.

assign 
   yht    = 0
   amterr = 0.
output stream excel to value("compile_" + entry(compile,xlist) + ".log").

for each wmodule by wsname with frame log:

  if search(wmodule.wsname) = ? then next.

  assign
     alku   = time
     source_mod = wmodule.wsname.

  display 
     wmodule.wsname         
     substr(wmodule.wlname,1,index(wmodule.wlname,wmodule.wsname) - 1)
     @ wmodule.wlname
     string(alku,"hh:mm:ss") @ alku.
  pause 0.   

  bOk = true.
  compile value(source_mod) save into value(path) no-error.

  err = FALSE.

  do i = 1 to error-status:num-messages:

     errmsg = error-status:get-message(i).

     if substr(errmsg,1,7) ne "Warning" then do:

        put stream excel unformatted
           "SRC: " wmodule.wlname my-nl 
                   errmsg         my-nl.

        ASSIGN
           bOk = false
           err = TRUE.

     end.

  end.

  if NOT bOk then put stream excel unformatted my-nl.

  assign
     loppu = time
     kesto = loppu - alku.
     yht   = yht   + kesto.

  display  
     string(loppu,"hh:mm:ss") @ loppu
     string(kesto,"hh:mm:ss") @ kesto
     string(yht,  "hh:mm:ss") @ yht
     err format "x/".
  down.
  pause 0 no-message.

  amterr = amterr + int(err).

end.
input close.

message 
   string(amterr) "errors." skip
   "Messages are in: compile_" + entry(compile,xlist) + ".log !"
VIEW-AS ALERT-BOX.
