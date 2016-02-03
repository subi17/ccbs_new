{Func/excel.i}
{Syst/country.i}

def var a      as c no-undo format "x(35)".
DEF VAR b      AS c NO-UNDO.
DEF VAR i      AS i NO-UNDO.
DEF VAR errmsg AS c NO-UNDO.
DEF VAR clst   AS c NO-UNDO.
DEF VAR row0   AS c NO-UNDO.
def var rpath  as c no-undo format "x(20)".
def var module as c no-undo.
def var host   as c no-undo.

input through value("hostname").
import unformatted host.
input close.

if host = "pallas"
then rpath = "/tmsapps/".
else rpath = "/frontapps".


form 
   "!  Compiled file into "  rpath  no-label " !" skip(1)
   "- Row 0 transaction will be shown -"         
WITH  no-box OVERLAY centered ROW 5 FRAME frm.

form
   a NO-LABEL AT 2
     help  "Give the name of the module to compile, without '.p'"
with title "    COMPILE    " OVERLAY centered ROW 9 FRAME a.

DISP rpath WITH FRAME frm.

LOOP:
repeat WITH FRAME a:

   view FRAME frm.

   update a validate(a = "" or search(a + ".p")   <> ? 
                            or search(a + ".i")   <> ?
                            or search(a + ".cls") <> ?,
            "Unknown module " + a + ".p/.i/.cls  !").
   if a = "" THEN QUIT.
   
   HIDE ALL no-pause.
   HIDE MESSAGE no-pause.

   if      search(a + ".p")   <> ? then module = a + ".p".
   else if search(a + ".i")   <> ? then module = a + ".i".
   else if search(a + ".cls") <> ? then module = a + ".cls".

   INPUT THROUGH VALUE("find /apps/tms/ -follow -name '" + module + "'").
   module = "?".
   REPEAT:
      IMPORT UNFORMATTED module.
      IF INDEX(MODULE,"Work") = 0 AND
         INDEX(MODULE,"Dev")  = 0 THEN LEAVE.
   END.

   IF module = "?" THEN DO:

      MESSAGE
         "File is not in production directory !"  SKIP
         module
      VIEW-AS ALERT-BOX.
      RETURN.

   END.

   clst = "/tmp/" + a + ".lst".
   compile value(module) save into value(rpath) listing value(clst) NO-ERROR.

   errmsg = "".
   DO i = 1 TO error-status:num-messages:
      IF error-status:get-number(i) NE 2886 THEN
         errmsg = errmsg + error-status:get-message(i) + my-nl.
   END.

   input through value("grep \"0 Procedure\" " + clst).
   repeat:
      IMPORT UNFORMATTED row0.
   END.
   if index(row0,"Yes") = 0 then row0 = "".

   os-delete value(clst).

   if errmsg ne "" THEN
      MESSAGE 
         errmsg SKIP
         row0   
      VIEW-AS ALERT-BOX error.
   else if row0 ne "" THEN MESSAGE row0 VIEW-AS ALERT-BOX MESSAGE.

   PUT SCREEN ROW 22 COL 1 "COMPILED: " + module.

END.

PUT SCREEN ROW 22 COL 1 FILL(" ",30).

