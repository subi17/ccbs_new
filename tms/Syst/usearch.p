/* usearch.p  - searches a module THROUGH PROPATH */   


{Syst/commpaa.i}

def var module  as c  format "x(8)".
def var source  as c  format "x(40)".
def var object  as c  format "x(40)".
def var lsource as lo format "Also in local dir/".
def var lobject as lo format "Also in local dir/".
form
"  Module ......:" module 
help "Name of a program module WITHOUT the .p -extention" SKIP
"   .p .........:" source lsource skip
"   .r .........:" object lobject skip
with overlay title " CHECK PROPATH " NO-LABELS centered ROW 6 FRAME frm.


PAUSE 0.
repeat WITH FRAME frm: 
   ehto = 9. RUN Syst/ufkey.p.
   UPDATE module.

   if module = "" THEN LEAVE.
   source = search(module + ".p").
   object = search(module + ".r").
   lsource = search("./" + module + ".p") NE ?.
   lobject = search("./" + module + ".r") NE ?.

   DISP source lsource object lobject.


   message "Next search - press ENTER !".
   PAUSE no-message.
   CLEAR FRAME frm.
END.        
HIDE FRAME frm.
HIDE MESSAGE.

