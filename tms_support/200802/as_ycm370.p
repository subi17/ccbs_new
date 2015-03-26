def stream sread.
input stream sread from /apps/snet/200802/salesman_dump_270208_latin1.
def stream slog.
output stream slog to /apps/snet/200802/as_salesman.log2.
def var lcline    as char no-undo.


DEFINE VARIABLE lcSalesman AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcName AS CHARACTER NO-UNDO.

FOR EACH salesman EXCLUSIVE-LOCK:
   DELETE salesman.
END.

repeat:
   import stream sread unformatted lcline.
    
    lcSalesman = trim(entry(1,lcLine,",")).
    lcName = trim(entry(2,lcLine,",")) + " " + trim(entry(3,lcLine,",")).
   /* 
    if num-entries(lcLine,chr(9)) = 5 then
    lcName = " " + trim(entry(5,lcLine,chr(9))).
    */
    put stream slog unformatted
      "salesman: "  lcSalesman  skip
      "name: "      lcName.
/*
    if num-entries(lcLine,chr(9)) = 5 then
    put stream slog  " " + entry(5,lcLine,chr(9)).
  */ 
   put stream slog skip(2).

   create salesman.
   assign
      salesman.brand    = "1"
      salesman.salesman = lcSalesman
      salesman.smname   = lcName.

end.

input stream sread close.
output stream slog close.
