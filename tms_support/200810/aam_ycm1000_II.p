{timestamp.i}

def stream sread.
input stream sread from /apps/snet/200810/chk_deny_billing.txt.

def var lcline    as char no-undo.
def var lccli     as char no-undo.
def var limsseq   as int  no-undo.
def var liagrcust as int  no-undo.
def var i         as int  no-undo.
def var j         as int  no-undo.
def var ldcurrent as dec  no-undo.
def var licode    as int  no-undo.

ldcurrent = fmakets().

repeat:

   import stream sread unformatted lcline.

   limsseq = 0.
   
   assign 
      lccli     = entry(1,lcline,chr(9))
      limsseq   = integer(entry(5,lcline,chr(9)))
      no-error.

   if error-status:error then next. 

   i = i + 1.
   

   licode = 0.
   
   for first actionlog no-lock where
             actionlog.brand = "1" and
             actionlog.tablename = "mobsub" and
             actionlog.keyvalue = string(limsseq) and
             actionlog.actionid = "denybill" and
             actionlog.fromdate = 11/1/8:
      licode = 1.
   end.
   
   if licode > 0 then j = j + 1.
       
   if i mod 100 = 0 then do:
      pause 0.
      disp i j with 1 down.
   end.
end.

input stream sread close.

disp i j.

