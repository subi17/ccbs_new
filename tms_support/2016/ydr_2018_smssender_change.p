def stream strin.
def stream stroutbefore.
def stream stroutafter.

output stream stroutbefore to "ydr_2018_smssender_change_before.txt".
output stream stroutafter  to "ydr_2018_smssender_change_after.txt".
input stream strin from "invkeyvalues.txt".

DEF VAR lcLine    AS CHAR NO-UNDO.
DEF VAR lcSMSText AS CHAR NO-UNDO.

define temp-table ttReadKeyValues no-undo
   field KeyValue as character
   field Spanish  as logical
   field English  as logical.

REPEAT:

   import stream strin unformatted lcLine.

   CREATE ttReadKeyValues.
   ASSIGN ttReadKeyValues.keyvalue = trim(entry(1,lcLine,";"))
          ttReadKeyValues.Spanish  = LOGICAL(entry(2,lcLine,";"))
          ttReadKeyValues.English  = LOGICAL(entry(3,lcLine,";")).

END.

DO TRANS:
   FOR EACH ttReadKeyValues NO-LOCK:

      FOR EACH invtext EXCLUSIVE-LOCK where
               invtext.Brand    = "1"                      and
               invtext.target   = "SMS"                    and
               invtext.keyvalue = ttReadKeyValues.keyvalue and
               invtext.todate   >= TODAY:

         put stream stroutbefore unformatted
            invtext.target   "|"
            invtext.keyvalue "|"
            invtext.language "|"
            invtext.invtext  "|"
            invtext.fromdate "|"
            invtext.todate   SKIP.
         IF INDEX(Invtext.invtext,"Yoigo Info:") > 0 THEN
             invtext.invtext = REPLACE(Invtext.invtext,"Yoigo Info:", "").

         put stream stroutafter unformatted
            invtext.target   "|"
            invtext.keyvalue "|"
            invtext.language "|"
            invtext.invtext  "|"
            invtext.fromdate "|"
            invtext.todate   SKIP.

         FOR EACH reptext EXCLUSIVE-LOCK where
                  reptext.brand    = "1"                   and
                  reptext.texttype = 32                    and
                  reptext.linkcode = string(invtext.itnum) and
                  reptext.todate  >= TODAY:

            put stream stroutbefore unformatted
               invtext.target   "|"
               invtext.keyvalue "|"
               reptext.language "|"
               reptext.reptext  "|"
               reptext.fromdate "|"
               reptext.todate   SKIP.

            IF INDEX(reptext.reptext,"Yoigo Info:") > 0 THEN
               reptext.reptext = REPLACE(reptext.reptext,"Yoigo Info:", "").

            put stream stroutafter unformatted
               invtext.target   "|"
               invtext.keyvalue "|"
               reptext.language "|"
               reptext.reptext  "|"
               reptext.fromdate "|"
               reptext.todate   SKIP.

         end.
      end.
   end.

end.

input stream strin close.
output stream stroutbefore close.
output stream stroutafter close.
