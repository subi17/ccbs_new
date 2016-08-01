def stream stroutbefore.
def stream stroutafter.
def stream strin.

input stream strin from "invkeyvalues.txt".
output stream stroutbefore to "ydr_2018_smssender_check_before.txt".
output stream stroutafter  to "ydr_2018_smssender_check_after.txt".

DEF VAR lcLine  AS CHAR NO-UNDO.
DEF VAR lcError AS CHAR NO-UNDO.

define temp-table ttReadKeyValues no-undo
   field KeyValue as character
   field Spanish  as logical
   field English  as logical.

define temp-table ttInvText no-undo
   field Target as character
   field keyvalue as character
   field language as int
   field smstext as character
   field fromdate as date
   field todate   as date.

REPEAT:

   import stream strin unformatted lcLine.

   CREATE ttReadKeyValues.
   ASSIGN ttReadKeyValues.keyvalue = trim(entry(1,lcLine,";"))
          ttReadKeyValues.Spanish  = LOGICAL(entry(2,lcLine,";"))
          ttReadKeyValues.English  = LOGICAL(entry(3,lcLine,";")).

END.

FOR EACH ttReadKeyValues NO-LOCK:

   FOR EACH invtext NO-LOCK where
            invtext.Brand    = "1"                      and
            invtext.target   = "SMS"                    and
            invtext.keyvalue = ttReadKeyValues.keyvalue and
            invtext.todate   >= TODAY:

      create ttInvText.
      assign ttInvText.target   = invtext.target
             ttInvText.keyvalue = invtext.keyvalue
             ttInvText.language = invtext.language
             ttInvText.smstext  = invtext.invtext
             ttInvText.fromdate = invtext.fromdate
             ttInvText.todate   = invtext.todate.

      FOR EACH reptext NO-LOCK where
               reptext.brand    = "1"                   and
               reptext.texttype = 32                    and
               reptext.linkcode = string(invtext.itnum) and
               reptext.todate  >= TODAY:

         if can-find(first ttInvtext NO-LOCK where
                           ttInvtext.target   = invtext.target    and
                           ttInvtext.Keyvalue = invtext.keyvalue  and
                           ttInvtext.language = reptext.language) then next.

         create ttInvText.
         assign ttInvText.target   = invtext.target
                ttInvText.keyvalue = invtext.keyvalue
                ttInvText.language = reptext.language
                ttInvText.smstext  = reptext.reptext
                ttInvText.fromdate = reptext.fromdate
                ttInvText.todate   = reptext.todate.
      end.

   end.

end.

FOR EACH ttInvtext NO-LOCK:

  put stream stroutbefore unformatted
      ttInvText.target   "|"
      ttInvText.keyvalue "|"
      ttInvText.language "|"
      ttInvText.smstext  "|"
      ttInvText.fromdate "|"
      ttInvText.todate SKIP.

end.

FOR EACH ttInvtext NO-LOCK:

   IF INDEX(ttInvtext.smstext,"Yoigo Info:") > 0 THEN
      ttInvtext.smstext = REPLACE(ttInvtext.smstext,"Yoigo Info:", "").

   put stream stroutafter unformatted
      ttInvText.target   "|"
      ttInvText.keyvalue "|"
      ttInvText.language "|"
      ttInvText.smstext  "|"
      ttInvText.fromdate "|"
      ttInvText.todate SKIP.

end.

input stream strin close.
output stream stroutbefore close.
output stream stroutafter close.
