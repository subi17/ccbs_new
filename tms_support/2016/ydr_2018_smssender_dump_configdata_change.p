def stream strkeyvalues.
def stream strdumpdata.

def stream strFilein.
def stream strLinein.
def stream stroutbefore.
def stream stroutafter.

input stream strkeyvalues from "invkeyvalues.txt".

DEF VAR lcIncDir        AS CHAR NO-UNDO.
DEF VAR lcKeyValuesLine AS CHAR NO-UNDO.
DEF VAR lcdumpdataLine  AS CHAR NO-UNDO.
DEF VAR lcKeyValue      AS CHAR NO-UNDO.
DEF VAR lcChangeLine    AS CHAR NO-UNDO.
DEF VAR lcFileName      AS CHAR NO-UNDO.
DEF VAR lcRequestLine   AS CHAR NO-UNDO.

def temp-table ttDumpData no-undo
   field DumpLine as character
   field lastentry as int.

def temp-table ttDataChange no-undo
   field lcLine as char
   field lastentry as int.

repeat:

   import stream strkeyvalues unformatted lcKeyValuesLine.

   lcKeyValue = entry(1,lcKeyValuesLine,";").

   unix silent value("grep -i " + lcKeyValue + " /apps/yoigo/tms_support/utilities/tabledump/config_tables/RequestAction.d >> ydr_2018_smssender_dump_configdata.txt").

end.

input stream strkeyvalues close.

/* Reading data from dump configdata textfile to temp-table */
input stream strdumpdata from "ydr_2018_smssender_dump_configdata.txt".
output stream stroutbefore to "ydr_2018_smssender_dump_configdata_change_before.txt".

repeat:

  import stream strdumpdata unformatted lcdumpdataLine.

  create ttDumpData.
  assign ttDumpData.DumpLine = lcdumpdataLine
         ttDumpData.lastentry = int(entry(10,lcdumpdataLine," ")).

 end.

FOR EACH ttDumpData NO-LOCK by
         ttDumpData.lastentry:

   put stream stroutbefore unformatted
      ttDumpData.DumpLine SKIP.

END.

input stream strdumpdata close.
output stream stroutbefore close.

/* Updating the sms sender values of dump configdata textfile lines */
lcIncDir = "/apps/yoigo/tms_support/utilities/tabledump/config_tables/".

output stream stroutafter to "ydr_2018_smssender_dump_configdata_change_after.txt".

input stream strFilein through value("ls -1tr " + lcIncDir).
repeat:

   import stream strFilein unformatted lcFileName.

   IF lcFileName eq "RequestAction.d" THEN DO:

      input stream strLinein from value(lcIncDir + lcFileName).

      RequestAction_LINE_LOOP:
      REPEAT TRANSACTION:

         import stream strLinein unformatted lcRequestLine.

         FOR EACH ttDumpData NO-LOCK where
                  ttDumpData.DumpLine matches lcRequestLine:

            lcChangeline = "".
            
            if index(lcRequestLine,"|22622") > 0 THEN
               lcChangeline = REPLACE(lcRequestLine,"|22622","").
            else if index(lcRequestLine,"|622")   > 0 THEN
               lcChangeline = REPLACE(lcRequestLine,"|622","").

            lcRequestLine = lcChangeline.

            create ttDataChange.
            assign ttDataChange.lcLine = lcChangeLine
                   ttDataChange.lastentry = int(entry(10,lcChangeLine," ")).
         END.
      END.

   END.

end.

FOR EACH ttDataChange NO-LOCK by
         ttDataChange.lastentry:

   put stream stroutafter unformatted
      ttDataChange.lcLine SKIP.

end.

input stream strdumpdata close.
output stream stroutafter close.
