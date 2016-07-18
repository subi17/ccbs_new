def stream strkeyvalues.
def stream strdumpdata.

def stream strFilein.
def stream strLinein.
def stream strreqaction.

input stream strkeyvalues from "invkeyvalues.txt".

DEF VAR lcIncDir        AS CHAR NO-UNDO.
DEF VAR lcKeyValuesLine AS CHAR NO-UNDO.
DEF VAR lcdumpdataLine  AS CHAR NO-UNDO.
DEF VAR lcKeyValue      AS CHAR NO-UNDO.
DEF VAR lcChangeLine    AS CHAR NO-UNDO.
DEF VAR lcFileName      AS CHAR NO-UNDO.
DEF VAR lcRequestLine   AS CHAR NO-UNDO.
DEF VAR llgAvailable    AS LOG  NO-UNDO. 

def temp-table ttDumpData no-undo
   field DumpLine as character
   field lastentry as int.

def temp-table ttDataChange no-undo
   field lcLine as char
   field lastentry as int.

repeat:

   import stream strkeyvalues unformatted lcKeyValuesLine.

   lcKeyValue = entry(1,lcKeyValuesLine,";").

   unix silent value("grep -i " + lcKeyValue + " /apps/yoigo/tms_support/utilities/tabledump/config_tables/RequestAction.d >> ydr_2018_smssender_real_exec_dump_configdata.txt").

end.

input stream strkeyvalues close.

/* Reading data from dump configdata textfile to temp-table */
input stream strdumpdata from "ydr_2018_smssender_real_exec_dump_configdata.txt".

repeat:

  import stream strdumpdata unformatted lcdumpdataLine.

  create ttDumpData.
  assign ttDumpData.DumpLine = lcdumpdataLine
         ttDumpData.lastentry = int(entry(10,lcdumpdataLine," ")).

end.

input stream strdumpdata close.

/* Creating temp-table WITH Requestaction.d file data AND update smssender data, 
   so that we can dump AND CREATE a new RequestAction.d file */

lcIncDir = "/apps/yoigo/tms_support/utilities/tabledump/config_tables/".

input stream strFilein through value("ls -1tr " + lcIncDir).
repeat:

   import stream strFilein unformatted lcFileName.

   IF lcFileName eq "RequestAction.d" THEN DO:

      input stream strLinein from value(lcIncDir + lcFileName).

      RequestAction_LINE_LOOP:
      REPEAT TRANSACTION:

         import stream strLinein unformatted lcRequestLine.

         llgAvailable = NO.

         FOR EACH ttDumpData NO-LOCK where
                  ttDumpData.DumpLine matches lcRequestLine:

            lcChangeline = "".
            
            if index(lcRequestLine,"|22622") > 0 THEN
               lcChangeline = REPLACE(lcRequestLine,"|22622","").
            else if index(lcRequestLine,"|622")   > 0 THEN
               lcChangeline = REPLACE(lcRequestLine,"|622","").

            create ttDataChange.
            assign ttDataChange.lcLine = lcChangeLine.

            llgAvailable = YES.

         END.

         if llgAvailable THEN NEXT.

         create ttDataChange.
         assign ttDataChange.lcLine = lcRequestLine.
      END.

   END.

end.

/* Removing old RequestAction.d file AND creating a new file WITH ttDataChange temp-table data */
DO TRANS:
   unix silent value ("rm /apps/yoigo/tms_support/utilities/tabledump/config_tables/RequestAction.d").

   OUTPUT STREAM strreqaction to "/apps/yoigo/tms_support/utilities/tabledump/config_tables/RequestAction.d".

   FOR EACH ttDataChange NO-LOCK:
      put STREAM strreqaction unformatted 
         ttDataChange.lcLine SKIP.
   end.
END.

input stream strdumpdata close.
OUTPUT STREAM strreqaction CLOSE.
