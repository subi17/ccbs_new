{commpaa.i}
assign gcbrand = "1"
       katun = "Qvantel".
{tmsconst.i}
{date.i}
{timestamp.i}
{fixedfee.i}
{fbundle.i}

DEF VAR lcbundle        AS CHAR NO-UNDO.
DEF VAR lcconfbundle    AS CHAR NO-UNDO.
DEF VAR lcprofile       AS CHAR NO-UNDO.
DEF VAR ldPeriodFrom    AS DEC  NO-UNDO.
DEF VAR ldPeriodTo      AS DEC  NO-UNDO.
DEF VAR ldaFromDate     AS DATE NO-UNDO FORMAT "99/99/9999".
DEF VAR ldaToDate       AS DATE NO-UNDO FORMAT "99/99/9999".
DEF VAR lcDelimiter     AS CHAR NO-UNDO INIT "|".
DEF VAR lcOutputFile    AS CHAR NO-UNDO FORMAT "X(45)".
DEF VAR liCount         AS INT  NO-UNDO.
DEF VAR liStatus        AS INT  NO-UNDO.

DEF TEMP-TABLE ttshaperinfo NO-UNDO
   FIELD MsSeq            AS INT
   FIELD MsRequest        AS INT
   FIELD CLI              AS CHAR
   FIELD CLIType          AS CHAR
   FIELD ActStamp         AS DEC
   FIELD DoneStamp        AS DEC
   FIELD Bundle           AS CHAR
   FIELD Profile          AS CHAR
   FIELD ExpProfile       AS CHAR
   INDEX MsSeq MsSeq.

fORM
   SKIP
   "Enter From Date          :" ldaFromDate SKIP
   "Enter To Date            :" ldaToDate   SKIP
   "Enter Output File Path   :" lcOutputFile
   WITH OVERLAY CENTERED ROW 10 TITLE " Check Shaper Profile " NO-LABELS
   FRAME fshaper.

UPDATE ldaFromDate
       ldaToDate
       lcOutputFile with FRAME fshaper.

IF lcOutputFile = "" THEN DO:
   MESSAGE "Please Enter Output File Path" VIEW-AS ALERT-BOX.
   RETURN.
END. /* IF lcOutputFile = "" THEN DO: */

/******** Main start ********/

ASSIGN
   ldPeriodFrom   = fMake2Dt(ldaFromDate,0)
   ldPeriodTo     = fMake2Dt(ldaToDate,86399).

OUTPUT TO VALUE(lcOutputFile).

/* Header Row */
PUT UNFORMATTED "CLI"              lcDelimiter
                "Subs. ID"         lcDelimiter
                "CLIType"          lcDelimiter
                "ActStamp"         lcDelimiter
                "DoneStamp"        lcDelimiter
                "Bundle"           lcDelimiter
                "Current Profile"  lcDelimiter
                "Expected Profile" SKIP.

DO liCount = 1 TO 2.

   IF liCount = 1 THEN liStatus = 9.
   ELSE liStatus = 2.

for each msrequest where
         msrequest.brand = "1" and
         msrequest.reqtype = 1 and
         msrequest.reqstatus = liStatus and
         msrequest.reqcparam1 = "shaper" and
         msrequest.actstamp >= ldPeriodFrom and
         msrequest.actstamp <= ldPeriodTo no-lock,
    first mobsub where mobsub.msseq = msrequest.msseq no-lock:

    if index(msrequest.reqcparam2,"GRACE") > 0 then next.

    assign lcbundle = ""
           lcprofile = ""
           lcconfbundle = "".

    case mobsub.clitype:
       when "contrd" then
           lcbundle = fGetActiveSpecificBundle(mobsub.msseq,fmakets(),mobsub.clitype).
       when "contf" then do:
           lcbundle = fGetActiveSpecificBundle(mobsub.msseq,msrequest.actstamp,mobsub.clitype).
           lcconfbundle = fGetActiveSpecificBundle(mobsub.msseq,fmakets(),"bono").
       end.
       otherwise
           lcbundle = fGetActiveSpecificBundle(mobsub.msseq,fmakets(),"bono"). 
    end case.

    IF lcbundle = "" THEN do:
       IF mobsub.clitype = "cont6" then lcbundle = "DUB".
       ELSE IF mobsub.clitype = "TARJRD1" then lcbundle = "TARJD1".
    END.

    case lcbundle:
       when "mdub" then do:
          if mobsub.clitype = "cont6" then lcprofile = "ladel4wbono8".
          else lcprofile = "bono8".
       end.
       when "mdub2" then do:
          if mobsub.clitype = "cont6" then lcprofile = "ladel4wbono15".
          else lcprofile = "bono15".
       end.
       when "mdub3" then do:
          if mobsub.clitype = "cont6" then lcprofile = "ladel4wbono25".
          else lcprofile = "bono25".
       end.
       when "mdub4" then do:
          if mobsub.clitype = "cont6" then lcprofile = "ladel4wbono35".
          else lcprofile = "bono35".
       end.
       when "mdub5" then do:
          if mobsub.clitype = "cont6" then lcprofile = "ladel4wbono12".
          else lcprofile = "bono12".
       end.
       when "contdata" then lcprofile = "IPL25".
       when "contd2" then lcprofile = "IPL35".
       when "contd3" then lcprofile = "IPL8".
       when "contd4" then lcprofile = "IPL15".
       when "contf20" OR when "contf10" then DO:
          if lcconfbundle = "mdub" then lcprofile = "bono8".
          else if lcconfbundle = "mdub2" then lcprofile = "bono15".
          else if lcconfbundle = "mdub3" then lcprofile = "bono25".
          else if lcconfbundle = "mdub4" then lcprofile = "bono35".
          else if lcconfbundle = "mdub5" then lcprofile = "bono12".
          else lcprofile = "Default".
       end.
       when "contf20D" then do:
          if lcconfbundle = "mdub" then lcprofile = "PLANA20wbono8".
          else if lcconfbundle = "mdub2" then lcprofile = "PLANA20wbono15".
          else if lcconfbundle = "mdub3" then lcprofile = "PLANA20wbono25".
          else if lcconfbundle = "mdub4" then lcprofile = "PLANA20wbono35".
          else if lcconfbundle = "mdub5" then lcprofile = "PLANA20wbono12".
          else lcprofile = "PLANA20".
       end.
       when "contf30" then do:
          if lcconfbundle = "mdub" then lcprofile = "PLANA30wbono8".
          else if lcconfbundle = "mdub2" then lcprofile = "PLANA30wbono15".
          else if lcconfbundle = "mdub3" then lcprofile = "PLANA30wbono25".
          else if lcconfbundle = "mdub4" then lcprofile = "PLANA30wbono35".
          else if lcconfbundle = "mdub5" then lcprofile = "PLANA30wbono12".
          else lcprofile = "PLANA30".
       end.
       when "contf40" then do:
          if lcconfbundle = "mdub" then lcprofile = "PLANA40wbono8".
          else if lcconfbundle = "mdub2" then lcprofile = "PLANA40wbono15".
          else if lcconfbundle = "mdub3" then lcprofile = "PLANA40wbono25".
          else if lcconfbundle = "mdub4" then lcprofile = "PLANA40wbono35".
          else if lcconfbundle = "mdub5" then lcprofile = "PLANA40wbono12".
          else lcprofile = "PLANA40".
       end.
       when "contf55" then do:
          if lcconfbundle = "mdub" then lcprofile = "PLANA55wbono8".
          else if lcconfbundle = "mdub2" then lcprofile = "PLANA55wbono15".
          else if lcconfbundle = "mdub3" then lcprofile = "PLANA55wbono25".
          else if lcconfbundle = "mdub4" then lcprofile = "PLANA55wbono35".
          else if lcconfbundle = "mdub5" then lcprofile = "PLANA55wbono12".
          else lcprofile = "PLANA55".
       end.
       when "pmdub" then lcprofile = "bono8_pre".
       when "TARJD1" then lcprofile = "IPL8_PRE".
       when "dub" then lcprofile = "ladel4".
       otherwise lcprofile = "default".
    end case.

    status default msrequest.reqcparam2 + "|" + lcprofile.

    create ttshaperinfo.
    assign ttshaperinfo.msseq      = mobsub.msseq
           ttshaperinfo.cli        = mobsub.cli
           ttshaperinfo.clitype    = mobsub.clitype
           ttshaperinfo.actstamp   = msrequest.actstamp
           ttshaperinfo.donestamp  = msrequest.donestamp
           ttshaperinfo.bundle     = lcbundle
           ttshaperinfo.profile    = msrequest.reqcparam2
           ttshaperinfo.expprofile = lcprofile
           ttshaperinfo.MsRequest  = msrequest.msrequest.

end.
end.

for each ttshaperinfo no-lock
    break by ttshaperinfo.msseq
          by ttshaperinfo.MsRequest DESC:
   if first-of(ttshaperinfo.msseq) then do:
      if ttshaperinfo.profile <> ttshaperinfo.expprofile then
          put unformatted
          ttshaperinfo.cli               lcDelimiter
          string(ttshaperinfo.msseq)     lcDelimiter
          ttshaperinfo.clitype           lcDelimiter
          string(ttshaperinfo.Actstamp)  lcDelimiter
          string(ttshaperinfo.donestamp) lcDelimiter
          ttshaperinfo.bundle            lcDelimiter
          ttshaperinfo.profile           lcDelimiter
          ttshaperinfo.expprofile        skip.
   end.
end.

output close.
