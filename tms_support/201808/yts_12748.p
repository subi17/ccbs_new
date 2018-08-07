Syst.Var:katun = "Qvantel".
{Syst/tmsconst.i}
{Func/flimitreq.i}

DEF VAR i AS INT NO-UNDO.

def stream sout.
output stream sout to /apps/yoigo/tms_support/201808/yts_12748.txt.

DEF TEMP-TABLE ttCustomer NO-UNDO
   FIELD custnum AS INT
   FIELD msisdns AS CHAR
INDEX custnum IS PRIMARY UNIQUE custnum.

DEF VAR lcCLiTypes AS CHAR NO-UNDO.

lcCLiTypes = {&TTF_SPECIAL_LIMIT_CLITYPES}.

do i = 1 to num-entries(lcCLiTypes):
FOR EACH mobsub NO-LOCK where
   mobsub.brand = "1" and
   mobsub.clitype = entry(i,lcCLiTypes):

   FIND FIRST ttCustomer NO-LOCK where
      ttCustomer.custnum = mobsub.custnum no-error.

   IF NOT AVAIL ttCustomer then do:
      create ttCustomer.
      assign
         ttCustomer.custnum = mobsub.custnum.
   end.

   ttCustomer.msisdns = ttCustomer.msisdns + "," + mobsub.cli.
end.
end.

def buffer blimit for limit.

DEF VAR ldValue AS DEC NO-UNDO extent 2.

ldValue[1] = 120.
ldValue[2] = 150.

DEF VAR liMsReq AS INT NO-UNDO.
DEF VAR ocResult AS CHAR NO-UNDO.

FOR EACH ttCustomer:

   FIND limit NO-LOCK where
      limit.custnum = ttCustomer.custnum and
      limit.limittype  = {&LIMIT_TYPE_TMRLIMIT} and
      limit.limitid = 1 and
      limit.tmruleseq = 3 and
      limit.todate > today no-error.
   IF NOT AVAIL limit then next.

   FIND blimit NO-LOCK where
      blimit.custnum = ttCustomer.custnum and
      blimit.limittype  = {&LIMIT_TYPE_TMRLIMIT} and
      blimit.limitid = 2 and
      blimit.tmruleseq = 3 and
      blimit.todate > today no-error.
   IF NOT AVAIL blimit then next.

   if blimit.defvalue ne limit.defvalue then next.

   if blimit.defvalue ne true then next.

   liMsReq = fLimitRequest(
     ?,           /* msseq */
     ttCustomer.custnum,   /* custum */
     Func.Common:mMakeTS(),   /* act.stamp */
     "update",    /* create, update */
     ldValue,  /* new limit values */
     FALSE,       /* default value */
     3, /* tmruleseq */
     1,           /* limit type */
     "5",         /* source of request  - Manual event web */
     OUTPUT ocResult).

   put stream sout unformatted
      ttCustomer.custnum ";"
      trim(ttCustomer.msisdns,",") ";"
      limit.limitamt ";"
      blimit.limitamt ";"
      liMsReq ";"
      ocResult skip.
end.
