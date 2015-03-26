/* --------------------------------------------------------------------------
 MODULE ...........: prmem.p
 TASK .............: print out a memo
 SOLUTION .........: 
 CREATED ..........: 13.11.2001 lp
 CHANGED ..........: 15.09.2003/aam brand
 Version ..........: M15
--------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}
{utumaa.i new }

DEF INPUT PARAMETER  HostTable LIKE memo.HostTable  NO-UNDO.
DEF INPUT PARAMETER  KeyValue  LIKE memo.KeyValue   NO-UNDO.
DEF INPUT PARAMETER  MemoSeq   LIKE memo.MemoSeq    NO-UNDO.

DEF TEMP-TABLE t-text
   FIELD tt-no   AS I
   FIELD tt-line AS C.


DEF VAR sl        AS I  NO-UNDO.
DEF VAR rl        AS I  NO-UNDO.
DEF VAR MemoText  AS C  NO-UNDO.
DEF VAR memoline  AS C  NO-UNDO.
DEF VAR i         AS I  NO-UNDO.
DEF VAR x         AS I  NO-UNDO.
DEF VAR space-pos AS I  NO-UNDO.
DEF VAR lf        AS C  NO-UNDO. 
DEF VAR lf-pos    AS I  NO-UNDO.
DEF VAR cru-name  AS C  NO-UNDO.
DEF VAR mou-name  AS C  NO-UNDO.
DEF VAR CrTime   AS C  NO-UNDO.
DEF VAR mo-time   AS C  NO-UNDO.
DEf VAR tabLbl    AS C  NO-UNDO FORMAT "x(20)".
DEF VAR MemoTitle    AS C  NO-UNDO FORMAT "x(60)".
DEF VAR db-name   AS C  NO-UNDO.

/* Line Feed Char used as line separator in MemoText field */
LF = chr(10).

FIND FIRST memo WHERE memo.Brand     = gcBrand   AND
                      memo.HostTable = HostTable AND
                      memo.KeyValue  = KeyValue  AND 
                      memo.MemoSeq   = MemoSeq NO-LOCK NO-ERROR.

ASSIGN tuni1 = "memo"
       tuni2 = "".
       tila  = true.
{tmsreport.i RETURN}

FORM HEADER                                                 
   FILL ("=",78) FORMAT "x(78)"                                          SKIP
   ynimi "MEMO/NOTE" AT 34 pvm FORMAT "99.99.99" TO 78                   SKIP
   memo.MemoTitle AT 28 "Page "  AT 71        sl FORMAT "zz9"      TO 78    SKIP
   FILL ("=",78) FORMAT "x(78)"                                          SKIP
   WITH
   WIDTH 78 NO-LABEL NO-BOX FRAME page_header.

FORM HEADER
   MemoTitle                                                                SKIP
   "- created by user" memo.CreUser cru-name FORMAT "x(28)" 
   "at" CrTime FORMAT "x(19)"                                           SKIP
   "- changed by user" memo.ChgUser mou-name FORMAT "x(28)" 
   "at" mo-time FORMAT "x(19)"                                           SKIP
   FILL ("-",78) FORMAT "x(78)"                                          SKIP 
   WITH
   WIDTH 78 NO-LABEL NO-BOX FRAME main_header.

/* Get label for table */
tabLbl = HostTable.
RUN ufile1(INPUT HostTable, OUTPUT db-name, OUTPUT tabLbl).

/* user names */
FIND TMSUser WHERE TMSUser.UserCode = memo.CreUser NO-LOCK NO-ERROR.
IF AVAIL TMSUser THEN cru-name = TMSUser.UserName.
ELSE cru-name = "? UNKNOWN USER ?".
CrTime = fTS2hms(memo.CreStamp).

IF memo.ChgUser NE "" THEN DO:
   FIND TMSUser WHERE TMSUser.UserCode = memo.ChgUser NO-LOCK NO-ERROR.
   IF AVAIL TMSUser THEN mou-name = TMSUser.UserName.
   ELSE mou-name = "? UNKNOWN USER ?".
   mo-time = fTS2hms(memo.ChgStamp).
END.

MemoTitle = "Memo belongs to   " + tabLbl + " '" + KeyValue + "'".


/* copy memotext (line BY line) into temp table */

RUN ulfscon(memo.memotext,60,OUTPUT TABLE t-text).

/* print page header */
sl = 1.
VIEW STREAM tul FRAME page_header.
VIEW STREAM tul FRAME main_header.
rl = 8.

/* print memo lines */
FOR EACH t-text:
/* does this line fit into page ? */
   IF rl >= skayt1 THEN DO:
      ASSIGN
      sl = sl + 1.
      PUT STREAM tul UNFORMATTED CHR(12).
      VIEW STREAM tul FRAME page_header.
      rl = 4.
   END.
   PUT STREAM tul tt-no FORMAT "Z9"  tt-line FORMAT "x(58)" AT 4
   SKIP.
   rl = rl + 1.
END.


/* eject last page from printer */
PUT STREAM tul UNFORMATTED CHR(12).

/* close stream */
tila = false.
{tmsreport.i}


