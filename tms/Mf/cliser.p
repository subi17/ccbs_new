/* ----------------------------------------------------------------------
  MODULE .......: CLISER.P
  TASK .. ......: Move part of CLISer to another customer
  APPLICATION ..: TMS
  AUTHOR .......: TK
  CREATED ......: 04.07.2002
  MODIFIED .....: 
  VERSION ......: M15
  --------------------------------------------------------------------- */
{Syst/commali.i}
{Func/cparam2.i}

DEF INPUT PARAM startCLI LIKE CLI.CLI          NO-UNDO.
DEF INPUT PARAM endCLI   LIKE CLI.CLI          NO-UNDO.
DEF INPUT PARAM newcust  LIKE Customer.CustNum NO-UNDO.

DEF VAR zeros  AS I  NO-UNDO.
DEF VAR lstamp AS DE NO-UNDO.

DEF BUFFER bufSer FOR CLISer.

lstamp = DEC(fCparamC("DefClStamp")).

/* find one active CLI */

FIND FIRST CLI WHERE
           CLI.CLI >= startCLI AND
           CLI.CLI <= endCLI   AND
           CLI.ClStamp = lstamp
NO-LOCK no-error.

/* now find CLISer of this CLI */

FIND FIRST CLISer WHERE
           CLISer.CLIFrom <= startCLI AND
           CLISer.CLITo   >= endCLI   AND
           CLISer.CustNum  = CLI.CustNum
EXCLUSIVE-LOCK.

/* new CLISer not starting from beginning of existing CLISer */     

IF startcli > CLISer.CLIFrom THEN DO:
   CREATE bufSer.
   BUFFER-COPY CLISer to bufSer.
   zeros = 0.
   DO WHILE SUBSTR(bufser.CLITo,zeros + 1,1) = "0".
      zeros = zeros + 1.
   END.
   bufSer.CliTo = FILL("0",zeros) + STRING(INT(startcli) - 1).
END.

/* new CLISer ending before existing CLISer */

IF endcli < CLISer.CLITo THEN DO:
   CREATE bufSer.
   BUFFER-COPY CLISer to bufSer.
   zeros = 0.
   DO WHILE SUBSTR(bufser.CLIFrom,zeros + 1,1) = "0".
      zeros = zeros + 1.
   END.
   bufSer.CliFrom = FILL("0",zeros) + STRING(INT(endcli) + 1).
END.


/* create new, active series for new customer */

CREATE bufSer.
BUFFER-COPY CLISer to bufSer.
ASSIGN
   bufSer.CLIFrom = startcli
   bufSer.CLITo   = endcli
   bufSer.CustNum = newcust.

/* create passive series for the old customer */

CREATE bufSer.
BUFFER-COPY CLISer to bufSer.
ASSIGN
   bufSer.CLIFrom = startcli
   bufSer.CLITo   = endcli.

/* Move CLIs to new customer */

RUN Mf/climove.p(CLISER.CustNum,   /* from cust */
              newcust,          /* to cust   */
              startcli,         /* from CLI  */
              endcli,           /* to CLI    */
              FALSE).           /* interact  */


/* remove original series */

DELETE CLISER.

