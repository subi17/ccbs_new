/*COFF YPR-5240*/
/*Program changes traiff name to CLIType and makes required reptext chantes.*/
DEF VAR liUpdate AS INT NO-UNDO.
DEF VAR lcRet AS CHAR NO-UNDO.

DEF TEMP-TABLE ttRepText LIKE reptext.

liUpdate = 0. /*Simulation == 0, Execution == 1*/


FUNCTION fUpdateName RETURNS CHAR
   (icCLIType AS CHAR,
    icNewName AS CHAR):

   DEF BUFFER CLIType FOR CLIType.
   DEF BUFFER breptext FOR reptext.
   DEF VAR lcMessage AS CHAR NO-UNDO.

   FIND FIRST CLIType WHERE 
           CLIType.clitype EQ icCLIType.

   IF AVAIL CLIType THEN DO:
       MESSAGE "changing CLIType name: " +
               CLIType.cliname + " -> " +
               icNewName + ". Update mode: " + 
               STRING(liUpdate) VIEW-AS ALERT-BOX.
      IF liUpdate NE 0 THEN DO:
         CLIType.cliname = icNewName.
      END.
   
   END.
   ELSE DO:
      lcMessage =  "CLIType " + icCliType + " not found".
      MESSAGE lcMessage VIEW-AS ALERT-BOX.
      RETURN lcMessage.
   END.

   FOR EACH reptext where 
            reptext.linkcode EQ icCLIType AND
            reptext.todate > TODAY:
      CREATE ttRepText.
      BUFFER-COPY reptext TO ttRepText.
      ttRepText.FromDate = TODAY.
      ttRepText.RepText = icNewName.
      DISP ttRepText.
      IF liUpdate NE 0 THEN DO:
         reptext.todate = today - 1.
         CREATE breptext.
         BUFFER-COPY ttRepText to breptext.
      END.
   END.
   RETURN "OK".
END.    

lcRet = fUpdateName("CONTDSL45","La Combinada Morada 20").
MESSAGE lcRet VIEW-AS ALERT-BOX.

lcRet = fUpdateName("CONTFH45_50","La Combinada Morada 50").
MESSAGE lcRet VIEW-AS ALERT-BOX.

lcRet = fUpdateName("CONTFH55_300","La Combinada Morada 300").
MESSAGE lcRet VIEW-AS ALERT-BOX.


