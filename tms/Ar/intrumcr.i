/* intrumcr.i           19.11.2002/aam

*/


DEF TEMP-TABLE ttError NO-UNDO
   FIELD ErrOrd   AS INT
   FIELD ErrCode  AS INT
   FIELD Inv      AS CHAR
   FIELD Cust     AS CHAR
   FIELD Org      AS CHAR
   FIELD Name     AS CHAR
   FIELD IntrRef  AS CHAR
   FIELD ClCancel AS INT
   FIELD Amount   AS DEC
   INDEX ErrOrd ErrOrd.

DEF VAR lcErrorExpl AS CHAR NO-UNDO.

lcErrorExpl = "Invoice nbr could not be determined,"  +
              "Invoice was not found from TMS,"       +
              "Claiming was already cancelled,"       +
              "Credit loss was not posted,"           +
              "Invoice credited,"                     +
              "Invoice already fully paid,"           +
              "Open balance less than file's amount," +
              "Credit loss posting less than open balance".


