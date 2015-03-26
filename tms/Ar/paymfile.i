/* paymfile.i       12.08.02/aam
   taken from nnkosu.p -> also nnocko* uses


                    26.02.03/aam DDError
                    11.04.03/aam POrder, DivMess
                    13.04.05/aam ErrorTxt  
                    09.03.07/aam ErrorCode, ExtInvId
*/


DEF TEMP-TABLE ttPayment
   FIELD AccDate   AS Date
   FIELD PaymDate   AS Date
   FIELD RefNum     AS C
   FIELD Inv       AS INT
   FIELD ExtInvID  AS CHAR
   FIELD ArchiveId AS C
   FIELD AmtPaid   AS DEC
   FIELD ocr-acct  AS I
   FIELD origin    AS C
   FIELD fname     AS C
   FIELD CustNum    AS I
   FIELD Voucher    AS I
   FIELD double    AS LOGIC INIT false
   FIELD Interest  AS DEC  /* Interest */
   FIELD CustName  AS C
   FIELD BankAcc   AS C
   FIELD memo      AS C
   FIELD ClaimCost AS DEC
   FIELD DDError   AS CHAR
   FIELD ErrorCode AS CHAR
   FIELD POrder    AS DEC
   FIELD DivMess   AS CHAR
   FIELD ErrorTxt  AS CHAR
   INDEX Voucher Voucher AccDate
   INDEX Inv     Inv
   INDEX CustNum CustNum RefNum.

