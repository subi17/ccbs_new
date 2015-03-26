/* refundfilett.i      05.09.07/aam
*/

DEF TEMP-TABLE ttRequest NO-UNDO
   FIELD MsRequest AS INT
   FIELD Printed   AS INT
   FIELD ZipCode   AS CHAR
   FIELD AccDate   AS DATE
   INDEX MsRequest IS UNIQUE MsRequest
   INDEX AccDate AccDate.

