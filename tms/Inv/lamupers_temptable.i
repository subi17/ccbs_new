/* lamupers_temptable.i   
*/


DEF TEMP-TABLE ttIR NO-UNDO {&ttReference} LIKE InvRow 
   FIELD MsSeq  AS INT 
   FIELD VatIncl AS LOG
   /* dnet & dgross are used to calculate sums first without rounding */
   FIELD dnet   AS DEC
   FIELD dgross AS DEC
   FIELD AgrCust AS INT 
   FIELD TaxClass AS CHAR
   FIELD DiscBase AS DEC 
   FIELD DiscQty AS INT
   FIELD DPId AS INT 
   FIELD CCN AS INT
   FIELD Period AS INT
   FIELD ITGroupID AS INT
   FIELD ITGDelType AS INT
   INDEX BillCode BillCode CLI CCN Period
   INDEX MsSeq MsSeq AgrCust
   INDEX CCN CLI CCN
   INDEX ITGroupID ITGroupID
   INDEX ToDate MsSeq ToDate.

DEF TEMP-TABLE ttCLI NO-UNDO {&ttReference} LIKE InvASub 
   FIELD VatIncl AS LOG
   FIELD AmtWVat AS DEC 
   FIELD MsSeq   AS INT
   INDEX iccn  MsSeq InvSeq CLI CCN BillCode.

DEF TEMP-TABLE ttInvSeq NO-UNDO {&ttReference}
   FIELD InvSeq AS INT
   FIELD MsSeq  AS INT  
   FIELD AgrCust AS INT
   INDEX MsSeq MsSeq.
 
DEF TEMP-TABLE ttInvSplit NO-UNDO
   FIELD MsSeq      AS INT
   FIELD AgrCust    AS INT
   FIELD FromDate   AS DATE
   FIELD SplitDate  AS DATE
   FIELD ToDate     AS DATE
   FIELD CLIEvent   AS CHAR
   FIELD ITGroupID  AS INT  EXTENT 2
   FIELD ITGDelType AS INT  EXTENT 2
   FIELD InvSeq     AS INT  EXTENT 2
   INDEX MsSeq AgrCust MsSeq ToDate DESC.


