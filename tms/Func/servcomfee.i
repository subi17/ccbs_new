/* servcomfee.i     30.12.04/aam rewritten 

   mobsub should be in buffer
*/

&IF "{&SERVCOMFEE_I}" NE "YES"
&THEN

&GLOBAL-DEFINE SERVCOMFEE_I YES

{Func/setfees.i}

/* fee for opening an service */
FUNCTION fServiceOpenFee RETURNS LOGICAL
   (icFeeModel AS CHAR,
    idtBegDate AS DATE,
    icMemo     AS CHAR,
    icContract AS CHAR,
    icUserCode AS CHAR,
    icFeeMemo AS CHAR):

   fMakeSetfees (icFeeModel,
                 MobSub.CustNum,
                 MobSub.MSSeq,
                 MobSub.BillTarget,  
                 "MSSeq" + STRING(MobSub.MsSeq),   
                 icMemo,             /* memo     */
                 YEAR(idtBegDate) * 100 + MONTH(idtBegDate),
                 idtBegDate,
                 ?,                /* price from feemodel */
                 icContract,       /* contract */
                 icUserCode,
                 icFeeMemo,
                 0,
                 "",
                 "").

END FUNCTION.

/* fee for closing an service */
FUNCTION fServiceChangeFee RETURNS LOGICAL
   (icFeeModel AS CHAR,
    idtDate    AS DATE,
    icMemo     AS CHAR,
    icContract AS CHAR,
    icUserCode AS CHAR,
    icFeeMemo AS CHAR):

   fMakeSetfees (icFeeModel,
                 MobSub.CustNum,
                 MobSub.MSSeq,
                 MobSub.BillTarget,  
                 "MSSeq" + STRING(MobSub.MsSeq),   
                 icMemo,             /* memo     */
                 YEAR(idtDate) * 100 + MONTH(idtDate),
                 idtDate,
                 ?,                /* price from feemodel */
                 icContract,       /* contract */
                 icUserCode,
                 icFeeMemo,
                 0,
                 "",
                 "").

END FUNCTION.

&ENDIF