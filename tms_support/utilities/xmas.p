{utilities/create_bono.i}
DEF VAR lcBaseDCEvent AS CHAR NO-UNDO.
DEF VAR lcDCEvent AS CHAR NO-UNDO.
DEF VAR lcDCName AS CHAR NO-UNDO.
DEF VAR lcMFFeeModel AS CHAR NO-UNDO.
DEF VAR lcBaseMFFeeModel AS CHAR NO-UNDO.
DEF VAR lcMFFeeModelName AS CHAR NO-UNDO.
DEF VAR lcFeeModel AS CHAR NO-UNDO.
DEF VAR lcBaseBillItem AS CHAR NO-UNDO.
DEF VAR lcBillItem AS CHAR NO-UNDO.
DEF VAR lcBillItemName AS CHAR NO-UNDO.
DEF VAR lcBaseMFBillItem AS CHAR NO-UNDO.
DEF VAR lcBaseFMItem AS CHAR NO-UNDO.
DEF VAR lcFMItemFeeModel AS CHAR NO-UNDO.
DEF VAR ldFMItemPrice AS DECIMAL NO-UNDO.

DEF VAR lcBillCode AS CHAR NO-UNDO.
DEF VAR ldaOldValidTo AS DATE NO-UNDO.
DEF VAR ldaValidFrom AS DATE NO-UNDO.
DEF VAR lcTrans_L1 AS CHAR NO-UNDO.
DEF VAR lcTrans_L2 AS CHAR NO-UNDO.
DEF VAR lcTrans_L3 AS CHAR NO-UNDO.
DEF VAR lcTrans_L5 AS CHAR NO-UNDO.
DEF VAR lcMFTrans_L1 AS CHAR NO-UNDO.
DEF VAR lcMFTrans_L2 AS CHAR NO-UNDO.
DEF VAR lcMFTrans_L3 AS CHAR NO-UNDO.
DEF VAR lcMFTrans_L5 AS CHAR NO-UNDO.
DEF VAR lcBaseMFRepTextItem AS CHAR NO-UNDO.
DEF VAR lcBaseRepTextItem AS CHAR NO-UNDO.
DEF VAR lcBrand AS CHAR NO-UNDO.
DEF VAR lcFeeName AS CHAR NO-UNDO.
DEF VAR lcBIName AS CHAR NO-UNDO.
DEF VAR lcBIMFName AS CHAR NO-UNDO.
DEF VAR ldFMAmount AS DEC NO-UNDO.
DEF VAR liUpdateMode AS INT NO-UNDO.
DEF VAR lcMFRepTextItem AS CHAR NO-UNDO.
DEF VAR lcRepTextItem AS CHAR NO-UNDO.
DEF VAR ldDataLimit AS DEC NO-UNDO.
DEF VAR lcClitypeList AS CHAR NO-UNDO.
DEF VAR llError AS LOG NO-UNDO.
DEF VAR lcServComParam AS CHAR NO-UNDO.
DEF VAR lcServLimitName AS CHAR NO-UNDO.
DEF VAR lcBaseDP AS CHAR NO-UNDO.
DEF VAR lcDP AS CHAR NO-UNDO.
DEF VAR lcDPName AS CHAR NO-UNDO.
DEF VAR lcDeactSMS AS CHAR NO-UNDO.
DEF VAR lcBaseIdName AS CHAR NO-UNDO.
DEF VAR lcIdName AS CHAR NO-UNDO.
DEF VAR ldLimitUnShaped AS DEC NO-UNDO.
DEF VAR ldLimitShaped AS DEC NO-UNDO.
DEF VAR liDCStatus AS INT NO-UNDO.
DEF VAR lcBaseTariff AS CHAR NO-UNDO.
DEF VAR lcTariff AS CHAR NO-UNDO.

liUpdateMode = {&SIMULATERUN}. /*0 = test mode, no DB writing. */
 liUpdateMode = {&MODIFYDB}.  /* 1 = real mode, add to DB. */

/*references to source entries that are copyyed for modifications*/
lcBrand = "1".
/*DayCampaign*/
lcBaseDCEvent = "DATA6".
lcDCEvent = "DATA7".
lcDCNAme = "Bono 3 GB".
liDCStatus = 3. /* 0=Inactive, 1=Active, 2=Retired, 3=Hidden */
lcBaseIdName = "BONO6".
lcIdName = "BONO7".
lcBaseTariff = "BONO6_1".
lcTariff = "BONO25".
ldLimitUnShaped = 3221225472.
ldLimitShaped = 161061274.
ldDataLimit = 3072.
/*ldaOldValidTo = 11/30/2015.*/
ldaVAlidFrom = 12/01/2015.
lcServComParam = "#CLITYPE#BONO7".
lcServLimitName = "Mobile Data Usage Bundle 7".
lcDPName = "Bono 3 GB discount".
lcDP = "BONO7DISC".
lcBaseDP = "BONO6DISC".
lcDeActSMS = "Yoigo info: Has pedido cancelacion de Bono 3 GB Internet de Contrato. Desde el dia 1 del mes prox no estara activo.".

/*FeeModel*/
lcBaseMFFeeModel = "DATA6MF".
lcMFFeemodel = "DATA7MF".
/*to which bundle is allowed to activate*/
lcClitypeList = "CONT9,CONTD9,CONT15,CONT23,CONT24,CONTF10,CONTF11,CONTF20,CONTF20D,CONTF30,CONTF40,CONTF55,CONTF8,CONTFF2,CONTS12,CONTS15,CONTS16,CONTS20,CONTS21,CONTS25,CONTS26,CONTS30,CONTS32,CONTS35,CONTS39,CONTSF10,CONTSF14,LADEL2,LADEL4". 
lcBaseMFRepTextItem = lcBaseMFFeeModel.
lcBaseRepTextItem = lcBaseMFFeeModel.
lcMFRepTextItem = lcMFFeemodel.
lcRepTextItem = lcDCEvent.
lcFeename = lcdcEvent + " monthly fee".
lcBIName = "Data 7 national GPRS".
lcBIMFName = "Bono 7 Package price".
ldFMAmount = 5.79.
/*lcTrans_L1 = "Consumido entre todas la líneas".
lcTrans_L2 = "Consumit entre totes les línies".
lcTrans_L3 = "Linea guztien artean kontsumitua".
lcTrans_L5 = "Spend on all lines".*/
lcMFTrans_L1 = "Bono 3 GB".
lcMFTrans_L2 = "Bono 3 GB".
lcMFTrans_L3 = "Bono 3 GB".
lcMFTrans_L5 = "Bono 3 GB".
lcTrans_L1 = "Datos".
lcTrans_L2 = "Dates".
lcTrans_L3 = "Datuak".
lcTrans_L5 = "Data".

llError = fcreateRepText(lcBaseMFRepTextItem, lcMFRepTextItem, lcMFTrans_L1, 
               ldaVAlidFrom, 1, liUpdateMode).
llError = fcreateRepText(lcBaseMFRepTextItem, lcMFRepTextItem, lcMFTrans_L2,
               ldaVAlidFrom, 2, liUpdateMode).
llError = fcreateRepText(lcBaseMFRepTextItem, lcMFRepTextItem, lcMFTrans_L3,
               ldaVAlidFrom, 3, liUpdateMode).
llError = fcreateRepText(lcBaseMFRepTextItem, lcMFRepTextItem, lcMFTrans_L5,
               ldaVAlidFrom, 5, liUpdateMode).
IF llError THEN MESSAGE "RepText ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "RepText error" VIEW-AS ALERT-BOX.

llError = fcreateRepText(lcBaseRepTextItem, lcRepTextItem, lcTrans_L1, 
                         ldaVAlidFrom, 1, liUpdateMode).
llError = fcreateRepText(lcBaseRepTextItem, lcRepTextItem, lcTrans_L2, 
                         ldaVAlidFrom, 2, liUpdateMode).
llError = fcreateRepText(lcBaseRepTextItem, lcRepTextItem, lcTrans_L3, 
                         ldaVAlidFrom, 3, liUpdateMode).
llError = fcreateRepText(lcBaseRepTextItem, lcRepTextItem, lcTrans_L5, 
                         ldaVAlidFrom, 5, liUpdateMode).
IF llError THEN MESSAGE "RepText2 ready" VIEW-AS ALERT-BOX.               
ELSE  MESSAGE "RepText2 error" VIEW-AS ALERT-BOX.

llError = fcreateBillItem (lcBaseDCEvent, lcDCEvent, lcBIName, liUpdateMode).

IF llError THEN MESSAGE "BillItems ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "BillItems error" VIEW-AS ALERT-BOX.

llError = fcreateBillItem (lcBaseMFFeeModel, lcMFFeeModel, lcBIMFName, liUpdateMode).

IF llError THEN MESSAGE "BillItems ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "BillItems error" VIEW-AS ALERT-BOX.

llError = fcreateFeeModel (lcBaseMFFeeModel, lcMFFeemodel, lcFeename, 
                           liUpdateMode).

IF llError THEN MESSAGE "Feemodel ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "FeeModel error" VIEW-AS ALERT-BOX.

llError = fcreateDayCampaign (lcBaseDCEvent, lcDCEvent, lcMFFeemodel, lcDCName,
                              ldaVAlidFrom, liDCStatus, liUpdateMode).

IF llError THEN MESSAGE "DayCampaign ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "DayCampaign error" VIEW-AS ALERT-BOX.

llError = fcreateFMItem (lcBaseMFFeeModel, lcMFFeemodel, ldaVAlidFrom, 
                         ldFMAmount, liUpdateMode).
IF llError THEN MESSAGE "FMItem ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "FMItem error" VIEW-AS ALERT-BOX.

llError = fcreateDiscountPlan (lcBaseMFFeeModel, lcMFFeemodel, ldaVAlidFrom, 
                           lcBaseDp, lcDp, lcDpName, ldFMAmount, liUpdateMode).
IF llError THEN MESSAGE "Discountplan ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "Discountplan error" VIEW-AS ALERT-BOX.

/*
llError = faddMatrixValue (lcBaseDCEvent, lcDCEvent, liUpdateMode).
IF llError THEN MESSAGE "Matrix ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "Matrix error" VIEW-AS ALERT-BOX.
*/

llError = fcreateServiceLimit (lcBaseDCEvent, lcDCEvent, ldaVAlidFrom, 
                               ldDataLimit, lcservLimitName, liUpdateMode).
IF llError THEN MESSAGE "Servicelimit ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "Servicelimit error" VIEW-AS ALERT-BOX.

llError = fcreateProgLimit (lcBaseDCEvent, lcDCEvent, ldaVAlidFrom, ldDataLimit,
                            liUpdateMode).
IF llError THEN MESSAGE "Proglimit ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "Proglimit error" VIEW-AS ALERT-BOX.

llError = fcreateSLGAnalyse (lcBaseDCEvent, lcDCEvent, ldaVAlidFrom, 
                             "" /*lcClitypeList*/, liUpdateMode).      
IF llError THEN MESSAGE "SLGAnalyse ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "SLGAnalyse error" VIEW-AS ALERT-BOX.

llError = faddTMSParam(lcBaseDCEvent, lcDCEvent, liUpdateMode).
IF llError THEN MESSAGE "TMSParam ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "TMSParam error" VIEW-AS ALERT-BOX.

llError = fcreateBDest(lcBaseDCEvent, lcDCEvent, ldaVAlidFrom, liUpdateMode).
IF llError THEN MESSAGE "BDest ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "BDest error" VIEW-AS ALERT-BOX.

llError = fcreateDCService(lcBaseDCEvent, lcDCEvent, ldaVAlidFrom, 
                           lcServComParam, liUpdateMode).
IF llError THEN MESSAGE "DCService ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "DCService error" VIEW-AS ALERT-BOX.

llError = faddRequestActionRules(lcBaseDCEvent, lcDCEvent, ldaVAlidFrom,
                                   lcDeActSMS, liUpdateMode).
IF llError THEN MESSAGE "RequestActionRule ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "RequestActionRule error" VIEW-AS ALERT-BOX.

llError = fcreateTariff(lcBaseDCEvent, lcDCEvent, ldaVAlidFrom, liUpdateMode).
IF llError THEN MESSAGE "Tariff ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "Tariff error" VIEW-AS ALERT-BOX.

llError = fcreateShaperConf(lcBaseIdName, lcIdName, ldLimitUnShaped, 
                            ldLimitShaped, lcClitypeList, lcBaseTariff,
                            lcTariff, liUpdateMode).
IF llError THEN MESSAGE "ShaperConf ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "ShaperConf error" VIEW-AS ALERT-BOX.

llError = fcreateTMSCodes(lcBaseDCEvent, lcDCEvent,
                          lcBaseIdName, lcIdName, liUpdateMode).
IF llError THEN MESSAGE "TMSCodes ready" VIEW-AS ALERT-BOX.
ELSE  MESSAGE "TMSCodes error" VIEW-AS ALERT-BOX.
