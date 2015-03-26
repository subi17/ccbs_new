/* ----------------------------------------------------------------------
  MODULE .......: ftopup.i
  TASK .........: topup handling
  CREATED ......: 29.11.06/aam 
  CHANGED ......: 22.05.07/aam icTaxZone
                  25.05.07 kl 997 into PPRequest
                  22.08.07/aam prefix 997 -> 97
                  02.10.07/aam icRequest
                  08.10.07/aam PPReqPrefix, MsSeq
  Version ......: yoigo
-------------------------------------------------------------------------- */

{commali.i}
{timestamp.i}

FUNCTION fCreateTopUpRequest RETURNS INTEGER
   (iiMsSeq      AS INT,
    icCLI        AS CHAR,
    icFunction   AS CHAR,
    icSource     AS CHAR,
    icRequest    AS CHAR,
    icPrefix     AS CHAR,
    icReference  AS CHAR,
    icTaxZone    AS CHAR,
    idActStamp   AS DEC,
    idTopUpAmt   AS DEC,    /* Vat 0% */
    idVatAmt     AS DEC):

   DEFINE VARIABLE liReturn    AS INTEGER   NO-UNDO.

   IF idActStamp = 0 OR idActStamp = ? THEN 
      idActStamp = fMakeTS().
      
   DO WHILE TRUE:
      liReturn = NEXT-VALUE(PrePaidReq).
      
      IF NOT CAN-FIND(FIRST PrePaidRequest WHERE
                            PrePaidRequest.Brand     = gcBrand AND
                            PrepaidRequest.PPRequest = liReturn)
      THEN LEAVE.
   END.
   
   CREATE PrepaidRequest.
   ASSIGN
      PrepaidRequest.Brand       = gcBrand
      PrepaidRequest.UserCode    = katun
      PrepaidRequest.PPRequest   = liReturn
      PrePaidRequest.PPReqPrefix = icPrefix
      PrePaidRequest.MsSeq       = iiMsSeq
      PrepaidRequest.CLI         = icCLI
      PrePaidRequest.CommLine    = icFunction
      PrepaidRequest.Request     = icRequest
      PrepaidRequest.Reference   = icReference
      PrepaidRequest.PPStatus    = 0
      PrepaidRequest.TopUpAmt    = idTopUpAmt
      PrepaidRequest.VatAmt      = idVatAmt
      PrepaidRequest.Source      = icSource
      PrePaidRequest.TaxZone     = icTaxZone.
      PrepaidRequest.TSRequest   = idActStamp.

   RELEASE PrepaidRequest.

   RETURN liReturn.
   
END FUNCTION.


FUNCTION fAddTopUp RETURNS INTEGER
   (iiMsSeq      AS INT,
    icCLI        AS CHAR,
    icFunction   AS CHAR,
    icSource     AS CHAR,
    icRequest    AS CHAR,
    icPrefix     AS CHAR,
    icTaxZone    AS CHAR,
    idTopUpAmt   AS DEC,    /* Vat 0% */
    idVatAmt     AS DEC):

   RETURN fCreateTopUpRequest(iiMsSeq,
                              icCLI,
                              icFunction,
                              icSource,
                              icRequest,
                              icPrefix,
                              "",
                              icTaxZone,
                              0,
                              idTopupAmt,
                              idVatAmt).
   
END FUNCTION.



