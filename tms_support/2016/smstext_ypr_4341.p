/* EDIT SMS TEXT */

FUNCTION fEditSMSText RETURNS LOGICAL (
   INPUT icTarget AS CHAR,
   INPUT icKey    AS CHAR,
   INPUT idaStartDate  AS DATE,
   INPUT idaEndDate  AS DATE,
   INPUT iiLang   AS INT,
   INPUT icTitle   AS CHAR,
   INPUT icSMSText AS CHAR,
   INPUT iipos AS INT,
   INPUT iiPrint AS INT,
   INPUT icRule AS CHAR,
   INPUT iiClass AS INT,
   INPUT ilAllowEnding AS LOG):

FIND FIRST InvText WHERE
           InvText.Target EQ icTarget AND
           InvText.KeyValue EQ icKey AND
           InvText.ToDate >= idaStartDate NO-LOCK NO-ERROR.
IF AVAIL InvText AND NOT(ilAllowEnding) THEN DO:
   MESSAGE "Active SMS message found for " + icKey VIEW-AS ALERT-BOX.
   RETURN FALSE.
END.
ELSE DO:
   IF AVAIL InvText THEN 
      ASSIGN InvText.ToDate = idaStartDate - 1.
   CREATE InvText.
   ASSIGN InvText.ToDate = idaEndDate
          InvText.FromDate = idaStartDate
          InvText.Language = iiLang
          Invtext.Brand = "1"
          InvText.ITNum = NEXT-VALUE(it-seq)
          InvText.Key = icKey
          InvText.Target = icTarget
          InvText.InvText = icSMSText
          InvText.TxtTitle = icTitle
          InvText.Position = iipos
          InvText.report = iiprint
          InvText.sendRule = icRule
          InvText.LetterClass = iiclass.
   RELEASE InvText. 
END.
RETURN TRUE.
END FUNCTION.

fEditSmsText(
   "SMS",     /*  icTarget AS CHAR, */
   "InvDelivTypeChanged",      /*  icKey    AS CHAR, */
   06/29/16,      /*  idaStartDate  AS DATE, */
   12/31/49,      /*  idaEndDate  AS DATE, */
   1,             /*  iiLang   AS INT, */
   "Invoice delivery method has been changed by customer request",    /*  icTitle   AS CHAR, */
   "Yoigo info: Listo! Ya te hemos activado la factura electronica. Recuerda, siempre tendras tus ultimas 12 facturas en Mi Yoigo APP y en Mi Yoigo en yoigo.com",     /*  icSMSText AS CHAR, */
   1,           /*  iipos AS INT,  */
   1,           /*  iiPrint AS INT, */
   "R1",        /*  icRule AS CHAR, */
   0,           /*  iiClass AS INT, */
   FALSE).      /*  ilAllowEnding  */

