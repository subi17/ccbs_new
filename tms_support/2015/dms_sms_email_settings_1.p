/* EDIT SMS TEXT */
/*kaaikas & ilsavola*/

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
   INPUT icCategory AS CHAR,
   INPUT ilAllowEnding AS LOG):

FIND FIRST InvText WHERE
           InvText.Target EQ icTarget AND
           InvText.KeyValue EQ icKey AND
           InvText.ToDate >= idaStartDate NO-ERROR.
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
          InvText.Category = icCategory
          InvText.LetterClass = iiclass.
   RELEASE InvText. 
END.
RETURN TRUE.
END FUNCTION.

fEditSmsText(
   "SMS",     /*  icTarget AS CHAR, */
   "GetConsultID",      /*  icKey    AS CHAR, */
   11/01/15,      /*  idaStartDate  AS DATE, */
   12/31/49,      /*  idaEndDate  AS DATE, */
   1,             /*  iiLang   AS INT, */
   "Get Consult Contract ID",    /*  icTitle   AS CHAR, */
   "Yoigo info: Este es tu Numero de Pedido: #INFO. Consulta el estado de tu solicitud. https://miyoigo.yoigo.com/estado-pedido/",     /*  icSMSText AS CHAR, */
   1,           /*  iipos AS INT,  */
   5,           /*  iiPrint AS INT, */
   "R1",        /*  icRule AS CHAR, */
   0,           /*  iiClass AS INT, */
   "SelfService", /*category*/
   TRUE).      /*  ilAllowEnding  */

fEditSmsText(
   "SMS",     /*  icTarget AS CHAR, */
   "GetMultiConsultID",      /*  icKey    AS CHAR, */
   11/01/15,      /*  idaStartDate  AS DATE, */
   12/31/49,      /*  idaEndDate  AS DATE, */
   1,             /*  iiLang   AS INT, */
   "Get Multiple Consult Contract ID",    /*  icTitle   AS CHAR, */
   "Yoigo info: Estos son tus Num. de Pedido: #INFO. Consulta el estado de tus solicitudes. https://miyoigo.yoigo.com/estado-pedido/",     /*  icSMSText AS CHAR, */
   1,           /*  iipos AS INT,  */
   5,           /*  iiPrint AS INT, */
   "R1",        /*  icRule AS CHAR, */
   0,           /*  iiClass AS INT, */
   "SelfService", /*category*/  
   TRUE).      /*  ilAllowEnding  */


fEditSmsText(
   "EMAIL",     /*  icTarget AS CHAR, */
   "GetConsultID",      /*  icKey    AS CHAR, */
   11/01/15,      /*  idaStartDate  AS DATE, */
   12/31/49,      /*  idaEndDate  AS DATE, */
   1,             /*  iiLang   AS INT, */
   "Tu numero de pedido de Yoigo",    /*  icTitle   AS CHAR, */
   "<html><body>Hola, <br /> <br /> Este es el número de pedido que tienes en trámite en Yoigo: <br /> #INFO <br /> <br /> Si quieres ver cómo va, entra en Mi Yoigo en <a href=~"https://miyoigo.yoigo.com/miyoigo-static/miyoigo/#consulta-ped ido~"> Consulta el estado de tu solicitud</a>. <br /> <br /> Saludos, <br />
Atención a clientes: <br /> www.yoigo.com <br /> Teléfono 1707 opción 2, si no eres cliente. <br /> Teléfono 622, si eres de Yoigo. <br />",     /*  icSMSText AS CHAR, */
   1,           /*  iipos AS INT,  */
   5,           /*  iiPrint AS INT, */
   "",        /*  icRule AS CHAR, */
   0,           /*  iiClass AS INT, */
   "", /*category*/  
   TRUE).      /*  ilAllowEnding  */

fEditSmsText(
   "EMAIL",     /*  icTarget AS CHAR, */
   "GetMultiConsultID",      /*  icKey    AS CHAR, */
   11/01/15,      /*  idaStartDate  AS DATE, */
   12/31/49,      /*  idaEndDate  AS DATE, */
   1,             /*  iiLang   AS INT, */
   "Tu numero de pedido de Yoigo",    /*  icTitle   AS CHAR, */
   "<html><body>Hola, <br /> <br /> Estos son los números de pedido que tienes en trámite en Yoigo: <br /> #INFO <br /> <br /> Si quieres ver cómo va, entra en Mi Yoigo en <a href=~"https://miyoigo.yoigo.com/miyoigo-static/miyoigo/#consulta-ped ido~"> Consulta el estado de tu solicitud</a>. <br /> <br /> Saludos, <br />
Atención a clientes: <br /> www.yoigo.com <br /> Teléfono 1707 opción 2, si no eres cliente. <br /> Teléfono 622, si eres de Yoigo. <br />",     /*  icSMSText AS CHAR, */
   1,           /*  iipos AS INT,  */
   5,           /*  iiPrint AS INT, */
   "",        /*  icRule AS CHAR, */
   0,           /*  iiClass AS INT, */
   "", /*category*/  
   TRUE).      /*  ilAllowEnding  */


/*
FOR EACH invtext NO-LOCK where
         invtext.keyvalue begins "Get" :
   DISP invtext.

END.   */


