FUNCTION fCreateRepText RETURNS LOGICAL
   (INPUT iiTextType AS INT,
    INPUT icLinkCode AS CHAR,
    INPUT iiLanguage AS INT,
    INPUT icRepText  AS CHAR):

   FIND FIRST RepText WHERE RepText.Brand      = "1"        AND
                      RepText.TextType = iiTextType AND
                      RepText.LinkCode = icLinkCode AND
                      RepText.Language = iiLanguage AND
                      RepText.ToDate   > TODAY      NO-LOCK NO-ERROR.
   IF NOT AVAIL RepText THEN
   DO:
      CREATE RepText.
      ASSIGN
          RepText.Brand    = "1"
          RepText.TextType = iiTextType
          RepText.LinkCode = icLinkCode
          RepText.Language = iiLanguage
          RepText.FromDate = TODAY
          RepText.ToDate   = DATE(12,31,2049)
          RepText.RepText  = icRepText.
   END.

   RETURN TRUE.

END FUNCTION.

fCreateRepText(1, /*text type */
               "VOICE200B", /*LinkCode*/
               1, /*Language */
               "Nacionales"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200B", /*LinkCode*/
               2, /*Language */
               "Nacionals"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200B", /*LinkCode*/
               3, /*Language */
               "Nazionalak"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200B", /*LinkCode*/
               5, /*Language */
               "National"). /* text */


fCreateRepText(1, /*text type */
               "VOICE200BCF", /*LinkCode*/
               1, /*Language */
               "Desvío de Llamadas"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200BCF", /*LinkCode*/
               2, /*Language */
               "Desviament de Trucades"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200BCF", /*LinkCode*/
               3, /*Language */
               "Dei-desbideraketa"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200BCF", /*LinkCode*/
               5, /*Language */
               "Call forwarding"). /* text */


fCreateRepText(1, /*text type */
               "VOICE200BMF", /*LinkCode*/
               1, /*Language */
               "Bono 200 minutos"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200BMF", /*LinkCode*/
               2, /*Language */
               "Bono 200 minutos"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200BMF", /*LinkCode*/
               3, /*Language */
               "Bono 200 minutos"). /* text */
fCreateRepText(1, /*text type */
               "VOICE200BMF", /*LinkCode*/
               5, /*Language */
               "Bono 200 minutos"). /* text */












