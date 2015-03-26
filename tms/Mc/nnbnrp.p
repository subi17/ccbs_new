/* ----------------------------------------------------------------------------
  MODULE .......: nnbnrp.p
  FUNCTION .....: List b-number prices FOR a Price list
  APPLICATION ..: NN
  AUTHOR .......: KL
  CREATED ......: 19-10-98
  MODIFIED .....: 22.10.02/aam use Tariff.Discount[6] etc.
                  12.09.03/aam brand
                  03.09.04/aam tuning,
                               MpmRid & ServRid added 
  Version ......: M15
  --------------------------------------------------------------------------- */


{excel.i}
{testpaa.i}

def var exPaymFile as c  no-undo format "x(30)".
DEF VAR i          AS i  NO-UNDO.
DEF VAR x          AS i  NO-UNDO.
def var PriceList  as c  no-undo init "".
def var PLName     LIKE  PriceList.PLName NO-UNDO.
def var ok         as lo no-undo format "Yes/No".
DEF VAR cpr        AS c  NO-UNDO.
def var bone       as lo no-undo format "Latest/All".
def var bMin       as lo no-undo format "Minute/Second" INIT TRUE.
DEF VAR lcNumeric  AS C  NO-UNDO.

form
   skip(1)
   "INSTRUCTION:  This module creates as tab separated ascii File of"
   "              all b-number prices and starting fees belonging to"
   "              the Price list determined below."                   skip(4)
   "              Price list ..:" PriceList
                  PLName AT 40 format "x(30)"                        SKIP
   "              All prices ..:" bone
      help "Print (L)atest prices / (A)ll prices"       skip(1)
   "              File Name ...:" exPaymFile                           skip(4)
with centered width 80 no-label title "B-number prices" FRAME frm.

DO FOR TMSUser:
   FIND FIRST TMSUser no-lock where
              TMSUser.UserCode = katun.
   exPaymFile = TMSUser.RepDir + "/bnrprice.txt".
END.

CRIT:
repeat WITH FRAME frm:

   HIDE MESSAGE no-pause.
   ehto = 9. RUN ufkey.
   UPDATE 
      PriceList 
      bone
      exPaymFile 
   WITH FRAME frm EDITING:
      READKEY.
      IF LOOKUP(KEYLABEL(LASTKEY),poisnap) > 0 THEN DO:
         IF lookup(frame-field,"PriceList") > 0 THEN DO:
            FIND FIRST PriceList WHERE 
                       PriceList.Brand  = gcBrand AND
                       PriceList.PriceList = INPUT PriceList 
            NO-LOCK NO-ERROR.

            IF AVAIL PriceList OR INPUT PriceList = "ALL" THEN DO:
               
               IF avail pricelist THEN DISP PriceList.PLName @ PLName.
               ELSE                    DISP "ALL"            @ PLName.
            END.
            ELSE DO:
               MESSAGE "Unknown Pricelist !".
               NEXT-PROMPT PriceList.
               NEXT.
            END.
         END.
      END.
      APPLY LASTKEY.
   END.


task:
   repeat WITH FRAME frm:
      ASSIGN ufk = 0 ufk[1] = 7 ufk[5] = 63 ufk[8] = 8 ehto = 0.
      RUN ufkey.
      IF toimi = 1 THEN NEXT  CRIT.
      IF toimi = 8 THEN LEAVE CRIT.

      IF toimi = 5 THEN DO:
         ok = FALSE.
         message "Are you SURE you want to start processing (Y/N) ?" UPDATE ok.
         IF ok THEN LEAVE task.
      END.
   END.

   ASSIGN lcNumeric = SESSION:NUMERIC-FORMAT
          SESSION:NUMERIC-FORMAT = "European".

   OUTPUT STREAM excel TO value(exPaymFile).

   message "Printing prices ...".

   FIND FIRST PriceList no-lock where
              PriceList.Brand  = gcBrand AND
              PriceList.PriceList = PriceList NO-ERROR.

   /* header texts */
   IF AVAIL PriceList THEN 
   PUT STREAM excel UNFORMATTED
   "B-number prices for pricelist: " + PriceList.PriceList + " - " + 
                                      PriceList.PLName  my-nl.
   ELSE "B-number prices for ALL pricelists ".                                   
   put stream excel unformatted "Price and Unit".

   PUT STREAM excel UNFORMATTED
      my-nl my-nl
      "PL code"          tab
      "CCN"              tab
      "Name"             tab
      "BDest"            tab 
      "Valid from"       tab
      "St-fee1 / price1" tab
      "St-fee2 / price2" tab
      "St-fee3 / price3" tab
      "St-fee4 / price4" tab
      "St-fee5 / price5" tab
      "St-fee we / Price we" tab
      "Call RepCode"             tab
      "Service RepCode"            
      my-nl.

   FOR EACH  Tariff no-lock where
             Tariff.Brand     = gcBrand AND
             (IF   pricelist    = "ALL" THEN TRUE 
              ELSE Tariff.PriceList = PriceList)
   BREAK
      BY Tariff.CCN               
      BY Tariff.BDest
      BY Tariff.ValidFrom DESC:

   IF (bone AND NOT first-of(Tariff.BDest)) THEN NEXT.

      IF Tariff.CCN > 0 THEN 
      FIND FIRST CCN NO-LOCK WHERE
             CCN.Brand = gcBrand AND
             CCN.CCN   = Tariff.CCN NO-ERROR.

      /* Price data */
      PUT STREAM excel UNFORMATTED
         Tariff.PriceList tab
         Tariff.CCN    tab
         (IF Tariff.CCN > 0 AND AVAILABLE CCN 
          THEN CCN.CCNName
          ELSE "")     tab
         Tariff.Bdest  tab
         Tariff.ValidFrom format "99-99-99" tab.

      /* starting fees 1 - 5: kr / Minutes */
      DO i = 1 TO 6.
         ASSIGN
            cpr = string(Tariff.StartCharge[i],"zz9.999").
         PUT STREAM excel UNFORMATTED
            cpr tab.
      END.

      PUT STREAM excel UNFORMATTED 
         my-nl tab tab tab tab tab.

      /* prices 1 - 5: ore/second OR kr/min */
      DO i = 1 TO 6.
         FIND FIRST tmscodes WHERE 
                    tmscodes.TableName = "TAriff"    AND 
                    tmscodes.FieldName = "DataType"  AND 
                    tmscodes.CodeGroup = "Tariff"    AND 
                    tmscodes.codevalue = STRING(tariff.datatype) 
         NO-LOCK NO-ERROR.
         
         IF avail TMSCOdes then cpr =  string(Tariff.Price[i],"zz9.99999")
                                       + " " + Tmscodes.Codename.
         ELSE  cpr =  string(Tariff.Price[i],"zz9.99999").                              
         PUT STREAM excel UNFORMATTED
            cpr tab.
      END.

      PUT STREAM excel UNFORMATTED 
          Tariff.MPMRid  tab
          Tariff.ServRid 
          my-nl.

   END.

END.

OUTPUT STREAM excel CLOSE.

SESSION:NUMERIC-FORMAT = lcNumeric.

