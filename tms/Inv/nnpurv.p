/* ------------------------------------------------------
  MODULE .......: NNPURV.P
  TEHTAVA ......: Puhelinraporttiohjelmien automaattivalinta laskutuksessa
  SOVELLUTUS ...: NN
  TEKIJA .......: TT
  CREATED ......: 03.13.96
  changePVM ....: 11.05.98 kl => nnpura5 = moduli 4
                  04.11.01 ht epltul
                  07.12.2001/aam edefine 
                  29.04.2002 ht  parameter "" FOR nnpura2
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{utumaa.i}
{cparam2.i}
{edefine.i new}

def input parameter asno1  as int  format "zzzzzz9"  NO-UNDO.
def input parameter asno2  as int  format "zzzzzz9"  NO-UNDO.
def input parameter pvm1   as Date format "99-99-99" NO-UNDO.
def input parameter pvm2   as Date format "99-99-99" NO-UNDO.
def input parameter tilak  as int  format "9"        NO-UNDO.
def input parameter moduli as int  format "9"        NO-UNDO.
DEF INPUT PARAMETER InvNum  LIKE InvSeq.InvNum         NO-UNDO.
DEF INPUT PARAMETER epltul AS lo                     NO-UNDO.

DEF VAR rap_moduli AS CHAR NO-UNDO.
DEF VAR mspit1     AS INT  NO-UNDO.
DEF VAR mskayt1    AS INT  NO-UNDO.

/* report 4 and 5 are the same */
IF moduli = 4 THEN moduli = 5. 

IF moduli > 7 THEN RETURN. 

rap_moduli = "nnpura" + STRING(moduli).

      /* Laitetaan memoryin nykyisen tehosteen sivuarvot */
      ASSIGN
        mspit1  = spit1
        mskayt1 = skayt1.

      /* Haetaan raporttitehoste */
      FIND FIRST TMSPrinter where
                 TMSPrinter.PrinterId = TMSPrinter
      NO-LOCK NO-ERROR.
      if available TMSPrinter and TMSPrinter.Device ne "-" THEN DO:
         /* Haetaan raporttitehoste */
         FIND FIRST TMSRepCfg where 
            TMSRepCfg.RepName = rap_moduli AND 
            TMSRepCfg.UserCode  = "" 
         no-lock no-error.
         FIND FIRST PrintCodes where 
            PrintCodes.PrinterId = TMSRepCfg.PrinterId AND
            PrintCodes.Effect = TMSRepCfg.Effect 
         no-lock no-error.
         ASSIGN
            spit1  = PrintCodes.PageLength
            skayt1 = PrintCodes.AvailLines.

         PUT STREAM tul control PrintCodes.EffOn[2].
      END. 

      IF moduli = 2 THEN 
      RUN value(rap_moduli) 
                 (0,
                  99999999,
                  pvm1,
                  pvm2,
                  1,       /* vain laskutetut puhelutapahtumat */
                  InvNum,
                  "",      /* CLI  */
                  epltul,  /* epl-tulostus                     */
                  FALSE). 

      ELSE
      RUN value(rap_moduli) 
                 (0,
                  99999999,
                  pvm1,
                  pvm2,
                  1,       /* vain laskutetut puhelutapahtumat */
                  InvNum,
                  epltul,   /* epl-tulostus                     */
                  FALSE). 

      /* Lopetetaan raporttitehoste */
      if TMSPrinter.Device ne "-" AND AVAILABLE PrintCodes THEN 
      PUT STREAM tul control PrintCodes.EffOff[2].

      /* Laitetaan takaisin memoryssa olleen tehosteen sivuarvot */
      ASSIGN
        spit1  = mspit1
        skayt1 = mskayt1.


