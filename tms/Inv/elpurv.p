/* ------------------------------------------------------
  MODULE .......: ELPURV.P 
  TEHTAVA ......: Puhelinraporttiohjelmien automaattivalinta laskutuksessa
  SOVELLUTUS ...: NN
  TEKIJA .......: TT
  CREATED ......: 03.13.96
  changePVM ....: 11.05.98 kl => nnpura5 = moduli 4
                  04.12.01 ht epl
                  07.12.2001/aam edefine.i
                  29.04.2002 ht  parameter "" FOR nnpura2
                  14.03.2003/aam report 8
  Version ......: M15
  ------------------------------------------------------ */

{commali.i}
{utumaa.i}
{cparam2.i}
{edefine.i}

def input parameter asno1  as int  format "zzzzzz9"  NO-UNDO.
def input parameter asno2  as int  format "zzzzzz9"  NO-UNDO.
def input parameter pvm1   as Date format "99-99-99" NO-UNDO.
def input parameter pvm2   as Date format "99-99-99" NO-UNDO.
def input parameter tilak  as int  format "9"        NO-UNDO.
def input parameter moduli as int  format "9"        NO-UNDO.
DEF INPUT PARAMETER InvNum  LIKE InvSeq.InvNum         NO-UNDO.

DEF VAR rap_moduli AS CHAR NO-UNDO.

/* report 4 and 5 are the same */
IF moduli = 4 THEN moduli = 5. 

IF moduli > 7 THEN RETURN. 

rap_moduli = "nnpura" + STRING(moduli).


      IF moduli = 2 THEN 
      RUN value(rap_moduli) 
                 (0,
                  99999999,
                  pvm1,
                  pvm2,
                  1,       /* vain laskutetut puhelutapahtumat */
                  InvNum,
                  "",      /* CLI  */
                  TRUE,    /* epl-tulostus                     */
                  FALSE). 

      ELSE
      RUN value(rap_moduli) 
                 (0,
                  99999999,
                  pvm1,
                  pvm2,
                  1,       /* vain laskutetut puhelutapahtumat */
                  InvNum,
                  TRUE,  /* epl-tulostus                     */
                  FALSE).


