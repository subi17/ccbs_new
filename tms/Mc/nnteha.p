 /* ------------------------------------------------------
  MODULE .......: NNTEHA.P
  FUNCTION .....: Tekstin haku
  APPLICATION ..: NN
  AUTHOR .......: PT
  CREATED ......: 28.04.96
  changePVM ....: 12.09.03/aam brand
  Version ......: M15
  SHARED .......: INPUT     NRO    tekstin numero
        INPUT     KIELI  kielikoodi  1 ... 9
        OUTPUT    TEKSTI teksti ko. kielellA
        OUTPUT    RC     paluukoodi
               0: OK
               1: ei ao. kielellA, annetaan suomeksi
               2: ei lOydy millAAn kielellA

  ------------------------------------------------------ */

  {commali.i}

  DEF INPUT  PARAMETER nro    AS INT  NO-UNDO.
  DEF INPUT  PARAMETER kieli  AS INT  NO-UNDO.
  DEF OUTPUT PARAMETER teksti AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER rc     AS INT NO-UNDO.


  /* oletetaan ettA kaikki OK */
  rc = 0.
  teksti = "".

  /* etsitAAn halutulla kielellA */
  FIND HdrText where 
       HdrText.Brand = gcBrand AND
       te-nro = nro AND 
       te-kie = kieli no-lock no-error.
  IF AVAIL HdrText THEN ASSIGN rc = 0 teksti = te-text.

  /* ellei lOydy, haetaan suomenkielistA */
  ELSE DO:
     FIND HdrText where 
          HdrText.Brand = gcBrand AND
          te-nro = nro AND 
          te-kie = 1 no-lock no-error.
     IF AVAIL HdrText THEN DO:
   teksti = te-text.
   /* jos was found vain suomeksi */
   IF te-kie NE kieli THEN rc = 1.
     END.
     /* jos ei lOytynyt millAAn kielellA */
     ELSE rc = 2.
  END.

