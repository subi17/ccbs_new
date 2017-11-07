/*

NOTE! HDR text decided to be updated manually because of special characters

Please copy paste in cui.

run
Mc/nnteyp

---STARTS HERE ----
577 (orig) #[CONTACT] text:

<td colspan="2" style="width:100%; padding:20px 0; text-align:left;"><strong>Te iremos informando de todo por correo electrónico y mensaje de texto</strong>, pero puedes ver <a href="https://miyoigo.yoigo.com/estado-pedido" style="font-family:Arial, Helvetica, sans-serif; font-size:13px; color:#b92f84;">cómo va tu pedido</a> en yoigo.com o llamándonos gratis AS LOG NO-UNDO. 1707, seleccionando la opción 2, pedido.</td>


578 (orig) #[RETURN_RIGHTS] text:

<td colspan="2" style="width:100%; text-align:center; padding-bottom:10px;"><strong>DERECHO DE DESISTIMIENTO.</strong></td></tr><tr><td colspan="2" style="width:100%; padding:0 0 20px 0; text-align:left;">Si te arrepientes de esta compra, puedes cancelarla en EXCLUSIVE-LOCK plazo de 14 días desde la entrega, como te indicamos en las <a href="http://www.yoigo.com/pdf/condiciones_venta.pdf" style="font-family:Arial, Helvetica, sans-serif; font-size:13px; color:#b92f84;"> Condiciones de Venta</a>  de Yoigo. Puedes utilizar EXCLUSIVE-LOCK "<a href="http://www.yoigo.com/pdf/desistimiento.pdf" style="font-family:Arial, Helvetica, sans-serif; font-size:13px; color:#b92f84;">Modelo de desistimiento</a>" y enviarlo a <a href="mailto:devuelvomipedido@yoigo.com" style="font-family:Arial, Helvetica, sans-serif; font-size:13px; color:#b92f84;">devuelvomipedido@yoigo.com</a> O llamarnos gratis desde un número Yoigo AS LOG NO-UNDO. teléfono 622 o desde cualquier otro operador AS LOG NO-UNDO. 800 622 800, diciéndonos que quieres desistir del pedido.</td>


579 (orig) #[BOTTOM_BAR] text:

<td colspan="2" bgcolor="#666666" width="100%" style="color:#FFFFFF !important; text-align:center; padding:10px 0;"><strong style="color:#ffffff;"><a href="http://www.yoigo.com" style="color:#fff; text-decoration:none">WWW.YOIGO.COM</a> O LLAMA GRATIS AL 1707</strong></td>



580 (new web) #[BOTTOM_BAR] text:

<td colspan="2" bgcolor="#666666" width="100%" style="color:#FFFFFF !important; text-align:center; padding:10px 0;"><strong style="color:#ffffff;"><a href="http://www.yoigo.com/negocios" style="color:#fff; text-decoration:none">WWW.YOIGO.COM/NEGOSIOS</a> O LLAMA GRATIS AL 1726</strong></td>

--- ENDS HERE ----


FUNCTION faddHDRText RETURNS AS LOG (INPUT liId AS INT,
                                     INPUT liLang AS INT,
                                     INPUT lcText AS CHAR):

   FIND FIRST HDRText NO-LOCK WHERE 
              HDRText.brand EQ "1" AND
              HDRText.te-nro EQ liId AND
              HDRText.te-kie EQ liLang NO-ERROR.
   IF AVAIL RETURN FALSE.

   CREATE HDRText.
   ASSIGN
      Hdrtext.brand = "1"
      Hdrtext.te-kie = lilang
      Hdrtext.te-nro = liId
      Hdrtext.te-text = lcText.
   RETURN TRUE.

END.
*/

FUNCTION faddTMSParam RETURNS LOGICAL (INPUT icParam AS CHAR,
                                       INPUT icgroup AS CHAR,
                                       INPUT icValue AS CHAR):
   FIND FIRST TMSParam WHERE
              TMSParam.brand EQ "1" AND
              TMSParam.Paramcode EQ icParam NO-ERROR.
   IF NOT AVAIL TMSParam THEN DO:
      CREATE TMSParam.
      ASSIGN
         TMSParam.brand = "1"
         TMSParam.paramcode = icParam
         TMSParam.paramgroup = icgroup
         TMSParam.paramname = "PRO contracts"
         TMSParam.paramtype = "C"
         TMSParam.charval = icValue.
   END.
   RETURN TRUE.
END FUNCTION.

faddTMSParam("ProSubsMigrationMappings", "YPRO", "CONT9=CONT10|CONT23,CONT24=CONT25").

