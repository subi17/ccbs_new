 FUNCTION fCreateHeaTxt RETURNS CHAR
   ( iiLang AS INT,
     iiCode AS INT,
     icText AS CHAR
   ):

   FIND FIRST HdrText NO-LOCK WHERE
              HdrText.Brand EQ "1" AND
              HdrText.te-nro EQ iiCode AND
              HdrText.te-kie EQ iiLang NO-ERROR.
   IF AVAIL HdrText THEN DO:
      MESSAGE  "Text already found " + 
                STRING(iiCode) + " " + 
                STRING(iiLang) VIEW-AS ALERT-BOX.
      RETURN "Text already found " + STRING(iiCode) + " " + STRING(iiLang).        END.

   CREATE HdrText.
   ASSIGN
      HdrText.Brand = "1"
      HdrText.te-nro = iiCode
      HdrText.te-kie = iiLang
      HdrText.te-text = icText.

   RETURN "".

END.

DEF VAR lcRet AS CHAR NO-UNDO.

/*Old 528 is copied to 573 and part of the old text is removed
The text is for premium services*/

lcRet = fCreateHeaTxt(1,
                      573,
                      "Cuando llamas a un número Premium, te cobramos la suma de dos conceptos: el coste de una llamada según tu tarifa con Yoigo, como si llamaras a un fijo o móvil nacional, más el de la empresa a la que llamas por el servicio que te dan. En el detalle (Proveedor de Servicios/Destino) de esa llamada aparece el nombre del operador al que pertenece el número al que llamaste.
Cuando envías o recibes un mensaje Premium, Yoigo te cobra 10 céntimos (sin impuestos indirectos), el resto es lo que cobra la empresa a la que envías o de la que recibes el mensaje, por eso en la factura puede aparecer más de una línea. En el detalle (Proveedor de Servicios/Destino) aparecerá el CIF y el nombre de la empresa. Además puede que no coincida con el nombre que aparece, ya que puede ser la denominación social de la empresa y no el nombre comercial o marca.").

lcRet = fCreateHeaTxt(2,
                      573,
                      "Quan truques a un número Premium et cobrem la suma de dos conceptes: el cost d'una trucada segons la teva tarifa amb Yoigo, com si truquessis a un fix o mobil nacional, més el de l'empresa on truques pel servei que et donen. En el detall (Proveidor de Serveis / Destí) d'aquella trucada apareix el nom de l'operador a qui pertany el número on truques.
Quan envies o reps un missatge Premium, Yoigo et cobra 10 centims (sense impostos indirectes), la resta és el que cobra l'empresa on envies o des d'on reps el missatge, per aixo a la factura hi pot apareixer més d'una línia. En el detall (Proveidor de Serveis / Destí) hi apareixera el CIF i el nom de l'empresa. A més és possible que no coincideixi amb el nom que hi apareix, ja que pot ser la denominació social de l'empresa i no el nom comercial o marca.").

lcRet = fCreateHeaTxt(3,
                      573,
                      "Premium zenbaki batera deitzen duzunean, bi kontzepturen batuketa kobratzen dizugu:deiaren kostua, Yoigo-rekin duzun tarifan oinarrituta (finko edo mugikor nazional batera deituko bazenu bezala), eta deitzen duzun enpresaren kostua, eskaintzen dizuten zerbitzuarengatik. Dei horren xehetasunetan (Zerbitzu-hornitzailea / Helmuga) deitutako zenbakiari dagokion operadorearen izena agertzen da.
Premium mezuak bidaltzen edo jasotzen dituzunean, Yoigo-k 10 zentimo kobratzen dizkizu (zeharkako zergak kanpo) eta, gainerakoa, mezua bidaltzen edo jasotzen duzun enpresak kobratzen duen zenbatekoa da. Horregatik, fakturan lerro bat baino gehiago ager daiteke. Xehetasunetan (Zerbitzu-hornitzailea / Helmuga) enpresaren IFZa eta izena agertuko dira. Horrez gain, baliteke agertzen den izenarekin bat ez etortzea, enpresaren izen soziala izan daitekeelako, eta ez izen komertziala edo marka.").
                      

lcRet = fCreateHeaTxt(5,
                      573,
                      "When you call a Premium number, we charge you the total for two items: the cost of a call according to your Yoigo rate, as if you were calling a Spanish landline or mobile, plus that of the company you call for the service it provides. The breakdown (Destination/Service Provider) for this call shows the name of the operator to which the number you called belongs.
When you send or receive a Premium message, Yoigo charges you 10 cents (indirect taxes not included), and the rest is what the company to which you send it or which receives the message charges you, which is why there may be more than one line on your bill. The breakdown (Destination/Service Provider) shows the company's name and tax number. The name shown may not match, as this may be the company name rather than its trade or band name.").                      

/*Google billing headers*/
lcRet = fCreateHeaTxt(1,
                      574,
                      "Cuando pagas tu compra de Contenidos de Google Play a través de Yoigo no se aplican impuestos al ser compras que has realizado directamente a Google. Tienes más información sobre las compras realizadas en wallet.google.com y play.google.com/store/account utilizando tu cuenta de Google.").

lcRet = fCreateHeaTxt(2,
                      574,
                      "Quan pagues la teva compra de continguts de Google Play a través de Yoigo no s.hi apliquen impostos ja que són compres que has fet directament a Google. Tens més informació sobre les compres fetes a wallet.google.com i play.google.com/store/account fent servir el teu compte de Google.").

lcRet = fCreateHeaTxt(3,
                      574,
                       "Google  Play edukiak erostean Yoigo-ren bidez ordaintzen duzunean, ez da zergarik aplikatuko, Google-ri zuzenean egin dizkiozun erosketak baitira. Google kontua erabilita egindako erosketei buruzko informazio gehiago duzu eskuragarri wallet.google.com eta play.google.com/store/account helbideetan.").

lcRet = fCreateHeaTxt(5,
                      574,
                       "When you pay for your purchase of Google Play Content through Yoigo no taxes are applied as they are purchases made directly from Google.
                      You can find further information about purchases you have made at wallet.google.com and play.google.com/store/account, using your Google account.").





