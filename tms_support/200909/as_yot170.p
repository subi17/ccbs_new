output to /apps/snet/200909/as_yot170.log.
FOR EACH invtext where
   invtext.brand = "1" and 
   target = "sms" and
   keyvalue = "corporateWelcome" exclusive-lock:

   export invtext.  
 
   case invtext.language:
      when 5 then
         invtext.invtext = "Yoigo info: Welcome. If you want to talk with us, call free to 633633336 from your Yoigo phone or from any other phone (cost depends on operator).".

      when 1 then invtext.invtext = "Yoigo info: Bienvenido. Si necesitas hablar con nosotros llama al 633633336 gratis desde tu Yoigo o desde otro telefono (coste segun operador).".

      when 2 then invtext.invtext = "Yoigo info: Benvingut. Si et cal parlar amb nosaltres truca gratis des del teu Yoigo al 633633336 o des d.un telèfon nacional (cost segons operador).". 
 
      when 3 then invtext.invtext = "Yoigo info: Ongietorri. Gurekin hitz egin behar baduzu dei doan zure Yoigo-tik edo beste telefono batetik 633633336ra (kostua operadorearen araberakoa).".
      
   end.

END.
