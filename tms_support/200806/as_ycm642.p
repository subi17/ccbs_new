def stream sin.
input stream sin from /apps/snet/200806/Addr_postpago_FINAL.txt. 

def stream soutskipped.
output stream soutskipped to /apps/snet/200806/as_ycm642_skipped.log.

def stream soutnewaddr.
output stream soutnewaddr to /apps/snet/200806/as_ycm642_changes.log append.

def stream soutexcl.
output stream soutexcl to /apps/snet/200806/as_ycm642_nothandled.log append.

def var lcLine AS CHARACTER NO-UNDO.
DEFINE VARIABLE liCustNum AS INTEGER NO-UNDO. 
DEFINE VARIABLE lcCurrentAddress AS CHAR NO-UNDO. 
DEFINE VARIABLE lcCurrentZipCode AS CHAR NO-UNDO. 
DEFINE VARIABLE lcCurrentPostOffice AS CHAR NO-UNDO. 
DEFINE VARIABLE lcCurrentCountry AS CHAR NO-UNDO. 
   
   
DEFINE VARIABLE lcTipovia AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcParticulas AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcCalle AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcNexo AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcResto AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcNumero AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcLocalidad AS CHARACTER NO-UNDO. 
DEFINE VARIABLE lcCodigoPostal AS CHARACTER NO-UNDO.
DEFINE VARIABLE lcProvincia AS CHARACTER NO-UNDO.

DEFINE VARIABLE lcNewAddress AS CHARACTER NO-UNDO. 

/* skip 1st line */
import stream sin unformatted lcLine.

repeat:

   import stream sin unformatted lcLine.

   liCustnum = int(entry(1,lcLine,"|")).
   lcCurrentAddress = entry(2,lcLine,"|").
   lcCurrentZipCode = entry(3,lcLine,"|").
   lcCurrentPostOffice = entry(4,lcLine,"|").
   lcCurrentCountry = entry(5,lcLine,"|").
   
   FIND FIRST customer where
      customer.custnum = liCustnum and
      customer.address = lcCurrentAddress and
      customer.postoffice = lcCurrentPostoffice and
      customer.country = lcCurrentCountry and
      customer.zipcode = lcCurrentZipCode NO-LOCK NO-ERROR.
   
   if not avail customer then do: 
      put stream soutexcl unformatted lcLine skip.
      next.
   end.

   lcTipovia = entry(6,lcline,"|").
   lcParticulas = entry(7,lcline,"|").
   lcCalle = entry(8,lcline,"|").
   lcNexo = entry(9,lcline,"|").
   lcNumero = entry(10,lcline,"|").
   lcResto = entry(11,lcline,"|").
   lcLocalidad = entry(12,lcline,"|").
   lcCodigoPostal = entry(13,lcline,"|").
   lcProvincia = entry(14,lcline,"|").

   lcNewAddress = lcTipovia.
   
   IF lcParticulas NE "" then 
      lcNewAddress = lcNewAddress + " " + lcParticulas.
    
   IF lcCalle NE "" then 
      lcNewAddress = lcNewAddress + " " + lcCalle.

   IF lcNexo NE "" then 
      lcNewAddress = lcNewAddress + " " + lcNexo.

   IF lcNumero NE "" then 
      lcNewAddress = lcNewAddress + " " + lcNumero.
   
   IF lcResto NE "" then 
      lcNewAddress = lcNewAddress + " " + lcResto.
/*   
   FIND FIRST region where rgname = lcProvincia NO-LOCK NO-ERROR.

   if not avail region then do:
      put stream soutskipped unformatted liCustNum " " lcProvincia skip.
      next.
   END.
*/
 
   find current customer exclusive-lock no-error no-wait.
   if locked customer then do:
      put stream soutskipped unformatted lcLine skip.
      next.
   end.
   
   put stream soutnewaddr unformatted liCustnum "|"
   customer.address "|" customer.zipcode "|" customer.postoffice "|"
   lcNewAddress "|" lcCodigoPostal "|" lcLocalidad skip.

   assign
      customer.address    = lcNewAddress
      customer.postoffice = lcLocalidad
      customer.zipcode    = lcCodigoPostal.

end.

input stream sin close.
output stream soutskipped close.
output stream soutexcl close.
output stream soutnewaddr close.
