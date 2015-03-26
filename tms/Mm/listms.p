/* ----------------------------------------------------------------------
  MODULE .......: listms.p
  TASK .........: List all subscriber's services 
  APPLICATION ..: nn
  AUTHOR .......: pt
  CREATED ......: 16.08-1999
  CHANGED ......: 11.10.02 jr Removed BillLevel
                  13.12.04/aam SSDate for SubSer
                  25.01.06/jt DYNAMIC-FUNCTION("fDispCustName"
                              removed mobsub.first + lastname 
                              now customer.custname customer.lastname
  VERSION ......: M15
---------------------------------------------------------------------- */

{commali.i} 

{utumaa.i new}

ASSIGN tuni1 = "listms"
       tuni2 = "".

DEF INPUT PARAMETER  MsSeq      LIKE mobsub.MsSeq NO-UNDO.

DEF VAR mobno         AS C  NO-UNDO.
DEF VAR rw-c          AS I  NO-UNDO.
DEF VAR rw-x          AS I  NO-UNDO.
DEF VAR pg-c          AS I  NO-UNDO.
DEF VAR need-rows     AS I  NO-UNDO.
DEF VAR username      AS C  NO-UNDO.
DEF VAR lcCustName    AS C  NO-UNDO.

FIND mobsub  WHERE mobsub.MsSeq   = MsSeq         NO-LOCK.
FIND sim     WHERE sim.icc         = mobsub.icc     NO-LOCK.
FIND msisdn  WHERE msisdn.CLI    = mobsub.CLI   NO-LOCK.
FIND Customer WHERE Customer.CustNum  = mobsub.CustNum  NO-LOCK.

lcCustName = DYNAMIC-FUNCTION("fDispCustName" IN ghFunc1,
                               BUFFER Customer).

FIND imsi    WHERE imsi.IMSI      = mobsub.IMSI   NO-LOCK.
FIND BillTarg WHERE BillTarg.BillTarg   = mobsub.BillTarg  AND
                   BillTarg.CustNum  = mobsub.CustNum  NO-LOCK.

FORM HEADER
   FILL("=",116) FORMAT "x(116)"         skip
      ynimi  at 1  FORMAT "x(23)"
      "SERVICES OF MOBILE SUBSCRIPTION" at 41
      pvm    format "99.99.9999" TO 116 skip
      mobno  FORMAT "x(20)"  at 41
      "Page"            TO 112   
      pg-c format "zz9" TO 116   skip

   FILL("=",116) FORMAT "x(116)" skip(1)

   "Customer no. ...:"  at 5  Customer.CustNum     
   "Invoicing Target:"  at 60 BillTarg.BillTarg   SKIP

   "Customer name ..:"  at 5  lcCustName
   "User ...........:"  at 60 username FORMAT "x(30)" skip

   "                 "  at 5  Customer.PostOffice     
   "User ...........:"  at 60 username FORMAT "x(30)" skip

   "Address ........:"  at 5  Customer.Address     
   "ICC ............:"  at 60 mobsub.icc     skip

   "PostCode City ..:"  at 5  Customer.ZipCode 
                              Customer.PostOffice skip(1)    

   FILL("-",116) FORMAT "x(116)" skip
   "ServCode"          at  5
   "Service"           at 16
   "Date"              AT 67 
   "Value"             to 80
   "Parameter"         at 82
   FILL("-",116) FORMAT "x(116)" skip

WITH
   WIDTH 116 NO-BOX NO-LABEL FRAME hdr.

tila = true.
{utuloste.i return}

/* reformat the MSISDN NO. */

mobno = "0" + substr(mobsub.CLI,3,3) + " " + 
              substr(mobsub.CLI,6,2) + " " +
              substr(mobsub.CLI,8,2) + " " +
              substr(mobsub.CLI,10,2).

username =    Customer.CustName + " " + Customer.FirstName.

/******************************
* Printer stream 'tul' is     *
* now opened.  Let us start   *
* with page header.           *
******************************/

pg-c  = 1.
VIEW STREAM tul FRAME hdr.
rw-c = 14.

/**********************************************
* Now print a list of each individual service *
**********************************************/

FOR 
EACH  subser  OF mobsub  NO-LOCK,
FIRST servcom WHERE 
      ServCom.Brand   = gcBrand AND 
      ServCom.servcom = subser.servcom  NO-LOCK,

FIRST service NO-LOCK WHERE 
      Service.Brand   = gcBrand AND 
      service.Service = servcom.Service

BREAK
BY service.Service
BY subser.ServCom
BY SubSer.SSDate DESC:

   need-rows = 1.
   IF FIRST-OF(service.Service) THEN need-rows = need-rows + 2.

   if rw-c + need-rows >= skayt1 THEN DO:
      pg-c = pg-c + 1.
      PUT STREAM tul UNFORMATTED chr(12).
      VIEW STREAM tul FRAME hdr.
      rw-c = 14.
   END.

   IF FIRST-OF(service.Service) THEN DO:
      PUT STREAM tul UNFORMATTED
      skip(1)
      string(service.Service)  at 1
      " " 
      CAPS(service.SEname)
      skip.
      rw-c = rw-c + 2.
   END.   

   put stream tul unformatted
   subser.ServCom      at  5 format "x(10)"
   servcom.scname  + " " + fill(".",50)   
                       at 16 format "x(50)"
   SubSer.SSDate      AT 67 FORMAT "99-99-99"                    
   subser.ssstat      to 80 format "zzz9"
   subser.ssparam     at 82 
   skip.
   rw-c = rw-c + 1.

END.

/* Eject the last page */
PUT STREAM tul UNFORMATTED chr(12).

/* Close the printer stream */
tila = FALSE.
{utuloste.i}

PAUSE 0.
