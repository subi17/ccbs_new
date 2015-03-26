def var i as int no-undo.

def stream slog.
output stream slog to /apps/snet/200708/aam_yts65_II.log append.

for each order exclusive-lock where
         order.brand = "1" and
         order.statuscode = "2" and
         order.crstamp > 20070801 and
         order.crstamp < 20070807 and
         order.cli  = ""        and
         order.orderchannel = "pos",
   first ordercustomer of order no-lock,
   FIRST msisdn exclusive-LOCK use-index pos WHERE 
         msisdn.statuscode = 1 AND 
         MSISDN.ValidTo >= 20070808 AND 
         MSISDN.POS = order.orderchannel:

      i = i + 1.
      
      assign 
         msisdn.statuscode = 2
         msisdn.orderid    = order.orderid
         order.cli         = msisdn.cli
         order.statuscode  = "1".

      put stream slog unformatted
         order.orderid  chr(9)
         order.msseq    chr(9)
         order.cli      chr(9)
         ordercustomer.custid  chr(9)
         ordercustomer.firstname + " " + ordercustomer.surname1 + " " + 
         ordercustomer.surname2 skip.
         
      
      pause 0.
      disp i order.orderid order.cli.
      
end.