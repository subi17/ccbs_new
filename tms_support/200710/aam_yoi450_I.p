{testpaa.i}
katun = "ari".

{timestamp.i}

def var i       as int  no-undo.
def var j       as int  no-undo.
def var k       as int  no-undo.
def var ldtdate as date no-undo.
def var litime  as int  no-undo.
def var licust  as int  no-undo.
def var lcvouch as char no-undo.
def var ldperc  as dec  no-undo.
def var limonth as int  no-undo.
def var ldtacc  as date no-undo.

DEF VAR ldPosting  AS DEC  NO-UNDO EXTENT 10.
DEF VAR liAccount  AS DEC  NO-UNDO EXTENT 10.

ldtacc = 10/15/7.

def buffer bpaym for payment.

for each prepaidrequest no-lock where 
         prepaidrequest.brand  = "1"       and
         prepaidrequest.source = "mincons" and
         round(prepaidrequest.vatamt / 100,2) ne 0 and 
         prepaidrequest.tsrequest > 20070701:

    if prepaidrequest.vatamt / prepaidrequest.topupamt < 0 then do:

       i = i + 1.
       
       fsplitts(prepaidrequest.tsrequest,
                output ldtdate,
                output litime).
                
       assign
          licust  = 0
          lcvouch = "".
       
       find first payment use-index refnum where
                  payment.brand   = "1"               and
                  payment.refnum  = string(pprequest) and
                  payment.accdate = ldtdate
       no-lock no-error.
       if available payment then do:
       
          if can-find(first bpaym use-index refnum where
                            bpaym.brand   = "1" and
                            bpaym.refnum  = payment.refnum and
                            bpaym.accdate >= 10/12/7)
          then next.
          
          ASSIGN 
             ldPosting[2] = 2 * round(prepaidrequest.vatamt / 100,2)
             ldPosting[1] = -1 * ldposting[2]
             liAccount[2] = payment.accnum[3]
             liAccount[1] = payment.accnum[1].

          /*  
          disp payment.posting[1 for 3] payment.paymsrc skip
               payment.acctype[1 for 3] skip
               payment.accnum[1 for 3] format ">>>>>>>9" skip
               ldposting[1 for 2] skip
               liaccount[1 for 2] format ">>>>>>>9" skip(1).
          */

          RUN createpaym (payment.CustNum,
                          payment.InvNum,
                          prepaidrequest.CLI,
                          ldtacc,
                          ldtacc,
                          ldPosting,
                          liAccount,
                          "X" + payment.paymsrc,
                          payment.PaymType,
                          payment.Refnum,
                          "Correction to tax posting on voucher " + 
                             payment.extvoucher,
                          OUTPUT k).
          
          if k > 0 then 
          j = j + 1.                              
       end.
    end.
   
    pause 0.
    disp i j prepaidrequest.tsrequest with 1 down.

end.

disp i j.





