/* -----------------------------------------------------------------
   MODULE .......: memodump.p
   TASK .........: dump memo's from any hosttable
   APPLICATION ..: TMS
   AUTHOR .......: mk
   CREATED ......: 14.09.04
   CHANGED ......: 13.01.06/aam Orgid from customer
                   22.11.06/aam OrderCustomer     
   Version ......: SHARK
   ----------------------------------------------------------------- */

{Syst/commali.i}
{Func/excel.i}
{Func/date.i}
{Func/timestamp.i}
{Func/cparam2.i}

DEF TEMP-TABLE ttMemo
   FIELD icnum    AS integer
   FIELD ccli     AS ch
   FIELD cmemo    as ch
   FIELD ctitle   as ch
   FIELD chetu    as ch
   FIELD msseq    as int.               

RUN Mm/memodumpper ("Mobsub","mobsubmemo").
EMPTY TEMP-TABLE ttMemo.
RUN Mm/memodumpper ("Order","ordermemo").


/* ----------------------------------------------------- */
PROCEDURE memodumpper:
DEFINE INPUT PARAMETER atablename AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER afilename AS CHARACTER  NO-UNDO.

def var numform     as c  no-undo.
def var lcDelim     as c no-undo.
def var lcspooldir  as ch no-undo.
def var lcoutdir    as ch no-undo.
def var lcdir       as ch no-undo.
def var lcfilename  as ch no-undo.
def var liToday     as integer no-undo.
def var timeto      as integer no-undo.
def var icounter    as integer no-undo init 0.

ASSIGN
    numform = session:numeric-format
    session:numeric-format = "AMERICAN"
    lcOutDir = fCParam("dumpoutgoing","memodump.p")
    lcSpoolDir = fCParam("dumpspool","memodump.p")
    lcDelim = "|"
    liToday = integer(fdatefmt(TODAY,"yyyymmdd"))
    lcFileName = afilename + fdatefmt(TODAY,"yyyymmdd") + ".dump"
    timeto = fmakets().

case atablename:
    when "order" THEN DO:
        for each memo no-lock where
            memo.brand = "1" and
            hosttable = "order":

            find first order no-lock where
                order.brand = "1" AND 
                order.orderid = int(memo.keyvalue) 
                use-index orderid no-error.
            if not avail order then next.

            FIND FIRST OrderCustomer NO-LOCK WHERE
                       OrderCustomer.Brand   = gcBrand       AND
                       OrderCustomer.OrderID = Order.OrderID AND
                       OrderCustomer.RowType = 1 NO-ERROR.
            
            CREATE ttMemo.
            ASSIGN
                ttMemo.icnum  = memo.custnum
                ttMemo.ccli   = order.cli
                ttMemo.chetu  = OrderCustomer.CustID WHEN AVAIL OrderCustomer
                ttMemo.ctitle = memo.memotitle
                ttMemo.cmemo  = memo.memotext
                ttMemo.msseq  = order.msseq.
         end.
    end.
    when "mobsub" THEN DO:
        for each memo no-lock where
            memo.brand = "1" and
            memo.hosttable = "mobsub":

            find mobsub no-lock where
                mobsub.custnum = memo.custnum and
                mobsub.msseq = int(memo.keyvalue) no-error.
                
            if not avail mobsub then next.
                  
            FIND Customer WHERE Customer.CustNum = MobSub.AgrCust 
            NO-LOCK NO-ERROR.
            
            CREATE ttMemo.
            ASSIGN 
                ttMemo.icnum  = memo.custnum  
                ttMemo.ccli   = mobsub.cli 
                ttMemo.chetu  = Customer.OrgID WHEN AVAILABLE Customer
                ttMemo.ctitle = memo.memotitle
                ttMemo.cmemo  = memo.memotext
                ttMemo.msseq  = mobsub.msseq.
        end.
    end.
end.

output stream excel to value(lcspooldir + lcfilename).

FOR EACH ttMemo NO-LOCK:

   /* dump the information */
   PUT STREAM excel UNFORMATTED
        ttMemo.ccli  lcDelim
        ttMemo.chetu lcDelim
        ttMemo.ctitle lcDelim       
        ttMemo.cmemo  skip.
END.

output stream excel close.

unix silent value("mv " + lcSpoolDir + lcFilename + " " +
                  lcOutDir).
session:numeric-format = numform.

END PROCEDURE.

