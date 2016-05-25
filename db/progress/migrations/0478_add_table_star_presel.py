from gearbox.migrations import Migration

class AddTablePresel(Migration):

    database = "star"

    def up(self):
        t = self.table('Presel', area="Schema Area", label="Preselection transactions", dump_name="presel", desc="Preselction transactions")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('CLI', 'character', format="x(20)", initial="", max_width=40, label="Subscriber No", column_label="Subscriber No", position=3, order=20, help="Subscriber (Telephone) number with area code")
        t.column('PsType', 'integer', valexp="INPUT pstype < 4", format="9", initial="0", max_width=4, label="Type", column_label="Type", position=4, order=30, valmsg="Type of preselction MUST be 0 ... 3 !", help="Type of presel. 0):None 1):Nat 2):Intn'l 3):Both")
        t.column('PsSeq', 'integer', format="zzzzzz9", initial="0", max_width=4, label="Sequence", column_label="Seq", position=5, order=40, help="Internal sequence no for a transaction")
        t.column('Orderer', 'character', format="x(60)", initial="", max_width=120, label="Name of Orderer", column_label="Orderer", position=6, order=60, help="Name of person who made this order at customer")
        t.column('ReturnCode', 'integer', format=">9", initial="0", max_width=4, label="Rc", column_label="Rc", position=7, order=70, help="Return Code")
        t.column('ErrText', 'character', format="x(20)", initial="", max_width=40, label="Error expl", column_label="Expl", position=8, order=80, help="Additional explanation of error")
        t.column('AuthNo', 'character', format="x(10)", initial="", max_width=20, label="AuthNo", column_label="AuthNo", position=9, order=90, help="Number or code of authorization")
        t.column('AuthDate', 'date', format="99-99-99", max_width=4, label="AuthDate", column_label="AuthDate", position=10, order=100, help="Date of Authorisation")
        t.column('CrStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Created", column_label="Created", position=11, order=110, help="TimeStamp: created")
        t.column('ChStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time When Preselect record was last changed", max_width=20, label="Changed", column_label="Changed", position=12, order=120, description="Time Stamp yyyymmdd.time (sec)")
        t.column('ConfDate', 'date', format="99-99-99", max_width=4, label="Date of confirm", column_label="ConfDate", position=13, order=130, help="Date of confirmation")
        t.column('FileSeq1', 'integer', format=">>>>>9", initial="0", max_width=4, label="OutFileSeq", column_label="OutFileSeq", position=14, order=140, help="Sequence of OutputFile")
        t.column('SentDate', 'date', format="99-99-99", max_width=4, label="SentDate", column_label="SentDate", position=15, order=150, help="Date when preselection has been sent")
        t.column('FileSeq2', 'integer', format=">>>>>9", initial="0", max_width=4, label="InFileSeq", column_label="InFileSeq", position=16, order=160, help="Sequence of InputFile")
        t.column('AuthNo2', 'character', format="x(10)", initial="", max_width=20, label="AuthNo", column_label="AuthNo", position=17, order=170, help="Second Authorisation code")
        t.column('CustName', 'character', format="x(30)", initial="", max_width=60, label="Customer's name", column_label="Customer's name", position=18, order=180, help="Customer's name")
        t.column('OrgCode', 'character', format="x(11)", initial="", max_width=22, label="Org/pers. nr", column_label="Org/pers. nr", position=19, order=190, help="Customer's organisation number or personal number")
        t.index('CLI', [['CLI']], area="Schema Area", primary=True, unique=True)
        t.index('CustNum', [['CustNum'], ['CLI']], area="Schema Area")
        t.index('fileseq', [['FileSeq1']], area="Schema Area")

    def down(self):
        self.drop_table('Presel')
