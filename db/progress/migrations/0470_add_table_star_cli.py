from gearbox.migrations import Migration

class AddTableCLI(Migration):

    database = "star"

    def up(self):
        t = self.table('CLI', area="Sta_Data_256", label="CLI", dump_name="cli", desc="CLI numbers")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer number")
        t.column('CLI', 'character', format="x(16)", initial="", max_width=32, label="A-Sub No.", column_label="A-Sub.No", position=3, order=20, help="Customer's A-subscriber Number with Area Code")
        t.column('Date', 'date', format="99-99-99", initial="today", max_width=4, label="Created", column_label="Created", position=4, order=30, help="Date when a-number was created / updated")
        t.column('OwnerName', 'character', format="x(30)", initial="", max_width=60, label="Owner", column_label="Owner's name", position=5, order=40, help="Name of A-subscriber number's owner")
        t.column('ValueLimit', 'integer', format=">,>>>,>>9", initial="0", max_width=4, label="ASub. limit", column_label="ASub. limit", position=6, order=141, help="A-Subscribers monthly limit")
        t.column('Ref', 'character', format="x(12)", initial="", max_width=24, label="RefNum", column_label="RefNum", position=7, order=150, help="Reference number for additional usage")
        t.column('Active', 'logical', format="Y/N", initial="no", max_width=1, label="Open", column_label="Open", position=8, order=160, help="Is this a-number open (ie. can this number call) ?")
        t.column('Pwd', 'integer', format="9999", initial="0", max_width=4, label="Password", column_label="Password", position=9, order=170, help="ASUB password")
        t.column('CrStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Created", column_label="Created", position=10, order=180, help="Time stamp of connection")
        t.column('ClStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=17, label="Closed", column_label="Closed", position=11, order=190, help="Time stamp of disconnection")
        t.column('CallLimit', 'integer', format=">,>>>,>>9", initial="0", max_width=4, label="ASub. call limit", column_label="ASub. call limit", position=12, order=200, help="A-Subscribers call limit")
        t.column('MthLimit', 'integer', format=">,>>>,>>9", initial="0", max_width=4, label="ASub. mth limit", column_label="ASub. mth limit", position=13, order=140, help="A-Subscribers monthly limit")
        t.column('BillTarget', 'integer', format=">9", initial="0", max_width=4, label="Billing Target", column_label="Bill.Targ", position=14, order=210, help="Customer's billing target")
        t.column('SerNum', 'integer', mandatory=True, format="zzzzzzz9", initial="0", max_width=4, label="SerNum", column_label="SerNum", position=15, order=220, help="Consecutive number (sequence) of series")
        t.column('Contract', 'character', format="x(8)", initial="", max_width=16, label="Contract ID", column_label="ContrID", position=16, order=230, help="Contract ID")
        t.index('CLI', [['CLI'], ['CustNum']], area="Sta_Index_2", primary=True)
        t.index('Contract', [['Contract']], area="Sta_Index_2")
        t.index('CustNum', [['CustNum'], ['BillTarget'], ['CLI']], area="Sta_Index_2")
        t.index('SerNum', [['SerNum'], ['CLI']], area="Sta_Index_2")
        t.index('Valid', [['CLI'], ['CrStamp', 'DESC'], ['ClStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CLI')
