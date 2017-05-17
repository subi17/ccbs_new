from gearbox.migrations import Migration

class AddTableHighUsage(Migration):

    database = "common"

    def up(self):
        t = self.table('HighUsage', area="Sta_Data_64", dump_name="highusag")
        t.column('InvSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="InvSeq", column_label="InvSeq", position=2, order=10, help="High usage Invoice sequence")
        t.column('CLI', 'character', format="x(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=3, order=20, help="MSISDN Subscriber No")
        t.column('Qty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="QTY", column_label="QTY", position=4, order=30, help="Amount of call to this  Invseq/cli")
        t.column('Duration', 'integer', format=">>>>>>>>9", initial="0.00", max_width=4, label="Duration", column_label="Duration", position=5, order=40, help="duration")
        t.column('Amount', 'decimal', format="z,zzz,zz9.99", decimals=2, initial="0", max_width=17, label="Price", column_label="Price", position=6, order=50, help="Total Price")
        t.column('HiUsageStatus', 'integer', format=">9", initial="0", max_width=4, label="Status", column_label="Status", position=7, order=60, help="Status")
        t.column('CrStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="When was the order created", max_width=20, label="Created", column_label="Created", position=8, order=70, description="Create timestamp")
        t.column('ChStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="When was the order changed", max_width=20, label="Change", column_label="Change", position=9, order=80, description="Change timestamp")
        t.column('Category', 'character', format="x(4)", initial="", max_width=8, label="Cat", column_label="Cat", position=10, order=90, help="Category code")
        t.column('Date', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=11, order=100, help="Date when last updated")
        t.column('date%', 'decimal', format=">>>>9.99-", decimals=2, initial="0", max_width=17, label="Date%", column_label="Date%", position=12, order=110)
        t.column('DateGrow', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="DateGrow", column_label="DateGrow", position=13, order=120)
        t.column('launch', 'character', format="x(8)", initial="", max_width=16, label="Launch", column_label="Launch", position=14, order=130, help="Launch")
        t.index('InvSeq', [['InvSeq'], ['CLI']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Amount', [['Amount', 'DESC']], area="Sta_Index_2")
        t.index('CLI', [['CLI'], ['CrStamp', 'DESC']], area="Sta_Index_2")
        t.index('HiUsageStatus', [['HiUsageStatus'], ['CrStamp', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('HighUsage')
