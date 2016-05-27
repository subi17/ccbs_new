from gearbox.migrations import Migration

class AddTableCallLimit(Migration):

    database = "common"

    def up(self):
        t = self.table('CallLimit', area="Sta_Data_128", dump_name="calllimi")
        t.column('CustNo', 'integer', format="zzzzzzzzz", initial="0", max_width=4, label="CustNo", column_label="CustNo", position=2, order=10, help="Customer Number")
        t.column('cli', 'character', format="X(16)", initial="", max_width=32, label="Cli", column_label="Cli", position=3, order=20, description="The cli number (A-number)")
        t.column('DeliType', 'integer', format=">9", initial="0", max_width=4, label="DelivereType", column_label="DT", position=4, order=30, help="DelivereType")
        t.column('DeliPara', 'character', format="x(40)", initial="", max_width=80, label="DeliverValue", column_label="DV", position=5, order=40)
        t.column('limit', 'integer', format=">>9", initial="0", help="limit", max_width=4, label="limit", column_label="limit", position=6, order=50, description="limit")
        t.column('Dfrom', 'date', format="99-99-99", initial="today", max_width=4, label="Valid From", column_label="From", position=7, order=60, help="Call Limit valid from")
        t.column('Dto', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=8, order=70, help="Call limit valid to")
        t.column('ActStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and Time When Call Limit Record was last activated", max_width=20, label="Activated", column_label="Activated", position=9, order=80, description="Time Stamp yyyymmdd.time (sec)")
        t.column('CLSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=10, order=90, help="Call Limit Sequence")
        t.column('CreditType', 'integer', format=">9", initial="0", max_width=4, label="CreditType", column_label="CreditType", position=11, order=100, help="Credit Type")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=110, help="Code Of Brand")
        t.index('CustNo', [['Brand'], ['CustNo'], ['cli'], ['Dto', 'DESC']], area="Sta_Index_2", primary=True)
        t.index('Cli', [['Brand'], ['cli'], ['Dto', 'DESC']], area="Sta_Index_2")
        t.index('Cli_s', [['cli'], ['Dto', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CallLimit')
