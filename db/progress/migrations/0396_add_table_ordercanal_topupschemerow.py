from gearbox.migrations import Migration

class AddTableTopupSchemeRow(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TopupSchemeRow', area="Sta_Data_128", label="Topup Scheme Row", table_trigger=[{'crc': '?', 'procedure': 'rd-topupschemerow.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-topupschemerow.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="topupschemerow", desc="Topup scheme row")
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Topup Amount", column_label="Amount", position=2, order=30, help="Topup amount")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="Bill.Item", position=3, order=40, help="Billing item")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=4, order=10, help="Code of brand")
        t.column('DiscountAmount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Discount Amount", column_label="Discount", position=5, order=50, help="Discount amount")
        t.column('DiscountBillCode', 'character', format="x(16)", initial="", max_width=32, label="Discount Billing Item", column_label="Discount B.Item", position=6, order=60, help="Discount billing item")
        t.column('TopupScheme', 'character', format="x(12)", initial="", max_width=24, label="Topup Scheme", column_label="Scheme ID", position=7, order=20, help="Topup scheme ID")
        t.column('BeginStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Valid From", column_label="From", position=8, order=70, help="Valid from")
        t.column('EndStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="Valid To", column_label="To", position=9, order=80, help="Valid to")
        t.column('TopupSchemeRowID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="Row ID", column_label="ID", position=10, order=90, help="Row ID")
        t.column('DisplayAmount', 'decimal', format="-zzz,zz9.99", initial="0", max_width=15, label="DisplayAmount", column_label="DisplayAmount", position=11, order=100, help="Display Amount")
        t.index('TopupSchemeRow', [['Brand'], ['TopupScheme'], ['TopupSchemeRowID']], area="Sta_Index_2", primary=True)
        t.index('EndStamp', [['Brand'], ['TopupScheme'], ['EndStamp', 'DESC']], area="Sta_Index_2")
        t.index('TopupSchemeRowID', [['TopupSchemeRowID']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('TopupSchemeRow')
