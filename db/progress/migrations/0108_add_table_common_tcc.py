from gearbox.migrations import Migration

class AddTableTCC(Migration):

    database = "common"

    def up(self):
        t = self.table('TCC', area="Sta_Data_128", label="Technical Call Case", dump_name="tcc", desc="Technical CCN")
        t.column('BCC', 'integer', mandatory=True, format="zzzz9", initial="0", max_width=5, label="Billing Call Case", column_label="Bill.CC", position=2, order=1000, help="Billing call case number (BCC)")
        t.column('TCCName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=3, order=30, help="Name")
        t.column('DialType', 'integer', format=">>>9", initial="0", max_width=4, label="Dialling Type", column_label="DT", position=4, order=35, help="Dialling type code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=5, help="Code of brand")
        t.column('Pulses', 'integer', format="zz9", initial="0", max_width=4, label="Pulses", column_label="Pulses", position=13, order=95, help="Pulses")
        t.column('ValidFrom', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid From", column_label="From", position=16, order=60, help="Effective from")
        t.column('ValidTo', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Valid To", column_label="To", position=23, order=70, help="Effective until")
        t.column('BDest', 'character', format="x(16)", initial="", max_width=50, label="B-Destination", column_label="B-Dest", position=24, order=80, help="B-Destination")
        t.column('DurTo', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Duration To", column_label="Dur.To", position=25, order=88, help="Duration to")
        t.column('GsmBnr', 'character', format="x(16)", initial="", max_width=50, label="B-Number", column_label="B-Nbr", position=26, order=81, help="B-number")
        t.column('MinBillQty', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Minimum Billing Qty", column_label="Min.BillQty", position=27, order=90, help="Minimum billing quantity")
        t.column('TCC', 'integer', mandatory=True, format="zzzz9", initial="0", max_width=5, label="Technical Call Case", column_label="TCC", position=28, order=10, help="Technical call case number (TCC)")
        t.column('TCCRule', 'integer', format="zz9", initial="0", max_width=5, label="TCC Rule", column_label="Rule", position=29, order=101, help="Technical rule for TCC")
        t.column('ErrorCode', 'integer', format=">>>9", initial="0", max_width=4, label="Error Code", column_label="Error", position=30, order=85, help="Error code")
        t.column('BType', 'integer', format=">9", initial="0", max_width=4, label="B-Type", column_label="BT", position=31, order=37, help="B-destination type")
        t.column('TariffRule', 'integer', format="9", initial="0", max_width=4, label="Tariff Rule", column_label="TR", position=32, order=100, help="Pricing method")
        t.column('TrafficType', 'integer', format="9", initial="0", max_width=5, label="Traffic Type", column_label="Traffic Type", position=33, order=900, help="Traffic type")
        t.column('BDestPref', 'character', format="x(16)", initial="", max_width=50, label="Prefix For B-Destination", column_label="Prefix For B-dest", position=34, order=82, help="Prefix for B-Destination")
        t.column('TCCPayer', 'integer', format="zz9", initial="0", max_width=5, label="TCC Payer", column_label="Payer", position=35, order=201, help="Payer; Where to find payer/owner information")
        t.index('TCC', [['Brand'], ['TCC'], ['DurTo'], ['ValidTo', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BCC', [['Brand'], ['BCC'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('DialType', [['Brand'], ['DialType'], ['ErrorCode']], area="Sta_Index_2")
        t.index('TCCName', [['Brand'], ['TCCName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TCC')
