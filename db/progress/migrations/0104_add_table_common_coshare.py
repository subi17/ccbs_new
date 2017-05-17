from gearbox.migrations import Migration

class AddTableCoShare(Migration):

    database = "common"

    def up(self):
        t = self.table('CoShare', area="Sta_Data_256", dump_name="coshare")
        t.column('CoTarg', 'character', format="X(10)", initial="", max_width=20, label="Commission Target", column_label="Commission Target", position=2, order=20, help="Commission Target Code based on Target type")
        t.column('RsLevel', 'integer', format=">9", initial="0", max_width=4, label="Reseller Level", column_label="RSLevel", position=3, order=30, help="Salesman's level in reseller's organization")
        t.column('CoPerc', 'decimal', format=">9.99", decimals=2, initial="0", max_width=17, label="Share Percentage", column_label="Share%", position=4, order=40, help="Percentage of the commission amount (target's share)")
        t.column('CoAmt', 'decimal', format="->>,>>9.99", decimals=2, initial="0", max_width=17, label="Share Amount", column_label="Amount", position=5, order=50, help="Fixed share of the commission amount")
        t.column('CoTargId', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Target ID", column_label="TargID", position=6, order=60, help="Commission target ID")
        t.column('TargType', 'character', format="X(1)", initial="", help="Commission Target Type. Valid values are C, S or R", max_width=2, label="Target Type", column_label="Target Type", position=7, order=10, description="Valid values are (C)ustomer, (S)alesman or (R)eseller")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=70, help="Code Of Brand")
        t.index('CoTargId', [['CoTargId'], ['TargType'], ['CoTarg'], ['RsLevel']], area="Sta_Index_2", primary=True)
        t.index('CoTarg', [['Brand'], ['TargType'], ['CoTarg']], area="Sta_Index_2")

    def down(self):
        self.drop_table('CoShare')
