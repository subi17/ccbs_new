from gearbox.migrations import Migration

class AddTablePNPGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('PNPGroup', area="Sta_Data_64", dump_name="pnpgroup")
        t.column('PNPGroup', 'character', format="x(10)", initial="", max_width=20, label="GroupCode", column_label="GroupCode", position=2, order=10)
        t.column('Name', 'character', format="x(16)", initial="", max_width=32, label="Name", column_label="Name", position=3, order=20)
        t.column('PNPSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=7, order=30, help="PNP Sequence")
        t.column('dFrom', 'date', format="99-99-99", max_width=4, label="DateFrom", column_label="DateFrom", position=8, order=40, help="FROM date for PNP Group validation")
        t.column('dTo', 'date', format="99-99-99", max_width=4, label="DateTo", column_label="DateTo", position=9, order=50, help="TO date for PNP Group validation")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=10, order=60, help="Rating CCN")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="BDest", column_label="BDest", position=11, order=70, help="B-number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=12, order=80, help="Code Of Brand")
        t.column('GroupType', 'integer', format=">9", initial="0", max_width=4, label="PNP Group Type", column_label="PNPGroupType", position=13, order=90, help="Group type of PNP")
        t.column('RateCCN', 'integer', format=">>9", initial="0", max_width=4, label="RateCCN", column_label="RateCCN", position=14, order=100, help="Rate CCN")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="Price List", column_label="PList", position=15, order=110, help="Code (identifier) for a Price List")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=16, order=120, help="Billing item code, max 16 characters")
        t.column('Pnpgrouptupe', 'integer', format=">9", initial="0", max_width=4, label="PnpGroup Type", column_label="PnpGroup Type", position=17, order=130, help="Type of pnp group")
        t.column('PNPListLimit', 'integer', format=">>9", initial="0", help="Max quantity for members", max_width=4, label="Member Limit", column_label="Limit", position=18, order=140, description='''

''')
        t.index('PNPGroup', [['Brand'], ['GroupType'], ['PNPGroup'], ['dTo', 'DESC']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CCN', [['Brand'], ['CCN']], area="Sta_Index_2")
        t.index('Name', [['Brand'], ['GroupType'], ['Name'], ['PNPGroup']], area="Sta_Index_2")
        t.index('PNPSeq', [['Brand'], ['PNPSeq']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('PNPGroup')
