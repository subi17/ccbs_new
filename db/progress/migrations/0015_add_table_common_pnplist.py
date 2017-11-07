from gearbox.migrations import Migration

class AddTablePNPList(Migration):

    database = "common"

    def up(self):
        t = self.table('PNPList', area="Sta_Data_64", dump_name="pnplist")
        t.column('BDestFrom', 'character', format="x(12)", initial="", max_width=24, label="BDestFrom", column_label="BDestFrom", position=2, order=20, help="FROM destination for PNP series")
        t.column('PNPSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=3, order=30, help="PNP Sequence")
        t.column('CustNum', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="CustNum", column_label="CustNum", position=4, order=10)
        t.column('BDestTo', 'character', format="x(12)", initial="", max_width=24, label="BDestTo", column_label="BDestTo", position=5, order=40, help="TO destination for PNP series")
        t.column('PNPGroup', 'character', format="x(10)", initial="", max_width=20, label="GroupCode", column_label="GroupCode", position=6, order=50, help="PNP group code")
        t.column('PriceList', 'character', format="x(8)", initial="", max_width=16, label="Price List", column_label="PList", position=7, order=60, help="Code (identifier) for a Price List")
        t.column('CLI', 'character', format="x(12)", initial="", max_width=24, label="CLI", column_label="CLI", position=8, order=70, help="CLI number")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=9, order=80, help="Code Of Brand")
        t.column('DialTypeUsed', 'logical', format="X/", initial="NO", max_width=60, label="Dialling Types Used", column_label="DT", extent=15, position=10, order=90, help="Dialling types used")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From Date", column_label="From", position=11, order=100, help="First effective date")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To Date", column_label="To", position=12, order=110, help="Last effective date")
        t.column('MSSeq', 'integer', format=">>>>>>>9", initial="0", help="Subscription ID", max_width=4, label="Subscription ID", column_label="Subscr.", position=13, order=120, description='''

''')
        t.column('MemberCLI', 'character', format="x(12)", initial="", max_width=24, label="Member", position=14, order=130, help="Member of the group (CLI)")
        t.index('CustNum', [['Brand'], ['CustNum'], ['BDestFrom'], ['BDestTo']], area="Sta_Index_2", primary=True)
        t.index('CustNum_s', [['CustNum'], ['BDestFrom'], ['BDestTo']], area="Sta_Index_2")
        t.index('MsSeq', [['MSSeq'], ['MemberCLI'], ['FromDate', 'DESC']], area="Sta_Index_2")
        t.index('PNPSeq', [['Brand'], ['PNPSeq'], ['BDestFrom'], ['BDestTo']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PNPList')
