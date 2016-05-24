from gearbox.migrations import Migration

class AddTableBItemGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('BItemGroup', area="Sta_Data_128", label="Product Groups", table_trigger=[{'crc': '?', 'procedure': 'rd-bitemgroup.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-bitemgroup.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="bitemgro", desc="Product Groups")
        t.column('BIGroup', 'character', format="x(8)", initial="", max_width=16, label="Bill.Item Group", column_label="BI Group", position=2, order=10, help="Billing item group code")
        t.column('BIGName', 'character', format="x(30)", initial="", max_width=60, label="BIGroup Name", column_label="BIGName", position=3, order=20, help="Billing Item group name")
        t.column('xxMemo', 'character', format="x(60)", initial="", max_width=610, label="Memo", column_label="Memo", extent=5, position=4, order=30, help="Memo for product groups")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.column('GroupType', 'integer', format=">9", initial="0", max_width=4, label="Group Type", column_label="Type", position=6, order=50, help="Group type")
        t.column('ReportCode', 'character', format="x(12)", initial="", max_width=24, label="Report Code", column_label="Report", position=7, order=60, help="Reporting code")
        t.column('InvoiceOrder', 'integer', format=">>9", initial="0", max_width=4, label="Invoice Order", column_label="Order", position=8, order=70, help="Printing order in invoice specification")
        t.index('BIGroup', [['Brand'], ['BIGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BIGName', [['Brand'], ['BIGName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BItemGroup')
