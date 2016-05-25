from gearbox.migrations import Migration

class AddTableFATGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('FATGroup', area="Sta_Data_64", label="FAT Group", table_trigger=[{'crc': '?', 'procedure': 'rd-fatgroup.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-fatgroup.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="fatgroup", desc="FAT Group for products")
        t.column('FtGrp', 'character', format="x(8)", initial="", max_width=16, label="FatGroup", column_label="FtGrp", position=2, order=10, help="Fat Group (for products)")
        t.column('FtgName', 'character', format="x(30)", initial="", max_width=60, label="Name", position=3, order=20, help="Group name")
        t.column('InvMemo', 'character', format="x(60)", initial="", max_width=610, label="Invoice Text", column_label="Inv.Txt", extent=5, position=4, order=30, help="Text to invoice")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="B.Item", position=5, order=40, help="Billing item")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.column('AmtLimit', 'decimal', format=">>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount Limit", column_label="Limit", position=7, order=60, help="Max amount of FAT per CLI")
        t.column('FATType', 'integer', format="9", initial="0", help="FATime type", max_width=4, label="FATime Type", column_label="Type", position=8, order=70, description="0=calls, 1=fixed fees, 2=all")
        t.column('Transfer', 'logical', format="Yes/No", initial="no", max_width=1, label="Transferrable", column_label="Transfer", position=9, order=80, help="Transferrable to next period")
        t.column('Amount', 'decimal', format="->>>>>>9.99", decimals=2, initial="0", max_width=17, label="Amount", position=10, order=90, help="Amount per period")
        t.column('FATPerc', 'decimal', format="->>>>>9.99", decimals=2, initial="0", help="FATime percentage", max_width=17, label="Percentage", column_label="Perc.", position=11, order=100, description='''

''')
        t.column('FatTarget', 'character', format="x(8)", initial="", max_width=16, label="FATime Target", column_label="Target", position=12, order=110, help="To whom FATime can be used")
        t.column('Priority', 'integer', format=">>9", initial="0", max_width=4, label="Priority", column_label="Pri", position=13, order=120, help="Relative priority to other FATimes")
        t.column('PeriodQty', 'integer', format=">9", initial="1", help="Period quantity", max_width=4, label="Period Quantity", column_label="Periods", position=14, order=130, description='''

''')
        t.column('QtyUnit', 'character', format="x(8)", initial="", max_width=16, label="Qty Unit", column_label="Unit", position=15, order=140, help="Unit of the amount")
        t.column('Interval', 'integer', format=">9", initial="1", max_width=4, label="Interval", position=16, order=150, help="Interval; number of months between events")
        t.column('GroupType', 'integer', format=">>9", initial="0", max_width=4, label="Group Type", column_label="GType", position=17, order=160, help="FATime group type")
        t.column('ValidPeriods', 'integer', format=">>9", initial="0", max_width=4, label="Valid Periods", column_label="Valid", position=18, order=170, help="How many periods is FATime valid for")
        t.index('FtGrp', [['Brand'], ['FtGrp']], area="Sta_Index_2", primary=True, unique=True)
        t.index('FatType', [['Brand'], ['FATType'], ['FtGrp']], area="Sta_Index_2")
        t.index('FtgName', [['Brand'], ['FtgName']], area="Sta_Index_2")
        t.index('GroupType', [['Brand'], ['GroupType'], ['FtGrp']], area="Sta_Index_2")
        t.index('Priority', [['Brand'], ['Priority', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('FATGroup')
