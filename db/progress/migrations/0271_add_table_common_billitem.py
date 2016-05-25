from gearbox.migrations import Migration

class AddTableBillItem(Migration):

    database = "common"

    def up(self):
        t = self.table('BillItem', area="Sta_Data_64", label="Billing Items", table_trigger=[{'crc': '?', 'procedure': 'rd-billitem.p', 'override_proc': True, 'event': 'REPLICATION-DELETE'}, {'crc': '?', 'procedure': 'rw-billitem.p', 'override_proc': True, 'event': 'REPLICATION-WRITE'}], dump_name="billitem", desc="Billing items")
        t.column('BillCode', 'character', format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=2, order=10, help="Billing item code, max 16 characters")
        t.column('BIName', 'character', format="x(30)", initial="", max_width=60, label="Bill.Item Name", column_label="BI Name", position=3, order=20, help="Billing item's name")
        t.column('AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Account", column_label="Account", position=4, order=30, help="Account number")
        t.column('BIGroup', 'character', format="x(8)", initial="", max_width=16, label="Bill.Item Group", column_label="BI Group", position=5, order=40, help="Billing item group code")
        t.column('DispMPM', 'logical', format="Yes/No", initial="no", help="Display MPM on specification reports", max_width=1, label="Display MPM", column_label="MPM", position=6, order=50, description='''

''')
        t.column('EUAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="EU Sales Account", column_label="EU", position=7, order=60, help="Account number for EU sales")
        t.column('FSAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Foreign Sales Account", column_label="Foreign", position=8, order=70, help="Account number for sales outside EU")
        t.column('TB1AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="TB1", column_label="TB1", position=9, order=260, help="Account for TB1")
        t.column('TB2AccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="TB2", column_label="TB2", position=10, order=270, help="Account for TB2")
        t.column('InvSect', 'character', format="x(8)", initial="", max_width=16, label="Invoice Section", column_label="Inv.Sect", position=11, order=280, help="Code of an Invoice Section")
        t.column('VATCode', 'integer', format="z9", initial="1", max_width=4, label="VAT code", column_label="VAT code", position=13, order=120, help="VAT code")
        t.column('BillType', 'character', format="x(8)", initial="", max_width=16, label="Billing type", column_label="Billing type", position=14, order=90, help="Billing type")
        t.column('ISOrder', 'integer', format=">>9", initial="0", max_width=4, label="Section Order", column_label="Order", position=15, order=100, help="Order within invoice section")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=16, order=290, help="Code Of Brand")
        t.column('EUVATCode', 'integer', format="z9", initial="0", max_width=4, label="EU VAT Code", column_label="EU VAT", position=17, order=300, help="VAT code for EU sales")
        t.column('EUConAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="EU Cons. Sales", column_label="EUCon", position=18, order=310, help="Account number for EU sales with VAT (consumers)")
        t.column('CostCentre', 'character', format="x(8)", initial="", max_width=16, label="Cost Centre", column_label="CCentre", position=19, order=320, help="Cost Centre")
        t.column('AltAccNum', 'integer', format=">>>>>9", initial="0", max_width=4, label="Own Use Account", column_label="Own Use", position=20, order=330, help="Account for own use")
        t.column('TaxClass', 'character', format="x(8)", initial="", help="Tax class", max_width=16, label="Tax Class", column_label="Class", position=21, order=340, description='''

''')
        t.column('SAPRid', 'character', format="x(8)", initial="", max_width=16, label="SAP Reporting ID", column_label="SAP RID", position=22, order=350, help="SAP reporting ID")
        t.column('VIPAccNum', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="VIP Account", column_label="VIP Acc", position=23, order=360, help="Account for VIP customers")
        t.column('OrderChannelOrder', 'integer', format=">>>>>>>>>9", initial="0", max_width=4, label="Order Channel Order", column_label="Order", position=24, order=370, help="Presentation order in order channel")
        t.column('Active', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Active", position=25, order=380, help="Is billing item active")
        t.index('BillCode', [['Brand'], ['BillCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BIGroup', [['Brand'], ['BIGroup'], ['BillCode']], area="Sta_Index_2", unique=True)
        t.index('BIName', [['Brand'], ['BIName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('BillItem')
