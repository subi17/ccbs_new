from gearbox.migrations import Migration

class AddTableCustPNPGroup(Migration):

    database = "common"

    def up(self):
        t = self.table('CustPNPGroup', area="Sta_Data_256", dump_name="custpnpg")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=2, order=10, help="Customer's number")
        t.column('PnpGroup', 'character', format="x(8)", initial="", max_width=16, label="PnPGroup", column_label="PnPGroup", position=3, order=20, help="PnPGroup")
        t.column('PnPPrior', 'integer', format=">9", initial="1", max_width=4, label="Priority", column_label="Priority", position=4, order=30, help="Priority")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.index('CustNum', [['Brand'], ['CustNum'], ['PnPPrior'], ['PnpGroup']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum_s', [['CustNum'], ['PnPPrior'], ['PnpGroup']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('CustPNPGroup')
