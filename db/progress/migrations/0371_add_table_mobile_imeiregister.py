from gearbox.migrations import Migration

class AddTableIMEIRegister(Migration):

    database = "mobile"

    def up(self):
        t = self.table('IMEIRegister', area="Sta_Data_256", label="IMEI Register", dump_name="imeiregister", desc="IMEI register")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Brand")
        t.column('IMEI', 'character', format="x(17)", initial="", max_width=34, label="IMEI", position=3, order=20, help="IMEI code")
        t.column('BillCode', 'character', mandatory=True, format="x(16)", initial="", max_width=32, label="Billing Item", column_label="BillItem", position=4, order=30, help="Billing item code")
        t.index('IMEI', [['Brand'], ['IMEI']], area="Sta_Index_2", primary=True, unique=True)
        t.index('BillCode', [['Brand'], ['BillCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('IMEIRegister')
