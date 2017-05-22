from gearbox.migrations import Migration

class AddTableIPRange(Migration):

    database = "common"

    def up(self):
        t = self.table('IPRange', area="Sta_Data_128", dump_name="iprange")
        t.column('NetworkAddress', 'character', format="x(16)", initial="", max_width=32, label="NetworkAddress", position=2, order=10, help="NetworkAddress")
        t.column('Netmask', 'integer', format=">>9", initial="0", max_width=4, label="NetMask", position=3, order=20, help="Network subnet mask")
        t.column('FirstIP', 'decimal', format=">>>>>>>>>>9", decimals=0, initial="0", max_width=15, label="FirstIP", column_label="FirstIP", position=4, order=30, help="First IP address in the range")
        t.column('LastIP', 'decimal', format=">>>>>>>>>>9", decimals=0, initial="0", max_width=15, label="LastIP", column_label="LastIP", position=5, order=40, help="Last IP address in the range")
        t.column('OperName', 'character', format="x(15)", initial="", max_width=30, label="OperName", position=7, order=50, help="Operator name")
        t.column('PLMN', 'character', format="x(8)", initial="", max_width=16, label="PLMN", position=8, order=60, help="PLMN code")
        t.index('IPRange', [['NetworkAddress']], area="Sta_Index_2", primary=True, unique=True)
        t.index('FromDate', [['FirstIP'], ['LastIP']], area="Sta_Index_2")

    def down(self):
        self.drop_table('IPRange')
