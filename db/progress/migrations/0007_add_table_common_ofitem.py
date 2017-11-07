from gearbox.migrations import Migration

class AddTableOFItem(Migration):

    database = "common"

    def up(self):
        t = self.table('OFItem', area="Sta_Data_2_256", dump_name="ofitem")
        t.column('OFID', 'integer', format=">>>>>9", initial="0", max_width=4, label="OFID", column_label="OFID", position=2, order=10, description='''



''')
        t.column('StatusCode', 'character', format="x(8)", initial="", help="Order's statuscode", max_width=16, label="StatusCode", column_label="StatudCode", position=3, order=20, description='''


''')
        t.index('OFID', [['OFID']], area="Sta_Index_4", primary=True)
        t.index('StatusCode', [['StatusCode'], ['OFID']], area="Sta_Index_4")

    def down(self):
        self.drop_table('OFItem')
