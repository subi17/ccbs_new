from gearbox.migrations import Migration

class AddTableOrderFunction(Migration):

    database = "common"

    def up(self):
        t = self.table('OrderFunction', area="Sta_Data_128", dump_name="orderfun")
        t.column('OFID', 'integer', format=">>>>>9", initial="0", max_width=4, label="OFID", column_label="OFID", position=2, order=10, description='''

''')
        t.column('OFName', 'character', format="x(30)", initial="", max_width=60, label="Name of the Order Function", column_label="Name of the Order Function", position=3, order=20, description='''

''')
        t.column('OFModule', 'character', format="x(16)", initial="", help="Progress module name", max_width=32, label="Module", column_label="Module", position=4, order=30, description='''

''')
        t.index('OFID', [['OFID']], area="Sta_Index_1", primary=True)
        t.index('OFName', [['OFName']], area="Sta_Index_2")

    def down(self):
        self.drop_table('OrderFunction')
