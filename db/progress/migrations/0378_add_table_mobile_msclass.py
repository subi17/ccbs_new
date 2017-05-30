from gearbox.migrations import Migration

class AddTableMSClass(Migration):

    database = "mobile"

    def up(self):
        t = self.table('MSClass', area="Sta_Data_256", label="MSISDN Classes", dump_name="msclass", desc='''
''')
        t.column('McCode', 'integer', format="zz9", initial="0", max_width=4, label="Class", column_label="Class", position=2, order=10, help="Code of MSISDN Class")
        t.column('McName', 'character', format="x(40)", initial="", max_width=80, label="Class Name", column_label="Class Name", position=3, order=20, help="Name of MSISDN Class")
        t.column('McAmount', 'integer', format="zz,zz9", initial="0", max_width=4, label="Price", column_label="Price", position=4, order=30, help="Price of a number of this class")
        t.column('McResAmount', 'integer', format="zz,zz9", initial="0", max_width=4, label="ResPrice", column_label="ResPrice", position=5, order=40, help="Monthly Reservation Fee for a MSISDN No. of this class")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="BrCode", column_label="BrCode", position=6, order=50, help="Code Of Brand")
        t.index('McCode', [['Brand'], ['McCode'], ['McName']], area="Sta_Index_3", primary=True, unique=True)
        t.index('McName', [['McName'], ['McCode']], area="Sta_Index_3")

    def down(self):
        self.drop_table('MSClass')
