from gearbox.migrations import Migration

class AddTablePersLeave(Migration):

    database = "common"

    def up(self):
        t = self.table('PersLeave', area="Sta_Data_256", label="Leaves", dump_name="persleav", desc="Personnel Leave Details")
        t.column('PersCode', 'character', format="X(8)", initial="", max_width=16, label="Person", column_label="Person", position=2, order=10, help="Person code of the person for whom leave will be applicable")
        t.column('FromStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of Leave From", max_width=20, label="From", column_label="From", position=3, order=20, description="Time Stamp yyyymmdd.time (sec)")
        t.column('ToStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of Leave To", max_width=20, label="To", column_label="To", position=4, order=30, description="Time Stamp yyyymmdd.time (sec)")
        t.column('CreUser', 'character', format="X(8)", initial="", max_width=16, label="Created By", position=5, order=40, help="User code")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of holiday creation", max_width=20, label="Created", column_label="Created", position=6, order=50, description="Time Stamp yyyymmdd.time (sec)")
        t.index('perscode', [['PersCode'], ['FromStamp']], area="Sta_Index_2", primary=True, unique=True)
        t.index('FromStamp', [['FromStamp']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PersLeave')
