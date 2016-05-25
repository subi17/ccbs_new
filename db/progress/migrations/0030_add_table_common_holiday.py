from gearbox.migrations import Migration

class AddTableHoliday(Migration):

    database = "common"

    def up(self):
        t = self.table('Holiday', area="Sta_Data_256", label="Holiday Master Table", dump_name="holiday", desc="Holiday Details")
        t.column('Holiday', 'date', format="99-99-9999", max_width=4, label="Holiday Date", column_label="Date", position=2, order=10, help="Holiday's date")
        t.column('Caption', 'character', format="x(40)", initial="", max_width=80, label="Holiday Caption", column_label="Caption", position=3, order=20, help="Holiday Caption")
        t.column('CreUser', 'character', format="X(8)", initial="", max_width=16, label="Created By", position=4, order=30, help="User code")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of holiday creation", max_width=20, label="Created", column_label="Created", position=5, order=40, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=6, order=50, help="Code Of Brand")
        t.index('Holiday', [['Brand'], ['Holiday']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('Holiday')
