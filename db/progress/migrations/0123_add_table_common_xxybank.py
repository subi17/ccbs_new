from gearbox.migrations import Migration

class AddTablexxyBank(Migration):

    database = "common"

    def up(self):
        t = self.table('xxyBank', area="Sta_Data_256", label="xxBank", dump_name="xxBank", desc="xxBank")
        t.column('BankOffice', 'character', mandatory=True, format="x(4)", initial="", max_width=80, label="Office", column_label="Office", position=3, order=20, help="Bank's name")
        t.column('ZipCode', 'character', format="x(5)", initial="", max_width=4, label="Zip", column_label="Zip", position=4, order=30, help="ZIP code")
        t.column('City', 'character', format="x(16)", initial="", max_width=32, label="City", column_label="City", position=5, order=40, help="Location city")
        t.column('Address', 'character', format="x(20)", initial="", max_width=40, label="Street", column_label="Street", position=6, order=50, help="Street address")
        t.column('BankId', 'character', format="X(4)", initial="", max_width=18, label="BankId", column_label="BankId", position=7, order=980, help="Identification code for bank")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=8, order=990, help="Code Of Brand")
        t.column('Name2', 'character', format="x(20)", initial="", max_width=80, label="Name2", column_label="Name2", position=9, order=1010, help="Bank's name")
        t.column('Name', 'character', format="x(20)", initial="", max_width=80, label="Name", column_label="Name", position=10, order=1000, help="Bank's name")
        t.column('FileDate', 'date', format="99-99-99", max_width=4, label="File Date", column_label="Date", position=11, order=1020, help="Date of the file from which data was read")
        t.index('BankId', [['Brand'], ['BankId'], ['BankOffice']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('xxyBank')
