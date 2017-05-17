from gearbox.migrations import Migration

class AddTablePostCode(Migration):

    database = "common"

    def up(self):
        t = self.table('PostCode', area="Sta_Data_128", label="Zip Codes", dump_name="xPostCod", desc="Zip codes")
        t.column('ZipCode', 'character', format="x(5)", initial="", max_width=16, label="ZipCode", column_label="ZipCode", position=2, order=10, help="Postal code")
        t.column('PostOffice', 'character', format="x(40)", initial="", max_width=80, label="Postal Office", column_label="PostOffice", position=3, order=20, help="Postal office")
        t.column('Country', 'character', format="xxx", initial="", max_width=6, label="Country", column_label="CCode", position=4, order=30, help="Country Code (according to ISO Standard)")
        t.column('Region', 'character', format="x(8)", initial="", max_width=16, label="Region", column_label="Region", position=5, order=40, help="Region code")
        t.index('Region', [['Country'], ['Region'], ['ZipCode']], area="Sta_Index_2", primary=True)
        t.index('PostOffice', [['Country'], ['PostOffice']], area="Sta_Index_2")
        t.index('ZipCode', [['Country'], ['ZipCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PostCode')
