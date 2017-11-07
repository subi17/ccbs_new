from gearbox.migrations import Migration

class AddTablePLMN(Migration):

    database = "common"

    def up(self):
        t = self.table('PLMN', area="Sta_Data_128", label="PLMN", dump_name="plmn", desc="PLMN")
        t.column('PLMN', 'character', format="x(8)", initial="", max_width=16, label="PLMN", column_label="PLMN", position=2, order=10, help="Code of PLMN")
        t.column('CommName', 'character', format="x(16)", initial="", max_width=32, label="CommName", column_label="CommName", position=4, order=30, help="Commercial Name")
        t.column('CoName', 'character', format="x(30)", initial="", max_width=60, label="Name of Country", column_label="Name of Country", position=5, order=40, help="Name of Country")
        t.column('Country', 'character', format="xxx", initial="", max_width=6, label="Country", column_label="CCode", position=6, order=50, help="Country Code (according to ISO Standard)")
        t.column('PrefToNor', 'character', format="x(8)", initial="", max_width=16, label="PrefixToNorway", column_label="PrefixToNorway", position=7, order=60, help="Prefix to Norway")
        t.column('CountryPrefix', 'character', format="x(6)", initial="", max_width=12, label="Country Prefix", column_label="Prefix", position=8, order=70, help="Country prefix")
        t.index('plmn', [['PLMN'], ['Country']], area="Sta_Index_2", primary=True, unique=True)
        t.index('Country', [['Country']], area="Sta_Index_2")
        t.index('CountryPrefix', [['CountryPrefix']], area="Sta_Index_2")

    def down(self):
        self.drop_table('PLMN')
