from gearbox.migrations import Migration

class AddTableRZItem(Migration):

    database = "common"

    def up(self):
        t = self.table('RZItem', area="Sta_Data_128", dump_name="RZItem", desc="Roaming Zone for PLMN")
        t.column('PlmnCode', 'character', format="x(8)", initial="", max_width=16, label="PlmnCode", column_label="PLMN Code", position=2, order=10, help="Code of PLMN")
        t.column('CountryPrefix', 'character', format="x(6)", initial="", max_width=6, label="CountryPrefix", column_label="CCode", position=6, order=20, help="Country Code ()")
        t.column('RoamZone', 'character', format="x(12)", initial="", max_width=16, label="RZItem", column_label="RoamZone", position=7, order=30, help="RoamZone")
        t.column('DialType', 'integer', format=">>9", initial="0", max_width=4, label="DialType", column_label="RoamZone", position=8, order=40, help="Dialtype for RZItem")
        t.index('PLMNCode', [['PlmnCode']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CountryPrefix', [['CountryPrefix']], area="Sta_Index_2")
        t.index('RoamZone', [['RoamZone'], ['PlmnCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('RZItem')
