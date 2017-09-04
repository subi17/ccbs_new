from gearbox.migrations import Migration

class AddTableRoamGPRS(Migration):

    database = "roamcdr"

    def up(self):
        t = self.table('RoamGPRS', area="Dyn_Data_64", dump_name="roamcdr-------1")
        t.column('TSRead', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="ReadIn", column_label="ReadIn", position=2, order=10, help="TimeStamp when EDR was read")
        t.column('DateStart', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date", column_label="Date", position=3, order=20)
        t.column('TimeStart', 'integer', format="99999", initial="0", max_width=4, label="Time", column_label="Time", position=4, order=30)
        t.column('PLMN', 'character', format="x(5)", initial="", max_width=10, label="PLMN", column_label="PLMN", position=5, order=40)
        t.column('CSV', 'character', format="x(8)", initial="", max_width=16, label="CSV", column_label="CSV", position=7, order=60)
        t.column('Units', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Units", column_label="Units", position=8, order=70)
        t.column('Version', 'character', format="x(6)", initial="", max_width=12, label="Version", column_label="Version", position=9, order=80)
        t.column('DateRead', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="DateRead", column_label="DateRead", position=10, order=90)
        t.column('EventType', 'character', format="x(4)", initial="", max_width=8, label="EventType", column_label="EventType", position=12, order=110)
        t.column('RSubType', 'integer', format=">>9", initial="0", max_width=4, label="RSubType", column_label="RSubType", position=13, order=120, description="Roaming subscriber type")
        t.column('Amount', 'decimal', format="->>>>9.999999", decimals=6, initial="0", max_width=21, label="Amount", column_label="Amount", position=14, order=130)
        t.column('ChargedParty', 'integer', format=">9", initial="0", max_width=4, label="ChargedParty", column_label="ChargedParty", position=15, order=140)
        t.column('CLI', 'character', format="x(10)", initial="", max_width=20, label="CLI", column_label="CLI", position=16, order=150)
        t.column('GsmBnr', 'character', format="x(12)", initial="", max_width=24, label="Bnr", column_label="Bnr", position=17, order=160)
        t.column('RateZone', 'integer', format=">9", initial="0", max_width=4, label="RateZone", column_label="RateZone", position=18, order=170)
        t.column('PartInd', 'integer', format="9", initial="0", max_width=4, position=20, order=180)
        t.column('PartRecNum', 'integer', format=">>9", initial="0", max_width=4, position=21, order=190)
        t.column('CallIdNum', 'int64', format=">>>>>>>>>9", initial="0", max_width=8, label="CallIdNum", column_label="CallIdNum", position=22, order=200, help="Call identification number")
        t.column('GGSNAddr', 'character', format="x(15)", initial="", max_width=30, label="GGSNAddr", column_label="GGSNAddr", position=23, order=210, help="SGSN address")
        t.column('TSDump', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TSDump", column_label="TSDump", position=24, order=220, help="TimeStamp when dumped to TAP3 file")
        t.index('DateStart', [['DateStart'], ['TimeStart'], ['CLI']], area="Dyn_Index2", primary=True)
        t.index('DateRead', [['DateRead']], area="Dyn_Index1")
        t.index('PartialRec', [['PLMN'], ['DateRead'], ['DateStart'], ['CallIdNum'], ['GGSNAddr']], area="Dyn_Index1")
        t.index('PLMN', [['PLMN'], ['DateStart', 'DESC']], area="Dyn_Index2")

    def down(self):
        self.drop_table('RoamGPRS')
