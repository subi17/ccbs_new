from gearbox.migrations import Migration

class AddTableRateCCN(Migration):

    database = "common"

    def up(self):
        t = self.table('RateCCN', area="Sta_Data_2_256", dump_name="rateccn")
        t.column('BDest', 'character', mandatory=True, format="x(25)", initial="", max_width=50, label="B-subNo", column_label="B-subNo", position=2, order=10, help="B-number")
        t.column('CCN', 'integer', mandatory=True, format="zz9", initial="0", max_width=4, label="CCN", column_label="CCN", position=3, order=20, help="Call case number")
        t.column('DialType', 'integer', format=">>9", initial="0", max_width=4, label="Dialling Type", column_label="DT", position=4, order=30, help="Dialling type code")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=5, order=40, help="Code Of Brand")
        t.column('DestType', 'integer', format=">9", initial="0", max_width=4, label="Destination Type", column_label="Dest.Type", position=6, order=50, help="Destination type")
        t.column('BdestID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="BDestination ID", column_label="BDest ID", position=7, order=60, help="Unique ID")
        t.index('BDest', [['Brand'], ['BDest'], ['DestType'], ['DialType']], area="Sta_Index_3", primary=True, unique=True)
        t.index('BDestID', [['BdestID'], ['DialType']], area="Sta_Index_3")
        t.index('CCN', [['Brand'], ['BDest'], ['CCN']], area="Sta_Index_3")

    def down(self):
        self.drop_table('RateCCN')
