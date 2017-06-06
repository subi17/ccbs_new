from gearbox.migrations import Migration

class AddTableRoamOper(Migration):

    database = "mobile"

    def up(self):
        t = self.table('RoamOper', area="Sta_Data_256", dump_name="roamoper")
        t.column('Country', 'character', format="x(20)", initial="", max_width=40, label="Country", column_label="Country", position=3, order=20)
        t.column('CommName', 'character', format="x(12)", initial="", max_width=24, label="Comm.Name", column_label="Comm.Name", position=4, order=30)
        t.column('Name', 'character', format="x(12)", initial="", max_width=24, label="Name", column_label="Name", position=5, order=40)
        t.column('Currency', 'character', format="x(4)", initial="", max_width=8, label="Currency", column_label="Currency", position=6, order=50)
        t.column('IMSI1', 'character', format="x(3)", initial="", max_width=6, label="IMSI1", column_label="IMSI1", position=7, order=60)
        t.column('IMSI2', 'character', format="x(2)", initial="", max_width=4, label="IMSI2", column_label="IMSI2", position=8, order=70)
        t.column('FileSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="FileSeq", column_label="FileSeq", position=9, order=80)
        t.column('PLMN', 'character', format="x(5)", initial="", max_width=10, label="PLMN", column_label="PLMN", position=10, order=90)
        t.column('Active', 'logical', format="Yes/No", initial="No", max_width=1, label="Active", column_label="Active", position=11, order=100, help="Are files sent automatically?")
        t.column('Production', 'integer', format="9", initial="0", max_width=4, label="Production", column_label="Production", position=12, order=120)
        t.column('TestFileSeq', 'integer', format=">>>>>>9", initial="0", max_width=4, label="FileSeq", column_label="FileSeq", position=13, order=110)
        t.column('RoamGroup', 'character', format="x(8)", initial="", max_width=16, label="RoamGroup", column_label="RoamGroup", position=14, order=130)
        t.column('IMSI', 'character', format="x(6)", initial="", max_width=6, label="IMSI", column_label="IMSI", position=15, order=140)
        t.index('CommName', [['CommName']], area="Sta_Index_1", primary=True)
        t.index('IMSI', [['IMSI']], area="Sta_Index_1")
        t.index('PLMN', [['PLMN']], area="Sta_Index_1")

    def down(self):
        self.drop_table('RoamOper')
