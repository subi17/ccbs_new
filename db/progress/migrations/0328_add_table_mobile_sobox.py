from gearbox.migrations import Migration

class AddTableSOBox(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SOBox', area="Sta_Data_256", label="Service Order Temporary Log", dump_name="sobox", desc='''
''')
        t.column('SoFile', 'integer', format="999999", initial="0", max_width=4, label="FileSeq", column_label="FileSeq", position=2, order=10, help="Sequence for a File")
        t.column('SoSeq', 'integer', format="999999", initial="0", max_width=4, label="OrdSeq", column_label="OrdSeq", position=3, order=20, help="Sequence for a Service Order")
        t.column('CLI', 'character', format="X(11)", initial="", max_width=22, label="MSISDN", column_label="MSISDN No", position=4, order=30, help="MSISDN Subscriber No")

    def down(self):
        self.drop_table('SOBox')
