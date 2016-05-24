from gearbox.migrations import Migration

class AddTableSCUpdRule(Migration):

    database = "mobile"

    def up(self):
        t = self.table('SCUpdRule', area="Sta_Data_128", label="Service Update Rule", dump_name="scupdrul", desc='''Service component update rule
''')
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=2, order=10, help="Code Of Brand")
        t.column('ServCom', 'character', format="x(12)", initial="", max_width=24, label="Service Component", column_label="Service", position=3, order=20, help="Code of service component")
        t.column('OldValue', 'integer', format=">>>9", initial="0", max_width=4, label="Old Value", column_label="Old", position=4, order=90, help="Old value")
        t.column('NewValue', 'integer', format=">>>9", initial="0", max_width=4, label="New Value", column_label="New", position=5, order=100, help="New value")
        t.column('UpdServCom', 'character', format="x(12)", initial="", max_width=24, label="Update Component", column_label="Upd.Service", position=6, order=110, help="Service component that will be updated")
        t.column('UpdValue', 'integer', format=">>>9", initial="0", max_width=4, label="Update Value", column_label="Update", position=7, order=120, help="Value for service component that will be updated")
        t.column('ScPriority', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Priority", column_label="Prior", position=8, order=130, help="Priority")
        t.column('ParamInt', 'integer', format=">>>>9", initial="0", max_width=4, label="Integer Parameter", column_label="ParamI", position=9, order=140, help="Integer parameter")
        t.column('ParamChar', 'character', format="x(30)", initial="", max_width=60, label="Character Parameter", column_label="ParamC", position=10, order=150, help="Character parameter")
        t.column('ParamDec', 'decimal', format="->>>>>9.99", decimals=5, initial="0", max_width=20, label="Decimal Parameter", column_label="ParamD", position=11, order=160, help="Decimal parameter")
        t.index('ServCom', [['Brand'], ['ServCom'], ['OldValue'], ['NewValue']], area="Sta_Index_3", primary=True)
        t.index('UpdServCom', [['Brand'], ['UpdServCom']], area="Sta_Index_3")

    def down(self):
        self.drop_table('SCUpdRule')
