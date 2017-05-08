from gearbox.migrations import Migration

class AddTableFraud(Migration):

    database = "mobile"

    def up(self):
        t = self.table('Fraud', area="Sta_Data_256", label="Fraud B-nros", dump_name="frbnro")
        t.column('Fraud', 'character', format="x(16)", initial="", max_width=32, label="BNumber", column_label="BNumber", position=2, order=10, help="Fraud B-Number")
        t.column('FraudName', 'character', format="x(30)", initial="", max_width=60, label="fr-Name", column_label="fr-Name", position=3, order=20, help="Name of Fraud B-Number")
        t.index('Fraud', [['Fraud'], ['FraudName']], area="Sta_Index_3", primary=True)
        t.index('FraudName', [['FraudName'], ['Fraud']], area="Sta_Index_3", unique=True)

    def down(self):
        self.drop_table('Fraud')
