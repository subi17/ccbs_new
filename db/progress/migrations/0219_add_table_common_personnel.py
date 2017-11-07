from gearbox.migrations import Migration

class AddTablePersonnel(Migration):

    database = "common"

    def up(self):
        t = self.table('Personnel', area="Sta_Data_256", label="Personnel User", dump_name="personne", desc="Personnel User")
        t.column('PersCode', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Person", column_label="Person", position=2, order=10, help="Person responsible for handling the troubles")
        t.column('PersName', 'character', format="X(30)", initial="", max_width=60, label="Person Name", column_label="Person Name", position=3, order=20, help="Person Name")
        t.column('MobileNbr', 'character', format="X(20)", initial="", max_width=40, label="GSM No.", column_label="GSM No.", position=4, order=30, help="Mobile No, of person to deliver SMS messages")
        t.column('EMail', 'character', format="X(30)", initial="", max_width=60, label="Email", column_label="Email", position=5, order=40, help="Email id comtact of person in case of any trouble.")
        t.column('Preferred', 'character', format="X(8)", initial="", help="Preferred mode of contact of responsible person", max_width=16, label="Preferred", column_label="Preferred", position=8, order=50, description='''Preferred mode of communication
Valid values may be GSM, Email etc.''')
        t.column('Manager', 'character', format="X(8)", initial="", max_width=16, label="Manager", column_label="Manager", position=9, order=60, help="Manger of person responsible for handling the troubles")
        t.column('UserCode', 'character', format="X(8)", initial="", max_width=16, label="User", position=10, order=70, help="User code")
        t.index('perscode', [['PersCode']], area="Sta_Index_2", primary=True, unique=True)

    def down(self):
        self.drop_table('Personnel')
