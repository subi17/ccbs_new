from gearbox.migrations import Migration

class AddTableVRKQuery(Migration):

    database = "common"

    def up(self):
        t = self.table('VRKQuery', area="Sta_Data_32", label="VRKQuery", dump_name="vrkquery")
        t.column('PersonId', 'character', format="x(11)", initial="", help="Personal Number", max_width=22, label="PersonId", column_label="PersonId", position=2, order=10, description="Personal Number")
        t.column('LastName', 'character', format="x(20)", initial="", help="Customer's last name", max_width=40, label="LastName", column_label="LastName", position=3, order=30, description="Customer's last name")
        t.column('FirstName', 'character', format="x(20)", initial="", help="Customer's fore/given name", max_width=40, label="ForeName", column_label="ForeName", position=4, order=40, description="Customer's forename")
        t.column('HomeCode', 'character', format="x(3)", initial="", max_width=6, label="HomeCode", column_label="HomeCode", position=5, order=50)
        t.column('HomeName', 'character', format="x(20)", initial="", max_width=40, label="HomeName", column_label="HomeName", position=6, order=60)
        t.column('Address', 'character', format="x(30)", initial="", max_width=60, label="Address", column_label="Address", position=7, order=70, help="Customer's mailing address (street, p.o. box)")
        t.column('ZipCode', 'character', format="x(8)", initial="", max_width=16, label="Postal code", column_label="Postcd", position=8, order=80, help="Customer's postal code")
        t.column('PostOffice', 'character', format="x(24)", initial="", max_width=48, label="Postal Addr.", column_label="Postaddr", position=9, order=90, help="Customer's postal address")
        t.column('MovingDate', 'date', format="99/99/99", initial=self.unknown, max_width=4, label="MovingDate", column_label="MovingDate", position=10, order=100)
        t.column('TempAddress', 'character', format="x(20)", initial="", max_width=40, label="TempAddress", column_label="TempAddress", position=11, order=110, description="Customer's temporary address")
        t.column('TempZip', 'character', format="x(5)", initial="", max_width=10, label="TempZip", column_label="TempZip", position=12, order=120, description="Customer's temporary zipcode")
        t.column('TempPOffice', 'character', format="x(24)", initial="", max_width=48, label="TempPOffice", column_label="TempPOffice", position=13, order=130, description="Customer's temporary post office")
        t.column('TempFrom', 'date', format="99/99/99", initial=self.unknown, max_width=4, label="TempFrom", column_label="TempFrom", position=14, order=140, description="Temporary address valid from")
        t.column('TempTo', 'date', format="99/99/99", initial=self.unknown, max_width=4, label="TempTo", column_label="TempTo", position=15, order=150, description="Temporary address valid to")
        t.column('Language', 'character', format="x(2)", initial="", max_width=4, label="Language", column_label="Language", position=16, order=160, description="Language code")
        t.column('LangName', 'character', format="x(30)", initial="", max_width=60, label="LangName", column_label="LangName", position=17, order=170, description="Language in text")
        t.column('DeathDay', 'date', format="99/99/99", initial=self.unknown, max_width=4, label="DeathDay", column_label="DeathDay", position=18, order=180, description="Date of death")
        t.column('Trusteeship', 'character', format="x(1)", initial="", help="Trusteeship", max_width=2, label="Trusteeship", column_label="Tship", position=19, order=190, description="Edunvalvonta")
        t.column('CompLimit', 'character', format="x(1)", initial="", help="Limitation of competence", max_width=2, label="Competence Limit", column_label="CompLimit", position=20, order=200, description="toimintakelpoisuuden rajoitus")
        t.column('TshipTxt', 'character', format="x(20)", initial="", help="Trusteeship as text", max_width=40, label="Trusteeship Text", column_label="Tship Txt", position=21, order=210, description="Edunvalvonta tekstina")
        t.column('CompLimitTxt', 'character', format="x(20)", initial="", help="Limitation of competence as text", max_width=40, label="Comp.Limit Text", column_label="CLimit Txt", position=22, order=220, description="rajoitus tekstina")
        t.column('CrStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Creation timestamp", max_width=20, label="Created", column_label="Created", position=23, order=20, description="Creation timestamp")
        t.index('PersonId', [['PersonId'], ['CrStamp', 'DESC']], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('VRKQuery')
