from gearbox.migrations import Migration

class AddTableTMSUser(Migration):

    database = "common"

    def up(self):
        t = self.table('TMSUser', area="Sta_Data_32", label="Users", dump_name="tmsuser", desc="Users")
        t.column('UserCode', 'character', mandatory=True, format="x(8)", initial="", max_width=16, label="User ID", column_label="User ID", position=2, order=10, help="User ID, 1 - 8 characters")
        t.column('Password', 'character', format="x(8)", initial="", max_width=16, label="Passwrd", column_label="Passwrd", position=3, order=20, help="Password, 1 - 8 characters")
        t.column('UserNum', 'integer', mandatory=True, format="999", initial="0", help="User's internal consecutive number", max_width=4, label="UserNo", column_label="UserNo", position=5, order=40, description="Unique key for the usercode")
        t.column('UserName', 'character', format="x(30)", initial="", max_width=60, label="Name", column_label="Name", position=6, order=50, help="Entire name of user")
        t.column('Active', 'logical', format="Yes/No", initial="yes", max_width=1, label="Activ", column_label="Activ", position=7, order=60, help="Active")
        t.column('Initials', 'character', format="x(3)", initial="", max_width=6, label="Sort", column_label="Sort", position=8, order=70, help="User initials ")
        t.column('StartMenu', 'character', format="x(5)", initial="", max_width=10, label="GroupNo", column_label="GroupNo", position=9, order=80, help="Salesgroup's ID")
        t.column('WorkStation', 'logical', format="B&W/Colour", initial="no", max_width=1, label="Workst.", column_label="Workst.", position=10, order=90, help="Is a workstation black&white or colour")
        t.column('Modem', 'character', format="x(10)", initial="", max_width=20, label="Modem", column_label="Modem", position=11, order=100, help="Location of the user's modem")
        t.column('NetCode', 'character', format="x(10)", initial="", max_width=20, label="NetNo", column_label="NetNo", position=12, order=110, help="Net code")
        t.column('Prefix', 'character', format="x(10)", initial="", max_width=20, label="Foreigh", column_label="Foreigh", position=13, order=120, help="Prefix for outgoing international calls")
        t.column('DialCmd', 'character', format="x(10)", initial="", max_width=20, label="DialCmd", column_label="DialCmd", position=14, order=130, help="Dialing Command")
        t.column('RepDir', 'character', format="x(30)", initial="", max_width=60, label="Txt-directory", column_label="Txt-directory", position=16, order=150, help="Default output  dirctory of .txt files")
        t.column('Email', 'character', format="x(40)", initial="", max_width=80, label="E-mail", column_label="E-mail", position=17, order=160, help="Customer's e-mail address")
        t.column('Brand', 'character', format="x(40)", initial="", max_width=80, label="Brand", column_label="Brand", position=18, order=170, help="Code Of Brands")
        t.column('CreditLimit', 'decimal', format=">>>>>>>>>9.99", decimals=2, initial="0", max_width=17, label="Crediting Limit", column_label="CreditLimit", position=19, order=180, help="Max. invoice amount that this user can credit")
        t.column('UserGroup', 'character', format="x(10)", initial="", max_width=20, label="GroupCode", column_label="GroupCode", position=20, order=30, help="Individual Code for a User Group")
        t.column('ErgoKeyb', 'logical', format="yes/no", initial="no", max_width=1, label="ErgoKbd", column_label="ErgoKeyboard", position=21, order=190, help="Is this Erconomy keyboard")
        t.column('Salesman', 'character', format="x(8)", initial="", max_width=16, label="Salesman", column_label="Salesman", position=22, order=200, help="Salesman's code")
        t.column('ForeignId', 'character', format="x(20)", initial="", max_width=40, label="ForeignId", column_label="ForeignId", position=23, order=210)
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date from", column_label="Date from", position=24, order=220, help="Activation date")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="Date to", column_label="Date to", position=25, order=230, help="Expiring date")
        t.column('TopUpLimit', 'integer', format="999", initial="75", max_width=4, label="TopUpLimit", column_label="TopUpLimit", position=26, order=240, help="Max amount TMS-user can give TopUp")
        t.column('AdminUser', 'logical', format="Yes/No", initial="no", max_width=1, label="Admin User", column_label="Admin", position=27, order=250, help="Admin level user")
        t.index('UserCode', [['UserCode', 'ABBREVIATED']], area="Sta_Index_2", primary=True, unique=True)
        t.index('ForeignId', [['ForeignId'], ['UserCode'], ['UserName']], area="Sta_Index_2")
        t.index('UserGroup', [['UserGroup']], area="Sta_Index_2")
        t.index('UserName', [['UserName'], ['UserCode']], area="Sta_Index_2", unique=True)
        t.index('UserNum', [['UserNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('TMSUser')
