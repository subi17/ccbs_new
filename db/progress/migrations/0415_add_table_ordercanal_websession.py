from gearbox.migrations import Migration

class AddTableWebSession(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('WebSession', area="Sta_Data_256", label="WebSession", dump_name="websessi", desc="Currently logged in users")
        t.column('sessionid', 'character', format="x(32)", initial="", max_width=64, case_sensitive=True, position=2, order=10, help="Unique identifier")
        t.column('usercode', 'character', format="X(20)", initial="", max_width=40, position=3, order=20, help="Identifies the user (foreign key to login in access tables)")
        t.column('sessiontype', 'character', format="X(8)", initial="", max_width=16, position=4, order=30, help="Type of the session (e.g. selfcare, portal)")
        t.column('createdate', 'date', format="99.99.99", max_width=4, position=5, order=40, help="Creation date (see also createtime)")
        t.column('createtime', 'integer', format=">>,>>>", initial="0", max_width=4, position=6, order=50, help="Creation time (see also createdate)")
        t.column('lastdate', 'date', format="99.99.99", max_width=4, position=7, order=60, help="Last day user requested a page (see also lasttime)")
        t.column('lasttime', 'integer', format=">>,>>>", initial="0", max_width=4, position=8, order=70, help="Last time user requested a page (see also lastdate)")
        t.column('data', 'character', format="X(100)", initial="", max_width=200, position=9, order=80, help="Container for session data that must be saved on server side")
        t.index('SessionId', [['sessionid']], area="Sta_Index_2", primary=True, unique=True)
        t.index('LastChange', [['lastdate'], ['lasttime']], area="Sta_Index_2")
        t.index('LastDate', [['lastdate']], area="Sta_Index_2")
        t.index('UserCode', [['usercode'], ['sessiontype']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('WebSession')
