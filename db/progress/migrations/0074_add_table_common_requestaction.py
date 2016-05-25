from gearbox.migrations import Migration

class AddTableRequestAction(Migration):

    database = "common"

    def up(self):
        t = self.table('RequestAction', area="Sta_Data_64", label="Request Actions", dump_name="requestaction", desc="Request actions")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", position=2, order=10, help="Code of brand")
        t.column('ReqType', 'integer', format=">>9", initial="0", max_width=4, label="Request Type", column_label="Type", position=3, order=20, description="Request type")
        t.column('CLIType', 'character', format="x(12)", initial="", max_width=24, label="CLI Type", column_label="CLIType", position=4, order=30, help="Type of the subscription")
        t.column('ValidFrom', 'date', format="99-99-99", max_width=4, label="Valid From", column_label="From", position=5, order=40, help="Date when configuration becomes effective")
        t.column('ValidTo', 'date', format="99-99-99", max_width=4, label="Valid To", column_label="To", position=6, order=50, help="Date when configuration expires")
        t.column('ActionType', 'character', format="x(20)", initial="", max_width=40, label="Action Type", column_label="Act.Type", position=7, order=60, help="Action type")
        t.column('ActionKey', 'character', format="x(12)", initial="", max_width=24, label="Action Key", column_label="Key", position=8, order=70, help="Key value of the action type")
        t.column('Action', 'integer', format=">9", initial="0", help="Action", max_width=4, label="Action", position=9, order=80, description="1=create,2=terminate,3=move")
        t.column('PayType', 'integer', format="9", initial="0", help="Payment type", max_width=4, label="Payment Type", column_label="PayType", position=10, order=90, description="postpaid, prepaid")
        t.column('RequestActionID', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Request Action ID", column_label="ID", position=11, order=100, help="Request action ID")
        t.index('RequestActionID', [['RequestActionID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CLIType', [['Brand'], ['CLIType'], ['ReqType'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('PayType', [['Brand'], ['PayType'], ['ReqType'], ['ValidTo', 'DESC']], area="Sta_Index_2")
        t.index('ReqType', [['Brand'], ['ReqType'], ['ValidTo', 'DESC']], area="Sta_Index_2")

    def down(self):
        self.drop_table('RequestAction')
