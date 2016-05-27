from gearbox.migrations import Migration

class AddTableCTServAttr(Migration):

    database = "mobile"

    def up(self):
        t = self.table('CTServAttr', area="Sta_Data_128", label="CLIType Service Attributes", dump_name="ctservat", desc='''Attributes of CLI type's service components

''')
        t.column('CTServEl', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="Element ID", column_label="ID", position=2, order=10, help="Unique ID of CTServEl")
        t.column('ServAttr', 'character', format="x(14)", initial="", max_width=28, label="Service Attribute", column_label="Attribute", position=3, order=20, help="Attribute of a service component")
        t.column('DefValue', 'character', format="x(8)", initial="", max_width=16, label="Default Value", column_label="Value", position=4, order=30, help="Default value")
        t.column('ChgAllowed', 'logical', format="Yes/No", initial="yes", max_width=1, label="Change Allowed", column_label="Changeable", position=5, order=40, help="Value can be changed on subscription level")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From Date", column_label="From", position=6, order=50, help="Valid from date")
        t.index('CTServAttr', [['CTServEl'], ['ServAttr'], ['FromDate', 'DESC']], area="Sta_Index_3", primary=True, unique=True)
        t.index('ServAttr', [['ServAttr'], ['FromDate', 'DESC']], area="Sta_Index_3")

    def down(self):
        self.drop_table('CTServAttr')
