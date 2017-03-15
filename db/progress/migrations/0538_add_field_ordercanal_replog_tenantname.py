from gearbox.migrations import Migration

class AddFieldTenantName(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('RepLog')
        t.column('TenantName', 'character', format="X(15)", max_width=30, initial="yoigo", label="Tenant name", column_label="TenantName", position=9, order=80, help="Tenant name")
        t.alter_index('SendTime', [['SendTime'], ['TenantName'], ['EventTime']], area="CDF_Index", primary=True)

    def down(self):
        t = self.alter_table('RepLog')
        t.alter_index('SendTime', [['SendTime'], ['EventTime']], area="CDF_Index", primary=True)
        t.drop_column('TenantName')
