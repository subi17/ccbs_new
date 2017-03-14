from gearbox.migrations import Migration

class AddFieldTenantName(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('RepLog')
        t.column('TenantName', 'character', format="X(15)", max_width=30, initial="yoigo", label="Tenant name", column_label="TenantName", position=9, order=80, help="Tenant name")

    def down(self):
        t = self.alter_table('RepLog')
        t.drop_column('TenantName')
