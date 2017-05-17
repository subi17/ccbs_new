from gearbox.migrations import Migration

class AddFieldTariffType(Migration):

    database = "ordercanal"

    def up(self):
        t = self.alter_table('CLIType')
        t.column('TariffType', 'integer', format="9", initial="0", max_width=4, label="Tariff Type", column_label="TariffType", position=35, order=330)

    def down(self):
        t = self.alter_table('CLIType')
        t.drop_column('TariffType')
