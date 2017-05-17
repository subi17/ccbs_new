from gearbox.migrations import Migration

class ChgTableCSVHeader(Migration):

    database = "mobile"

    def up(self):
        t = self.alter_table('CSVHeader', buffer_pool="Alternate")
        t.alter_indexdata('Version', buffer_pool="Alternate")

    def down(self):
        self.alter_table('CSVHeader')
        t.alter_indexdata(Version)
