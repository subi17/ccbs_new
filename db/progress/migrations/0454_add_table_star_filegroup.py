from gearbox.migrations import Migration

class AddTablefilegroup(Migration):

    database = "star"

    def up(self):
        t = self.table('filegroup', dump_name="filegrp", area="Sta_Data_256")
        t.column('filegroup', 'character', format="X(12)", initial="", max_width=24, label="File Group", position=2, order=10, help="File group")
        t.column('filetimestamp', 'character', format="X(14)", initial="", max_width=28, label="TimeStamp", position=3, order=20, help="File TimeStamp YYYYMMDDhhmmss")

    def down(self):
        self.drop_table('filegroup')
