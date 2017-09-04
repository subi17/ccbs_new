from gearbox.migrations import Migration

class AddTablefiles(Migration):

    database = "star"

    def up(self):
        t = self.table('files', area="Sta_Data_256", label="File Storage", dump_name="files", desc="Name and status")
        t.column('filename', 'character', format="X(50)", initial="", max_width=100, label="Filename", position=2, order=10, help="Filename with directoryname")
        t.column('filetimestamp', 'character', format="X(14)", initial="", max_width=28, label="TimeStamp", position=3, order=30, help="File TimeStamp YYYYMMDDhhmmss")
        t.column('filecrc', 'character', format="X(30)", initial="", max_width=60, label="CRC", position=5, order=40, help="File crc")
        t.column('filegroup', 'character', format="X(20)", initial="", max_width=40, label="File Group", position=6, order=20, help="Normaly first directory")
        t.index('filename', [['filename']], area="Sta_Index_2", primary=True, unique=True)
        t.index('filetimestamp', [['filegroup'], ['filetimestamp']], area="Sta_Index_2")

    def down(self):
        self.drop_table('files')
