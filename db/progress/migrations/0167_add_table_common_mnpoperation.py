from gearbox.migrations import Migration

class AddTableMNPOperation(Migration):

    database = "common"

    def up(self):
        t = self.table('MNPOperation', area="Sta_Data_64", dump_name="mnpoper")
        t.column('MNPOperationID', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="MNPOperationID", column_label="MNPOperationID", position=2, order=10)
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=3, order=20)
        t.column('StatusCode', 'integer', format=">>9", initial="0", max_width=4, label="StatusCode", column_label="StatusCode", position=4, order=30)
        t.column('MNPSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="MNPSeq", column_label="MNPSeq", position=5, order=40)
        t.column('Sender', 'integer', format=">>9", initial="0", max_width=4, label="Sender", column_label="Sender", position=6, order=50)
        t.column('SentTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="SentTS", column_label="SentTS", position=7, order=60)
        t.column('MessageType', 'character', format="x(8)", initial="", max_width=16, label="MessageType", column_label="MessageType", position=8, order=70)
        t.column('MsgTurn', 'integer', format=">>9", initial="0", max_width=4, label="MsgTurn", column_label="MsgTurn", position=9, order=80)
        t.column('XMLSeq', 'integer', format=">>>>>>>>9", initial="0", max_width=4, label="XMLSeq", column_label="XMLSeq", position=10, order=90, help="Reference to MNPXml table")
        t.column('ErrorCode', 'character', format="x(10)", initial="", max_width=20, label="ErrorCode", column_label="ErrorCode", position=13, order=120, help="ErrorCode")
        t.column('XMLRequest', 'clob', column_label="XMLRequest", clob_collation="basic", format="x(40)", label="XML Request", lob_size="1M", lob_bytes=1048576, lob_area="Lob_Data", position=14, clob_type=1, order=100, clob_codepage="utf-8", help="XML request message")
        t.column('XMLResponse', 'clob', column_label="XML Response", clob_collation="basic", format="x(40)", label="XMLResponse", lob_size="1M", lob_bytes=1048576, lob_area="Lob_Data", position=15, clob_type=1, order=110, clob_codepage="utf-8", help="XML response message")
        t.column('ErrorHandled', 'integer', format=">9", initial="0", max_width=4, label="ErrorHandled", column_label="ErrorHandled", position=16, order=130, help="ErrorHandled")
        t.column('ErrorDesc', 'character', format="x(50)", initial="", max_width=100, label="ErrorDesc", column_label="ErrorDesc", position=17, order=140, help="Error description")
        t.index('MNPOperation', [['MNPOperationID']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CreatedTS', [['CreatedTS', 'DESC']], area="Sta_Index_2")
        t.index('ErrorCode', [['ErrorHandled'], ['ErrorCode']], area="Sta_Index_2")
        t.index('MNPSeq', [['MNPSeq'], ['CreatedTS']], area="Sta_Index_2")
        t.index('Sender', [['Sender'], ['StatusCode']], area="Sta_Index_2")

    def down(self):
        self.drop_table('MNPOperation')
