from gearbox.migrations import Migration

class AddTableForeignRequest(Migration):

    database = "common"

    def up(self):
        t = self.table('ForeignRequest', area="Sta_Data_256", dump_name="foreignr")
        t.column('MessageId', 'decimal', format=">>>>>>>>>>>>>>>9", decimals=2, initial="0", max_width=17, label="MessageID", column_label="MessageID", position=2, order=10, help="Message identification number")
        t.column('TimeStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="TimeStamp", column_label="TimeStamp", position=3, order=20, help="Record creation timestamp")
        t.column('ForeignRequest', 'character', format="x(8)", initial="", max_width=16, label="Request", column_label="Request", position=4, order=30, help="Complete request")
        t.column('StatusFlag', 'integer', format=">9", initial="0", max_width=4, label="Status", column_label="Status", position=5, order=40, help="Status information flag")
        t.column('InTopic', 'character', format="x(8)", initial="", max_width=16, label="InTopic", column_label="InTopic", position=6, order=50)
        t.column('OutTopic', 'character', format="x(8)", initial="", max_width=16, label="OutTopic", column_label="OutTopic", position=7, order=60)
        t.column('Request', 'character', format="x(8)", initial="", max_width=16, label="Request", column_label="Request", position=8, order=70, help="What was requested ?")
        t.column('ErrClass', 'integer', format=">>9", initial="0", max_width=4, label="Error Class", column_label="Error Class", position=9, order=80)
        t.column('Severity', 'integer', format=">>9", initial="0", max_width=4, label="Severity", column_label="Severity", position=10, order=90)
        t.column('ErrNum', 'integer', format=">>9", initial="0", max_width=4, label="Error Number", column_label="Error Number", position=11, order=100)
        t.index('MessaggeId', [['MessageId']], area="Sta_Index_2", primary=True)
        t.index('StatusFlag', [['StatusFlag'], ['Request']], area="Sta_Index_2")

    def down(self):
        self.drop_table('ForeignRequest')
