from gearbox.migrations import Migration

class AddTableAckMessage(Migration):

    database = 'common'

    def up(self):
        t = self.table('AckMessage', area="Sta_Data_128", label="Ack Message", dump_name="ackmessage", desc="Ack messages")
        t.column('HostTable', 'character', format="x(16)", initial="", label="Host Table", column_label="Table", help="Host table")
        t.column('KeyValue', 'character', format="X(16)", initial="", label="Key Value", column_label="Key", help="Key value of the record")
        t.column('QueueName', 'character', format="x(20)", initial="", label="Queue", help="Response queue name")
        t.column('Created', 'datetime', format="99-99-9999 HH:MM:SS.SSS", initial=None, label="Created", help="Response created")
        t.column('AckStatus', 'integer', format="9", initial="0", label="Status", help="Response status")
        t.column('AckTarget', 'character', format="x(16)", initial="", label="Response Target", column_label="Target", help="Response target (external request ID)")
        t.column('AckResult', 'character', format="x(8)", initial="", label="Result", help="Response result")
        t.column('ResultDescription', 'character', format="x(30)", initial="", label="Result Description", column_label="Description", help="Result description (error message)")
        t.column('AddInfo', 'character', format="x(30)", initial="", label="Additional Info", column_label="Info", help="Additional info")
        t.index('AckStatus', ['AckStatus', 'Created'], area="Sta_Index_1")
        t.index('HostTable', ['HostTable', 'KeyValue', ('Created', 'DESC')], area="Sta_Index_2", primary=True)

    def down(self):
        self.drop_table('AckMessage')
