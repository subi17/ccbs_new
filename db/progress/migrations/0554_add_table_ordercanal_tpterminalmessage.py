from gearbox.migrations import Migration

class AddTableTPServiceMessage(Migration):

    database = "ordercanal"

    def up(self):
        t = self.table('TPServiceMessage', area="Sta_Data_64", label="TP Terminal Message", dump_name="tpterminalmessage", desc="Contains messages between TMS and Third party service provider")
        t.column('ServSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="ServSeq", column_label="ServSeq", position=1, order=10, description="Service sequence")
        t.column('MessageSeq', 'integer', format=">>>>>>>9", initial="0", max_width=4, label="MessageSeq", column_label="MessageSeq", position=2, order=20, description="Message sequence")
        t.column('MessageID', 'character', format="x(37)", initial="", max_width=74, label="MessageId", column_label="MessageID", position=3, order=30, description="Auto-generated UUID")
        t.column('MessageType', 'character', format="x(16)", initial="", max_width=32, label="Message Type", column_label="MessageType", position=4, order=40, description="Message type")
        t.column('Source', 'character', format="x(8)", initial="", max_width=16, label="Message Source", column_label="Source", position=5, order=50, description="Messsage source (TMS/Masmovil)")
        t.column('CreatedTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="CreatedTS", column_label="CreatedTS", position=6, order=60, description="CreatedTS")
        t.column('UpdateTS', 'decimal', format="99999999.99999", decimals=5, initial="0", max_width=20, label="UpdateTS", column_label="UpdateTS", position=7, order=70, description="UpdatedTS")
        t.column('Status', 'character', format="x(15)", initial="", max_width=30, label="Status", column_label="Status", position=8, order=80, description="Message status")
        t.column('ResponseCode', 'character', format="x(10)", initial="", max_width=20, label="Response Code", column_label="ResponseCode", position=9, order=90, description="Response code")
        t.column('AdditionalInfo', 'character', format="x(40)", initial="", max_width=80, label="Additional Info", column_label="AdditionalInfo", position=10, order=100, description="Additional status info")
        
        t.index('ServSeq', [['ServSeq', 'DESC'], ['MessageSeq']], area="Dyn_Index_1", primary=True, unique=True)
        t.index('MessageSeq', ['MessageSeq', 'DESC'], area="Dyn_Index_1", unique=True)
        t.index('MessageID', ['MessageID', 'DESC'], area="Dyn_Index_1", unique=True)
        t.index('Status', [['Source'], ['Status']], area="Dyn_Index_1")
        t.index('UpdatedTS', ['UpdatedTS', 'DESC'], area="Dyn_Index_1")

    def down(self):
        t = self.drop_table('TPServiceMessage')
        t.drop_column('ServSeq')
        t.drop_column('MessageSeq')
        t.drop_column('MessageID')
        t.drop_column('MessageType')
        t.drop_column('Source')
        t.drop_column('CreatedTS')
        t.drop_column('UpdatedTS')
        t.drop_column('Status')
        t.drop_column('ResponseCode') 
        t.drop_column('AdditionalInfo')
        
