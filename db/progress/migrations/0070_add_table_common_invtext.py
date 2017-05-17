from gearbox.migrations import Migration

class AddTableInvText(Migration):

    database = "common"

    def up(self):
        t = self.table('InvText', area="Sta_Data_32", label="Invoice Texts", dump_name="invtext", desc="Texts to be printed to invoices")
        t.column('ITNum', 'integer', format=">>>>>>9", initial="0", max_width=4, label="Seq", column_label="Seq", position=2, order=10, help="Internal sequenco no. of an Invoice Text element")
        t.column('FromDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="From Date", column_label="From", position=3, order=20, help="First effective date")
        t.column('ToDate', 'date', format="99-99-99", initial=self.unknown, max_width=4, label="To Date", column_label="To", position=4, order=30, help="Last effective date")
        t.column('Target', 'character', format="x(16)", initial="", help="Target into which this text element belongs to", max_width=32, label="Target", column_label="Target", position=5, order=40, description="One of pre-defined targets (hard-coded by programmer)")
        t.column('KeyValue', 'character', mandatory=True, format="x(16)", initial="", help="Unique value of hosting record", max_width=32, label="KeyValue", column_label="KeyValue", position=6, order=50, description="Key value of hosting rec converted into formatted string expression")
        t.column('Position', 'integer', format="9", initial="1", max_width=4, label="Pos", column_label="Pos", position=7, order=60, help="Position on invoice 1:InvStart 2:InvEnd 3:SectStart 4:SectEnd")
        t.column('TxtTitle', 'character', format="x(40)", initial="", max_width=80, label="Title", column_label="Title", position=8, order=70, help="Short header/description of message (not to be printed)")
        t.column('InvText', 'character', format="x(255)", initial="", max_width=510, label="Text", column_label="Text", view_as="VIEW-AS EDITOR SIZE 60 BY 7 scrollbar-vertical", position=9, order=80, help="Information text")
        t.column('Language', 'integer', format=">>9", initial="0", max_width=4, label="Language", column_label="Language", position=10, order=90, help="Text language code")
        t.column('Report', 'integer', format=">>9", initial="0", max_width=4, label="Printed To", column_label="Printed", position=11, order=100, help="Where to print; 0=invoice, 1=reminder, 2=eMail")
        t.column('EPLForm', 'character', format="x(8)", initial="", max_width=16, label="EPL Form", column_label="EPLForm", position=12, order=110, help="EPL form number.")
        t.column('Attachment', 'character', format="x(30)", initial="", max_width=60, label="Attachments", column_label="Attachm.", position=13, order=120, help="Attachments to eMail")
        t.column('InfoType', 'character', format="x(8)", initial="", max_width=16, label="Information Type", column_label="Type", position=14, order=130, help="Type of information text")
        t.column('Brand', 'character', format="x(8)", initial="", max_width=16, label="Brand", column_label="Brand", position=15, order=140, help="Code Of Brand")
        t.column('RemLevel', 'integer', format=">9", initial="0", max_width=4, label="Reminder Level", column_label="RemLevel", position=16, order=150, help="Reminder level, i.e. how many times invoice has been reminded")
        t.column('LetterClass', 'integer', format="9", initial="0", max_width=4, label="Letter Class", column_label="LCLass", position=17, order=160, help="Letter class for EPL")
        t.column('AddrTarget', 'integer', format="9", initial="0", max_width=4, label="Address Target", column_label="AddrTarg", position=18, order=170, help="Address where letter is sent to")
        t.column('MainTitle', 'character', format="x(50)", initial="", max_width=100, label="Main Title", position=19, order=180, help="Main title for letter")
        t.column('PrintOnce', 'logical', format="Yes/No", initial="no", max_width=1, label="Print Only Once", column_label="Once", position=20, order=190, help="Use this text only once per customer")
        t.column('FilePrefix', 'character', format="x(8)", initial="", max_width=16, label="File Prefix", column_label="Prefix", position=21, order=200, help="Prefix for printing file name")
        t.column('CStateList', 'character', format="x(12)", initial="", help="List of claiming states for which this text is used", max_width=24, label="Claim States", column_label="Cl.State", position=22, order=210, description='''


''')
        t.column('SendRule', 'character', format="x(8)", initial="", max_width=16, label="SendRule", position=23, order=220, help="Send rule for SMS")
        t.column('Sender', 'character', format="x(16)", initial="", max_width=32, label="Sender", position=24, order=230, help="SMS sender number")
        t.column('Category', 'character', format="x(16)", initial="", max_width=32, label="Category", position=25, order=240, help="Text category")
        t.column('Active', 'logical', format="Yes/No", initial="Yes", max_width=1, label="Active", position=26, order=250, help="Is the configuration active")
        t.index('target', [['Brand'], ['Target'], ['KeyValue'], ['FromDate']], area="Sta_Index_2", primary=True)
        t.index('FromDate', [['Brand'], ['FromDate', 'DESC'], ['Target'], ['KeyValue']], area="Sta_Index_2")
        t.index('ITNum', [['ITNum']], area="Sta_Index_2", unique=True)

    def down(self):
        self.drop_table('InvText')
