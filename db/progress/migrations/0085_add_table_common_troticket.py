from gearbox.migrations import Migration

class AddTableTroTicket(Migration):

    database = "common"

    def up(self):
        t = self.table('TroTicket', area="Sta_Data_256", label="Trouble Tickets", dump_name="troticke", desc="Trouble Tickets")
        t.column('CustNum', 'integer', mandatory=True, format=">>>>>>>>9", initial="0", max_width=4, label="Customer", column_label="Customer", position=3, order=20, help="Customer's number")
        t.column('CreStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time when trouble ticket was created", max_width=20, label="Created", column_label="Created", position=4, order=30, description="Time Stamp yyyymmdd.time (sec)")
        t.column('CreUser', 'character', format="x(8)", initial="", max_width=16, label="C-user", column_label="C-user", position=5, order=40, help="User who created this trouble ticket")
        t.column('TTType', 'character', format="X(8)", initial="", max_width=16, label="Type", column_label="Type", position=6, order=50, help="Code of trouble type")
        t.column('Handler', 'character', format="x(8)", initial="", help="Code of the responsible person", max_width=16, label="Responsible Person", column_label="Responsible Person", position=7, order=60, description="Responsible Person")
        t.column('TTStatus', 'character', mandatory=True, format="X(8)", initial="", max_width=16, label="Status", column_label="Status", position=8, order=70, help="Code of trouble status")
        t.column('CLI', 'character', format="X(16)", initial="", help="Customer's A-sub Number with Area Code/MSISDN Subscriber No", max_width=32, label="A-Sub No./MSISDN No", column_label="A-Sub No./MSISDN No", position=10, order=90, description="A-Sub No./MSISDN No")
        t.column('Contact', 'character', format="x(30)", initial="", max_width=60, label="Contact Person", column_label="Contact Person", position=11, order=100, help="Customer's contact person name")
        t.column('SolvedStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time when trouble  was solved", max_width=20, label="Solved", column_label="Solved", position=12, order=110, description="Time Stamp yyyymmdd.time (sec)")
        t.column('Solver', 'character', format="x(8)", initial="", help="Person who rectified the problem.", max_width=16, label="Solved By", column_label="Solved By", position=13, order=120, description="Person who rectified the problem.")
        t.column('TTNum', 'integer', format=">,>>>,>>9", initial="0", help="Reference no. for Customer", max_width=4, label="Reference", column_label="Reference", position=14, order=130, description="Auto-generated sequence number for Trouble Ticket")
        t.column('CustType', 'logical', format="Mobile/Fixed", initial="Mobile", max_width=1, label="Customer Type", position=15, order=140, help="Mobile or Fixed Telephone problem?")
        t.column('TargetStamp', 'decimal', format="99999999.99999", decimals=5, initial="0", help="Date and time of target time for Trouble ticket to be solved", max_width=20, label="Target", column_label="Created", position=16, order=150, description="Time Stamp yyyymmdd.time (sec)")
        t.column('EMail', 'character', format="X(30)", initial="", max_width=60, label="Email", column_label="Email", position=17, order=160, help="Email id comtact of person in case of any trouble.")
        t.column('MobileNbr', 'character', format="X(20)", initial="", max_width=40, label="GSM No.", column_label="GSM No.", position=18, order=170, help="Mobile No, of person to deliver SMS messages")
        t.column('Memo', 'character', format="x(60)", initial="", max_width=120, label="Memo", column_label="Memo", position=19, order=180, help="Explanation field for trouble ticket")
        t.index('TTNum', [['TTNum']], area="Sta_Index_2", primary=True, unique=True)
        t.index('CustNum', [['CustNum']], area="Sta_Index_2")
        t.index('tr-nro', [['CLI']], area="Sta_Index_2")

    def down(self):
        self.drop_table('TroTicket')
